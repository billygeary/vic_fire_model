
#### SETUP ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(dplyr)
library(tidyr)
library(gbm)
library(pROC)
library(caret)
library(dismo)

data = read.csv("F:/vic_fire_mapping/output_data/full_fire_covariates.csv")

data = data %>% filter(year > 1990) # What year to go back to in the data?

data$fuel_management_zones = as.factor(data$fuel_management_zones)

#### ADD TEMPORAL FOLDS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
data$time_folds <- cut(data$year, breaks = c(1990, 2001, 2012, 2024))
data = data %>% mutate(time_folds = case_when(time_folds == "(1.99e+03,2e+03]" ~ "1990_2001",
                                              time_folds == "(2e+03,2.01e+03]" ~ "2002_2012",
                                              time_folds == "(2.01e+03,2.02e+03]" ~ "2013_2024"))

data$spatial_temporal_folds_lab = paste0(data$spatial_folds, "_", data$time_folds)
data$spatial_temporal_folds <- as.numeric(as.factor(data$spatial_temporal_folds_lab))

#### CHECK FOR COVARIATE CORRELATION ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cor.data=data %>% dplyr::select("year", "ffdi_mean", "thunderstorm_days", "spei12_mean", "spei24_mean",
                                "ffdi_95_days","kbdi_95_days","distance_roads",
                                "broad_refuges","local_refuges","autocov",
                                "twi","bio1","bio18","bio5","bdw","cly","nvc","phw") %>%
  cor(use="pairwise.complete.obs")

corrplot::corrplot(cor.data)

#### SELECT COVARIATES TO MODEL ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fire_modelling = data %>% 
  dplyr::select(x,y, spatial_temporal_folds, burnt, cly, distance_roads, bio5, ffdi_95_days, thunderstorm_days, 
                spei12_mean, broad_refuges, local_refuges, year) %>%
  drop_na()

#### TRAIN THE FIRST MODEL ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source("scripts/fun_fit_iterative_brt.R")
job::job({fit_iterative_brt(fire_modelling, 0.025, seed = 123)})

#### MODEL EXPLORATION ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
output_dir <- "F:/vic_fire_mapping/output_data"
fire_brt = readr::read_rds(file.path(output_dir, "lr_0.025_fire_model.RDS"))

#### GENERATE AUTOCOVARIATE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Follows Crase et al. 2012 Ecography
# https://nsojournals.onlinelibrary.wiley.com/doi/epdf/10.1111/j.1600-0587.2011.07138.x

gbm::plot.gbm(fire_brt, "ffdi_95_days", type="response")
gbm::plot.gbm(fire_brt, "spei12_mean", type="response")
gbm::plot.gbm(fire_brt, "bio5", type="response")
gbm::plot.gbm(fire_brt, "local_refuges", type="response")
gbm::plot.gbm(fire_brt, "thunderstorm_days", type="response")
gbm::plot.gbm(fire_brt, "rac", type="response")

gbm::plot.gbm(fire_brt, c("ffdi_95_days","spei12_mean"), type="response")
gbm::plot.gbm(fire_brt, c("ffdi_95_days","thunderstorm_days"), type="response")


#### MODEL EVALUATION ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Out of bag error, k fold cross validation error, AUC
fire_brt$cv.statistics

# Training AUC
pred_probs <- predict(fire_brt, newdata = fire_modelling, n.trees = gbm.perf(fire_brt, method = "test", plot.it=FALSE), type = "response")
(auc_value <- roc(fire_modelling$burnt, pred_probs))

#### CHECK SPATIAL AUTOCORRELATION ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Assuming your data has spatial coordinates: lon (longitude) and lat (latitude)
coords <- cbind(fire_modelling$x, fire_modelling$y)
coords.unique = coords %>% as.data.frame() %>% distinct()
resid= coords %>% as.data.frame() %>% mutate(residuals = fire_brt$residuals) %>% group_by(V1,V2) %>% summarise(mean.resid = mean(residuals))

# Create spatial neighbors using k-nearest neighbors (adjust k as needed)# Create spatial neighbors using k-nearest neighbors (residualsadjust k as needed)
nb <- knn2nb(knearneigh(coords, k = 12))  # 8 nearest neighbors

# Convert to a spatial weights matrix
weights <- nb2listw(nb, style = "W", zero.policy = TRUE)

# Morans I - Overall
moran_test <- moran.test(resid$mean.resid, weights, zero.policy = TRUE)
moran_test

# Spline
library(ncf)
coords <- cbind(fire_modelling$x, fire_modelling$y)
coords.unique = coords %>% as.data.frame() %>% distinct()
resid= coords %>% as.data.frame() %>% mutate(residuals = fire_brt$residuals) %>% group_by(V1,V2) %>% summarise(mean.resid = mean(residuals))

spline_corr <- spline.correlog(x = resid$V1, 
                               y = resid$V2, 
                               z = resid$residuals, 
                               resamp = 100)  # resampling for CI

plot(spline_corr, 
     main = "Spline Correlogram of BRT Residuals", 
     xlab = "Distance (units of your coordinates)", 
     ylab = "Spatial autocorrelation (Moran's I)")

# This is the minimum distance of the autocorrelation (x value when y = 0)
# In otherwords, this will be the buffer used to ID the number of neighbours per year
summary(spline_corr)$estimate[1]

# Yearly Morans I
# This bit needs fixing up and integrating into Charlie's code
fire_brt = readr::read_rds(file.path(output_dir, "lr_0.025_fire_model.RDS"))
years <- unique(fire_modelling$year)
coords <- cbind(fire_modelling$x, fire_modelling$y, fire_modelling$year)
resid= coords %>% as.data.frame() %>% mutate(residuals = fire_brt$residuals) %>% group_by(V1,V2) %>% summarise(mean.resid = mean(residuals))

residuals_brt = fire_modelling %>% mutate(residuals = fire_brt$residuals)
for (y in 1:length(years)) {
  target_year = years[y]
  yearly_residuals <- residuals_brt %>% dplyr::filter(year == target_year)
  
  spline_corr <- spline.correlog(x = yearly_residuals$V1, 
                                 y = yearly_residuals$V2, 
                                 z = yearly_residuals$residuals, 
                                 resamp = 100)  # resampling for CI
  
  int = summary(spline_corr)$estimate[1]
  cat("Year:", target_year, "Min Distance", int, "\n")
}
