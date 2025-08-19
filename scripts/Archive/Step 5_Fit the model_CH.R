
#### SETUP ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(dplyr)
library(tidyr)
library(gbm)
library(pROC)
library(caret)
library(dismo)
library(spdep)


og_data = read.csv("F:/vic_fire_mapping/output_data/full_fire_covariates.csv")

data = og_data %>% filter(year > 1990) # What year to go back to in the data?

data$fuel_management_zones = as.factor(data$fuel_management_zones)

# #### ADD TEMPORAL FOLDS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# data$time_folds <- cut(data$year, breaks = c(1990, 2001, 2012, 2024))
# data = data %>% mutate(time_folds = case_when(time_folds == "(1.99e+03,2e+03]" ~ "1990_2001",
#                                               time_folds == "(2e+03,2.01e+03]" ~ "2002_2012",
#                                               time_folds == "(2.01e+03,2.02e+03]" ~ "2013_2024"))
# 
# data$spatial_temporal_folds_lab = paste0(data$spatial_folds, "_", data$time_folds)
# data$spatial_temporal_folds <- as.numeric(as.factor(data$spatial_temporal_folds_lab))


#### ADD TEMPORAL FOLDS (V2: One Fold Per Year) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create one time fold per year as a character label
data$time_folds <- as.character(data$year)

# Combine spatial and temporal folds to create unique fold IDs
data$spatial_temporal_folds_lab <- paste0(data$spatial_folds, "_", data$time_folds)

# Convert to numeric fold IDs
data$spatial_temporal_folds <- as.numeric(as.factor(data$spatial_temporal_folds_lab))

cat("Number of unique spatial-temporal folds:", length(unique(data$spatial_temporal_folds)), "\n")

#check counts in each fold
table(data$spatial_temporal_folds, data$burnt)

# # Identify folds with both burnt == 0 and 1
# fold_counts <- data %>%
#   group_by(spatial_temporal_folds, burnt) %>%
#   summarise(n = n(), .groups = "drop") %>%
#   tidyr::pivot_wider(names_from = burnt, values_from = n, values_fill = 0)
# 
# # Keep only folds that have at least one of each class
# valid_folds <- fold_counts %>%
#   filter(`0` > 0 & `1` > 0) %>%
#   pull(spatial_temporal_folds)
# 
# # Filter your data
# data <- data %>% filter(spatial_temporal_folds %in% valid_folds)
# 
# #check counts in each fold
# table(data$spatial_temporal_folds, data$burnt)

set.seed(123)  # Reproducibility

# Unique fold labels
fold_labels <- unique(data$spatial_temporal_folds)

# Randomly assign each label to one of 15 groups
collapsed_map <- sample(1:15, size = length(fold_labels), replace = TRUE)
names(collapsed_map) <- fold_labels

# Apply to dataset
data$cv_folds <- collapsed_map[as.character(data$spatial_temporal_folds)]

# Quick check of distribution
cat("Number of data points per CV fold:\n")
print(table(data$cv_folds, data$burnt))




#### CHECK FOR COVARIATE CORRELATION ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cor.data=og_data %>% dplyr::select("year", "ffdi_mean", "thunderstorm_days", "spei12_mean", "spei24_mean",
                                "ffdi_95_days","kbdi_95_days","distance_roads",
                                "broad_refuges","local_refuges","autocov",
                                "twi","bio1","bio18","bio5","bdw","cly","nvc","phw") %>%
  cor(use="pairwise.complete.obs")

corrplot::corrplot(cor.data)

#### SELECT COVARIATES TO MODEL ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

colnames(data)


fire_modelling = data %>% 
  dplyr::select(x,y, cv_folds, burnt, cly, distance_roads, bio5, ffdi_95_days, thunderstorm_days, 
                spei12_mean, broad_refuges, local_refuges, year) %>%
  drop_na() %>%
  rename(spatial_temporal_folds = cv_folds)  # 🔄 Added this line to match model expectation

#### TRAIN THE FIRST MODEL ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#source("scripts/fun_fit_iterative_brt.R")
source("C:/Users/charliehart/The University of Melbourne/Billy Geary - vic_fire_model/scripts/fun_fit_iterative_brt.R")
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

gbm::plot.gbm(fire_brt, c("ffdi_95_days","spei12_mean"), type="response")
gbm::plot.gbm(fire_brt, c("ffdi_95_days","thunderstorm_days"), type="response")


##check spread of data
hist(fire_modelling$ffdi_95_days,
     breaks = 30,
     main = "Distribution of FFDI (95th Percentile)",
     xlab = "ffdi_95_days",
     col = "lightblue",
     border = "white")
hist(fire_modelling$spei12_mean,
     breaks = 30,
     main = "Distribution of spei12_mean",
     xlab = "spei12 mean",
     col = "lightblue",
     border = "white")
hist(fire_modelling$bio5,
     breaks = 30,
     main = "Distribution of bio_5",
     xlab = "bio_5",
     col = "lightblue",
     border = "white")
hist(fire_modelling$local_refuges,
     breaks = 30,
     main = "Distribution of local_refuges",
     xlab = "local refuges",
     col = "lightblue",
     border = "white")
hist(fire_modelling$thunderstorm_days,
     breaks = 30,
     main = "Distribution of thunderstorm days",
     xlab = "thunderstorm dys",
     col = "lightblue",
     border = "white")

#we should probably remove the outlier for local_refuges
max(fire_modelling$local_refuges, na.rm = TRUE)
summary(fire_modelling$local_refuges)



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

# Morans I - Overall (Billy's original)
moran_test <- moran.test(resid$mean.resid, weights, zero.policy = TRUE)
moran_test


##Billy's code gave me an error. Object of different length.
moran_test <- moran.test(fire_brt$residuals, weights, zero.policy = TRUE)
moran_test


# Yearly Morans I
years <- unique(fire_modelling$year)
residuals_brt = fire_modelling %>% mutate(residuals = fire_brt$residuals)
for (y in 1:length(years)) {
  target_year = years[y]
  yearly_residuals <- residuals_brt %>% dplyr::filter(year == target_year)
  moran_test <- moran.test(yearly_residuals$residuals, weights, zero.policy = TRUE)
  cat("Year:", target_year, "Moran’s I:", moran_test$estimate[1], "p-value:", moran_test$p.value, "\n")
}

#still getting same error about object lengths
for (y in 1:length(years)) {
  target_year <- years[y]
  
  # Filter by year
  yearly_residuals <- fire_modelling %>%
    filter(year == target_year) %>%
    mutate(residuals = fire_brt$residuals[fire_modelling$year == target_year])
  
  # Get this year's coordinates
  coords_year <- cbind(yearly_residuals$x, yearly_residuals$y)
  
  # Build weights from this year's coords
  nb_year <- knn2nb(knearneigh(coords_year, k = 12))
  weights_year <- nb2listw(nb_year, style = "W", zero.policy = TRUE)
  
  # Moran's I
  moran_test <- moran.test(yearly_residuals$residuals, weights_year, zero.policy = TRUE)
  cat("Year:", target_year, "Moran’s I:", moran_test$estimate[1], "p-value:", moran_test$p.value, "\n")
}






#### RUN MODEL ON NEW CV FOLDS (3 Time Periods) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### SETUP ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(dplyr)
library(tidyr)
library(gbm)
library(pROC)
library(caret)
library(dismo)
library(spdep)

# Load the new dataset with updated folds
data <- read.csv("F:/vic_fire_mapping/output_data/full_fire_covariates_CV_new.csv")

# Convert relevant columns to factor
data$fuel_management_zones <- as.factor(data$fuel_management_zones)

#### SKIP TEMPORAL FOLD CREATION — Already handled in the new folds ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ✅ No need to recreate temporal folds or randomly assign collapsed CV folds — already handled

# Check the final number of folds
cat("Number of unique spatial-temporal folds:", length(unique(data$spatial_temporal_fold)), "\n")

#### SELECT COVARIATES TO MODEL ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fire_modelling <- data %>% 
  dplyr::select(x, y, spatial_temporal_fold, burnt, cly, distance_roads, bio5, ffdi_95_days, thunderstorm_days, 
                spei12_mean, broad_refuges, local_refuges, year) %>%
  drop_na()

#### FIT THE MODEL ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source("C:/Users/charliehart/The University of Melbourne/Billy Geary - vic_fire_model/scripts/fun_fit_iterative_brt.R")

# Rename fold column to match model expectation
fire_modelling <- fire_modelling %>% 
  rename(spatial_temporal_folds = spatial_temporal_fold)

# Train the model
job::job({fit_iterative_brt(fire_modelling, 0.025, seed = 123)})

#### NEXT STEPS AFTER MODEL COMPLETION ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# After the model runs, evaluate it the same way:
#  - Load the model from output directory
#  - Check `cv.statistics`
#  - Predict on training data
#  - Compute AUC
#  - Check spatial autocorrelation (Moran's I)


#### LOAD MODEL ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
output_dir <- "F:/vic_fire_mapping/output_data"
fire_brt <- readr::read_rds(file.path(output_dir, "lr_0.025_fire_model.RDS"))

#### MODEL PERFORMANCE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check CV statistics
print(fire_brt$cv.statistics)

# Predict on training data
pred_probs <- predict(fire_brt,
                      newdata = fire_modelling,
                      n.trees = gbm.perf(fire_brt, method = "test", plot.it = FALSE),
                      type = "response")

# AUC
library(pROC)
auc_result <- roc(fire_modelling$burnt, pred_probs)
print(auc_result)

#### CHECK SPATIAL AUTOCORRELATION (MORAN'S I) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(spdep)

# Extract residuals and coordinates
fire_modelling$residuals <- fire_brt$residuals
coords <- cbind(fire_modelling$x, fire_modelling$y)

# Create spatial neighbors (k = 12)
nb <- knn2nb(knearneigh(coords, k = 12))
weights <- nb2listw(nb, style = "W", zero.policy = TRUE)

# Compute Moran's I
moran_result <- moran.test(fire_modelling$residuals, weights, zero.policy = TRUE)
print(moran_result)

gbm::plot.gbm(fire_brt, "ffdi_95_days", type="response")
gbm::plot.gbm(fire_brt, "spei12_mean", type="response")
gbm::plot.gbm(fire_brt, "bio5", type="response")
gbm::plot.gbm(fire_brt, "local_refuges", type="response")
gbm::plot.gbm(fire_brt, "thunderstorm_days", type="response")









#### IMPLEMENTING RAC METHOD (FULL DATASET) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

message("Starting RAC implementation on full dataset...")


# 2. Fit model on full data (needed to get residuals)
source("C:/Users/charliehart/The University of Melbourne/Billy Geary - vic_fire_model/scripts/fun_fit_iterative_brt_CH_RAC.R")
fire_modelling <- data %>%
  dplyr::select(x, y, spatial_temporal_fold, burnt, cly, distance_roads, bio5, ffdi_95_days, thunderstorm_days, 
                spei12_mean, broad_refuges, local_refuges, year) %>%
  drop_na() 

message("Fitting BRT model on full dataset...")
brt_full <- fit_iterative_brt(fire_modelling, 0.025, seed = 123)

summary(brt_full)

# 3. Add residuals
fire_modelling$residuals <- brt_full$residuals

#### BASELINE SPATIAL AUTOCORRELATION CHECK (BEFORE RAC) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

message("Calculating Moran's I for residuals from the original model...")

# Ensure residuals are available from the original (non-RAC) model
fire_modelling$residuals <- brt_full$residuals

# Create spatial weights from full dataset coordinates
coords_original <- cbind(fire_modelling$x, fire_modelling$y)
nb_original <- knn2nb(knearneigh(coords_original, k = 12))
weights_original <- nb2listw(nb_original, style = "W", zero.policy = TRUE)

# Moran’s I for the original model residuals
moran_original <- moran.test(fire_modelling$residuals, weights_original, zero.policy = TRUE)
print(moran_original)


# 4. Build neighbor list (k = 12) and weights matrix (takes a while)
message("Building neighbor list and spatial weights matrix...")
nb_full <- knn2nb(knearneigh(coords_full, k = 12))
weights_full <- nb2listw(nb_full, style = "W", zero.policy = TRUE)

# 5. Match residuals to unique coordinates
message("Computing residual autocovariate (RAC)...")
residual_df_full <- fire_modelling %>%
  filter(!is.na(residuals)) %>%
  dplyr::select(x, y,year, residuals) 

# residual.plot = residual_df_full %>% filter(year ==2009) %>%
#   ggplot() + 
#   geom_point(aes(x=x,y=y, colour = residuals)) + 
#   scale_colour_viridis_c()
# residual.plot

# 6. Compute RAC values
rac.out = list()
for (y in unique(residual_df_full$year)){
  residual_df_sub <- residual_df_full[residual_df_full$year == y,]
  rac_values_full <- lag.listw(weights_full, residual_df_sub$residuals, zero.policy = TRUE)
  residual_df_sub$rac = rac_values_full
  rac.out[[y]] <- residual_df_sub
}

rac.out = do.call("rbind", rac.out)



# 7. Join RAC values back into main dataset
residual_df_full$rac <- rac.out$rac
fire_modelling_rac <- fire_modelling %>%
  left_join(residual_df_full, by = c("x", "y", "year"))

# 8. Check RAC summary
summary(fire_modelling_rac$rac)


#### 9. Prepare model input with RAC included ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fire_modelling_rac_ready <- fire_modelling_rac %>%
  dplyr::select(x, y, burnt, cly, distance_roads, bio5, ffdi_95_days, thunderstorm_days, 
                spei12_mean, broad_refuges, local_refuges, rac, year) %>%
  drop_na() %>%
  mutate(blank_col = NA) %>%
  dplyr::relocate(blank_col, .after = y)  # Insert blank_col in position 3

#### 10. Fit final model with RAC ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

message("Fitting final RAC model on full data...")
brt_full_rac <- fit_iterative_brt(fire_modelling_rac_ready, 0.025, seed = 123)

#### 11. Evaluate full RAC model ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Predicted probabilities and AUC
pred_probs_full_rac <- predict(brt_full_rac, newdata = fire_modelling_rac_ready,
                               n.trees = gbm.perf(brt_full_rac, method = "test", plot.it = FALSE),
                               type = "response")

(auc_full_rac <- pROC::roc(fire_modelling_rac_ready$burnt, pred_probs_full_rac))

# CV performance
brt_full_rac$cv.statistics

#### 12. Global Moran's I ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fire_modelling_rac_ready$residuals <- brt_full_rac$residuals

coords_rac_full <- cbind(fire_modelling_rac_ready$x, fire_modelling_rac_ready$y)
nb_rac_full <- knn2nb(knearneigh(coords_rac_full, k = 12))
weights_rac_full <- nb2listw(nb_rac_full, style = "W", zero.policy = TRUE)

moran_rac_full <- moran.test(fire_modelling_rac_ready$residuals, weights_rac_full, zero.policy = TRUE)
moran_rac_full

gbm::plot.gbm(brt_full_rac, "ffdi_95_days", type="response")
gbm::plot.gbm(brt_full_rac, "spei12_mean", type="response")
gbm::plot.gbm(brt_full_rac, "bio5", type="response")
gbm::plot.gbm(brt_full_rac, "local_refuges", type="response")
gbm::plot.gbm(brt_full_rac, "thunderstorm_days", type="response")
gbm::plot.gbm(brt_full_rac, "rac", type="response")

summary(brt_full_rac, plotit = TRUE)

















##### IMPLEMENTING RAC METHOD + SPLINE CORRELOGRAM~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

message("Starting RAC implementation on full dataset...")

# 1. Load BRT model from previous run
message("Loading previously saved BRT model...")
brt_full <- readRDS("F:/vic_fire_mapping/output_data/lr_0.025_fire_model.RDS")

# 2. Prepare dataset and attach residuals
fire_modelling <- data %>%
  dplyr::select(x, y, spatial_temporal_fold, burnt, cly, distance_roads, bio5, ffdi_95_days, thunderstorm_days, 
                spei12_mean, broad_refuges, local_refuges, year) %>%
  drop_na()
source("C:/Users/charliehart/The University of Melbourne/Billy Geary - vic_fire_model/scripts/fun_fit_iterative_brt_CH_RAC.R")


fire_modelling$residuals <- brt_full$residuals

# Optional: print model summary
summary(brt_full)

#### 3. SPLINE CORRELOGRAM TO ESTIMATE DISTANCE PER YEAR ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
message("Estimating autocorrelation distance per year using spline correlogram...")

# Storage list for spline distances
spline_distances <- list()

# Loop through each year
years <- sort(unique(fire_modelling$year))

for (target_year in years) {
  message(paste("Processing year:", target_year))
  
  # Subset residuals for this year
  yearly_df <- fire_modelling %>%
    filter(year == target_year) %>%
    group_by(x, y) %>%
    summarise(mean_resid = mean(residuals), .groups = "drop")
  
  # Run spline correlogram
  spline_corr <- spline.correlog(x = yearly_df$x,
                                 y = yearly_df$y,
                                 z = yearly_df$mean_resid,
                                 resamp = 100)
  
  # Plot correlogram
  plot(spline_corr,
       main = paste("Spline Correlogram -", target_year),
       xlab = "Distance",
       ylab = "Spatial autocorrelation (Moran's I)")
  
  # Extract x-intercept (distance where autocorrelation = 0)
  spline_dist <- summary(spline_corr)$estimate[1]
  spline_distances[[as.character(target_year)]] <- spline_dist
  
  cat("Year:", target_year, " | Estimated min autocorrelation distance:", spline_dist, "\n")
}







#### 4. COMPUTE RAC USING DISTANCE-BASED NEIGHBOURHOOD ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
message("Calculating RAC using spline-derived distances...")

rac_all_years <- list()

for (target_year in years) {
  message(paste("Computing RAC for year:", target_year))
  
  dist_thresh <- spline_distances[[as.character(target_year)]]
  
  sub_df <- fire_modelling %>%
    filter(year == target_year) %>%
    dplyr::select(x, y, residuals)
  
  coords <- cbind(sub_df$x, sub_df$y)
  nb <- dnearneigh(coords, 0, dist_thresh)
  lw <- nb2listw(nb, style = "W", zero.policy = TRUE)
  
  # Calculate RAC
  rac_vals <- lag.listw(lw, sub_df$residuals, zero.policy = TRUE)
  sub_df$rac <- rac_vals
  sub_df$year <- target_year
  
  rac_all_years[[as.character(target_year)]] <- sub_df
}

# Combine all RAC values
rac_combined <- bind_rows(rac_all_years)

# Join back to full dataset
fire_modelling_rac <- fire_modelling %>%
  left_join(rac_combined %>% dplyr::select(x, y, year, rac), by = c("x", "y", "year"))

# Final input
fire_modelling_rac_ready <- fire_modelling_rac %>%
  dplyr::select(x, y, burnt, cly, distance_roads, bio5, ffdi_95_days,
                thunderstorm_days, spei12_mean, broad_refuges, local_refuges,
                rac, year) %>%
  drop_na()

#### 5. FIT MODEL WITH DISTANCE-BASED RAC ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
message("Fitting final model with RAC based on spline correlogram distances...")

brt_rac <- fit_iterative_brt(fire_modelling_rac_ready, lr = 0.025, seed = 123)

# Evaluate
pred_probs_rac <- predict(brt_rac, newdata = fire_modelling_rac_ready,
                          n.trees = gbm.perf(brt_rac, method = "test", plot.it = FALSE),
                          type = "response")
auc_rac <- pROC::roc(fire_modelling_rac_ready$burnt, pred_probs_rac)
print(auc_rac)

#### 6. Global Moran's I Check on Final Residuals ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fire_modelling_rac_ready$residuals <- brt_rac$residuals
coords_final <- cbind(fire_modelling_rac_ready$x, fire_modelling_rac_ready$y)
nb_final <- dnearneigh(coords_final, 0, max(unlist(spline_distances)))  # use max distance
weights_final <- nb2listw(nb_final, style = "W", zero.policy = TRUE)

moran_final <- moran.test(fire_modelling_rac_ready$residuals, weights_final, zero.policy = TRUE)
print(moran_final)












##Evaluating Neighborhood Density for Spatial Smoothing by Year Group


library(dplyr)
library(readr)
library(geosphere)  # for distGeo
library(tidyr)
library(purrr)

# 1. Load and filter data -------------------------------------------------
og_data <- read.csv("F:/vic_fire_mapping/output_data/full_fire_covariates.csv")

data <- og_data %>%
  filter(year >= 1990) %>%  # Adjust if needed
  mutate(fuel_management_zones = as.factor(fuel_management_zones)) %>%
  drop_na()

# Rename for clarity
late_data_dist <- data

# 2. Define years with small vs large spatial autocorrelation ------------
small_years <- c(1990, 1994, 1996, 1997, 2001, 2003, 2004, 2005, 2006, 2007,
                 2008, 2009, 2013, 2014, 2015, 2016, 2019, 2020)

large_years <- c(1991, 1992, 1993, 1995, 1998, 2000, 2002, 2010, 2011, 2012,
                 2017, 2018, 2021, 2022, 2023)

# 3. Subset into small and large distance groups --------------------------
small_group <- late_data_dist %>% filter(year %in% small_years)
large_group <- late_data_dist %>% filter(year %in% large_years)

# 4. Function to count neighbors within radius ----------------------------
count_neighbors_sampled <- function(df, radius, n_sample = 100) {
  coords <- df %>% select(x, y)
  n_points <- nrow(coords)
  
  # If fewer points than n_sample, use all
  sample_idx <- if (n_points <= n_sample) {
    1:n_points
  } else {
    sample(1:n_points, n_sample)
  }
  
  neighbor_counts <- map_int(sample_idx, function(i) {
    this_point <- coords[i, ]
    distances <- sqrt((coords$x - this_point$x)^2 + (coords$y - this_point$y)^2)
    sum(distances <= radius) - 1
  })
  
  return(neighbor_counts)
}


# 5. Loop by year and season, calculate average neighbors -----------------
summarise_neighbors_sampled <- function(group_df, radius, label, n_sample = 100) {
  group_df %>%
    group_by(year) %>%
    nest() %>%
    mutate(
      neighbors = map(data, ~ count_neighbors_sampled(.x, radius, n_sample)),
      avg_neighbors = map_dbl(neighbors, mean),
      n_points = map_int(data, nrow)
    ) %>%
    select(year, avg_neighbors, n_points) %>%
    mutate(group = label)
}

# Run for each group
summary_small <- summarise_neighbors_sampled(small_group, radius = 100000, label = "small")
summary_large <- summarise_neighbors_sampled(large_group, radius = 300000, label = "large")

# Combine and view
neighbor_summary <- bind_rows(summary_small, summary_large)

print(neighbor_summary, n=50)


neighbor_summary %>%
  group_by(group) %>%
  summarise(
    mean_avg_neighbors = mean(avg_neighbors),
    sd_avg_neighbors   = sd(avg_neighbors),
    min_avg_neighbors  = min(avg_neighbors),
    max_avg_neighbors  = max(avg_neighbors),
    n_years            = n()
  )












# =============================================================================
# RAC Implementation with Variable Smoothing (1100 vs 5800 Neighbors)
# =============================================================================

library(dplyr)
library(spdep)
library(readr)
library(tidyr)

message("🔄 Starting RAC implementation using variable smoothing distances...")

# -----------------------------------------------------------------------------
# 1. Load and prepare data ----------------------------------------------------
# -----------------------------------------------------------------------------

# Define small and large autocorrelation years
small_years <- c(1990, 1994, 1996, 1997, 2001, 2003, 2004, 2005, 2006, 2007,
                 2008, 2009, 2013, 2014, 2015, 2016, 2019, 2020)
large_years <- c(1991, 1992, 1993, 1995, 1998, 2000, 2002, 2010, 2011, 2012,
                 2017, 2018, 2021, 2022, 2023)

# Build model input
fire_modelling <- data %>%
  dplyr::select(x, y, spatial_temporal_fold, burnt, cly, distance_roads, bio5, ffdi_95_days, thunderstorm_days,
         spei12_mean, broad_refuges, local_refuges, year) %>%
  drop_na()


# -----------------------------------------------------------------------------
# 2. Fit base BRT model to get residuals --------------------------------------
# -----------------------------------------------------------------------------

message("🌲 Fitting base BRT model (no RAC)...")

source("C:/Users/charliehart/The University of Melbourne/Billy Geary - vic_fire_model/scripts/fun_fit_iterative_brt_CH_RAC.R")

brt_full <- fit_iterative_brt(data = fire_modelling, 0.025, seed = 123)

# Save it for future use
saveRDS(brt_full, "F:/vic_fire_mapping/output_data/lr_0.025_fire_model_REBUILT.RDS")

# Add residuals to dataset
fire_modelling$residuals <- brt_full$residuals

# -----------------------------------------------------------------------------
# 3. Compute RAC using adaptive neighbor sizes --------------------------------
# -----------------------------------------------------------------------------

# Reload pre-fit model (assumes it was saved before the crash)
brt_full <- readRDS("F:/vic_fire_mapping/output_data/lr_0.025_fire_model_REBUILT.RDS")
fire_modelling$residuals <- brt_full$residuals

# Convert to tibble (to avoid dplyr::select issues)
fire_modelling <- dplyr::as_tibble(fire_modelling)


message("📍 Computing RAC using distance thresholds (100 km / 300 km)...")

rac_results <- list()

for (yr in unique(fire_modelling$year)) {
  message(paste("➡️  Year:", yr))
  
  sub_df <- fire_modelling %>%
    filter(year == yr) %>%
    select(x, y, residuals)
  
  coords <- as.matrix(sub_df[, c("x", "y")])
  
  # Choose distance threshold (in meters)
  d_thresh <- if (yr %in% small_years) 100000 else 300000
  
  # Create distance-based neighbors
  nb <- dnearneigh(x=coords, d1=0, d2=d_thresh, longlat = FALSE)
  lw <- nb2listw(nb, style = "W", zero.policy = TRUE)
  
  # Compute RAC
  rac_vals <- lag.listw(lw, sub_df$residuals, zero.policy = TRUE)
  sub_df$rac <- rac_vals
  sub_df$year <- yr
  
  rac_results[[as.character(yr)]] <- sub_df
}

rac_combined <- bind_rows(rac_results)


# -----------------------------------------------------------------------------
# 4. Join RAC values to full dataset ------------------------------------------
# -----------------------------------------------------------------------------

fire_modelling_rac <- fire_modelling %>%
  left_join(rac_combined %>% select(x, y, year, rac), by = c("x", "y", "year"))

library(ggplot2)

rac_combined %>%
  filter(year == 2020) %>%
  ggplot(aes(x = x, y = y, color = rac)) +
  geom_point(size = 0.6) +
  scale_color_viridis_c() +
  theme_minimal() +
  labs(title = "RAC values - 2009")


# -----------------------------------------------------------------------------
# 5. Prepare final dataset for RAC model --------------------------------------
# -----------------------------------------------------------------------------

fire_modelling_rac_ready <- fire_modelling_rac %>%
  select(x, y, burnt, cly, distance_roads, bio5, ffdi_95_days, thunderstorm_days,
         spei12_mean, broad_refuges, local_refuges, rac, year) %>%
  dplyr::filter(year > 1990) %>%
  drop_na() %>%

str(fire_modelling_rac_ready_df$burnt)

# Optionally save this cleaned RAC-ready dataset
write_csv(fire_modelling_rac_ready, "F:/vic_fire_mapping/output_data/fire_modelling_rac_ready_variable_neighbors.csv")

# -----------------------------------------------------------------------------
# 6. Fit final BRT model with RAC ---------------------------------------------
# -----------------------------------------------------------------------------
fire_modelling_rac_ready <- read.csv("F:/vic_fire_mapping/output_data/fire_modelling_rac_ready_variable_neighbors.csv")
head(fire_modelling_rac_ready)

source("C:/Users/charliehart/The University of Melbourne/Billy Geary - vic_fire_model/scripts/fun_fit_iterative_brt_CH_RAC.R")

message("🔥 Fitting final BRT model with RAC included...")


brt_full_rac <- fit_iterative_brt(fire_modelling_rac_ready, 0.025, seed = 123)

# Save model
saveRDS(brt_full_rac, "F:/vic_fire_mapping/output_data/brt_full_rac_variable_neighbours.rds")

# -----------------------------------------------------------------------------
# 7. Evaluate model -----------------------------------------------------------
# -----------------------------------------------------------------------------

message("📈 Evaluating final RAC model...")

# Predict
pred_probs_full_rac <- predict(brt_full_rac, newdata = fire_modelling_rac_ready,
                               n.trees = gbm.perf(brt_full_rac, method = "test", plot.it = FALSE),
                               type = "response")

# AUC
auc_full_rac <- pROC::roc(fire_modelling_rac_ready$burnt, pred_probs_full_rac)
print(auc_full_rac)

# CV performance
print(brt_full_rac$cv.statistics)

# Variable importance (optional)
summary(brt_full_rac, plotit = TRUE)

