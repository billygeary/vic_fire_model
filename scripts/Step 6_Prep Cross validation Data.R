library(dplyr)
library(tidyr)
library(gbm)
library(terra)
library(blockCV)
library(sf)
set.seed(123)

# Cross validation
# https://htmlpreview.github.io/?https://github.com/rvalavi/blockCV/blob/master/inst/doc/tutorial_2.html

fire_modelling_rac_ready <- read.csv("F:/vic_fire_mapping/output_data/fire_modelling_rac_ready_tsf.csv")
fire_modelling_year = read.csv("F:/vic_fire_mapping/output_data/full_fire_covariates_tsf_spei.csv")

data=left_join(fire_modelling_rac_ready, fire_modelling_year) # Get the year variable back

data$fuel_management_zones = as.factor(data$fuel_management_zones)

summary(data)

#### ADD SPATIAL FOLDS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
random_sample = vect("F:/vic_fire_mapping/covariates/random_points.shp")
random_sample = random_sample %>% st_as_sf()

## BlockCV Folds
random_sample = vect("F:/vic_fire_mapping/covariates/random_points.shp")
random_sample = random_sample %>% st_as_sf()
random_sample_cv <- random_sample 

all.data = read.csv("F:/vic_fire_mapping/output_data/full_fire_covariates.csv")
df = all.data %>% group_by(x,y) %>% summarise(burnt.sum = max(burnt,na.rm=TRUE))
random_sample_cv$burnt = df$burnt.sum

sb <- cv_spatial(x = random_sample_cv,
                 "burnt",
                 k = 5, # number of folds
                 size = 150000, # size of the blocks in metres
                 selection = "random", # random blocks-to-fold
                 seed=123,
                 iteration = 50) # find evenly dispersed folds

df$spatial_folds = sb$folds_ids
df = df %>% dplyr::select(x,y,spatial_folds)
data = left_join(data, df, by=c("x","y"))

#### ADD TEMPORAL FOLDS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\
data$temporal_fold <- dplyr::ntile(data$year, 5)  # 5 temporal bins of ~6 years each

#### COMBINE SPATIAL AND TEMPORAL FOLDS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
data$composite_fold <- interaction(data$spatial_folds, data$temporal_fold)

# Sample unique combinations to create n spatial-temporal folds
unique_folds <- unique(data$composite_fold)
data$cv_fold <- sample(rep(1:10, length.out = length(unique_folds)))[match(data$composite_fold, unique_folds)]

# Check the distribution of folds

data$composite_fold_no <- as.numeric(data$composite_fold)
table(data$composite_fold_no)
table(data$composite_fold_no, data$burnt)   # Balance of burned/unburned per fold
table(data$composite_fold_no, data$year)    # Temporal representation
table(data$composite_fold_no, data$spatial_fold)  # Spatial representation
year.folds = data %>% group_by(cv_fold, temporal_fold, spatial_folds) %>% summarise(count = n(), burnt = sum(burnt))


write.csv(data, "F:/vic_fire_mapping/output_data/fire_modelling_rac_ready_forCV.csv")

#### LOOK AT AUC RESULTS  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
all_preds <- bind_rows(results)
roc_curve <- pROC::roc(all_preds$obs, all_preds$pred)
print(roc_curve$auc)

