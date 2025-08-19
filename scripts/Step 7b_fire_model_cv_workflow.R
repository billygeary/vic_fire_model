##############################
### Command Line Arguments ###
##############################

message("\n\n### Command Line Arguments ###\n\n")

args <- commandArgs(trailingOnly = TRUE)

fold_id <- args[1]

################
### Set Seed ###
################

set.seed(123 + as.numeric(fold_id))

#####################
### Load Packages ###
#####################
message("\n\n### Load Packages ###\n\n")
.libPaths("/home/geary/R_libs/4.4.0")

library(dplyr)
library(tidyr)
library(gbm)
library(terra)

#################
### Load Data ###
#################

message("\n\n### Load Data ###\n\n")

## Constants ----
source("fun_fit_iterative_brt_rac_forcv.R")

data = read.csv("fire_modelling_rac_ready_forCV.csv")

## Fold specific ----
fold = fold_id

############################
### Run Cross Validation ###
############################

message("\n\n### Starting Cross Validation ###\n\n")

# Split the data
message(paste0("Starting fold ", fold))
train_data <- data %>% filter(composite_fold_no != fold)
test_data  <- data %>% filter(composite_fold_no == fold)

message("\n\n### Fitting Model ###\n\n")
  # Fit the model
  cv_model <- fit_iterative_brt(train_data, 0.05, seed = 123)

message("\n\n### Predict to Test Data ###\n\n")
# Make prediction to testing data
preds <- predict.gbm(cv_model, test_data, n.trees = cv_model$gbm.call$best.trees, type = "response")

message("\n\n### Save Outputs ###\n\n")  
# Save the outputs
output.out = data.frame(obs = test_data$burnt, pred = preds, composite_fold_no = fold)
write.csv(output.out, paste0("outputs/crossvalidation_preds_spat_temp_fold_", fold, ".csv"))
gc()

message("\n\n### Script Complete ###\n\n")  
