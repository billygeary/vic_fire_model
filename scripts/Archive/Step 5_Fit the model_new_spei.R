# =============================================================================
# RAC Implementation with Variable Smoothing (1100 vs 5800 Neighbors)
# Extended Predictor Set Based on VIF Analysis
# =============================================================================
library(dplyr)
library(spdep)
library(readr)
library(tidyr)
library(pROC)

message("🔄 Starting RAC implementation using variable smoothing distances...")

# -----------------------------------------------------------------------------
# 1. Load and prepare data ----------------------------------------------------
# -----------------------------------------------------------------------------
setwd("F:/")

library(dplyr)
library(readr)
library(geosphere)  # for distGeo
library(tidyr)
library(purrr)
library(gbm)
library(dismo)

# 1. Load and filter data -------------------------------------------------

# Load the new dataset with updated folds
data <- read.csv("F:/vic_fire_mapping/output_data/full_fire_covariates_tsf_spei.csv")

# Convert relevant columns to factor
data$fuel_management_zones <- as.factor(data$fuel_management_zones)


#### SELECT COVARIATES TO MODEL ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Define small and large autocorrelation years
small_years <- c(1981, 1982, 1983, 1984, 1985, 1987, 1988, 1990, 1994, 
                 1996, 1997, 2001, 2003, 2004, 2005, 2006, 2007,
                 2008, 2009, 2013, 2014, 2015, 2016, 2019, 2020)
large_years <- c(1986, 1989, 1991, 1992, 1993, 1995, 1998, 2000,
                 2002, 2010, 2011, 2012, 2017, 2018, 2021, 2022,
                 2023)

fire_modelling <- data %>%
  # only keep observations after 1981
  dplyr::filter(year > 1980) %>%
  # select all your predictors 
  dplyr::select(
    x, y, year, burnt,
    bio5, spei12_mean, ffdi_95_days, spei24_mean, bio18,
    kbdi_95_days, twi, local_refuges, broad_refuges, thunderstorm_days,
    fuel_management_zones, distance_roads
  ) %>%
  # drop any rows with missing values
  tidyr::drop_na()

# Remove rows with NA or Inf in any column
fire_modelling <- fire_modelling %>%
  filter(if_all(everything(), ~ !is.na(.) & is.finite(.)))
message("Original row count: ", nrow(data))
message("After filtering: ", nrow(fire_modelling))

#fix order so burnt 3rd
fire_modelling_for_brt <- fire_modelling %>%
  dplyr::select(x, y, burnt, everything(), -year)
str(fire_modelling_for_brt)

# -----------------------------------------------------------------------------
# 2. Fit base BRT model to get residuals --------------------------------------
# -----------------------------------------------------------------------------

message("🌲 Fitting base BRT model (no RAC)...")

source("C:/Users/charliehart/The University of Melbourne/Billy Geary - vic_fire_model/scripts/fun_fit_iterative_brt_CH_RAC_v2.R")

brt_full_spei <- fit_iterative_brt(data = fire_modelling_for_brt, 0.05, seed = 123)

# Save it for future use
saveRDS(brt_full_spei, "F:/vic_fire_mapping/output_data/lr_0.05_fire_model_spei.RDS")

# Add residuals to dataset
fire_modelling$residuals <- brt_full_spei$residuals

# 1. Read the model back into R
brt_full_spei <- readRDS("F:/vic_fire_mapping/output_data/lr_0.05_fire_model_spei.RDS")

# 2. Inspect it (optional)
print(brt_full_spei)
str(brt_full_spei)

# 3. (Optionally) save it in your .RData workspace so it’s automatically loaded next time
save(brt_full_spei, file = "F:/vic_fire_mapping/output_data/fire_model_spei.RData")


# -----------------------------------------------------------------------------
# 3. Compute RAC using adaptive neighbor sizes --------------------------------
# -----------------------------------------------------------------------------

# Reload pre-fit model (assumes it was saved before the crash)

fire_modelling$residuals <- brt_full_spei$residuals

# Convert to tibble (to avoid dplyr::select issues)
fire_modelling <- dplyr::as_tibble(fire_modelling)


message("📍 Computing RAC using distance thresholds (100 km / 300 km)...")

rac_results <- list()

for (yr in unique(fire_modelling$year)) {
  message(paste("➡️  Year:", yr))
  
  sub_df <- fire_modelling %>%
    filter(year == yr) %>%
    dplyr::select(x, y, residuals)
  
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
  left_join(rac_combined %>% dplyr::select(x, y, year, rac), by = c("x", "y", "year"))


# -----------------------------------------------------------------------------
# 5. Prepare final dataset for RAC model --------------------------------------
# -----------------------------------------------------------------------------

fire_modelling_rac_ready <- fire_modelling_rac %>%
  dplyr::select(x, y, burnt, bio5, spei12_mean, ffdi_95_days, spei24_mean, bio18,
         kbdi_95_days, twi, local_refuges, broad_refuges, thunderstorm_days,
         fuel_management_zones, distance_roads, rac) %>%
  mutate(fuel_management_zones = as.factor(fuel_management_zones)) %>%
  drop_na()

write_csv(fire_modelling_rac_ready,
           "F:/vic_fire_mapping/output_data/fire_modelling_rac_ready_spei.csv")

fire_modelling_rac_ready <- as.data.frame(fire_modelling_rac_ready)

str(fire_modelling_for_brt)
str(fire_modelling_rac_ready)

# -----------------------------------------------------------------------------
# 6. Fit final BRT model with RAC ---------------------------------------------
# -----------------------------------------------------------------------------
#fire_modelling_rac_ready <- read.csv("F:/vic_fire_mapping/output_data/fire_modelling_rac_ready_spei.csv")
#fire_modelling_rac_ready <- fire_modelling_rac_ready %>%
#   mutate(fuel_management_zones = as.factor(fuel_management_zones)) %>%
#  drop_na()

message("🔥 Fitting final BRT model with RAC included (extended predictors)...")

source("C:/Users/charliehart/The University of Melbourne/Billy Geary - vic_fire_model/scripts/fun_fit_iterative_brt_CH_RAC_v2.R")

brt_full_rac_extra <- fit_iterative_brt(fire_modelling_rac_ready, 0.05, seed = 123)

saveRDS(brt_full_rac_extra,
        "F:/vic_fire_mapping/output_data/brt_full_rac_spei.rds")

# -----------------------------------------------------------------------------
# 7. Evaluate model -----------------------------------------------------------
# -----------------------------------------------------------------------------

message("📈 Evaluating final RAC model...")

pred_probs_full_rac <- predict(brt_full_rac_extra, newdata = fire_modelling_rac_ready,
                               n.trees = gbm.perf(brt_full_rac_extra, method = "test", plot.it = FALSE),
                               type = "response")

auc_full_rac <- pROC::roc(fire_modelling_rac_ready$burnt, pred_probs_full_rac)
cat("✅ AUC:\n")
print(auc_full_rac)

cat("\n📊 CV Performance:\n")
print(brt_full_rac_extra$cv.statistics)

cat("\n📈 Variable Importance:\n")
summary(brt_full_rac_extra, plotit = TRUE)


brt_full_rac_extra

class(fire_modelling_rac_ready$fuel_management_zones)


##cur
plot.gbm(brt_full_rac_extra, "rac", type = "response")
plot.gbm(brt_full_rac_extra, "fuel_management_zones", type = "response")
plot.gbm(brt_full_rac_extra, "spei12_mean", type = "response")
plot.gbm(brt_full_rac_extra, "ffdi_95_days", type = "response")
plot.gbm(brt_full_rac_extra, "spei24_mean", type = "response")
plot.gbm(brt_full_rac_extra, "thunderstorm_days", type = "response")
plot.gbm(brt_full_rac_extra, "tsf", type = "response")
