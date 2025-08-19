# =============================================================================
# Script: calculate_vif_from_model_data.R
# Purpose: Calculate VIF from fire model predictor dataset (excluding ID/spatial vars)
# =============================================================================

# Load libraries
library(dplyr)
library(car)
library(readr)
library(tidyr)

# Read the full dataset
og_data <- read_csv("F:/vic_fire_mapping/output_data/full_fire_covariates.csv")

# Filter to years after 1990
data <- og_data %>% filter(year > 1990)

# Convert factors if needed
data$fuel_management_zones <- as.factor(data$fuel_management_zones)

# Select only predictor columns
data_for_vif <- data %>%
  select(
    ffdi_mean, ffdi_95_days, thunderstorm_days,
    spei12_mean, spei24_mean, kbdi_95_days,
    distance_roads, fuel_management_zones,
    broad_refuges, local_refuges, twi,
    bio1, bio18, bio5, bdw,autocov
  ) %>%
  na.omit()

# Create a dummy response (required for VIF calculation)
dummy_response <- rnorm(nrow(data_for_vif))

# Fit linear model
lm_model <- lm(dummy_response ~ ., data = data_for_vif)

# Calculate VIF
vif_values <- car::vif(lm_model)


# If using factors, vif() returns a matrix — convert to data.frame
if (is.matrix(vif_values)) {
  vif_df <- as.data.frame(vif_values)
  vif_df$Predictor <- rownames(vif_df)
  vif_df <- vif_df %>% select(Predictor, everything()) %>% arrange(desc(`GVIF^(1/(2*Df))`))
} else {
  vif_df <- data.frame(
    Predictor = names(vif_values),
    VIF = round(vif_values, 2)
  ) %>% arrange(desc(VIF))
}

# View it nicely
print(vif_df)





##run without ffdi_mean 
data_for_vif2 <- data %>%
  select(
    # ffdi_mean excluded
    ffdi_95_days, thunderstorm_days,
    spei12_mean, spei24_mean, kbdi_95_days,
    distance_roads, fuel_management_zones,
    broad_refuges, local_refuges, twi,
    bio1, bio18, bio5,bdw,autocov
  ) %>%
  na.omit()

dummy_response2 <- rnorm(nrow(data_for_vif2))
lm_model2 <- lm(dummy_response2 ~ ., data = data_for_vif2)
vif_values2 <- car::vif(lm_model2)

if (is.matrix(vif_values2)) {
  vif_df2 <- as.data.frame(vif_values2)
  vif_df2$Predictor <- rownames(vif_df2)
  vif_df2 <- vif_df2 %>% select(Predictor, everything()) %>% arrange(desc(`GVIF^(1/(2*Df))`))
} else {
  vif_df2 <- data.frame(
    Predictor = names(vif_values2),
    VIF = round(vif_values2, 2)
  ) %>% arrange(desc(VIF))
}

cat("\n📊 VIF results **without ffdi_mean**:\n")
print(vif_df2)




## Run without bdw
data_for_vif3 <- data %>%
  select(
    # ffdi_mean and bdw excluded
    ffdi_95_days, thunderstorm_days,
    spei12_mean, spei24_mean, kbdi_95_days,
    distance_roads, fuel_management_zones,
    broad_refuges, local_refuges, twi,
    bio1, bio18, bio5, autocov
  ) %>%
  na.omit()

dummy_response3 <- rnorm(nrow(data_for_vif3))
lm_model3 <- lm(dummy_response3 ~ ., data = data_for_vif3)
vif_values3 <- car::vif(lm_model3)

if (is.matrix(vif_values3)) {
  vif_df3 <- as.data.frame(vif_values3)
  vif_df3$Predictor <- rownames(vif_df3)
  vif_df3 <- vif_df3 %>% select(Predictor, everything()) %>% arrange(desc(`GVIF^(1/(2*Df))`))
} else {
  vif_df3 <- data.frame(
    Predictor = names(vif_values3),
    VIF = round(vif_values3, 2)
  ) %>% arrange(desc(VIF))
}

cat("\n📊 VIF results **without ffdi_mean and bdw**:\n")
print(vif_df3)



## Run without bdw and bio1
data_for_vif4 <- data %>%
  select(
    # ffdi_mean, bdw, and bio1 excluded
    ffdi_95_days, thunderstorm_days,
    spei12_mean, spei24_mean, kbdi_95_days,
    distance_roads, fuel_management_zones,
    broad_refuges, local_refuges, twi,
    bio18, bio5, autocov
  ) %>%
  na.omit()

dummy_response4 <- rnorm(nrow(data_for_vif4))
lm_model4 <- lm(dummy_response4 ~ ., data = data_for_vif4)
vif_values4 <- car::vif(lm_model4)

if (is.matrix(vif_values4)) {
  vif_df4 <- as.data.frame(vif_values4)
  vif_df4$Predictor <- rownames(vif_df4)
  vif_df4 <- vif_df4 %>% select(Predictor, everything()) %>% arrange(desc(`GVIF^(1/(2*Df))`))
} else {
  vif_df4 <- data.frame(
    Predictor = names(vif_values4),
    VIF = round(vif_values4, 2)
  ) %>% arrange(desc(VIF))
}

cat("\n📊 VIF results **without ffdi_mean, bdw, and bio1**:\n")
print(vif_df4)








#### compare with COVARIATE CORRELATION ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cor.data= data %>% dplyr::select("year", "ffdi_mean", "thunderstorm_days", "spei12_mean", "spei24_mean",
                                   "ffdi_95_days","kbdi_95_days","distance_roads",
                                   "broad_refuges","local_refuges","autocov",
                                   "twi","bio1","bio18","bio5","bdw","cly","nvc","phw") %>%
  cor(use="pairwise.complete.obs")

corrplot::corrplot(cor.data)

print(round(cor.data, 2))
