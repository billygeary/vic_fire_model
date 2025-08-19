check.files = list.files("F:/vic_fire_mapping/climatedatacheck", full=T)

access.stack = rast(check.files[c(1,3)])
obs.stack =  rast(check.files[c(2,4)])

rain = rast("F:/agcd_climate_data/precip/precip_fullstack.tif")
rain = rast("F:/agcd_climate_data/precip/precip_fullstack.tif")

# Check Rainfall


# Check Evapotranspiration

# Water balance
obs_wb = rast("F:/agcd_climate_data/water_balance_obs_vic.tif")
access_wb = rast("F:/future_predictions/ACCESS1-0 RCP85/processed/water_balance_access.tif")

plot(obs_wb[[1]])
plot(access_wb[[1]])


# Check SPEI

spei.modelled = list.files("F:/future_predictions/ACCESS1-0 RCP85/spei_output", full=T)
spei.observed = list.files("F:/awo_climate_data/spei_output", full=T)

spei.m <- rast(spei.modelled[[48]])
spei.o <- rast(spei.observed[[48]])

library(tidyterra)
library(ggplot2)
library(patchwork)

a=ggplot() + geom_spatraster(data=spei.o) + scale_fill_viridis_c(limits = c(-2.5, 3))
b=ggplot() + geom_spatraster(data=spei.m) + scale_fill_viridis_c(limits = c(-2.5, 3))
a+b



#########################################################
obs_wb = rast("F:/agcd_climate_data/water_balance_obs_vic.tif")
acc_wb = rast("F:/future_predictions/ACCESS1-0 RCP85/processed/water_balance_access.tif")

plot(obs_wb[[1]])
plot(access_wb[[1]])

library(terra)
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)

# Load your raster stacks
# Replace these with your actual file paths
observed_wb <- obs_wb[[1:312]]
access_wb <- acc_wb[[1:312]]

# Check that dimensions and extents match
print("Observed data info:")
print(observed_wb)
print("ACCESS data info:")
print(access_wb)

# If extents don't match, you may need to resample or crop
 access_wb <- resample(access_wb, observed_wb)

# 1. Calculate summary statistics across all pixels and time
obs_global_mean <- global(observed_wb, "mean", na.rm = TRUE)
access_global_mean <- global(access_wb, "mean", na.rm = TRUE)

obs_global_sd <- global(observed_wb, "sd", na.rm = TRUE)
access_global_sd <- global(access_wb, "sd", na.rm = TRUE)

print("Global Statistics:")
print(paste("Observed mean:", round(mean(obs_global_mean$mean), 2)))
print(paste("ACCESS mean:", round(mean(access_global_mean$mean), 2)))
print(paste("Observed SD:", round(mean(obs_global_sd$sd), 2)))
print(paste("ACCESS SD:", round(mean(access_global_sd$sd), 2)))

# 2. Calculate pixel-wise temporal means and differences
obs_temporal_mean <- app(observed_wb, mean, na.rm = TRUE)
access_temporal_mean <- app(access_wb, mean, na.rm = TRUE)

# Calculate bias (ACCESS - Observed)
bias_map <- access_temporal_mean - obs_temporal_mean

# 3. Create comparison plots
# Convert to data frames for ggplot
obs_df <- as.data.frame(obs_temporal_mean, xy = TRUE)
access_df <- as.data.frame(access_temporal_mean, xy = TRUE)
bias_df <- as.data.frame(bias_map, xy = TRUE)

names(obs_df)[3] <- "water_balance"
names(access_df)[3] <- "water_balance" 
names(bias_df)[3] <- "bias"

# Plot temporal means
p1 <- ggplot(obs_df, aes(x = x, y = y, fill = water_balance)) +
  geom_raster() +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue",
                       midpoint = 0, name = "WB (mm)") +
  labs(title = "Observed Water Balance (Temporal Mean)", 
       x = "Longitude", y = "Latitude") +
  theme_minimal()

p2 <- ggplot(access_df, aes(x = x, y = y, fill = water_balance)) +
  geom_raster() +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue",
                       midpoint = 0, name = "WB (mm)") +
  labs(title = "ACCESS Water Balance (Temporal Mean)", 
       x = "Longitude", y = "Latitude") +
  theme_minimal()

p3 <- ggplot(bias_df, aes(x = x, y = y, fill = bias)) +
  geom_raster() +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue",
                       midpoint = 0, name = "Bias (mm)") +
  labs(title = "Bias (ACCESS - Observed)", 
       x = "Longitude", y = "Latitude") +
  theme_minimal()

# Display plots
print(p1)
print(p2) 
print(p3)

# 4. Time series comparison (spatial average)
# Calculate spatial means for each time step
obs_spatial_means <- global(observed_wb, "mean", na.rm = TRUE)$mean
access_spatial_means <- global(access_wb, "mean", na.rm = TRUE)$mean

# Assuming monthly data from 1985-2005 (252 months)
# Adjust this based on your actual time dimension
n_months <- nlyr(observed_wb)
dates <- seq(as.Date("1985-01-01"), by = "month", length.out = n_months)

ts_data <- data.frame(
  date = dates,
  observed = obs_spatial_means,
  access = access_spatial_means
) %>%
  pivot_longer(cols = c(observed, access), names_to = "dataset", values_to = "water_balance")

# Time series plot
p4 <- ggplot(ts_data, aes(x = date, y = water_balance, color = dataset)) +
  geom_line(size = 0.7) +
  labs(title = "Spatially-Averaged Water Balance Time Series",
       x = "Date", y = "Water Balance (mm)", color = "Dataset") +
  theme_minimal() +
  scale_color_manual(values = c("observed" = "black", "access" = "red"))

print(p4)

# 5. Seasonal cycle comparison
ts_data$month <- format(ts_data$date, "%m")
ts_data$year <- format(ts_data$date, "%Y")

seasonal_data <- ts_data %>%
  group_by(month, dataset) %>%
  summarise(mean_wb = mean(water_balance, na.rm = TRUE),
            sd_wb = sd(water_balance, na.rm = TRUE),
            .groups = 'drop') %>%
  mutate(month_num = as.numeric(month))

p5 <- ggplot(seasonal_data, aes(x = month_num, y = mean_wb, color = dataset)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = mean_wb - sd_wb, ymax = mean_wb + sd_wb, fill = dataset), 
              alpha = 0.2, color = NA) +
  labs(title = "Seasonal Cycle of Water Balance (1985-2005)",
       x = "Month", y = "Water Balance (mm)", 
       color = "Dataset", fill = "Dataset") +
  scale_x_continuous(breaks = 1:12, 
                     labels = month.abb) +
  theme_minimal() +
  scale_color_manual(values = c("observed" = "black", "access" = "red")) +
  scale_fill_manual(values = c("observed" = "black", "access" = "red"))

print(p5)

# 6. Statistical comparison
# Calculate correlation between datasets
obs_values <- values(observed_wb)
access_values <- values(access_wb)

# Remove rows where either dataset has NA
complete_cases <- complete.cases(obs_values, access_values)
obs_clean <- obs_values[complete_cases, ]
access_clean <- access_values[complete_cases, ]

# Calculate correlations for each time step
correlations <- sapply(1:ncol(obs_clean), function(i) {
  cor(obs_clean[, i], access_clean[, i], use = "complete.obs")
})

print(paste("Mean temporal correlation:", round(mean(correlations, na.rm = TRUE), 3)))
print(paste("Min temporal correlation:", round(min(correlations, na.rm = TRUE), 3)))
print(paste("Max temporal correlation:", round(max(correlations, na.rm = TRUE), 3)))

# 7. Scatter plot comparison (sample of pixels)
# Sample 1000 random pixels for visualization
sample_indices <- sample(nrow(obs_clean), min(1000, nrow(obs_clean)))

scatter_data <- data.frame(
  observed = as.vector(obs_clean[sample_indices, ]),
  access = as.vector(access_clean[sample_indices, ])
)

p6 <- ggplot(scatter_data, aes(x = observed, y = access)) +
  geom_point(alpha = 0.3, size = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Scatter Plot: ACCESS vs Observed Water Balance",
       x = "Observed Water Balance (mm)", 
       y = "ACCESS Water Balance (mm)") +
  theme_minimal() +
  coord_equal()

print(p6)

# Calculate R-squared
lm_result <- lm(access ~ observed, data = scatter_data)
r_squared <- summary(lm_result)$r.squared
print(paste("R-squared:", round(r_squared, 3)))

# 8. Summary assessment
cat("\n=== WATER BALANCE COMPARISON SUMMARY ===\n")
cat("1. Check if mean values are similar\n")
cat("2. Look at bias map for spatial patterns\n") 
cat("3. Examine time series for temporal alignment\n")
cat("4. Review seasonal cycles for consistency\n")
cat("5. Consider correlation values (>0.7 generally good)\n")
cat("6. Assess scatter plot linearity and R-squared\n")
cat("\nIf these comparisons show good agreement, you can proceed\n")
cat("directly to SPEI calculation without additional bias correction.\n")
