## plots and maps for methods powerpoint



library(terra)
library(ggplot2)
library(viridis)

# List and filter to 2009 fire maps
fire.maps <- list.files("F:/vic_fire_mapping/fire_data", pattern = ".tif$", full.names = TRUE)
fire.2009 <- fire.maps[grepl("2009", fire.maps)]

# Read in the first 2009 fire raster (or choose your target layer)
r1 <- rast(fire.2009[1])

# Set color scheme: 0 = yellow, 1 = red
col_scheme <- c("yellow", "red")

# Plot with custom legend and colors
plot(r1, col = col_scheme, breaks = c(-0.1, 0.5, 1.1), 
     main = "Fire History 2009", axes = FALSE, legend = FALSE)
legend("topright", legend = c("No Fire", "Fire"), fill = col_scheme, border = "black")

library(terra)
library(ozmaps)
library(sf)

library(terra)
library(ozmaps)
library(sf)

# Step 1: Load Victoria outline and raster CRS
vic_sf <- ozmaps::ozmap_states %>% subset(NAME == "Victoria")
vic_vect <- vect(vic_sf)

# Step 2: Filter rasters for 2009 and 2020
fire.maps <- list.files("F:/vic_fire_mapping/fire_data", pattern = ".tif$", full.names = TRUE)
fire.2009.path <- fire.maps[grepl("2009", fire.maps)][1]
fire.2020.path <- fire.maps[grepl("2020", fire.maps)][1]

# Step 3: Load rasters
r2009 <- rast(fire.2009.path)
r2020 <- rast(fire.2020.path)

# Step 4: Reproject Victoria boundary to match raster CRS
vic_proj <- project(vic_vect, crs(r2009))

# Step 5: Clip rasters to Victoria
r2009_vic <- mask(crop(r2009, vic_proj), vic_proj)
r2020_vic <- mask(crop(r2020, vic_proj), vic_proj)

# Step 6: Plot side by side with legend
col_scheme <- c("yellow", "red")

par(mfrow = c(1, 2), mar = c(2, 2, 2, 1))  # Side-by-side layout, tighter margins

plot(r2009_vic, col = col_scheme, breaks = c(-0.1, 0.5, 1.1),
     main = "Fire History 2009", axes = FALSE, legend = FALSE)

plot(r2020_vic, col = col_scheme, breaks = c(-0.1, 0.5, 1.1),
     main = "Fire History 2020", axes = FALSE, legend = FALSE)




##plot covariates
library(terra)
library(viridis)
library(ozmaps)
library(sf)

# Set directories
cov_dir <- "F:/vic_fire_mapping/covariates/masked"
output_dir <- "C:/Users/charliehart/The University of Melbourne/Billy Geary - vic_fire_model/documents/plots for powerpoint"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Load base stack
base_stack <- rast(file.path(cov_dir, "masked_static_covariate_stack.tif"))

# Load multi-year rasters
ffdi     <- rast(file.path(cov_dir, "ffdi95", list.files(file.path(cov_dir, "ffdi95"), pattern = ".tif$")))
thunder  <- rast(file.path(cov_dir, "thunderstorm", list.files(file.path(cov_dir, "thunderstorm"), pattern = ".tif$")))
spei     <- rast(file.path(cov_dir, "spei12", list.files(file.path(cov_dir, "spei12"), pattern = ".tif$")))

# Extract 2020 layers
ffdi_2020    <- ffdi[[grep("2020", names(ffdi))]]
thunder_2020 <- thunder[[grep("2020", names(thunder))]]
spei_2020    <- spei[[grep("2020", names(spei))]]

# Select static layers
selected_layers <- c("cly", "distance_roads", "bio5", "broad_refuges", "local_refuges")
static_layers <- base_stack[[selected_layers]]

# Combine
cov_stack_selected <- c(static_layers, ffdi_2020, thunder_2020, spei_2020)
names(cov_stack_selected) <- c(
  "cly", "distance_roads", "bio5",
  "broad_refuges", "local_refuges",
  "ffdi_95_days", "thunderstorm_days", "spei12_mean"
)

# Clip to Victoria
vic_sf <- ozmaps::ozmap_states %>% subset(NAME == "Victoria")
vic_vect <- vect(vic_sf)
vic_proj <- project(vic_vect, crs(cov_stack_selected))
cov_stack_clipped <- mask(crop(cov_stack_selected, vic_proj), vic_proj)

# Titles for plots
pretty_names <- c(
  cly = "Clay Content (%)",
  distance_roads = "Distance to Roads (m)",
  bio5 = "Max Temp of Warmest Month (°C)",
  ffdi_95_days = "FFDI > 25 (Days)",
  thunderstorm_days = "Thunderstorm Days",
  spei12_mean = "Mean 12-Month SPEI",
  broad_refuges = "Broad Refuges",
  local_refuges = "Local Refuges"
)

# Plot and save
for (i in 1:nlyr(cov_stack_clipped)) {
  layer_name <- names(cov_stack_clipped)[i]
  title <- pretty_names[[layer_name]]
  
  png(file.path(output_dir, paste0(layer_name, "_2020.png")), width = 1000, height = 800)
  plot(cov_stack_clipped[[i]],
       col = viridis(30),
       main = title,
       cex.main = 3.2,  # increase title size
       axes = FALSE,
       mar = c(2, 2, 2, 5))
  dev.off()
}







#model results - RAC model (needs updating to year specific RAC)

library(gbm)

# Load the final model with residual autocovariate
model_path <- "F:/vic_fire_mapping/output_data/lr_0.025_fire_model_RAC.RDS"
brt_full_rac <- readRDS(model_path)

# Print key results
print(brt_full_rac)
summary(brt_full_rac)  # Prints variable importance

library(ggplot2)

# Create a reordered factor for plotting
var_imp$var <- factor(var_imp$var, levels = rev(var_imp$var))

# Make the plot
ggplot(var_imp, aes(x = var, y = rel.inf)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # horizontal bars
  theme_minimal(base_size = 14) +
  labs(
    title = "Relative Importance of Predictors (BRT with RAC)",
    x = "Predictor",
    y = "Relative Importance (%)"
  ) +
  theme(
    axis.text.x = element_text(angle = 0),   # horizontal x-axis labels
    axis.title.x = element_text(margin = margin(t = 10)),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
  )








#pdp
# Set output folder
outdir <- "F:/vic_fire_mapping/output_data/pdp_plots"
dir.create(outdir, showWarnings = FALSE)

# List of key predictors
key_preds <- c("ffdi_95_days", "spei12_mean", "bio5", "local_refuges", "thunderstorm_days", "rac")

gbm::plot.gbm(brt_full_rac, "ffdi_95_days", type="response")
gbm::plot.gbm(brt_full_rac, "spei12_mean", type="response")
gbm::plot.gbm(brt_full_rac, "bio5", type="response")
gbm::plot.gbm(brt_full_rac, "local_refuges", type="response")
gbm::plot.gbm(brt_full_rac, "thunderstorm_days", type="response")
gbm::plot.gbm(brt_full_rac, "rac", type="response")



library(gbm)
library(ggplot2)

# Predictor labels
pdp_vars <- c(
  ffdi_95_days = "FFDI > 25 (Days)",
  spei12_mean = "12-Month SPEI",
  bio5 = "Max Temp of Warmest Month (°C)",
  local_refuges = "Local Refuges",
  thunderstorm_days = "Thunderstorm Days",
  rac = "Residual Autocovariate (RAC)"
)

# Output directory
output_dir <- "C:/Users/charliehart/The University of Melbourne/Billy Geary - vic_fire_model/documents/plots for powerpoint"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Loop through predictors
for (var in names(pdp_vars)) {
  # Extract PDP data
  pdp_df <- gbm::plot.gbm(brt_full_rac, var, type = "response", return.grid = TRUE)
  names(pdp_df) <- c("x", "predicted_fire_prob")
  
  # Build ggplot
  p <- ggplot(pdp_df, aes(x = x, y = predicted_fire_prob)) +
    geom_line(color = "firebrick", linewidth = 1.3) +
    theme_minimal(base_size = 16) +
    labs(
      title = paste("Partial Dependence:", pdp_vars[[var]]),
      x = pdp_vars[[var]],
      y = "Predicted Fire Probability"
    ) +
    theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 14)
    )
  
  # Save plot
  ggsave(
    filename = file.path(output_dir, paste0("pdp_", var, ".png")),
    plot = p,
    width = 7, height = 5, dpi = 300
  )
}






#model results
# Print the model object to see its structure
brt_full_rac

# Extract cross-validation statistics
auc <- brt_full_rac$cv.statistics$discrimination.mean
deviance <- brt_full_rac$cv.statistics$deviance.mean
corr <- brt_full_rac$cv.statistics$correlation.mean

# Tree info
n_trees <- brt_full_rac$n.trees
lr <- brt_full_rac$learning.rate
tc <- brt_full_rac$interaction.depth

# Print
cat("\nModel Summary (BRT with RAC):\n")
cat("------------------------------\n")
cat("Learning Rate:            ", lr, "\n")
cat("Tree Complexity:          ", tc, "\n")
cat("Number of Trees:          ", n_trees, "\n\n")

cat("Cross-Validation AUC:     ", round(auc, 3), "\n")
cat("CV Correlation:           ", round(corr, 3), "\n")
cat("CV Deviance:              ", round(deviance, 3), "\n")

