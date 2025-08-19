library(terra)
library(future.apply)

# Set up paths
burn_dir    <- "F:/vic_fire_mapping/fire_data"
mask_path   <- "F:/vic_fire_mapping/covariates/masked/masked_covariate_stack.tif"
output_dir  <- "F:/vic_fire_mapping/covariates/raw/tsf"
dir.create(output_dir, showWarnings = FALSE)

# Set up parallel plan
plan(multisession, workers = 3)

# TSF function
make_tsf <- function(yr) {
  library(terra)
  
  message("🔄 Processing TSF for year: ", yr)
  
  # Load template raster
  template <- rast("F:/vic_fire_mapping/covariates/masked/masked_covariate_stack.tif")[[1]]
  
  # Get relevant burn rasters (up to year)
  burn_files <- list.files("F:/vic_fire_mapping/fire_data", pattern = "burned_area_\\d{4}_75m.tif$", full.names = TRUE)
  get_year <- function(fname) as.numeric(gsub(".*burned_area_(\\d{4})_75m.tif", "\\1", fname))
  burn_years <- sapply(burn_files, get_year)
  burn_df <- data.frame(file = burn_files, year = burn_years)
  relevant <- burn_df[burn_df$year <= yr, ]
  if (nrow(relevant) == 0) return(NULL)
  
  # Create empty raster to hold most recent fire year
  fire_year_rast <- rast(template)
  values(fire_year_rast) <- NA
  
  for (i in seq_len(nrow(relevant))) {
    r <- rast(relevant$file[i])
    r <- resample(r, template, method = "near")
    burned <- r > 0
    burned[burned == 0] <- NA
    burned[burned == 1] <- relevant$year[i]
    fire_year_rast <- cover(burned, fire_year_rast)  # Use newest fire year per pixel
  }
  
  # Fill NAs with default fire year (1990)
  fire_year_rast[is.na(fire_year_rast)] <- 1900
  
  # Calculate TSF
  tsf <- yr - fire_year_rast
  names(tsf) <- paste0("tsf_", yr)
  
  # Mask and save
  tsf_masked <- mask(tsf, template)
  out_file <- file.path("F:/vic_fire_mapping/covariates/raw/tsf", paste0("tsf_", yr, ".tif"))
  writeRaster(tsf_masked, out_file, overwrite = TRUE)
  cat("✅ Saved:", out_file, "\n")
  return(out_file)
}

# Reset to sequential mode
plan(sequential)

# Run safely in sequence
for (yr in 1990:2025) {
  # Skip if already completed
  out_file <- file.path("F:/vic_fire_mapping/covariates/raw/tsf", paste0("tsf_", yr, ".tif"))
  if (file.exists(out_file)) {
    cat("⏭️ Skipping year", yr, "- already exists\n")
    next
  }
  
  try(make_tsf(yr), silent = FALSE)
  gc()  # Free memory after each year
}



