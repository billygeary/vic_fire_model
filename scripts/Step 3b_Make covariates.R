
# Part A: Script for processing the monthly FFDI data into fire season data

ffdi.path = "F:/ffdi/FFDI_1980-2024"

sum_summer_rasters <- function(folder, year, pattern = "FFDI_") {
  # Define the months of interest
  prev_year <- year - 1
  months <- c(sprintf("%d11", prev_year), sprintf("%d12", prev_year), 
              sprintf("%d01", year), sprintf("%d02", year), sprintf("%d03", year))
  
  # List all files in the folder
  all_files <- list.files(folder, pattern = "\\.nc$", full.names = TRUE)
  
  # Filter files matching the required summer months
  summer_files <- grep(paste0(pattern, "(", paste0(months, collapse = "|"), ")\\.nc"), all_files, value = TRUE)
  
  if (length(summer_files) == 0) {
    warning(paste("No matching files found for summer of", year))
    return(NULL)
  }
  
  # Read rasters and sum them
  summer_rasters <- rast(summer_files)
  summer_sum <- sum(summer_rasters, na.rm = TRUE)
  
  # Save the output raster
  output_file <- file.path(folder, sprintf("FFDI_summer_sum_%d.nc", year))
  writeRaster(summer_sum, output_file, overwrite = TRUE)
  
  return(output_file)
}

years <- 1980:2024
output_files <- lapply(years, function(y) sum_summer_rasters(ffdi.path, y))

ffdi.path = "F:/ffdi/FFDI_1980-2024"

mean_summer_rasters <- function(folder, year, pattern = "FFDI_") {
  # Define the months of interest
  prev_year <- year - 1
  months <- c(sprintf("%d11", prev_year), sprintf("%d12", prev_year), 
              sprintf("%d01", year), sprintf("%d02", year), sprintf("%d03", year))
  
  # List all files in the folder
  all_files <- list.files(folder, pattern = "\\.nc$", full.names = TRUE)
  
  # Filter files matching the required summer months
  summer_files <- grep(paste0(pattern, "(", paste0(months, collapse = "|"), ")\\.nc"), all_files, value = TRUE)
  
  if (length(summer_files) == 0) {
    warning(paste("No matching files found for summer of", year))
    return(NULL)
  }
  
  # Read rasters and sum them
  summer_rasters <- rast(summer_files)
  summer_sum <- mean(summer_rasters, na.rm = TRUE)
  
  # Save the output raster
  output_file <- file.path(folder, sprintf("FFDI_summer_mean_%d.nc", year))
  writeRaster(summer_sum, output_file, overwrite = TRUE)
  
}

years <- 1980:2024
lapply(years, function(y) mean_summer_rasters(ffdi.path, y))


##### KBDI

kbdi.path = "F:/kbdi/KBDI_95pc_1980-2024"

sum_summer_rasters <- function(folder, year, pattern = "KBDI_gt95perc_") {
  # Define the months of interest
  prev_year <- year - 1
  months <- c(sprintf("%d11", prev_year), sprintf("%d12", prev_year), 
              sprintf("%d01", year), sprintf("%d02", year), sprintf("%d03", year))
  
  # List all files in the folder
  all_files <- list.files(folder, pattern = "\\.nc$", full.names = TRUE)
  
  # Filter files matching the required summer months
  summer_files <- grep(paste0(pattern, "(", paste0(months, collapse = "|"), ")\\.nc"), all_files, value = TRUE)
  
  if (length(summer_files) == 0) {
    warning(paste("No matching files found for summer of", year))
    return(NULL)
  }
  
  # Read rasters and sum them
  summer_rasters <- rast(summer_files)
  summer_sum <- sum(summer_rasters, na.rm = TRUE)
  
  # Save the output raster
  output_file <- file.path(folder, sprintf("KBDI_summer_sum_%d.nc", year))
  writeRaster(summer_sum, output_file, overwrite = TRUE)
  
  return(output_file)
}

years <- 1980:2024
output_files <- lapply(years, function(y) sum_summer_rasters(kbdi.path, y))


ffdi.path = "F:/kbdi/KBDI_1980-2024"

mean_summer_rasters <- function(folder, year, pattern = "KBDI_") {
  # Define the months of interest
  prev_year <- year - 1
  months <- c(sprintf("%d11", prev_year), sprintf("%d12", prev_year), 
              sprintf("%d01", year), sprintf("%d02", year), sprintf("%d03", year))
  
  # List all files in the folder
  all_files <- list.files(folder, pattern = "\\.nc$", full.names = TRUE)
  
  # Filter files matching the required summer months
  summer_files <- grep(paste0(pattern, "(", paste0(months, collapse = "|"), ")\\.nc"), all_files, value = TRUE)
  
  if (length(summer_files) == 0) {
    warning(paste("No matching files found for summer of", year))
    return(NULL)
  }
  
  # Read rasters and sum them
  summer_rasters <- rast(summer_files)
  summer_sum <- mean(summer_rasters, na.rm = TRUE)
  
  # Save the output raster
  output_file <- file.path(folder, sprintf("KBDI_summer_mean_%d.nc", year))
  writeRaster(summer_sum, output_file, overwrite = TRUE)
  
}

years <- 1980:2024
lapply(years, function(y) mean_summer_rasters(ffdi.path, y))




## THUNDERSTORM DATA
ts.path = "F:/thunderstorms/thunderstorm_occurrence_1980_2023"
ts.files = list.files(ts.path, full = T)

is_leap_year <- function(year) {
  return((year %% 4 == 0 & year %% 100 != 0) | (year %% 400 == 0))
}

sum_summer_thunderstorm <- function(folder, year, pattern = "BTE_ERA5_") {
  prev_year <- year - 1  # Previous year needed for Nov-Dec
  
  # Identify if leap years
  leap_current <- is_leap_year(year)
  leap_prev <- is_leap_year(prev_year)
  
  # Expected number of days in each year
  days_prev <- 30 + 31  # November + December = 61 days
  days_curr <- 31 + ifelse(leap_current, 29, 28) + 31  # Jan + Feb + Mar
  
  # Expected number of layers
  layers_per_day <- 4
  total_layers_prev <- days_prev * layers_per_day  # Layers for Nov-Dec
  total_layers_curr <- days_curr * layers_per_day  # Layers for Jan-Mar
  
  # Find the files for both years
  file_current <- list.files(folder, pattern = paste0(pattern, year, "\\.nc$"), full.names = TRUE)
  file_prev <- list.files(folder, pattern = paste0(pattern, prev_year, "\\.nc$"), full.names = TRUE)
  
  if (length(file_current) == 0 || length(file_prev) == 0) {
    warning(paste("Missing files for year", year, "or", prev_year))
    return(NULL)
  }
  
  # Load rasters
  r_current <- rast(file_current)
  r_prev <- rast(file_prev)
  
  # Extract layers corresponding to summer months
  layers_prev <- 1:total_layers_prev
  layers_curr <- (total_layers_prev + 1):(total_layers_prev + total_layers_curr)
  
  summer_prev <- r_prev[[layers_prev]]
  summer_curr <- r_current[[layers_curr]]
  
  # Merge Nov-Mar layers
  summer_rasters <- c(summer_prev, summer_curr)
  
  # Convert to daily presence/absence (max over 4 layers per day)
  num_days <- nlyr(summer_rasters) / layers_per_day
  daily_presence <- rast(lapply(seq_len(num_days), function(d) {
    max(summer_rasters[[((d - 1) * layers_per_day + 1):(d * layers_per_day)]], na.rm = TRUE)
  }))
  
  # Sum across summer days
  summer_sum <- sum(daily_presence, na.rm = TRUE)
  
  # Save output
  output_file <- file.path(folder, sprintf("thunderstorm_summer_days_%d.nc", year))
  writeRaster(summer_sum, output_file, overwrite = TRUE)

}

years <- 1980:2023
lapply(years, function(y) sum_summer_thunderstorm(ts.path, y, pattern = "BTE_ERA5_"))



## SPEI Data
average_summer_spei <- function(spei_raster, year, start_year = 1980, output_folder = ".") {
  # Define the previous year
  prev_year <- year - 1
  
  # Compute the layer indices based on the start year
  start_layer <- (prev_year - start_year) * 12  # Layers before prev_year starts
  layers <- c(start_layer + 11, start_layer + 12,  # Nov-Dec of prev_year
              start_layer + 13, start_layer + 14, start_layer + 15)  # Jan-Mar of year
  
  # Check if layer indices are valid
  if (max(layers) > nlyr(spei_raster)) {
    warning(paste("Requested year", year, "exceeds available layers in the raster."))
    return(NULL)
  }
  
  # Extract summer months
  summer_layers <- spei_raster[[layers]]
  
  # Compute mean SPEI
  summer_mean <- mean(summer_layers, na.rm = TRUE)
  
  # Save the result
  output_file <- file.path(output_folder, sprintf("SPEI12_summer_avg_%d.tif", year))
  writeRaster(summer_mean, output_file, overwrite = TRUE)
}

# Example usage:
# Load the SPEI raster (assuming it's already loaded as a `terra` raster stack)
spei.path = "F:/spei"
spei.files = list.files(spei.path, pattern=".nc", full=T)
spei.files = spei.files[grep("12", spei.files)]
a_spei_raster <- rast(spei.files)  # Update with actual file path

years <- 1981:2024
lapply(years, function(y) average_summer_spei(spei_raster, y, start_year = 1901, output_folder = spei.path))



## 12 Month SPEI from AWO

# Example usage:
# Load the SPEI raster (assuming it's already loaded as a `terra` raster stack)
average_summer_spei <- function(spei_raster, year, start_year = 1980, output_folder = ".") {
  # Define the previous year
  prev_year <- year - 1
  
  # Compute the layer indices based on the start year
  start_layer <- (prev_year - start_year) * 12  # Layers before prev_year starts
  layers <- c(start_layer + 11, start_layer + 12,  # Nov-Dec of prev_year
              start_layer + 13, start_layer + 14, start_layer + 15)  # Jan-Mar of year
  
  # Check if layer indices are valid
  if (max(layers) > nlyr(spei_raster)) {
    warning(paste("Requested year", year, "exceeds available layers in the raster."))
    return(NULL)
  }
  
  # Extract summer months
  summer_layers <- spei_raster[[layers]]
  
  # Compute mean SPEI
  summer_mean <- mean(summer_layers, na.rm = TRUE)
  
  # Save the result
  output_file <- file.path(output_folder, sprintf("SPEI12_summer_avg_%d.tif", year))
  writeRaster(summer_mean, output_file, overwrite = TRUE)
}

spei.path = "F:/awo_climate_data/spei_output"
spei.files = list.files(spei.path, pattern=".tif", full=T)
spei.files = spei.files[grep("_12", spei.files)]

spei_raster <- rast(spei.files)  # Update with actual file path

years <- 1981:2024
lapply(years, function(y) average_summer_spei(spei_raster, y, start_year = 1980, output_folder = spei.path))


## 24 Month SPEI from AWO

average_summer_spei <- function(spei_raster, year, start_year = 1901, output_folder = ".") {
  # Define the previous year
  prev_year <- year - 1
  
  # Compute the layer indices based on the start year
  start_layer <- (prev_year - start_year) * 12  # Layers before prev_year starts
  layers <- c(start_layer + 11, start_layer + 12,  # Nov-Dec of prev_year
              start_layer + 13, start_layer + 14, start_layer + 15)  # Jan-Mar of year
  
  # Check if layer indices are valid
  if (max(layers) > nlyr(spei_raster)) {
    warning(paste("Requested year", year, "exceeds available layers in the raster."))
    return(NULL)
  }
  
  # Extract summer months
  summer_layers <- spei_raster[[layers]]
  
  # Compute mean SPEI
  summer_mean <- mean(summer_layers, na.rm = TRUE)
  
  # Save the result
  output_file <- file.path(output_folder, sprintf("SPEI24_summer_avg_%d.tif", year))
  writeRaster(summer_mean, output_file, overwrite = TRUE)
}

spei.path = "F:/awo_climate_data/spei_output"
spei.files = list.files(spei.path, pattern=".tif", full=T)
spei.files = spei.files[grep("_24", spei.files)]
spei_raster <- rast(spei.files)  # Update with actual file path

years <- 1982:2024
lapply(years, function(y) average_summer_spei(spei_raster, y, start_year = 1980, output_folder = spei.path))






#Part B: Create time since fire rasters from fire history data


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
for (yr in 1980:2025) {
  # Skip if already completed
  out_file <- file.path("F:/vic_fire_mapping/covariates/raw/tsf", paste0("tsf_", yr, ".tif"))
  if (file.exists(out_file)) {
    cat("⏭️ Skipping year", yr, "- already exists\n")
    next
  }
  
  try(make_tsf(yr), silent = FALSE)
  gc()  # Free memory after each year
}














