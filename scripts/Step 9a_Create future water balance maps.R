
# Script for preparing rainfall and evapotranspiration data
# Calculating Water Balance
# Bias Correction of Climate Model Data

library(terra)
library(R.utils)
library(SPEI)
library(SpatIndex)

####################################
#### 1) MAKE SPEI FROM AWO DATA ####
####################################

# Make SPEI from observed AGCD data

rainfall_files <- list.files(file.path("F:/agcd_climate_data/precip"), 
                             pattern = "\\.nc$", 
                             full.names = TRUE)

rainfall_files <- rainfall_files[grepl("r005", rainfall_files)]

et_files <- list.files(file.path("F:/awo_climate_data/etot"), 
                       pattern = "\\.nc$", 
                       full.names = TRUE)


baserast = rast("F:/vic_fire_mapping/output_data/prediction_stacks/baseline_observed_predstack.tif")

################################
#### CALCULATE SPEI RASTERS ####
################################

calculate_spei_raster <- function(rainfall_files, et_files, output_dir, temp_directory = temp_dir) {
  
  # Read raster stacks
  cat("Reading rainfall rasters...\n")
  rainfall_stack <- rast(rainfall_files)
  cat("Reading evapotranspiration rasters...\n")
  et_stack <- rast(et_files)
  
  # Make ET monthly
  # Extract dates from layer names if they are like "X2020.01.01"
  layer_dates <- terra::time(et_stack)
  month_index <- format(layer_dates, "%Y-%m")  # e.g., "2020-01", "2020-02"
  # For monthly **total** evapotranspiration
  monthly_et_sum <- tapp(et_stack, index = month_index, fun = sum, na.rm = TRUE)
  et_stack <- monthly_et_sum
  
  rainfall_stack <- project(rainfall_stack, et_stack) # Get the rasters to line up
  
  # Calculate SPEI
  cat("Calculating SPEI \n")
  
  water_balance = rainfall_stack - et_stack
  water_balance = project(water_balance, y = crs(baserast))
  water_balance = crop(water_balance, baserast)
  
  month_index <- format(time(rainfall_stack), "%Y-%m")
  time(water_balance) <- as.Date(paste0(month_index, "-01"))
  
  writeRaster(water_balance, filename = "F:/agcd_climate_data/water_balance_obs_vic.tif", overwrite=TRUE)
  
  spei_stack <- terra::app(water_balance, fun = function(x,  scale = 12, na.rm = TRUE, ...) {
    res <- tryCatch({
      ts_data <- ts(x, start = c(1980, 1), frequency = 12)
      spei_out <- SPEI::spei(ts_data, scale = 12, 
                             ref.start = c(1981, 1), ref.end = c(2005, 12), 
                             na.rm = TRUE, verbose = FALSE, ...)
      as.numeric(spei_out$fitted)
    }, error = function(e) rep(NA, length(x)))
    res
  })
  
  spei24_stack <- terra::app(water_balance, fun = function(x,  scale = 24, na.rm = TRUE, ...) {
    res <- tryCatch({
      ts_data <- ts(x, start = c(1980, 1), frequency = 12)
      
      spei_out <- SPEI::spei(ts_data, scale = 24, 
                             ref.start = c(1981, 1), ref.end = c(2005, 12), 
                             na.rm = TRUE, verbose = FALSE, ...)
      as.numeric(spei_out$fitted)
    }, error = function(e) rep(NA, length(x)))
    res
  })
  
  
  # Copy time information if available
  if (!is.null(time(rainfall_stack))) {
    time(spei_stack) <- time(rainfall_stack)
  }
  # Copy time information if available
  if (!is.null(time(rainfall_stack))) {
    time(spei24_stack) <- time(rainfall_stack)
  }
  
  cat("Saving results...\n")
  
  # Optionally save individual layers
  for (i in 1:nlyr(spei_stack)) {
    t = time(spei_stack[[i]])
    writeRaster(spei_stack[[i]], 
                filename = file.path(output_dir, paste0("spei_12m_",t, ".tif")),
                overwrite = TRUE)
  }
  
  # Optionally save individual layers
  for (i in 1:nlyr(spei24_stack)) {
    t = time(spei24_stack[[i]])
    writeRaster(spei24_stack[[i]], 
                filename = file.path(output_dir, paste0("spei_24m_",t, ".tif")),
                overwrite = TRUE)
  }
  
  
  cat("SPEI calculation complete!\n")
  cat(paste("Results saved to:", output_dir, "\n"))
  
  return(spei_stack)
}


calculate_spei_raster(rainfall_files, et_files, output_dir = "F:/awo_climate_data/spei_output", temp_directory = "F:/silo_climate_data/temp")

####################################################
#### 2_ Calculate Water Balance - Modelled Data ####
####################################################

# Future Climate layers
future.climate = "F:/future_predictions"
climate.model = "ACCESS1-0 RCP85"

temp_dir = "F:/vic_fire_mapping/terra_temp"

all_files <- list.files(file.path(future.climate, climate.model), 
                        pattern = "\\.nc\\.gz$", 
                        full.names = TRUE)

baserast = rast("F:/vic_fire_mapping/output_data/prediction_stacks/baseline_observed_predstack.tif")

calculate_water_balance <- function(rainfall_files, et_files, output_dir = file.path(future.climate, climate.model, "water_balance"), temp_directory = temp_dir) {
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Create temp directory if it doesn't exist
  if (!dir.exists(temp_directory)) {
    dir.create(temp_directory, recursive = TRUE)
  }
  
  # Decompress rainfall files if needed
  cat("Processing rainfall files...\n")
  temp_rainfall_files <- character(length(rainfall_files))
  for(i in seq_along(rainfall_files)) {
    if (grepl("\\.gz$", rainfall_files[i])) {
      temp_rainfall_files[i] <- file.path(temp_directory, 
                                          paste0(tools::file_path_sans_ext(basename(rainfall_files[i]), compression = TRUE), "_temp.nc"))
      cat("Decompressing:", basename(rainfall_files[i]), "\n")
      R.utils::gunzip(rainfall_files[i], destname = temp_rainfall_files[i], remove = FALSE)
    } else {
      temp_rainfall_files[i] <- rainfall_files[i]
    }
  }
  
  # Decompress ET files if needed
  cat("Processing ET files...\n")
  temp_et_files <- character(length(et_files))
  for(i in seq_along(et_files)) {
    if (grepl("\\.gz$", et_files[i])) {
      temp_et_files[i] <- file.path(temp_directory, 
                                    paste0(tools::file_path_sans_ext(basename(et_files[i]), compression = TRUE), "_temp.nc"))
      cat("Decompressing:", basename(et_files[i]), "\n")
      R.utils::gunzip(et_files[i], destname = temp_et_files[i], remove = FALSE)
    } else {
      temp_et_files[i] <- et_files[i]
    }
  }
  
  # Read raster stacks
  cat("Reading rainfall rasters...\n")
  rainfall_stack <- rast(temp_rainfall_files)
  rainfall_stack[rainfall_stack<0]<-NA
  cat("Reading evapotranspiration rasters...\n")
  et_stack <- rast(temp_et_files)
  et_stack[et_stack>2000]<-NA
  
  # Check that both stacks have the same dimensions and extent
  if (!identical(dim(rainfall_stack), dim(et_stack))) {
    stop("Rainfall and ET rasters must have the same dimensions")
  }
  
  # Set meaningful layer names based on the file dates
  if (length(rainfall_files) > 1) {
    # Extract dates from rainfall filenames (no underscore after date)
    rr_dates <- regmatches(rainfall_files, regexpr("rr_(\\d{6})ACCESS1-0", rainfall_files))
    rr_dates <- gsub("rr_|ACCESS1-0", "", rr_dates)
    
    # Extract dates from ET filenames (with underscore after date)  
    et_dates <- regmatches(et_files, regexpr("etot_(\\d{6})_ACCESS1-0", et_files))
    et_dates <- gsub("etot_|_ACCESS1-0", "", et_dates)
    
    # Format dates nicely (YYYYMM to YYYY-MM)
    rr_formatted_dates <- paste0(substr(rr_dates, 1, 4), "-", substr(rr_dates, 5, 6))
    et_formatted_dates <- paste0(substr(et_dates, 1, 4), "-", substr(et_dates, 5, 6))
    
    names(rainfall_stack) <- paste0("rainfall_", rr_formatted_dates)
    names(et_stack) <- paste0("etot_", et_formatted_dates)
  }
  
  terra::time(rainfall_stack) <- as.Date(paste0(rr_formatted_dates, "-01"), format = "%Y-%m-%d")
  terra::time(et_stack) <- as.Date(paste0(et_formatted_dates, "-01"), format = "%Y-%m-%d")
  
  # Calculate SPEI
  cat("Calculating Water Balance \n")
  
  water_balance = rainfall_stack - et_stack
  water_balance = project(water_balance, y = crs(baserast))
  water_balance = crop(water_balance, baserast)
  
  # Save
  cat("saving data \n")
  writeRaster(water_balance, "F:/future_predictions/ACCESS1-0 RCP85/processed/water_balance_access.tif")
  
  # Clean up temporary files
  cat("Cleaning up temporary files...\n")
  temp_files_to_remove <- c(temp_rainfall_files[temp_rainfall_files != rainfall_files],
                            temp_et_files[temp_et_files != et_files])
  if (length(temp_files_to_remove) > 0) {
    unlink(temp_files_to_remove)
  }
  
  cat("calculation complete!\n")
}

# Get list of files with your specific naming patterns
rainfall_files <- all_files[grepl("rr",all_files)]
et_files <- all_files[grepl("etot",all_files)]

# Sort files chronologically based on the date in filename (using appropriate pattern for each)
rainfall_files <- sort_files_by_date(rainfall_files, "rr")
et_files <- sort_files_by_date(et_files, "etot")

# Print file information for verification
cat("Found", length(rainfall_files), "rainfall files\n")
cat("Found", length(et_files), "ET files\n")

if (length(rainfall_files) > 0) {
  cat("First rainfall file:", basename(rainfall_files[1]), "\n")
  cat("Last rainfall file:", basename(rainfall_files[length(rainfall_files)]), "\n")
}

if (length(et_files) > 0) {
  cat("First ET file:", basename(et_files[1]), "\n")
  cat("Last ET file:", basename(et_files[length(et_files)]), "\n")
}

# Check that we have matching numbers of files
if (length(rainfall_files) != length(et_files)) {
  stop("Number of rainfall files (", length(rainfall_files), 
       ") does not match number of ET files (", length(et_files), ")")
}

# Calculate Water Balance
calculate_water_balance(rainfall_files, et_files)

# #########################
# #### BIAS CORRECTION ####
# #########################

# Simplified SPEI Quantile Mapping Bias Correction using terra::app()

library(terra)
library(qmap)  # install with: install.packages("qmap")

# Simplified function to perform quantile mapping bias correction
wb_bias_correction <- function(model_wb_stack,
                                 observed_wb_stack,
                                 output_dir,
                                 name,
                                 method = "QUANT") {

  # Create output directory
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Resample observed data to match model grid if needed
  if (!identical(ext(model_wb_stack), ext(observed_wb_stack)) ||
      !identical(res(model_wb_stack), res(observed_wb_stack))) {
    cat("Resampling observed data to match model grid...\n")
    observed_wb_stack <- terra::project(observed_wb_stack, model_wb_stack, method = "bilinear")
  }

  cat("Performing quantile mapping bias correction...\n")

  # Get number of time steps
  n_time <- nlyr(model_wb_stack)

  # Define the correction function to apply to each pixel
  correct_pixel <- function(values) {
    # values is a vector with length = 2 * n_time
    # First n_time values are model, next n_time values are observed
    model_ts <- values[1:n_time]
    observed_ts <- values[(n_time + 1):(2 * n_time)]

    # Skip if insufficient data
    if (sum(!is.na(model_ts)) < 10 || sum(!is.na(observed_ts)) < 10) {
      return(model_ts)
    }

    tryCatch({
      # Fit and apply quantile mapping
      qm_fit <- fitQmap(obs = observed_ts, mod = model_ts, method = method,
                        wet.day = FALSE, qstep = 0.01)
      return(doQmap(model_ts, qm_fit))
    }, error = function(e) {
      message(paste("Error", e))
      return(model_ts)  # Return original if correction fails
    })
  }

  # Combine model and observed stacks
  combined_stack <- c(model_wb_stack, observed_wb_stack)

  # Apply correction function using terra::app()
  corrected_stack <- app(combined_stack, fun = correct_pixel)

  # Set proper names
  names(corrected_stack) <- paste0("WB_corrected_", time(model_wb_stack))

  # Copy time information if available
  if (!is.null(time(model_wb_stack))) {
    time(corrected_stack) <- time(model_wb_stack)
  }

  cat("Saving bias-corrected WB data...\n")

  # Save the corrected stack
  writeRaster(corrected_stack,
              filename = file.path(output_dir, paste0(name, "_bias_corrected.tif")),
              overwrite = TRUE)

  cat("Bias correction complete!\n")
  return(corrected_stack)
}


# Simplified function to apply correction to future projections
apply_wb_correction <- function(future_wb_stack,
                                  reference_model_stack,
                                  reference_observed_stack,
                                  output_dir,
                                  name,
                                  method = "QUANT") {

  cat("Applying bias correction to future projections...\n")

  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Resample if needed - use resample instead of project for matching grids
  if (!identical(ext(reference_model_stack), ext(reference_observed_stack)) ||
      !identical(res(reference_model_stack), res(reference_observed_stack))) {
    cat("Resampling observed data to match model grid...\n")
    reference_observed_stack <- terra::project(reference_observed_stack, reference_model_stack, method = "bilinear")
  }

  # Also ensure future stack matches reference grid
  if (!identical(ext(future_wb_stack), ext(reference_model_stack)) ||
      !identical(res(future_wb_stack), res(reference_model_stack))) {
    cat("Resampling future data to match reference grid...\n")
    future_wb_stack <- terra::project(future_wb_stack, reference_model_stack, method = "bilinear")
  }

  # Get dimensions
  n_future <- nlyr(future_wb_stack)
  n_ref <- nlyr(reference_model_stack)

  cat("Processing", n_future, "future layers with", n_ref, "reference layers...\n")

  # Define correction function - values comes as a VECTOR, not matrix
  correct_future_pixel <- function(values) {
    # values is a vector with structure:
    # [future_layer1, future_layer2, ..., ref_model_layer1, ref_model_layer2, ..., ref_obs_layer1, ref_obs_layer2, ...]

    # Extract time series as vectors
    future_ts <- values[1:n_future]
    ref_model_ts <- values[(n_future + 1):(n_future + n_ref)]
    ref_obs_ts <- values[(n_future + n_ref + 1):(n_future + 2*n_ref)]

    # Check for sufficient data in reference period
    if (sum(!is.na(ref_model_ts)) < 10 || sum(!is.na(ref_obs_ts)) < 10) {
      return(future_ts)  # Return original future values if insufficient reference data
    }

    # Skip if future data is all NA
    if (all(is.na(future_ts))) {
      return(future_ts)
    }

    tryCatch({
      # Fit correction using reference period
      qm_fit <- fitQmap(obs = ref_obs_ts, mod = ref_model_ts, method = method,
                        wet.day = FALSE, qstep = 0.01)

      # Apply correction to future data
      corrected_future <- doQmap(future_ts, qm_fit)
      return(corrected_future)

    }, error = function(e) {
      # If correction fails, return original future values
      warning(paste("Correction failed for pixel:", e$message))
      return(future_ts)
    })
  }

  # Combine all stacks in the correct order
  combined_stack <- c(future_wb_stack, reference_model_stack, reference_observed_stack)

  cat("Applying bias correction...\n")

  # Apply correction using terra::app()
  corrected_future <- app(combined_stack, fun = correct_future_pixel)

  # Set proper names
  names(corrected_future) <- paste0("wb_future_corrected_", 1:n_future)

  # Copy time information if available
  if (!is.null(time(future_wb_stack))) {
    time(corrected_future) <- time(future_wb_stack)
  }

  # Create output filename
  output_filename <- file.path(output_dir, paste0(name, "_20802099_future_bias_corrected.tif"))

  cat("Saving results to:", output_filename, "\n")

  # Save results
  writeRaster(corrected_future,
              filename = output_filename,
              overwrite = TRUE)

  cat("Future projection correction complete!\n")
  return(corrected_future)
}

# Load Water Balance data
wb_observed_stack = rast("F:/agcd_climate_data/water_balance_obs_vic.tif")
wb_observed_stack_reference = subset(wb_observed_stack,  
                                     time(wb_observed_stack) >= as.Date("1985-01-01") &
                                       time(wb_observed_stack) < as.Date("2006-01-01"))
  
wb_access_stack = rast("F:/future_predictions/ACCESS1-0 RCP85/processed/water_balance_access.tif")

wb_access_stack_reference = subset(wb_access_stack,  
                                   time(wb_access_stack) >= as.Date("1985-01-01") &
                                     time(wb_access_stack) < as.Date("2006-01-01"))

wb_access_stack_future =  subset(wb_access_stack,  
                              time(wb_access_stack) >= as.Date("2050-01-01"))

# Apply bias correction to modelled current data
corrected <- wb_bias_correction(wb_access_stack_reference,
                                  wb_observed_stack_reference,
                                  output_dir = "F:/future_predictions/ACCESS1-0 RCP85/wb_output",
                                  name = "water_balance",
                                  method = "QUANT")


# Apply bias correction to modelled future data
corrected_future <- apply_wb_correction(wb_access_stack_future,
                                          wb_access_stack_reference,
                                          wb_observed_stack_reference,
                                          "F:/future_predictions/ACCESS1-0 RCP85/wb_output",
                                          name = "water_balance",
                                          method = "QUANT")



