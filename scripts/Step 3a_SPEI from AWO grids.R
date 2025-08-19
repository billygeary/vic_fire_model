 # Make SPEI from observed silo data
library(terra)
library(R.utils)
library(SPEI)
library(SpatIndex)


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


