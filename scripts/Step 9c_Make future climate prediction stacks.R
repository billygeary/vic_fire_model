
library(terra)
library(R.utils)

# Future Climate layers
future.climate = "F:/future_predictions"
climate.model = "ACCESS1-0 RCP85"

temp_dir = "F:/vic_fire_mapping/terra_temp"

all_files <- list.files(file.path(future.climate, climate.model), 
                        pattern = "\\.nc\\.gz$", 
                        full.names = TRUE)

################################################
####  PREPARE TIME VARYING COVARIATE INPUTS ####
################################################

# spei12_mean, spei24_mean
# ffdi_95_days, 
# kbdi_95_days
# thunderstorm_days

#### FFDI ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ffdi.files <- all_files[grepl("FFDI", all_files, ignore.case = TRUE)]
ffdi95.files <- ffdi.files[grepl("FFDI_", ffdi.files, ignore.case = TRUE)] 
ffdi.files <-  ffdi.files[!grepl("FFDI_", ffdi.files, ignore.case = TRUE)] 

mean_summer_rasters_gz <- function(gz_folder, year, temp_directory = temp_dir, pattern = "FFDI", output_folder = NULL) {
  # Define the months of interest
  prev_year <- year - 1
  mindate <- as.Date(paste0("01/11/",prev_year), format = c("%d/%m/%Y"))
  maxdate <- as.Date(paste0("31/03/",year),format = c("%d/%m/%Y"))

  # List all .gz files in the folder
  all_gz_files <- list.files(gz_folder, pattern = "\\.nc\\.gz$", full.names = TRUE)
  
  # Filter files matching the required summer months
  summer_gz_files <- c(grep(paste0(pattern,prev_year, ".nc"), all_gz_files, value = TRUE),
                       grep(paste0(pattern,year, ".nc"), all_gz_files, value = TRUE))
  
  if (length(summer_gz_files) == 0) {
    warning(paste("No matching files found for summer of", year))
    return(NULL)
  }
  
  cat("Processing", length(summer_gz_files), "files for summer", year, "\n")
  
  # Decompress files to temp directory
  temp_nc_files <- character(length(summer_gz_files))
  
  for(i in seq_along(summer_gz_files)) {
    # Create temp filename
    temp_nc_files[i] <- file.path(temp_directory, 
                                  paste0(tools::file_path_sans_ext(basename(summer_gz_files[i]), compression = TRUE), "_temp.nc"))
    
    # Decompress
    cat("Decompressing:", basename(summer_gz_files[i]), "\n")
    R.utils::gunzip(summer_gz_files[i], destname = temp_nc_files[i], remove = FALSE)
  }
  
  # Read rasters and sum them
  cat("Reading and averaging rasters...\n")
  prev_summer_rasters <- rast(temp_nc_files[1])
  prev_summer_rasters <- subset(prev_summer_rasters, time(prev_summer_rasters) > mindate)
  
  post_summer_rasters <- rast(temp_nc_files[2])
  post_summer_rasters <- subset(post_summer_rasters, time(prev_summer_rasters) < maxdate)
  
  summer_rasters <- c(prev_summer_rasters, post_summer_rasters)
  summer_mean <- mean(summer_rasters)
  summer_mean[summer_mean < 0] <- NA
  
  # Define output location
  if(is.null(output_folder)) {
    output_folder <- gz_folder
  }
  output_file <- file.path(output_folder, sprintf("FFDI_summer_mean_%d.nc", year))
  
  # Save the output raster
  cat("Saving result to:", output_file, "\n")
  writeRaster(summer_mean, output_file, overwrite = TRUE)
  
  # Clean up temporary files
  cat("Cleaning up temporary files...\n")
  unlink(temp_nc_files)
  
  return(output_file)
}

# Get the source folder path
source_folder <- file.path(future.climate, climate.model)

# Define the years you want to process
years_to_process <- 1981:2005  # Adjust this range as needed

# Process each year
results <- list()
for(year in years_to_process) {
  cat("\n=== Processing summer", year, "===\n")
  
  tryCatch({
    result <- mean_summer_rasters_gz(gz_folder = source_folder, 
                                    year = year, 
                                    temp_directory = temp_dir,
                                    output_folder = file.path(source_folder, "processed"))  # Save back to source folder
    results[[as.character(year)]] <- result
    
  }, error = function(e) {
    cat("Error processing year", year, ":", e$message, "\n")
    results[[as.character(year)]] <- NA
  })
  
  # Optional: force garbage collection to free memory
  gc()
}

# Summary of results
successful_years <- sum(!is.na(results))
cat("\nProcessing complete:", successful_years, "out of", length(years_to_process), "years processed successfully.\n")


#### FFDI 95 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sum_summer_rasters_gz <- function(gz_folder, year, temp_directory = temp_dir, pattern = "FFDI_gt95perc_", output_folder = NULL) {
  # Define the months of interest
  prev_year <- year - 1
  months <- c(sprintf("%d11", prev_year), sprintf("%d12", prev_year), 
              sprintf("%d01", year), sprintf("%d02", year), sprintf("%d03", year))
  
  # List all .gz files in the folder
  all_gz_files <- list.files(gz_folder, pattern = "\\.nc\\.gz$", full.names = TRUE)
  
  # Filter files matching the required summer months
  summer_gz_files <- grep(paste0(pattern, "(", paste0(months, collapse = "|"), ")_ACCESS1-0.nc"), all_gz_files, value = TRUE)
  
  if (length(summer_gz_files) == 0) {
    warning(paste("No matching files found for summer of", year))
    return(NULL)
  }
  
  cat("Processing", length(summer_gz_files), "files for summer", year, "\n")
  
  # Decompress files to temp directory
  temp_nc_files <- character(length(summer_gz_files))
  
  for(i in seq_along(summer_gz_files)) {
    # Create temp filename
    temp_nc_files[i] <- file.path(temp_directory, 
                                  paste0(tools::file_path_sans_ext(basename(summer_gz_files[i]), compression = TRUE), "_temp.nc"))
    
    # Decompress
    cat("Decompressing:", basename(summer_gz_files[i]), "\n")
    R.utils::gunzip(summer_gz_files[i], destname = temp_nc_files[i], remove = FALSE)
  }
  
  # Read rasters and sum them
  cat("Reading and summing rasters...\n")
  summer_rasters <- rast(temp_nc_files)
  summer_sum <- sum(summer_rasters, na.rm = TRUE)
  
  # Define output location
  if(is.null(output_folder)) {
    output_folder <- gz_folder
  }
  output_file <- file.path(output_folder, sprintf("FFDI_95_summer_sum_%d.nc", year))
  
  # Save the output raster
  cat("Saving result to:", output_file, "\n")
  writeRaster(summer_sum, output_file, overwrite = TRUE)
  
  # Clean up temporary files
  cat("Cleaning up temporary files...\n")
  unlink(temp_nc_files)
  
  return(output_file)
}

# Get the source folder path
source_folder <- file.path(future.climate, climate.model)

# Define the years you want to process
years_to_process <- 2081:2099  # Adjust this range as needed

# Process each year
results <- list()
for(year in years_to_process) {
  cat("\n=== Processing summer", year, "===\n")
  
  tryCatch({
    result <- sum_summer_rasters_gz(gz_folder = source_folder, 
                                     year = year, 
                                     temp_directory = temp_dir,
                                     output_folder = file.path(source_folder, "processed"))  # Save back to source folder
    results[[as.character(year)]] <- result
    
  }, error = function(e) {
    cat("Error processing year", year, ":", e$message, "\n")
    results[[as.character(year)]] <- NA
  })
  
  # Optional: force garbage collection to free memory
  gc()
}

# Summary of results
successful_years <- sum(!is.na(results))
cat("\nProcessing complete:", successful_years, "out of", length(years_to_process), "years processed successfully.\n")

#### KBDI ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
kbdi.files <- all_files[grepl("KBDI", all_files, ignore.case = TRUE)]

sum_summer_rasters_gz <- function(gz_folder, year, temp_directory = temp_dir, pattern = "KBDI_gt95perc_", output_folder = NULL) {
  # Define the months of interest
  prev_year <- year - 1
  months <- c(sprintf("%d11", prev_year), sprintf("%d12", prev_year), 
              sprintf("%d01", year), sprintf("%d02", year), sprintf("%d03", year))
  
  # List all .gz files in the folder
  all_gz_files <- list.files(gz_folder, pattern = "\\.nc\\.gz$", full.names = TRUE)
  
  # Filter files matching the required summer months
  summer_gz_files <- grep(paste0(pattern, "(", paste0(months, collapse = "|"), ")_ACCESS1-0.nc"), all_gz_files, value = TRUE)
  
  if (length(summer_gz_files) == 0) {
    warning(paste("No matching files found for summer of", year))
    return(NULL)
  }
  
  cat("Processing", length(summer_gz_files), "files for summer", year, "\n")
  
  # Decompress files to temp directory
  temp_nc_files <- character(length(summer_gz_files))
  
  for(i in seq_along(summer_gz_files)) {
    # Create temp filename
    temp_nc_files[i] <- file.path(temp_directory, 
                                  paste0(tools::file_path_sans_ext(basename(summer_gz_files[i]), compression = TRUE), "_temp.nc"))
    
    # Decompress
    cat("Decompressing:", basename(summer_gz_files[i]), "\n")
    R.utils::gunzip(summer_gz_files[i], destname = temp_nc_files[i], remove = FALSE)
  }
  
  # Read rasters and sum them
  cat("Reading and summing rasters...\n")
  summer_rasters <- rast(temp_nc_files)
  summer_sum <- sum(summer_rasters, na.rm = TRUE)
  
  # Define output location
  if(is.null(output_folder)) {
    output_folder <- gz_folder
  }
  output_file <- file.path(output_folder, sprintf("KBDI_95_summer_sum_%d.nc", year))
  
  # Save the output raster
  cat("Saving result to:", output_file, "\n")
  writeRaster(summer_sum, output_file, overwrite = TRUE)
  
  # Clean up temporary files
  cat("Cleaning up temporary files...\n")
  unlink(temp_nc_files)
  
  return(output_file)
}

# Get the source folder path
source_folder <- file.path(future.climate, climate.model)

# Define the years you want to process
years_to_process <- 1981:1995  # Adjust this range as needed

# Process each year
results <- list()
for(year in years_to_process) {
  cat("\n=== Processing summer", year, "===\n")
  
  tryCatch({
    result <- sum_summer_rasters_gz(gz_folder = source_folder, 
                                    year = year, 
                                    temp_directory = temp_dir,
                                    output_folder = file.path(source_folder, "processed"))  # Save back to source folder
    results[[as.character(year)]] <- result
    
  }, error = function(e) {
    cat("Error processing year", year, ":", e$message, "\n")
    results[[as.character(year)]] <- NA
  })
  
  # Optional: force garbage collection to free memory
  gc()
}

#### SPEI ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
spei.files = list.files("F:/future_predictions/ACCESS1-0 RCP85/spei_output/", pattern = ".tif", full=T)
spei12 = spei.files[grepl("12m", spei.files)]
spei24 = spei.files[grepl("24m", spei.files)]

spei12 = rast(spei12)
spei24 = rast(spei24)

mean_spei_summer <- function(stack,lag_length, year, output_folder){
  prev_year <- year - 1
  dates <- c(as.Date(paste0("01-11-", prev_year), format = c("%d-%m-%Y")),
             as.Date(paste0("01-12-", prev_year), format = c("%d-%m-%Y")), 
             as.Date(paste0("01-01-", year), format = c("%d-%m-%Y")), 
             as.Date(paste0("01-02-", year), format = c("%d-%m-%Y")), 
             as.Date(paste0("01-03-", year), format = c("%d-%m-%Y")))
  
  # Filter files matching the required summer months
  year_stack = subset(stack, time(stack) %in% dates)
  
  # Create average for summer period
  spei_summer_mean = mean(year_stack, na.rm=TRUE)
  
  output_file <- file.path(output_folder, paste0("spei",lag_length,"_summer_mean_", year,".tif"))
  
  # Save the output raster
  cat("Saving result to:", output_file, "\n")
  writeRaster(spei_summer_mean, output_file, overwrite = TRUE)
}

# Get the source folder path
source_folder <- file.path(future.climate, climate.model)

# Define the years you want to process
years_to_process <- 2081:2099  # Adjust this range as needed

# Process each year
results <- list()
for(year in years_to_process) {
  cat("\n=== Processing SPEI12 summer", year, "===\n")
  
  tryCatch({
    result <- mean_spei_summer(spei12, 
                               lag_length = "12",
                               year = year, 
                               output_folder = file.path(source_folder, "processed"))  # Save back to source folder
    results[[as.character(year)]] <- result
    
  }, error = function(e) {
    cat("Error processing year", year, ":", e$message, "\n")
    results[[as.character(year)]] <- NA
  })
  
  # Optional: force garbage collection to free memory
  gc()
  cat("\n=== Processing SPEI24 summer", year, "===\n")
  
  tryCatch({
    result <- mean_spei_summer(spei24, 
                               lag_length = "24",
                               year = year, 
                               output_folder = file.path(source_folder, "processed"))  # Save back to source folder
    results[[as.character(year)]] <- result
    
  }, error = function(e) {
    cat("Error processing year", year, ":", e$message, "\n")
    results[[as.character(year)]] <- NA
  })
  
  # Optional: force garbage collection to free memory
  gc()
}

### PAST FOR CHECKING #### 
years_to_process <- 1981:2005  # Adjust this range as needed

# Process each year
results <- list()
for(year in years_to_process) {
  cat("\n=== Processing summer SPEI12", year, "===\n")
  
  tryCatch({
    result <- mean_spei_summer(spei12, 
                               lag_length = "12",
                               year = year, 
                               output_folder = file.path(source_folder, "processed"))  # Save back to source folder
    results[[as.character(year)]] <- result
    
  }, error = function(e) {
    cat("Error processing year", year, ":", e$message, "\n")
    results[[as.character(year)]] <- NA
  })
  
  # Optional: force garbage collection to free memory
  gc()
  
  cat("\n=== Processing summer SPEI24", year, "===\n")
  
  tryCatch({
    result <- mean_spei_summer(spei24, 
                               lag_length = "24",
                               year = year, 
                               output_folder = file.path(source_folder, "processed"))  # Save back to source folder
    results[[as.character(year)]] <- result
    
  }, error = function(e) {
    cat("Error processing year", year, ":", e$message, "\n")
    results[[as.character(year)]] <- NA
  })
  
  # Optional: force garbage collection to free memory
  gc()
}



#### THUNDERSTORM ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ts.files = all_files[grepl("BTE", all_files, ignore.case = TRUE)]

is_leap_year <- function(year) {
  return((year %% 4 == 0 & year %% 100 != 0) | (year %% 400 == 0))
}


sum_summer_thunderstorm <- function(gz_folder, year, pattern = "BTE_v1.1_ACCESS1-0_", temp_directory = temp_dir, output_folder = NULL) {
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
  # List all .gz files in the folder
  all_gz_files <- list.files(gz_folder, pattern = "\\.nc\\.gz$", full.names = TRUE)
  
  # Filter files matching the required summer months
  summer_gz_files <- c(grep(paste0(pattern,prev_year, ".nc"), all_gz_files, value = TRUE),
                       grep(paste0(pattern,year, ".nc"), all_gz_files, value = TRUE))
  
  if (length(summer_gz_files) == 0) {
    warning(paste("No matching files found for summer of", year))
    return(NULL)
  }
  
  cat("Processing", length(summer_gz_files), "files for summer", year, "\n")
  
  # Decompress files to temp directory
  temp_nc_files <- character(length(summer_gz_files))
  
  for(i in seq_along(summer_gz_files)) {
    # Create temp filename
    temp_nc_files[i] <- file.path(temp_directory, 
                                  paste0(tools::file_path_sans_ext(basename(summer_gz_files[i]), compression = TRUE), "_temp.nc"))
    
    # Decompress
    cat("Decompressing:", basename(summer_gz_files[i]), "\n")
    R.utils::gunzip(summer_gz_files[i], destname = temp_nc_files[i], remove = FALSE)
  }
  
  # Load rasters
  
  r_current <- rast(temp_nc_files[1])
  r_prev <- rast(temp_nc_files[1])
  
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
  
  # Define output location
  if(is.null(output_folder)) {
    output_folder <- gz_folder
  }
  output_file <- file.path(output_folder, sprintf("thunderstorm_summer_days_%d.nc", year))
  
  # Save the output raster
  cat("Saving result to:", output_file, "\n")
  writeRaster(summer_sum, output_file, overwrite = TRUE)
  
  # Clean up temporary files
  cat("Cleaning up temporary files...\n")
  unlink(temp_nc_files)
  
  return(output_file)
}

# Define the years you want to process
years_to_process <- 1995:2005  # Adjust this range as needed
#years_to_process <- 2081:2099  # Adjust this range as needed

source_folder <- file.path(future.climate, climate.model)

# Process each year
results <- list()
for(year in years_to_process) {
  cat("\n=== Processing summer", year, "===\n")
  
  tryCatch({
    result <- sum_summer_thunderstorm(gz_folder = source_folder, 
                                    year = year, 
                                    temp_directory = temp_dir,
                                    output_folder = file.path(source_folder, "processed"))  # Save back to source folder
    results[[as.character(year)]] <- result
    
  }, error = function(e) {
    cat("Error processing year", year, ":", e$message, "\n")
    results[[as.character(year)]] <- NA
  })
  
  # Optional: force garbage collection to free memory
  gc()
}

###################################################
####  Project and Mask to Study Region ####
###################################################
cov.stack = rast("F:/vic_fire_mapping/covariates/masked/masked_static_covariate_stack.tif")
cov.path = "F:/vic_fire_mapping/covariates"
mask = cov.stack[[1]]

#### FFDI ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ffdi.path = "F:/future_predictions/ACCESS1-0 RCP85/processed"
ffdi.files = list.files(ffdi.path, pattern="FFDI_summer_mean", full=T)
ffdi.files <- ffdi.files[!grepl("\\.nc\\.aux\\.xml$", ffdi.files)]

ffdi.stack <- list()
for (f in ffdi.files){
  ffdi.ras <- rast(f)
  ffdi.ras <- terra::project(ffdi.ras, crs(mask))
  ffdi.ras = terra::resample(ffdi.ras, mask, method = "bilinear")
  ffdi.ras =  terra::mask(ffdi.ras, mask)
  names(ffdi.ras) <- basename(f)
  filename <- gsub("\\.nc$", ".tif", basename(f))
  writeRaster(ffdi.ras, file.path(cov.path, "modelled_masked",climate.model, "ffdi", filename), overwrite =TRUE)
  message(paste(f, "complete"))
  gc()
}

#### FFDI 95 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ffdi.path = "F:/future_predictions/ACCESS1-0 RCP85/processed"
ffdi.files = list.files(ffdi.path, pattern="FFDI_95_summer_sum", full=T)
ffdi.files <- ffdi.files[!grepl("\\.nc\\.aux\\.xml$", ffdi.files)]

ffdi.stack <- list()
for (f in ffdi.files){
  ffdi.ras <- rast(f)
  ffdi.ras <- terra::project(ffdi.ras, crs(mask))
  ffdi.ras = terra::resample(ffdi.ras, mask, method = "bilinear")
  ffdi.ras =  terra::mask(ffdi.ras, mask)
  names(ffdi.ras) <- basename(f)
  filename <- gsub("\\.nc$", ".tif", basename(f))
  writeRaster(ffdi.ras, file.path(cov.path, "modelled_masked",climate.model,"ffdi95", filename), overwrite =TRUE)
  message(paste(f, "complete"))
  gc()
}


#### KBDI ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
kbdi.path = "F:/future_predictions/ACCESS1-0 RCP85/processed"
kbdi.files = list.files(kbdi.path, pattern="KBDI_95_summer_sum", full=T)
kbdi.files <- kbdi.files[!grepl("\\.nc\\.aux\\.xml$", kbdi.files)]

kbdi.stack <- list()
for (f in kbdi.files){
  kbdi.ras <- rast(f)
  kbdi.ras <- terra::project(kbdi.ras, crs(mask))
  kbdi.ras = terra::resample(kbdi.ras, mask, method = "bilinear")
  kbdi.ras =  terra::mask(kbdi.ras, mask)
  names(kbdi.ras) <- basename(f)
  filename <- gsub("\\.nc$", ".tif", basename(f))
  writeRaster(kbdi.ras, file.path(cov.path, "modelled_masked",climate.model, "kbdi95", filename), overwrite =TRUE)
  message(paste(f, "complete"))
  gc()
}

#### SPEI 12 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
spei.path = "F:/future_predictions/ACCESS1-0 RCP85/processed"
spei.files = list.files(spei.path, pattern="spei12", full=T)
spei.files <- spei.files[!grepl("\\.nc\\.aux\\.xml$", spei.files)]

spei.stack <- list()
for (f in spei.files){
  spei.ras <- rast(f)
  spei.ras <- terra::project(spei.ras, crs(mask))
  spei.ras = terra::resample(spei.ras, mask, method = "bilinear")
  spei.ras =  terra::mask(spei.ras, mask)
  names(spei.ras) <- basename(f)
  filename <- gsub("\\.nc$", ".tif", basename(f))
  writeRaster(spei.ras, file.path(cov.path, "modelled_masked",climate.model, "spei12", filename), overwrite =TRUE)
  message(paste(f, "complete"))
  gc()
}

#### SPEI 24 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
spei.path = "F:/future_predictions/ACCESS1-0 RCP85/processed"
spei.files = list.files(spei.path, pattern="spei24", full=T)
spei.files <- spei.files[!grepl("\\.nc\\.aux\\.xml$", spei.files)]

spei.stack <- list()
for (f in spei.files){
  spei.ras <- rast(f)
  spei.ras <- terra::project(spei.ras, crs(mask))
  spei.ras = terra::resample(spei.ras, mask, method = "bilinear")
  spei.ras =  terra::mask(spei.ras, mask)
  names(spei.ras) <- basename(f)
  filename <- gsub("\\.nc$", ".tif", basename(f))
  writeRaster(spei.ras, file.path(cov.path, "modelled_masked",climate.model, "spei24", filename), overwrite =TRUE)
  message(paste(f, "complete"))
  gc()
}

#### Thunderstorm ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
thunder.path = "F:/future_predictions/ACCESS1-0 RCP85/processed"
thunder.files = list.files(thunder.path, pattern="thunderstorm_summer_days", full=T)
thunder.files <- thunder.files[!grepl("\\.nc\\.aux\\.xml$", thunder.files)]

thunder.stack <- list()
for (f in thunder.files){
  thunder.ras <- rast(f)
  thunder.ras <- terra::project(thunder.ras, crs(mask))
  thunder.ras = terra::resample(thunder.ras, mask, method = "bilinear")
  thunder.ras =  terra::mask(thunder.ras, mask)
  names(thunder.ras) <- basename(f)
  filename <- gsub("\\.nc$", ".tif", basename(f))
  writeRaster(thunder.ras, file.path(cov.path, "modelled_masked",climate.model, "thunderstorm", filename), overwrite =TRUE)
  message(paste(f, "complete"))
  gc()
}


###################################################
####  Generate Climate Model Prediction Stacks ####
###################################################

# Read static in covariate stack
cov.stack = rast("F:/vic_fire_mapping/covariates/masked/masked_static_covariate_stack.tif")
cov.path = "F:/vic_fire_mapping/covariates"
output_dir <- "F:/vic_fire_mapping/output_data"

make_prediction_stack <- function(start_year, end_year, 
                                  prediction.stack,
                                  cov.path = NULL){
  year_pattern <- paste0("(", paste(start_year:end_year, collapse = "|"), ")")
  # FFDI Prediction Raster
  ffdi_95_days <- rast(list.files(file.path(cov.path,"ffdi95"), 
                                  pattern = paste0(year_pattern, ".tif"), 
                                  full.names = TRUE))
  
  ffdi_95_days = mean(ffdi_95_days, na.rm=TRUE)
  
  names(ffdi_95_days) <- "ffdi_95_days"
  prediction.stack <- c(prediction.stack, ffdi_95_days)
  
  # KBDI Prediction Raster
  kbdi_95_days <- rast(list.files(file.path(cov.path,"kbdi95"), 
                                  pattern = paste0(year_pattern, ".tif"), 
                                  full.names = TRUE))
  
  kbdi_95_days = mean(kbdi_95_days, na.rm=TRUE)
  
  names(kbdi_95_days) <- "kbdi_95_days"
  prediction.stack <- c(prediction.stack, kbdi_95_days)
  
  # SPEI Prediction Raster
  spei12_mean <- rast(list.files(file.path(cov.path, "spei12"), 
                                  pattern=paste0(year_pattern,".tif"), full=T))
  spei12_mean[is.infinite(spei12_mean)] <- NA
  spei12_mean <- mean(spei12_mean, na.rm=TRUE)
  names(spei12_mean) <- "spei12_mean"
  prediction.stack <- c(prediction.stack, spei12_mean)
  
  spei24_mean <- rast(list.files(file.path(cov.path, "spei24"), 
                                  pattern=paste0(year_pattern,".tif"), full=T))
  spei24_mean[is.infinite(spei24_mean)] <- NA
  spei24_mean <- mean(spei24_mean, na.rm=TRUE)
  names(spei24_mean) <- "spei24_mean"
  prediction.stack <- c(prediction.stack, spei24_mean)
  
  
  # Thunderstorm Prediction Raster
  thunderstorm_days <- rast(list.files(file.path(cov.path, "thunderstorm"), 
                                       pattern=paste0(year_pattern,".tif"), full=T))
  thunderstorm_days <- mean(thunderstorm_days, na.rm=TRUE)
  names(thunderstorm_days) <- "thunderstorm_days"
  prediction.stack <- c(prediction.stack, thunderstorm_days)
  
  return(prediction.stack)
}

#### Baseline - Observed Data #### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
baseline_predstack = make_prediction_stack(1985, 2005, 
                                           prediction.stack = cov.stack,
                                           cov.path = "F:/vic_fire_mapping/covariates/masked")

writeRaster(baseline_predstack, filename = "F:/vic_fire_mapping/output_data/prediction_stacks/baseline_observed_predstack.tif",
            overwrite=TRUE)

#### Baseline - Observed Data #### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
baseline_2006_23_predstack = make_prediction_stack(2006, 2023, 
                                           prediction.stack = cov.stack,
                                           cov.path = "F:/vic_fire_mapping/covariates/masked")

writeRaster(baseline_2006_23_predstack, filename = "F:/vic_fire_mapping/output_data/prediction_stacks/baseline_observed_2006_23_predstack.tif",
            overwrite=TRUE)


#### Baseline - ACCESS Model Data #### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
baseline_modelled_predstack = make_prediction_stack(1985,2005, 
                                           prediction.stack = cov.stack,
                                           cov.path = "F:/vic_fire_mapping/covariates/modelled_masked/ACCESS1-0 RCP85")

writeRaster(baseline_modelled_predstack, 
            filename = "F:/vic_fire_mapping/output_data/prediction_stacks/baseline_ACCESS_modelled_predstack.tif",
            overwrite=TRUE)

#### Future - ACCESS Model RCP 9.5 Data #### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
access_20812099_modelled_predstack = make_prediction_stack(2081, 2099, 
                                                    prediction.stack = cov.stack,
                                                    cov.path = "F:/vic_fire_mapping/covariates/modelled_masked/ACCESS1-0 RCP85")

writeRaster(access_20812099_modelled_predstack, 
            filename = "F:/vic_fire_mapping/output_data/prediction_stacks/access_rcp95_20812099_modelled_predstack.tif",
            overwrite=TRUE)







