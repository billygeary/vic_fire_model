
# Process silo climate grids

library(terra)

grids = list.files("F:/silo_climate_data/min_temp", pattern = ".nc", full = T)

r = rast(grids[1])

# Define years
years <-c(1996,2000,2006)

input_dir <- "F:/silo_climate_data/min_temp_daily"  # Update this to your actual path
output_dir <- "F:/silo_climate_data/annual_min_temp_warmest_month"         # Update this to your preferred output path

# Function to calculate min temp in the warmest month
calc_min_temp_warmest_month <- function(year) {
  # Load the raster stack for the year
  file_path <- file.path(input_dir, paste0(year, "_min_temp.nc"))  # Adjust filename if needed
  r_stack <- rast(file_path)
  
  # Extract dates from layers assuming the layers are ordered by day
  # Modify this step if your raster has an associated time metadata
  num_days <- nlyr(r_stack)
  dates <- seq(as.Date(paste0(year, "-01-01")), length.out = num_days, by = "day")
  
  # Assign the corresponding dates to each layer
  time(r_stack) <- dates
  
  # Compute the mean temperature for each month
  monthly_mean <- tapp(r_stack, format(dates, "%Y-%m"), mean)
  
  # Identify the warmest month
  warmest_month <- which.max(global(monthly_mean, "mean", na.rm=TRUE)$mean)
  
  # Select the corresponding daily layers for the warmest month
  warmest_dates <- dates[format(dates, "%m") == format(dates[warmest_month * 30], "%m")]
  warmest_stack <- subset(r_stack, which(dates %in% warmest_dates))
  
  # Compute the minimum temperature in the warmest month
  min_temp_warmest_month <- app(warmest_stack, min)
  
  # Save the result
  output_file <- file.path(output_dir, paste0("min_temp_warmest_month_", year, ".tif"))
  writeRaster(min_temp_warmest_month, output_file, overwrite = TRUE)
  
  message("Processed year: ", year)
}

# Process each year
lapply(years, process_year)


# Define years
years <-1980:2024

input_dir <- "F:/silo_climate_data/max_temp_daily"  # Update this to your actual path
output_dir <- "F:/silo_climate_data/hot_days"         # Update this to your preferred output path


# Function to count the number of days with min temp > 35°C
count_hot_days <- function(year) {
  # Load the raster stack for the year
  file_path <- file.path(input_dir, paste0(year, "_max_temp.nc"))  # Adjust filename if needed
  r_stack <- rast(file_path)
  ext(r_stack) <- ext(min_temp_warmest_month)
  crs(r_stack) <- crs(min_temp_warmest_month)
  # Count the number of days where temp > 35°C for each pixel
  hot_days <- app(r_stack, fun = function(x) {
    sum(x > 35, na.rm = TRUE)})  # Count days where temperature exceeds 35°C
  
  # Save the result
  output_file <- file.path(output_dir, paste0("hot_35_days_", year, ".tif"))
  writeRaster(hot_days, output_file, overwrite = TRUE)
  
  message("Processed hot days for year: ", year)
}

# Process each year
lapply(years, count_hot_days)

years = 1980



##

temp.files <- "F:/silo_climate_data/annual_min_temp_warmest_month"         # Update this to your preferred output path
files <- list.files(temp.files, pattern = ".tif", full=TRUE)

data <- data.frame(filepath = files)
data$year = readr::parse_number(data$filepath)
data = data %>%dplyr::filter(year < 2001)
temp.rasts = rast(data$filepath)
mean.rast = mean(temp.rasts, na.rm=TRUE)

writeRaster(mean.rast, "F:/silo_climate_data/mean_min_temp_warmest_month_1980-2000.tif")
