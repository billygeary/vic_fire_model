# Define parameters

years <- c(2013, 2016)
variables <- c("et_morton_potential")
base_url <- "https://s3-ap-southeast-2.amazonaws.com/silo-open-data/Official/annual"

# Create a directory to store the files
filepath <- "F:/silo_climate_data"

# Loop through years and variables to download the files
for (var in variables) {
  for (year in years) {
    file_url <- sprintf("%s/%s/%d.%s.nc", base_url, var, year, var)
    dest_file <- sprintf(file.path(filepath,paste0(var), "%d_%s.nc"), year, var)
    
    # Download the file
    tryCatch({
      download.file(file_url, destfile = dest_file, mode = "wb")
      message(sprintf("Downloaded: %s", dest_file))
    }, error = function(e) {
      message(sprintf("Failed to download: %s", file_url))
    })
  }
}
