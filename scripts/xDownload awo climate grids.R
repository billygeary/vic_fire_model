# Define parameters

years <- c(1980:2024)
variables <- c("etot")
base_url <- "https://thredds.nci.org.au/thredds/fileServer/iu04/australian-water-outlook/historical/v1/AWRALv7/"

# Create a directory to store the files
filepath <- "F:/awo_climate_data"

# Increase the timeout length
options(timeout=3600)

# Loop through years and variables to download the files
for (var in variables) {
  for (year in years) {
    file_url <- sprintf("%s%s_%d.nc", base_url, var, year)
    dest_file <- sprintf(file.path(filepath,paste0(var), "%d_%s.nc"), year, var)
    
    # Download the file
    tryCatch({
      download.file(file_url, destfile = dest_file, mode = "wb", )
      message(sprintf("Downloaded: %s", dest_file))
    }, error = function(e) {
      message(sprintf("Failed to download: %s", file_url))
    })
  }
}



years <- c(1980:2024)
variables <- c("agcd_v2_precip_total_r005_monthly")
base_url <- "https://thredds.nci.org.au/thredds/fileServer/zv2/agcd/v2-0-3/precip/total/r005/01month/"

# Create a directory to store the files
filepath <- "F:/agcd_climate_data"

# Increase the timeout length
options(timeout=3600)

# Loop through years and variables to download the files
for (var in variables) {
  for (year in years) {
    file_url <- sprintf("%s%s_%d.nc", base_url, var, year)
    dest_file <- sprintf(file.path(filepath,"precip", "%s_%d.nc"), var, year)
    
    # Download the file
    tryCatch({
      download.file(file_url, destfile = dest_file, mode = "wb", )
      message(sprintf("Downloaded: %s", dest_file))
    }, error = function(e) {
      message(sprintf("Failed to download: %s", file_url))
    })
  }
}
 