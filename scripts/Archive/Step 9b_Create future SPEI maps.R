library(terra)
library(R.utils)
library(SPEI)
library(SpatIndex)

####################################
#### Read In Water Balance Data ####
####################################
wb.files = list.files("F:/future_predictions/ACCESS1-0 RCP85/wb_output", full=TRUE)

ref_wb = rast(wb.files[2])
future_wb = rast(wb.files[1])

water_balance = c(ref_wb, future_wb)

###########################
#### SPEI Calculations ####
###########################

spei_stack <- terra::app(water_balance, fun = function(x,  scale = 12, na.rm = TRUE, ...) {
  res <- tryCatch({
    ts_data <- ts(x, start = c(1985, 1), frequency = 12)
    spei_out <- SPEI::spei(ts_data, scale = 12, 
                           ref.start = c(1985, 1), ref.end = c(2005, 12), 
                           na.rm = TRUE, verbose = FALSE, ...)
    as.numeric(spei_out$fitted)
  }, error = function(e) rep(NA, length(x)))
  res
})


spei24_stack <- terra::app(water_balance, fun = function(x,  scale = 24, na.rm = TRUE, ...) {
  res <- tryCatch({
    ts_data <- ts(x, start = c(1985, 1), frequency = 12)
    
    spei_out <- SPEI::spei(ts_data, scale = 24, 
                           ref.start = c(1985, 1), ref.end = c(2005, 12), 
                           na.rm = TRUE, verbose = FALSE, ...)
    as.numeric(spei_out$fitted)
  }, error = function(e) rep(NA, length(x)))
  res
})

##########################
#### Save the Results ####
##########################
output_dir = "F:/future_predictions/ACCESS1-0 RCP85/spei_output"
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


