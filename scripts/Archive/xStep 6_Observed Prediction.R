library(dplyr)
library(tidyr)
library(gbm)
library(terra)

# Predictions
cov.path = "F:/vic_fire_mapping/covariates"
output_dir <- "F:/vic_fire_mapping/output_data"
mod = readr::read_rds(file.path(output_dir, "lr_0.025_fire_model_RAC.RDS"))
prediction.stack = rast(file.path(cov.path, "masked/masked_covariate_stack.tif"))


make_year_prediction <- function(pred_year){
  # FFDI Prediction Raster
  ffdi_95_days <- rast(list.files(file.path(cov.path, "masked", "ffdi95"), pattern=paste0(pred_year,".tif"), full=T))
  names(ffdi_95_days) <- "ffdi_95_days"
  prediction.stack <- c(prediction.stack, ffdi_95_days)
  
  # SPEI Prediction Raster
  spei12_mean <- rast(list.files(file.path(cov.path, "masked", "spei12"), pattern=paste0(pred_year,".tif"), full=T))
  names(spei12_mean) <- "spei12_mean"
  prediction.stack <- c(prediction.stack, spei12_mean)
  
  # Thunderstorm Prediction Raster
  thunderstorm_days <- rast(list.files(file.path(cov.path, "masked", "thunderstorm"), pattern=paste0(pred_year,".tif"), full=T))
  names(thunderstorm_days) <- "thunderstorm_days"
  prediction.stack <- c(prediction.stack, thunderstorm_days)
  
  # Make the stack a data frame
  prediction.xy <- terra::as.data.frame(prediction.stack, xy=TRUE)
  
  # Make year
  prediction.xy$year = pred_year
  
  # Make the predictions
  predicted <- gbm::predict.gbm(mod, prediction.xy, 
                                type="response", 
                                #ntrees = gbm.perf(mod, method = "test", plot.it=FALSE)
                                )
  
  prediction.xy$prediction = predicted
  predicted.xy = prediction.xy %>% select(x,y,prediction)
  
  # Rasterise
  predicted.map <- terra::rast(predicted.xy, type="xyz", crs = crs(prediction.stack))
  
  return(predicted.map)
}

pred.2009 <- make_year_prediction(2009)
pred.2020 <- make_year_prediction(2020)

library(tidyterra)
library(patchwork)

plot09 = ggplot() + geom_spatraster(data=pred.2009) + 
  scale_fill_viridis_c(option="B")
plot20 = ggplot() + geom_spatraster(data = pred.2020) +
  scale_fill_viridis_c(option="B")

plot09 / plot20
