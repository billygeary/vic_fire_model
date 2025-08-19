library(dplyr)
library(tidyr)
library(gbm)
library(terra)

# Predictions
cov.path = "F:/vic_fire_mapping/covariates"
output_dir <- "F:/vic_fire_mapping/output_data"

mod = readr::read_rds(file.path(output_dir, "brt_full_rac_variable_neighbours.rds"))
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
  
  # Make RAC 
  prediction.xy$rac = 0
  
  # Make the predictions
  predicted <- gbm::predict.gbm(mod, prediction.xy, 
                                type="response", 
                                #ntrees = gbm.perf(mod, method = "test", plot.it=FALSE)
                                )
  
  prediction.xy$prediction = predicted
  predicted.xy = prediction.xy %>% dplyr::select(x,y,prediction)
  
  # Rasterise
  predicted.map <- terra::rast(predicted.xy, type="xyz", 
                               crs = crs(prediction.stack))
  names(predicted.map) <-paste(pred_year)
  gc()
  writeRaster(predicted.map,filename= file.path(output_dir, "predictions", paste0("predictions_",pred_year,".tif")), overwrite=TRUE)
  print(pred_year)
}

years = 20:2013
lapply(years, make_year_prediction)

library(tidyterra)
library(patchwork)
library(ggplot2)
rast = list.files("F:/vic_fire_mapping/output_data/predictions", pattern = ".tif", full=T)
rast = rast(rast[15:34])

mean.fire = mean(rast)
ggplot() + geom_spatraster(data=mean.fire) + 
  scale_fill_viridis_c(option="B")


plot(rast$`2007`)

plot09 = ggplot() + geom_spatraster(data=rast$'2007') + 
  scale_fill_viridis_c(option="D", na.value = "white") + labs(fill = "Year 2007") + theme_void()
plot20 = ggplot() + geom_spatraster(data = mean.fire) +
  scale_fill_viridis_c(option="D", na.value = "white") + labs(fill = "Mean 2005-2024") + theme_void()

plot09 / plot20



gbm::plot.gbm(mod, "ffdi_95_days", type="response")
gbm::plot.gbm(mod, "spei12_mean", type="response")
gbm::plot.gbm(mod, "bio5", type="response")
gbm::plot.gbm(mod, "local_refuges", type="response")
gbm::plot.gbm(mod, "thunderstorm_days", type="response")
gbm::plot.gbm(mod, "rac", type="response")
gbm::plot.gbm(mod, c("ffdi_95_days","spei12_mean"), type="response")
gbm::plot.gbm(mod, c("ffdi_95_days","thunderstorm_days"), type="response")


plot(mod, i.var = "spei12_mean", smooth = TRUE)

library(pdp)
partial(mod, train=fire_modelling, n.trees=mod$n.trees, pred.var = "thunderstorm_days", plot = TRUE, smooth = TRUE)

pred = list.files(file.path(output_dir, "predictions"), full=T)[14:33]
preds = rast(pred)
mean.pred = mean(preds)
plot(mean.pred)