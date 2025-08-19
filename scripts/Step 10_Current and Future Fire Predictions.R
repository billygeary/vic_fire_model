library(dplyr)
library(tidyr)
library(gbm)
library(terra)

# Predictions
cov.path = "F:/vic_fire_mapping/covariates"
output_dir <- "F:/vic_fire_mapping/output_data"
mod = readr::read_rds(file.path(output_dir, "lr_0.05_fire_model_spei.RDS"))

make_average_prediction <- function(covariate.stack, brt.model){
  # Make the stack a data frame
  prediction.xy <- terra::as.data.frame(covariate.stack, xy=TRUE)
  
  # Make the predictions
  predicted <- gbm::predict.gbm(mod, prediction.xy, 
                                type="response", 
                                #ntrees = gbm.perf(mod, method = "test", plot.it=FALSE)
  )
  
  prediction.xy$prediction = predicted
  predicted.xy = prediction.xy %>% select(x,y,prediction)
  # Rasterise
  predicted.map <- terra::rast(predicted.xy, type="xyz", crs = crs(covariate.stack))
  return(predicted.map)
}

#### Baseline
baseline_observed_predstack = rast( "F:/vic_fire_mapping/output_data/prediction_stacks/baseline_observed_predstack.tif")
baseline_observed_fire = make_average_prediction(baseline_observed_predstack, mod)
writeRaster(baseline_observed_fire, "F:/vic_fire_mapping/output_data/predictions/fire_observed_baseline_19851995.tif")

gc()

#### Baseline
baseline_observed_0623_predstack = rast( "F:/vic_fire_mapping/output_data/prediction_stacks/baseline_observed_2006_23_predstack.tif")
baseline_observed_0623_fire = make_average_prediction(baseline_observed_0623_predstack, mod)
writeRaster(baseline_observed_fire, "F:/vic_fire_mapping/output_data/predictions/fire_observed_baseline_20062023.tif")


baseline_modelled_predstack = rast("F:/vic_fire_mapping/output_data/prediction_stacks/baseline_ACCESS_modelled_predstack.tif")
baseline_modelled_fire = make_average_prediction(baseline_observed_predstack, mod)
writeRaster(baseline_modelled_fire, "F:/vic_fire_mapping/output_data/predictions/fire_ACCESS1-0_baseline_19851995.tif", overwrite=TRUE)

gc()

#### RCP8.5 - ACCESS1-0
access_20812099_modelled_predstack = rast("F:/vic_fire_mapping/output_data/prediction_stacks/access_rcp95_20812099_modelled_predstack.tif")
access_rcp85_20812099_fire = make_average_prediction(access_20812099_modelled_predstack, mod)
writeRaster(access_rcp85_20812099_fire, "F:/vic_fire_mapping/output_data/predictions/fire_ACCESS1-0_rcp85_20812099.tif", overwrite = TRUE)

dif = access_rcp85_20812099_fire - baseline_modelled_fire

library(tidyterra)
library(viridis)
library(ggplot2)
library(paletteer)
library(scico)
ggplot() + geom_spatraster(data = access_rcp85_20812099_fire) + scale_fill_viridis(trans="log")


ggplot() + 
  geom_spatraster(data = access_rcp85_20812099_fire) + 
  scale_fill_viridis(
    trans = "log",
    breaks = scales::log_breaks(n = 5)  # automatically generates good log breaks
  )

current = ggplot() + 
  geom_spatraster(data = baseline_observed_fire) + 
  scale_fill_viridis(
    option = "inferno",
    trans = "sqrt",
    breaks = c(0, 0.01, 0.05, 0.1, 0.15,0.25, 0.35, 0.5, 0.75, 1),
    lim = c(0,0.9),
    labels = scales::number_format(accuracy = 0.01), na.value = "transparent", 
    name = "Pr(Fire) 1985-2005",
  ) + theme_bw()

future = ggplot() + 
  geom_spatraster(data = access_rcp85_20812099_fire) + 
  scale_fill_viridis(
    option = "inferno",
    trans = "sqrt",
    breaks = c(0, 0.01, 0.05, 0.1, 0.15,0.25, 0.35, 0.5, 0.75, 1),
    lim = c(0,0.9),
    labels = scales::number_format(accuracy = 0.01), na.value = "transparent", 
    name = "Pr(Fire) 2081-99",
  ) + theme_bw()

change = ggplot() + 
  geom_spatraster(data = dif) + 
  scale_fill_scico(
    palette = "vik",
    #trans = "log",
    #breaks = c(-0.5, -0.25, -0.1, -0.05, 0, 0.05, 0.1, 0.25, 0.5),
    lim = c(-0.5,0.5),
    labels = scales::number_format(accuracy = 0.01), na.value = "transparent", 
    name = "Change in Pr(Fire)"
  ) + theme_bw()


library(patchwork)
current + future + change + plot_layout(nrow=1)
