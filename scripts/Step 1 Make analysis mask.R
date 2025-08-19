library(terra)
library(sf)
library(dplyr)

#### Make analysis mask
mask = rast("F:/deeca_data/hdms_2025/_75m/HDM_10253_Tyto tenebricosa_Sooty Owl_75m_vg2020.tif")
vic = read_sf("F:/deeca_data/outlines/STE_2021_AUST_GDA2020.shp") %>% filter(STE_NAME21=="Victoria") %>% st_transform(crs(mask)) %>% vect() 
mask = rasterize(vic, mask)

names(mask) <- "victoria_mask"
varnames(mask) <- "victoria_mask"

writeRaster(mask, "F:/vic_fire_mapping/covariates/victoria_mask_75m.tif")

#### Make random points for study region
points = terra::spatSample(mask, size = 10000, method = "stratified", as.points = TRUE)

writeVector(points, "F:/vic_fire_mapping/covariates/random_points.shp")
