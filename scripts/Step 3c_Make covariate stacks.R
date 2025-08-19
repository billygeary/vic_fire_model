## Create covariate layers
library(terra)
library(dplyr)

#############################################################################################
#### Set Up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#############################################################################################
cov.path = "F:/vic_fire_mapping/covariates"
mask = rast("F:/vic_fire_mapping/covariates/victoria_mask_75m.tif")

#############################################################################################
#### TOPOGRAPHY ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#############################################################################################

## Topographic Wetness Index
twi = rast(file.path(cov.path, "raw/topography/twi_3s.tif"))
twi_proj = terra::project(twi, crs(mask))
twi_75m_vic = resample(twi_proj, mask)
twi_75m_vic = mask(twi_75m_vic, mask)
writeRaster(twi_75m_vic, file.path(cov.path, "masked/topography/twi_75m_masked.tif"))

## Broad refuges
broad_refuges = rast(file.path(cov.path, "raw/topography/Broadscale_refuge.tif"))
broadrefuges_75m_vic = resample(broad_refuges, mask)
broadrefuges_75m_vic = mask(broadrefuges_75m_vic, mask)
writeRaster(broadrefuges_75m_vic, file.path(cov.path, "masked/topography/broadrefuges_75m_masked.tif"))

## Local Refuges
local_refuges = rast(file.path(cov.path, "raw/topography/local_refugia1.tif"))
localrefuges_75m_vic = resample(local_refuges, mask)
localrefuges_75m_vic = mask(localrefuges_75m_vic, mask)
localrefuges_75m_vic = ifel(localrefuges_75m_vic > 1000, 0, localrefuges_75m_vic)

writeRaster(localrefuges_75m_vic, file.path(cov.path, "masked/topography/localrefuges_75m_masked.tif"),overwrite=TRUE)

#############################################################################################
#### Long-term climate ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#############################################################################################

# Bioclim 1
bio1 <- rast("F:/bioclim/BIOCLIM 1990/b01.flt")
bio1_proj = terra::project(bio1, crs(mask))
bio1_75m_vic = resample(bio1_proj, mask)
bio1_75m_vic = mask(bio1_75m_vic, mask)
writeRaster(bio1_75m_vic, file.path(cov.path, "masked/vegetation/bio1_75m_masked.tif"))

# Bioclim 5
bio5 <- rast("F:/bioclim/BIOCLIM 1990/b05.flt")
bio5_proj = terra::project(bio5, crs(mask))
bio5_75m_vic = resample(bio5_proj, mask)
bio5_75m_vic = mask(bio5_75m_vic, mask)
writeRaster(bio5_75m_vic, file.path(cov.path, "masked/vegetation/bio5_75m_masked.tif"))

# Bioclim 18
bio18 <- rast("F:/bioclim/BIOCLIM 1990/b18.flt")
bio18_proj = terra::project(bio18, crs(mask))
bio18_75m_vic = resample(bio18_proj, mask)
bio18_75m_vic = mask(bio18_75m_vic, mask)
writeRaster(bio18_75m_vic, file.path(cov.path, "masked/vegetation/bio18_75m_masked.tif"))

#############################################################################################
#### Vegetation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#############################################################################################
nvc = rast(file.path(cov.path, "raw/vegetation/NVR2017_CONDITION.tif"))
nvc_75m_vic = resample(nvc, mask)
writeRaster(nvc_75m_vic, file.path(cov.path, "masked/vegetation/nvc_75m_masked.tif"))

#######################################################################################
#### SOIL ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#######################################################################################

## Soil bulk density
bdw = rast(file.path(cov.path, "raw/vegetation/BDW_000_005_EV_N_P_AU_TRN_N_20230607.tif"))
bdw_proj = terra::project(bdw, crs(mask))
bdw_75m_vic = resample(bdw_proj, mask)
bdw_75m_vic = mask(bdw_75m_vic, mask)
writeRaster(bdw_75m_vic, file.path(cov.path, "masked/vegetation/bdw_75m_masked.tif"))

## Clay Content
cly = rast(file.path(cov.path, "raw/vegetation/CLY_000_005_EV_N_P_AU_TRN_N_20210902.tif"))
cly_proj = terra::project(cly, crs(mask))
cly_75m_vic = resample(cly_proj, mask)
cly_75m_vic = mask(cly_75m_vic, mask)
writeRaster(cly_75m_vic, file.path(cov.path, "masked/vegetation/cly_75m_masked.tif"))

## Soil Ph
phw = rast(file.path(cov.path, "raw/vegetation/PHW_000_005_EV_N_P_AU_TRN_N_20220520.tif"))
phw_proj = terra::project(phw, crs(mask))
phw_75m_vic = resample(phw_proj, mask)
phw_75m_vic = mask(phw_75m_vic, mask)
writeRaster(phw_75m_vic, file.path(cov.path, "masked/vegetation/phw_75m_masked.tif"))

#######################################################################################
#### IGNITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#######################################################################################
# Distance from road
roads = vect(file.path(cov.path, "raw/ignition/TR_ROAD_ALL.shp"))
roads = project(roads, crs(mask))
roads_ras = rasterize(roads, mask, touches=TRUE)
distance_roads = terra::distance(roads_ras)
distance_roads_75m_vic = mask(distance_roads, mask)
writeRaster(distance_roads_75m_vic, file.path(cov.path, "masked/ignition/distance_roads_75m_masked.tif"))

# Fuel Management Zones
#  "0 - Not Zoned"  "3 - Landscape Management Zone" "2 - Bushfire Moderation Zone" "1 - Asset Protection Zone" "4 - Planned Burn Exclusion Zone"
fmz = vect(file.path(cov.path, "raw/ignition/FIREFMZ.shp"))
fmz = project(fmz, crs(mask))
fmz_ras = rasterize(fmz, mask, field = "ZONETYPE", touches=TRUE)
fmz_ras_75m_vic = mask(fmz_ras, mask)
writeRaster(fmz_ras_75m_vic, file.path(cov.path, "masked/ignition/fuelmgmntzones_75m_masked.tif"))

# Lightning - Andrew Dowdy
# https://doi.org/10.3389/fclim.2025.1539873
thunder.path = "F:/thunderstorms/thunderstorm_occurrence_1980_2023"
thunder.files = list.files(thunder.path, pattern="summer_days", full=T)
thunder.files <- thunder.files[!grepl("\\.nc\\.aux\\.xml$", thunder.files)]

thunder.stack <- list()
for (f in thunder.files){
  thunder.ras <- rast(f)
  thunder.ras <- project(thunder.ras, crs(mask))
  thunder.ras = resample(thunder.ras, mask, method = "bilinear")
  thunder.ras = mask(thunder.ras, mask)
  names(thunder.ras) <- basename(f)
  filename <- gsub("\\.nc$", ".tif", basename(f))
  writeRaster(thunder.ras, file.path(cov.path, "masked", "thunderstorm", filename), overwrite =TRUE)
  gc()
}

##########################################################################################
#### Fire Weather ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##########################################################################################
## Fuel Dryness - SPEI6, SPEI12, SPEI24 KBDE - Andrew Dowdy
kbdi.path = "F:/kbdi/KBDI_95pc_1980-2024"
kbdi.files = list.files(kbdi.path, pattern="summer_sum", full=T)
kbdi.files <- kbdi.files[!grepl("\\.nc\\.aux\\.xml$", kbdi.files)]

kbdi.stack <- list()
for (f in kbdi.files){
  kbdi.ras <- rast(f)
  kbdi.ras <- project(kbdi.ras, crs(mask))
  kbdi.ras = resample(kbdi.ras, mask, method = "bilinear")
  kbdi.ras = mask(kbdi.ras, mask)
  names(kbdi.ras) <- basename(f)
  filename <- gsub("\\.nc$", ".tif", basename(f))
  writeRaster(kbdi.ras, file.path(cov.path, "masked", "kbdi95", filename), overwrite =TRUE)
  gc()
}

## SPEI
spei.path = "F:/awo_climate_data/spei_output"
spei.files = list.files(spei.path, pattern=".tif", full=T)
spei.files = spei.files[grep("12_summer_avg", spei.files)]

for (f in spei.files){
  spei.ras <- rast(f)
  mask.spei <- project(mask, crs(spei.ras))  # Ensure the mask is in the same CRS as SPEI
  spei.ras <- crop(spei.ras, mask.spei)  # Crop SPEI to mask extent
  spei.ras <- project(spei.ras, crs(mask.spei))  # Reproject SPEI to match the mask CRS
  spei.ras <- terra::resample(spei.ras, mask, method = "bilinear")  # Resample to match resolution
  spei.ras <- mask(spei.ras, mask) 
  names(spei.ras) <- basename(f)
  filename <- gsub("\\.nc$", ".tif", basename(f))
  writeRaster(spei.ras, file.path(cov.path, "masked", "spei12_awo", filename), overwrite =TRUE)
  gc()
}

spei.path = "F:/awo_climate_data/spei_output"
spei.files = list.files(spei.path, pattern=".tif", full=T)
spei.files = spei.files[grep("24_summer_avg", spei.files)]

for (f in spei.files){
  spei.ras <- rast(f)
  mask.spei <- project(mask, crs(spei.ras))  # Ensure the mask is in the same CRS as SPEI
  spei.ras <- crop(spei.ras, mask.spei)  # Crop SPEI to mask extent
  spei.ras <- project(spei.ras, crs(mask))  # Reproject SPEI to match the mask CRS
  spei.ras <- terra::resample(spei.ras, mask, method = "bilinear")  # Resample to match resolution
  spei.ras <- mask(spei.ras, mask) 
  names(spei.ras) <- basename(f)
  filename <- gsub("\\.nc$", ".tif", basename(f))
  writeRaster(spei.ras, file.path(cov.path, "masked", "spei24_awo", filename), overwrite =TRUE)
  gc()
}


## Fire Weather - FFDI25 and FFDI50 - Andrew Dowdy
ffdi.path = "F:/ffdi/FFDI_95pc_1980-2024"
ffdi.files = list.files(ffdi.path, pattern="summer_sum", full=T)
ffdi.files <- ffdi.files[!grepl("\\.nc\\.aux\\.xml$", ffdi.files)]

for (f in ffdi.files){
  ffdi.ras <- rast(f)
  ffdi.ras <- project(ffdi.ras, crs(mask))
  ffdi.ras = resample(ffdi.ras, mask)
  ffdi.ras = mask(ffdi.ras, mask)
  names(ffdi.ras) <- basename(f)
  filename <- gsub("\\.nc$", ".tif", basename(f))
  writeRaster(ffdi.ras, file.path(cov.path, "masked", "ffdi95", filename), overwrite =TRUE)
  gc()
}

ffdi.path = "F:/ffdi/FFDI_1980-2024"
ffdi.files = list.files(ffdi.path, pattern="summer_mean", full=T)
ffdi.files <- ffdi.files[!grepl("\\.nc\\.aux\\.xml$", ffdi.files)]

ffdi.stack <- list()
for (f in ffdi.files){
  ffdi.ras <- rast(f)
  ffdi.ras <- project(ffdi.ras, crs(mask))
  ffdi.ras = resample(ffdi.ras, mask)
  ffdi.ras = mask(ffdi.ras, mask)
  names(ffdi.ras) <- basename(f)
  filename <- gsub("\\.nc$", ".tif", basename(f))
  writeRaster(ffdi.ras, file.path(cov.path, "masked", "ffdi_mean", filename), overwrite =TRUE)
  gc()
}



############################################################################################
#### Make the stack ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
############################################################################################
masked.covs <- list.files(file.path(cov.path, "masked"), pattern = "75m_masked.tif", recursive = T, full=T)

cov.stack = rast(masked.covs)

names(cov.stack) <- c("distance_roads", "fuel_management_zones", "broad_refuges", "local_refuges",
                      "twi", "bio1", "bio18", "bio5", "bdw", "cly",  "nvc", "phw")

writeRaster(cov.stack, file.path(cov.path, "masked/masked_static_covariate_stack.tif"), overwrite=TRUE)

