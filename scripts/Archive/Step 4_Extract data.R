## Extract points
library(sf)
library(terra)
library(dplyr)

random_sample = vect("F:/vic_fire_mapping/covariates/random_points.shp")

## Extract covariates
cov_stack = rast(file.path(cov.path, "masked/masked_static_covariate_stack.tif"))

cov_extracted = terra::extract(cov_stack, random_sample, xy=TRUE)

# Check correlation between variables
cors = cor(cov_extracted[,2:ncol(cov_extracted)], method="pearson", us="pairwise.complete.obs")
corrplot::corrplot(cors)

## Extract FFDI values - mean
ffdi.maps = list.files(file.path(cov.path, "masked", "ffdi_mean"), pattern=".tif", full=T)
extract_ffdi_data = function(ffdi.ras.path, random.points){
  ffdi.ras = rast(ffdi.ras.path)
  ffdi.data = terra::extract(ffdi.ras, random.points, xy=TRUE)
  names(ffdi.data) <- c("ID", "ffdi_mean", "x","y")
  ffdi.data$year = readr::parse_number(names(ffdi.ras))
  return(ffdi.data)
}

ffdi_extract = lapply(ffdi.maps, FUN = extract_ffdi_data, random_sample)
ffdi_extract_long = do.call("rbind", ffdi_extract)

## Extract FFDI values - 95 percentile
ffdi.maps = list.files(file.path(cov.path, "masked", "ffdi95"), pattern=".tif", full=T)
extract_ffdi_data = function(ffdi.ras.path, random.points){
  ffdi.ras = rast(ffdi.ras.path)
  ffdi.data = terra::extract(ffdi.ras, random.points, xy=TRUE)
  names(ffdi.data) <- c("ID", "ffdi_95_days", "x","y")
  ffdi.data$year = readr::parse_number(names(ffdi.ras))
  return(ffdi.data)
}

ffdi95_extract = lapply(ffdi.maps, FUN = extract_ffdi_data, random_sample)
ffdi95_extract_long = do.call("rbind", ffdi95_extract)

## Extract kbdi values
kbdi.maps = list.files(file.path(cov.path, "masked", "kbdi95"), pattern=".tif", full=T)
extract_kbdi_data = function(kbdi.ras.path, random.points){
  kbdi.ras = rast(kbdi.ras.path)
  kbdi.data = terra::extract(kbdi.ras, random.points, xy=TRUE)
  names(kbdi.data) <- c("ID", "kbdi_95_days", "x","y")
  kbdi.data$year = readr::parse_number(names(kbdi.ras))
  return(kbdi.data)
}

kbdi_extract = lapply(kbdi.maps, FUN = extract_kbdi_data, random_sample)
kbdi_extract_long = do.call("rbind", kbdi_extract)

## Extract spei values
spei.maps = list.files(file.path(cov.path, "masked", "spei12"), pattern=".tif", full=T)
extract_spei_data = function(spei.ras.path, random.points){
  spei.ras = rast(spei.ras.path)
  spei.data = terra::extract(spei.ras, random.points, xy=TRUE)
  names(spei.data) <- c("ID", "spei12_mean", "x","y")
  spei.data$year = readr::parse_number(gsub("SPEI12","", names(spei.ras)))
  return(spei.data)
}

spei_extract = lapply(spei.maps, FUN = extract_spei_data, random_sample)
spei12_extract_long = do.call("rbind", spei_extract)

spei.maps = list.files(file.path(cov.path, "masked", "spei24"), pattern=".tif", full=T)
extract_spei_data = function(spei.ras.path, random.points){
  spei.ras = rast(spei.ras.path)
  spei.data = terra::extract(spei.ras, random.points, xy=TRUE)
  names(spei.data) <- c("ID", "spei24_mean", "x","y")
  spei.data$year = readr::parse_number(gsub("SPEI24","", names(spei.ras)))
  return(spei.data)
}

spei_extract = lapply(spei.maps, FUN = extract_spei_data, random_sample)
spei24_extract_long = do.call("rbind", spei_extract)

## Extract Thunderstorm values
thunder.maps = list.files(file.path(cov.path, "masked", "thunderstorm"), pattern=".tif", full=T)
extract_thunder_data = function(thunder.ras.path, random.points){
  thunder.ras = rast(thunder.ras.path)
  thunder.data = terra::extract(thunder.ras, random.points, xy=TRUE)
  names(thunder.data) <- c("ID", "thunderstorm_days", "x","y")
  thunder.data$year = readr::parse_number(names(thunder.ras))
  return(thunder.data)
}

thunder_extract = lapply(thunder.maps, FUN = extract_thunder_data, random_sample)
thunder_extract_long = do.call("rbind", thunder_extract)

## Spatial Autocorrelation
# library(spdep)
# library(sf)
# 
# coords <- random_sample %>% st_as_sf() %>% st_coordinates()
# pcnm_vars <- vegan::pcnm(dist(coords))$vectors[, 1:5]  # Extract 5 PCNMs
# data.in <- cbind(data.in, pcnm_vars)  # Add to dataset
# 

## Extract fire presences and absences. 
fire.maps = list.files(file.path("F:/vic_fire_mapping/fire_data"),pattern = ".tif", full=TRUE)

extract_fire_data = function(fire.ras.path, random.points){
  fire.ras = rast(fire.ras.path)
  fire.data = terra::extract(fire.ras, random.points, xy=TRUE)
  names(fire.data) <- c("ID", "burnt", "x","y")
  fire.data$year = readr::parse_number(names(fire.ras))
  return(fire.data)
}

burn_extract = lapply(fire.maps, FUN = extract_fire_data, random_sample)

burn_extract_long = do.call("rbind", burn_extract)


## Cross-validation folds
library(blockCV)
random_sample = vect("F:/vic_fire_mapping/covariates/random_points.shp")

random_sample = random_sample %>% st_as_sf()

## Spatial Autocovariate
df = burn_extract_long %>% group_by(x,y) %>% summarise(burnt.sum = sum(burnt))
autocov = autocov_dist(df$burnt.sum, as.matrix(data.frame(df$x, df$y)), 
                       nbs = 23000, type = "inverse", 
                       zero.policy = TRUE, style = "B", longlat=NULL)
df$autocov <- autocov
ggplot(df) + geom_point(aes(x=x,y=y, colour=autocov)) + scale_color_viridis_c()

## BlockCV Folds
random_sample_cv <- random_sample 
random_sample_cv$burnt = if_else(df$burnt.sum>0, 1, 0)
sb <- cv_spatial(x = random_sample_cv,
                  column = "burnt", # the response column (binary or multi-class)
                  k = 5, # number of folds
                  size = 23000, # size of the blocks in metres
                  selection = "random", # random blocks-to-fold
                  iteration = 50) # find evenly dispersed folds

df$spatial_folds = sb$folds_ids
df = df %>% dplyr::select(x,y,autocov, spatial_folds)

burn_extract_long = burn_extract_long %>% left_join(df, by = c("x", "y"))

## Join the data together
joined_data = left_join(burn_extract_long, ffdi_extract_long, by = c("ID", "x", "y", "year"))
joined_data = left_join(joined_data, ffdi95_extract_long, by = c("ID", "x", "y", "year"))
joined_data = left_join(joined_data, thunder_extract_long, by = c("ID", "x", "y", "year"))
joined_data = left_join(joined_data, spei12_extract_long, by = c("ID", "x", "y", "year"))
joined_data = left_join(joined_data, spei24_extract_long, by = c("ID", "x", "y", "year"))
joined_data = left_join(joined_data, kbdi_extract_long, by = c("ID", "x", "y", "year"))
joined_data = left_join(joined_data, cov_extracted, by = c("ID", "x", "y"))

write.csv(joined_data, "F:/vic_fire_mapping/output_data/full_fire_covariates.csv")

