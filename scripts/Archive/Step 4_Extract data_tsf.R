## Extract points
library(sf)
library(terra)
library(dplyr)

random_sample = vect("F:/vic_fire_mapping/covariates/random_points.shp")

## Extract covariates
cov.path <- "F:/vic_fire_mapping/covariates"

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
spei.maps = list.files(file.path(cov.path, "masked", "spei12_awo"), pattern=".tif", full=T)
extract_spei_data = function(spei.ras.path, random.points){
  spei.ras = rast(spei.ras.path)
  spei.data = terra::extract(spei.ras, random.points, xy=TRUE)
  names(spei.data) <- c("ID", "spei12_mean", "x","y")
  spei.data$year = readr::parse_number(gsub("SPEI12","", names(spei.ras)))
  return(spei.data)
}

spei_extract = lapply(spei.maps, FUN = extract_spei_data, random_sample)
spei12_extract_long = do.call("rbind", spei_extract)

spei.maps = list.files(file.path(cov.path, "masked", "spei24_awo"), pattern=".tif", full=T)
extract_spei_data = function(spei.ras.path, random.points){
  spei.ras = rast(spei.ras.path)
  spei.data = terra::extract(spei.ras, random.points, xy=TRUE)
  names(spei.data) <- c("ID", "spei24_mean", "x","y")
  spei.data$year = readr::parse_number(gsub("SPEI24","", names(spei.ras)))
  return(spei.data)
}

spei_extract = lapply(spei.maps, FUN = extract_spei_data, random_sample)
spei24_extract_long = do.call("rbind", spei_extract)

## Extract TSF (Time Since Fire) values
tsf.maps = list.files("F:/vic_fire_mapping/covariates/raw/tsf", pattern = ".tif$", full.names = TRUE)

extract_tsf_data = function(tsf.ras.path, random.points){
  tsf.ras = rast(tsf.ras.path)
  tsf.data = terra::extract(tsf.ras, random.points, xy=TRUE)
  names(tsf.data) <- c("ID", "tsf", "x", "y")
  tsf.data$year = readr::parse_number(names(tsf.ras))
  return(tsf.data)
}

tsf_extract = lapply(tsf.maps, FUN = extract_tsf_data, random_sample)
tsf_extract_long = do.call("rbind", tsf_extract)

# lag TSF by one year
tsf_extract_lagged <- tsf_extract_long %>%
  mutate(year = year + 1)  # So that the TSF value from year Y-1 gets applied to fire year Y


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
install.packages("blockCV")
library(blockCV)
random_sample = vect("F:/vic_fire_mapping/covariates/random_points.shp")
random_sample = random_sample %>% st_as_sf()


# ====== OLD BLOCKCV SECTION: Assigns folds across whole time period ======
# random_sample_cv <- random_sample 
# random_sample_cv$burnt = if_else(df$burnt.sum > 0, 1, 0)
# sb <- cv_spatial(x = random_sample_cv,
#                  column = "burnt",
#                  k = 5,
#                  size = 23000,
#                  selection = "random",
#                  iteration = 50)
# df$spatial_folds = sb$folds_ids
# df = df %>% dplyr::select(x, y, autocov, spatial_folds)

# ====== NEW BLOCKCV SECTION: Assign folds separately for each time period ======
#burn_extract_long = burn_extract_long %>% left_join(df, by = c("x", "y"))

## Spatial Autocovariate
df = burn_extract_long %>% group_by(x, y) %>% summarise(burnt.sum = sum(burnt))
autocov = spdep::autocov_dist(df$burnt.sum, as.matrix(data.frame(df$x, df$y)), 
                       nbs = 23000, type = "inverse", 
                       zero.policy = TRUE, style = "B", longlat = NULL)
df$autocov <- autocov
ggplot(df) + geom_point(aes(x = x, y = y, colour = autocov)) + scale_color_viridis_c()

# Assign temporal bins
burn_extract_long$period <- cut(burn_extract_long$year,
                                breaks = c(1980, 2001, 2012, 2024),
                                labels = c("1990_2001", "2002_2012", "2013_2024"),
                                include.lowest = TRUE, right = TRUE)

# Run blockCV separately for each time period
fold_df_list <- list()
periods <- unique(na.omit(burn_extract_long$period))  # exclude NA

for (p in periods) {
  message("Running blockCV for period: ", p)
  sub_data <- burn_extract_long %>% filter(period == p)
  sub_points <- unique(sub_data[, c("x", "y")])
  
  sub_df <- df %>% inner_join(sub_points, by = c("x", "y"))
  sub_sp <- st_as_sf(sub_df, coords = c("x", "y"), crs = 3111)
  sub_sp$burnt <- if_else(sub_df$autocov > 0, 1, 0)
  
  sb <- cv_spatial(x = sub_sp,
                   column = "burnt",
                   k = 5,
                   size = 23000,
                   selection = "random",
                   iteration = 50)
  
  sub_df$spatial_fold <- sb$folds_ids
  sub_df$period <- p
  fold_df_list[[as.character(p)]] <- sub_df[, c("x", "y", "spatial_fold", "period")]
}

# Combine results
folds_combined <- bind_rows(fold_df_list)

# Merge back to main dataset
burn_extract_long <- burn_extract_long %>%
  left_join(folds_combined, by = c("x", "y", "period"))

# Generate unique spatio-temporal fold IDs
burn_extract_long <- burn_extract_long %>%
  mutate(spatial_temporal_fold = paste0(spatial_fold, "_", period),
         spatial_temporal_fold = as.numeric(as.factor(spatial_temporal_fold)))

# Merge all extracted covariates into one dataset
joined_data <- burn_extract_long %>%
  left_join(ffdi_extract_long, by = c("ID", "x", "y", "year")) %>%
  left_join(ffdi95_extract_long, by = c("ID", "x", "y", "year")) %>%
  left_join(thunder_extract_long, by = c("ID", "x", "y", "year")) %>%
  left_join(spei12_extract_long, by = c("ID", "x", "y", "year")) %>%
  left_join(spei24_extract_long, by = c("ID", "x", "y", "year")) %>%
  left_join(kbdi_extract_long, by = c("ID", "x", "y", "year")) %>%
  left_join(tsf_extract_lagged,    by = c("ID", "x", "y", "year")) %>%
  left_join(cov_extracted, by = c("ID", "x", "y"))

# Remove problematic fold
joined_data <- joined_data %>% filter(spatial_temporal_fold != 16)

# Save full dataset including all covariates and folds
write.csv(joined_data, "F:/vic_fire_mapping/output_data/full_fire_covariates_tsf_spei.csv", row.names = FALSE)
head(joined_data)

