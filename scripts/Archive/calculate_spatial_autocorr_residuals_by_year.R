library(dplyr)
library(ncf)          # for spline.correlog
library(readr)
library(future.apply) # for parallel lapply

# 0. Plan the parallel backend
#    Here we leave one core free; adjust workers as you like.
plan(multisession, workers = availableCores() - 1)

# 1. Load model & data
brt_model      <- readRDS("F:/vic_fire_mapping/output_data/lr_0.025_fire_model_post1981.RDS")
fire_modelling <- read_csv("F:/vic_fire_mapping/output_data/fire_modelling_post1981.csv")
fire_modelling$residuals <- brt_model$residuals
stopifnot(all(c("year","residuals") %in% names(fire_modelling)))

# 2. Define years and run in parallel
target_years <- 1980:1995

spline_results <- future_lapply(target_years, function(yr) {
  message("Processing year: ", yr)
  fire_year <- filter(fire_modelling, year == yr)
  
  if (nrow(fire_year) > 100 && !all(is.na(fire_year$residuals))) {
    resid_summary <- fire_year %>%
      select(x, y, residuals) %>%
      group_by(x, y) %>%
      summarise(mean_resid = mean(residuals, na.rm=TRUE), .groups="drop")
    
    sc <- spline.correlog(
      x     = resid_summary$x,
      y     = resid_summary$y,
      z     = resid_summary$mean_resid,
      resamp= 50    # reduced from 100 to speed up
    )
    
    est <- summary(sc)$estimate[1]
  } else {
    message("Skipping year ", yr, ": insufficient data.")
    est <- NA
  }
  
  gc()  # free memory
  return(est)
}, future.seed=TRUE)

# 3. Assemble into a data.frame
spline_df <- tibble(
  year     = target_years,
  distance = unlist(spline_results)
)

# 4. Split into small/large by the median
threshold  <- median(spline_df$distance, na.rm=TRUE)

# Corrected interpretation:
large_autocorr_years <- spline_df %>% filter(distance > threshold) %>% pull(year)  # large autocorrelation = long lag
small_autocorr_years <- spline_df %>% filter(distance <= threshold) %>% pull(year) # small autocorrelation = short lag

message("Large autocorr years (long lag): ", paste(sort(large_autocorr_years), collapse = ", "))
message("Small autocorr years (short lag): ", paste(sort(small_autocorr_years), collapse = ", "))

print(spline_df %>% arrange(desc(distance)))


