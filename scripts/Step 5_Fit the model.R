# =============================================================================
# Unified RAC pipeline: run with and without TSF (base BRT + RAC BRT)
# =============================================================================
library(dplyr)
library(readr)
library(tidyr)
library(spdep)
library(gbm)
library(dismo)
library(pROC)
library(purrr)

message("🔄 Starting unified RAC pipeline (no TSF + with TSF) ...")

# ----------------------------------------------------------------------------- 
# 0) CONFIG
# -----------------------------------------------------------------------------
setwd("F:/")

# Update this to the CSV you just saved from the extraction script
INPUT_CSV <- "F:/vic_fire_mapping/output_data/full_fire_covariates_no_folds.csv"

# Path to your BRT helper
SOURCE_FUN <- "C:/Users/charliehart/The University of Melbourne/Billy Geary - vic_fire_model/scripts/fun_fit_iterative_brt.R"

# Learning rate etc.
LEARNING_RATE <- 0.05
SEED <- 123

# Year-specific neighbor distances (as per your previous code)
small_years <- c(1981, 1982, 1983, 1984, 1985, 1987, 1988, 1990, 1994, 
                 1996, 1997, 2001, 2003, 2004, 2005, 2006, 2007,
                 2008, 2009, 2013, 2014, 2015, 2016, 2019, 2020)
large_years <- c(1986, 1989, 1991, 1992, 1993, 1995, 1998, 2000,
                 2002, 2010, 2011, 2012, 2017, 2018, 2021, 2022, 2023)

# ----------------------------------------------------------------------------- 
# 1) Helpers
# -----------------------------------------------------------------------------
safe_factor <- function(x) { if (!is.null(x)) as.factor(x) else x }

prep_data <- function(df, include_tsf = FALSE) {
  base_cols <- c(
    "x","y","year","burnt",
    "bio5","spei12_mean","ffdi_95_days","spei24_mean","bio18",
    "kbdi_95_days","twi","local_refuges","broad_refuges","thunderstorm_days",
    "fuel_management_zones","distance_roads"
  )
  cols <- if (include_tsf) c(base_cols, "tsf") else base_cols
  
  # sanity if TSF requested but absent
  if (include_tsf && !"tsf" %in% names(df)) {
    stop("TSF requested but 'tsf' column not found in input CSV: ", INPUT_CSV)
  }
  
  out <- df %>%
    filter(year > 1980) %>%
    dplyr::select(all_of(cols)) %>%
    mutate(fuel_management_zones = safe_factor(fuel_management_zones)) %>%
    drop_na() %>%
    filter(if_all(everything(), ~ !is.na(.) & is.finite(.)))
  
  return(out)
}

compute_rac_by_year <- function(df_with_residuals) {
  # df_with_residuals must have x, y, year, residuals
  years <- sort(unique(df_with_residuals$year))
  rac_list <- vector("list", length(years))
  names(rac_list) <- as.character(years)
  
  for (yr in years) {
    message("➡️  RAC for year: ", yr)
    sub_df <- df_with_residuals %>%
      filter(year == yr) %>%
      dplyr::select(x, y, residuals)
    
    coords <- as.matrix(sub_df[, c("x","y")])
    
    d_thresh <- if (yr %in% small_years) 100000 else 300000  # meters
    nb <- dnearneigh(x = coords, d1 = 0, d2 = d_thresh, longlat = FALSE)
    lw <- nb2listw(nb, style = "W", zero.policy = TRUE)
    rac_vals <- lag.listw(lw, sub_df$residuals, zero.policy = TRUE)
    
    rac_list[[as.character(yr)]] <- sub_df %>%
      mutate(rac = rac_vals, year = yr)
  }
  
  bind_rows(rac_list)
}

fit_and_eval <- function(df_ready, tag) {
  # Save RAC-ready table
  out_csv <- paste0("F:/vic_fire_mapping/output_data/fire_modelling_rac_ready_", tag, ".csv")
  write_csv(df_ready, out_csv)
  
  # Fit final model with RAC
  source(SOURCE_FUN)
  message("Fitting final BRT with RAC: ", tag)
  set.seed(SEED)
  m_final <- fit_iterative_brt(df_ready, LEARNING_RATE, seed = SEED)
  
  saveRDS(m_final, paste0("F:/vic_fire_mapping/output_data/brt_full_rac_", tag, ".rds"))
  
  # Evaluate
  nt <- gbm.perf(m_final, method = "test", plot.it = FALSE)
  pred <- predict(m_final, newdata = df_ready, n.trees = nt, type = "response")
  auc_val <- pROC::roc(df_ready$burnt, pred)
  
  cat("\n========== ", toupper(tag), " ==========\n", sep = "")
  cat("AUC:\n"); print(auc_val)
  cat("\nCV stats:\n"); print(m_final$cv.statistics)
  cat("\nTop variables:\n"); print(utils::head(summary(m_final), 15))
  invisible(m_final)
}

run_variant <- function(include_tsf = FALSE) {
  variant <- if (include_tsf) "tsf" else "spei"  # keep your old naming convention
  message("\n==============================")
  message("Running variant: ", toupper(variant))
  message("==============================\n")
  
  data <- read.csv(INPUT_CSV)
  data$fuel_management_zones <- as.factor(data$fuel_management_zones)
  
  # Prepare modelling data
  fire_modelling <- prep_data(data, include_tsf = include_tsf)
  
  # Base model input (order with burnt 3rd; keep year out for BRT)
  fire_modelling_for_brt <- fire_modelling %>%
    dplyr::select(x, y, burnt, everything(), -year)
  
  # Fit base model
  source(SOURCE_FUN)
  message("Fitting BASE BRT (no RAC): ", toupper(variant))
  set.seed(SEED)
  m_base <- fit_iterative_brt(fire_modelling_for_brt, LEARNING_RATE, seed = SEED)
  saveRDS(m_base, paste0("F:/vic_fire_mapping/output_data/lr_", sprintf("%.3f", LEARNING_RATE),
                         "_fire_model_", variant, ".RDS"))
  
  # Add residuals back by year (need year, so attach to the 'fire_modelling' copy)
  fire_modelling$residuals <- m_base$residuals
  
  # Compute RAC by year
  message("Computing RAC (100km vs 300km) ...")
  rac_combined <- compute_rac_by_year(fire_modelling %>% dplyr::select(x, y, year, residuals))
  
  # Join RAC to full data
  fire_modelling_rac <- fire_modelling %>%
    left_join(rac_combined %>% dplyr::select(x, y, year, rac),
              by = c("x","y","year"))
  
  # Build RAC-ready table (with or without TSF)
  base_predictors <- c(
    "x","y","burnt","bio5","spei12_mean","ffdi_95_days","spei24_mean","bio18",
    "kbdi_95_days","twi","local_refuges","broad_refuges","thunderstorm_days",
    "fuel_management_zones","distance_roads","rac"
  )
  cols_rac <- if (include_tsf) c(base_predictors, "tsf") else base_predictors
  
  fire_modelling_rac_ready <- fire_modelling_rac %>%
    dplyr::select(all_of(cols_rac)) %>%
    mutate(fuel_management_zones = as.factor(fuel_management_zones)) %>%
    drop_na() %>%
    as.data.frame()
  
  # Fit + evaluate final RAC model
  m_final <- fit_and_eval(fire_modelling_rac_ready, tag = if (include_tsf) "tsf" else "spei")
  
  # (Optional) PDPs — guard 'tsf' if not present
  plot.gbm(m_final, "rac", type = "response")
  plot.gbm(m_final, "fuel_management_zones", type = "response")
  plot.gbm(m_final, "spei12_mean", type = "response")
  plot.gbm(m_final, "ffdi_95_days", type = "response")
  plot.gbm(m_final, "spei24_mean", type = "response")
  plot.gbm(m_final, "thunderstorm_days", type = "response")
  if (include_tsf) plot.gbm(m_final, "tsf", type = "response")
  
  invisible(list(base = m_base, final = m_final))
}

# ----------------------------------------------------------------------------- 
# 2) RUN BOTH VARIANTS
# -----------------------------------------------------------------------------
res_no_tsf <- run_variant(include_tsf = FALSE)  # base + RAC WITHOUT tsf
res_with_tsf <- run_variant(include_tsf = TRUE) # base + RAC WITH tsf
