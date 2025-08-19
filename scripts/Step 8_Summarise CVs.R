# Setup 
library(PresenceAbsence)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(ggplot2)

#### Read in the data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
no_rac_data = lapply(list.files("F:/vic_fire_mapping/output_data/cross_validation/no_rac", pattern = ".csv", full=T), read.csv)

cv_data = read.csv("F:/vic_fire_mapping/output_data/fire_modelling_rac_ready_forCV.csv")
cv_folds = cv_data %>% group_by(composite_fold_no, composite_fold, temporal_fold, spatial_folds) %>% summarise(min_year = min(year), max_year = max(year))

#### Area Under the Receiver Operating Curve ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fold_numbers = sapply(no_rac_data, FUN = function(x){composite_fold_no = unique(x$composite_fold_no)})
no_rac_auc = sapply(no_rac_data, FUN = function(x){pROC::roc(x$obs, x$pred)$auc[[1]]})
fold_statistics = data.frame(composite_fold_no = fold_numbers, 
                        AUC = no_rac_auc)
mean(fold_statistics$AUC)



#### True Skill Statistic ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Function to calculate TSS across thresholds for one fold
calc_tss <- function(df, thresholds) {
  sapply(thresholds, function(thresh) {
    conf_mat <- try(cmx(DATA = df[, 1:3], threshold = thresh), silent = TRUE)
    if (inherits(conf_mat, "try-error")) return(NA)
    
    sens <- try(sensitivity(conf_mat)[1, 1], silent = TRUE)
    spec <- try(specificity(conf_mat)[1, 1], silent = TRUE)
    
    if (inherits(sens, "try-error") || inherits(spec, "try-error")) return(NA)
    
    tss = (sens + spec - 1)
    
    return(tss)
  })
}

# Average TSS at a Threshold
no_rac_tss = lapply(no_rac_data, FUN = function(x){
  optimal_threshold <- optimal.thresholds(DATA = x[,1:3], threshold = 100)[2,2]
  conf_mat <- cmx(DATA = x[,1:3], threshold = optimal_threshold)
  tss <- sensitivity(conf_mat)[1,1] + specificity(conf_mat)[1,1] - 1
  tss
  return(data.frame(composite_fold_no = unique(x$composite_fold_no),
                    max_tss = tss))})

no_rac_tss = do.call("rbind", no_rac_tss)

fold_statistics = left_join(fold_statistics, no_rac_tss)

mean(fold_statistics$max_tss) # Mean

# TSS Across a set of thresholds
thresholds <- seq(0, 1, by = 0.01)

# Apply to each fold and return a data frame
no_rac_tss_list <- map2(no_rac_data, seq_along(no_rac_data), ~{
  tss_vals <- calc_tss(.x, thresholds)
  tibble(fold = .y, threshold = thresholds, TSS = tss_vals)
})

# Combine into a single tidy data frame
no_rac_tss_df <- bind_rows(no_rac_tss_list)

# Compute average TSS across folds
mean_tss_df <- no_rac_tss_df %>%
  group_by(threshold) %>%
  summarise(mean_TSS = mean(TSS, na.rm = TRUE), .groups = "drop") %>%
  mutate(fold = "mean")

# Plot using ggplot
ggplot(no_rac_tss_df, aes(x = threshold, y = TSS, group = as.factor(fold))) +
  geom_line(alpha = 0.4, colour = "grey50") +
  geom_line(data = mean_tss_df, aes(x = threshold, y = mean_TSS,group = as.factor(fold)), color = "red", linewidth = 1.2) +
  labs(x = "Threshold", y = "True Skill Statistic") +
  theme_minimal()

#### BRIER SCORES ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Assuming `data` is a list of data frames with 'obs' and 'pred' columns
brier_scores <- lapply(no_rac_data, function(df) {
  brier = mean((df$pred - df$obs)^2)
  composite_fold_no = unique(df$composite_fold_no)
  return(data.frame(brier = brier, composite_fold_no=composite_fold_no))
})

brier_scores = bind_rows(brier_scores)

fold_statistics = left_join(fold_statistics, brier_scores)


mean(brier_scores$brier)  # Average across folds

brier_df <- tibble(
  fold = brier_scores$composite_fold_no,
  brier_score = brier_scores$brier
)

# Plot
ggplot(brier_df, aes(x = fold, y = brier_score)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = mean(brier_scores), linetype = "dashed", color = "red") +
  labs(title = "Brier Score per Fold",
       y = "Brier Score",
       x = "Fold") +
  theme_minimal()

#### Compile Statistics ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fold_statistics = left_join(fold_statistics, cv_folds)
fold_statistics$Autocorrelation = "No RAC"

#### Read in the data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rac_data = lapply(list.files("F:/vic_fire_mapping/output_data/cross_validation/with_rac", pattern = ".csv", full=T), read.csv)

#### Area Under the Receiver Operating Curve ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fold_numbers = sapply(rac_data, FUN = function(x){composite_fold_no = unique(x$composite_fold_no)})
rac_auc = sapply(rac_data, FUN = function(x){pROC::roc(x$obs, x$pred)$auc[[1]]})
fold_statistics_with_rac = data.frame(composite_fold_no = fold_numbers, 
                             AUC = rac_auc)
mean(fold_statistics_with_rac$AUC)



#### True Skill Statistic ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Function to calculate TSS across thresholds for one fold
calc_tss <- function(df, thresholds) {
  sapply(thresholds, function(thresh) {
    conf_mat <- try(cmx(DATA = df[, 1:3], threshold = thresh), silent = TRUE)
    if (inherits(conf_mat, "try-error")) return(NA)
    
    sens <- try(sensitivity(conf_mat)[1, 1], silent = TRUE)
    spec <- try(specificity(conf_mat)[1, 1], silent = TRUE)
    
    if (inherits(sens, "try-error") || inherits(spec, "try-error")) return(NA)
    
    tss = (sens + spec - 1)
    
    return(tss)
  })
}

# Average TSS at a Threshold
rac_tss = lapply(rac_data, FUN = function(x){
  optimal_threshold <- optimal.thresholds(DATA = x[,1:3], threshold = 100)[2,2]
  conf_mat <- cmx(DATA = x[,1:3], threshold = optimal_threshold)
  tss <- sensitivity(conf_mat)[1,1] + specificity(conf_mat)[1,1] - 1
  tss
  return(data.frame(composite_fold_no = unique(x$composite_fold_no),
                    max_tss = tss))})

rac_tss = do.call("rbind", rac_tss)

fold_statistics_with_rac = left_join(fold_statistics_with_rac, rac_tss)

mean(fold_statistics_with_rac$max_tss) # Mean

# TSS Across a set of thresholds
thresholds <- seq(0, 1, by = 0.01)

# Apply to each fold and return a data frame
rac_tss_list <- map2(rac_data, seq_along(rac_data), ~{
  tss_vals <- calc_tss(.x, thresholds)
  tibble(fold = .y, threshold = thresholds, TSS = tss_vals)
})

# Combine into a single tidy data frame
rac_tss_df <- bind_rows(rac_tss_list)

# Compute average TSS across folds
mean_tss_df <- rac_tss_df %>%
  group_by(threshold) %>%
  summarise(mean_TSS = mean(TSS, na.rm = TRUE), .groups = "drop") %>%
  mutate(fold = "mean")

# Plot using ggplot
ggplot(rac_tss_df, aes(x = threshold, y = TSS, group = as.factor(fold))) +
  geom_line(alpha = 0.4, colour = "grey50") +
  geom_line(data = mean_tss_df, aes(x = threshold, y = mean_TSS,group = as.factor(fold)), color = "red", linewidth = 1.2) +
  labs(x = "Threshold", y = "True Skill Statistic") +
  theme_minimal()

#### BRIER SCORES ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Assuming `data` is a list of data frames with 'obs' and 'pred' columns
brier_scores <- lapply(rac_data, function(df) {
  brier = mean((df$pred - df$obs)^2)
  composite_fold_no = unique(df$composite_fold_no)
  return(data.frame(brier = brier, composite_fold_no=composite_fold_no))
})

brier_scores = bind_rows(brier_scores)

fold_statistics_with_rac = left_join(fold_statistics_with_rac, brier_scores)


mean(brier_scores$brier)  # Average across folds

brier_df <- tibble(
  fold = brier_scores$composite_fold_no,
  brier_score = brier_scores$brier
)

# Plot
ggplot(brier_df, aes(x = fold, y = brier_score)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = mean(brier_scores), linetype = "dashed", color = "red") +
  labs(title = "Brier Score per Fold",
       y = "Brier Score",
       x = "Fold") +
  theme_minimal()

#### Compile Statistics ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fold_statistics_with_rac = left_join(fold_statistics_with_rac, cv_folds)
fold_statistics_with_rac$Autocorrelation = "With RAC"

#### Join Together ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
compiled_statistics = rbind(fold_statistics, fold_statistics_with_rac)
compiled_statistics$Autocorrelation = as.factor(compiled_statistics$Autocorrelation)
compiled_statistics$temporal_fold_lab = paste0(compiled_statistics$min_year, "_", compiled_statistics$max_year)

#### Plot summaries ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
auc.plot = ggplot(compiled_statistics) + geom_tile(aes(x = temporal_fold_lab, y = as.factor(spatial_folds), fill = AUC)) + scale_fill_viridis_c() + theme_minimal() + labs(x = "Temporal Fold", y = "Spatial Fold") + facet_wrap(~Autocorrelation)
tss.plot = ggplot(compiled_statistics) + geom_tile(aes(x = temporal_fold_lab, y = as.factor(spatial_folds), fill = max_tss)) + scale_fill_viridis_c() + theme_minimal() + labs(x = "Temporal Fold", y = "Spatial Fold") + facet_wrap(~Autocorrelation)
brier.plot = ggplot(compiled_statistics) + geom_tile(aes(x = temporal_fold_lab, y = as.factor(spatial_folds), fill = brier)) + scale_fill_viridis_c() + theme_minimal() + labs(x = "Temporal Fold", y = "Spatial Fold")  + facet_wrap(~Autocorrelation)

library(patchwork)
combined.plot = auc.plot/tss.plot/brier.plot + plot_layout(axes = "collect", guides = "collect")

ggsave(plot = combined.plot, filename = "C:/Users/geary/OneDrive - The University of Melbourne/Research/Projects/2025/vic_fire_model/outputs/crossvalidation_results_tsfmodel.pdf", width = 6, height = 8, scale = 1.5)
