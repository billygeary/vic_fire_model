# Visualise the model outputs
library(dismo)
library(gbm)

rac.model = readRDS("F:/vic_fire_mapping/output_data/brt_full_rac_tsf.rds")

# RAC MODEL
summary(rac.model)

gbm.interactions(rac.model)

gbm.perspec(rac.model, x = "rac", y = "spei12_mean", z.range = c(0,0.03))

##cur
plot.gbm(rac.model, "rac", type = "response")
plot.gbm(rac.model, "fuel_management_zones", type = "response") # THis looks good
plot.gbm(rac.model, "bio5", type = "response") # THis looks good
plot.gbm(rac.model, "bio18", type = "response") # THis looks good
plot.gbm(rac.model, "broad_refuges", type = "response") # THis looks good
plot.gbm(rac.model, "local_refuges", type = "response") # THis looks good
plot.gbm(rac.model, "spei12_mean", type = "response", smooth=TRUE) # high parts in right spot but a bit funky
plot.gbm(rac.model, "ffdi_95_days", type = "response", smooth=TRUE) # High parts in right spot but a bit funky
plot.gbm(rac.model, "spei24_mean", type = "response", smooth=TRUE) # High parts in two ends for 24 - fuel?
plot.gbm(rac.model, "thunderstorm_days", type = "response", smooth=TRUE) # High parts in right spot
plot.gbm(rac.model, "tsf", type = "response", smooth=TRUE) # High parts in right spot

plot.gbm(rac.model, c("fuel_management_zones", "ffdi_95_days"), type = "response") # THis looks good
plot.gbm(rac.model, c("fuel_management_zones", "spei12_mean"), type = "response") # THis looks good
plot.gbm(rac.model, c("fuel_management_zones", "spei24_mean"), type = "response") # THis looks good
plot.gbm(rac.model, c("fuel_management_zones", "thunderstorm_days"), type = "response") # THis looks good

plot.gbm(rac.model, c("spei12_mean", "ffdi_95_days"), type = "response") # THis looks good
plot.gbm(rac.model, c("spei24_mean", "ffdi_95_days"), type = "response") # THis looks good
plot.gbm(rac.model, c("rac", "ffdi_95_days"), type = "response") # THis looks good

# No RAC MODEL
model = readRDS("F:/vic_fire_mapping/output_data/lr_0.05_fire_model_tsf.RDS")

summary(model)

## Search for key interactions
gbm.interactions(model)
plot.gbm(model, c("spei24_mean", "spei12_mean"), type = "response") # THis looks good
plot.gbm(model, c("thunderstorm_days", "spei24_mean"), type = "response") # THis looks good
plot.gbm(model, c("kbdi_95_days", "bio18"), type = "response") # THis looks good
plot.gbm(model, c("spei24_mean", "ffdi_95_days"), type = "response") # THis looks good
plot.gbm(model, c("spei24_mean", "bio5"), type = "response") # THis looks good
plot.gbm(model, c("thunderstorm_days", "distance_roads"), type = "response") # THis looks good
plot.gbm(model, c("thunderstorm_days", "spei12_mean"), type = "response") # THis looks good



plot.gbm(model, "fuel_management_zones", type = "response") # THis looks good
plot.gbm(model, "spei12_mean", type = "response", smooth=TRUE) # high parts in right spot but a bit funky
plot.gbm(model, "ffdi_95_days", type = "response", smooth=TRUE) # High parts in right spot but a bit funky
plot.gbm(model, "spei24_mean", type = "response", smooth=TRUE) # High parts in two ends for 24 - fuel?
plot.gbm(model, "thunderstorm_days", type = "response", smooth=TRUE) # High parts in right spot
plot.gbm(model, "tsf", type = "response", smooth=TRUE) # High parts in right spot


library(dismo)
gbm.plot(model, c("fuel_management_zones"), type = "response") # THis looks good
plot.gbm(model, c("fuel_management_zones", "spei12_mean"), type = "response") # THis looks good
plot.gbm(model, c("fuel_management_zones", "spei24_mean"), type = "response") # THis looks good
plot.gbm(model, c("fuel_management_zones", "thunderstorm_days"), type = "response") # THis looks good
plot.gbm(model, c("fuel_management_zones", "tsf"), type = "response") # THis looks good
plot.gbm(model, c("fuel_management_zones", "ffdi_95_days"), type = "response") # THis looks good



library(ggplot2)
library(gbm)

# Simpler method using plot.gbm directly
get_gbm_plot_data <- function(gbm_model, var_name) {
  
  # Use plot.gbm to generate the plot data
  plot_data <- plot.gbm(gbm_model, i.var = var_name, 
                        return.grid = TRUE, type = "response")
  
  # Extract the data
  data.frame(
    variable = plot_data[[var_name]],  # Use variable name as column name
    response = plot_data$y
  )
}

# Usage
response_data <- get_gbm_plot_data(model, "ffdi_95_days")

# Create ggplot
ggplot(response_data, aes(x = variable, y = response)) +
  geom_line(color = "black", size = 1) +
  geom_smooth(method="loess", span = 1, se = FALSE, color = "blue") +
  labs(x = "spei12_mean", 
       y = "Pr(Fire-affected)") +
  theme_minimal()

