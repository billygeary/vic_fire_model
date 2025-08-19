# Function to fit the model for fire

fit_fire_model = function(data.in, lr){
    cat("Testing: Learning Rate =", lr, "\n")
    
    model <- gbm.step(data = data.in, 
                      gbm.x = 2:10,
                      gbm.y = 1,
                      family = "bernoulli", 
                      tree.complexity = 2, 
                      learning.rate = lr,
                      n.trees = 100,
                      max.trees = 20000,
                      plot.main = FALSE,
                      cv.folds = 5,
                      bag.fraction = 0.75)
    
    message(paste("Model with lr", lr, "complete"))
    
    # Define and check output directory
    output_dir <- "F:/vic_fire_mapping/output_data"
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
    
    # Save the model
    filename.out <- file.path(output_dir, paste0("lr_", lr, "_fire_model.RDS"))
    readr::write_rds(model, file = filename.out)
    
    message("Saved model to: ", filename.out)
}



