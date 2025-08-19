# Function for fitting iterative BRTs to find learning rate and ntrees that work best

# Function modified version of function by Charlie Hart for RAC

fit_iterative_brt = function(data.in, initialLearningRate, seed, max_trees = 10000){
  
  # Initialize values
  ntrees <- 0
  modelBRT <- NULL
  counter <- 0
  learningRate <- initialLearningRate
  
  # Iterate up to 10 times or until optimal ntrees is found
  while ((is.null(modelBRT) | ntrees < 1000 | ntrees >= max_trees) & counter < 10) {
    
    if (counter >= 1) {
      # Adjust learning rate:
      if (is.null(modelBRT) | ntrees < 1000) {
        learningRate <- learningRate / 2  # Decrease LR if too few trees
      } else if (ntrees >= max_trees) {
        learningRate <- learningRate * 2  # Increase LR if trees hit max
        ntrees <- 0  # Reset trees to force refitting
      }
    }
    
    message(paste("Iteration:", counter + 1, "| Learning Rate:", learningRate))
    
    # Fit model with set seed
    withr::with_seed(seed, {
      modelBRT <- 
        dismo::gbm.step(data = data.in,
                        gbm.x = 5:ncol(data.in),
                        gbm.y = 4,
                        family = "bernoulli",
                        tree.complexity = 2,
                        #fold.vector = data.in$spatial_temporal_folds,
                        n.folds = 15,
                        #length(unique(data.in$spatial_temporal_folds)),
                        learning.rate = learningRate,
                        bag.fraction = 0.75,
                        max.trees = max_trees,
                        plot.main = FALSE)})
    
    # Check if model fitted correctly
    if (!is.null(modelBRT)) {
      ntrees <- modelBRT$n.trees
      message(paste("Model fitted with", ntrees, "trees"))
      
      # Force refitting if it maxed out at 10,000
      if (ntrees >= max_trees) {
        message("Model maxed out at", max_trees, "trees, increasing learning rate...")
      } else {
        break  # Stop if we found an optimal model
      }
    } else {
      message("Model failed, adjusting learning rate...")
    }
    
    counter <- counter + 1
  }
  
  if (is.null(modelBRT)) {
    warning("Failed to fit a valid model after 10 attempts.")
  } else {
    message("Final model fitted with learning rate:", learningRate)
    # Define and check output directory
    output_dir <- "F:/vic_fire_mapping/output_data"
    # Save the model
    filename.out <- file.path(output_dir, paste0("lr_", learningRate, "_fire_model_RAC.RDS"))
    readr::write_rds(modelBRT, file = filename.out)
    
    message("Saved model to: ", filename.out)
    return(modelBRT)
    }
}
