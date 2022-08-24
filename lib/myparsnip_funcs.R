
#' Mimic the basic functionality of tune::last_fit()
#' 
#' @param model A parsnip model object
#' @param model_formula A formula for the `model`
#' @return A list containing the metrics summary tibble and a ggplot
myparsnip_last_fit <- function(model, model_formula, init_split) {
  
  # Get training and test data from init_split
  train_data <- analysis(init_split)
  test_data <- assessment(init_split)
  
  # Do the train, predict score this model instance
  result <- myparsnip_tps(model, model_formula, train_data, test_data)
  
  # Mimic the .metrics structure from tune as lists
  rmse_metric_for_df <- list(.metric="rmse",
                             .estimator="standard",
                             .estimate=result$rmse_test,
                             .config="Preprocessor1_Model1")
  
  mae_metric_for_df <- list(.metric="mae",
                            .estimator="standard",
                            .estimate=result$mae_test,
                            .config="Preprocessor1_Model1")
  
  # Convert the metrics lists to a tibble
  metrics_tibble <- as_tibble(rbind(unlist(rmse_metric_for_df), unlist(mae_metric_for_df)))
  
  # Make sure .estimate is numeric
  metrics_tibble$.estimate <- as.numeric(metrics_tibble$.estimate)
  
  # Bind in the split id with the metrics tibble
  metrics_summary <- as_tibble(bind_cols(init_split$id, tibble(.metrics=metrics_tibble)))
  
  # Pull out the plot returned from myparsnip_tps
  act_vs_pred_plot <- result$g_act_vs_pred
  
  # Combine the metrics summary and the plot into a results list to return
  results <- list(metrics_summary=metrics_summary, act_vs_pred_plot=act_vs_pred_plot)
}

#' Mimic the basic functionality of tune::fit_resamples()
#' 
#' @param model A parsnip model object
#' @param model_formula A formula for the `model`
#' @param resamples A split object created by rsample::vfold_cv()
#' @param kfold_number Number of folds in each repeat of cv process
#' @param kfold_repeats Numer of repeats of the cv process
#' @return A tibble similar in structure to that returned by tune::fit_resamples() but only containing metrics 
myparsnip_fit_resamples <- function(model, model_formula, resamples, kfold_number, kfold_repeats) {
  
  # Setup container list for results
  listsize <- kfold_number * kfold_repeats
  metrics_for_tibble <- vector("list", listsize)
  
  # Iterate over the splits
  for( i in as.numeric(rownames(resamples)) ) {  
    
    # Get repeat and fold info for this split
    repeatid <- resamples[i, "id"]
    foldid <- resamples[i, "id2"]
    repeat_num <- as.numeric(str_extract(repeatid, "\\d+"))
    fold_num <- as.numeric(str_extract(foldid, "\\d+"))
    
    # Get training and test data for this split
    resample <- resamples$splits[[i]]
    train_data <- analysis(resample)
    test_data <- assessment(resample)
    
    # Do the train, predict score this model instance and data split
    fold_results <- myparsnip_tps(model, model_formula, train_data, test_data)
    
    # Mimic the .metrics structure from tune as lists
    rmse_metric_for_df <- list(.metric="rmse",
                               .estimator="standard",
                               .estimate=fold_results$rmse_test,
                               .config="Preprocessor1_Model1")
    
    mae_metric_for_df <- list(.metric="mae",
                              .estimator="standard",
                              .estimate=fold_results$mae_test,
                              .config="Preprocessor1_Model1")
    # Convert the metrics lists to a tibble
    metrics_tibble <- as_tibble(rbind(unlist(rmse_metric_for_df), unlist(mae_metric_for_df)))
    
    # Make sure .estimate is numeric
    metrics_tibble$.estimate <- as.numeric(metrics_tibble$.estimate)
    
    # Store this metrics tibble in the container
    metrics_for_tibble[[i]] <- metrics_tibble
  }
  
  # Convert results container to a tibble containing the split info
  results <- as_tibble(bind_cols(in_train_splits, tibble(.metrics=metrics_for_tibble)))
  
}

#' Do a single train, predict score for this model instance
#' 
#' @param model A parsnip model object
#' @param model_formula A formula for the `model`
#' @param train_data Training dataframe
#' @param test_data Number of folds in each repeat of cv process
#' @param kfold_repeats Numer of repeats of the cv process
#' @return A tibble similar in structure to that returned by tune::fit_resamples() but only containing metrics 

myparsnip_tps <- function(model, model_formula, train_data, test_data,
                        pm_string = "mean initial wait time (min)",
                        plot_title = "Nonlinear queueing based model") 
                      
{
  # Fit the model
  model_fit <- 
    model %>% 
    fit(formula = model_formula, data = train_data)
  
  # Make predicitons on train and test
  train_predict <- predict(model_fit, new_data = train_data)
  test_predict <- predict(model_fit, new_data = test_data)
  
  # Pull out dependent variable from model_formula
  y_name <- deparse(model_formula[[2]])
  
  # Collect actual and predicted from train and test
  train_y_actual <- as.vector(train_data[, y_name])
  train_y_pred <- train_predict$.pred
  test_y_actual <- as.vector(test_data[, y_name])
  test_y_pred <- test_predict$.pred
  
  # Compute rmse and mae
  rmse_train <- MLmetrics::RMSE(train_y_actual, train_y_pred)
  rmse_test <- MLmetrics::RMSE(test_y_actual, test_y_pred)
  mae_train <- MLmetrics::MAE(train_y_actual, train_y_pred)
  mae_test <- MLmetrics::MAE(test_y_actual, test_y_pred)
  
  # Create plot of actual vs predicted on test
  plot_df <- data.frame(test_y_pred, test_y_actual)
  names(plot_df) <- c(paste(y_name,'pred',sep='_'), paste(y_name,'act',sep='_'))
  
  g_title <- plot_title
  
  g <- ggplot(data=plot_df) + ggtitle(g_title) + 
    aes_string(x=names(plot_df)[1], y=names(plot_df)[2]) + 
    coord_obs_pred() +
    geom_point(alpha = .15) + geom_abline(color = "red") + 
    xlab(sprintf("Actual %s", pm_string)) +
    ylab("Metamodel prediction")
  
  results <- list(rmse_train=rmse_train, rmse_test=rmse_test,
                  mae_train=mae_train, mae_test=mae_test,
                  g_act_vs_pred=g, scenario=plot_title)

}



