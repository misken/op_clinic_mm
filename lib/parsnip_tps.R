

# Let's write a somewhat generic function for automating a model train, predict, summarize cycle.

parsnip_last_fit <- function(model, model_formula, init_split) {
  
  train_data <- analysis(init_split)
  test_data <- assessment(init_split)
  
  result <- parsnip_tps(model, model_formula, train_data, test_data)
  
  rmse_metric_for_df <- list(.metric="rmse",
                             .estimator="standard",
                             .estimate=result$rmse_test,
                             .config="Preprocessor1_Model1")
  
  mae_metric_for_df <- list(.metric="mae",
                            .estimator="standard",
                            .estimate=result$mae_test,
                            .config="Preprocessor1_Model1")
  
  metrics_tibble <- as_tibble(rbind(unlist(rmse_metric_for_df), unlist(mae_metric_for_df)))
  
  metrics_tibble$.estimate <- as.numeric(metrics_tibble$.estimate)
  
  metrics_summary <- as_tibble(bind_cols(init_split$id, tibble(.metrics=metrics_tibble)))
  act_vs_pred_plot <- result$g_act_vs_pred
  
  results <- list(metrics_summary=metrics_summary, act_vs_pred_plot=act_vs_pred_plot)
  
  
}

parsnip_fit_resamples <- function(model, model_formula, resamples, kfold_number, kfold_repeats) {
  
  # Container list for results
  listsize <- kfold_number * kfold_repeats
  metrics_for_tibble <- vector("list", listsize)
  
  for( i in as.numeric(rownames(resamples)) ) {  
    repeatid <- resamples[i, "id"]
    foldid <- resamples[i, "id2"]
    
    repeat_num <- as.numeric(str_extract(repeatid, "\\d+"))
    fold_num <- as.numeric(str_extract(foldid, "\\d+"))
    
    resample <- resamples$splits[[i]]
    train_data <- analysis(resample)
    test_data <- assessment(resample)
    
    fold_results <- parsnip_tps(model, model_formula, train_data, test_data)
    
    rmse_metric_for_df <- list(.metric="rmse",
                               .estimator="standard",
                               .estimate=fold_results$rmse_test,
                               .config="Preprocessor1_Model1")
    
    mae_metric_for_df <- list(.metric="mae",
                              .estimator="standard",
                              .estimate=fold_results$mae_test,
                              .config="Preprocessor1_Model1")
    
    metrics_tibble <- as_tibble(rbind(unlist(rmse_metric_for_df), unlist(mae_metric_for_df)))
    
    metrics_tibble$.estimate <- as.numeric(metrics_tibble$.estimate)
    

    metrics_for_tibble[[i]] <- metrics_tibble
  }
  

  
  results <- as_tibble(bind_cols(in_train_splits, tibble(.metrics=metrics_for_tibble)))
  
}


parsnip_tps <- function(model, model_formula, train_data, test_data,
                        scenario = NULL, method = 'nls') 
                      
{
  model_fit <- 
    model %>% 
    fit(formula = model_formula, data = train_data)
  
  train_predict <- predict(model_fit, new_data = train_data)
  test_predict <- predict(model_fit, new_data = test_data)
  
  y_name <- deparse(model_formula[[2]])
  
  train_y_actual <- as.vector(train_data[, y_name])
  train_y_pred <- train_predict$.pred
  
  test_y_actual <- as.vector(test_data[, y_name])
  test_y_pred <- test_predict$.pred
  
  rmse_train <- MLmetrics::RMSE(train_y_actual, train_y_pred)
  rmse_test <- MLmetrics::RMSE(test_y_actual, test_y_pred)
  
  mae_train <- MLmetrics::MAE(train_y_actual, train_y_pred)
  mae_test <- MLmetrics::MAE(test_y_actual, test_y_pred)
  
  plot_df <- data.frame(test_y_pred, test_y_actual)
  names(plot_df) <- c(paste(y_name,'pred',sep='_'), paste(y_name,'act',sep='_'))
  
  g_title <- "Nonlinear queueing based model"
  
  g <- ggplot(data=plot_df) + ggtitle(g_title) + 
    aes_string(x=names(plot_df)[1], y=names(plot_df)[2]) + 
    coord_obs_pred() +
    geom_point(alpha = .15) + geom_abline(color = "red") + 
    xlab("Actual mean initial wait time (min)") +
    ylab("Metamodel prediction")
  
  results <- list(rmse_train=rmse_train, rmse_test=rmse_test,
                  mae_train=mae_train, mae_test=mae_test,
                  g_act_vs_pred=g, scenario=g_title)

}



