

# Let's write a somewhat generic function for automating a model train, predict, summarize cycle.


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
  
  g_title <- ifelse(length(scenario)>0, scenario, paste(method, y_name))
  
  g <- ggplot(data=plot_df) + ggtitle(g_title) + 
    aes_string(x=names(plot_df)[1], y=names(plot_df)[2]) + geom_point() + geom_abline()
  
  results <- list(rmse_train=rmse_train, rmse_test=rmse_test,
                  mae_train=mae_train, mae_test=mae_test,
                  g_act_vs_pred=g, scenario=g_title)

}



