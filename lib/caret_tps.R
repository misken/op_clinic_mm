

# Let's write a somewhat generic function for automating a model train, predict, summarize cycle.


caret_tps <- function(model_formula, train_data, test_data, 
                      method, control=trainControl(method="None"),
                      scenario="", ...){
  
  extra_fit_args <- list(...)

  fit <- train(model_formula, 
               data = train_data, 
               method = method, 
               trControl = control, ...)
  
  pred <- predict(fit, newdata = test_data)
  
  y_name <- deparse(model_formula[[2]])
  
  train_y_actual <- fit$trainingData$.outcome
  train_y_pred <- fitted.values(fit)
  
  test_y_actual <- test_data[, y_name]
  test_y_pred <- pred
  
  rmse_train <- rmse(train_y_actual, train_y_pred)
  rmse_test <- rmse(test_y_actual, test_y_pred)
  
  mae_train <- mae(train_y_actual, train_y_pred)
  mae_test <- mae(test_y_actual, test_y_pred)
  
  plot_df <- data.frame(test_y_pred, test_y_actual)
  names(plot_df) <- c(paste(y_name,'pred',sep='_'), paste(y_name,'act',sep='_'))
  
  g_title <- ifelse(length(scenario)>0, scenario, paste(method, y_name))
  
  g <- ggplot(data=plot_df) + ggtitle(g_title) + 
    aes_string(x=names(plot_df)[1], y=names(plot_df)[2]) + geom_point() + geom_abline()
  
  results <- list(caretFit=fit, caretPred=pred, 
                  rmse_train=rmse_train, rmse_test=rmse_test,
                  mae_train=mae_train, mae_test=mae_test,
                  g_act_vs_pred=g, scenario=g_title)

}



