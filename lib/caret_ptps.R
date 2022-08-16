

# Let's write a somewhat generic function for automating a model partition, 
# train, predict, summarize cycle.

# See p71 in Applied Predictive Modeling for discussion of repeated train/test partitioning.

# How to extract final model correctly for use on new data?
# http://machinelearningmastery.com/finalize-machine-learning-models-in-r/
# https://stackoverflow.com/questions/30535227/how-to-use-trained-caret-object-to-predict-on-new-data-not-used-while-training
# http://topepo.github.io/caret/model-training-and-tuning.html#pred

library(caret)
library(MLmetrics)

if ( Sys.info()['sysname'] != "Windows") {
  library(doMC)
  registerDoMC(cores = 3)
}


caret_ptps_arglister <- function(df){
  
  # Just using the first row, no matter how many rows in data passed in.
  # Hacky but fine for now.
  argdf <- df[1,]
  
  pm_unit_model_id <- argdf$pm_unit_model_id
  model_formula <- as.formula(argdf$model_formula)
  data <- get(argdf$data)
  method <- argdf$method
  #pm_unit_model <- argdf$pm_unit_model
  
  perf_measure <- argdf$perf_measure
  unit <- argdf$unit
  model <- argdf$model
  
  pct_train_val <- eval(parse(text=argdf$pct_train_val))
  partition.times <- argdf$partition.times
  kfold_number <- argdf$kfold_number
  kfold_repeats <- argdf$kfold_repeats
  seed.partition <- argdf$seed.partition
  seed.resample <- argdf$seed.resample
  extra_args <- argdf$extra_args
  
  arglist <- list(pm_unit_model_id=pm_unit_model_id, model_formula=model_formula, data=data, 
                  method=method,
                  perf_measure=perf_measure, unit=unit, model=model,
                  kfold_number=kfold_number, kfold_repeats=kfold_repeats,
                  pct_train_val=pct_train_val, partition.times=partition.times)
  
  if (!is.na(seed.partition))  {
    arglist[["seed.partition"]] <- seed.partition
  } 
  
  if (!is.na(seed.resample))  {
    arglist[["seed.resample"]] <- seed.resample
  } 
  
  # For now assuming only one additional argument specified.
  
  if (nchar(extra_args) > 0)  {
    arg <- strsplit(extra_args, "=")
    argname <- arg[[1]][1]
    argval <- eval(parse(text=arg[[1]][2]))
    
    arglist[[argname]] <- argval
  } 
  
  return(arglist)
}

caret_ptps <- function(pm_unit_model_id, model_formula, data, method,
                       perf_measure, unit, model,
                       pct_train_val=0.6, partition.times=1,
                       kfold_number=0, kfold_repeats=0,
                       seed.partition=NULL, seed.resample=NULL,
                       ...){
  
  # Container list for results
  listsize <- length(pct_train_val) * partition.times
  results_for_df <- vector("list", listsize) 
  results_objs <- vector("list", listsize) 
  
  # Specify how the model validation will work.
  if (kfold_number > 1) {
    fitControl <- trainControl(## k-fold CV
      method = "repeatedcv",
      number = kfold_number,
      ## repeated how many times
      repeats = kfold_repeats)
  } else {
    fitControl=trainControl(method="None")
  }
  
  # Get y variable name from formula object. The ~ is [[1]], LHS is [[2]] and RHS[[3]].
  y.name <- deparse(model_formula[[2]])
  
  # Outer loop for training set size
  itemnum <- 0
  # Potentially use user provided seed for partitioning.

  for (pct_train in pct_train_val){
    print(pct_train)
    if (seed.partition > 0){
      set.seed(seed.partition) # Each set will use same seed
    }
    # Create split(s) for this pct_train. 
    trainrecs <- createDataPartition(data[[y.name]], p = pct_train, 
                                     list = FALSE, times = partition.times)
    
    # Loop over data partitions
    for (s in seq(1,partition.times)){
      print(s)
      modeling_test_df <- data[-trainrecs[,s], ]
      modeling_train_df <- data[trainrecs[,s], ]
      # Debugging line
      #print(summary(modeling_train_df))
      
      # Train the model on the current partition's training set
      if (seed.resample > 0){
        set.seed(seed.resample) # Each set will use same seed
      }
      fit <- train(model_formula, 
                   data = modeling_train_df, 
                   method = method, 
                   trControl = fitControl,
                   ...)
      
      # Make predictions for the test set
      pred <- predict(fit, newdata=modeling_test_df)

      # Gather actual and predicted values
      train_y_actual <- fit$trainingData$.outcome
      train_y_pred <- fitted.values(fit)
      
      test_y_actual <- modeling_test_df[, y.name]
      test_y_pred <- pred
      
      # Compute RMSE for train and test
      rmse_train <- RMSE(train_y_actual, train_y_pred)
      rmse_test <- RMSE(test_y_actual, test_y_pred)
      
      # Computer other stats such as MAE and MAPE
      mae_train <- MAE(train_y_actual, train_y_pred)
      mae_test <- MAE(test_y_actual, test_y_pred)
      
      mape_train <- MAPE(train_y_actual, train_y_pred)
      mape_test <- MAPE(test_y_actual, test_y_pred)
      
      merr_train <- mean(train_y_pred - train_y_actual)
      merr_test <- mean(test_y_pred - test_y_actual)
      
      # Store key items in lists for later processing
      itemnum <- itemnum + 1
      
      result_for_df <- list(pm_unit_model_id=pm_unit_model_id, ptrain=pct_train, sample=s, 
                            perf_measure=perf_measure, unit=unit, model=model,
                            kfold_number=kfold_number, kfold_repeats=kfold_repeats,
                            partition.times=partition.times,
                            seed.partition=seed.partition, seed.resample=seed.resample,
                            rmse_train = rmse_train,
                            rmse_test = rmse_test,
                            mae_train = mae_train,
                            mae_test = mae_test,
                            mape_train = mape_train,
                            mape_test = mape_test,
                            merr_train = merr_train,
                            merr_test = merr_test)
      
      results_for_df[[itemnum]] <- result_for_df
      
      result_objs <- list(pm_unit_model_id=pm_unit_model_id, 
                          ptrain=pct_train, sample=s,
                          test_y_actual = test_y_actual,
                          test_y_pred = test_y_pred,
                          trainrecs=trainrecs[,s],
                          trained_model = fit)
      
      results_objs[[itemnum]] <- result_objs
    }
  }
  #browser()
  results_list <- list(rmse=results_for_df, fitpred=results_objs)
  return(results_list) 

}



