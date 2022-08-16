# The ptps_ldr_wdownstream_results.RData file has a ptps.models_list object
# that doesn't contain pct_train nor partition sample id. Each scenario
# has a list of 100 items (5 pct_train vals X 20 partitions)


pct_train_list <- seq(0.5,0.9,0.1)
for (s in 1:length(ptps.models_list)) {
  
  for (i in 1:100) {
    which_pct_train <- floor((i-1)/20) + 1
    pct_train <- pct_train_list[[which_pct_train]]
    partition <- i-(which_pct_train-1)*20
    print(c(s, i, pct_train, partition))
    
    # Following blows up in middle. Have feeling will be related
    # to some list memory allocation issue. Just rerun the exp.
    #ptps.models_list[[s]][[i]][["pct_train"]] <- pct_train
    #ptps.models_list[[s]][[i]][["partition"]] <- partition
  }
  
}