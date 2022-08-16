library(plyr)
library(ggplot2)

train_df_withqngapproxs <- read.csv("train_df_withqngapproxs.csv")

# Generate k folds and store in data.frame.

k <- 5
set.seed(14)
folds <- data.frame(Fold=sample(rep(x=1:k, length.out=nrow(train_df_withqngapproxs))), Row=1:nrow(train_df_withqngapproxs))

# Create empty list to store results. The list will be indexed by fold

train_errors <- list()
test_errors <- list()

# Loop over folds
# f <- 1
for(f in 1:k)
{
#   # Split data based on fold
  testrows <- folds[folds$Fold==f, "Row"]
  test_df <- train_df_withqngapproxs[testrows,]
  train_df <- train_df_withqngapproxs[-testrows,]
  
  # Fit logistic regression model
  
  pct_waitq_LogM1 <- glm(mean_pct_waitq_ldr ~ load_ldr * rho_ldr,
                      data=train_df, family=binomial(link="logit"))
  
  pct_waitq_LogM2 <- glm(mean_pct_waitq_ldr ~ load_ldr * rho_ldr + load_pp * rho_pp,
                         data=train_df, family=binomial(link="logit"))

  
  # Make predictions using logistic models
  
  # Model 1
  pct_waitq_LogM1_pred <- predict(pct_waitq_LogM1, newdata=test_df, type="response")
  test_err_logistic_1 <- pct_waitq_LogM1_pred - test_df$mean_pct_waitq_ldr
  
  pct_waitq_LogM1_train_pred <- predict(pct_waitq_LogM1, type="response")
  train_err_logistic_1 <- pct_waitq_LogM1_train_pred - train_df$mean_pct_waitq_ldr
  
  # Model 2
  pct_waitq_LogM2_pred <- predict(pct_waitq_LogM2, newdata=test_df, type="response")
  test_err_logistic_2 <- pct_waitq_LogM2_pred - test_df$mean_pct_waitq_ldr
  
  pct_waitq_LogM2_train_pred <- predict(pct_waitq_LogM2, type="response")
  train_err_logistic_2 <- pct_waitq_LogM2_train_pred - train_df$mean_pct_waitq_ldr
  
  # Use M/G/c approximation to compute prediction error for qng model
  test_err_mgc <- test_df$prob_waitq_ldr_simplemgc_approx - test_df$mean_pct_waitq_ldr
  
  # Use M/G/c approximation to compute "fit" error (really just error on train set) for qng model
  train_err_mgc <- train_df$prob_waitq_ldr_simplemgc_approx - train_df$mean_pct_waitq_ldr
  
  
  test_errors[[f]] <- data.frame(fold=f, scenario=test_df$scenario, 
                                 mean_pct_waitq_ldr=test_df$mean_pct_waitq_ldr, 
                                 pred_logistic_1=pct_waitq_LogM1_pred,
                                 pred_logistic_2=pct_waitq_LogM2_pred,
                                 pred_mgc=test_df$prob_waitq_ldr_simplemgc_approx,
                                 err_logistic_1=test_err_logistic_1, 
                                 err_logistic_2=test_err_logistic_2,
                                 err_mgc=test_err_mgc)
  
  train_errors[[f]] <- data.frame(fold=f, scenario=train_df$scenario, 
                                  mean_pct_waitq_ldr=train_df$mean_pct_waitq_ldr, 
                                  pred_logistic_1=pct_waitq_LogM1$fitted.values,
                                  pred_logistic_2=pct_waitq_LogM2$fitted.values,
                                  pred_mgc=train_df$prob_waitq_ldr_simplemgc_approx,
                                  err_logistic_1=train_err_logistic_1, 
                                  err_logistic_2=train_err_logistic_2,
                                  err_mgc=train_err_mgc)
  
  
}

test_errors_df <- ldply(test_errors, data.frame)
train_errors_df <- ldply(train_errors, data.frame)

# Test errors
ggplot(data=test_errors_df) + aes(x=err_logistic_1) + geom_histogram() + facet_wrap(~fold) + ggtitle("Test")
ggplot(data=test_errors_df) + aes(x=err_logistic_2) + geom_histogram() + facet_wrap(~fold) + ggtitle("Test")
ggplot(data=test_errors_df) + aes(x=err_mgc) + geom_histogram() + facet_wrap(~fold) + ggtitle("Test")

ggplot(data=test_errors_df) + aes(x=mean_pct_waitq_ldr, y=pred_logistic_1) + geom_point() + facet_wrap(~fold) + ggtitle("Test") + geom_abline()
ggplot(data=test_errors_df) + aes(x=mean_pct_waitq_ldr, y=pred_logistic_2) + geom_point() + facet_wrap(~fold) + ggtitle("Test") + geom_abline()
ggplot(data=test_errors_df) + aes(x=mean_pct_waitq_ldr, y=pred_mgc) + geom_point() + facet_wrap(~fold) + ggtitle("Test") + geom_abline()

# Train errors
ggplot(data=train_errors_df) + aes(x=err_logistic_1) + geom_histogram() + facet_wrap(~fold) + ggtitle("Train")
ggplot(data=train_errors_df) + aes(x=err_logistic_2) + geom_histogram() + facet_wrap(~fold) + ggtitle("Train")
ggplot(data=train_errors_df) + aes(x=err_mgc) + geom_histogram() + facet_wrap(~fold) + ggtitle("Train")

ggplot(data=train_errors_df) + aes(x=mean_pct_waitq_ldr, y=pred_logistic_1) + geom_point() + facet_wrap(~fold) + ggtitle("Train") + geom_abline()
ggplot(data=train_errors_df) + aes(x=mean_pct_waitq_ldr, y=pred_logistic_2) + geom_point() + facet_wrap(~fold) + ggtitle("Train") + geom_abline()
ggplot(data=train_errors_df) + aes(x=mean_pct_waitq_ldr, y=pred_mgc) + geom_point() + facet_wrap(~fold) + ggtitle("Train") + geom_abline()

# MSE
mean(train_errors_df$err_mgc ** 2)
mean(train_errors_df$err_logistic_1 ** 2)
mean(train_errors_df$err_logistic_2 ** 2)
mean(test_errors_df$err_logistic_1 ** 2)
mean(test_errors_df$err_logistic_2 ** 2)
mean(test_errors_df$err_mgc ** 2)
mean((train_df_withqngapproxs$prob_waitq_ldr_approx - train_df_withqngapproxs$mean_pct_waitq_ldr) ** 2)


# Clean up
rm(test_df)
rm(train_df)
rm(test_err_logistic_1)
rm(train_err_logistic_1)
rm(test_err_logistic_2)
rm(train_err_logistic_2)
rm(test_err_mgc)
rm(train_err_mgc)
rm(testrows)

