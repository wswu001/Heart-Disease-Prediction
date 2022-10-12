########## Model Validation ########## 
## Create a k fold cross validation function
k_fold <- function(k, glm_formula, data, seed){
  set.seed(seed)
  size_of_test = ceiling(nrow(data) / k) #size of each testing dataset
  test_indexes = matrix(nrow=k-1, ncol=size_of_test) #the indexes that will make up the test dataset for each fold
  total_rows = c(1:nrow(heart))
  for(i in c(1:k)){
    if(length(total_rows) < size_of_test){
      leftovers = total_rows #when the dataset isnt divisible by size_of_test there will be leftovers
    }
    else{
      idx = sample(total_rows, size_of_test, replace = FALSE)
      total_rows = setdiff(total_rows, idx)
      test_indexes[i,] = idx 
    }
  }
  accuracy <- c()
  precision <- c()
  recall <- c()
  f_measure <- c()
  for(i in c(1:k)){
    if (i == k){ #after all the full folds are done we have to run the leftovers as test data so every observation is able to be part of the test data
      test_data = data[leftovers, ]
      train_data = data[-leftovers,]
    }
    else{
      test_data = data[test_indexes[i,], ]
      train_data = data[-test_indexes[i,],]
    }
    
    glm_fit <- glm(as.formula(glm_formula), data = train_data, family="binomial") #fit model with formula that was passed in 
    response_v <- as.character(attributes(glm_fit$terms)$variables[[2]])
    
    predicted <- predict(glm_fit, test_data, type="response") #predictions of the model on the test data this fold
    confusion <- table(ifelse(predicted > 0.5, 1, 0),
                       test_data[[response_v]],
                       dnn = c("Predicted","True"))
    #performance metrics for the current fold
    accuracy[i] <- sum(diag(confusion)) / sum(confusion)
    precision[i] <- confusion[2,2] / sum(confusion[2,])
    recall[i] <- confusion[2,2] / sum(confusion[,2])
    f_measure[i] <- (2 *(precision[i] * recall[i])) / (precision[i] + recall[i])
  }
  result <- setNames(c(mean(accuracy), mean(precision), mean(recall), mean(f_measure)), c("Accuracy", "Precision", "Recall", "F-measure"))
  return(result)
  
}

## k-fold cross validation on the models
folds = 10

step.bic.kfold = k_fold(folds, step.BIC.formula, heart, 12082021)
step.aic.kfold = k_fold(folds, step.AIC.formula, heart, 12082021)
full.model.kfold = k_fold(folds, HeartDisease~., heart, 12082021)

valid.df = data.frame(Model = c("Full Model", "BIC Model", "AIC Model"),
                      "Num_predictors(p)" = c(11, 7, 8),
                      Accuracy = c(full.model.kfold[1], step.bic.kfold[1], step.aic.kfold[1]),
                      Precision = c(full.model.kfold[2], step.bic.kfold[2], step.aic.kfold[2]),
                      Recall = c(full.model.kfold[3], step.bic.kfold[3], step.aic.kfold[3]), "F1_Score" =  c(full.model.kfold[4], step.bic.kfold[4], step.aic.kfold[4]))

require(kableExtra)

valid.df %>%
  kbl() %>%
  kable_styling()

final.model = glm(as.formula(step.BIC.formula), data = heart, family = "binomial")
summary(final.model)

predicted.data <- data.frame(prob.of.HeartDisease = final.model$fitted.values, HeartDisease = heart$HeartDisease)

predicted.data <- predicted.data[order(predicted.data$prob.of.HeartDisease, decreasing = FALSE), ]

predicted.data$index <- c(1:nrow(heart))

final.pred = ggplot(data=predicted.data, aes(x=index, y=prob.of.HeartDisease)) + geom_point(aes(color=HeartDisease), alpha = 1, shape = 4, stroke = 2) + xlab("Index") + ylab("Predicted Probability of having Heart Disease") + labs(title = "Final Model Predictions")

ggsave("Final Model Predictions.pdf", final.pred)
