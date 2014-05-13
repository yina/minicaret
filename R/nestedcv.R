# nested cross validation.
nestedcv <- function(x, y, 
                     method = "rf",  
                     preProcess = NULL,
                     weights = NULL,
                     metric = ifelse(is.factor(y), "Accuracy", "RMSE"),  
                     maximize = ifelse(metric == "RMSE", FALSE, TRUE),
                     trControl = trainControl(), 
                     tuneGrid = NULL, 
                     tuneLength = 3,
                     nestedrepeats = 1,
                     verbose = 0) {
  
  results <- NULL
  # iterate over 
  for(repeatnum in 1:nestedrepeats) {
    # split into folds
    if(verbose!= 0) {
      cat("Processing repeat iteration", repeatnum, "/", nestedrepeats, "\n")
    }
    cv = createFolds(y, k = trControl$number)

    # iterate over folds
    for(iter in 1:length(cv)) {
      # test indices
      if(verbose!= 0) {
        cat("Processing outer loop iteration", iter, "/", length(cv), "\n")
      }
      test_indices <- cv[[iter]]
      train_indices <- setdiff(seq(length(y)), test_indices)
  
      # train model with caret package
      model <- train(x[train_indices,], 
                     y[train_indices], 
                     method=method,
                     preProcess=preProcess,
                     weights=weights,
                     metric=metric,
                     maximize=maximize,
                     trControl=trControl,
                     tuneGrid=tuneGrid,
                     tuneLength=tuneLength)
    
      # do predictions
      predictions <- predict(model, x[test_indices,], type="raw")
      probValues <- predict(model, newdata=x[test_indices,], type="prob")
      
      # format predictions into form for summary Function
      tmp <- data.frame(pred=predictions,
                        obs=y[test_indices],
                        stringsAsFactors=FALSE)
      tmp <- cbind(tmp, probValues)
      
      thisResample <- trControl$summaryFunction(tmp, lev = levels(y), 
                                                model = method)
      
      thisResample <- c(thisResample, caret:::flatTable(tmp$pred, tmp$obs))
      thisResample <- as.data.frame(t(thisResample))
      
      thisResample$Resample <- iter
       
      # append this result to a list
      show(thisResample)
      results <- rbind(results, thisResample)
      
      
    } # end for(iter in 1:length(cv))
  } # end for(iter in 1:nestedrepeats)
  return(results)
  
  #resamples <- rbind.fill(result[names(result) == "resamples"])
  
  # for each fold
  # with the train set
  # run train from caret package to do grid search 
  # grid search will produce the best model
  
  # run this best model on the test set of documents.
  # calculate performance metrics
  # (what does caret use to calculate these parameters?)
  # contained in trControl$summaryfunction. see how in nominalWorkflow function
  # run this caret function caret:::probFunction
  
  # in the train set, run grid search over parameters of interest.
  # select the best parameter from the grid search and build a final model.
  
  # apply the final model to the test set and record performance
}

