# leave pair out cross validation
# see airola 2009

lpocv <- function(x, y, 
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
    cv = createPairFolds(y)
    
    # for internal parameter selection
    
    # iterate over folds
    for(iter in 1:length(cv)) {
      # test indices
      if(verbose!= 0) {
        cat("Processing outer loop iteration", iter, "/", length(cv), "\n")
      }
      test_indices <- cv[[iter]]
      train_indices <- setdiff(seq(length(y)), test_indices)
      
      cat(".")
      show(test_indices)
      
      # apply preProcess to trainx
      preProc <- preProcess(x[train_indices,], preProcess)
      train_ex <- predict(preProc, x[train_indices,])
      #show(train_ex)
      
      # train model with caret package
      model <- train(train_ex,
                     y[train_indices], 
                     method=method,
                     preProcess=NULL,
                     weights=weights,
                     metric=metric,
                     maximize=maximize,
                     trControl=trControl,
                     tuneGrid=tuneGrid,
                     tuneLength=tuneLength)
      
      test_ex <- predict(preProc, x[test_indices,])
      #show(test_ex)
      
      # do predictions
      predictions <- predict(model, test_ex, type="raw")
      probValues <- predict(model, newdata=test_ex, type="prob")
      
      tmp <- cbind(probValues, target=y[test_indices])
      
      
      #show(tmp)
      
      diff <- tmp[tmp$target=="pos",]$pos -
              tmp[tmp$target=="neg",]$pos
      
      int <- data.frame(test1=test_indices[1],
                        test2=test_indices[2],
                        diff=diff)
      results <- rbind(results, int)
      
      
    } # end for(iter in 1:length(cv))
    
  } # end for(iter in 1:nestedrepeats)
  auc <- calcH(results$diff)

  return(list(auc=auc, results=results))
}

createPairFolds <- function(y, list = TRUE, returnTrain = FALSE) {
  # get sequence number
  rownum <- seq(y)
  # get every pair
  rowpairs <- combn(rownum, 2)
  rowclass <- combn(y, 2)
  
  # remove pairs where the classifications are the same
  noteqcols <- rowpairs[,!rowclass[1,]==rowclass[2,]]
  
  # turn mismatched columns into a list
  # see http://stackoverflow.com/questions/6819804/how-to-convert-a-matrix-to-a-list-in-r
  out <- lapply(seq_len(ncol(noteqcols)), function(i) noteqcols[,i])
  
  names(out) <- paste("Fold", gsub(" ", "0", format(seq(along = out))), 
                      sep = "")
  out
}

calcH <- function(scores) {
  den <- length(scores)
  scores[scores > 0] <- 1
  scores[scores == 0] <- 0.5
  scores[scores < 0] <- 0
  return(sum(scores)/den)
}
