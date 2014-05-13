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

# if (is.null(trControl$index)) {
#   trControl$index <- switch(tolower(trControl$method), 
#                             oob = NULL, alt_cv = , cv = createFolds(y, trControl$number, 
#                                                                     returnTrain = TRUE), 
#                             repeatedcv = , adaptive_cv = createMultiFolds(y, 
#                                                                           trControl$number, trControl$repeats), 
#                             loocv = createFolds(y, length(y), returnTrain = TRUE), 
#                             
#                             boot = , boot632 = , 
#                             adaptive_boot = createResample(y, trControl$number), 
#                             
#                             test = createDataPartition(y, 1, trControl$p), lgocv = createDataPartition(y, 
#                                                                                                        trControl$number, trControl$p), timeslice = createTimeSlices(seq(along = y), 
#                                                                                                                                                                     initialWindow = trControl$initialWindow, horizon = trControl$horizon, 
#                                                                                                                                                                     fixedWindow = trControl$fixedWindow)$train)
# }