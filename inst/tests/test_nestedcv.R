# test nestedcv

loadPima <- function() {
  load("data/pima.Rd")
  return(Pima)
}

loadHeartScale <- function() {
  data <- read.matrix.csr("data/heart_scale")
  return(data)
}

test_that("nestedcv with dense data", {
  set.seed(100)
  d <- loadPima()
  # grid search over cost = 1 only
  grid <- expand.grid( .C = c(1))
  
  tr.control <- trainControl(number = 3, 
                             repeats = 3, 
                             method = "cv",
                             classProbs = TRUE,
                             summaryFunction = twoClassSummary)
  results <- nestedcv(d[,1:7],d[,8], 
                      method="svmLinear",
                      #preProcess=c("range"),
                      trControl=tr.control,
                      tuneGrid = grid,
                      metric="ROC",
                      nestedrepeats=1)
  averages <- colMeans(results)
  expect_equivalent(round(averages[1], digits=2), 0.85)
})

test_that("nestedcv with svm sparse converted to dense data", {
  set.seed(100)
  
  d <- loadHeartScale()
  grid <- expand.grid( .C = c(1))
  x <- as.matrix(d[[1]])[,1:13]
  
  y <- d[[2]]
  y <- as.character(y)
  y[y=="+1"] <- "Yes"
  y[y=="-1"] <- "No"
  y <- as.factor(y)
  
  tr.control <- trainControl(number = 3, 
                             repeats = 3, 
                             method = "cv",
                             classProbs = TRUE,
                             summaryFunction = twoClassSummary)
  results <- nestedcv(x,y, 
                      method="svmLinear",
                      trControl=tr.control,
                      tuneGrid = grid,
                      metric="ROC",
                      nestedrepeats=1)
  averages <- colMeans(results)
  expect_equivalent(round(averages[1], digits=2), 0.91)
})

test_that("merge two binary columns", {
  tmp <- data.frame(a=c(1,0,1,0), b=c(1,1,0,0))
  tmp$c <- mergeBinaryColumns(tmp, c("a", "b"))
  
  true <- data.frame(a=c(1,0,1,0), b=c(1,1,0,0),c=c(1,1,1,0))
  expect_equal(tmp, true)
})

test_that("merge three binary columns", {
  tmp <- data.frame(fir=c(1,1,0,1,0,0,1,0), 
                    sec=c(1,1,1,0,0,1,0,0),
                    thi=c(1,0,1,1,1,0,0,0))
  tmp$fou <- mergeBinaryColumns(tmp, c("fir", "sec", "thi"))
  
  true <- data.frame(fir=c(1,1,0,1,0,0,1,0), 
                    sec=c(1,1,1,0,0,1,0,0),
                    thi=c(1,0,1,1,1,0,0,0),
                    fou=c(1,1,1,1,1,1,1,0))
  expect_equal(tmp, true)
})

test_that("throw error with non existent column", {
  tmp <- data.frame(a=c(1,0,1,0), b=c(1,1,0,0))
  expect_error(mergeBinaryColumns(tmp, c("a", "blue")))
})

test_that("test tf-idf code with dense matrix", {
  tmp <- data.frame(a=c(1,1,0),b=c(1,0,1),c=c(0,0,1))
  ret <- constructTfIdf(tmp)
  true <- data.frame(a=c(0.203, 0.405, 0), b=c(0.203, 0, 0.203), c=c(0,0,0.549))
  
  expect_equivalent(round(ret, digits=3), true)
})

