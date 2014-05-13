loadPima <- function() {
  load("data/pima.Rd")
  return(Pima)
}

loadHeartScale <- function() {
  data <- read.matrix.csr("data/heart_scale")
  return(data)
}

loadSynthetic <- function() {
  data <- data.frame(var1=c(0.2,0.25,0.3,0.6,0.7,0.8,0.81, 0.75),
                     var2=c(0.2,0.25,0.3,0.6,0.7,0.8,0.81,0.75),
                     type=c("neg", "neg", "neg", "pos", "pos", "pos", "neg", "pos"))
  return(data)
}

test_that("lpocv with synthetic data", {
  set.seed(100)
  d <- loadSynthetic()
  # grid search over cost = 1 only
  grid <- expand.grid( .C = c(1))
  
  tr.control <- trainControl(number = 1, 
                             repeats = 1, 
                             method = "none",
                             classProbs = TRUE,
                             summaryFunction = twoClassSummary)
   
  results <- lpocv(d[,1:2],d[,3],
                      method="svmLinear",
                      preProcess=c("range"),
                      trControl=tr.control,
                      tuneGrid = grid,
                      metric="ROC",
                      nestedrepeats=1)
  
  expect_equivalent(results$auc, 0.75)
})

# this test has an AUC of less than 0.5 because of the mixed
# nature of the positive and negative distributions. The
# positive result ends up occuring less than the negative
# result.
test_that("lpocv with synthetic data auc less than 0.5", {
  set.seed(100)
  d <- loadSynthetic()
  d$type <- c("pos", "neg", "pos", "neg", "pos", "neg", "pos", "neg")
  d$type <- as.factor(d$type)

  # grid search over cost = 1 only
  grid <- expand.grid( .C = c(1))
  
  tr.control <- trainControl(number = 1, 
                             repeats = 1, 
                             method = "none",
                             classProbs = TRUE,
                             summaryFunction = twoClassSummary)
  
  results <- lpocv(d[,1:2],d[,3],
                   method="svmLinear",
                   preProcess=c("range"),
                   trControl=tr.control,
                   tuneGrid = grid,
                   metric="ROC",
                   nestedrepeats=1)
  
  expect_equivalent(round(results$auc, digits=2), 0.34)
})

# test_that("lpocv with real data", {
#   set.seed(100)
#   
#   d <- loadPima()  
#   d <- head(d, 15)
#   d$type <- as.character(d$type)
#   d$type[d$type=="Yes"] <- "pos"
#   d$type[d$type=="No"] <- "neg"
#   d$type <- as.factor(d$type)
#   
#   # grid search over cost = 1 only
#   grid <- expand.grid( .C = c(1))
#   
#   tr.control <- trainControl(number = 1, 
#                              repeats = 1, 
#                              method = "none",
#                              classProbs = TRUE,
#                              summaryFunction = twoClassSummary)
#   
#   results <- lpocv(d[,1:7],d[,8],
#                    method="svmLinear",
#                    preProcess=c("range"),
#                    trControl=tr.control,
#                    tuneGrid = grid,
#                    metric="ROC",
#                    nestedrepeats=1)
#   
#   ans <- calcH(results$diff)
#   show(results)
#   show(ans)
# })

test_that("calcH with scores", {
  scores <- c(0.5, -0.2, 0)
  auc <- calcH(scores)
  expect_equivalent(auc, 0.5)
})