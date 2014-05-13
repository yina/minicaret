constructTfIdf <- function(inputdf) {
  colnames <- names(inputdf)
  
  idf <- log(nrow(inputdf)/colSums(inputdf))
  tf <- as.matrix(inputdf) / rowSums(inputdf)
  tmp <- as.matrix(tf) %*% diag(idf)
  ret <- as.data.frame(tmp)
  names(ret) <- colnames
  return(ret)
}