mergeBinaryColumns <- function(inputdf, varnames) {
  tmp <- inputdf[,varnames]
  return(1*(rowSums(tmp)>0))
}