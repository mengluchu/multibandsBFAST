#' @title paralle apply, similar to mclapply and mc.calc
#' @param mc.cores the number of cores to use
#' @import parallel
#' @author Meng Lu
#' @note adopt the idea from mc.calc of the bfastSpatial package 
#' @export
mcapply <- function(arr, margin, fun, mc.cores=1, ...) {
  if(mc.cores == 1) { 
    out <- apply(X=arr, MARGIN = margin, FUN = fun,...)
    return(out)
  } else {
    bls <- seq(1, mc.cores)
    blocs <- floor(dim(arr)[2] / mc.cores)
    allblocs1<- unlist(lapply(1: (mc.cores), function(k) blocs*(k-1)+1) )
    allblocs2<- c(unlist(lapply(1: (mc.cores-1), function(k) blocs*(k)) ), dim(arr)[2])
    fun2 <- function(i) {
      b <- arr[,allblocs1[i]:allblocs2[i],]
      out <- apply(b, margin, FUN=fun) # Does this line need an elipsis
      return(out)
    }
    listOut <- unlist(mclapply(X=bls, FUN=fun2, mc.cores=mc.cores))
    return(listOut)
  }
}
 