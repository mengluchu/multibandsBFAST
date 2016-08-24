#' @title remove extreme values
#' @param x
#' @export
rmsat <-
function(x){ 
  
  x[x >10000]<-NA
  x[x<0] <-NA
  return(x)
}
