#' @title remove extreme values
#' @param x time series
#' @return x time series with extreme value and negative value filled with NA
#' @export
rmsat <-
function(x){ 
  
  x[x >10000]<-NA
  x[x<0] <-NA
  return(x)
}
 
