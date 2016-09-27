#' @title sequentially Monitor change with BFAST, with multicore
#' @param multibandsarr A 2-d array, with spatial and time as dimensions  
#' @param timearr The time of the time dimension of the 3-d array 
#' @param moy Monitoring period for the sequential test. moy is 1 means to monitor every 2 years.
#' @return bfmic Detected change time for each pixel
#' @export

# do the preprocess before run the function
mcbfmVI <- function(multibandsarr, monitoryear = 2005, timearr, moy = 1, history = "all", 
                  type = "OLS-MOSUM",mc.cores=1) {
  bfmic <- try(mcapply(multibandsarr, 1, mc.cores=mc.cores, uniindex_bfm, myear = monitoryear, history = history, 
                     type = type, my_dates = timearr, moy = moy), silent = F)
  if (class(bfmic) == "try-error") 
    bfmic = -0.1
  return(bfmic)
}
