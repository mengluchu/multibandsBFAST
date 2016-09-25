#' @title sequentially Monitor change with BFAST 
#' @param multibandsarr 3-d array, space, time and spectral bands
#' @param timearr The time of the time dimension of the 3-d array 
#' @param moy Period for sequential test. moy is 1 means to monitor every 2 years.
#' @export

# do the preprocess before run the function
bfmVI<-function(multibandsarr,  monitoryear=2005,  
                timearr,
                moy=1, history="all", type="OLS-MOSUM"){ 
    
  bfmic<-try(apply(multibandsarr, 1, uniindex_bfm,myear=monitoryear,history=history,
        type=type, 
        my_dates= timearr, moy=moy), silent=F)
  
  if(class(bfmic)=='try-error')
    bfmic = -0.1
  
  return(bfmic)
}

