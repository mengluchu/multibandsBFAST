#' @title sequentially Monitor change with BFAST 
#' @param multibandsarr 3-d array, space, time and spectral bands
#' @param timearr The time of the time dimension of the 3-d array 
#' @param moy Period for sequential test. moy is 1 means to monitor every 2 years.
#' @import lubridate
#' @export
bfmVI<-function(multibandsarr,  monitoryear=2005,  
                timearr,
                moy=1, history="all", type="OLS-MOSUM"){ 
  
  # changedate1<-decimal_date(strptime(Change09t ,format="%Y%j"))
  a7bandsrm <- aaply(multibandsarr,2,rmsat) #remove extreme value outside valid  range (1-10000)
  a7bandsrm2 <- aaply(a7bandsrm,2,removedips) # remove low value
  
  timebfm<-c()
  #timemulbands<-timendmi[-as.numeric(attr(na.omit(arr7bands1[1,]), "na.action"))]
  breakpointx<-NA
  bfmic<-try(apply(arr7bandsrm2, 2, uniindex_bfm,myear=monitoryear,history=history,
        type=type, 
        my_dates= timearr, moy=moy), silent=F)
 
  return(bfmic)
}

