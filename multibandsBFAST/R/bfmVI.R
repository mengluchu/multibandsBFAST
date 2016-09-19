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
 # a7bandsrm <- aaply(multibandsarr,1,rmsat) #remove extreme value outside valid  range (1-10000)
#  a7bandsrm2 <- aaply(a7bandsrm,1,removedips) # remove low value

  #timemulbands<-timendmi[-as.numeric(attr(na.omit(arr7bands1[1,]), "na.action"))]
  
  bfmic<-try(apply(multibandsarr, 1, uniindex_bfm,myear=monitoryear,history=history,
        type=type, 
        my_dates= timearr, moy=moy), silent=F)
 
  return(bfmic)
}

