#' @title sequentially monitor change with BFAST-PCA
#' @param arr7bansd2 4-d array, longitude, latitue, time and spectral bands
#' @param hisweight if T, use the historical PCA method. If False, use the PCA score method.
#' @param sca if True scale the newly aquired data with the whole time series.
#' @param timearr the time of the time dimension of the 4-d array 
#' @return detected change time for each pixel
#' @export 
bfmPCAALL<-function(arr7bands2,history=c("all", "ROC", "BP"),
                    hisweight=T, 
                    timearr, pcacomp, 
                    moy=1, 
                    scoreselect=F,
                    lastordetect=c("last","detect"),sca=F,plot=F){ 
  
  timebfm <-rep(NA,length=dim(arr7bands2)[2])
  
  a7bandsrm<- aaply(arr7bands2,c(1,2),rmsat) #remove extreme value outside valid  range (1-10000)
  a7bandsrm2<-aaply(a7bandsrm,c(1,2),removedips) # remove low value
  
  # timemulbands<-timendmi[-as.numeric(attr(na.omit(t(arr7bands2[,1,])), "na.action"))]
  breakpointx<-NA
  
  for (i in 1:dim(a7bandsrm2)[2])
  { 
    ta7b<-t(a7bandsrm2[,i,])  
    bfmic<- try(ybfastmonitorPCA(arr=ta7b,hisweight= hisweight, history= history ,myear=2005,my_dates=timearr, moy=moy,plot=plot,lastordetect=lastordetect, scoreselect=scoreselect, pcacomp=pcacomp, sca=sca),silent=F)
    
    if(class(bfmic)!='try-error'){
      if(!is.na(bfmic))
      {  
        timebfm[i]<-bfmic    
      }
    }
  } 
  return(timebfm)  
}
