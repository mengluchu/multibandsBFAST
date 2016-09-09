#' @title sequentially Monitor change with BFAST-PCA
#' @param multibandsarr 3-d array, space, time and spectral bands
#' @param hisweight If True, Use the historical PCA method. If False, use the PCA score method.
#' @param sca If True, Scale the newly aquired data with the whole time series.
#' @param scoreselect if True, select a PC score, specified in pcacomp
#' @param timearr The time of the time dimension of the 3-d array 
#' @param lastordetect Use the last observation or detected change date for validation. Useful for PC score method

#' @return detected Change time for each pixel
#' @import plyr
#' @export 
bfmPCA<-function(multibandsarr,history=c("all", "ROC", "BP"),
                    hisweight=T, 
                    timearr, pcacomp, 
                    moy=1, 
                    scoreselect=F,
                    lastordetect=c("last","detect"),sca=F ){ 
  
  timebfm <-rep(NA,length=dim(multibandsarr)[2])
  
  a7bandsrm<- aaply(multibandsarr,c(1,2),rmsat) #remove extreme value outside valid  range (1-10000)
  a7bandsrm2<-aaply(a7bandsrm,c(1,2),removedips) # remove low value
  
  # timemulbands<-timendmi[-as.numeric(attr(na.omit(t(arr7bands2[,1,])), "na.action"))]
  breakpointx<-NA
  a7bandsrm2<-aperm(a7bandsrm2, c(3,2,1))
  bfmic <- try (apply(a7bandsrm2, 2, pca_bfm, 
                      hisweight= hisweight, 
        history= history, myear=2005,
        my_dates=timearr, moy=moy,
        lastordetect=lastordetect,
        scoreselect=scoreselect, pcacomp=pcacomp,
        sca=sca), silent=F)
  
  #for (i in 1:dim(a7bandsrm2)[2])
  #{ 
   # ta7b<-t(a7bandsrm2[,i,])  
  #  bfmic<- try(pca_bfm(arr=ta7b,hisweight= hisweight, 
  #                      history= history ,myear=2005,
  #                      my_dates=timearr, moy=moy,
  ##                      lastordetect=lastordetect,
  #                      scoreselect=scoreselect, pcacomp=pcacomp,
  #                      sca=sca),silent=F)
    
    #if(class(bfmic)!='try-error'){
    #  if(!is.na(bfmic))
     # {  
    #    timebfm[i]<-bfmic    
     # }
    #}
   
  return(bfmic)  }
#}




 
