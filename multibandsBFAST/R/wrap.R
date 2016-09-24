#' @title  wrap for running different dataset
#' @param multibandsarrno the second dataset
#' @import plyr
#' @export 
wrap<-function(multibandsarr ,multibandsarrno ,timearr ,history ,
               lastordetect, hisweight, pcacomp, moy, scoreselect, sca )
{
  a1<-bfmPCA(multibandsarr=multibandsarr,timearr=timearr,history=history,
             lastordetect = lastordetect, hisweight=hisweight, 
             pcacomp=pcacomp, 
             moy=moy, 
             scoreselect=scoreselect,
             sca=sca)
  
  a2<-bfmPCA(multibandsarr=multibandsarrno,timearr=timearr,history=history,
             lastordetect = lastordetect, hisweight=hisweight, 
             pcacomp=pcacomp, 
             moy=moy, 
             scoreselect=scoreselect,
             sca=sca)
  
  res<-c(a1,a2)
  return(res)
}
