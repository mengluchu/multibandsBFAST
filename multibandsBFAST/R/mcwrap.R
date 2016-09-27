#' @title wrap for PCA, running different dataset, use multicore
#' @param multibandsarrno the second dataset
#' @return time of change for each variable 
#' @import plyr
#' @export 
mcwrap <- function(multibandsarr, multibandsarrno, timearr, history, lastordetect, 
                 hisweight, pcacomp, moy, scoreselect, sca, mc.cores) {
  a1 <- mcbfmPCA(multibandsarr = multibandsarr, timearr = timearr, history = history, 
               lastordetect = lastordetect, hisweight = hisweight, pcacomp = pcacomp, moy = moy, 
               scoreselect = scoreselect, sca = sca, mc.cores=mc.cores)
  
  a2 <- mcbfmPCA(multibandsarr = multibandsarrno, timearr = timearr, history = history, 
               lastordetect = lastordetect, hisweight = hisweight, pcacomp = pcacomp, moy = moy, 
               scoreselect = scoreselect, sca = sca, mc.cores=mc.cores)
  
  res <- c(a1, a2)
  return(res)
}
