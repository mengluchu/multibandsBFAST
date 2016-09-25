#' @title sequentially Monitor change with BFAST-PCA
#' @param multibandsarr A 3-d array, with spectral bands, space and time as dimensions.
#' @param hisweight If True, Use the historical PCA method. If False, use the PCA score method.
#' @param sca If True, Scale the newly aquired data with the whole time series.
#' @param scoreselect For the PCA score method, if True, automatically select a PC score. If false, pc order is specified in pcacomp. Ignored for the historical PCA method. 
#' @param timearr The time of the time dimension of the 3-d array 
#' @param lastordetect Use the last observation or detected change date for validation. Useful for PC score method
#' @return detected Change time for each pixel
#' @import plyr
#' @export 
bfmPCA <- function(multibandsarr, history = c("all", "ROC", "BP"), hisweight, 
    timearr, pcacomp, moy = 1, myear = 2005, scoreselect, lastordetect = c("last", 
        "detect"), sca = F) {
    timebfm <- rep(NA, length = dim(multibandsarr)[2])
    a7bandsrm <- aaply(multibandsarr, c(1, 2), rmsat)  #remove extreme value outside valid  range (1-10000)
    a7bandsrm2 <- aaply(a7bandsrm, c(1, 2), removedips)  # remove low value
    breakpointx <- NA
    a7bandsrm2 <- aperm(a7bandsrm2, c(3, 2, 1))
    bfmic <- try(apply(a7bandsrm2, 2, pca_bfm, hisweight = hisweight, history = history, 
        myear = myear, my_dates = timearr, moy = moy, lastordetect = lastordetect, 
        scoreselect = scoreselect, pcacomp = pcacomp, sca = sca), silent = F)
    if (class(bfmic) == "try-error") 
        bfmic = -0.1
    return(bfmic)
}
