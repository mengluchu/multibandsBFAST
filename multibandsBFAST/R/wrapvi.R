#' @title  wrap for univariable
#' @return time of change for each variable 
#' @export
wrapVI <- function(multibandsarr, multibandsarrno, timearr, history, pcacomp, 
    moy) {
    a1 <- bfmVI(multibandsarr = multibandsarr, timearr = timearr, history = history, 
        moy = moy)
    a2 <- bfmVI(multibandsarr = multibandsarrno, timearr = timearr, history = history, 
        moy = moy)
    res <- c(a1, a2)
    return(res)
}
