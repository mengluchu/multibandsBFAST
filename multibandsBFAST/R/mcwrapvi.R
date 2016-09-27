#' @title  wrap for univariable, with multicores
#' @return time of change for each variable 
#' @export
mcwrapVI <- function(multibandsarr, multibandsarrno, timearr, history, pcacomp, moy, mc.cores=1) {
    a1 <- mcbfmVI(multibandsarr = multibandsarr, timearr = timearr, history = history, 
        moy = moy, mc.cores=mc.cores)
    a2 <- mcbfmVI(multibandsarr = multibandsarrno, timearr = timearr, history = history, 
        moy = moy, mc.cores=mc.cores)
    res <- c(a1, a2)
    return(res)
}
