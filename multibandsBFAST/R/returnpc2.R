#' @title return PC-greenness and selected PC, for reproducing the results.
#' @return re book keeping the index of omited value
#' @return PChistsauto score of the Historical PCA
#' @return PChists2 score of the PC greenness
#' @import plyr
#' @export
returnpc2 <- function(inputarr, timearr, loca, preprocess = T, monitoryear) {
    if (preprocess) {
        a7bandsrm <- aaply(inputarr, c(1, 2), rmsat)  #remove extreme value outside valid  range (1-10000)
        a7bandsrm2 <- aaply(a7bandsrm, c(1, 2), removedips)  # remove low value
    } else a7bandsrm2 = inputarr
    
    arr <- na.omit(t(a7bandsrm2[, loca, ]))
    re <- attributes(arr)$na.action
    arrbandsrm <- t(a7bandsrm2[, loca, ])
    
    # historical period
    PCAhis <- arrbandsrm[(round(decimal_date(timearr), digits = 6) < monitoryear), 
        ]
    
    # using historical period to compute pc loading
    fit <- prcomp(na.omit(PCAhis), scale. = T)
    
    # the PC historical method selected PC
    pcacompauto <- which.max(abs(apply(fit$rotation[1:3, ] - fit$rotation[4:6, 
        ], 2, sum)))
    
    # the PC-greenness: In the Bolivian site, the time series are more
    # homogenuous.  The PC greenness are mostly the PC2.  so for simplicity
    # only select between PC2 and PC3.
    
    pcacompauto2 <- which.max(abs(fit$rotation[4, 2:3] - fit$rotation[6, 
        2:3])) + 1
    
    # to further avoid mistakenly select PC, if PC-greenness select the same
    # PC as historical PCA method, change the order (didnt describ in the
    # paper).
    
    if (pcacompauto == pcacompauto2) 
        pcacompauto = ifelse(pcacompauto2 == 3, 2, 3)
    
    PChweightauto <- fit$rotation[, pcacompauto]
    PC2hweightauto <- fit$rotation[, pcacompauto2]
    
    PChistsauto <- drop(arr %*% PChweightauto)
    PChists2 <- drop(arr %*% PC2hweightauto)
    
    res <- list(re, PChistsauto, PChists2)
    
    return(res)
}
