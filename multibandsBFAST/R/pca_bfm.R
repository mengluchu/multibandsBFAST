#' @title Integrate PCA in bfast monitor
#' @param arr An imput array
#' @import zoo
#' @import bfast
#' @return detected Change time for each pixel
#' @export
pca_bfm <- function(arr, hisweight, pcacomp, myear, history = c("all"), 
    my_dates, scoreselect, lastordetect, minumum_observations = 15, sca, 
    type = "OLS-MOSUM", moy = 1) {
    omarr <- na.omit(arr)
    ondtime <- my_dates[-attributes(omarr)$na.action]  # not ordered but dense
    densetime <- sort(ondtime)  # ordered dense time   
    breakpointx <- NA
    breakpointx2 <- NA
    if (length(densetime) > minumum_observations) {
        historyPeriod2 <- window(densetime, endTime = strptime(myear, format = "%Y"))  # control time length
        if (length(historyPeriod2) > 6) {
            atim1 <- c(round(decimal_date(densetime), digits = 0))  #by year
            atim2 <- unique(atim1)
            atimex <- subset(atim2, atim2 >= myear)  #time after 2005
            acoun <- 1
            tim1 <- c(round(decimal_date(densetime), digits = 6))  #by date
            tim2 <- unique(tim1)
            timex <- subset(tim2, tim2 >= myear)  #time after 2005
            coun <- 1
            while (is.na(breakpointx) & coun < length(timex)) {
                stmon <- timex[coun]  # for each time step
                stmon2 <- atimex[acoun]
                ## this is for one value dtat2 <- subset(dtat, round (dtat$my_date ,
                ## digits = 6) < stmon) # control time length
                inputPCAarr <- omarr[(round(decimal_date(ondtime), digits = 6) < 
                  stmon), ]  # unordered, select data in monitor
                timebfast <- ondtime[(round(decimal_date(ondtime), digits = 6) < 
                  stmon)]  # unordered
                # not ordered so request book-keeping PCA came in
                if (hisweight) {
                  PCAhis <- omarr[(round(decimal_date(ondtime), digits = 6) < 
                    stmon2), ]  # using historical period to compute pc loading  
                  fit <- prcomp(PCAhis, scale. = T)
                  # select eigen vector
                  pcacomp <- which.max(abs(apply(fit$rotation[1:3, ] - fit$rotation[4:6, 
                    ], 2, sum)))
                  PCweight <- fit$rotation[, pcacomp]
                  inputPCAarr[inputPCAarr == "-Inf"] <- NA
                  # PCts<- scale(inputPCAarr) %*% PCweight #scale multiply by weight
                  if (sca) 
                    inputPCAarr <- apply(inputPCAarr, 2, scale)  #scale the data
                  PCts <- inputPCAarr %*% PCweight  #original  multiply by weight(unscaled)
                  PCts <- PCts[, 1]
                } else {
                  # PC score
                  fit <- prcomp(inputPCAarr, scale. = T)
                  if (scoreselect) {
                    pcacomp <- which.max(abs(apply(fit$rotation[1:3, ] - 
                      fit$rotation[4:6, ], 2, sum)))
                  }
                  if (sca) 
                    PCts <- fit$x[, pcacomp] else {
                    PCts <- inputPCAarr %*% fit$rotation[, pcacomp]
                    PCts <- PCts[, 1]
                  }
                }
                bfts <- bfastts(PCts, timebfast, type = c("irregular"))
                bfm <- bfastmonitor(data = bfts, formula = response ~ 1, 
                  start = c(stmon2, 1), history = history, type = type)
                breakpointx <- round(bfm$breakpoint, digit = 5)
                if (!is.na(breakpointx)) {
                  pred <- mean(as.numeric((fitted(bfm$model))))
                  breakpointx <- round(bfm$breakpoint, digit = 5)
                  breakpointx2 <- round(stmon, digit = 5)
                }
                coun <- coun + 1
                if (stmon > (myear + acoun + moy)) {
                  acoun <- acoun + 1
                }
            }
        }
    }
    if (lastordetect == "last") 
        output <- as.numeric(breakpointx2) else if (lastordetect == "detect") 
        output <- as.numeric(breakpointx)
    return(output)
    
}
