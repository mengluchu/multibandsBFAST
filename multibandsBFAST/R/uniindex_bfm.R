#' @title BFAST sequential monitor on single index
#' @param x input time series coredata
#' @param my_dates the time book keeped for the time series
#' @import bfast
#' @import lubridate
#' @return timeofchange detected change time for a time series
#' @export 
uniindex_bfm <- function(x, moy = 1, myear = 2005, history, my_dates, minumum_observations = 15, 
    type = "OLS-MOSUM") {
    
    proCell <- as.numeric(x)/10000
    zv <- subset(as.numeric(proCell), !is.na(as.numeric(proCell)))
    magnitudex <- 0
    breakpointx <- NA
    
    if (length(zv) > minumum_observations) {
        dtat <- as.data.frame(proCell)
        dtat$dates <- my_dates
        dtat$my_date <- decimal_date(dtat$dates)
        dtat <- subset(dtat, !is.na(dtat$proCell))
        # dtat$proCell<- removedips(dtat$proCell )
        
        historyPeriod2 <- subset(dtat, round(decimal_date(dtat$dates), digits = 5) < 
            myear)
        if (length(historyPeriod2$dates) > 6) {
            tim1 <- c(round(decimal_date(dtat$dates), digits = 0))
            tim2 <- unique(tim1)
            timex <- subset(tim2, tim2 >= myear)
            coun <- 1
            
            while (is.na(breakpointx) & coun < length(timex)) {
                stmon <- timex[coun]
                dtat2 <- subset(dtat, round(decimal_date(dtat$dates), digits = 5) < 
                  (stmon + moy))
                bpts <- bfastts(dtat2$proCell, dtat2$dates, type = c("irregular"))
                bfm <- bfastmonitor(data = bpts, order = 1, formula = response ~ 
                  harmon, start = c(stmon, 1), history = history, type = type)
                breakpoint1 <- round(bfm$breakpoint, digit = 5)
                breakpointx <- NA
                if (!is.na(breakpoint1)) {
                  pred <- mean(as.numeric((fitted(bfm$model))))
                  tbreak <- breakpoint1 - 0.004
                  xbreak <- breakpoint1 + 0.004
                  cdte <- subset(dtat2, round(dtat2$my_date, digits = 2) >= 
                    round(tbreak, digits = 2) & round(dtat2$my_date, digits = 3) <= 
                    round(xbreak, digits = 3))
                  observedx <- cdte$proCell
                  magnitudex <- observedx - pred
                  if (magnitudex < 0) {
                    breakpointx <- breakpoint1
                  }
                }
                coun = coun + 1
            }
        }
    }
    output <- as.numeric(breakpointx)
    return(structure(timeofchange = output))
}


