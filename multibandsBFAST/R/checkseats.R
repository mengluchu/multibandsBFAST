#' @title check seasonality of time series
#' @param ts core data of time series to check 
#' @param  order order of harmonics
#' @param time1 time of the time series
#' @return r sqaure of harmonic fitting
#' @export


checkseats<-function(ts, order, time1)
{ 
  yday365 <- function(x) {
    x <- as.POSIXlt(x)
    mdays <- c(31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)
    cumsum(c(0L, mdays))[1L + x$mon] + x$mday
  }
  
  zz <- zoo(na.omit(ts),1900 + as.POSIXlt(time1)$year 
            + (yday365(time1) - 1)/365, frequency = 365)
  # zz<-zz[ time(zz)>'2000(023)'&time(zz)<"2015(023)"]
  # zz <- aggregate(zz, as.yearmon, mean)
  si <-sin(outer(2*pi*time(zz),c(1:3))[,order])
  co <-cos(outer(2*pi*time(zz),c(1:3))[,order])
  
  fit<-lm(zz~co+si)
  # result6[i]<-summary(fit)$ adj.r.squared 
  output<-summary(fit)$r.squared   
  
  return(output)
}
