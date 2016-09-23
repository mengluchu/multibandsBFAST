require(lubridate) 
require(zoo)
load("Boliviaarrno.Rdata")
load("time_B1000.Rdata")
 
#inputarr: input array: Boliviaarrno: dim(Boliviaarrno)    6 1033  444
#timearr: the time of the imagery series. 444
#loca: spatial index
#monitoryear: before the monitor year is the historical period
#return 3 variables: 1. the removed na index. 2 Historical PC score, 3 pc greeness score  

returnpc2<-function(inputarr, timearr, loca, preprocess=T, monitoryear)
{
  if(preprocess)
  {  
    a7bandsrm <- aaply(inputarr,c(1,2),rmsat) #remove extreme value outside valid  range (1-10000)
    a7bandsrm2 <- aaply(a7bandsrm,c(1,2),removedips) # remove low value
  
  } else  a7bandsrm2 = inputarr 
  
  arr <- na.omit(t(a7bandsrm2[,loca,]))
  re  <- attributes(arr)$na.action
  arrbandsrm<-t(a7bandsrm2[,loca,])
  
  #historical period
  PCAhis <-  arrbandsrm[(round (decimal_date(timearr) , digits = 6) <  monitoryear),] 
  
  # using historical period to compute pc loading
  fit<-prcomp(na.omit(PCAhis), scale.=T)
  
  # the PC historical method selected PC
  pcacompauto <- which.max(abs(apply(fit$rotation[1:3,] 
                                     -fit$rotation[4:6,], 2, sum)))
  
  # the PC-greenness: In the Bolivian site, the time series are more homogenuous. The PC greenness are mostly the PC2. 
  # so for simplicity only select between PC2 and PC3. 
  
  pcacompauto2 <- which.max(abs(fit$rotation[4,2:3] - fit$rotation[6,2:3])) + 1
  
  # to further avoid mistakenly select PC, if PC-greenness select the same PC as historical PCA method, change the order (didnt describ in the paper). 
  
  if(pcacompauto == pcacompauto2)
    pcacompauto = ifelse (pcacompauto2==3, 2, 3)
  
    PChweightauto <- fit$rotation[,pcacompauto]
    PC2hweightauto <- fit$rotation[,pcacompauto2]
  
    PChistsauto <- drop(arr %*% PChweightauto)
    PChists2 <- drop(arr %*% PC2hweightauto)
    
    res <- list(re,PChistsauto, PChists2)
   
    return(res)
}

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

#if preprocess, need the library(multibandsBFAST)
#Boliviaarrno<- aaply(Boliviaarrno,c(1,2),rmsat) #remove extreme value outside valid  range (1-10000)
#Boliviaarrno<-aaply(Boliviaarrno,c(1,2),removedips) # remove low value

arrpc2no<-c() # save results

# i can be 1: 1003
# test 
for ( i in 1:100) 
{ 
  tts <- returnpc2(inputarr = Boliviaarrno,
                 timearr = time_B1000, 
                 loca = i, preprocess = F, monitoryear = 2005 )
  
 
  time1 <- time_B1000[-tts[[1]]] # when compute PCA, the NA values are removed. time1 is the time of PC scors.
  otss <- zoo(tts[[3]], time1)
  rs <- checkseats(coredata(otss), order=1, time1=time(otss))
  arrpc2no[i] <- rs
}

summary(arrpc2no)
