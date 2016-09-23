require(lubridate) 
require(zoo)
load("Boliviaarrno.Rdata")
load("time_B1000.Rdata")
 
#inputarr: input array: Boliviaarrno: dim(Boliviaarrno)    6 1033  444
#timearr: the time of the imagery series. 444
#loca: spatial index
#monitoryear: before the monitor year is the historical period
returnpc2<-function(inputarr, timearr, loca, preprocess=T, monitoryear)
{
  if(preprocess)
  {  
    a7bandsrm<- aaply(inputarr,c(1,2),rmsat) #remove extreme value outside valid  range (1-10000)
    a7bandsrm2<-aaply(a7bandsrm,c(1,2),removedips) # remove low value
  
  } else {a7bandsrm2=inputarr}
  
  arr <- na.omit(t(a7bandsrm2[,loca,]))
  re  <- attributes(arr)$na.action
  arrbandsrm<-t(a7bandsrm2[,loca,])
   #historical pca
  PCAhis <-  arrbandsrm[ (round (decimal_date(timearr) , digits = 6) <  monitoryear),] # using historical period to compute pc loading
  fit<-prcomp(na.omit(PCAhis), scale.=T)
 
  pcacompauto <- which.max(abs(apply(fit$rotation[1:3,] 
                                     -fit$rotation[4:6,], 2, sum)))
  pcacompauto2 <- which.max(abs(fit$rotation[4,2:3] - fit$rotation[6,2:3])) + 1
  # to further avoid mistakenly selected pc, but not described in the paper. 
  if(pcacompauto==pcacompauto2)
    pcacompauto= ifelse (pcacompauto2==3, 2, 3)
  
  
  PChweightauto<-fit$rotation[,pcacompauto]
  PC2hweightauto<-fit$rotation[,pcacompauto2]
  
  PChistsauto<- drop ( arr %*% PChweightauto )
  PChists2<- drop ( arr %*% PC2hweightauto )
    
  res <- list(re,PChistsauto, PChists2)
   
  return(res)
}

#return 3 variables: 1. the removed na index. 2 Historical PC score, 3 pc greeness score  
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
for ( i in 1:100) 
{ 
  tts<-returnpc2(inputarr = Boliviaarrno,
                 timearr=time_B1000, 
                 loca=i, preprocess = F, monitoryear=2005 )
  
 
    time1<-time_B1000[-tts[[1]]] # when compute PCA, the NA values are removed. time1 is the time of PC scors.
  otss<-zoo(tts[[3]], time1)
   rs<-checkseats(coredata(otss), order=1, time1=time(otss))
  arrpc2no[i]<-rs
}
summary(arrpc2no)

#periodogram
pc2sa<-c()
for ( i in 1:100) 
{ 
  tts<-returnpc2(inputarr = Boliviaarrno,
                 timearr=time_B1000, 
                 loca=i, preprocess = F, monitoryear=2005 )
  
  
  time1<-time_B1000[-tts[[1]]] # when compute PCA, the NA values are removed. time1 is the time of PC scors.
  otss<-zoo(tts[[3]], time1)
  trimts<- window(otss, start = as.Date("2003-01-01"), end = as.Date("2014-12-31"))
  monthts<- aggregate(trimts, as.yearmon, mean)
  rt<-as.Date(range(time(monthts)))
  z1<-zoo(, as.yearmon(seq(from=rt[1], to=rt[2], by = "month")))
  
  zm1 <- merge(monthts, z1)
  zm <- na.approx(zm1)
  sazm<-spec.ar(zm)
  mf<-sazm$freq[which.max(sazm$spec)]
  #rs<-checkseats(coredata(otss), order=1, time1=time(otss))
   pc2sa[i]<-mf
}
summary(pc2sa)