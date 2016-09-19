#' @title validation
#' @param cx2 validation chart, dataframe with true date in the first row
#' @param oridensetime array time 
#' @param oritemplate a matrix of the location of each time stamp of each time series.To compare the time by observations 
#' @return daf3 dataframe with confusion matrix and time delay. can use stargazer to generate a table
#' @import stargazer
#' @export 
#VALIDATION
#**************************************************************************
#require(stargazer)
#set working directory
#setwd("C:/Users/m_lu0002/Dropbox")
#load("C:/Users/m_lu0002/Dropbox/mengluchu/multispectralbfast/valichart3.Rdata")
#load("C:/Users/m_lu0002/Dropbox/mengluchu/multispectralbfast/time_B1000.Rdata")
#load("C:/Users/m_lu0002/Dropbox/mengluchu/multispectralbfast/bobo5.Rdata")
#load("C:/Users/m_lu0002/Dropbox/mengluchu/multispectralbfast/brbo5.Rdata")
#load("C:/Users/m_lu0002/Dropbox/mengluchu/multispectralbfast/valichartBrazil3.Rdata")

# Bolivia 
#bovali<-valitable(cx2=valichart,oridensetime=time_B1000, oritemplate= bobo5)

# Brazil
#brvali<-valitable(valichartBrazil2, Braziltime, brbo5)
#stargazer(bovali, summary =FALSE)
 
valitable<-function(cx2, oridensetime, oritemplate,EarlyDateIsCommission=T, totalp, nofchange,colmWith=2){
  
 
  spatialAccurayAssessmentx <- function(x,nOfindices, EarlyDateIsCommission,
                                        TotalSamplesize, 
                                        snumberOfsamplefromChange,
                                        colmWith =2){
    require(lubridate)
    
    if (is.null(TotalSamplesize) |is.null(snumberOfsamplefromChange)){
      stop("TotalSamplesize and snumberOfsamplefromChange must not be null")
      
    }
    # x is the table that contain the data
    cx2 <- x
    cx2$ChangeDate <- replace (cx2$ChangeDate, cx2$ChangeDate == 8, NA)
    
    ct <- subset(cx2, is.na(cx2$ChangeDate))
    ct2 <- subset(cx2, !is.na(cx2$ChangeDate))
    
    ct2$Chdate <- decimal_date(as.Date(as.character(ct2$ChangeDate), format = "%Y%j"))
    
    cx1 <- rbind(ct, ct2)
    
    xo <- c("Pontius Producer's Accuracy", "Sensitivity", "Precision")
    nc <- colmWith
    nz <- colmWith
    pa4 <-nOfindices
    va0 <- c()
    for (i in 1: pa4){
      nc <- nc + 2
      cp1 <- as.vector(cx1[, nz])
      cp2 <- as.vector(cx1[, nc])
      va0 <- c()
      for (j in 1: length(cp1)) {
        xt1 <- cp1[j]
        xt2 <- cp2[j]
        print(c(xt1,xt2 ))
        
        #check if it is true positive or true negative
        if (is.na(xt1) & is.na(xt2)){
          ba0 <- "tn"
          va0 <- c(va0, ba0)
          tl0 <- F
        }else {tl0 <- T}
        
        tr0 <- xt2 + 0.002
        if (tl0 == T & !is.na (xt1) & !is.na(xt2) & xt1 <= tr0 |tl0 == T & !is.na (xt1) & !is.na(xt2) & xt1 > tr0 & EarlyDateIsCommission == F){
          ba0 <- "tp"
          va0 <- c(va0, ba0)
          et3 <- F
        }else {et3 <- T}
        
        # check if it is omission error
        if (tl0 == T & et3 == T &!is.na (xt1) & is.na (xt2) ){
          ba0 <- "fn"
          va0 <- c(va0, ba0)
          be1 <- F
        }else { be1 <- T}
        
        #Check if it is commission error
        
        if (tl0 == T & et3 == T & be1 ==T & !is.na (xt1) & !is.na (xt2) |tl0 == T & et3 == T & be1 ==T & !is.na (xt1) & !is.na (xt2) & xt1 > xt2 & EarlyDateIsCommission == T ){
          ba0 <- "fp"
          va0 <- c(va0, ba0)
          ze0 <- F
        }else {ze0 <- T}
        
        if (tl0 == T & et3 == T & ze0  == T & is.na (xt1) & !is.na (xt2)){
          ba0 <- "fp"
          va0 <- c(va0, ba0)
        }
      }
      # calculate overall accuracy and errors
      #co <- (length(subset  (va0 , va0  == "co"))/ length(va0))*100
      #ok <- (length(subset  (va0 , va0  == "ok"))/ length(va0))*100
      #om <- (length(subset  (va0 , va0  == "om"))/ length(va0))*100
      #x0 <- c(ok, co, om)
      # xo <- rbind(xo,x0)
      
      # number of true
      
      uSA <- (length(subset  (va0 , va0  == "tp")))/ (length(subset  (va0 , va0  == "tp")) + length(subset  (va0 , va0  == "fp"))) * 100
      pPA <- (length(subset  (va0 , va0  == "tp")))/ (length(subset  (va0 , va0  == "tp")) + length(subset  (va0 , va0  == "fn")))*100
      po <- length(subset  (va0 , va0  == "tp"))/ (length(subset  (va0 , va0  == "tp")) + length(subset  (va0 , va0  == "fn"))+ length(subset  (va0 , va0  == "fp"))) * 100
      dox <- c(round(po, digit=1), round(pPA,digit=1),round(uSA,digit=1))
      xo <- rbind(xo, dox)
    }
    
    return(xo)
  }
  
  
  
  temporalDeley <- function(vdata, dtes, TmSeriesOfSamplePixels, nOfindices =1){
    
    dt90 <- vdata 
    tda <- TmSeriesOfSamplePixels
    xme <- c()
    tmo <- as.data.frame(matrix(nrow =1, ncol =2))
    xx0 <- 2
    for (i in 1:nOfindices){
      xx0 <- xx0+2
      t4xx <- c()
      t5x <- c()
      for (i in 1: nrow(tda)){
        tc <- as.numeric(tda[i,])
        tc <- tc[1:length(tc)]
        tc2 <- as.data.frame(tc)
        tc2$date <- round (dtes, digits = 4)
        tx5 <- dt90[i, ]
        a <- tx5$ChangeDate
        if (!is.na(a)){
          b90 <- as.Date(as.character(a), format = "%Y%j")
          b80 <- decimal_date(b90)
          tx12 <- round(b80,digits =4)-0.001
          tx13 <- round (tx5[,xx0],digits = 4)+0.002
          tz2 <- subset (tc2, (tc2$date >= tx12 & tc2$date <= tx13))
          x1x <-  length(subset (tz2$tc, !is.na(tz2$tc)))
          x2x <- subset (tz2$date, !is.na(tz2$tc))
          if (x1x != 0){
            x1x1 <-  x1x - 1
          }else{
            x1x1 <- NA
            x1x2 <- NA
          }
        }else {
          x1x1 <- NA
        }
        t4xx <- c(t4xx, x1x1)
      }
      t5z <- replace (t4xx, t4xx == -1, 0 )
      tmo <- cbind(tmo,t5z)
      x <- subset(t5z, !is.na(t5z))
      xme <- c(xme, median(x))
    }
    
    return(xme)
    
  }
  
  
  varnames<-names(cx2)[-1]
  
  Len<-length(cx2)
  lcx2<-length(cx2)
  for( i in (lcx2-1):1){
    cx2<- data.frame(cx2[,1: (lcx2-i)] , NA,cx2[,(lcx2-i+1):lcx2 ])
    lcx2<-length(cx2)
  }
  cx2<- data.frame(cx2[,1: 2] , NA,cx2[,3:lcx2 ])
  
  names(cx2)[1:2]<-c("ChangeDate", "Chdate")
  names(cx2)[length(cx2)]<- varnames[length(varnames)]
  
  #cx2$ChangeDate <- replace (cx2$ChangeDate, cx2$ChangeDate ==8, NA)
  
  nOfindices2 <-  Len-1 
  s<-spatialAccurayAssessmentx (x=cx2, nOfindices=nOfindices2,EarlyDateIsCommission= EarlyDateIsCommission, TotalSamplesize = totalp, 
                                snumberOfsamplefromChange = nofchange,
                                colmWith = colmWith )
  
  Bo5df<-data.frame(oritemplate)
  
  dtes2 <- decimal_date (as.Date(oridensetime, format = "%Y-%m-%d"))
  xme<-temporalDeley(vdata=cx2, dtes=dtes2,TmSeriesOfSamplePixels=Bo5df,nOfindices=nOfindices2)
  
  s1<-s[-1,]
  daf3<-cbind(s1,xme)
  
  colnames(daf3)<-c('Pontius Producers accuracy',"sensitivity","precision","temporal_delay")
  rownames(daf3)<-varnames  
  return(daf3)
  #stargazer(daf3, summary =FALSE)
}
 