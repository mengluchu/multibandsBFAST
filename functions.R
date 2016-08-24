load( "C:/Users/m_lu0002/Dropbox/mengluchu/multispectralbfast/Boliviaarr.Rdata")
load( "C:/Users/m_lu0002/Dropbox/mengluchu/multispectralbfast/Boliviaarrno.Rdata")
load( "C:/Users/m_lu0002/Dropbox/mengluchu/multispectralbfast/Brazilarr.Rdata")
load( "C:/Users/m_lu0002/Dropbox/mengluchu/multispectralbfast/Brazilarrno.Rdata")
load( "C:/Users/m_lu0002/Dropbox/mengluchu/multispectralbfast/time_B1000.Rdata")
load( "C:/Users/m_lu0002/Dropbox/mengluchu/multispectralbfast/Braziltime.Rdata")

#BTest<-read.csv("C:/Users/m_lu0002/Dropbox/mengluchu/multispectralbfast/BoliviaValidation_1000.csv")
#BTest<-read.csv("C:/Users/m_lu0002/Dropbox/mengluchu/multispectralbfast/Brazil.csv")
load("C:\\Users\\m_lu0002\\Dropbox\\mengluchu\\multispectralbfast\\valichartBrazil2.Rdata")
head(valichartBrazil2)
load("C:\\Users\\m_lu0002\\Dropbox\\mengluchu\\multispectralbfast\\valichart.Rdata")
load("C:\\Users\\m_lu0002\\Dropbox\\mengluchu\\multispectralbfast\\valichartBrazil2.Rdata")
length(which( (valichartBrazil2$PCA2h-brhisauto  )!=0 ))
length(which(!is.na(brhisauto)))
#library(knitr)
#library(bfastSpatial) 
library(plyr)
library(bfast)
library(raster)
library(lubridate)
library(devtools)
#install_github("edzer/spacetime")
#library(spacetime)

#library(devtools)
#install_github('dutri001/bfastSpatial')

rmsat<-function(x){ 
  
  x[x >10000]<-NA
  x[x<0] <-NA
  return(x)
}# remove saturated pixels 

removedips <- function(x) {
  #x <- na.approx(x, rule = 2)
  
  y <- as.numeric(x)
  leng <- length(x) - 2
  for(i in 1:leng)
  {
    ## moving window - check distance
    b <- i + 2
    c <- b - 1
    if(any(is.na(x[b]),is.na(x[c]),is.na(x[i])))
      next
    
    mida <- x[c] - x[i]
    midc <- x[c] - x[b] 
    
    # Find 20 percent
    threshold1 <- (-1/100) * x[i] 
    threshold2 <- (-1/100) * x[b]
    
    # check threshold
    if( mida < 0 & midc < 0 &  mida < threshold1 | midc < threshold2) {
      y[c] <- (x[b] + x[i]) / 2} 
  }
  return(y)
}


bfmPCAALL<-function(arr7bands2,history=c("all", "ROC", "BP"),
                    hisweight=T, 
                    timearr, pcacomp, 
                    moy=1, 
                    scoreselect=F,
                    lastordetect=c("last","detect"),sca=F,plot=F){ 
  
  timebfm <-rep(NA,length=dim(arr7bands2)[2])
  
  a7bandsrm<- aaply(arr7bands2,c(1,2),rmsat) #remove extreme value outside valid  range (1-10000)
  a7bandsrm2<-aaply(a7bandsrm,c(1,2),removedips) # remove low value
  
  # timemulbands<-timendmi[-as.numeric(attr(na.omit(t(arr7bands2[,1,])), "na.action"))]
  breakpointx<-NA
  
  for (i in 1:dim(a7bandsrm2)[2])
  { 
    ta7b<-t(a7bandsrm2[,i,])  
    bfmic<- try(ybfastmonitorPCA(arr=ta7b,hisweight= hisweight, history= history ,myear=2005,my_dates=timearr, moy=moy,plot=plot,lastordetect=lastordetect, scoreselect=scoreselect, pcacomp=pcacomp, sca=sca),silent=F)
    
    if(class(bfmic)!='try-error'){
      if(!is.na(bfmic))
      {  
        timebfm[i]<-bfmic    
      }
    }
  } 
  return(timebfm)  
}


bfmndmiALL<-function(arr7bands1, timearr,lastordetect=c("last","detect"), history=c("all", "ROC", "BP"), plot=FALSE,moy=1){ 
  
  a7bandsrm<- aaply(arr7bands1,2,rmsat) #remove extreme value outside valid  range (1-10000)
  a7bandsrm2<-aaply(a7bandsrm,2,removedips) # remove low value
  
  timebfm<-c()
  #timemulbands<-timendmi[-as.numeric(attr(na.omit(arr7bands1[1,]), "na.action"))]
  breakpointx<-NA
  for (i in 1:dim(a7bandsrm2)[2])
  { 
    ta7b<-arr7bands1[i,]
    bfmic<- try(ybfastmonitorndmi(x=ta7b,myear=2005,history=history,my_dates=timearr, moy=moy,plot=plot),silent=TRUE)
    if(class(bfmic)!='try-error'){
      if(!is.na(bfmic))
        timebfm[i]<-bfmic    
    }
  }
  
  #  plot(timedif3,main='ndmi',typ='h', ylab='time differences compare with validation data')
  #return(timedif3)
  
  return(timebfm)
}



ybfastmonitorPCA <- function(arr, hisweight, pcacomp, 
                             myear,plot = F,  
                             history = c("all"),
                             my_dates, scoreselect=F,
                             lastordetect=c("last","detect"), 
                             minumum_observations = 15, sca,
                             type ="OLS-MOSUM",moy=1) { 
  
  omarr<-na.omit(arr)
  ondtime<-my_dates[-attributes(omarr)$na.action]# not ordered but dense
  densetime<-sort(ondtime) # ordered dense time   
  
  breakpointx<-NA
  breakpointx2<-NA
  if (length (densetime) > minumum_observations){   
    historyPeriod2 <- window(densetime,endTime= strptime(myear,format="%Y")) # control time length
    
    if (length(historyPeriod2) > 6){
      
      atim1 <- c(round (decimal_date(densetime), digits =0)) #by year
      atim2 <- unique(atim1)
      atimex <- subset (atim2, atim2 >= myear) #time after 2005
      acoun <- 1  
      
      tim1 <- c(round (decimal_date(densetime), digits =6)) #by date
      tim2 <- unique(tim1)
      timex <- subset (tim2, tim2 >= myear) #time after 2005
      coun <- 1     
      
      while (is.na(breakpointx) & coun < length(timex)){
        
        stmon <- timex[coun]# for each time step
        stmon2 <- atimex[acoun]
        ## this is for one value
        #dtat2 <- subset(dtat, round (dtat$my_date , digits = 6) <  stmon) # control time length
        
        inputPCAarr <- omarr[(round (decimal_date(ondtime), digits = 6) < stmon),] # unordered, select data in monitor
        timebfast <- ondtime[(round (decimal_date(ondtime), digits = 6) < stmon)] # unordered
        # not ordered so request book-keeping
        
        ##PCA came in
        if(hisweight)
        { 
          PCAhis <- omarr[(round (decimal_date(ondtime), digits = 6) < stmon2),] # using historical period to compute pc loading  
          fit<-prcomp(PCAhis,scale.=T)
          #select eigen vector
          pcacomp <- which.max( abs(apply(fit$rotation[1:3,] 
                                          -fit$rotation[ 4:6,] ,2, sum)))
          
          PCweight <- fit$rotation[,pcacomp]
          
          inputPCAarr[inputPCAarr=="-Inf"] <- NA
          inputPCAarr[inputPCAarr=="Inf"] <- NA
          #PCts<- scale(inputPCAarr) %*% PCweight #scale multiply by weight
          if(sca)  
            inputPCAarr<-apply(inputPCAarr, 2, scale)#scale the data
          
          PCts <- inputPCAarr  %*% PCweight #original  multiply by weight(unscaled)
          PCts <- PCts[,1] 
          
        }else{
          # PC score 
          fit <- prcomp(inputPCAarr, scale.=T)
          
          if (scoreselect)
          {
            pcacomp<-which.max(abs(apply(fit$rotation[1:3,] 
                                         -fit$rotation[4:6,], 2, sum)))
          }
          if(sca)
            PCts <- fit$x[,pcacomp] # scale the data
          else{ 
            PCts <-  inputPCAarr  %*% fit$rotation[,pcacomp]
            PCts <- PCts[,1] 
          }
        }
        
        bfts<-bfastts(PCts, timebfast, type = c("irregular"))        
        bfm <- bfastmonitor(data = bfts,formula = response ~ 1, start = c(stmon2, 1),plot = plot, history = history,type =type)
        breakpointx <- round(bfm$breakpoint, digit = 5)
        if (!is.na(breakpointx)){
          pred <- mean(as.numeric((fitted(bfm$model))))
          breakpointx <- round(bfm$breakpoint, digit = 5)
          breakpointx2 <- round(stmon, digit = 5)
        } 
        
        coun <- coun +1
        if(stmon > (myear+acoun+moy))
        {
          acoun <- acoun+1
          # plot(bfm)}  
        }  
      } 
    }
  }
  if(lastordetect=='last')
    output <- as.numeric(breakpointx2) 
  else if(lastordetect=='detect')
    output <- as.numeric(breakpointx) 
  return(output)
  
}


ybfastmonitorndmi <- function(x,moy=1,myear = 2005,plot = F,pCentile = 0.05,history , my_dates ,minumum_observations  = 15,type ="OLS-MOSUM") { 
  
  proCell<- as.numeric(x)/10000
  zv <- subset(as.numeric(proCell), !is.na(as.numeric(proCell)))
  magnitudex <- 0
  breakpointx  <- NA
  
  if (length (zv) > minumum_observations){
    dtat <- as.data.frame(proCell)
    dtat$dates <- my_dates
    dtat$my_date <- decimal_date(dtat$dates)
    dtat <- subset(dtat, !is.na(dtat$proCell))
    dtat$proCell<- removedips(dtat$proCell )
    
    historyPeriod2 <- subset(dtat, round (decimal_date(dtat$dates), digits = 5) < myear)
    if (length(historyPeriod2$dates) > 6){
      tim1 <- c(round (decimal_date(dtat$dates), digits =0))
      tim2 <- unique(tim1)
      timex <- subset (tim2, tim2 >= myear)
      coun <- 1
      
      while (is.na(breakpointx) & coun < length(timex)){
        stmon <- timex[coun]
        dtat2 <- subset(dtat, round (decimal_date(dtat$dates), digits = 5) < (stmon+moy))
        bpts <- bfastts(dtat2$proCell,  dtat2$dates , type = c("irregular"))
        
        
        bfm <- bfastmonitor(data = bpts,order=1, formula = response ~ harmon, start=c(stmon, 1),plot = plot, history = history,type =type)
        breakpoint1 <- round(bfm$breakpoint, digit = 5)
        bfm$magnitude
        if (!is.na(breakpoint1)){
          pred <- mean(as.numeric((fitted(bfm$model))))
          breakpointx <- round(bfm$breakpoint, digit = 5)
          tbreak <- breakpointx - 0.004
          xbreak <- breakpointx + 0.004
          cdte <- subset(dtat2, round(dtat2$my_date , digits = 2) >= round( tbreak , digits =2) & round(dtat2$my_date , digits = 3) <= round( xbreak  , digits =3) )
          observedx <- cdte$proCell
          magnitudex <- observedx - pred
          if (magnitudex < 0 ){ 
            breakpointx <- breakpoint1 
          } 
        }
        coun=coun+1
        
      }
    }
  }
  output<-as.numeric(breakpointx)
  return(output)
}

#load("C:/Users/m_lu0002/Dropbox/mengluchu/figi/arr7bands.Rdata")

#load('C:/Users/m_lu0002/Dropbox/mengluchu/multispectralbfast/Bchange_1000.Rdata')
#array data with change bolivia
#load('C:/Users/m_lu0002/Dropbox/mengluchu/multispectralbfast/Bnochange_1000.Rdata')
#array data without change bolivia 
#load timearr

#BTestnochange1<-subset(BTest, BTest$ChangeDate==8)
#valinochangebr<-BTestnochange1$ChangeDate

#BTestchange1<-subset(BTest, BTest$ChangeDate!=8)
#valichangebr<-BTestchange1$ChangeDate


#load('C:/Users/m_lu0002/Dropbox/mengluchu/multispectralbfast/Brazilchange.Rdata')
#array data with change brazil
#load('C:/Users/m_lu0002/Dropbox/mengluchu/multispectralbfast/Brazilnochange.Rdata')
#array data without change brazil 

###########################################
#Bochange<-Brazilchange  # brazil 
#Bochange<-Bchange_1000  # bolivia

#Bo5<-Bochange[ ,grep("sr_band5",x=colnames(Bochange))]
#Bo4<-Bochange[ ,grep("sr_band4",x=colnames(Bochange))]
#Bo1<-Bochange[ ,grep("sr_band1",x=colnames(Bochange))]
#Bo2<-Bochange[ ,grep("sr_band2",x=colnames(Bochange))]
#Bo3<-Bochange[ ,grep("sr_band3",x=colnames(Bochange))]
#Bo7<-Bochange[ ,grep("sr_band7",x=colnames(Bochange))]

#lc<-dim(Bochange)[1]

#arr6bands<-array(NA,c(6,lc,ncol(Bo5)))
#arr6bands[1,,]<-Bo1
#arr6bands[2,,]<-Bo2
#arr6bands[3,,]<-Bo3
#arr6bands[4,,]<-Bo4

#arr6bands[5,,]<-Bo5
#arr6bands[6,,]<-Bo7 

#ndmichange<-(as.matrix(Bo4)-as.matrix(Bo5))/(as.matrix(Bo4)+as.matrix(Bo5))
#ndvichange<-(as.matrix(Bo4)-as.matrix(Bo3))/(as.matrix(Bo3)+as.matrix(Bo4)) # try ndvi

########################################################

t2t3ns<-bfmPCAALL(arr7bands2=Brazilarr,hisweight=T, timearr=Braziltime,
                  plot=FALSE,
                  pcacomp=2,moy=1,lastordetect='last', history="all")
nt2t3ns<-bfmPCAALL(arr7bands2=Brazilarrno,hisweight=T, timearr=Braziltime,
                   plot=FALSE,
                   pcacomp=2,moy=1,lastordetect='last' )

wrap<-function(arr7 , arr7no , plot=F,
               pcacomp, moy,lastordetect='last',
               wholet , history="all", hisweight=T,sca, scoreselect=F)
{
  t1<-bfmPCAALL(arr7bands2=arr7 ,hisweight=hisweight, sca=sca,
                timearr=wholet, plot=plot,
                pcacomp=pcacomp,moy=moy,lastordetect=lastordetect,history=history,scoreselect=scoreselect)
  
  nt1<-bfmPCAALL(arr7bands2=arr7no ,hisweight=hisweight,sca=sca , timearr=wholet, plot=plot,
                 pcacomp=pcacomp,moy=moy,lastordetect=lastordetect,history=history,scoreselect=scoreselect)
  tn<-c(t1,nt1)
  return(tn)
}


brhisautos <- wrap(arr7 = Brazilarr, arr7no = Brazilarrno,
                   wholet = Braziltime, hisweight=T, moy=1,sca=T)

brscore3s <- wrap(arr7 = Brazilarr, arr7no = Brazilarrno, pcacomp=3,
                  wholet = Braziltime, hisweight=F, moy=1,sca=T)

brscore2s <- wrap(arr7 = Brazilarr, arr7no =Brazilarrno, pcacomp=2,
                  wholet = Braziltime, hisweight=F, moy=1,sca=T)

brscore1s <- wrap(arr7 = Brazilarr, arr7no = Brazilarrno, pcacomp=1,
                  wholet = Braziltime, hisweight=F, moy=1,sca=T)

brscoreautos <- wrap(arr7 = Brazilarr, arr7no = Brazilarrno,
                     wholet = Braziltime, hisweight=F, scoreselect = T, moy=1,sca=T)




bohisautunscale <- wrap(arr7 = Boliviaarr, arr7no = Boliviaarrno,
                        wholet = time_B1000, hisweight=T, moy=1 ,sca=T)

boscore3scale <- wrap(arr7 = Boliviaarr, arr7no = Boliviaarrno,pcacomp=3,
                      wholet = time_B1000, hisweight=F, moy=1,sca=T)

boscore2scale <- wrap(arr7 = Boliviaarr, arr7no = Boliviaarrno,pcacomp=2,
                      wholet = time_B1000, hisweight=F, moy=1,sca=T)

boscore1scale <- wrap(arr7 = Boliviaarr, arr7no = Boliviaarrno,pcacomp=1,
                      wholet = time_B1000, hisweight=F, moy=1,sca=T)

boscoreautoscale <- wrap(arr7 = Boliviaarr, arr7no = Boliviaarrno,
                         wholet = time_B1000, hisweight=F, scoreselect=T, moy=1,sca=T)

valichartBrazil2[,"brscore2scale"] <- brscoreauto
valichartBrazil2[,"brhisautounscale"]<-brhisautos
valichartBrazil2[,"brscoreautoscale"]<-brscoreautos
valichartBrazil2[,"brscore1scale"]<-brscore1s
valichartBrazil2[,"brscore2scale"]<-brscore2s
valichartBrazil2[,"brscore3scale"]<-brscore3s

valichart[,"bohisautoscale"]<-bohisautoscale
valichart[,"boscoreautoscale"]<-boscoreautoscale
valichart[,"boscore1scale"]<-boscore1scale
valichart[,"boscore2scale"]<-boscore2scale
valichart[,"boscore3scale"]<-boscore3scale
valichartBrazil2$PCA3h-brhisauto
#valichart
valichart[,"bopcauto2"]<-bohisauto
str(valichart)
#vctest<-read.csv("VCtest.csv")
#vctest[,"ChangeDate"]<-
valichart<- data.frame(cbind(BTest$ChangeDate,band6o1,band7o1, PCA1, PCA2, PCA3, ndmi, ndvi))
save(valichartBrazil2, file="C:\\Users\\m_lu0002\\Dropbox\\mengluchu\\multispectralbfast\\valichartBrazil2.Rdata")

valichartBrazil2[,"PCAhisauto"]<-PCAhisauto 
valichart[,"PCAhisauto"]<-PCAhisauto 

#save(valichart, file="valichart.Rdata")
#save(valichartBrazil2, file="valichartBrazil2.Rdata")

############### TCT ###############################################################
#arr: spectral, spatial, time
#l1: length of landsat tm5 236 for bolivia, 120 for brazil

load('C:/Users/m_lu0002/Dropbox/mengluchu/multispectralbfast/tctbr.Rdata')
load('C:/Users/m_lu0002/Dropbox/mengluchu/multispectralbfast/tctbo.Rdata')

#tasselbridig<- TBbr-decimal_date(strptime(valichangebr,format="%Y%j"))
#tasselgredif<-TWbr-decimal_date(strptime(valichangebo,format="%Y%j"))
#tasselwetdif<-tasselwet-decimal_date(strptime(BTestchange1$ChangeDate,format="%Y%j"))
#plot(tasselbridig,typ='h',main='tassled cap brightness')
#plot(tasselgredif,typ='h',main='tassled cap greeness')

#summary(tasselbridig)


TBbo<-bfmndmiALL(arr7bands1= tctbo[[1]], timearr=time_B1000,moy=1,plot=FALSE )
TGbo<-bfmndmiALL(arr7bands1=tctbo[[2]], timearr=time_B1000,moy=1,plot=FALSE )
TWbo<-bfmndmiALL(arr7bands1=tctbo[[3]], timearr=time_B1000,moy=1,plot=FALSE )

TBbono<-bfmndmiALL(arr7bands1=tctbono[[1]], timearr=time_B1000,moy=1,plot=FALSE )
TGbono<-bfmndmiALL(arr7bands1=tctbono[[2]], timearr=time_B1000,moy=1,plot=FALSE )
TWbono<-bfmndmiALL(arr7bands1=tctbono[[3]], timearr=time_B1000,moy=1,plot=FALSE )


TBbr<-bfmndmiALL(arr7bands1= tctbr[[1]], results='time',timearr=Braziltime,moy=1,plot=FALSE )
TGbr<-bfmndmiALL(arr7bands1=tctbr[[2]], results='time',timenarr=Braziltime,moy=1,plot=FALSE )
TWbr<-bfmndmiALL(arr7bands1=tctbr[[3]], results='time',timenarr= Braziltime,moy=1,plot=FALSE )

TBbrno<-bfmndmiALL(arr7bands1=tctbrno[[1]], results='time',timearr=Braziltime,moy=1,plot=FALSE )
TGbrno<-bfmndmiALL(arr7bands1=tctbrno[[2]], results='time',timearr=Braziltime,moy=1,plot=FALSE )
TWbrno<-bfmndmiALL(arr7bands1=tctbrno[[3]], results='time',timearr=Braziltime,moy=1,plot=FALSE )

TB<-c(TBbr, TBbrno)
TG<-c(TGbr, TGbrno)
TW<-c(TWbr, TWbrno)

######################## animation ##############
# saveGIF({
#  ani.options(nmax = 30)
#  ybfastmonitorPCA(arr=t(arr7bands[,11,]),myear=2005,my_dates=timendmi1,moy=1,lastordetect='detect',plot=FALSE)
#  ybfastmonitorndmi(x= ndmichange[9,],myear=2005,my_dates=timendmi,moy=2,plot=TRUE)
#}, interval = 0.1, movie.name = "bfmndmi.gif", ani.width = 600, ani.height = 600)
#


############# data
get4d<-function(rasterstack)
{
  b1<-subset(arr, grep("sr_band1",x=names(allbandsfigis)))
  b2<-subset(arr, grep("sr_band2",x=names(allbandsfigis)))
  b3<-subset(arr, grep("sr_band3",x=names(allbandsfigis)))
  b4<-subset(arr, grep("sr_band4",x=names(allbandsfigis)))
  b5<-subset(arr, grep("_sr_band5",x=names(allbandsfigis)))
  b7<-subset(arr, grep("_sr_band7",x=names(allbandsfigis)))
  
  arrb1<-as.array(b1)
  arrb2<-as.array(b2)
  arrb3<-as.array(b3)
  arrb4<-as.array(b4)
  arrb5<-as.array(b5)
  arrb7<-as.array(b7)
  Dim<-dim(arr)
  d4array<-array(,c(Dim[1],Dim[2],Dim[3],6))
  d4array[,,,1]<-arrfigib1
  d4array[,,,2]<-arrfigib2
  d4array[,,,3]<-arrfigib3
  d4array[,,,4]<-arrfigib4
  d4array[,,,5]<-arrfigib5
  d4array[,,,6]<-arrfigib7
  return(d4array)
}