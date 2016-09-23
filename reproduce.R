#reproduce the results of the paper 
library(devtools)
install_github("mengluchu/multibandsBFAST/multibandsBFAST")
library(multibandsBFAST)

#data

data(Boliviaarrno)  
data(Brazilarrno)  
data(Boliviaarr)   
data(Brazilarr)  
data(Braziltime) 
data(time_B1000) 

#for validation 
data(BDbo)
data(BDbr)
data(bobo5) 
data(brbo5)  
data(valichartbo) 
data(valichartbr)  


## Bolivia site 
historypca<-wrap (multibandsarr=Boliviaarr,multibandsarrno=Boliviaarrno,timearr=time_B1000,history="all",
                  lastordetect = "last", scoreselect=F, sca=F, hisweight=T, moy=1)

pcascore<-wrap (multibandsarr=Boliviaarr,multibandsarrno=Boliviaarrno,timearr=time_B1000,history="all",
                lastordetect = "last", scoreselect = T, sca = F, hisweight = F,moy=1)
 
# vegetation indices
Boliviaarr <- aaply(Boliviaarr,c(1,2),rmsat) #remove extreme value outside valid  range (1-10000)
Boliviaarr <- aaply(Boliviaarr,c(1,2),removedips) # remove low value

Boliviaarrno <- aaply(Boliviaarrno,c(1,2),rmsat) #remove extreme value outside valid  range (1-10000)
Boliviaarrno <- aaply(Boliviaarrno,c(1,2),removedips) # remove low value


ndmiarr <- (Boliviaarr[4,,] - Boliviaarr[5,,])/(Boliviaarr[4,,] + Boliviaarr[5,,])
ndviarr <- (Boliviaarr[4,,] - Boliviaarr[3,,])/(Boliviaarr[4,,] + Boliviaarr[3,,])

ndmiarrno <- (Boliviaarrno[4,,] - Boliviaarrno[5,,])/(Boliviaarrno[4,,] + Boliviaarrno[5,,])
ndviarrno <- (Boliviaarrno[4,,] - Boliviaarrno[3,,])/(Boliviaarrno[4,,] + Boliviaarrno[3,,])

#tct indices
tctbo<-tct(Boliviaarr, 236)
tctbono<-tct(Boliviaarrno, 236)
 
#
NDVI<-wrapVI (multibandsarr=ndviarr,multibandsarrno=ndviarrno,timearr=time_B1000,history="all",
              moy=1)
 
NDMI<-wrapVI (multibandsarr=ndmiarr,multibandsarrno=ndmiarrno,timearr=time_B1000,history="all",
              moy=1)


tctbright<-wrapVI (multibandsarr=tctbo[[1]],multibandsarrno=tctbono[[1]],timearr=time_B1000,history="all",
                   moy=1)

tctgreen<-wrapVI (multibandsarr=tctbo[[2]],multibandsarrno=tctbono[[2]],timearr=time_B1000,history="all",
                  moy=1)

tctwet<-wrapVI (multibandsarr=tctbo[[3]],multibandsarrno=tctbono[[3]],timearr=time_B1000,history="all",
                moy=1)


##### Brazil site ################################
historypcabr<-wrap (multibandsarr=Brazilarr,multibandsarrno=Brazilarrno,timearr=Braziltime,history="all",
                    lastordetect = "last", scoreselect=F, sca=F, hisweight=T, moy=1)

pcascorebr<-wrap (multibandsarr=Brazilarr,multibandsarrno=Brazilarrno,timearr=Braziltime,history="all",
                  lastordetect = "last", scoreselect = T, sca = F, hisweight = F,moy=1)
 
## vegetation indices
Brazilarr<- aaply(Brazilarr,c(1,2),rmsat) #remove extreme value outside valid  range (1-10000)
Brazilarr<-aaply(Brazilarr,c(1,2),removedips) # remove low value

Brazilarrno <- aaply(Brazilarrno,c(1,2),rmsat) #remove extreme value outside valid  range (1-10000)
Brazilarrno <- aaply(Brazilarrno,c(1,2),removedips) # remove low value

ndmiarrbr <- (Brazilarr[4,,] - Brazilarr[5,,])/(Brazilarr[4,,] + Brazilarr[5,,])
ndviarrbr <- (Brazilarr[4,,] - Brazilarr[3,,])/(Brazilarr[4,,] + Brazilarr[3,,])

ndmiarrnobr <- (Brazilarrno[4,,] - Brazilarrno[5,,])/(Brazilarrno[4,,] + Brazilarrno[5,,])
ndviarrnobr <- (Brazilarrno[4,,] - Brazilarrno[3,,])/(Brazilarrno[4,,] + Brazilarrno[3,,])

#tct
tctbr<-tct(Brazilarr, 120)
tctbrno<-tct(Brazilarrno, 120)

#univariable
NDVIbr<-wrapVI (multibandsarr=ndviarrbr,multibandsarrno=ndviarrnobr,timearr=Braziltime,history="all",
                moy=1)
NDMIbr<-wrapVI (multibandsarr=ndmiarrbr,multibandsarrno=ndmiarrnobr,timearr=Braziltime,history="all",
                moy=1)



tctbrightbr<-wrapVI (multibandsarr=tctbr[[1]],multibandsarrno=tctbrno[[1]],timearr=Braziltime,history="all",
                     moy=1)

tctgreenbr<-wrapVI (multibandsarr=tctbr[[2]],multibandsarrno=tctbrno[[2]],timearr=Braziltime,history="all",
                    moy=1)

tctwetbr<-wrapVI (multibandsarr=tctbr[[3]],multibandsarrno=tctbrno[[3]],timearr=Braziltime,history="all",
                  moy=1)


 
#save(valichartbo, file= "C:/Users/m_lu0002/Dropbox/mengluchu/multiBFAST/valichartbo.Rdata")
#save(valichartbr, file= "C:/Users/m_lu0002/Dropbox/mengluchu/multiBFAST/valichartbr.Rdata")
 
valichartbo[,"historyPCA"] <- historypca
valichartbo[,"PCAscore"] <-  pcascore
valichartbo[,"ndvi2"] <- NDVI
valichartbo[,"ndmi2"] <-  NDMI
valichartbo[,"tctbright"] <- tctbright
valichartbo[,"tctgreen"] <-  tctgreen
valichartbo[,"tctwet"] <- tctwet
 
 
valichartbr[,"historyPCA"] <- historypcabr
valichartbr[,"PCAscore"] <-  pcascorebr
valichartbr[,"ndvi"] <- NDVIbr
valichartbr[,"ndmi"] <-  NDMIbr
valichartbr[,"tctbright"] <- tctbrightbr
valichartbr[,"tctgreen"] <-  tctgreenbr
valichartbr[,"tctwet"] <- tctwetbr

# validation
# bolivia
bovali<-valitable(cx2=valichartbo,oridensetime=time_B1000,EarlyDateIsCommission=F,
                  oritemplate= bobo5, totalp=1136, nofchange=103,colmWith=2)

# Brazil
brvali<-valitable(valichartbr, Braziltime, brbo5, totalp=470, EarlyDateIsCommission = F, nofchange=141,colmWith=2)

# table
stargazer(brvali, summary =FALSE)
stargazer(bovali, summary =FALSE)



#####################################

# return ts 
#tts <- returnts2(inputarr = Boliviaarrno,
#timearr = time_B1000, tctl1=256,
#loca = i, preprocess = F, monitoryear = 2005 )


#reproduce pc loading figure 
rep_figloading<-function(arr, timearr, varname="bands", plotw="bands", xaxisname="bands", obsname="temporal spatial points")
{  
  b1<- aaply(arr,c(1,2),rmsat) #remove extreme value outside valid  range (1-10000)
  b2<-aaply(b1,c(1,2),removedips) # remove low value
  
  Boliviaarr2 <- rearrange_array(b2, flatten=c(2,3),)
  fit<-prcomp(na.omit(t(Boliviaarr2)),scale.=T)
  
  #jpeg("Brno.jpg") 
  
  plotloading(PCfit = fit, varname = varname, obsname = obsname,xaxisname = xaxisname,
              addline = 0, nl=4, plotw=plotw)
  #dev.off()
}

rep_figloading(arr = Boliviaarrno, timearr = time_B1000)
rep_figloading(arr = Brazilarrno, timearr = Braziltime)

#reproduce plot time series figures, image is stored, name specified in the nameplot
for( i in c(1,27))
{  
  plotts2 (arr = Boliviaarr,tctl1 = 236,timearr=time_B1000,
           id=i, BTestchangeDate=BDbo, nameplot="boli_", monitoryear=2005)
}

for( i in c(1,9))
{
  plotts2 (arr = Brazilarr ,tctl1=120, timearr=Braziltime,
           id=i,BTestchangeDate=BDbr,
           nameplot="br_",monitoryear=2005)
}

#check seasonatlity of PC
arrpc2no<-c() # save results

# i can be 1: 1033
# test 
for ( i in 1:1033) 
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

# check periodogram
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


