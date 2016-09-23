# note: eliakim s paper remove low values for NDVI. Here we remove low values for each of the bands

wrap<-function(multibandsarr ,multibandsarrno ,timearr ,history ,
               lastordetect, hisweight, pcacomp, moy, scoreselect, sca )
{
a1<-bfmPCA(multibandsarr=multibandsarr,timearr=timearr,history=history,
           lastordetect = lastordetect, hisweight=hisweight, 
             pcacomp=pcacomp, 
           moy=moy, 
           scoreselect=scoreselect,
            sca=sca)

a2<-bfmPCA(multibandsarr=multibandsarrno,timearr=timearr,history=history,
           lastordetect = lastordetect, hisweight=hisweight, 
           pcacomp=pcacomp, 
           moy=moy, 
           scoreselect=scoreselect,
           sca=sca)

res<-c(a1,a2)
return(res)
}

wrapVI<-function(multibandsarr ,multibandsarrno ,timearr ,history ,
                 pcacomp, moy )
{
  a1<-bfmVI(multibandsarr=multibandsarr,timearr=timearr,history=history,
             moy=moy, 
             )
  
  a2<-bfmVI(multibandsarr=multibandsarrno,timearr=timearr,history=history,
             moy=moy)
  
  res<-c(a1,a2)
  return(res)
}

valichart[,"historyPCA"] <- historypca
valichart[,"PCAscore"] <-  pcascore
valichart[,"ndvi2"] <- NDVI
valichart[,"ndmi2"] <-  NDMI
valichart[,"tctbright"] <- tctbright
valichart[,"tctgreen"] <-  tctgreen
valichart[,"tctwet"] <- tctwet
 save(valichart,file="valichart2.Rdata")
 require(stargazer)
bovali<-valitable(cx2=valichart,oridensetime=time_B1000,EarlyDateIsCommission=F,
                  oritemplate= bobo5, totalp=1136, nofchange=103,colmWith=2)

# Brazil
brvali<-valitable(valichartBrazil2, Braziltime, brbo5, totalp=470, EarlyDateIsCommission = F, nofchange=141,colmWith=2)
stargazer(brvali, summary =FALSE)
stargazer(bovali, summary =FALSE)

ptm <- proc.time()
historypca<-wrap (multibandsarr=Boliviaarr,multibandsarrno=Boliviaarrno,timearr=time_B1000,history="all",
               lastordetect = "last", scoreselect=F, sca=F, hisweight=T, moy=1)

hpbotime<-ptm - proc.time()

ptm <- proc.time()

pcascore<-wrap (multibandsarr=Boliviaarr,multibandsarrno=Boliviaarrno,timearr=time_B1000,history="all",
                  lastordetect = "last", scoreselect = T, sca = F, hisweight = F,moy=1)
psbotime<-ptm - proc.time()



require(plyr)
Boliviaarr<- aaply(Boliviaarr,c(1,2),rmsat) #remove extreme value outside valid  range (1-10000)
Boliviaarr<-aaply(Boliviaarr,c(1,2),removedips) # remove low value

Boliviaarrno<- aaply(Boliviaarrno,c(1,2),rmsat) #remove extreme value outside valid  range (1-10000)
Boliviaarrno<-aaply(Boliviaarrno,c(1,2),removedips) # remove low value





ndmiarr <- (Boliviaarr[4,,] - Boliviaarr[5,,])/(Boliviaarr[4,,] + Boliviaarr[5,,])
ndviarr <- (Boliviaarr[4,,] - Boliviaarr[3,,])/(Boliviaarr[4,,] + Boliviaarr[3,,])

ndmiarrno <- (Boliviaarrno[4,,] - Boliviaarrno[5,,])/(Boliviaarrno[4,,] + Boliviaarrno[5,,])
ndviarrno <- (Boliviaarrno[4,,] - Boliviaarrno[3,,])/(Boliviaarrno[4,,] + Boliviaarrno[3,,])

tctbr<-tct(Brazilarr, 120)
tctbrno<-tct(Brazilarrno, 120)

tctbo<-tct(Boliviaarr, 236)
tctbono<-tct(Boliviaarrno, 236)
ptm <- proc.time() 
NDVI<-wrapVI (multibandsarr=ndviarr,multibandsarrno=ndviarrno,timearr=time_B1000,history="all",
                   moy=1)

NDVItime<-ptm - proc.time()

NDMI<-wrapVI (multibandsarr=ndmiarr,multibandsarrno=ndmiarrno,timearr=time_B1000,history="all",
                  moy=1)



tctbright<-wrapVI (multibandsarr=tctbo[[1]],multibandsarrno=tctbono[[1]],timearr=time_B1000,history="all",
              moy=1)

tctgreen<-wrapVI (multibandsarr=tctbo[[2]],multibandsarrno=tctbono[[2]],timearr=time_B1000,history="all",
               moy=1)

tctwet<-wrapVI (multibandsarr=tctbo[[3]],multibandsarrno=tctbono[[3]],timearr=time_B1000,history="all",
               moy=1)


#####################################
ptm <- proc.time()

historypcabr<-wrap (multibandsarr=Brazilarr,multibandsarrno=Brazilarrno,timearr=Braziltime,history="all",
                  lastordetect = "last", scoreselect=F, sca=F, hisweight=T, moy=1)

hpbrtime<-proc.time() - ptm


ptm <- proc.time()
pcascorebr<-wrap (multibandsarr=Brazilarr,multibandsarrno=Brazilarrno,timearr=Braziltime,history="all",
                lastordetect = "last", scoreselect = T, sca = F, hisweight = F,moy=1)
psbrtime<-proc.time() - ptm


## uni vairant
Brazilarr<- aaply(Brazilarr,c(1,2),rmsat) #remove extreme value outside valid  range (1-10000)
Brazilarr<-aaply(Brazilarr,c(1,2),removedips) # remove low value

Brazilarrno <- aaply(Brazilarrno,c(1,2),rmsat) #remove extreme value outside valid  range (1-10000)
Brazilarrno <- aaply(Brazilarrno,c(1,2),removedips) # remove low value


ndmiarrbr <- (Brazilarr[4,,] - Brazilarr[5,,])/(Brazilarr[4,,] + Brazilarr[5,,])
ndviarrbr <- (Brazilarr[4,,] - Brazilarr[3,,])/(Brazilarr[4,,] + Brazilarr[3,,])

ndmiarrnobr <- (Brazilarrno[4,,] - Brazilarrno[5,,])/(Brazilarrno[4,,] + Brazilarrno[5,,])
ndviarrnobr <- (Brazilarrno[4,,] - Brazilarrno[3,,])/(Brazilarrno[4,,] + Brazilarrno[3,,])

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


valichartBrazil2[,"historyPCA"] <- historypcabr
valichartBrazil2[,"PCAscore"] <-  pcascorebr
valichartBrazil2[,"ndvi"] <- NDVIbr
valichartBrazil2[,"ndmi"] <-  NDMIbr
valichartBrazil2[,"tctbright"] <- tctbrightbr
valichartBrazil2[,"tctgreen"] <-  tctgreenbr
valichartBrazil2[,"tctwet"] <- tctwetbr


 
#####################################
load( "C:/Users/m_lu0002/Dropbox/mengluchu/multispectralbfast/Boliviaarr.Rdata")
load( "C:/Users/m_lu0002/Dropbox/mengluchu/multispectralbfast/Boliviaarrno.Rdata")
load( "C:/Users/m_lu0002/Dropbox/mengluchu/multispectralbfast/Brazilarr.Rdata")
load( "C:/Users/m_lu0002/Dropbox/mengluchu/multispectralbfast/Brazilarrno.Rdata")
load('C:/Users/m_lu0002/Dropbox/mengluchu/multispectralbfast/time_B1000.Rdata')
load('C:/Users/m_lu0002/Dropbox/mengluchu/multispectralbfast/Braziltime.Rdata')

#for validation
load("C:/Users/m_lu0002/Dropbox/mengluchu/multiBFAST/valichart.Rdata")
load("C:/Users/m_lu0002/Dropbox/mengluchu/multiBFAST/valichartBrazil.Rdata")

load("C:/Users/m_lu0002/Dropbox/mengluchu/multispectralbfast/valichartBrazil2.Rdata")
load("C:/Users/m_lu0002/Dropbox/mengluchu/multispectralbfast/bobo5.Rdata")
load("C:/Users/m_lu0002/Dropbox/mengluchu/multispectralbfast/brbo5.Rdata")

BoTest<-read.csv("C:/Users/m_lu0002/Dropbox/mengluchu/multispectralbfast/BoliviaValidation_1000.csv")

BrTest<-read.csv("C:/Users/m_lu0002/Dropbox/mengluchu/multispectralbfast/Brazilvali.csv")
BDbo<-BoTest$ChangeDate[BoTest[,"ChangeDate"]!=8]
BDbr<-BrTest$ChangeDate[BrTest[,"ChangeDate"]!=8]



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

#reproduce plot time series figures
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


#BoTest<-read.csv("C:/Users/m_lu0002/Dropbox/mengluchu/multispectralbfast/BoliviaValidation_1000.csv")

#BrTest<-read.csv("C:/Users/m_lu0002/Dropbox/mengluchu/multispectralbfast/Brazilvali.csv")
#BDbo<-BoTest$ChangeDate[BoTest[,"ChangeDate"]!=8]
#BDbr<-BrTest$ChangeDate[BrTest[,"ChangeDate"]!=8]

#save(Boliviaarrno, file="data/Boliviaarrno.rda") 
#save(Brazilarrno, file="data/Brazilarrno.rda") 
#save(Boliviaarr, file="data/Boliviaarr.rda") 
#save(Brazilarr, file="data/Brazilarr.rda") 
#save(Braziltime,file="data/Braziltime.rda")
#save(time_B1000,file="data/time_B1000.rda")
#save(bobo5,file="data/bobo5.rda")
#save(brbo5, file="data/brbo5.rda")
#save(BDbo,file="data/BDbo.rda")
#save(BDbr, file="data/BDbr.rda")
#save(valichartbo,file="data/valichartbo.rda")
#save(valichartbr, file="data/valichartbr.rda")

  