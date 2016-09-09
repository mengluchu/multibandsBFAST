#' @title plot time series produced from return ts
#' @param arr array with dimension spectral bands, space, time series
#' @param tctl1 the index that the landsat 7 starts.
#' @param timearr times for the array
#' @param id spatial id for the time series plot to return
#' @param nameplot the path and name to save the plot
#' @param monitoryear year to start monitoring
#' @param BTestchangeDate validation change date for each location
# @example for( i in c(1,27,9)){ plotts (arr =Boliviaarr,tctl1=236,timearr=time_B1000,id=i, BTestchangeDate=BDbo, nameplot="boli_"); plotts (arr =Brazilarr ,tctl1=120, timearr=Braziltime,id=i,BTestchangeDate=BDbr, nameplot="br_")}

#' @export


plotts<-function(arr ,tctl1, timearr,id,
                 nameplot, BTestchangeDate,monitoryear)
{
  a1<- try(returnts(arr7bands1=arr, 
                    tctl1=tctl1, timearr=timearr,
                    loca=id ))
  if(class(a1)!="try-errors"){  
    dta<- cbind(a1[[2]],a1[[3]] ,
                a1[[12]],a1[[13]],a1[[14]],a1[[11]],a1[[7]],a1[[8]],
                a1[[9]] )  
    selectv<-c(1: ncol(dta)) 
    dtas<-dta[,selectv]
    n=ncol(dtas)  # one for the time and one for pca list
    variable.groups <- rep(c(1:n), each=nrow(dtas))
    BFMtime=0
    timerm<- timearr[-as.numeric(attributes( a1[[1]])$names)]
    timedec<-decimal_date(timerm)
    
    
    for( j in   selectv)
    {
      bfts<-bfastts(dta[,j], timerm, type = c("irregular"))
      bfm<-bfastmonitor(bfts, start=c(monitoryear,01),  history =   "all",  formula = response ~  harmon, order=1, type="OLS-MOSUM")
      # plot(zoo(dta[,i],times))
      
      BFMtime<- cbind(BFMtime, bfm$breakpoint)
    }
    
    BFMtime <- BFMtime[-1]
    vnames <- c("NDMI","NDVI", 
                "TB", "TG", "TW","Historical PCA","PCA score",
                "PCB","PCG")
    # paste( "PChisauto: PC", a1[[12]]))
    colnames(dtas) <- vnames
    #names(dtas)[(1+ncol(dta1)): n]<- vnames
    #[length(selectv)-ncol(dta1)] # note: only select consecutive, this line of code has to be edited
    times<-decimal_date(strptime( BTestchangeDate,format="%Y%j"))
    #[i]
    
    dtatime2<-rep (timedec, n )
    
    
    #dtatime2<-decimal_date (dtatime2)
    
    melted <- cbind(variable.groups,dtatime2, melt(dtas)[,2:3])
    #tail(melted)
    names(melted)<-c("ID","time","PCscoreID","value")
    # levels(melted$PCscoreID)
    
    zd= data.frame(z=BFMtime, PCscoreID = vnames ) # dimension name need to be the same "PCSCORE"
    #levels(melted$PCscoreID
    melted$PCscoreID = factor(melted$PCscoreID, levels=vnames)
    
    barplot <- ggplot(data=melted, 
                      aes(x=time,  y=value, 
                          fill=ID),position = "identity", show_guide=T,stat="identity") +
      geom_point() + geom_line() +
      facet_wrap(~PCscoreID,scale = "free_y" , ncol=2)+
      theme(legend.position="none")+xlab(paste("time"))+
      #ylab(paste(""))+
      
      # ggtitle(paste("Time of Real change vs. Detected")) + 
      geom_vline(show_guide = TRUE,xintercept = times[i],col="red",size=1.2, linetype=2)+ 
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size=14),
            
            axis.title.x = element_text(size = 16),axis.title.y = element_text(size = 16),
            strip.text = element_text(size = 15)
      )+
      geom_vline(aes(xintercept = z ,show_guide = TRUE), size =1.2, data=zd, col="blue",linetype=3)
    
    #scale_linetype_manual( "", breaks = c("real change", "detected change" ), values=c("red","blue"),
    
    #, ndvi, bands ,ct
    
    #jpeg(paste(nameplot,id,".jpg",sep=""),width=1200,height=800)
    plot(barplot )
    ggsave(paste(nameplot,id,".png",sep=""), height=12, width=15, units='in', dpi=500)
    
    #dev.off()
  }
}





