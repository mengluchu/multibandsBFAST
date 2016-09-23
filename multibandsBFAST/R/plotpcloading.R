#' @title  reproduce the plot of pc loading
#' @export 

  
plotloading <- function(PCfit, varname="", obsname="", xaxisname="",tax=16, plotw=c("time", "bands", "bandsts"), nl=4, rastertime, addline = 0) {
  re <- length(PCfit$rotation[, 1]) 
  i <- length(PCfit$x[,1])
  variable.groups <- rep(1:nl, each=re)
  if (plotw=="time") {
    T1 <- rep(rastertime, nl)
    melted <- cbind(variable.groups, melt(PCfit$rotation[, 1:nl]), T1)
    names(melted) <- c("variable.groups", "X1", "X2", "value", "T1")
    barplot <- ggplot(data = melted) + geom_bar(aes(x = T1, y = value, fill = variable.groups), 
                                                position = "identity", show.legend = T, stat = "identity") + facet_wrap(~X2) + 
      theme(legend.position = "none") + xlab(xaxisname) + ylab("PC loadings") + theme(strip.text.x = element_text(size = tax*0.8), axis.title=element_text(size=tax),axis.text.x = element_text(angle = 45, 
                                                                                                                                                                                                hjust = 1)) + geom_vline(xintercept = as.numeric(as.Date(addline)), linetype = 4, 
                                                                                                                                                                                                                         colour = "brown")  
    # ggtitle(bquote(atop(.("PC loadings"), atop(paste("variable: ", .(varname), 
    #                                                 "   ", "observation: ", .(obsname), sep = "")))))
    plot(barplot)
    
  } else if (plotw=="bandstime") {
    melted <- cbind(variable.groups, melt(PCfit$rotation[, 1:nl]))
    names(melted) <- c("variable.groups", "X1", "X2", "value")
    
    barplot <- ggplot(data = melted) + geom_bar(aes(x = X1, y = value, fill = variable.groups), 
                                                position = "identity", show.legend = F, stat = "identity") + facet_wrap(~X2) + 
      theme( strip.text.x = element_text(size = tax*0.8), axis.title=element_text(size=tax),legend.position = "none", plot.title = element_text(size = 18, 
                                                                                                                                                colour = "black", vjust = -1)) + xlab(xaxisname) + ylab("PC loadings") + geom_vline(xintercept = addline, linetype = "dashed", 
                                                                                                                                                                                                                                    colour = "gray")  
    #ggtitle(bquote(atop(.(""), atop(paste("variable: ", .(varname), 
    #                                                 "   ", "observation: ", .(obsname), sep = "")))))
    plot(barplot)
  }else if (plotw=="bands")
  {
    melted <- cbind(variable.groups, melt(PCfit$rotation[, 1:nl]))
    names(melted) <- c("variable.groups", "X1", "X2", "value")
    
    barplot <- ggplot(data = melted) + geom_bar(aes(x = X1, y = value, fill = variable.groups), 
                                                position = "identity", show.legend = F, stat = "identity") + facet_wrap(~X2) + 
      theme( strip.text.x = element_text(size = tax*0.8), axis.title=element_text(size=tax),legend.position = "none", plot.title = element_text(size = 18, 
                                                                                                                                                colour = "black", vjust = -1)) + xlab(xaxisname) + ylab("PC loadings")                                                                                                                                                                                       
    
    #ggtitle(bquote(atop(.(""), atop(paste("variable: ", .(varname), 
    #                                                 "   ", "observation: ", .(obsname), sep = "")))))
    plot(barplot)
  }
}
  
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

#rep_figloading(arr = Boliviaarrno, timearr = time_B1000)
#rep_figloading(arr = Brazilarrno, timearr = Braziltime)
