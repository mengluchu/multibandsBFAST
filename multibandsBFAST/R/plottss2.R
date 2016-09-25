#' @title plot time series produced from return ts, PC score method end a
#' @param arr array with dimension spectral bands, space, time series
#' @param tctl1 the index that the landsat 7 starts.
#' @param timearr times for the array
#' @param id spatial id for the time series plot to return
#' @param nameplot the path and name to save the plot
#' @param monitoryear year to start monitoring
#' @param BTestchangeDate validation change date for each location
#' @return plot time series, as well as the detected and real change time as in the paper. The results is saved.
#' @import ggplot2
#' @import plyr
#' @import reshape2
#' @export

plotts2 <- function(arr, tctl1, timearr, id, nameplot, BTestchangeDate, 
    monitoryear) {
    a1 <- try(returnts2(inputarr = arr, tctl1 = tctl1, timearr = timearr, 
        loca = id, monitoryear = monitoryear))
    rpcts <- as.numeric(rownames(na.omit(a1[[length(a1)]])))
    if (class(a1) != "try-errors") {
        dta <- data.frame(a1[[2]], a1[[3]], a1[[12]], a1[[13]], a1[[14]], 
            a1[[11]], a1[[8]], a1[[8]], a1[[9]])
        dtav <- c(a1[[2]], a1[[3]], a1[[12]], a1[[13]], a1[[14]], a1[[11]], 
            a1[[7]], a1[[8]], a1[[9]])
        selectv <- c(1:ncol(dta))
        dtas <- dta[, selectv]
        n = ncol(dtas)  # one for the time and one for pca list
        variable.groups0 <- rep(c(1:6), each = nrow(dtas))
        variable.groups1 <- rep(7, length(a1[[7]]))
        variable.groups2 <- rep(c(8:n), each = nrow(dtas))
        variable.groups <- c(variable.groups0, variable.groups1, variable.groups2)
        # variable.groups <- rep(c(1:n), each=nrow(dtas))
        BFMtime = 0
        timerm <- timearr[-as.numeric(attributes(a1[[1]])$names)]
        timerm2 <- timearr[-c(as.numeric(attributes(a1[[1]])$names), rpcts)]
        timedec <- decimal_date(timerm)
        timedec2 <- decimal_date(timerm2)
        for (j in selectv) {
            bfts <- bfastts(dta[, j], timerm, type = c("irregular"))
            bfm <- bfastmonitor(bfts, start = c(monitoryear, 1), history = "all", 
                formula = response ~ harmon, order = 1, type = "OLS-MOSUM")
            BFMtime <- cbind(BFMtime, bfm$breakpoint)
        }
        BFMtime <- BFMtime[-1]
        ti <- (length(a1) - 1)
        BFMtime[7] <- a1[[ti]]
        # the pc order
        idpcg <- a1[[length(a1) - 2]]
        idpcauto <- a1[[length(a1) - 3]]
        vnames <- c("NDMI", "NDVI", "TB", "TG", "TW", paste("Historical PCA (PC", 
            idpcauto, ")", sep = ""), paste("PCA score (PC", idpcauto, ")", 
            sep = ""), "PC-brightness (PC1) ", paste("PC-greenness (PC", 
            idpcg, ")", sep = ""))
        
        colnames(dtas) <- vnames
        times <- decimal_date(strptime(BTestchangeDate, format = "%Y%j"))
        
        dtatime0 <- rep(timedec, 6)
        dtatime02 <- rep(timedec, (n - 7))
        dtatime2 <- c(dtatime0, timedec2, dtatime02)
        
        names0 <- rep(vnames[1:6], each = nrow(dtas))
        names01 <- rep(vnames[7], length(a1[[7]]))
        names02 <- rep(vnames[8:n], each = nrow(dtas))
        namesall <- c(names0, names01, names02)
        
        length(variable.groups)
        melted <- data.frame(as.numeric(variable.groups), as.numeric(dtatime2), 
            namesall, as.numeric(dtav))
        tail(melted)
        colnames(melted) <- c("ID", "time", "PCscoreID", "value")
        
        zd = data.frame(z = BFMtime, PCscoreID = vnames)  # dimension name need to be the same 'PCSCORE'
        # levels(melted$PCscoreID
        melted$PCscoreID = factor(melted$PCscoreID, levels = vnames)
        # plot
        tsplot <- ggplot(data = melted, aes(x = time, y = value, fill = ID), 
            position = "identity", show_guide = T, stat = "identity") + 
            geom_point() + geom_line() + facet_wrap(~PCscoreID, scale = "free_y", 
            ncol = 2) + theme(legend.position = "none") + xlab(paste("time")) + 
            geom_vline(show_guide = TRUE, xintercept = times[i], col = "red", 
                size = 1.2, linetype = 2) + theme(axis.text.x = element_text(angle = 45, 
            hjust = 1, size = 14), axis.title.x = element_text(size = 16), 
            axis.title.y = element_text(size = 16), strip.text = element_text(size = 15)) + 
            geom_vline(aes(xintercept = z, show_guide = TRUE), size = 1.2, 
                data = zd, col = "blue", linetype = 3)
        # scale_linetype_manual( '', breaks = c('real change', 'detected change'
        # ), values=c('red','blue'),
        plot(tsplot)
        ggsave(paste(nameplot, id, ".png", sep = ""), height = 12, width = 15, 
            units = "in", dpi = 500)
    }
}


# require (reshape2) require(ggplot2) for( i in c(1,27,9)) { i=10

# plotts2 (arr = Boliviaarr,tctl1 = 236,timearr=time_B1000, id=i,
# BTestchangeDate=BDbo, nameplot='boli_', monitoryear=2005)


# plotts2 (arr = Brazilarr ,tctl1=120, timearr=Braziltime,
# id=i,BTestchangeDate=BDbr, nameplot='br_',monitoryear=2005) }
