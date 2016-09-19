tctts<-function(arr, l1)
  
{ 
  cgreeness <-c() 
  cbrightness <-c()
  cwetness <- c()

    tasselcap2<-function (arr, sat ) 
  {
    sat <- tolower(sat)
    if (!sat %in% c("landsat4tm", "landsat5tm", "landsat7etm", 
                    "landsat8oli", "modis")) 
      stop("Sensor not implemented. See ?tasseledCap for options.")
    d <- list(NULL, c("brightness", "greenness", "wetness"))
    coefs <- list(landsat4tm = matrix(c(0.2043, 0.4158, 0.5524, 
                                        0.5741, 0.3124, 0.2303, -0.1063, -0.2819, -0.4934, 0.794, 
                                        -2e-04, -0.1446, 0.0315, 0.2021, 0.3102, 0.1594, -0.6806, 
                                        -0.6109), ncol = 3, dimnames = d), 
                  landsat5tm = matrix(c(0.2043,  0.4158, 0.5524,
                                        0.5741, 0.3124, 0.2303, 
                                        0.1063, -0.2819, 
                                        -0.4934, 
                                        0.794, -2e-04, -0.1446,
                                        0.0315, 0.2021, 0.3102, 
                                        0.1594, -0.6806, -0.6109), ncol = 3, dimnames = d), 
                  
                  landsat7etm = matrix(c(0.3561, 0.3972, 0.3904, 
                                         0.6966, 0.2286, 0.1596, 
                                         -0.3344, -0.3544, -0.4556, 
                                         0.6966, -0.0242, -0.263, 
                                         0.2626, 0.2141, 0.0926, 
                                         0.0656, -0.7629, -0.5388), ncol = 3, dimnames = d), 
                  landsat8oli = matrix(c(0.3029, -0.243, -0.5424,
                                         0.7276, 0.0713, -0.1608, 
                                         0.1511, 0.1973, 0.3283, 
                                         0.3407, -0.7117, -0.4559), ncol = 3, 
                                       dimnames = d), modis = matrix(c(0.4395, 0.5945, 
                                                                       0.246, 0.3918, 0.3506, 0.2136, 0.2678, -0.4064, 
                                                                       0.5129, -0.2744, -0.2893, 0.4882, -0.0036, -0.4169, 
                                                                       0.1147, 0.2489, 0.2408, 0.3132, -0.3122, -0.6416, 
                                                                       -0.5087), ncol = 3, dimnames = d))
    #tct <- function(x, cof = coefs[[sat]]) {
    #arr<-t(Boliviaarr[,1,])
    #sat="landsat5tm"    
    arr %*% coefs[[sat]]
    
    #}
    #apply(arr,1, tct)
  }
  
   
    #237:445 for bolivia
    #121 for brazil
    
    
    #    tas2<-as.matrix(data.frame(cbind(arr[1,i,],arr[2,i,],
    #                                    arr[3,i,],arr[4,i,],arr[5,i,],arr[6,i,])))[1:l1,]
    
    tas<-t(arr)[1:l1,]
    aa<-tasselcap2(tas,"landsat7etm")
    #   tas2<-as.matrix(data.frame(cbind(arr[1,i,],arr[2,i,],arr[3,i,],
    #                                     arr[4,i,],arr[5,i,],arr[6,i,])))[(l1+1):dim(arr)[3],]
    
    tas2<-t(arr)[(l1+1):dim(arr)[2],]
    aa2<-tasselcap2(tas2,"landsat5tm")
    
    aa3<-rbind(aa,aa2)
    
    cbrightness <-aa3[,1]
    cgreeness<-aa3[,2]
    cwetness<-aa3[,3]
   
  return(list(cbrightness,cgreeness,cwetness))
  
}
