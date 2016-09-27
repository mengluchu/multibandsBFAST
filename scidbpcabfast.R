library(scidb)
scidbconnect(username  =  "menglu",
             password = "mv5d2t2up3fr5dte",
             port= 41411, host="gis-bigdata.uni-muenster.de", protocol = "https", auth_type = "digest")

#scidblist
lb<-scidb("crsubboli4")
scidblist()
show(scidb("LANDSAT_BOLIVIA") )
 scidb("crsubboli2")
 
 lb1array<-array(lb1$band4, c(4,4,length(unique(lb1$t))))
 library(zoo)
plot(zoo(Boliviaarr[4,70,], time_B1000),typ='l', ylim=c(500,7000)) 
plot(zoo(lb1array[4,2,],sort(time_B1000)),typ='l',ylim=c(500,7000))
 
a<-array(1:12, c(3,4))
a2<-t(array(as.vector(a), c(4,3)))
a
a2
a <- scidb("unpack(r_exec(crsubboli4, 'output_attrs=3',
       'expr= 
       sink(paste(\"/home/menglu/\",the_i[1],the_j[1],\".txt\" ))
       library(multibandsBFAST)
       library(zoo)
       load(\"/home/menglu/time_B1000.Rdata\")
       dim1<-as.double(length(unique(the_i)))
       dim2<-as.double(length(unique(the_j)))
       dim3<-as.double(length(unique(the_t)))
       
       arr6bands <- array(NA,c(6,dim3, dim2, dim1))
       arr6bands[1,,,]<-band1
       arr6bands[2,,,]<-band2
       arr6bands[3,,,]<-band3
       arr6bands[4,,,]<-band4
       arr6bands[5,,,]<-band5
       arr6bands[6,,,]<-band6
       
       newarray  <- aperm (arr6bands, c(1,4,3,2))
       newarr    <-  rearrange_array (newarray, flatten=c(2,3))
       timearrbo <-  sort(time_B1000)      
       
       sidx <- seq(1, by =444, length=dim1*dim2)
       
       rowi  <- array(the_i[sidx],c(dim2,dim1))
       rrow  <- t(rowi)
       rrow  <- as.vector(rrow)   
       
       colj  <- array(the_j[sidx],c(dim2,dim1))
       rcol  <- t(colj)
       rcol <- as.vector(rcol)  
       
       #timearrbo(newarray)
       a1 <- try(bfmPCA(multibandsarr= newarr, timearr=timearrbo, history=\"all\",
       lastordetect = \"last\", hisweight=T, 
       pcacomp=3, 
       moy=1, 
       scoreselect=F,
       sca=F))
       
       if(class(a1)==\"try-error\")
       {
        a1 = rep(-1.0, dim1*dim2)
       }  
 
      a1<-as.vector(a1)

        list(a1,  as.double(rcol),as.double( rrow))'),tmpDim)")


out4<-scidbeval(a,name = "out4")


 
       
 #################################################
######  restore the output array to SciDB array##
#################################################
#Restore the dimensions (attribute double to int64, then redimension)
iquery("store(
       redimension(
       subarray(  project(
       apply( 
       cast(project(
       out4,
       expr_value_0,expr_value_1,expr_value_2 
       ),<timec:double,thecol:double,therow:double>[tmpDim=0:*,1000000,0]),
       col1, int64(thecol), row1, int64(therow)
       ),
       col1,row1, timec
       ), 0,11108888),
       <timec:double> [col1=0:3332,2,0, row1=0:3332,2,0]
       ), res1)",return=TRUE)

#iquery("dimensions(resarefpscidbf02)",return=TRUE)
# mind the name has to match!!
