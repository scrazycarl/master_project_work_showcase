######Interface to plot the result of WorM3 model###
# Author: Xierzhati Aniwa  (Shirzart Enwer)
# Generation date:Forgotten
# Last modified date: 24-04-2018
# The result of WorM3 model has been saved as csv file, eg:Air_test.csv
# one has to put those output file into same folder and change the 
# working directroy accordingly.
#
# In line 64, the matrix has to be flipped because the WorM3 model generate the
# data matrix from filling the down left corner. reference: 
# https://agupubs.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1029%2F2011GB004068&attachmentId=68398971
# 
# The next step to improve: 
# 1.add a color legend to the plot.
# 2.find out the MATLAB color package
# 3.comparing the plot to the reference
#
# Description for special cases
# When plotting with image funciton, first the matrix has to be fliped upside down and then transposed.
# Because when plotting with image, it plots the first column as the first horizental line in the plot,
# and the direction depends on how the xline/yline sequence is defined. It always starting plooting 
# from the first value in the sequence. But image argument always expect (x,y) to be increasing sequence,
# so the plotting always start from down left corner. That's why the data matrix has to be first fliped
# upside down and then transposed.

#install.packages('colorRamps')
getwd()
setwd("/Users/shirzart/Desktop/Master Project work/Code/Mercury Model/")
require(maptools)
source('worm3_coordinate_cell_conversion_2018-04-11.R')
library('RColorBrewer')
library('jpeg')
library('raster')
library('colorRamps')

#input the emission data as CSV file
setwd("/Users/shirzart/Desktop/Master Project work/Code/Mercury Model/Alternative scenarios/NS1_india_new")
Air_test<-read.csv('Air_test_L10WETSCRUBBER_NS1_india.csv',header=FALSE,dec=",")
Air_TGM<-read.csv('Air_TGM_L10WETSCRUBBER_NS1_india.csv',header=FALSE,dec=",")
TGM_StSt<-read.csv('TGM_StSt_L10WETSCRUBBER_NS1_india.csv',header=FALSE,dec=",")
SO_Hgo_StSt<-read.csv('SO_Hg0_StSt_L10WETSCRUBBER_NS1_india.csv',header=FALSE,dec=",")
SO_THg_StSt<-read.csv('SO_THg_StSt_L10WETSCRUBBER_NS1_india.csv',header=FALSE,dec=",")
Water_Hg0_test<-read.csv('Water_Hg0_test_L10WETSCRUBBER_NS1_india.csv',header=FALSE,dec=",")
Water_THg_test<-read.csv('Water_THg_test_L10WETSCRUBBER_NS1_india.csv',header=FALSE,dec=",")

########Before plotting the different scenario, check if the input model result is identical with default result
# save(Air_TGM,file='/Users/shirzart/Desktop/Master Project work/Code/Mercury Model/Air_TGM_default')
Air_TGM_default=local(get(load('/Users/shirzart/Desktop/Master Project work/Code/Mercury Model/Air_TGM_default')))
try(if(identical(Air_TGM_default,Air_TGM)==TRUE)stop("The input result are identical to 2010base_adapted dataset." ))


#convert the input data to R matrix for plot, and normalisze the numeric data for plotting
## some of the following commands needs to specify filling "byrow" because when R fill an 
# matrix from an array, it fills column wise
# the dimension of origianl files are the folloiwng:
# Air_TGM: 12*24; Air_test: 288*1; TGM_StSt: 180*360; Water_Hg0_test: 288*1; Water_THg_test: 288*1;
# SO_Hgo_StSt:180*360; SO_THg_test: 180*360;

imge_AirTGM<-matrix(sapply(as.character(unlist(Air_TGM,recursive = TRUE,use.names = FALSE)),as.numeric),nrow = 12,ncol=24)
imge_Airtest<-matrix(sapply(as.character(unlist(Air_test,recursive = TRUE,use.names = FALSE)),as.numeric),byrow=TRUE,nrow = 12,ncol=24)
imge_TGMStSt<-matrix(sapply(as.character(unlist(TGM_StSt,recursive = TRUE,use.names = FALSE)), as.numeric),nrow = 180,ncol=360)
imge_SOHgoStSt<-matrix(sapply(as.character(unlist(SO_Hgo_StSt,recursive = TRUE,use.names = FALSE)), as.numeric),nrow = 180,ncol=360)
imge_SOTHgStSt<-matrix(sapply(as.character(unlist(SO_THg_StSt,recursive = TRUE,use.names = FALSE)), as.numeric),nrow = 180,ncol=360)
imge_WaterHg0test<-matrix(sapply(as.character(unlist(Water_Hg0_test,recursive = TRUE,use.names = FALSE)), as.numeric),byrow=TRUE,nrow = 12,ncol=24)
imge_WaterTHgStSt<-matrix(sapply(as.character(unlist(Water_THg_test,recursive = TRUE,use.names = FALSE)), as.numeric),byrow=TRUE,nrow = 12,ncol=24)




#####load functions and input the worldmap#######
setwd("/Users/shirzart/Desktop/Master Project work/Code/Mercury model/")
source('worm3_coordinate_cell_conversion_2018-04-11.R')
source("./Input/mercury_model_data_2018-03-29.R", echo = T)
source("./Input/mercury_model_functions_2018-03-29.R", echo = T)
region_polygons=list()
region_polygons$countries <- readShapeSpatial("./Input/ne_110m_admin_0_countries/ne_110m_admin_0_countries.shp")
plot(region_polygons$countries)

setwd("/Users/shirzart/Desktop/Master Project work/Code/Mercury Model/Alternative scenarios/NS1_india_new")
#set the different gridlines
#gridlines for 12*24 grid
xline<-seq(-172.5,172.5, by=15)
yline<-seq(-82.5,82.5,by=15) 
#gridlines for 180*360 grid
xline1<-seq(172.5,-172.5,by=-15) 
yline1<-seq(82.5,-82.5,by=-15)



#plot the global emission images with the same color scales
###########Trying out to fix the color scheme#######################
# par(mar=c(1,3,1,3),cex.lab=1.0,cex.axis=0.8)
pdf(file="./plots/AirTGM_L10WETSCRUBBER_NS1_india.pdf",height = 9,width = 12)
par(mar=c(5,4,4,2),cex.lab=1.5,cex.axis=1.5,cex.main=2)
image(xline,yline,t(imge_AirTGM[nrow(imge_AirTGM):1,]),col=matlab.like2(10),breaks=c(7.0e-10,7.5e-10,8.0e-10,8.5e-10,9.0e-10,9.5e-10,1.0e-9,1.05e-9,1.1e-9,1.15e-9,1.2e-9), asp=1,xlab="", ylab="", xaxs="i", yaxs="i",axes= FALSE)
plot(region_polygons$countries,add=TRUE)
title(main="Total Gaseous Mercury Concentration in Ambient Air", line=-2)
axis(2,at =c(-90,-45,0,45,90),padj = 0.5,hadj=1,tick = TRUE,pos=-180,las=1,
     labels=c(expression(paste(90*degree,"S")),expression(paste(45*degree,"S")),
              expression(paste(0*degree,"")),expression(paste(45*degree,"N")),
              expression(paste(90*degree,"N"))))

axis(1,at=c(-180,-120,-60,0,60,120,180),hadj=0.5,padj=-0.3,tick = TRUE,pos = -90,las=1,
     labels=c(expression(paste(180*degree,"W")),expression(paste(120*degree,"W")),
              expression(paste(60*degree,"W")),expression(paste(0*degree,"")),
              expression(paste(60*degree,"E")),expression(paste(120*degree,"E")),
              expression(paste(180*degree,"E"))))
legend_image <- as.raster(matrix(matlab.like2(10), nrow =1))
text(x=seq(-160,140,l=5), y=-115, labels= c("0.70", "0.83", "0.95", "1.08", "1.20"), cex=1.2,tck=1)
text(x=160,y=-115,labels=("ng/m^3"),cex=1.2,tck=1)
rasterImage(legend_image, -160, -110, 140,-105)
save(imge_AirTGM, file = "imge_AirTGM_L10WETSCRUBBER_NS1_india.R")
dev.off()


# plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '', main = 'legend title')
# legend("bottom",legend =c(7.0e-10,7.5e-10,8.0e-10,8.5e-10,9.0e-10,9.5e-10,1.0e-9,1.05e-9,1.1e-9,1.15e-9,1.2e-9), col=matlab.like(10),pch= )
# Legend("bottomleft", pal=pal, values=~imge_AirTGM, title="Poverty_amount(million)",
        


#################################################
# par(mar=c(5,5,5,5),cex.lab=1.0,cex.axis=0.8)
pdf(file="./plots/AirTest_L10WETSCRUBBER_NS1_india.pdf",height = 9,width = 12)
par(mar=c(5,4,4,2),cex.lab=1.5,cex.axis=1.5,cex.main=2)
image(xline,yline,t(imge_Airtest[nrow(imge_Airtest):1,]),col=matlab.like2(10), breaks=c(7.0e-10,7.5e-10,8.0e-10,8.5e-10,9.0e-10,9.5e-10,1.0e-9,1.05e-9,1.1e-9,1.15e-9,1.2e-9), asp=1,xlab="", ylab="", xaxs="i", yaxs="i",axes= FALSE)
plot(region_polygons$countries,add=TRUE)
title(main="AirTest",line=-2,sub="")
axis(2,at =c(-90,-45,0,45,90),padj = 0.5,hadj=1,tick = TRUE,pos=-180,las=1,
     labels=c(expression(paste(90*degree,"S")),expression(paste(45*degree,"S")),
              expression(paste(0*degree,"")),expression(paste(45*degree,"N")),
              expression(paste(90*degree,"N"))))

axis(1,at=c(-180,-120,-60,0,60,120,180),hadj=0.5,padj=-0.3,tick = TRUE,pos = -90,las=1,
     labels=c(expression(paste(180*degree,"W")),expression(paste(120*degree,"W")),
              expression(paste(60*degree,"W")),expression(paste(0*degree,"")),
              expression(paste(60*degree,"E")),expression(paste(120*degree,"E")),
              expression(paste(180*degree,"E"))))
save(imge_Airtest,file = "imge_Airtest_L10WETSCRUBBER_NS1_india.R")
legend_image <- as.raster(matrix(matlab.like2(10), nrow =1))
text(x=seq(-160,140,l=5), y=-115, labels= c("0.70", "0.83", "0.95", "1.08", "1.20"), cex=1.2,tck=1)
text(x=160,y=-115,labels=("ng/m^3"),cex=1.2,tck=1)
rasterImage(legend_image, -160, -110, 140,-105)
dev.off()

######Transform the gaseous merucry concentration to 12*24 dimension
imge_TGMStSt_12_24_dimension<-matrix(nrow=12,ncol=24)

for (i in 1:nrow(imge_TGMStSt_12_24_dimension)){
  for(j in 1:ncol(imge_TGMStSt_12_24_dimension)){
    if (i==1&j==1){
      imge_TGMStSt_12_24_dimension[i,j]=imge_TGMStSt[i,j]
    }
    imge_TGMStSt_12_24_dimension[i,j]=imge_TGMStSt[i*15,j*15]
    
  }
  
}

pdf(file="./plots/TGMStSt_L10WETSCRUBBER_NS1_india.pdf",height = 9,width = 12)
par(mar=c(5,4,4,2),cex.lab=1.5,cex.axis=1.5,cex.main=2)
image(xline,yline,t(imge_TGMStSt_12_24_dimension[nrow(imge_TGMStSt_12_24_dimension):1,]),col=matlab.like2(10), breaks=c(7.0e-10,7.5e-10,8.0e-10,8.5e-10,9.0e-10,9.5e-10,1.0e-9,1.05e-9,1.1e-9,1.15e-9,1.2e-9),asp=1,xlab="", ylab="", xaxs="i", yaxs="i",axes= FALSE)
plot(region_polygons$countries,add=TRUE)
title(main="Total Gaseous Mercury Concentration",line=-2,sub="TGMStSt")
axis(2,at =c(-90,-45,0,45,90),padj = 0.5,hadj=1,tick = TRUE,pos=-180,las=1,
     labels=c(expression(paste(90*degree,"S")),expression(paste(45*degree,"S")),
              expression(paste(0*degree,"")),expression(paste(45*degree,"N")),
              expression(paste(90*degree,"N"))))

axis(1,at=c(-180,-120,-60,0,60,120,180),hadj=0.5,padj=-0.3,tick = TRUE,pos = -90,las=1,
     labels=c(expression(paste(180*degree,"W")),expression(paste(120*degree,"W")),
              expression(paste(60*degree,"W")),expression(paste(0*degree,"")),
              expression(paste(60*degree,"E")),expression(paste(120*degree,"E")),
              expression(paste(180*degree,"E"))))
save(imge_TGMStSt_12_24_dimension,file = "imge_AirTGMStSt_L10WETSCRUBBER_NS1_india.R")
legend_image <- as.raster(matrix(matlab.like2(10), nrow =1))
text(x=seq(-160,140,l=5), y=-115, labels= c("0.70", "0.83", "0.95", "1.08", "1.20"), cex=1.2,tck=1)
text(x=160,y=-115,labels=("ng/m^3"),cex=1.2,tck=1)
rasterImage(legend_image, -160, -110, 140,-105)
dev.off()


#######Transform the Surface ocean data in 12*24 dimension
imge_SOTHgStSt_12_24_dimension<-matrix(nrow = 12,ncol=24)
imge_SOHgoStSt_12_24_dimension<-matrix(nrow=12, ncol = 24)

for (i in 1:nrow(imge_SOTHgStSt_12_24_dimension)){
  for(j in 1:ncol(imge_SOTHgStSt_12_24_dimension)){
    if (i==1&j==1){
      imge_SOTHgStSt_12_24_dimension[i,j]=imge_SOTHgStSt[i,j]*10^12
      imge_SOHgoStSt_12_24_dimension[i,j]=imge_SOHgoStSt[i,j]*10^14
    }
    imge_SOTHgStSt_12_24_dimension[i,j]=imge_SOTHgStSt[i*15,j*15]*10^12
    imge_SOHgoStSt_12_24_dimension[i,j]=imge_SOHgoStSt[i*15,j*15]*10^14
    
  }
  
}
# for (i in 1:nrow(imge_SOTHgStSt_12_24_dimension)){
#   for(j in 1:ncol(imge_SOTHgStSt_12_24_dimension)){
#     if (imge_SOTHgStSt_12_24_dimension[i,j]>=7){
#       imge_SOTHgStSt_12_24_dimension[i,j]=0
#     }
#     # if(imge_SOHgoStSt_12_24_dimension[i,j]>=7){
#     #   imge_SOHgoStSt_12_24_dimension[i,j]=0
#     # }
#   }
#   
# }
imge_SOTHgStSt_12_24_dimension[5,13]<-(imge_SOTHgStSt_12_24_dimension[5,13]*0.01)
imge_SOTHgStSt_12_24_dimension[5,14]<-(imge_SOTHgStSt_12_24_dimension[5,14]*0.01)
imge_SOHgoStSt_12_24_dimension[5,13]<-(imge_SOHgoStSt_12_24_dimension[5,13]*0.01)
imge_SOHgoStSt_12_24_dimension[5,14]<-(imge_SOHgoStSt_12_24_dimension[5,14]*0.01)

pdf(file="./plots/SOHgoStSt_L10WETSCRUBBER_NS1_india.pdf",height = 9,width = 12)
par(mar=c(5,4,4,2),cex.lab=1.5,cex.axis=1.5,cex.main=2)
image(xline,yline,t(imge_SOHgoStSt_12_24_dimension[nrow(imge_SOHgoStSt_12_24_dimension):1,]),col=matlab.like2(15),breaks=c(2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,12.0,13.0,14.0,15.0,16.0,19.0,20.0),asp=1,xlab="", ylab="", xaxs="i", yaxs="i",axes= FALSE)
plot(region_polygons$countries,add=TRUE)
title(main="Surface Ocean Hg Concentration",line=-2)
axis(2,at =c(-90,-45,0,45,90),padj = 0.5,hadj=1,tick = TRUE,pos=-180,las=1,
     labels=c(expression(paste(90*degree,"S")),expression(paste(45*degree,"S")),
              expression(paste(0*degree,"")),expression(paste(45*degree,"N")),
              expression(paste(90*degree,"N"))))

axis(1,at=c(-180,-120,-60,0,60,120,180),hadj=0.5,padj=-0.3,tick = TRUE,pos = -90,las=1,
     labels=c(expression(paste(180*degree,"W")),expression(paste(120*degree,"W")),
              expression(paste(60*degree,"W")),expression(paste(0*degree,"")),
              expression(paste(60*degree,"E")),expression(paste(120*degree,"E")),
              expression(paste(180*degree,"E"))))
save(imge_SOHgoStSt_12_24_dimension,file = "imge_SOHgoStSt_L10WETSCRUBBER_NS1_india.R")
legend_image <- as.raster(matrix(matlab.like2(10), nrow =1))
text(x=seq(-160,140,l=9), y=-115, labels= c("2.0", "4.0", "6.0", "8.0", "10.0","13.0","15.0","19.0","20.0"), cex=1.2,tck=1)
text(x=160,y=-115,labels=("100 pg/m^3"),cex=1.2,tck=1)
rasterImage(legend_image, -160, -110, 140,-105)
dev.off()

pdf(file="./plots/SOTHgStSt_L10WETSCRUBBER_NS1_india.pdf",height = 9,width = 12)
par(mar=c(5,4,4,2),cex.lab=1.5,cex.axis=1.5,cex.main=2)
image(xline,yline,t(imge_SOTHgStSt_12_24_dimension[nrow(imge_SOTHgStSt_12_24_dimension):1,]),col=matlab.like2(12),breaks=c(1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.5,6.0,6.5,7.0,8.5),asp=1,xlab="", ylab="", xaxs="i", yaxs="i",axes= FALSE)
plot(region_polygons$countries,add=TRUE)
title(main='Total mercury in surface oceans',lines=-2)
axis(2,at =c(-90,-45,0,45,90),padj = 0.5,hadj=1,tick = TRUE,pos=-180,las=1,
     labels=c(expression(paste(90*degree,"S")),expression(paste(45*degree,"S")),
              expression(paste(0*degree,"")),expression(paste(45*degree,"N")),
              expression(paste(90*degree,"N"))))

axis(1,at=c(-180,-120,-60,0,60,120,180),hadj=0.5,padj=-0.3,tick = TRUE,pos = -90,las=1,
     labels=c(expression(paste(180*degree,"W")),expression(paste(120*degree,"W")),
              expression(paste(60*degree,"W")),expression(paste(0*degree,"")),
              expression(paste(60*degree,"E")),expression(paste(120*degree,"E")),
              expression(paste(180*degree,"E"))))
save(imge_SOTHgStSt_12_24_dimension,file = "imge_SOTHgStSt_L10WETSCRUBBER_NS1_india.R")
legend_image <- as.raster(matrix(matlab.like2(10), nrow =1))
text(x=seq(-160,140,l=7), y=-115, labels= c("1.0", "2.0", "3.0", "4.0", "5.5","6.5","8.5"), cex=1.2,tck=1)
text(x=160,y=-115,labels=("pg/m^3"),cex=1.2,tck=1)
rasterImage(legend_image, -160, -110, 140,-105)
dev.off()

#######Transform the water data in 12*24 dimension
imge_WaterHg0test_12_24_dimension<-matrix(nrow=12,ncol=24)
imge_WaterTHgStSt_12_24_dimension<-matrix(nrow=12,ncol = 24)

for (i in 1:nrow(imge_WaterHg0test_12_24_dimension)){
  for(j in 1:ncol(imge_WaterHg0test_12_24_dimension)){

      imge_WaterHg0test_12_24_dimension[i,j]=imge_WaterHg0test[i,j]*10^7
      imge_WaterTHgStSt_12_24_dimension[i,j]=imge_WaterTHgStSt[i,j]*10^7
  }
  
}
# for (i in 1:nrow(imge_WaterHg0test_12_24_dimension)){
#   for(j in 1:ncol(imge_WaterHg0test_12_24_dimension)){
#     if (imge_WaterHg0test_12_24_dimension[i,j]>=7){
#       imge_WaterHg0test_12_24_dimension[i,j]=0
#     }
#     if(imge_WaterTHgStSt_12_24_dimension[i,j]>=7){
#       imge_WaterTHgStSt_12_24_dimension[i,j]=0
#     }
#   }
#   
# }
imge_WaterHg0test_12_24_dimension[5,13]<-(imge_WaterHg0test_12_24_dimension[5,13]*0.01)
imge_WaterHg0test_12_24_dimension[5,14]<-(imge_WaterHg0test_12_24_dimension[5,14]*0.01)
imge_WaterTHgStSt_12_24_dimension[5,13]<-(imge_WaterTHgStSt_12_24_dimension[5,13]*0.01)
imge_WaterTHgStSt_12_24_dimension[5,14]<-(imge_WaterTHgStSt_12_24_dimension[5,14]*0.01)


pdf(file="./plots/WaterHg0test_L10WETSCRUBBER_NS1_india.pdf",height = 9,width = 12)
par(mar=c(5,4,4,2),cex.lab=1.5,cex.axis=1.5,cex.main=2)
image(xline,yline,t(imge_WaterHg0test_12_24_dimension[nrow(imge_WaterHg0test_12_24_dimension):1,]),col=matlab.like2(8),breaks=c(0.0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4),asp=1,xlab="", ylab="", xaxs="i", yaxs="i",axes= FALSE)
plot(region_polygons$countries,add=TRUE)
title(main='Hg0 concentration in Water',sub = "WaterHg0test",lines=-2)
axis(2,at =c(-90,-45,0,45,90),padj = 0.5,hadj=1,tick = TRUE,pos=-180,las=1,
     labels=c(expression(paste(90*degree,"S")),expression(paste(45*degree,"S")),
              expression(paste(0*degree,"")),expression(paste(45*degree,"N")),
              expression(paste(90*degree,"N"))))

axis(1,at=c(-180,-120,-60,0,60,120,180),hadj=0.5,padj=-0.3,tick = TRUE,pos = -90,las=1,
     labels=c(expression(paste(180*degree,"W")),expression(paste(120*degree,"W")),
              expression(paste(60*degree,"W")),expression(paste(0*degree,"")),
              expression(paste(60*degree,"E")),expression(paste(120*degree,"E")),
              expression(paste(180*degree,"E"))))
save(imge_WaterHg0test_12_24_dimension,file = "imge_WaterHg0test_L10WETSCRUBBER_NS1_india.R")
legend_image <- as.raster(matrix(matlab.like2(8), nrow =1))
text(x=seq(-160,140,l=7), y=-115, labels= c("0.0", "0.1", "0.2", "0.3", "0.4"), cex=1.2,tck=1)
text(x=160,y=-115,labels=("pg/m^3"),cex=1.2,tck=1)
rasterImage(legend_image, -160, -110, 140,-105)
dev.off()


pdf(file="./plots/WaterTHgStSt_L10WETSCRUBBER_NS1_india.pdf",height = 9,width = 12)
par(mar=c(5,4,4,2),cex.lab=1.5,cex.axis=1.5,cex.main=2)
image(xline,yline,t(imge_WaterTHgStSt_12_24_dimension[nrow(imge_WaterTHgStSt_12_24_dimension):1,]),col=matlab.like2(12),breaks=c(2.0,3.0,4.0,5.0,6.0,7.0,8.0,10.0,11.0,12.0,13.0,15.0,17.0),asp=1,xlab="", ylab="", xaxs="i", yaxs="i",axes= FALSE)
#the unit of waterThgStSt is 10e-7 m.
plot(region_polygons$countries,add=TRUE)
title(main='Total Hg concentration in water', lines=-2)
axis(2,at =c(-90,-45,0,45,90),padj = 0.5,hadj=1.0,tick = TRUE,pos=-180,las=1,
     labels=c(expression(paste(90*degree,"S")),expression(paste(45*degree,"S")),
              expression(paste(0*degree,"")),expression(paste(45*degree,"N")),
              expression(paste(90*degree,"N"))))

axis(1,at=c(-180,-120,-60,0,60,120,180),hadj=0.5,padj=-0.3,tick = TRUE,pos = -90,las=1,
     labels=c(expression(paste(180*degree,"W")),expression(paste(120*degree,"W")),
              expression(paste(60*degree,"W")),expression(paste(0*degree,"")),
              expression(paste(60*degree,"E")),expression(paste(120*degree,"E")),
              expression(paste(180*degree,"E"))))
save(imge_WaterTHgStSt_12_24_dimension,file = "imge_WaterTHgStSt_L10WETSCRUBBER_NS1_india.R")
legend_image <- as.raster(matrix(matlab.like2(12), nrow =1))
text(x=seq(-160,140,l=7), y=-115, labels= c("2.0", "4.0", "6.0", "8.0", "11.0","13.0","17.0"), cex=1.2,tck=1)
text(x=160,y=-115,labels=("pg/m^3"),cex=1.2,tck=1)
rasterImage(legend_image, -160, -110, 140,-105)
dev.off()






######Check if the plots are identical with previous simulation###
getwd()
# pic1 = as.raster(readJPEG('./Alternatice scenarios/S2/AirTGM.JPEG'))
# pic2 = as.raster(readJPEG('./Alternatice scenarios/S2/AirTGM.JPEG'))
# # 
# # rasterFromXYZ(xline,yline,z=t(imge_AirTGM[nrow(imge_AirTGM):1,]))
# matrix1 =as.matrix(read.csv('./Alternatice scenarios/S2/Air_TGM.csv', header = FALSE))
# matrix2 =as.matrix(read.csv('./Alternatice scenarios/S2/Air_TGM.csv', header = FALSE))
######## Clear workspace ########
# AirTGM_raster_trial<-raster(imge_AirTGM)
# rm(list = ls())

