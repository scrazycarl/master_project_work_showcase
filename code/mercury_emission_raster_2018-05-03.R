
########## Script for reading mercury emissions and filling them into a raster/ matrix format ##########

# Author: Christopher Oberschelp
# Contact: oberschelp@ifu.baug.ethz.ch
# Last iteration: 2018-05-03
# version number: 0.01

# Description: File for reading, formatting and comparing mercury emissions

# Changelog:
#   2018-05-03 - Initial version created

######## Preparation/ setup ########

# Start with clean workspace
rm(list = ls())

###### Install packages for manipulation of spatial files ######

# only needed once
# install.packages("sp")
# install.packages("raster")
# install.packages("rgdal")
# install.packages("rgeos") 

###### Activate packages ######

require("sp")
require("raster")
require("rgdal")
require("rgeos")
require('R.matlab')
######## Convert 2010 emission data into raster/ matrix format ########

###### Load data ######

# load grid characterization file
cellSurfaceArea05_14feb2013_FS <- read.table("/Users/shirzart/Desktop/Master Project work/Data/Gridded emission data/gridded_Hg_emissions_2010v1.zip/cellSurfaceArea05_14feb2013_FS.txt", header = T, sep = "\t")

# load emission data file
globalHgEmission2010ALL_14feb2013_FS_SW <- read.table("/Users/shirzart/Desktop/Master Project work/Data/Gridded emission data/gridded_Hg_emissions_2010v1.zip/globalHgEmission2010ALL_14feb2013_FS_SW.txt", header = T, sep = ",")
globalHgEmission2010SC<- read.table("/Users/shirzart/Desktop/Master Project work/Data/gridded_Hg_emissions_2010v1_sector_subsets_2jan2014/Hg_globalEmissions2010_StationaryCombustion_2jan2014.txt", header = T, sep = ",")
###### Create empty raster file for data #######

HgT_emission_raster <- raster(
  nrows=length(unique(cellSurfaceArea05_14feb2013_FS$centerLat)), 
  ncols=length(unique(cellSurfaceArea05_14feb2013_FS$centerLon)), 
  xmn=min(cellSurfaceArea05_14feb2013_FS$centerLon)-(max(cellSurfaceArea05_14feb2013_FS$centerLon)-min(cellSurfaceArea05_14feb2013_FS$centerLon))/(length(unique(cellSurfaceArea05_14feb2013_FS$centerLon))-1)/2, 
  xmx=max(cellSurfaceArea05_14feb2013_FS$centerLon)+(max(cellSurfaceArea05_14feb2013_FS$centerLon)-min(cellSurfaceArea05_14feb2013_FS$centerLon))/(length(unique(cellSurfaceArea05_14feb2013_FS$centerLon))-1)/2, 
  ymn=min(cellSurfaceArea05_14feb2013_FS$centerLat)-(max(cellSurfaceArea05_14feb2013_FS$centerLat)-min(cellSurfaceArea05_14feb2013_FS$centerLat))/(length(unique(cellSurfaceArea05_14feb2013_FS$centerLat))-1)/2, 
  ymx=max(cellSurfaceArea05_14feb2013_FS$centerLat)+(max(cellSurfaceArea05_14feb2013_FS$centerLat)-min(cellSurfaceArea05_14feb2013_FS$centerLat))/(length(unique(cellSurfaceArea05_14feb2013_FS$centerLat))-1)/2)

Hg0andHgII_emission_raster<-HgT_emission_raster
HgP_emission_raster<-HgT_emission_raster
##empty raster file for stationary combustion data
HgT_sc_emission_raster<-HgT_emission_raster
Hg0andHgII_sc_emission_raster<-HgT_emission_raster
HgP_sc_emission_raster<-HgT_emission_raster

###### Fill values in ######

# this will take a while because there are many data fields
# HgT
for(i in 1:nrow(globalHgEmission2010ALL_14feb2013_FS_SW)){
  
  centerLon <- cellSurfaceArea05_14feb2013_FS$centerLon[match(globalHgEmission2010ALL_14feb2013_FS_SW$cell_code[i], cellSurfaceArea05_14feb2013_FS$cellCode)]
  centerLat <- cellSurfaceArea05_14feb2013_FS$centerLat[match(globalHgEmission2010ALL_14feb2013_FS_SW$cell_code[i], cellSurfaceArea05_14feb2013_FS$cellCode)]
  
  values(HgT_emission_raster)[cellFromXY(object = HgT_emission_raster, xy = c(centerLon, centerLat))] <- globalHgEmission2010ALL_14feb2013_FS_SW$HgT[i]
  values(Hg0andHgII_emission_raster)[cellFromXY(object = Hg0andHgII_emission_raster, xy = c(centerLon, centerLat))] <- (globalHgEmission2010ALL_14feb2013_FS_SW$Hg0_1[i]
                                                                                                          +globalHgEmission2010ALL_14feb2013_FS_SW$Hg0_2[i]
                                                                                                          +globalHgEmission2010ALL_14feb2013_FS_SW$Hg0_3[i]
                                                                                                          +globalHgEmission2010ALL_14feb2013_FS_SW$Hg2_1[i]
                                                                                                          +globalHgEmission2010ALL_14feb2013_FS_SW$Hg2_2[i]
                                                                                                          +globalHgEmission2010ALL_14feb2013_FS_SW$Hg2_3[i])
  values(HgP_emission_raster)[cellFromXY(object = HgP_emission_raster, xy = c(centerLon, centerLat))] <- (globalHgEmission2010ALL_14feb2013_FS_SW$HgP_1[i]
                                                                                                        +globalHgEmission2010ALL_14feb2013_FS_SW$HgP_2[i]
                                                                                                        +globalHgEmission2010ALL_14feb2013_FS_SW$HgP_3[i])
  rm(centerLon, centerLat)  
  
};rm(i)

###fill in value for stationary combustion data raster
for(i in 1:nrow(globalHgEmission2010SC)){
  
  centerLon <- cellSurfaceArea05_14feb2013_FS$centerLon[match(globalHgEmission2010SC$cell_code[i], cellSurfaceArea05_14feb2013_FS$cellCode)]
  centerLat <- cellSurfaceArea05_14feb2013_FS$centerLat[match(globalHgEmission2010SC$cell_code[i], cellSurfaceArea05_14feb2013_FS$cellCode)]
  
  values(HgT_sc_emission_raster)[cellFromXY(object = HgT_sc_emission_raster, xy = c(centerLon, centerLat))] <- globalHgEmission2010SC$HgT[i]
  values(Hg0andHgII_sc_emission_raster)[cellFromXY(object = Hg0andHgII_sc_emission_raster, xy = c(centerLon, centerLat))] <- (globalHgEmission2010SC$Hg0_1[i]
                                                                                                                        +globalHgEmission2010SC$Hg0_2[i]
                                                                                                                        +globalHgEmission2010SC$Hg0_3[i]
                                                                                                                        +globalHgEmission2010SC$Hg2_1[i]
                                                                                                                        +globalHgEmission2010SC$Hg2_2[i]
                                                                                                                        +globalHgEmission2010SC$Hg2_3[i])
  values(HgP_sc_emission_raster)[cellFromXY(object = HgP_sc_emission_raster, xy = c(centerLon, centerLat))] <- (globalHgEmission2010SC$HgP_1[i]
                                                                                                          +globalHgEmission2010SC$HgP_2[i]
                                                                                                          +globalHgEmission2010SC$HgP_3[i])
  rm(centerLon, centerLat)  
  
};rm(i)

###### Plot results ######

plot(HgT_emission_raster)
plot(Hg0andHgII_emission_raster)
plot(HgP_emission_raster)

###### Extract results into a matrix ######
###### Aggregate raster by facotr of 2 to match the input dimension of WorM3 and extract data as matrix

#Extract data
HgT_emission_matrix_original<- raster::as.matrix(aggregate(x = HgT_emission_raster, fun = sum, fact=2))
Hg0andHgII_emission_matrix_original<- raster::as.matrix(aggregate(x = Hg0andHgII_emission_raster, fun = sum, fact=2))
HgP_emission_matrix_original<- raster::as.matrix(aggregate(x = HgP_emission_raster, fun = sum, fact=2))

HgT_emission_matrix_sc<- raster::as.matrix(aggregate(x = HgT_sc_emission_raster, fun = sum, fact=2))
Hg0andHgII_emission_matrix_sc<- raster::as.matrix(aggregate(x = Hg0andHgII_sc_emission_raster, fun = sum, fact=2))
HgP_emission_matrix_sc<- raster::as.matrix(aggregate(x = HgP_sc_emission_raster, fun = sum, fact=2))

# Set NA data to 0
HgT_emission_matrix_original[is.na(HgT_emission_matrix_original)] <- 0
Hg0andHgII_emission_matrix_original[is.na(Hg0andHgII_emission_matrix_original)] <- 0
HgP_emission_matrix_original[is.na(HgP_emission_matrix_original)] <- 0

HgT_emission_matrix_sc[is.na(HgT_emission_matrix_sc)] <- 0
Hg0andHgII_emission_matrix_sc[is.na(Hg0andHgII_emission_matrix_sc)] <- 0
HgP_emission_matrix_sc[is.na(HgP_emission_matrix_sc)] <- 0
###### Aggregate raster by a factor of ten and summing up the values #######

HgT_emission_matrix_aggregated <- raster::aggregate(x = HgT_emission_raster, fun = sum, fact=10)


#####generate data matrix without coal power plant mercury emission#####
### According to the data in AMAP 2013 report, SC-PP-Coal=83.44% *SC

HgT_emission_matrix_without_ppcoal<-(HgT_emission_matrix_original-0.8344*HgT_emission_matrix_sc)
Hg0andHgII_emission_matrix_without_ppcoal<-(Hg0andHgII_emission_matrix_original-0.8344*Hg0andHgII_emission_matrix_sc)
HgP_emission_matrix_without_ppcoal<-(HgP_emission_matrix_original-0.8344*HgP_emission_matrix_sc)



######Generate data with our_pp_coal+the_rest_original_concentration matrix
own_pp_coal_Hg0andHgII_emission<-matrix(unlist(readMat(con='/Users/shirzart/Desktop/Master Project work/Code/third simulation/E_2012_Hg0andHgII.mat',fixNames = FALSE)),nrow=180,ncol=360)
own_pp_coal_HgP_emission<-matrix(unlist(readMat(con='/Users/shirzart/Desktop/Master Project work/Code/third simulation/E_2012_Hg_P.mat',fixNames = FALSE)),nrow=180,ncol=360)


###### Plot aggregated results ######

plot(HgT_emission_matrix_aggregated)

######## Convert own power plant emissions into raster format ########

# Follows an almost identical approach, but emissions are not overwritten in the raster file as before but instead added up

###### Create empty raster file for data #######

pp_hg_emission_raster <- raster(
  nrows=length(unique(cellSurfaceArea05_14feb2013_FS$centerLat)), 
  ncols=length(unique(cellSurfaceArea05_14feb2013_FS$centerLon)), 
  xmn=min(cellSurfaceArea05_14feb2013_FS$centerLon)-(max(cellSurfaceArea05_14feb2013_FS$centerLon)-min(cellSurfaceArea05_14feb2013_FS$centerLon))/(length(unique(cellSurfaceArea05_14feb2013_FS$centerLon))-1)/2, 
  xmx=max(cellSurfaceArea05_14feb2013_FS$centerLon)+(max(cellSurfaceArea05_14feb2013_FS$centerLon)-min(cellSurfaceArea05_14feb2013_FS$centerLon))/(length(unique(cellSurfaceArea05_14feb2013_FS$centerLon))-1)/2, 
  ymn=min(cellSurfaceArea05_14feb2013_FS$centerLat)-(max(cellSurfaceArea05_14feb2013_FS$centerLat)-min(cellSurfaceArea05_14feb2013_FS$centerLat))/(length(unique(cellSurfaceArea05_14feb2013_FS$centerLat))-1)/2, 
  ymx=max(cellSurfaceArea05_14feb2013_FS$centerLat)+(max(cellSurfaceArea05_14feb2013_FS$centerLat)-min(cellSurfaceArea05_14feb2013_FS$centerLat))/(length(unique(cellSurfaceArea05_14feb2013_FS$centerLat))-1)/2)

###### Fill values in ######

# default value = 0
values(pp_hg_emission_raster) <- 0

# add all other values (this will take a while because there are many data fields)
for(i in 1:nrow(pp_hg)){
  
  centerLon <- pp_hg$LONG[i]
  centerLat <- pp_hg$LAT[i]
  
  values(pp_hg_emission_raster)[cellFromXY(object = pp_hg_emission_raster, xy = c(centerLon, centerLat))] <- pp_hg$HG_KG[i] + values(pp_hg_emission_raster)[cellFromXY(object = pp_hg_emission_raster, xy = c(centerLon, centerLat))]
  
  rm(centerLon, centerLat)  
  
};rm(i)

###### Plot results ######

plot(pp_hg_emission_raster)

###### Plot value comparisons ######

# plot comparison on highest resolution
plot(values(HgT_emission_raster), values(pp_hg_emission_raster), xlab="HgT emissions", ylab="pp_hg emissions", asp=1); abline(0, 1, col="red", lwd=2)

# plot aggregated comparison (factor 10)
plot(values(raster::aggregate(x = HgT_emission_raster, fun = sum, fact=10)), values(raster::aggregate(x = pp_hg_emission_raster, fun = sum, fact=10)), xlab="HgT emissions (aggregated)", ylab="pp_hg emissions (aggregated)", asp=1); abline(0, 1, col="red", lwd=2)

####save without_pp_coal matrix as R file###
save(Hg0andHgII_emission_matrix_without_ppcoal,file = "/Users/shirzart/Desktop/Master Project work/Code/Mercury model/Hg0andHgII_emission_matrix_without_ppcoal_2010.R")
save(HgP_emission_matrix_without_ppcoal,file="/Users/shirzart/Desktop/Master Project work/Code/Mercury model/HgP_emission_matrix_without_ppcoal_2010.R")
# save(Hg0andHgII_emission_matrix_without_ppcoal,file='/Users/shirzart/Desktop/Master Project work/Code/Mercury model/Hg0andHgII_emission_matrix_without_ppcoal_2010.R')
# save(HgP_emission_matrix_without_ppcoal, file="/Users/shirzart/Desktop/Master Project work/Code/Mercury model/HgP_emission_matrix_without_ppcoal_2010.R")
#######Creat Emission.mat file to WorM3#######
writeMat(con='/Users/shirzart/Desktop/Master Project work/Code/AMAP_2010_simulation/E_2010_Hg0andHgII_original.mat',E_2010_Hg0andHgII_original=Hg0andHgII_emission_matrix_original)
writeMat(con='/Users/shirzart/Desktop/Master Project work/Code/AMAP_2010_simulation/E_2010_Hg_P_original.mat',E_2010_Hg_P_original=HgP_emission_matrix_original)
writeMat(con='/Users/shirzart/Desktop/Master Project work/Code/AMAP_2010_simulation/E_2010_Hg0andHgII_without_pp_coal.mat',E_2010_Hg0andHgII_without_pp_coal=Hg0andHgII_emission_matrix_without_ppcoal)
writeMat(con='/Users/shirzart/Desktop/Master Project work/Code/AMAP_2010_simulation/E_2010_Hg_P_without_pp_coal.mat',E_2010_Hg_P_without_pp_coall=HgP_emission_matrix_without_ppcoal)
######From the literature, (Pacyna,etal 2005) 
# Hg0=53% of HgT, HgII=37% of HgT, HgP=10% of HgT
# SC-PP-coal is 83.45% of SC




