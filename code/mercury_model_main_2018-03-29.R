########## Mercury emission model ##########

# Author: Christopher Oberschelp
# Contact: oberschelp@ifu.baug.ethz.ch
# Last iteration: 2018-03-29
# version number: 0.01

# Description: Central file for calculating mercury emissions from coal power plants 

# Changelog:
#   2018-03-29 - Initial version created

# Modification LOG
# 2018-04-20-Adpated the output data into the format
# 1) After getting the emission data pp_hg, Hg0andHgII, HgP are calculated,transformed
#  into non-zero-integer-coordinate number index matrix. Then, two matlab varialbes are 
#  written as input to the WorM3 model.
#
# Modification LOG
# 2018-07-31
# 1)  ATTENTION: before running the code, the input and output file name has to be changed
#   for last 20% scenario S1: ..._adapted_2010base.output,   last 40% scenairo: ..._adapted_2010base.output
#   
### NOTICE: In the line 180 and 181, the path has to be changed accroding to users local 
###  address


######## Basic setup ########

# Start with clear workspace
rm(list = ls())

# Set directory (adapt if necessary)
setwd("/Users/shirzart/Desktop/Master Project work/Code/Mercury model")

# Install packages (if necessary - only needed once)
# install.packages("maptools") # needed for map import

# Activate packages
require(maptools)
require(R.matlab)
# Load data
source("./Input/mercury_model_data_2018-03-29.R", echo = T)

# Load functions
source("./Input/mercury_model_functions_2018-03-29.R", echo = T)

######## Run model ########

# Calculate power plant useful heat output (co-generation/ CHP)
pp_hg$HEAT_OUTPUT_MWH <- pp_hg$NET_GENERATION_MWH*pp_hg$HEAT_TO_POWER_RATIO
pp_hg$HEAT_OUTPUT_MJ <- pp_hg$HEAT_OUTPUT_MWH*3600

# Calculate power plant fuel heat input
pp_hg$HEAT_INPUT_FUEL_MWH <- pp_hg$NET_GENERATION_MWH/pp_hg$ELECTRICAL_EFFICIENCY/(100-pp_hg$INTERNAL_ELECTRICITY_LOSSES_PERCENT)*100
pp_hg$HEAT_INPUT_FUEL_MJ <- pp_hg$HEAT_INPUT_FUEL_MWH*3600

# Calculate washed coal properties per mine
coal_properties_per_mine$washed <- t(sapply(1:nrow(coal_properties_per_mine$raw), function(x) coal_washing(coal_sample = data.frame(coal_properties_per_mine$raw[x,]))))

# Calculate distances between power plants and mines
distances_km <- spatial.distance_table_for_matching_property(from_long = mines_hg$Longitude.degrees, from_lat = mines_hg$Latitude.degrees, from_property = mines_hg$Coal.Group, to_long = pp_hg$LONG, to_lat = pp_hg$LAT, to_property = pp_hg$FUELTYPE_SIMPLE, no_match="all")

# Calculate coal flow shares for anthracite coal from mines to power plants
input_shares_matrix <- spatial.distribute_inputs_per_property(property = "Anthracite", from_property = mines_hg$Coal.Group, from_region = mines_hg$ISO2, to_property = pp_hg$FUELTYPE_SIMPLE, to_region = pp_hg$ISO2, distances = distances_km, trade_region_by_region=IEA_coal_sources$Anthracite, input_shares_matrix=NA, add_to_input_shares_matrix = F, from_exporting = mines_hg$Export_coal, from_domestic = mines_hg$Domestic_coal)

# Calculate coal flow shares for bituminous coal from mines to power plants
input_shares_matrix <- spatial.distribute_inputs_per_property(property = "Bituminous", from_property = mines_hg$Coal.Group, from_region = mines_hg$ISO2, to_property = pp_hg$FUELTYPE_SIMPLE, to_region = pp_hg$ISO2, distances = distances_km, trade_region_by_region=IEA_coal_sources$Other_bituminous, input_shares_matrix=input_shares_matrix, add_to_input_shares_matrix = T, from_exporting = mines_hg$Export_coal, from_domestic = mines_hg$Domestic_coal)

# Calculate coal flow shares for subbituminous coal from mines to power plants
input_shares_matrix <- spatial.distribute_inputs_per_property(property = "Subbituminous", from_property = mines_hg$Coal.Group, from_region = mines_hg$ISO2, to_property = pp_hg$FUELTYPE_SIMPLE, to_region = pp_hg$ISO2, distances = distances_km, trade_region_by_region=IEA_coal_sources$Subbituminous, input_shares_matrix=input_shares_matrix, add_to_input_shares_matrix = T, from_exporting = mines_hg$Export_coal, from_domestic = mines_hg$Domestic_coal)

# Calculate coal flow shares for lignite coal from mines to power plants
input_shares_matrix <- spatial.distribute_inputs_per_property(property = "Lignite", from_property = mines_hg$Coal.Group, from_region = mines_hg$ISO2, to_property = pp_hg$FUELTYPE_SIMPLE, to_region = pp_hg$ISO2, distances = distances_km, trade_region_by_region=IEA_coal_sources$Lignite, input_shares_matrix=input_shares_matrix, add_to_input_shares_matrix = T, from_exporting = mines_hg$Export_coal, from_domestic = mines_hg$Domestic_coal)

# Calculate all possible distances for plants currently without input
distances_km <- spatial.calculate_fallback_distances(input_shares_matrix=input_shares_matrix, distances=distances_km, from_long=mines_hg$Longitude.degrees, from_lat=mines_hg$Latitude.degrees, to_long=pp_hg$LONG, to_lat=pp_hg$LAT)

# Calculate coal flow shares for trade data gaps from mines to power plants
input_shares_matrix <- spatial.get_closest_input_with_property(input_shares_matrix=input_shares_matrix, distances=distances_km, from_property=mines_hg$Coal.Group, to_property=pp_hg$FUELTYPE_SIMPLE)

# Calculate coal flow shares for waste coal and remaining trade data gaps from mines to power plants
input_shares_matrix <- spatial.get_closest_input(input_shares_matrix=input_shares_matrix, distances=distances_km)

# Drop all unneeded mines
unneeded_mines <- apply(input_shares_matrix, 1, function(x) sum(x)==0)
distances_km <- distances_km[!unneeded_mines,]
input_shares_matrix <- input_shares_matrix[!unneeded_mines,]
mines_hg <- mines_hg[!unneeded_mines,]
coal_properties_per_mine$waste <- coal_properties_per_mine$waste[!unneeded_mines,]
coal_properties_per_mine$raw <- coal_properties_per_mine$raw[!unneeded_mines,]
coal_properties_per_mine$washed <- coal_properties_per_mine$washed[!unneeded_mines,]
rm(unneeded_mines)

# Calculating washed/ unwashed/ waste coal shares (coal-specific function)
input_type_shares <- spatial.set_input_coal_type_shares(to_property=pp_hg$FUELTYPE_SIMPLE, input_shares_matrix=input_shares_matrix, to_region=pp_hg$ISO2, from_region=mines_hg$ISO2, waste_properties="Waste coal", domestic_washing_share=mines_hg$Washing_share_domestic, export_washing_share=mines_hg$Washing_share_export)

# Calculate coal properties at power plants
average_properties_at_consumer <- spatial.get_average_properties_at_consumer(input_shares_matrix = input_shares_matrix, input_type_shares = input_type_shares, input_properties_list = coal_properties_per_mine)

# Import key coal properties into power plant database (and convert Hg and Cl content from dry coal mass base to total coal mass base)
pp_hg <- pp_hg.add_coal_properties(pp_hg = pp_hg, average_properties_at_consumer = average_properties_at_consumer, property_vector = colnames(coal_properties_per_mine$waste), DEBUG=F)

# Get total annual coal consumption in power plant database
pp_hg$COAL_AVG_ANNUAL_DEMAND_KG <- pp_hg$HEAT_INPUT_FUEL_MJ/pp_hg$COAL_AVG_LHV_MJ_PER_KG

# Add Hg flue gas treatment removal rates
pp_hg <- pp_hg.add_removal_rates(pp_hg = pp_hg, DEBUG=F)

# Calculating Hg emissions at the power plants
pp_hg <- pp_hg.calculate_emissions(pp_hg = pp_hg, DEBUG=F)

# Calculating Hg emission speciation
pp_hg <- pp_hg.hg_species_split(pp_hg = pp_hg)

# Emission calculations complete

######## Plot some results for plausibility check ########

# Load world map
region_polygons=list()
region_polygons$countries <- readShapeSpatial("./Input/ne_110m_admin_0_countries/ne_110m_admin_0_countries.shp")

# Plot world map
# Add power plant mercury emissions (area of circles proportional to annual release amount)
par(mar=c(2,2,2,2))
plot(region_polygons$countries)
points(pp_hg_sorted$LONG, pp_hg_sorted$LAT, pch=21, bg="red", cex=sqrt(pp_hg_sorted$HG_KG)/max(sqrt(pp_hg_sorted$HG_KG), na.rm=T)*3)
all_plants_adapted_2010base_map<-save.image("./Output/all_plants_adapted_2010base_map.RData")
dev.copy(jpeg,'./Output/all_plants_adapted_2010base_map.JPEG',res=100)
dev.off()

####Plot world map
####PLOT the dirtiest power plant that has been changed
plot(region_polygons$countries)
points(pp_hg$LONG[index_lastNpercent_data], pp_hg$LAT[index_lastNpercent_data], pch=21, bg="red", cex=sqrt(pp_hg$HG_KG)/max(sqrt(pp_hg$HG_KG), na.rm=T)*3)
plants_adapted_2010base_map<-save.image("./Output/plants_adapted_2010base_map.RData")
dev.copy(jpeg,'./Output/plants_adapted_2010base_map.JPEG',res=100)
dev.off()


######## Save output ########

# Save entire workspace



# Export power plant database to csv
write.table(x = pp_hg, file = "./Output/pp_hg_adapted_2010base.csv", row.names = F, sep = ";")


# Files can be loaded with "load.image()" and "read.table()"
load.image(resultmap)
read.table('./Output/pp_hg_adapted_2010base.csv',comment.char = '',sep=";")


###save pp_hg emission as deafult scenario. Later on, the alternative scenarios are modified based on this one
save(pp_hg, file = "default_pp_hg.R",pp_hg)
##########Above gives the mercury emission calculted by CPPE model

##########Preparing output as input into Qureshi model#######
###round the coordniate number and calculate Hg0+HgII, HgP emission
pp_hg_with_coordinate_number<-cbind(pp_hg,cbind(LONG_round=round(pp_hg$LONG,digits=0),LAT_round=round(pp_hg$LAT,digits=0)))


###calculate Hg_P, Hg0andHgII emission,for the WorM3 model
Hg_P<-array(as.array(pp_hg$HG_KG)*as.array(pp_hg$HG_P_PERCENT/100))
Hg0andHgII<-array(as.array(pp_hg$HG_KG)*as.array(pp_hg$HG_0_PERCENT/100)+as.array(pp_hg$HG_KG)*as.array(pp_hg$HG_2P_PERCENT/100))

pp_hg_with_coordinate_number<-cbind(pp_hg_with_coordinate_number,cbind(Hg_P,Hg0andHgII))

###sum up the emissions which at the same longitude and latitude AND transform the emission data into matrix###
Hg0andHgII_integer_coordinate_emission<-as.matrix(aggregate(pp_hg_with_coordinate_number$Hg0andHgII,list(pp_hg_with_coordinate_number$LONG_round,pp_hg_with_coordinate_number$LAT_round),sum),ncol=3)
Hg_P_integer_coordinate_emission<-as.matrix(aggregate(pp_hg_with_coordinate_number$Hg_P,list(pp_hg_with_coordinate_number$LONG_round,pp_hg_with_coordinate_number$LAT_round),sum),ncol=3)

###set the coordinates to none-zero value###
Hg0andHgIIemission_coordinateNonzero<-cbind(Hg0andHgII_integer_coordinate_emission[,1]+180,cbind(Hg0andHgII_integer_coordinate_emission[,2]+90,Hg0andHgII_integer_coordinate_emission[,3]))
Hg_P_emission_coordinateNonzero<-cbind(Hg_P_integer_coordinate_emission[,1]+180,cbind(Hg_P_integer_coordinate_emission[,2]+90,Hg_P_integer_coordinate_emission[,3]))

###Transform the emission data into MATLAB formmatrix of (longitude, latitude, emission)
Hg0andHgII_input2matlab_matrix<-matrix(0,ncol = 360,nrow=180)
Hg_P_input2matlab_matrix<-matrix(0,ncol = 360,nrow=180)
#set up coordinate-indexed matrix
for (j in 1:360) {
  for (i in 1:180){
    if (length(Hg0andHgIIemission_coordinateNonzero[Hg0andHgIIemission_coordinateNonzero[,1]==j&Hg0andHgIIemission_coordinateNonzero[,2]==i,3])!=0){
      Hg0andHgII_input2matlab_matrix[i,j]=Hg0andHgIIemission_coordinateNonzero[Hg0andHgIIemission_coordinateNonzero[,1]==j&Hg0andHgIIemission_coordinateNonzero[,2]==i,3]
      
    }
  }
}

for (j in 1:360) {
  for (i in 1:180){
    if (length(Hg_P_emission_coordinateNonzero[Hg_P_emission_coordinateNonzero[,1]==j&Hg_P_emission_coordinateNonzero[,2]==i,3])!=0){
      Hg_P_input2matlab_matrix[i,j]=Hg_P_emission_coordinateNonzero[Hg_P_emission_coordinateNonzero[,1]==j&Hg_P_emission_coordinateNonzero[,2]==i,3]
      
    }
  }
}

####Source AMAP 2010 data without pp_coal##
load("/Users/shirzart/Desktop/Master Project work/Code/Mercury model/Hg0andHgII_emission_matrix_without_ppcoal_2010.R")
load("/Users/shirzart/Desktop/Master Project work/Code/Mercury model/HgP_emission_matrix_without_ppcoal_2010.R")

E_adapted_Hg0andHgII<-(Hg0andHgII_emission_matrix_without_ppcoal+Hg0andHgII_input2matlab_matrix)
E_adapted_HgP<-(HgP_emission_matrix_without_ppcoal+Hg_P_input2matlab_matrix)
######################Double check the  total Hg emission####
Total_Hg_CPPE<-sum(pp_hg$HG_KG)
Total_Hg0HgII_HgP<-sum(pp_hg_with_coordinate_number$Hg0andHgII+pp_hg_with_coordinate_number$Hg_P)
try(if(identical(Total_Hg_CPPE,Total_Hg0HgII_HgP)!=TRUE)stop("The total mercury emission amount doesnt match after extracting Hg0andHgII, HgP" ))

######save Hg0andHgII, HgP as matlab matrix####
##save Hg0andHgII as matlab matrix
writeMat(con='./Alternative scenarios/E_L10WETSCRUBBER_adapted_Hg0andHgII_adapted_2010base.mat',E_L10WETSCRUBBER_adapted_Hg0andHgII_adapted_2010base=E_adapted_Hg0andHgII)
writeMat(con='./Alternative scenarios/E_L10WETSCRUBBER_adapted_Hg_P_adapted_2010base.mat',E_L10WETSCRUBBER_adapted_Hg_P_adapted_2010base=E_adapted_HgP)

######## Clear workspace ########



#########The following generate the index, plot of several criteria
######modification of the pp_hg matrix########

pp_hg<-cbind(pp_hg,efficiency=(pp_hg$HG_KG/pp_hg$NET_GENERATION_MWH))
pp_hg$efficiency<-pp_hg$efficiency*10^7

###create sorted emission data frame
pp_hg_sorted<-data.frame(pp_hg[with(pp_hg,order(pp_hg$HG_KG,decreasing=FALSE)),])
pp_hg_sorted_new<-data.frame(pp_hg[with(pp_hg,order(pp_hg$NET_GENERATION_MWH,decreasing=FALSE)),])
pp_hg_sorted_new2<-data.frame(pp_hg[with(pp_hg,order(pp_hg$efficiency,decreasing=FALSE)),])

#####generate different index for powerplants with/without SO2/AC control. wo means without
woSO2CTL_MWHsorted_index<-which(pp_hg_sorted_new$SO2CTL_SIMPLE=="None")
withSO2CTL_MWHsorted_index<-which(pp_hg_sorted_new$SO2CTL_SIMPLE!="None")
withAC_MWHsorted_index<-which(pp_hg_sorted_new$HGCTL_SIMPLE=="Activated carbon injection")
woAC_MWHsorted_index<-which(pp_hg_sorted_new$HGCTL_SIMPLE=="None")

#####generate cumulative plot of Hg emission VS Capacity increase of Power Plants
##### and save as pdf pltos
x<-pp_hg_sorted_new$NET_GENERATION_MWH
HGKG<-cumsum(pp_hg_sorted_new$HG_KG)
pdf(file="./comparison/cumulative distribution of Hg emission vs capacity of power plants.pdf",height = 9,width = 12)
par(mar=c(5,7,5,3),cex.main=2.5,cex.label=2, cex.axis=2,cex.sub=2)
plot(x,HGKG,title(main="Cumulative distribution of Hg Emission
    vs net electricity generation increase of power plants"),xlim=c(min(pp_hg_sorted_new$NET_GENERATION_MWH),
                                                     max(pp_hg_sorted_new$NET_GENERATION_MWH)),
     ylim=c(min(HGKG),max(HGKG)),las=1,ylab="",xlab="")
points(x[woSO2CTL_MWHsorted_index],HGKG[woSO2CTL_MWHsorted_index],col="RED",pch=23)
# points(x[withAC_MWHsorted_index],HGKG[withAC_MWHsorted_index],col="blue")
title(ylab="",line = 4,las=1, cex=1.5)
title(xlab="" ,cex=2)
legend("bottomright",legend = c("All power plants","Power plants without SO2 control"),
       pch=21,col = c("Black","Red"),cex =2)
dev.off()

####Generate the map of total power plants in red. Power plants without SO2 control in blue
pdf(file = "./comparison/woSO2versustotal.pdf",height = 9,width = 12)
par(mar=c(5,4,4,2))
plot(region_polygons$countries)
points(pp_hg_sorted_new$LONG, pp_hg_sorted_new$LAT, pch=21, bg="red", cex=sqrt(pp_hg_sorted_new$HG_KG)/max(sqrt(pp_hg_sorted_new$HG_KG), na.rm=T)*3)
points(points(pp_hg_sorted_new$LONG[woSO2CTL_MWHsorted_index], pp_hg_sorted_new$LAT[woSO2CTL_MWHsorted_index],
              pch=21, bg="BLUE", cex=sqrt(pp_hg_sorted_new$HG_KG)/max(sqrt(pp_hg_sorted_new$HG_KG),
                                                                      na.rm=T)*3))
legend("bottomleft",legend = c("All power plants","Power plants without SO2 control"),
       pch=21,col = c("Red","blue"),cex = 1)
plants_S1_map<-save.image("./Output/plants_last100percent_default_map.RData")
title("Distribution of power plants")
dev.off()

####Generate the map of total power plants in red. Power plants without AC control in blue
pdf(file = "./comparison/woACversustotal.pdf",height = 9,width = 12)
par(mar=c(5,4,4,2))
plot(region_polygons$countries)
points(pp_hg_sorted_new$LONG, pp_hg_sorted_new$LAT, pch=21, bg="red", cex=sqrt(pp_hg_sorted_new$HG_KG)/max(sqrt(pp_hg_sorted_new$HG_KG), na.rm=T)*3)
points(points(pp_hg_sorted_new$LONG[woAC_MWHsorted_index], pp_hg_sorted_new$LAT[woAC_MWHsorted_index],
              pch=21, bg="BLUE", cex=sqrt(pp_hg_sorted_new$HG_KG)/max(sqrt(pp_hg_sorted_new$HG_KG),
                                                                      na.rm=T)*3))
legend("bottomleft",legend = c("All power plants","Power plants without AC control"),
       pch=21,col = c("Red","blue"),cex = 1)
title("Distribution of power plants",cex=2)
dev.off()


# ###############cumulative distribution Hg_kg vs hg_kg/net_generation
# woSO2CTL_efficiencySorted_index<-which(pp_hg_sorted_new2$SO2CTL_SIMPLE=="None")
# withSO2CTL_efficiencySorted_index<-which(pp_hg_sorted_new2$SO2CTL_SIMPLE!="None")
# withAC_efficiencySorted_index<-which(pp_hg_sorted_new2$HGCTL_SIMPLE=="Activated carbon injection")
# woAC_efficiencySorted_index<-which(pp_hg_sorted_new2$HGCTL_SIMPLE=="None")
# 
# 
# quartz(width = 10,height=10,pointsize = 5)
# y<-pp_hg_sorted_new2$efficiency
# HGKG<-cumsum(pp_hg_sorted_new2$HG_KG)
# par(mar=c(1,3,1,3))
# plot(y,HGKG,title(main="Cumulative Distribution of Hg Emission 
#                   versus Efficiency Increase of Power Plant"),xlim=c(min(pp_hg_sorted_new2$efficiency  ),
#                                                                    max(pp_hg_sorted_new2$efficiency)),
#      ylim=c(min(HGKG),max(HGKG)),las=1,ylab="",xlab="")
# points(y[woSO2CTL_efficiencySorted_index],HGKG[woSO2CTL_efficiencySorted_index],col="RED")
# # points(y[woAC_efficiencySorted_index],HGKG[woAC_efficiencySorted_index],col="blue")
# legend("bottomright",legend = c("All power plants","Power plants without SO2 control"),
#        pch=21,col = c("Black","Red"),cex = 0.5)
# par(axis(2,las=1))
# title(ylab="Hg emission [KG]",line = 4,las=1)
# title(xlab="Capacity of pwer plant [MWH]" )




######The following code is used for generation the index of desired power palnts
#####get the index of the last N percent of data according to Hg_KG Rank
n=0.2
nn_pp_coal<-as.integer(length(pp_hg_sorted$ISO2)*n)
lastNpercent_data<-data.frame(tail(pp_hg_sorted,nn_pp_coal))

index_lastNpercent_data<-match(lastNpercent_data$UNITID,pp_hg$UNITID)

######extract the SO2 and AC equipment globally 

last20percent_inS02_data<-data.frame(tail(pp_hg_sorted_new[woSO2CTL_MWHsorted_index,],as.integer(length(pp_hg_sorted_new$ISO2[woSO2CTL_MWHsorted_index])*0.2)))
last40percent_inS02_data<-data.frame(tail(pp_hg_sorted_new[woSO2CTL_MWHsorted_index,],as.integer(length(pp_hg_sorted_new$ISO2[woSO2CTL_MWHsorted_index])*0.4)))
index_last20percent_inS02_data<-match(last20percent_inS02_data$UNITID,pp_hg$UNITID)
index_last40percent_inS02_data<-match(last40percent_inS02_data$UNITID,pp_hg$UNITID)

last20percent_inAC_data<-data.frame(tail(pp_hg_sorted_new[woAC_MWHsorted_index,],as.integer(length(pp_hg_sorted_new$ISO2[woAC_MWHsorted_index])*0.2)))
last40percent_inAC_data<-data.frame(tail(pp_hg_sorted_new[woAC_MWHsorted_index,],as.integer(length(pp_hg_sorted_new$ISO2[woAC_MWHsorted_index])*0.4)))
index_last20percent_inAC_data<-match(last20percent_inAC_data$UNITID,pp_hg$UNITID)
index_last40percent_inAC_data<-match(last40percent_inAC_data$UNITID,pp_hg$UNITID)

save(index_last20percent_inS02_data,file = 'index_last20percent_inS02_data.R')
save(index_last40percent_inS02_data,file = 'index_last40percent_inS02_data.R')
save(index_last20percent_inAC_data,file = 'index_last20percent_inAC_data.R')
save(index_last40percent_inAC_data,file = 'index_last40percent_inAC_data.R')

########Extract Indian Power Plants
indian_pp_hg_sorted_new<-data.frame(pp_hg_sorted_new[which(pp_hg_sorted_new$ISO2=="IN"),])
rest_of_world<-data.frame(pp_hg_sorted_new[which(pp_hg_sorted_new$ISO2="IN"),])
woSO2CTL_MWHsorted_india_index<-which(indian_pp_hg_sorted_new$SO2CTL_SIMPLE=="None")
woAC_MWHsorted_india_index<-which(indian_pp_hg_sorted_new$HGCTL_SIMPLE=="None")

index_woSO2CTL_MWHsorted_india_index<-match(indian_pp_hg_sorted_new$UNITID[woSO2CTL_MWHsorted_india_index],pp_hg$UNITID)
index_woAC_MWHsorted_india_index<-match(indian_pp_hg_sorted_new$UNITID[woAC_MWHsorted_india_index],pp_hg$UNITID)

save(index_woSO2CTL_MWHsorted_india_index,file = 'index_woSO2CTL_MWHsorted_india_index.R')
save(index_woAC_MWHsorted_india_index,file="index_woAC_MWHsorted_india_index.R")

###PLOTTING FUNCTION for different percentage of data
######Generate the map of Indian power plants in red. Power plants without AC/SO2 in blue
pdf(file = "./comparison/ppSO2equipment20India.pdf",height = 9,width = 12)
par(mar=c(5,4,4,2),cex.main=2)
plot(region_polygons$countries)
# points(pp_hg_sorted_new$LONG[woSO2CTL_MWHsorted_index], pp_hg_sorted_new$LAT[woSO2CTL_MWHsorted_index], pch=21, bg="red", cex=sqrt(pp_hg_sorted_new$HG_KG)/max(sqrt(pp_hg_sorted_new$HG_KG), na.rm=T)*3)
points(pp_hg$LONG[index_last20percent_india_inS02_data], pp_hg$LAT[index_last20percent_india_inS02_data], pch=21, bg="blue", cex=sqrt(pp_hg$HG_KG)/max(sqrt(pp_hg$HG_KG), na.rm=T)*3)
title("Distribution of Indian power plants being upgraded with Wet Scrubber
      --20% scenario", cex=2)
dev.off()

pdf(file = "./comparison/ppACequipment20-India.pdf",height = 9,width = 12)
par(mar=c(5,4,4,2),cex.main=2)
plot(region_polygons$countries)
# points(pp_hg_sorted_new$LONG[woSO2CTL_MWHsorted_index], pp_hg_sorted_new$LAT[woSO2CTL_MWHsorted_index], pch=21, bg="red", cex=sqrt(pp_hg_sorted_new$HG_KG)/max(sqrt(pp_hg_sorted_new$HG_KG), na.rm=T)*3)
points(pp_hg$LONG[index_last20percent_india_inAC_data], pp_hg$LAT[index_last20percent_india_inAC_data], pch=21, bg="blue", cex=sqrt(pp_hg$HG_KG)/max(sqrt(pp_hg$HG_KG), na.rm=T)*3)
title("Distribution of Indian power plants being upgraded with Activated Carbon
      --20% scenario", cex=2)
dev.off()


###########check if the extractin index was correct those two vectors are identical####
try(if(identical(pp_hg$UNIT[index_last40percent_inS02_data],last40percent_inS02_data$UNIT)!=TRUE) stop("Wrong arrays were extraced as last N percent data"))
save(index_last10percent_inS02_data,file = 'index_last10percent_inS02_data.R')
save(index_last20percent_inS02_data,file = 'index_last20percent_inS02_data.R')
save(index_last30percent_inS02_data,file = 'index_last30percent_inS02_data.R')
save(index_last40percent_inS02_data,file = 'index_last40percent_inS02_data.R')
save(index_last50percent_inS02_data,file = 'index_last50percent_inS02_data.R')
save(index_last60percent_inS02_data,file = 'index_last60percent_inS02_data.R')
save(index_last70percent_inS02_data,file = 'index_last70percent_inS02_data.R')
save(index_last80percent_inS02_data,file = 'index_last80percent_inS02_data.R')

save(pp_hg,file="pp_hg_default.R")
save(pp_hg_sorted_new, file="pp_hg_sorted_MWH.R")
######## Clear workspace ########
# rm(list = ls())

# The following codes was used to see the ranking of different technologies.
# 
# ####check the percentage of chinese power plants in the dirtiest power plants
# 
# ###Trying out factorial regression
# #factorize AC
# pp_hg_sorted$AC<-factor(pp_hg_sorted$HGCTL_SIMPLE)
# is.factor(pp_hg_sorted$AC)
# levels(pp_hg_sorted$AC)[1]<-1   ##Activated carbon injection
# levels(pp_hg_sorted$AC)[2]<-0   ##None
# pp_hg_sorted$AC[1:15]
# pp_hg_sorted$AC[565:8004]
# ##assign with AC  as level 1, without AC level 0.
# #Factorize SO2
# pp_hg_sorted$SO2CTL<-factor(pp_hg_sorted$SO2CTL_SIMPLE)
# is.factor(pp_hg_sorted$SO2CTL)
# # levels(pp_hg_sorted$SO2CTL)
# # levels(pp_hg_sorted$SO2CTL)[1]<-1     ##Dry Scrubber
# # levels(pp_hg_sorted$SO2CTL)[2]<-2     ##Fluidized bed
# # levels(pp_hg_sorted$SO2CTL)[3]<-0     ##None
# # levels(pp_hg_sorted$SO2CTL)[4]<-3     ##Spray-dry adsorption
# # levels(pp_hg_sorted$SO2CTL)[5]<-4     ## Trona
# # levels(pp_hg_sorted$SO2CTL)[6]<-5     ## Wet scrubber
# 
# 
# #Factorize NOX removal
# pp_hg_sorted$NOX<-factor(pp_hg_sorted$NOXCTL_SIMPLE)
# # levels(pp_hg_sorted$NOX)
# # levels(pp_hg_sorted$NOX)[1]<-1   ##Low Nox burner
# # levels(pp_hg_sorted$NOX)[2]<-2   ##Low Nox burner/selective catalytic reduction
# # levels(pp_hg_sorted$NOX)[3]<-0   ##None
# # levels(pp_hg_sorted$NOX)[4]<-3   ##Selective catalytic reduction
# # levels(pp_hg_sorted$NOX)[5]<-4   ##Selective non-catalytic reduction
# 
# #Factorize PM removal
# pp_hg_sorted$PM<-factor(pp_hg_sorted$PARTCTL_SIMPLE)
# # levels(pp_hg_sorted$PM)
# # levels(pp_hg_sorted$PM)[1]<-1   ##Baghouse
# # levels(pp_hg_sorted$PM)[2]<-2   ##Cyclone
# # levels(pp_hg_sorted$PM)[3]<-3   ##Eletrostatic precipitator
# # levels(pp_hg_sorted$PM)[4]<-0   ##None
# # levels(pp_hg_sorted$PM)[5]<-4   ##Wet scrubber
# 
# #Factorize Fueltype simple removal
# pp_hg_sorted$FS<-factor(pp_hg_sorted$FUELTYPE_SIMPLE)
# # levels(pp_hg_sorted$FS)
# # levels(pp_hg_sorted$FS)[1]<-1   ##Anthracite
# # levels(pp_hg_sorted$FS)[2]<-2   ##Bituminous
# # levels(pp_hg_sorted$FS)[3]<-3   ##Lignite
# # levels(pp_hg_sorted$FS)[4]<-4   ##Subbituminous
# # levels(pp_hg_sorted$FS)[5]<-5   ##Waste coal
# 
# #Factorize Boiltype simple 
# pp_hg_sorted$boil<-factor(pp_hg_sorted$BOILTYPE_SIMPLE)
# # levels(pp_hg_sorted$boil)
# # levels(pp_hg_sorted$boil)[1]<-1   ##Fluidized bed boiler
# # levels(pp_hg_sorted$FS)[2]<-2     ##Pulverized coal boiler
# # levels(pp_hg_sorted$FS)[3]<-3     ##Stoker/grate-firing boiler
# 
# #Factorize hg content of coal
# # pp_hg_sorted$HG_CONTENT<-factor(pp_hg_sorted$COAL_AVG_HG_CONTENT_MASS_PERCENT)
# 
# summary(lm(pp_hg_sorted$HG_KG ~ pp_hg_sorted$COAL_AVG_ANNUAL_DEMAND_KG+pp_hg_sorted$ELECTRICAL_EFFICIENCY+pp_hg_sorted$AC+pp_hg_sorted$SO2CTL+pp_hg_sorted$PM+pp_hg_sorted$NOX+pp_hg_sorted$FS+pp_hg_sorted$boil+pp_hg_sorted$COAL_AVG_HG_CONTENT_MASS_PERCENT, data=pp_hg_sorted))
# summary(lm(pp_hg_sorted$HG_KG ~ pp_hg_sorted$COAL_AVG_ANNUAL_DEMAND_KG+pp_hg_sorted$SO2CTL+pp_hg_sorted$PM+pp_hg_sorted$NOX, data=pp_hg_sorted))
# summary(lm(pp_hg_sorted$HG_KG ~ pp_hg_sorted$COAL_AVG_ANNUAL_DEMAND_KG+pp_hg_sorted$FS+pp_hg_sorted$COAL_AVG_HG_CONTENT_MASS_PERCENT))
# 
# par(mar=c(5,5,5,5),cex.axis=0.5, cex.lab=0.5, cex.main=1)
# plot(lm(log(pp_hg_sorted$HG_KG) ~ pp_hg_sorted$COAL_AVG_ANNUAL_DEMAND_KG+pp_hg_sorted$COAL_AVG_HG_CONTENT_MASS_PERCENT))
# summary(lm(logpp_hg_sorted$HG_KG ~ pp_hg_sorted$COAL_AVG_ANNUAL_DEMAND_KG+pp_hg_sorted$COAL_AVG_HG_CONTENT_MASS_PERCENT+pp_hg_sorted$COAL_AVG_ANNUAL_DEMAND_KG:pp_hg_sorted$COAL_AVG_HG_CONTENT_MASS_PERCENT))
# 
# fit1<-lm(pp_hg_sorted$HG_KG ~ pp_hg_sorted$COAL_AVG_ANNUAL_DEMAND_KG+pp_hg_sorted$COAL_AVG_HG_CONTENT_MASS_PERCENT+pp_hg_sorted$COAL_AVG_ANNUAL_DEMAND_KG:pp_hg_sorted$COAL_AVG_HG_CONTENT_MASS_PERCENT)
# drop1(fit1,test="F")
# 
# pp_hg_sorted$ACSO2<-interaction(pp_hg_sorted$AC,pp_hg_sorted$SO2CTL)
# pp_hg_sorted$PMACSO2<-interaction(pp_hg_sorted$ACSO2,pp_hg_sorted$PM)
# pp_hg_sorted$PMACSO2NOX<-interaction(pp_hg_sorted$PMACSO2,pp_hg_sorted$NOX)
# 
# match(c("Activated carbon injection","Trona","Baghouse","Low Nox burner"),pp_hg_sorted$HGCTL_SIMPLE&pp_hg_sorted$SO2CTL_SIMPLE&pp_hg_sorted$PARTCTL_SIMPLE&pp_hg_sorted$NOXCTL_SIMPLE)
# 
# index_LETC<-which((pp_hg_sorted$HGCTL_SIMPLE=="Activated carbon injection"&pp_hg_sorted$SO2CTL_SIMPLE=="Trona"&pp_hg_sorted$PARTCTL_SIMPLE=="Baghouse"&pp_hg_sorted$NOXCTL_SIMPLE=="Low Nox burner")==TRUE)
# pp_hg_sorted$HG_KG[index_LETC]
# 
# 
# 
# 
# pp_hg_sorted$PMACSO2NOX<-interaction(pp_hg_sorted$PMACSO2,pp_hg_sorted$NOX)
# boxplot(pp_hg_sorted$HG_KG/(pp_hg_sorted$COAL_AVG_ANNUAL_DEMAND_KG*pp_hg_sorted$COAL_AVG_HG_CONTENT_MASS_PERCENT)~pp_hg_sorted$PMACSO2NOX)
# A<-boxplot(pp_hg_sorted$HG_KG/(pp_hg_sorted$COAL_AVG_ANNUAL_DEMAND_KG*pp_hg_sorted$COAL_AVG_HG_CONTENT_MASS_PERCENT)~pp_hg_sorted$PMACSO2NOX)
# View(A)
# A[["stats"]]
