library(mongolite) # Added for mongo connectivity
library(lubridate)
library(plyr)
library(ggplot2)
library(ggmap) # Added for maps

accident <- mongo(collection = "ACCIDENT",db = "ACCIDENTDB",url = "mongodb://localhost",verbose = FALSE)
road_surface_cond <- mongo(collection = "ROAD_SURFACE_COND",db = "ACCIDENTDB",url = "mongodb://localhost",verbose = FALSE)
atmospheric_cond <- mongo(collection = "ATMOSPHERIC_COND",db = "ACCIDENTDB",url = "mongodb://localhost",verbose = FALSE)
accident_node <- mongo(collection = "ACCIDENT_NODE",db = "ACCIDENTDB",url = "mongodb://localhost",verbose = FALSE)

setwd("/Users/blackbox/Documents/Misc/Datasets/ACCIDENT/")

vehicle_df <- read.csv("VEHICLE.csv")
accident_dataframe <- accident$find()
accident_node_df <- accident_node$find()
atmospheric_cond_df <- atmospheric_cond$find()
road_surface_cond_df <- road_surface_cond$find()

accident_dataframe_new <- accident_dataframe
write.csv(accident_dataframe_new, file = "AccidentFreqAll.csv")

# Adding a year column to the Accident data frame
accident_dataframe$ACCIDENTYEAR <- year(as.Date(accident_dataframe$ACCIDENTDATE, format = '%d/%m/%Y'))

# Accident taken between 2007 and 2016
accident_dataframe <- subset(accident_dataframe,accident_dataframe$ACCIDENTYEAR != 2006 & accident_dataframe$ACCIDENTYEAR != 2017)

accidentAnalysis <- function()
{
  AccidentTrend <- count(accident_dataframe$ACCIDENTYEAR)
  
  Years <- c(AccidentTrend$x)
  
  ggplot(AccidentTrend, aes(x = AccidentTrend$x, y = AccidentTrend$freq)) +
    geom_line( colour = "red") +
    ggtitle("Accident Trend in Victoria") + 
    xlab(" Year ") +
    ylab("Accident Count") +
    theme(plot.title = element_text(face = "bold", hjust = 0.5),
          axis.title = element_text(face = "bold", color = "black"),
          axis.text.x = element_text(face = "bold", size = 9),
          axis.text.y = element_text(face = "bold", size = 8))+
    scale_x_discrete(limits=Years)
  
  write.csv(AccidentTrend, file = "AccidentFreq.csv")
}

AccidentSeverityAnalysis <- function()
{
  AccidentSeverityFreq <- count(accident_dataframe$SEVERITY)
  
  names <- c("Fatal accident", 
             "Serious injury accident", 
             "Other injury accident",
             "Non injury accident")
  
  ggplot(AccidentSeverityFreq, aes(x,freq, label = freq))+
    geom_bar(stat = "identity", fill = "steelblue", width = 0.5) +
    geom_text(vjust = -0.5, color = "black") +
    labs(title = "Accident Severity", x = "Severity Level", y = "Accident Count") +
    theme(title = element_text(face = "bold", color = "black"), 
          axis.title = element_text(face = "bold", color = "black"),
          axis.text.x = element_text(face = "bold",size = 8),
          axis.text.y = element_text(face = "bold", size = 8),
          plot.title = element_text(hjust = 0.5)) +
    scale_x_discrete(limits=names,labels=names)
  
  write.csv(AccidentSeverityFreq, file = "AccidentSeverityFreq.csv")
}

AccidentVariableAnalysis <- function()
{
  merged <- merge(accident_dataframe, accident_node_df, by=c("ACCIDENT_NO"))
  
  # Removing the invalid values
  merged <- subset(merged, merged$SPEED_ZONE != 777 | merged$SPEED_ZONE != 888 | merged$SPEED_ZONE != 999)
  
  #Merging atmospheric condition with Accident and Node datasets
  merged <- merge(merged, atmospheric_cond_df, by=c("ACCIDENT_NO"))
  
  #Merging All the tables
  merged <- merge(merged,road_surface_cond_df,by=c("ACCIDENT_NO"))
  
  install.packages("randomForest")
  library(party)
  library(randomForest)
  
  AccidentImportanceAnalysis <- merged[sample(nrow(merged),10000), ]
  
  AccidentImportanceAnalysis$SEVERITY <- as.factor(AccidentImportanceAnalysis$SEVERITY)
  AccidentImportanceAnalysis$SPEED_ZONE <- as.factor(AccidentImportanceAnalysis$SPEED_ZONE)
  AccidentImportanceAnalysis$ACCIDENT_TYPE <- as.factor(AccidentImportanceAnalysis$ACCIDENT_TYPE)
  AccidentImportanceAnalysis$LIGHT_CONDITION <- as.factor(AccidentImportanceAnalysis$LIGHT_CONDITION)
  AccidentImportanceAnalysis$ROAD_GEOMETRY <- as.factor(AccidentImportanceAnalysis$ROAD_GEOMETRY)
  AccidentImportanceAnalysis$ATMOSPH_COND <- as.factor(AccidentImportanceAnalysis$ATMOSPH_COND)
  
  output.forest <- randomForest(SEVERITY ~ LIGHT_CONDITION +
                                  ATMOSPH_COND +
                                  ROAD_GEOMETRY+
                                  SPEED_ZONE + 
                                  ACCIDENT_TYPE, 
                                data = AccidentImportanceAnalysis)
  
  print(output.forest)
  
  print(importance(output.forest,type = 2))
}

VehicleTypeAnalysis <- function()
{
  vehicleFreq <- count(vehicle_df$Vehicle.Type.Desc)
  
  ggplot(vehicleFreq, aes(x,freq, label = freq)) +
    geom_bar(stat = "identity", fill = "steelblue", width = 0.5) +
    geom_text(vjust = -0.5, color = "black") +
    labs(title = "Light Condition Frequency", x = "Light Condition types", y = "Frequency") +
    theme(title = element_text(face = "bold", color = "black"), 
          axis.title = element_text(face = "bold", color = "black"),
          axis.text.x = element_text(face = "bold",angle = 60, hjust=1, size = 8),
          axis.text.y = element_text(face = "bold", size = 8),
          plot.title = element_text(hjust = 0.5)) +
    scale_x_discrete(limits=vehicleFreq$x,labels=vehicleFreq$x)
}

LightCconditionAnalysis <- function()
{
  LighConditionFreq <- count(accident_dataframe$LIGHT_CONDITION)
  
  names <- c("Day", 
             "Dusk/Dawn", 
             "Dark street lights on", 
             "Dark Street lights off",
             "Dark no street lights",
             "Dark no street unknown")
  
  ggplot(LighConditionFreq, aes(x,freq, label = freq)) +
    geom_bar(stat = "identity", fill = "steelblue", width = 0.5) +
    geom_text(vjust = -0.5, color = "black") +
    coord_cartesian(xlim=c(1,6)) +
    labs(title = "Light Condition Frequency", x = "Light Condition types", y = "Frequency") +
    theme(title = element_text(face = "bold", color = "black"), 
          axis.title = element_text(face = "bold", color = "black"),
          axis.text.x = element_text(face = "bold",angle = 60, hjust=1, size = 8),
          axis.text.y = element_text(face = "bold", size = 8),
          plot.title = element_text(hjust = 0.5)) +
    scale_x_discrete(limits=names,labels=names)
  
  write.csv(LighConditionFreq, file = "LighConditionFreq.csv")
  
  # Considering all accident that happened when there it was dark and No Street light
  DarkNoStreetLight <- subset(accident_dataframe,accident_dataframe$LIGHT_CONDITION == 5)
  Merged_NoLight_Node <- merge(DarkNoStreetLight, accident_node_df, by=c("ACCIDENT_NO"))
  Merged_NoLight_Node <- unique( Merged_NoLight_Node[ , ] )
  Merged_NoLight_Node_vehicle <- merge(Merged_NoLight_Node, vehicle_df, by=c("ACCIDENT_NO"))
  Merged_NoLight_Node_vehicle <- unique( Merged_NoLight_Node_vehicle[ , ] )
  write.csv(Merged_NoLight_Node_vehicle, file = "NoLightAccidentAllVehicle.csv")
  
  postcodeNoLight <- count(Merged_NoLight_Node_vehicle$POSTCODE)
  write.csv(postcodeNoLight, file = "PostcodewiseNoLight.csv")
  
  # Top 5 suburbs with Accident count during no street light
  TopAccidents <- postcodeNoLight[order(postcodeNoLight$freq,decreasing=T)[1:5],]
}


BicycleAccidentAnalysis <- function()
{
  BicycleVehicleData <- subset (vehicle_df, vehicle_df$VEHICLE_TYPE == 13)
  write.csv(BicycleVehicleData, file = "BicycleAccident.csv")
  
  FatalAccident <- subset(accident_dataframe, accident_dataframe$SEVERITY == 1 | accident_dataframe$SEVERITY == 2)
  Merged <- merge(FatalAccident, BicycleVehicleData, by=c("ACCIDENT_NO"))
  Merged <- merge(accident_node_df, Merged, by=c("ACCIDENT_NO"))
  #Merged has all Fatal and serious accidents for Bicycle users
  Merged <- unique( Merged[ , ] )
  write.csv(Merged, file = "BicyleSeriousAccidents.csv")
  
  BicycleAccidentType <- count(Merged$ACCIDENT_TYPE)
  write.csv(BicycleAccidentType, file = "BicycleAccidentType.csv")
  
  # We consider only collision with vehicle
  Merged_VehilceCollision <- subset(Merged, Merged$ACCIDENT_TYPE==1)
  PostcodeWiseBicycleAccident <- count(Merged_VehilceCollision$POSTCODE)
  write.csv(PostcodeWiseBicycleAccident, file = "PostcodeWiseBicycleAccident.csv")
  
  #Postcodes 3000, 3121, 3053, 3182, 3065
  TopAccidents <- PostcodeWiseBicycleAccident[order(PostcodeWiseBicycleAccident$freq,decreasing=T)[1:10],]
  
  print(TopAccidents)
  
  #Finding all bicycle accidents that killed
  Merged_bicycle_Killed <- subset(Merged, Merged$NO_PERSONS_KILLED > 0)
  write.csv(Merged_bicycle_Killed, file = "BicycleAccidents_Killed.csv")
  
  #****************************Locating Bicycle Accidents for 3000**************************
  
  Merged_3000 <- subset(Merged_VehilceCollision, Merged_VehilceCollision$POSTCODE == 3000)
  
  write.csv(Merged_3000, file = "3000_BicycleAccidents.csv")
  
  maps <- get_map(location = c(lon = mean(Merged_3000$LONGITUDE), lat = mean(Merged_3000$LATITUDE)),
                  zoom = 15,
                  maptype = "roadmap", 
                  scale = 2)
  
  ggmap(maps) +
    geom_point(data = Merged_3000, aes(x = Merged_3000$LONGITUDE, y = Merged_3000$LATITUDE, 
                                       fill = "Black", alpha = 1), 
               size = 3,
               shape = 20) +
    guides(fill=FALSE, alpha=FALSE, size=FALSE) +
    labs(title = "Postcode 3000 Bicycle Accident locations", x = "Longitude", y = "Latitude") +
    theme(title = element_text(face = "bold", color = "black"),
          axis.title = element_text(face = "bold", color = "black"),
          axis.text.x = element_text(face = "bold", size = 8),
          axis.text.y = element_text(face = "bold", size = 8),
          plot.title = element_text(hjust = 0.5))
  
  #********************************************************************************
  
  
  
  #****************************Locating Bicycle Accidents for 3121**************************
  
  Merged_3121 <- subset(Merged_VehilceCollision, Merged_VehilceCollision$POSTCODE == 3121)
  
  write.csv(Merged_3121, file = "3121_BicycleAccidents.csv")
  
  maps <- get_map(location = c(lon = mean(Merged_3121$LONGITUDE), lat = mean(Merged_3121$LATITUDE)),
                  zoom = 14,
                  maptype = "roadmap", 
                  scale = 2)
  
  ggmap(maps) +
    geom_point(data = Merged_3121, aes(x = Merged_3121$LONGITUDE, y = Merged_3121$LATITUDE, fill = "red", alpha = 0.8), 
               size = 2, 
               shape = 20) +
    guides(fill=FALSE, alpha=FALSE, size=FALSE) +
    labs(title = "Postcode 3121 Bicycle Accident locations", x = "Longitude", y = "Latitude") +
    theme(title = element_text(face = "bold", color = "black"),
          axis.title = element_text(face = "bold", color = "black"),
          axis.text.x = element_text(face = "bold", size = 8),
          axis.text.y = element_text(face = "bold", size = 8),
          plot.title = element_text(hjust = 0.5))
  
  #********************************************************************************
  
  #****************************Locating Bicycle Accidents for 3053**************************
  
  Merged_3053 <- subset(Merged_VehilceCollision, Merged_VehilceCollision$POSTCODE == 3053)
  
  maps <- get_map(location = c(lon = mean(Merged_3053$LONGITUDE), lat = mean(Merged_3053$LATITUDE)),
                  zoom = 15,
                  maptype = "roadmap", 
                  scale = 2)
  
  ggmap(maps) +
    geom_point(data = Merged_3053, aes(x = Merged_3053$LONGITUDE, y = Merged_3053$LATITUDE, 
                                       fill = "Black", alpha = 0.8), 
               size = 2, 
               shape = 20) +
    guides(fill=FALSE, alpha=FALSE, size=FALSE) +
    labs(title = "Postcode 3053 Bicycle Accident locations", x = "Longitude", y = "Latitude") +
    theme(title = element_text(face = "bold", color = "black"),
          axis.title = element_text(face = "bold", color = "black"),
          axis.text.x = element_text(face = "bold", size = 8),
          axis.text.y = element_text(face = "bold", size = 8),
          plot.title = element_text(hjust = 0.5))
  
  #********************************************************************************
  
  #****************************Locating Bicycle Accidents for 3182**************************
  
  Merged_3182 <- subset(Merged_VehilceCollision, Merged_VehilceCollision$POSTCODE == 3182)
  
  maps <- get_map(location = c(lon = mean(Merged_3182$LONGITUDE), lat = mean(Merged_3182$LATITUDE)),
                  zoom = 14,
                  maptype = "roadmap", 
                  scale = 2)
  
  ggmap(maps) +
    geom_point(data = Merged_3182, aes(x = Merged_3182$LONGITUDE, y = Merged_3182$LATITUDE, 
                                       fill = "red", alpha = 0.8), 
               size = 2, 
               shape = 21) +
    guides(fill=FALSE, alpha=FALSE, size=FALSE) +
    labs(title = "Postcode 3182 Bicycle Accident locations", x = "Longitude", y = "Latitude") +
    theme(title = element_text(face = "bold", color = "black"),
          axis.title = element_text(face = "bold", color = "black"),
          axis.text.x = element_text(face = "bold", size = 8),
          axis.text.y = element_text(face = "bold", size = 8),
          plot.title = element_text(hjust = 0.5))
  
  #********************************************************************************
  
  #****************************Locating Bicycle Accidents for 3065**************************
  
  Merged_3065 <- subset(Merged_VehilceCollision, Merged_VehilceCollision$POSTCODE == 3065)
  
  maps <- get_map(location = c(lon = mean(Merged_3065$LONGITUDE), lat = mean(Merged_3065$LATITUDE)),
                  zoom = 14,
                  maptype = "roadmap", 
                  scale = 2)
  
  ggmap(maps) +
    geom_point(data = Merged_3065, aes(x = Merged_3065$LONGITUDE, y = Merged_3065$LATITUDE, 
                                       fill = "red", alpha = 0.8), 
               size = 2, 
               shape = 21) +
    guides(fill=FALSE, alpha=FALSE, size=FALSE) +
    labs(title = "Postcode 3065 Bicycle Accident locations", x = "Longitude", y = "Latitude") +
    theme(title = element_text(face = "bold", color = "black"),
          axis.title = element_text(face = "bold", color = "black"),
          axis.text.x = element_text(face = "bold", size = 8),
          axis.text.y = element_text(face = "bold", size = 8),
          plot.title = element_text(hjust = 0.5))
  
  #********************************************************************************
  
  #****************************Locating Bicycle Accidents for 3186**************************
  
  Merged_3186 <- subset(Merged_VehilceCollision, Merged_VehilceCollision$POSTCODE == 3186)
  
  write.csv(Merged_3186, file = "3186_Accidents.csv")
  
  maps <- get_map(location = c(lon = mean(Merged_3186$LONGITUDE), lat = mean(Merged_3186$LATITUDE)),
                  zoom = 14,
                  maptype = "roadmap", 
                  scale = 2)
  
  ggmap(maps) +
    geom_point(data = Merged_3186, aes(x = Merged_3186$LONGITUDE, y = Merged_3186$LATITUDE, 
                                       fill = "Black", alpha = 1), 
               size = 3,
               shape = 20) +
    guides(fill=FALSE, alpha=FALSE, size=FALSE) +
    labs(title = "Postcode 3186 Bicycle Accident locations", x = "Longitude", y = "Latitude") +
    theme(title = element_text(face = "bold", color = "black"),
          axis.title = element_text(face = "bold", color = "black"),
          axis.text.x = element_text(face = "bold", size = 8),
          axis.text.y = element_text(face = "bold", size = 8),
          plot.title = element_text(hjust = 0.5))
  
  #********************************************************************************
  
  #****************************Locating Bicycle Accidents for 3195**************************
  
  Merged_3195 <- subset(Merged_VehilceCollision, Merged_VehilceCollision$POSTCODE == 3195)
  
  write.csv(Merged_3195, file = "3121_BicycleAccidents.csv")
  
  maps <- get_map(location = c(lon = mean(Merged_3195$LONGITUDE), lat = mean(Merged_3195$LATITUDE)),
                  zoom = 14,
                  maptype = "roadmap", 
                  scale = 2)
  
  ggmap(maps) +
    geom_point(data = Merged_3195, aes(x = Merged_3195$LONGITUDE, y = Merged_3195$LATITUDE, 
                                       fill = "Black", alpha = 1), 
               size = 3,
               shape = 20) +
    guides(fill=FALSE, alpha=FALSE, size=FALSE) +
    labs(title = "Postcode 3195 Bicycle Accident locations", x = "Longitude", y = "Latitude") +
    theme(title = element_text(face = "bold", color = "black"),
          axis.title = element_text(face = "bold", color = "black"),
          axis.text.x = element_text(face = "bold", size = 8),
          axis.text.y = element_text(face = "bold", size = 8),
          plot.title = element_text(hjust = 0.5))
  
  #********************************************************************************
}

accidentAnalysis()
AccidentSeverityAnalysis()
AccidentVariableAnalysis()
BicycleAccidentAnalysis()
LightCconditionAnalysis()
VehicleTypeAnalysis()
