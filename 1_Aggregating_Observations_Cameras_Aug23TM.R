###############################################################################
#                                                                             #
#=============================================================================#
#             AGGREGATE DIFFERENT PARTS OF THE DATABASE                       #
#=============================================================================#
#                                                                             #
#                                                                             #
#   WRITTEN BY DUCHENNE Francois, SUMMER 2022, WSL, BIRMENSDORF, SWITZERLAND  #
#   francois.duchenne@wsl.ch                                                  #
#                                                                             #
#   most of this is a straight copy from Rafi's script on github              #
#                                                                             #
#                                                                             #
###############################################################################


## Data taken from dropbox folder (EPHI_data_clean/Ecuador_2023-08-24)
## Set working directory and load packages
setwd(dir="C:/Users/guevara/Dropbox/PC/Documents/TraitMatching")
library(data.table)
library(dplyr)

## ASSOCIATE INTERACTIONS AND CAMERA DATA

# Load interactions and camera data 
data=fread("Interactions_data_Ecuador.txt",na.strings = c("",NA))
cameras=fread("Cameras_data_Ecuador.txt",na.strings = c("",NA))

dim(data)
data=merge(data,cameras,by="waypoint",all.x=T,all.y=F) #if you want to exclude camera which did not detect any hummingbird
dim(data)

#dat=merge(data,cameras,by="waypoint",all=T) #if you want to keep camera which did not detect any hummingbird
#dim(data)

data_clean <-data[!(data$plant_species=="NA" | data$hummingbird_species==""),] # remove observations without species ID

# Remove observations corresponding to sampling done after 2020
data_clean$year=year(data_clean$date) #
data_clean1 <-subset(data_clean, year!="2021")
data_clean2 <-subset(data_clean1, year!="2022")
data_clean3 <-subset(data_clean2, year!="2023")

fwrite(data_clean3, "InteractionsAg.txt") # write plant-hummingbird interactions file
