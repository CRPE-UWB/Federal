# Assigning census block data to schools
# Center on Reinventing Public Education
# Alton Lu
# June 19 2017

# rm(list=ls()) 

# You'll need to install all the packages
# this code contains two parts
# First is the function for drawing census boundary lines in a datafile based on lat/long
# the second is a function for reading datafiles and combinging them

options(scipen = 999)
library(ggmap)
library(XML)
library(RCurl)
library(plyr)
library(sp)
library(rgdal)
library(rgeos)
library(maps)
library(maptools)
library(foreign)
library(reshape)
library(dplyr)
library(stringr)

# creating the function ---------------------------------------------------------------------

setwd("/Users/Alton/Google Drive/CRPE/Database/Shape/block_state/") # if on Mac 

directory <- "/Users/Alton/Google Drive/CRPE/Database/Shape/block_state/"

master <- read.csv("/Users/Alton/Google Drive/CRPE/Database/Other/Master_School_Cities.csv")
master <- readRDS("/Users/Alton/Google Drive/CRPE/Database/Other/Master_School_Cities.Rdata")

names(master)[13] <- "buildinglon"
names(master)[12] <- "buildinglat"


# function to read in data with lat/longs and produce a new dataframe with census numbers
blockAssign <- function(state){
  
  dataframe <- master
  
  
  # Setting the directory to point to correct state
  setwd(paste(directory, state, sep = ""))
  filename <- as.character(strsplit(list.files()[3], ".dbf"))
  state_code <- as.integer(str_sub(filename, 9, 10))
  data <- filter(dataframe, fipst == state_code)
  setwd(directory)
  
  # read the block data in as OGR
  blocks <- readOGR(state, filename)
  
  # Rename some of the data 
  data$buildinglat[is.na(data$buildinglat)] <- 0
  data$buildinglon[is.na(data$buildinglon)] <- 0
  
  coordinates(data) <- c("buildinglon", "buildinglat")
  proj4string(data) <- proj4string(blocks)
  
  ## Combine is.na() with over() to do a containment test (note that this also "demotes" blocks to a SpatialPolygons object)
  inside.place <- !is.na(over(data, as(blocks, "SpatialPolygons")))
  
  ## Use "over" again, this time with blocks as a SpatialPolygonsDataFrame object, 
  # to determine which blocks (if any) contains each building, and store the geographic names as an attribute of the student data (this must be done for blocks, tracts, and counties - since block IDs are repeated across tracts, and tract IDs are repeated across counties). Be sure to check that the names of the geographic units are what I have listed below (you can use 'names(blocks)').
  data$block <- rep(NA, nrow(data))
  data$block <- over(data, blocks)$BLOCKCE10
  data$tract <- rep(NA, nrow(data))
  data$tract <- over(data, blocks)$TRACTCE10
  data$county <- rep(NA, nrow(data))
  data$county <- over(data, blocks)$COUNTYFP10
  
  ## Convert back to a dataframe (note that I like to call it something new, so that I don't overwrite the initial dataframe; also, the last 3 lines ensure that the block, tract, and county IDs are in numeric format)
  data1 <- as.data.frame(data)
  data1$block <- as.numeric(as.character(data1$block))
  data1$tract <- as.numeric(as.character(data1$tract))
  data1$county <- as.numeric(as.character(data1$county))
  
  data1$state <- state
  data1 <- filter(data1, block != "NA")
  ### - - - - -
  saveRDS(data1, 
          paste("/Users/Alton/Google Drive/CRPE/Database/Shape/blk_", state, ".Rdata", sep = ""))
  # write.csv(data1, 
  #        paste("/Users/Alton/Google Drive/CRPE/Database/Shape/blk_", state, ".csv", sep = ""))
  ### - - - - -
  return(data1)
}

# Drawing census blocks ---------------------------------------------------
AL <- as.data.frame(blockAssign("Alabama"))
AK <- as.data.frame(blockAssign("Alaska"))
AZ <- as.data.frame(blockAssign("Arizona"))
AR <- as.data.frame(blockAssign("Arkansas"))
CA <- as.data.frame(blockAssign("California")) 
CO <- as.data.frame(blockAssign("Colorado"))
CT <- as.data.frame(blockAssign("Connecticut"))
DE <- as.data.frame(blockAssign("Delaware"))
DC <- as.data.frame(blockAssign("District of Columbia")) 
FL <- as.data.frame(blockAssign("Florida")) 
GA <- as.data.frame(blockAssign("Georgia")) 
HI <- as.data.frame(blockAssign("Hawaii")) 
ID <- as.data.frame(blockAssign("Idaho")) 
IL <- as.data.frame(blockAssign("Illinois"))
IN <- as.data.frame(blockAssign("Indiana")) 
IA <- as.data.frame(blockAssign("Iowa"))
KS <- as.data.frame(blockAssign("Kansas"))
KY <- as.data.frame(blockAssign("Kentucky")) 
LA <- as.data.frame(blockAssign("Louisiana")) 
ME <- as.data.frame(blockAssign("Maine")) 
MD <- as.data.frame(blockAssign("Maryland")) 
MA <- as.data.frame(blockAssign("Massachusetts")) 
MI <- as.data.frame(blockAssign("Michigan")) 
MN <- as.data.frame(blockAssign("Minnesota")) 
MS <- as.data.frame(blockAssign("Mississippi")) 
MO <- as.data.frame(blockAssign("Missouri")) 
MT <- as.data.frame(blockAssign("Montana")) 
NE <- as.data.frame(blockAssign("Nebraska"))
NV <- as.data.frame(blockAssign("Nevada"))  
NH <- as.data.frame(blockAssign("New Hampshire")) 
NJ <- as.data.frame(blockAssign("New Jersey")) 
NM <- as.data.frame(blockAssign("New Mexico")) 
NY <- as.data.frame(blockAssign("New York")) 
NC <- as.data.frame(blockAssign("North Carolina"))
ND <- as.data.frame(blockAssign("North Dakota")) 
OH <- as.data.frame(blockAssign("Ohio"))
OK <- as.data.frame(blockAssign("Oklahoma"))
OR <- as.data.frame(blockAssign("Oregon")) 
PA <- as.data.frame(blockAssign("Pennsylvania")) 
RI <- as.data.frame(blockAssign("Rhode Island")) 
SC <- as.data.frame(blockAssign("South Carolina")) 
SD <- as.data.frame(blockAssign("South Dakota")) 
TN <- as.data.frame(blockAssign("Tennessee"))
UT <- as.data.frame(blockAssign("Utah")) 
VT <- as.data.frame(blockAssign("Vermont")) 
VA <- as.data.frame(blockAssign("Virginia")) 
WA <- as.data.frame(blockAssign("Washington")) 
WV <- as.data.frame(blockAssign("West Virginia")) 
WI <- as.data.frame(blockAssign("Wisconsin")) 
WY <- as.data.frame(blockAssign("Wyoming"))

# Combine all into a dataset ---------------------------------
data <- rbind(AL, AK, AZ, AR, CA, CO, CT, DE, DC, FL, GA, HI, ID, 
              IL, IN, IA, KS, KY, LA, ME, MD, MA, MI, MN, MS, 
              MO, MT, NE, NV, NH, NJ, NM, NY, NC, ND, OH, OK, OR, 
              PA, RI, SC, SD, TN, TX, UT, VT, VA, WA, WV, WI, WY)

write.csv(data, "/Users/Alton/Google Drive/CRPE/Database/Other/Master_school_list.csv", row.names = FALSE)


# Reading data in if you don't have it in your working space. Don't need to run. -------------------------------------------------------
readBlocks <- function(state){
  data <- read.csv(paste("/Users/Alton/Google Drive/CRPE/Database/Shape/blk_", state, ".csv", sep = ""))
  return(data)
}

AL <- as.data.frame(readBlocks("Alabama"))
AK <- as.data.frame(readBlocks("Alaska"))
AZ <- as.data.frame(readBlocks("Arizona"))
AR <- as.data.frame(readBlocks("Arkansas"))
CA <- as.data.frame(readBlocks("California")) 
CO <- as.data.frame(readBlocks("Colorado"))
CT <- as.data.frame(readBlocks("Connecticut"))
DE <- as.data.frame(readBlocks("Delaware"))
DC <- as.data.frame(readBlocks("District of Columbia")) 
FL <- as.data.frame(readBlocks("Florida")) 
GA <- as.data.frame(readBlocks("Georgia")) 
HI <- as.data.frame(readBlocks("Hawaii")) 
ID <- as.data.frame(readBlocks("Idaho")) 
IL <- as.data.frame(readBlocks("Illinois"))
IN <- as.data.frame(readBlocks("Indiana")) 
IA <- as.data.frame(readBlocks("Iowa"))
KS <- as.data.frame(readBlocks("Kansas"))
KY <- as.data.frame(readBlocks("Kentucky")) 
LA <- as.data.frame(readBlocks("Louisiana")) 
ME <- as.data.frame(readBlocks("Maine")) 
MD <- as.data.frame(readBlocks("Maryland")) 
MA <- as.data.frame(readBlocks("Massachusetts")) 
MI <- as.data.frame(readBlocks("Michigan")) 
MN <- as.data.frame(readBlocks("Minnesota")) 
MS <- as.data.frame(readBlocks("Mississippi")) 
MO <- as.data.frame(readBlocks("Missouri")) 
MT <- as.data.frame(readBlocks("Montana")) 
NE <- as.data.frame(readBlocks("Nebraska"))
NV <- as.data.frame(readBlocks("Nevada"))  
NH <- as.data.frame(readBlocks("New Hampshire")) 
NJ <- as.data.frame(readBlocks("New Jersey")) 
NM <- as.data.frame(readBlocks("New Mexico")) 
NY <- as.data.frame(readBlocks("New York")) 
NC <- as.data.frame(readBlocks("North Carolina"))
ND <- as.data.frame(readBlocks("North Dakota")) 
OH <- as.data.frame(readBlocks("Ohio"))
OK <- as.data.frame(readBlocks("Oklahoma"))
OR <- as.data.frame(readBlocks("Oregon")) 
PA <- as.data.frame(readBlocks("Pennsylvania")) 
RI <- as.data.frame(readBlocks("Rhode Island")) 
SC <- as.data.frame(readBlocks("South Carolina")) 
SD <- as.data.frame(readBlocks("South Dakota")) 
TN <- as.data.frame(readBlocks("Tennessee"))
TX <- as.data.frame(readBlocks("Texas")) 
UT <- as.data.frame(readBlocks("Utah")) 
VT <- as.data.frame(readBlocks("Vermont")) 
VA <- as.data.frame(readBlocks("Virginia")) 
WA <- as.data.frame(readBlocks("Washington")) 
WV <- as.data.frame(readBlocks("West Virginia")) 
WI <- as.data.frame(readBlocks("Wisconsin")) 
WY <- as.data.frame(readBlocks("Wyoming"))


TX <- select(TX, -X)
TX$ncessch <- as.character(TX$ncessch)
TX$fipst <- as.character(TX$fipst)
TX$leaid <- as.character(TX$leaid)
TX$schno <- as.character(TX$schno)
TX$stid <- as.character(TX$stid)
TX$seasch <- as.character(TX$seasch)


data <- rbind(AL, AK, AZ, AR, CA, CO, CT, DE, DC, FL, GA, HI, ID, 
              IL, IN, IA, KS, KY, LA, ME, MD, MA, MI, MN, MS, 
              MO, MT, NE, NV, NH, NJ, NM, NY, NC, ND, OH, OK, OR, 
              PA, RI, SC, SD, TN, TX, UT, VT, VA, WA, WV, WI, WY)

glimpse(data)

data$stid <- as.character(data$stid)
data$seasch <- as.character(data$seasch)
data$leanm <- as.character(data$leanm)
data$schnam <- as.character(data$schnam)
data$chartr <- as.character(data$chartr)
data$level <- as.character(data$level)
data$magnet <- as.character(data$magnet)

data[data == "N"] <- "-99"
data[data == "M"] <- "-99"

write.csv(data, "/Users/Alton/Google Drive/CRPE/Database/Other/Master_school_list.csv", row.names = FALSE)
saveRDS(data, "/Users/Alton/Google Drive/CRPE/Database/Other/Master_school_list.Rdata")


# Altering the master list --------------------------------------------------------------------------
data <- readRDS("/Users/Alton/Google Drive/CRPE/Database/Other/Master_school_list.Rdata")
glimpse(data)

data <- data %>% filter(notschool == 0) %>%
  select(-year, -state, -notschool)

data[is.na(data)] <- "-99"

# The only values that should appear are numbers and -99.
# If "NA", not applicable, YES, or no, appears, you'll have to remove them.
# -99 is NA. Yes = 1, No = 2
unique(data$chartr)
data$chartr[data$chartr == "No"] <- "2"
data$chartr[data$chartr == "Yes"] <- "1"
data$chartr[data$chartr == "NA"] <- "-99"
data$chartr[data$chartr == "Not Applicable"] <- "-99"

unique(data$status)
unique(data$chartr)
unique(data$ulocal)
unique(data$type)


unique(data$ncessch)
data <- subset(data, !duplicated(c(ncessch)))


data$fipst <- as.character(data$fipst)
data$leaid <- as.character(data$leaid)
data$schno <- as.character(data$schno)

data$ncessch <- as.character(data$ncessch)
data$ncessch <- paste(data$leaid, data$schno, sep = "")

data$urban <- as.character(data$urban)

data$urban[data$urban == "not urban"] <- "0"
data$urban[data$urban == "urban"] <- "1"

glimpse(data)
data$schnam <- gsub(",", "", data$schnam)


write.csv(data, "/Users/Alton/Google Drive/CRPE/Database/Other/School_List_Subset.csv", row.names = FALSE)
saveRDS(data, "/Users/Alton/Google Drive/CRPE/Database/Other/School_List_Subset.Rdata")






