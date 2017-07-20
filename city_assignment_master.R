# June 18 2017
# Alton Lu
# Center on Reinventing Public Education

rm(list=ls()) 

library(sp)
library(rgdal)
library(maps)
library(maptools)
library(foreign)
library(dplyr)
data <- readRDS("/Users/Alton/OneDrive/Work/CRPE/City Wide/ccd_2010_15.Rdata")
directory <- "/Users/Alton/Google Drive/CRPE/Database/Shape/place_state/"

cityMerge <- function(state){

  ccd <- data
  
  setwd(paste(directory, state, sep = "")) # if on Mac 
  
  # path = "/Users/Alton/OneDrive/Work/CRPE/City Wide/shapefiles"
  filenames <- list.files(pattern = ".dbf", recursive = TRUE)
  filenames <- unlist(strsplit(filenames, split='/', fixed=TRUE))
  filenames
  
  ## STEP 2: Load geographic data   
  #read in Census Places polygons
  shape <- ""
  NJ <- readOGR(filenames)
  
  #combine all states' shapefiles 
  summary(NJ$NAME)
  plot(NJ)
  ##first, need to change the polygon IDs so that they are not duplicated across shapefile sets
  NJ1 <- spChFIDs(NJ, as.character(NJ$GEOID))
  
  ## STEP 3: Attach city onto state data
  #set missing lon/lat to 0, & set lon/lat to coordinates
  ccd$latcod[is.na(ccd$latcod)] <- 0
  ccd$loncod[is.na(ccd$loncod)] <- 0
  coordinates(ccd) <- c("loncod", "latcod")
  
  #tell R that school coordinates are in the same lat/long reference system as the places data
  proj4string(ccd) <- proj4string(NJ1)
  
  #combine is.na() with over() to do the containment test (note that we need to "demote" places to a SpatialPolygons object first)
  inside.place <- !is.na(over(ccd, as(NJ1, "SpatialPolygons")))
  
  # use "over" again, this time with places as a SpatialPolygonsDataFrame object, to determine which places (if any) 
  # contains each school, and store the place name as attribute of the schools data
  ccd$city <- rep(NA, nrow(ccd))
  ccd$city <- over(ccd, NJ1)$NAMELSAD
  
  #write the augmented state dataset to new .dta file
  ccd <- as.data.frame(ccd)
  
  schools <- filter(ccd, city != "NA")
  
  for(i in 1:dim(schools)[2]) {
    if(class(schools[,i]) == "factor")
      schools[,i] <- as.character(schools[,i])
  }
  
  ### - - - - -
  saveRDS(schools, 
          paste("/Users/Alton/Google Drive/CRPE/Database/Other/ccd_", state, ".Rdata", sep = ""))
  ### - - - - -
  return(schools)
}

# Run function-------------------------------------------------------------------
AL <- as.data.frame(cityMerge("Alabama"))
AK <- as.data.frame(cityMerge("Alaska"))
AZ <- as.data.frame(cityMerge("Arizona"))
AR <- as.data.frame(cityMerge("Arkansas"))
CA <- as.data.frame(cityMerge("California"))
CO <- as.data.frame(cityMerge("Colorado"))
CT <- as.data.frame(cityMerge("Connecticut"))
DE <- as.data.frame(cityMerge("Delaware"))
DC <- as.data.frame(cityMerge("District of Columbia")) 
FL <- as.data.frame(cityMerge("Florida")) 
GA <- as.data.frame(cityMerge("Georgia")) 
HI <- as.data.frame(cityMerge("Hawaii")) 
ID <- as.data.frame(cityMerge("Idaho")) 
IL <- as.data.frame(cityMerge("Illinois"))
IN <- as.data.frame(cityMerge("Indiana")) 
IA <- as.data.frame(cityMerge("Iowa"))
KS <- as.data.frame(cityMerge("Kansas"))
KY <- as.data.frame(cityMerge("Kentucky")) 
LA <- as.data.frame(cityMerge("Louisiana")) 
ME <- as.data.frame(cityMerge("Maine")) 
MD <- as.data.frame(cityMerge("Maryland")) 
MA <- as.data.frame(cityMerge("Massachusetts")) 
MI <- as.data.frame(cityMerge("Michigan")) 
MN <- as.data.frame(cityMerge("Minnesota")) 
MS <- as.data.frame(cityMerge("Mississippi")) 
MO <- as.data.frame(cityMerge("Missouri")) 
MT <- as.data.frame(cityMerge("Montana")) 
NE <- as.data.frame(cityMerge("Nebraska"))
NV <- as.data.frame(cityMerge("Nevada"))  
NH <- as.data.frame(cityMerge("New Hampshire")) 
NJ <- as.data.frame(cityMerge("New Jersey")) 
NM <- as.data.frame(cityMerge("New Mexico")) 
NY <- as.data.frame(cityMerge("New York")) 
NC <- as.data.frame(cityMerge("North Carolina")) 
ND <- as.data.frame(cityMerge("North Dakota")) 
OH <- as.data.frame(cityMerge("Ohio")) 
OK <- as.data.frame(cityMerge("Oklahoma"))
OR <- as.data.frame(cityMerge("Oregon")) 
PA <- as.data.frame(cityMerge("Pennsylvania")) 
RI <- as.data.frame(cityMerge("Rhode Island")) 
SC <- as.data.frame(cityMerge("South Carolina")) 
SD <- as.data.frame(cityMerge("South Dakota")) 
TN <- as.data.frame(cityMerge("Tennessee"))
TX <- as.data.frame(cityMerge("Texas")) 
UT <- as.data.frame(cityMerge("Utah")) 
VT <- as.data.frame(cityMerge("Vermont")) 
VA <- as.data.frame(cityMerge("Virginia")) 
WA <- as.data.frame(cityMerge("Washington")) 
WV <- as.data.frame(cityMerge("West Virginia")) 
WI <- as.data.frame(cityMerge("Wisconsin")) 
WY <- as.data.frame(cityMerge("Wyoming"))


# Read data in -----------------------------------------------------------
read_ccd_file <- function(state){
  data <- readRDS(paste("/Users/Alton/Google Drive/CRPE/Database/Other/ccd_", state, ".Rdata", sep = ""))
  return(data)
}

AL <- read_ccd_file("Alabama")
AK <- as.data.frame(read_ccd_file("Alaska"))
AZ <- as.data.frame(read_ccd_file("Arizona"))
AR <- as.data.frame(read_ccd_file("Arkansas"))
CA <- as.data.frame(read_ccd_file("California")) 
CO <- as.data.frame(read_ccd_file("Colorado"))
CT <- as.data.frame(read_ccd_file("Connecticut"))
DE <- as.data.frame(read_ccd_file("Delaware"))
DC <- as.data.frame(read_ccd_file("District of Columbia")) 
FL <- as.data.frame(read_ccd_file("Florida")) 
GA <- as.data.frame(read_ccd_file("Georgia")) 
HI <- as.data.frame(read_ccd_file("Hawaii")) 
ID <- as.data.frame(read_ccd_file("Idaho")) 
IL <- as.data.frame(read_ccd_file("Illinois"))
IN <- as.data.frame(read_ccd_file("Indiana")) 
IA <- as.data.frame(read_ccd_file("Iowa"))
KS <- as.data.frame(read_ccd_file("Kansas"))
KY <- as.data.frame(read_ccd_file("Kentucky")) 
LA <- as.data.frame(read_ccd_file("Louisiana")) 
ME <- as.data.frame(read_ccd_file("Maine")) 
MD <- as.data.frame(read_ccd_file("Maryland")) 
MA <- as.data.frame(read_ccd_file("Massachusetts")) 
MI <- as.data.frame(read_ccd_file("Michigan")) 
MN <- as.data.frame(read_ccd_file("Minnesota")) 
MS <- as.data.frame(read_ccd_file("Mississippi")) 
MO <- as.data.frame(read_ccd_file("Missouri")) 
MT <- as.data.frame(read_ccd_file("Montana")) 
NE <- as.data.frame(read_ccd_file("Nebraska"))
NV <- as.data.frame(read_ccd_file("Nevada"))  
NH <- as.data.frame(read_ccd_file("New Hampshire")) 
NJ <- as.data.frame(read_ccd_file("New Jersey")) 
NM <- as.data.frame(read_ccd_file("New Mexico")) 
NY <- as.data.frame(read_ccd_file("New York")) 
NC <- as.data.frame(read_ccd_file("North Carolina"))
ND <- as.data.frame(read_ccd_file("North Dakota")) 
OH <- as.data.frame(read_ccd_file("Ohio"))
OK <- as.data.frame(read_ccd_file("Oklahoma"))
OR <- as.data.frame(read_ccd_file("Oregon")) 
PA <- as.data.frame(read_ccd_file("Pennsylvania")) 
RI <- as.data.frame(read_ccd_file("Rhode Island")) 
SC <- as.data.frame(read_ccd_file("South Carolina")) 
SD <- as.data.frame(read_ccd_file("South Dakota")) 
TN <- as.data.frame(read_ccd_file("Tennessee"))
TX <- as.data.frame(read_ccd_file("Texas")) 
UT <- as.data.frame(read_ccd_file("Utah")) 
VT <- as.data.frame(read_ccd_file("Vermont")) 
VA <- as.data.frame(read_ccd_file("Virginia")) 
WA <- as.data.frame(read_ccd_file("Washington")) 
WV <- as.data.frame(read_ccd_file("West Virginia")) 
WI <- as.data.frame(read_ccd_file("Wisconsin")) 
WY <- as.data.frame(read_ccd_file("Wyoming"))


# Final part -----------------------------------------------------------------------------

test <- rbind(AL, AK, AZ, AR, CA, CO, CT, DE, DC, FL, GA, HI, ID, 
              IL, IN, IA, KS, KY, LA, ME, MD, MA, MI, MN, MS, 
              MO, MT, NE, NV, NH, NJ, NM, NY, NC, ND, OH, OK, OR, 
              PA, RI, SC, SD, TN, TX, UT, VT, VA, WA, WV, WI, WY)

write.csv(test, "/Users/Alton/Google Drive/CRPE/Database/Other/Master_School_Cities.csv", row.names = FALSE)
saveRDS(test, "/Users/Alton/Google Drive/CRPE/Database/Other/Master_School_Cities.Rdata")
