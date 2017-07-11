### Center on Reinventing Public Education
# Edfacts graduation data cleaning for CRPE Database
# AWS integration for cleaning
# Created July 2nd, 2017
# Alton Lu
# Last Edit: July 6th, 2017 - Alton Lu
# CRPE Github at ___________

# Raw data at https://www2.ed.gov/about/inits/ed/edfacts/data-files/index.html
# Codebook available at above link 'Regulatory Adjusted Cohort Graduation Rate'

# AWS = Amazon Web Services, S3 = Simple Storage System (files)
# CCD = common core of data

# rm(list=ls()) 

# Setup ----------------------------------------------------
# install.packages("aws.s3")
# install.packages("magrittr")
# install.packages("readr")
# install.packages("plyr")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("stringr")
# install.packages("lubridate")

library(aws.s3)
library(magrittr)
library(readr)
library(plyr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

# Connecting to AWS --------------------------------------------------
# Key ID
access <- read.csv("/Users/Alton/OneDrive/Work/CRPE/credentials.csv")

key <- as.character(access[1,3])
secret <- as.character(access[1,4])

Sys.setenv(
  "AWS_ACCESS_KEY_ID" = key,
  "AWS_SECRET_ACCESS_KEY" = secret
)

# Should give a list of buckets in CRPE AWS S3
bucket_list_df()

# function that reads AWS data -------------------------
readAWSData <- function(filename){
  
  location <- "Data/Federal/edfacts/raw_data/"
  bucketname <- "database.crpe"
  
  save_object(paste(location, filename, sep =""), bucket = bucketname, file = filename)
  data <- read.csv(filename)
  
  if (file.exists(filename)) file.remove(filename)
  
  return(data)
}

filename <- "EdFacts 2015.csv"
# Reading in data from AWS using the readAWSData function ------------------
data15 <- readAWSData("EdFacts 2015.csv")
data14 <- readAWSData("EdFacts 2014.csv")
data14 <- select(data14, -28)
data13 <- readAWSData("EdFacts 2013.csv")
data12 <- readAWSData("EdFacts 2012.csv")
data11 <- readAWSData("EdFacts 2011.csv")

glimpse(data14)

# Function that cleans the data ---------------------------------------------
Clean <- function(data, year){
  
  namelist <- c("STNAM", "FIPST", "LEAID", "LEANM", "ncessch",
                "SCHNAM", "ALL_COHORT", "ALL_RATE", "MAM_COHORT",
                "MAM_RATE", "MAS_COHORT", "MAS_RATE", "MBL_COHORT",
                "MBL_RATE", "MHI_COHORT", "MHI_RATE", "MTR_COHORT",
                "MTR_RATE", "MWH_COHORT", "MWH_RATE", "CWD_COHORT", 
                "CWD_RATE", "ECD_COHORT", "ECD_RATE", 
                "LEP_COHORT", "LEP_RATE", "YEAR")
  
  names(data) <- namelist
  data[data == "."] <- "NA"
  
  # changes all factor types to character types
  for(i in 1:dim(data)[2]) {
    if(class(data[,i]) == "factor")
      data[,i] <- as.character(data[,i])
  }
  
  # Changes these specifics
  data[data == "GE95"] <- "95"
  data[data == "GE90"] <- "90"
  data[data == "GE80"] <- "80"
  data[data == "GE50"] <- "50"
  glimpse(data)
  
  # Splits xy-xy into the midpoint single number
  data$ALL_RATE <- sapply(strsplit(data$ALL_RATE, split = "-"),
                          function(x) mean(as.numeric(x)))
  data$MAM_RATE <- sapply(strsplit(data$MAM_RATE, split = "-"),
                          function(x) mean(as.numeric(x)))
  data$MAS_RATE <- sapply(strsplit(data$MAS_RATE, split = "-"),
                          function(x) mean(as.numeric(x)))
  data$MBL_RATE <- sapply(strsplit(data$MBL_RATE, split = "-"),
                          function(x) mean(as.numeric(x)))
  data$MHI_RATE <- sapply(strsplit(data$MHI_RATE, split = "-"),
                          function(x) mean(as.numeric(x)))
  data$MTR_RATE <- sapply(strsplit(data$MTR_RATE, split = "-"),
                          function(x) mean(as.numeric(x)))
  data$MWH_RATE <- sapply(strsplit(data$MWH_RATE, split = "-"),
                          function(x) mean(as.numeric(x)))
  data$CWD_RATE <- sapply(strsplit(data$CWD_RATE, split = "-"),
                          function(x) mean(as.numeric(x)))
  data$ECD_RATE <- sapply(strsplit(data$ECD_RATE, split = "-"),
                          function(x) mean(as.numeric(x)))
  data$LEP_RATE <- sapply(strsplit(data$LEP_RATE, split = "-"),
                          function(x) mean(as.numeric(x)))
  
  data$YEAR <- year
  
  data[,7:27] <- as.numeric(unlist(data[,7:27]))
  
  
  return(data)
  
}

# running the function --------------------------------------------------------

# New datafiles may have an incorrect number of columns. You'll need to check
# the codebook to ensure they match. Opening in excel also works. Should have 27 variables
data <- as.data.frame(Clean(data15, 2015))
# 2014 data had an extra column. It didn't affect the function
data1 <- as.data.frame(Clean(data14, 2014))
data1 <- select(data1, -28)

data2 <- as.data.frame(Clean(data13, 2013))
data3 <- as.data.frame(Clean(data12, 2012))
data4 <- as.data.frame(Clean(data11, 2011))

# Bind the data together
df <- rbind(data, data1, data2, data3, data4)

# Saving data ----------------------------------------------------------

# Save the data to your own computer if you want
# setwd(directory)
# write.csv(df, "name.csv", row.names = FALSE)
# saveRDS(df, "name.Rdata")

# The database doesn't read NA. Inputting as -99 instead
df[is.na(df)] <- "-99"

# Changes columns 7:27 to a numeric. 
df[,7:27] <- as.numeric(unlist(df[,7:27]))

# This creates a temporary directory that disappears when you close R
temp <- tempfile(pattern = "", fileext = ".csv")

# write the csv to the temporary directory
write.csv(df, temp, row.names = FALSE)

# Put the temporary csv into AWS S3
put_object(
  file = temp, 
  object = "/Data/Federal/edfacts/clean_data/edfacts_clean_2015.csv", # Change
  bucket = "database.crpe")


