### Center on Reinventing Public Education
# CCD Financial data cleaning for CRPE Database
# AWS integration for cleaning
# Created July 2nd, 2017
# Alton Lu
# Last Edit: July 6th, 2017 - Alton Lu
# CRPE Github at https://github.com/CRPE-UWB

# F33 CCD Financial data at https://nces.ed.gov/ccd/f33agency.asp
# Codebook at https://nces.ed.gov/ccd/pdf/2015304.pdf

# AWS = Amazon Web Services, S3 = Simple Storage System (files)
# CCD = common core of data

# rm(list=ls()) 

# Setup ------------------------------------------------------------------
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

# Key ID - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Point this to wherever you store your AWS Credentials - - - - - - - 
access <- read.csv("/Users/Alton/OneDrive/Work/CRPE/credentials.csv")
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

key <- as.character(access[1,3])
secret <- as.character(access[1,4])

Sys.setenv(
  "AWS_ACCESS_KEY_ID" = key,
  "AWS_SECRET_ACCESS_KEY" = secret
)

# Should give a list of buckets in CRPE AWS S3
bucket_list_df()

# reads into AWS, pulls a file
# column is specification for different files: see appendix for more
readAWSData <- function(filename, column){
  
  location <- "Data/Federal/ccd_financial/raw_data/"
  bucketname <- "database.crpe"
  
  save_object(paste(location, filename, sep =""), bucket = bucketname, file = filename)
  data <- read.delim(filename, header = TRUE, sep="\t", na.strings = 'N')
  data <- dplyr::select(data, 1:column)
  
  if (file.exists(filename)) file.remove(filename)
  
  return(data)
}


# Reading in data ------------------------------------------------------------

# Ignore parsing failures with a "FL"/"GSHI" in front of variable
data14 <- readAWSData("F2013_14.txt", 141)


data13 <- readAWSData("F2012_13.txt", 141)
data12 <- readAWSData("F2011_12.txt", 141)
data11 <- readAWSData("F2010_11.txt", 141)
data10 <- readAWSData("F2009_10.txt", 141)
data09 <- readAWSData("F2008_09.txt", 141)
data08 <- readAWSData("F2007_08.txt", 138)
data07 <- readAWSData("F2006_07.txt", 138)
data06 <- readAWSData("F2005_06.txt", 138)
data05 <- readAWSData("F2004_05.txt", 134)
data04 <- readAWSData("F2003_04.txt", 134)
data04$LEAID <- str_pad(data04$LEAID, width = 7, side = "left", pad = "0")
data03 <- readAWSData("F2002_03.txt", 129)
data02 <- readAWSData("F2001_02.txt", 128)
data01 <- readAWSData("F2000_01.txt", 128)
data00 <- readAWSData("F1999_00.txt", 128)
data99 <- readAWSData("F1998_99.txt", 128)
data98 <- readAWSData("F1997_98.txt", 129)
data97 <- readAWSData("F1996_97.txt", 131)
data96 <- readAWSData("F1995_96.txt", 127)
data95 <- readAWSData("F1994_95.txt", 131)

# Manipulating data -----------------------------------------------------------

# Combine all data into one dataframe
df <- rbind.fill(data14, data13, data12, data11, data10, data09, 
                 data08, data07, data06, data05, data04, data03, 
                 data02, data01, data00, data99, data98, data97, 
                 data96, data95)

# Remove unneeded columns
df<- select(df, -CONUM, -CCDNF, -CENFILE, -GSLO, -GSHI, -MEMBERSCH, -WEIGHT,
            -CENSUSID, -CSA, -CBSA, -STNAME, -STABBR, -SCHLEV, -AGCHRT, -FIPSCO,
            -CMSA, - WEIGHT)

# Remove the schools without identification
df <- filter(df, LEAID != "NA")
# Remove schools with less than 1 student
df <- filter(df, V33 >= 1)

glimpse(df)
names(df)

# Change the names of a few columns (R doesn't like "_" at the beginning)
colnames(df)[117] <- "X_19H"
colnames(df)[118] <- "X_21F"
colnames(df)[119] <- "X_31F"
colnames(df)[120] <- "X_41F"
colnames(df)[121] <- "X_61V"
colnames(df)[122] <- "X_66V"
                  
# Change the year to more common structure
df$YEAR[df$YEAR == 14] <- 2014
df$YEAR[df$YEAR == 13] <- 2013
df$YEAR[df$YEAR == 12] <- 2012
df$YEAR[df$YEAR == 11] <- 2011
df$YEAR[df$YEAR == 10] <- 2010
df$YEAR[df$YEAR == 9] <- 2009
df$YEAR[df$YEAR == 8] <- 2008
df$YEAR[df$YEAR == 7] <- 2007
df$YEAR[df$YEAR == 6] <- 2006
df$YEAR[df$YEAR == 5] <- 2005
df$YEAR[df$YEAR == 4] <- 2004
df$YEAR[df$YEAR == 3] <- 2003
df$YEAR[df$YEAR == 2] <- 2002
df$YEAR[df$YEAR == 1] <- 2001
df$YEAR[df$YEAR == 0] <- 2000
df$YEAR[df$YEAR == 99] <- 1999
df$YEAR[df$YEAR == 98] <- 1998
df$YEAR[df$YEAR == 97] <- 1997
df$YEAR[df$YEAR == 96] <- 1996
df$YEAR[df$YEAR == 95] <- 1995

# Changes all NA to -99 for database
df[is.na(df)] <- "-99"

# Saving data ----------------------------------------------------------

# Save the data to your own computer if you want
# setwd(directory)
# write.csv(df, "school_finance.csv", row.names = FALSE)
# saveRDS(df, "school_finance.Rdata")

# This creates a temporary directory that disappears when you close R
temp <- tempfile(pattern = "", fileext = ".csv")

# write the csv to the temporary directory
write.csv(df, temp, row.names = FALSE)

# Put the temporary csv into AWS S3
put_object(
  file = temp, 
  object = "Data/Federal/ccd_financial/clean_data/ccd_finance_2014.csv", # Change
  bucket = "database.crpe")

### Appendix ----------------------------------------------------------------
# It's helpful to check the data in an excel file to see if it matches up properly. 

