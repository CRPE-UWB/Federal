#### Center on Reinventing Public Education #### 
# Title: Cleaning CCD Public Elementary/Secondary School Universe Survey Data
# Description: Attempt to Clean Public Elementary/Secondary School Universe Survey Data
# Created by: Kevin Cha on 06-30-17
# Updated by: Kevin Cha on 08-10-17
# Data from: https://nces.ed.gov/ccd/pubschuniv.asp
# Link to Github: https://github.com/CRPE-UWB/Federal
# Notes:
# * 0 = No Occurences
# * -1, M = Data Missing
# * -2, N, NA = Data NA
# * -9 = Data Not Meeting NCES data quality Standards (so prob make it same as NA)
# * Cleaning: -99 = NA or Missing
#
#########################################
# IMPORTANT NOTE: This is a partial clean up due to the fact that older format (2006-07 and earlier or basically when the data is split up into 3) 
#                 is too messy to clean even with R. 
#########################################

# Setup: Prep --------------------------------------------------------------------------------------------------------------
rm(list=ls())
setwd("/Users/crpe/Documents/CCD_Cleaning") # MAC

library(plyr)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

# Functions ------------------------------------------------------------------------------
read.function <- function(filename){
  data <- read.delim(filename, header = TRUE, sep="\t", na.strings = c('M', 'N'))
}

na_to_neg99 <- function(df) {
  # Turn columns into numeric columns
  df$LEVEL <- as.numeric(df$LEVEL)
  df$TITLEI <- as.numeric(df$TITLEI)
  df$STITLI <- as.numeric(df$STITLI)
  df$MAGNET <- as.numeric(df$MAGNET)
  df$CHARTR <- as.numeric(df$CHARTR)
  df$SHARED <- as.numeric(df$SHARED)
  
  # Changes the numeric NAs into -99
  df[na_list][is.na(df[na_list])] <- -99 
  df[df == -2] <- -99
  df[df == -1] <- -99
  
  return(df)
}

# We want to create a standard column name list for all of the different years. 
# Main Difference: -they add a num for the year the end (ex 10, 09)
#                  -AND THEY ALL DON'T INCLUDE THE SAME INFO GAHH   
# Because of that, this only applies to 2008, 2009, 2010
change_colnames <- function(df) {
  # Get the column names (excluding the first 4 since they don't have the year)
  columns <- colnames(df[,5:ncol(df)])
  # Substring the column names so that it doesn't include the 09
  columns_fixed <- substring(columns, 1, nchar(columns) - 2)
  # Append the new list to include the first 4 columns of the original file
  columns_list <- c(colnames(df[,1:4]), columns_fixed)
  # Set the column names 
  colnames(df) <- columns_list
  
  return(df)
}

# Setup: Lists ------------------------------------------------------------------------------
# List of Columns to Turn NA => -99
na_list <- c("LEVEL", "TITLEI", "STITLI", "MAGNET", "CHARTR", "SHARED")

# 2009-10 ------------------------------------------------------------------------------
# Read in the file
ccd_10 <- read.function("data/sc092a.txt")

# Change column names
ccd_10 <- change_colnames(ccd_10)

## TESTING PURPOSES:
# temp <- ccd_10 # store into temp to avoid having to re-read the data
# sapply(ccd_10, mode) # checks the mode
# sapply(ccd_10, class) # checks the class

# Turn columns into numeric columns
ccd_10$LEVEL <- as.numeric(ccd_10$LEVEL)
ccd_10$TITLEI <- as.numeric(ccd_10$TITLEI)
ccd_10$STITLI <- as.numeric(ccd_10$STITLI)
ccd_10$MAGNET <- as.numeric(ccd_10$MAGNET)
ccd_10$CHARTR <- as.numeric(ccd_10$CHARTR)
ccd_10$SHARED <- as.numeric(ccd_10$SHARED)

# Changes the numeric NAs into -99
ccd_10[na_list][is.na(ccd_10[na_list])] <- -99 
ccd_10[ccd_10 == -2] <- -99
ccd_10[ccd_10 == -1] <- -99

# Add YEAR column
ccd_10$YEAR <- 2010

# Gets rid of commas
ccd_10$SCHNAM <- gsub(",", " ", ccd_10$SCHNAM)
ccd_10$LEANM <- gsub(",", " ", ccd_10$LEANM)

# Writes to .csv file
# write.csv(ccd_10, 'cleaned_data/CCD_200910.csv')

# 2008-09 ------------------------------------------------------------------------------
ccd_09 <- read.function("data/sc081b.txt")

# Change column names
ccd_09 <- change_colnames(ccd_09)

## TESTING PURPOSES:
# temp <- ccd_10 # store into temp to avoid having to re-read the data
# sapply(ccd_10, mode) # checks the mode
# sapply(ccd_10, class) # checks the class

# Turn columns into numeric columns
ccd_09$LEVEL <- as.numeric(ccd_09$LEVEL)
ccd_09$TITLEI <- as.numeric(ccd_09$TITLEI)
ccd_09$STITLI <- as.numeric(ccd_09$STITLI)
ccd_09$MAGNET <- as.numeric(ccd_09$MAGNET)
ccd_09$CHARTR <- as.numeric(ccd_09$CHARTR)
ccd_09$SHARED <- as.numeric(ccd_09$SHARED)

# Changes the numeric NAs into -99
ccd_09[na_list][is.na(ccd_09[na_list])] <- -99 
ccd_09[ccd_09 == -2] <- -99
ccd_09[ccd_09 == -1] <- -99

# Add YEAR column
ccd_09$YEAR <- 2009

# Gets rid of commas
ccd_09$SCHNAM <- gsub(",", " ", ccd_09$SCHNAM)
ccd_09$LEANM <- gsub(",", " ", ccd_09$LEANM)



# Writes to .csv file
write.csv(ccd_09, 'cleaned_data/CCD_200809.csv')



# 2007-08 ------------------------------------------------------------------------------
ccd_08 <- read.function("data/sc071b.txt")

# Change column names
ccd_08 <- change_colnames(ccd_08)

## TESTING PURPOSES:
# temp <- ccd_08 # store into temp to avoid having to re-read the data
# sapply(ccd_08, mode) # checks the mode
# sapply(ccd_08, class) # checks the class

# Turn columns into numeric columns
ccd_08$LEVEL <- as.numeric(ccd_08$LEVEL)
ccd_08$TITLEI <- as.numeric(ccd_08$TITLEI)
ccd_08$STITLI <- as.numeric(ccd_08$STITLI)
ccd_08$MAGNET <- as.numeric(ccd_08$MAGNET)
ccd_08$CHARTR <- as.numeric(ccd_08$CHARTR)
ccd_08$SHARED <- as.numeric(ccd_08$SHARED)

# Changes the numeric NAs into -99
ccd_08[na_list][is.na(ccd_08[na_list])] <- -99 
ccd_08[ccd_08 == -2] <- -99
ccd_08[ccd_08 == -1] <- -99

# Add YEAR column
ccd_08$YEAR <- 2008

# Gets rid of commas
ccd_08$SCHNAM <- gsub(",", " ", ccd_08$SCHNAM)
ccd_08$LEANM <- gsub(",", " ", ccd_08$LEANM)

# Writes to .csv file
write.csv(ccd_08, 'cleaned_data/CCD_200708.csv')


# 2006-07 ------------------------------------------------------------------------------
# Read in .dat for states A-I
ccd_07_ai <- read_table("data/Sc061cai.dat", col_names = FALSE, col_types = NULL)

# SEPARATES COLUMNS
ccd_07_ai <- separate(ccd_07_ai, X1, c("X1.a","X1.b"), sep = -4) # separates STID from NCESSCH
ccd_07_ai <- separate(ccd_07_ai, X5, c("X5.a","X5.b"), sep = 10) # separates PHONE from MSTREE
ccd_07_ai <- separate(ccd_07_ai, X7, c("X7.a","X7.b"), sep = 2) # separates MSTATE from MZIP+MZIP4+LSTREE+LCITY
ccd_07_ai <- separate(ccd_07_ai, X7.b, c("X7.b","X7.c"), sep = 5) # separates MZIP from MZIP4+LSTREE+LCITY
ccd_07_ai <- separate(ccd_07_ai, X8, c("X8.a","X8.b"), sep = 2) # separates LSTATE from LZIP+LZIP4+OTHERS
ccd_07_ai <- separate(ccd_07_ai, X8.b, c("X8.b","X8.c"), sep = 5) # separates LZIP from LZIP4+OTHERS
ccd_07_ai <- separate(ccd_07_ai, X8.c, c("X8.c","X8.d"), sep = -10) # separates the lat coords from OTHERs
ccd_07_ai <- separate(ccd_07_ai, X9, c("X9.a","X9.c"), sep = 19) # separates the lon coords and CONUM from OTHERs
ccd_07_ai <- separate(ccd_07_ai, X9.a, c("X9.a","X9.b"), sep = -5) # separates CONUM from lon coords
ccd_07_ai <- separate(ccd_07_ai, X12, c("X12.a","X12.b"), sep = 3) # separates 2 digits
# Turn NA/Missing => -99
ccd_07_ai[ccd_07_ai == -2] <- -99
ccd_07_ai[ccd_07_ai == -1] <- -99
ccd_07_ai[ccd_07_ai == 'M'] <- -99
ccd_07_ai[ccd_07_ai == 'N'] <- -99
# Set COLNAMES
colnames(ccd_07_ai) <- c("NCESSCH", "STID", "SEASCH", "LEANM", "SCHNAM",
                         "PHONE", "MSTREE", "MCITY", "MSTATE","MZIP",
                         "MZIP4_LSTREE","LSTATE","LZIP","LZIP4_TYPE_STATUS_ULOCAL",
                         "LATCOD","LONCOD","CONUM","CONAME", "OTHERS",
                         'X20', 'X21', 'X22', 'X23', 'X24', 'X25', 'X26', 'X27', 'X28', 'X29', 'X30', 'X31', 'X32', 'X34', 'X35', 'X36', 'X37', 'X38', 'X39', 'X40',
                         'X41', 'X42', 'X43', 'X44', 'X45', 'X46', 'X47', 'X48', 'X49', 'X50', 'X51', 'X52', 'X53', 'X54', 'X55', 'X56', 'X57', 'X58', 'X59', 'X50',
                         'X61', 'X62', 'X63', 'X64', 'X65', 'X66', 'X67', 'X68', 'X69', 'X70', 'X71', 'X72', 'X73', 'X74', 'X75', 'X76', 'X77', 'X78', 'X79', 'X80',
                         'X81', 'X82', 'X83', 'X84', 'X85', 'X86', 'X87', 'X88', 'X89', 'X90', 'X81', 'X82', 'X83', 'X84', 'X95', 'X96', 'X97', 'X98', 'X99', 'X100',
                         'X101', 'X102', 'X103', 'X104', 'X105', 'X106', 'X107', 'X108', 'X109', 'X110', 'X111', 'X112', 'X113', 'X114', 'X115', 'X116', 'X117', 'X118', 'X119', 'X120',
                         'X121', 'X122', 'X123', 'X124', 'X125', 'X126', 'X127', 'X128', 'X129', 'X130', 'X131', 'X132', 'X133', 'X134', 'X135', 'X136', 'X137', 'X138', 'X139', 'X140',
                         'X141', 'X142', 'X143', 'X144', 'X145', 'X146', 'X147', 'X148', 'X159', 'X150', 'X151', 'X152', 'X153', 'X154', 'X155', 'X156', 'X157', 'X158', 'X159', 'X160',
                         'X161', 'X162', 'X163', 'X164', 'X165', 'X166', 'X167', 'X168', 'X169', 'X170', 'X171', 'X172', 'X173', 'X174', 'X175', 'X176', 'X177', 'X178', 'X179', 'X180',
                         'X181', 'X182', 'X183', 'X184', 'X185', 'X186', 'X187', 'X188', 'X189', 'X190', 'X191', 'X192', 'X193', 'X194', 'X195', 'X196', 'X197', 'X198', 'X199', 'X200',
                         'X201', 'X202', 'X203', 'X204', 'X205', 'X206', 'X207', 'X208', 'X209', 'X210', 'X211', 'X212', 'X213', 'X214', 'X215', 'X216', 'X217', 'X218', 'X219', 'X220',
                         'X221', 'X222', 'X223', 'X224', 'X225', 'X226', 'X227', 'X228', 'X229', 'X230', 'X231', 'X232', 'X233', 'X234', 'X235', 'X236', 'X237', 'X238', 'X239', 'X240',
                         'X241', 'X242', 'X243', 'X244', 'X245', 'X246', 'X247', 'X248', 'X249', 'X250', 'X251', 'X252', 'X253', 'X254', 'X255', 'X256', 'X257', 'X258', 'X259', 'X260',
                         'X261', 'X262', 'X263', 'X264', 'X265', 'X266', 'X267', 'X268', 'X269', 'X270', 'X271', 'X272', 'X273', 'X274', 'X275', 'X276', 'X277', 'X278', 'X279', 'X280',
                         'X281', 'X282', 'X283', 'X284')
# Add YEAR column
ccd_07_ai$YEAR <- 2007
# Gets rid of commas
ccd_07_ai$SCHNAM <- gsub(",", " ", ccd_07_ai$SCHNAM)
ccd_07_ai$LEANM <- gsub(",", " ", ccd_07_ai$LEANM)


# Read in .dat for states K-N
ccd_07_kn <- read_table("data/Sc061ckn.dat", col_names = FALSE, col_types = NULL)

# SEPARATES COLUMNS
ccd_07_kn <- separate(ccd_07_kn, X1, c("X1.a","X1.b"), sep = -6) # separates STID from NCESSCH
ccd_07_kn <- separate(ccd_07_kn, X5, c("X5.a","X5.b"), sep = 10) # separates PHONE from MSTREE
ccd_07_kn <- separate(ccd_07_kn, X7, c("X7.a","X7.b"), sep = 2) # separates MSTATE from MZIP+MZIP4+LSTREE+LCITY
ccd_07_kn <- separate(ccd_07_kn, X7.b, c("X7.b","X7.c"), sep = 5) # separates MZIP from MZIP4+LSTREE+LCITY
ccd_07_kn <- separate(ccd_07_kn, X9, c("X9.a","X9.b"), sep = 2) # separates LSTATE from LZIP+LZIP4+OTHERS
ccd_07_kn <- separate(ccd_07_kn, X9.b, c("X9.b","X9.c"), sep = 5) # separates LZIP from LZIP4+OTHERS
# Turn NA/Missing => -99
ccd_07_kn[ccd_07_kn == -2] <- -99
ccd_07_kn[ccd_07_kn == -1] <- -99
ccd_07_kn[ccd_07_kn == 'M'] <- -99
ccd_07_kn[ccd_07_kn == 'N'] <- -99
# Set COLNAMES
colnames(ccd_07_kn) <- c("NCESSCH", "STID", "SEASCH", "LEANM", "SCHNAM",
                         "PHONE", "MSTREE", "MCITY", "MSTATE","MZIP",
                         "MZIP4_LSTREE","LSTATE","LZIP","LZIP4_TYPE_STATUS_ULOCAL_LATCOD_LONCOD","OTHER",
                         'X20', 'X21', 'X22', 'X23', 'X24', 'X25', 'X26', 'X27', 'X28', 'X29', 'X30', 'X31', 'X32', 'X34', 'X35', 'X36', 'X37', 'X38', 'X39', 'X40',
                         'X41', 'X42', 'X43', 'X44', 'X45', 'X46', 'X47', 'X48', 'X49', 'X50', 'X51', 'X52', 'X53', 'X54', 'X55', 'X56', 'X57', 'X58', 'X59', 'X50',
                         'X61', 'X62', 'X63', 'X64', 'X65', 'X66', 'X67', 'X68', 'X69', 'X70', 'X71', 'X72', 'X73', 'X74', 'X75', 'X76', 'X77', 'X78', 'X79', 'X80',
                         'X81', 'X82', 'X83', 'X84', 'X85', 'X86', 'X87', 'X88', 'X89', 'X90', 'X81', 'X82', 'X83', 'X84', 'X95', 'X96', 'X97', 'X98', 'X99', 'X100',
                         'X101', 'X102', 'X103', 'X104', 'X105', 'X106', 'X107', 'X108', 'X109', 'X110', 'X111', 'X112', 'X113', 'X114', 'X115', 'X116', 'X117', 'X118', 'X119', 'X120',
                         'X121', 'X122', 'X123', 'X124', 'X125', 'X126', 'X127', 'X128', 'X129', 'X130', 'X131', 'X132', 'X133', 'X134', 'X135', 'X136', 'X137', 'X138', 'X139', 'X140',
                         'X141', 'X142', 'X143', 'X144', 'X145', 'X146', 'X147', 'X148', 'X159', 'X150', 'X151', 'X152', 'X153', 'X154', 'X155', 'X156', 'X157', 'X158', 'X159', 'X160',
                         'X161', 'X162', 'X163', 'X164', 'X165', 'X166', 'X167', 'X168', 'X169', 'X170', 'X171', 'X172', 'X173', 'X174', 'X175', 'X176', 'X177', 'X178', 'X179', 'X180',
                         'X181', 'X182', 'X183', 'X184', 'X185', 'X186', 'X187', 'X188', 'X189', 'X190', 'X191', 'X192', 'X193', 'X194', 'X195', 'X196', 'X197', 'X198', 'X199', 'X200',
                         'X201', 'X202', 'X203', 'X204', 'X205', 'X206', 'X207', 'X208', 'X209', 'X210', 'X211', 'X212', 'X213', 'X214', 'X215', 'X216', 'X217', 'X218', 'X219', 'X220',
                         'X221', 'X222', 'X223', 'X224', 'X225', 'X226', 'X227', 'X228', 'X229', 'X230', 'X231', 'X232', 'X233', 'X234', 'X235', 'X236', 'X237', 'X238', 'X239', 'X240',
                         'X241', 'X242', 'X243', 'X244', 'X245', 'X246', 'X247', 'X248', 'X249', 'X250', 'X251', 'X252', 'X253', 'X254', 'X255', 'X256', 'X257', 'X258', 'X259', 'X260',
                         'X261', 'X262', 'X263', 'X264', 'X265', 'X266', 'X267', 'X268', 'X269', 'X270', 'X271', 'X272', 'X273', 'X274', 'X275', 'X276', 'X277', 'X278', 'X279', 'X280',
                         'X281', 'X282', 'X283', 'X284')
# Add YEAR column
ccd_07_kn$YEAR <- 2007
# Gets rid of commas
ccd_07_kn$SCHNAM <- gsub(",", " ", ccd_07_kn$SCHNAM)
ccd_07_kn$LEANM <- gsub(",", " ", ccd_07_kn$LEANM)

# Read in .dat for states O-W
ccd_07_ow <- read_table("data/Sc061cow.dat", col_names = FALSE, col_types = NULL)

# SEPARATES COLUMNS
ccd_07_ow <- separate(ccd_07_ow, X1, c("X1.a","X1.b"), sep = -6) # separates STID from NCESSCH
ccd_07_ow <- separate(ccd_07_ow, X4, c("X4.a","X4.b"), sep = 2) # separates MSTATE from MZIP+MZIP4+LSTREE+LCITY
ccd_07_ow <- separate(ccd_07_ow, X4.b, c("X4.b","X4.c"), sep = 5) # separates MZIP from MZIP4+LSTREE+LCITY
ccd_07_ow <- separate(ccd_07_ow, X5, c("X5.a","X5.b"), sep = 2) # separates MSTATE from MZIP+MZIP4+LSTREE+LCITY
ccd_07_ow <- separate(ccd_07_ow, X5.b, c("X5.b","X5.c"), sep = 5) # separates MZIP from MZIP4+LSTREE+LCITY
ccd_07_ow <- separate(ccd_07_ow, X5.c, c("X5.c","X5.d"), sep = -10) # separates the lat coords from OTHERs
ccd_07_ow <- separate(ccd_07_ow, X6, c("X6.a","X6.c"), sep = 19) # separates the lon coords and CONUM from OTHERs
ccd_07_ow <- separate(ccd_07_ow, X6.a, c("X6.a","X6.b"), sep = -5) # separates CONUM from lon coords
ccd_07_ow <- separate(ccd_07_ow, X8, c("X8.a","X8.b"), sep = 2) # separates CONUM from lon coords
# Turn NA/Missing => -99
ccd_07_ow[ccd_07_ow == -2] <- -99
ccd_07_ow[ccd_07_ow == -1] <- -99
ccd_07_ow[ccd_07_ow == 'M'] <- -99
ccd_07_ow[ccd_07_ow == 'N'] <- -99
# Set COLNAMES
colnames(ccd_07_ow) <- c("NCESSCH", "STID", "SEASCH", "LEANM_SCHNAM", 
                         "MSTATE", "MZIP", "MZIP4_LSTREE","LSTATE","LZIP","LZIP4_TYPE_STATUS_ULOCAL", "LATCOD", "LONCOD", "CONUM", "CONAME", "OTHER",
                         'X20', 'X21', 'X22', 'X23', 'X24', 'X25', 'X26', 'X27', 'X28', 'X29', 'X30', 'X31', 'X32', 'X34', 'X35', 'X36', 'X37', 'X38', 'X39', 'X40',
                         'X41', 'X42', 'X43', 'X44', 'X45', 'X46', 'X47', 'X48', 'X49', 'X50', 'X51', 'X52', 'X53', 'X54', 'X55', 'X56', 'X57', 'X58', 'X59', 'X50',
                         'X61', 'X62', 'X63', 'X64', 'X65', 'X66', 'X67', 'X68', 'X69', 'X70', 'X71', 'X72', 'X73', 'X74', 'X75', 'X76', 'X77', 'X78', 'X79', 'X80',
                         'X81', 'X82', 'X83', 'X84', 'X85', 'X86', 'X87', 'X88', 'X89', 'X90', 'X81', 'X82', 'X83', 'X84', 'X95', 'X96', 'X97', 'X98', 'X99', 'X100',
                         'X101', 'X102', 'X103', 'X104', 'X105', 'X106', 'X107', 'X108', 'X109', 'X110', 'X111', 'X112', 'X113', 'X114', 'X115', 'X116', 'X117', 'X118', 'X119', 'X120',
                         'X121', 'X122', 'X123', 'X124', 'X125', 'X126', 'X127', 'X128', 'X129', 'X130', 'X131', 'X132', 'X133', 'X134', 'X135', 'X136', 'X137', 'X138', 'X139', 'X140',
                         'X141', 'X142', 'X143', 'X144', 'X145', 'X146', 'X147', 'X148', 'X159', 'X150', 'X151', 'X152', 'X153', 'X154', 'X155', 'X156', 'X157', 'X158', 'X159', 'X160',
                         'X161', 'X162', 'X163', 'X164', 'X165', 'X166', 'X167', 'X168', 'X169', 'X170', 'X171', 'X172', 'X173', 'X174', 'X175', 'X176', 'X177', 'X178', 'X179', 'X180',
                         'X181', 'X182', 'X183', 'X184', 'X185', 'X186', 'X187', 'X188', 'X189', 'X190', 'X191', 'X192', 'X193', 'X194', 'X195', 'X196', 'X197', 'X198', 'X199', 'X200',
                         'X201', 'X202', 'X203', 'X204', 'X205', 'X206', 'X207', 'X208', 'X209', 'X210', 'X211', 'X212', 'X213', 'X214', 'X215', 'X216', 'X217', 'X218', 'X219', 'X220',
                         'X221', 'X222', 'X223', 'X224', 'X225', 'X226', 'X227', 'X228', 'X229', 'X230', 'X231', 'X232', 'X233', 'X234', 'X235', 'X236', 'X237', 'X238', 'X239', 'X240',
                         'X241', 'X242', 'X243', 'X244', 'X245', 'X246', 'X247', 'X248', 'X249', 'X250', 'X251', 'X252', 'X253', 'X254', 'X255', 'X256', 'X257', 'X258', 'X259', 'X260',
                         'X261', 'X262', 'X263', 'X264', 'X265', 'X266', 'X267', 'X268', 'X269', 'X270', 'X271', 'X272', 'X273', 'X274', 'X275', 'X276', 'X277')
# Add YEAR column
ccd_07_ow$YEAR <- 2007
# Gets rid of commas
ccd_07_ow$LEANM_SCHNAM <- gsub(",", " ", ccd_07_ow$LEANM_SCHNAM)

# Merge All of Them Into One
ccd_07 <- merge(ccd_07_ai, ccd_07_kn, all = TRUE)
ccd_07 <- merge(ccd_07, ccd_07_ow , all = TRUE)



# Writes to .csv file
write.csv(ccd_07, 'cleaned_data/CCD_200607.csv')


# 2005-06 ------------------------------------------------------------------------------
# Read in .dat for states A-I
ccd_06_ai <- read_table("data/Sc051aai.dat", col_names = FALSE, col_types = NULL)

# SEPARATES COLUMNS
ccd_06_ai <- separate(ccd_06_ai, X1, c("X1.a","X1.b"), sep = -4) # separates STID from NCESSCH
ccd_06_ai <- separate(ccd_06_ai, X5, c("X5.a","X5.b"), sep = 10) # separates PHONE from MSTREE
ccd_06_ai <- separate(ccd_06_ai, X6, c("X6.a","X6.b"), sep = 2) # separates MSTATE from MZIP+MZIP4+LSTREE+LCITY
ccd_06_ai <- separate(ccd_06_ai, X6.b, c("X6.b","X6.c"), sep = 5) # separates MZIP from MZIP4+LSTREE+LCITY
ccd_06_ai <- separate(ccd_06_ai, X7, c("X7.a","X7.b"), sep = 2) # separates LSTATE from LZIP+LZIP4+OTHERS
ccd_06_ai <- separate(ccd_06_ai, X7.b, c("X7.b","X7.c"), sep = 5) # separates LZIP from LZIP4+OTHERS
ccd_06_ai <- separate(ccd_06_ai, X7.c, c("X7.c","X7.e"), sep = -10) # separates the lat coords from OTHERs
ccd_06_ai <- separate(ccd_06_ai, X7.c, c("X7.c","X7.e"), sep = -2) # separates the lat coords from OTHERs
ccd_06_ai <- separate(ccd_06_ai, X8, c("X8.a","X8.c"), sep = 15) # separates the lon coords and CONUM from OTHERs
ccd_06_ai <- separate(ccd_06_ai, X8.a, c("X8.a","X8.b"), sep = -5) # separates CONUM from lon coords
# Turn NA/Missing => -99
ccd_06_ai[ccd_06_ai == -2] <- -99
ccd_06_ai[ccd_06_ai == -1] <- -99
ccd_06_ai[ccd_06_ai == 'M'] <- -99
ccd_06_ai[ccd_06_ai == 'N'] <- -99
# Set COLNAMES
colnames(ccd_06_ai) <- c("NCESSCH", "STID", "SEASCH", "LEANM", "SCHNAM",
                         "PHONE", "MSTREE", "MCITY", "MSTATE","MZIP",
                       "LSTATE","LZIP","LZIP4_TYPE_STATUS", "ULOCAL",
                         "LATCOD","LONCOD","CONUM","CONAME", "OTHERS",
                         'X20', 'X21', 'X22', 'X23', 'X24', 'X25', 'X26', 'X27', 'X28', 'X29', 'X30', 'X31', 'X32', 'X34', 'X35', 'X36', 'X37', 'X38', 'X39', 'X40',
                         'X41', 'X42', 'X43', 'X44', 'X45', 'X46', 'X47', 'X48', 'X49', 'X50', 'X51', 'X52', 'X53', 'X54', 'X55', 'X56', 'X57', 'X58', 'X59', 'X50',
                         'X61', 'X62', 'X63', 'X64', 'X65', 'X66', 'X67', 'X68', 'X69', 'X70', 'X71', 'X72', 'X73', 'X74', 'X75', 'X76', 'X77', 'X78', 'X79', 'X80',
                         'X81', 'X82', 'X83', 'X84', 'X85', 'X86', 'X87', 'X88', 'X89', 'X90', 'X81', 'X82', 'X83', 'X84', 'X95', 'X96', 'X97', 'X98', 'X99', 'X100',
                         'X101', 'X102', 'X103', 'X104', 'X105', 'X106', 'X107', 'X108', 'X109', 'X110', 'X111', 'X112', 'X113', 'X114', 'X115', 'X116', 'X117', 'X118', 'X119', 'X120',
                         'X121', 'X122', 'X123', 'X124', 'X125', 'X126', 'X127', 'X128', 'X129', 'X130', 'X131', 'X132', 'X133', 'X134', 'X135', 'X136', 'X137', 'X138', 'X139', 'X140',
                         'X141', 'X142', 'X143', 'X144', 'X145', 'X146', 'X147', 'X148', 'X159', 'X150', 'X151', 'X152', 'X153', 'X154', 'X155', 'X156', 'X157', 'X158', 'X159', 'X160',
                         'X161', 'X162', 'X163', 'X164', 'X165', 'X166', 'X167', 'X168', 'X169', 'X170', 'X171', 'X172', 'X173', 'X174', 'X175', 'X176', 'X177', 'X178', 'X179', 'X180',
                         'X181', 'X182', 'X183', 'X184', 'X185', 'X186', 'X187', 'X188', 'X189', 'X190', 'X191', 'X192', 'X193', 'X194', 'X195', 'X196', 'X197', 'X198', 'X199', 'X200',
                         'X201', 'X202', 'X203', 'X204', 'X205', 'X206', 'X207', 'X208', 'X209', 'X210', 'X211', 'X212', 'X213', 'X214', 'X215', 'X216', 'X217', 'X218', 'X219', 'X220',
                         'X221', 'X222', 'X223', 'X224', 'X225', 'X226', 'X227', 'X228', 'X229', 'X230', 'X231', 'X232', 'X233', 'X234', 'X235', 'X236', 'X237', 'X238', 'X239', 'X240',
                         'X241', 'X242', 'X243', 'X244', 'X245', 'X246', 'X247', 'X248', 'X249', 'X250', 'X251', 'X252', 'X253', 'X254', 'X255', 'X256', 'X257', 'X258', 'X259', 'X260',
                         'X261', 'X262', 'X263', 'X264', 'X265', 'X266', 'X267', 'X268', 'X269', 'X270', 'X271', 'X272', 'X273', 'X274', 'X275', 'X276', 'X277', 'X278', 'X279', 'X280',
                         'X281')
# Add YEAR column
ccd_06_ai$YEAR <- 2006
# Gets rid of commas
ccd_06_ai$SCHNAM <- gsub(",", " ", ccd_06_ai$SCHNAM)
ccd_06_ai$LEANM <- gsub(",", " ", ccd_06_ai$LEANM)

# Read in .dat for states K-N
ccd_06_kn <- read_table("data/Sc051akn.dat", col_names = FALSE, col_types = NULL)


# SEPARATES COLUMNS
ccd_06_kn <- separate(ccd_06_kn, X1, c("X1.a","X1.b"), sep = -6) # separates STID from NCESSCH
ccd_06_kn <- separate(ccd_06_kn, X5, c("X5.a","X5.b"), sep = 10) # separates PHONE from MSTREE
ccd_06_kn <- separate(ccd_06_kn, X7, c("X7.a","X7.b"), sep = 2) # separates MSTATE from MZIP+MZIP4+LSTREE+LCITY
ccd_06_kn <- separate(ccd_06_kn, X7.b, c("X7.b","X7.c"), sep = 5) # separates MZIP from MZIP4+LSTREE+LCITY
ccd_06_kn <- separate(ccd_06_kn, X9, c("X9.a","X9.b"), sep = 2) # separates LSTATE from LZIP+LZIP4+OTHERS
ccd_06_kn <- separate(ccd_06_kn, X9.b, c("X9.b","X9.c"), sep = 5) # separates 
ccd_06_kn <- separate(ccd_06_kn, X9.c, c("X9.c","X9.e"), sep = -10) # separates 
ccd_06_kn <- separate(ccd_06_kn, X9.c, c("X9.c","X9.d"), sep = -2) # 
ccd_06_kn <- separate(ccd_06_kn, X10, c("X10.a","X10.c"), sep = 15) # separates 
ccd_06_kn <- separate(ccd_06_kn, X10.a, c("X10.a","X10.b"), sep = -5) # separates 
# Turn NA/Missing => -99
ccd_06_kn[ccd_06_kn == -2] <- -99
ccd_06_kn[ccd_06_kn == -1] <- -99
ccd_06_kn[ccd_06_kn == 'M'] <- -99
ccd_06_kn[ccd_06_kn == 'N'] <- -99
# Set COLNAMES
colnames(ccd_06_kn) <- c("NCESSCH", "STID", "SEASCH", "LEANM", "SCHNAM",
                         "PHONE", "MSTREE", "MCITY", "MSTATE","MZIP",
                         "MZIP4_LSTREE", "LCITY", "LSTATE","LZIP","LZIP4_TYPE_STATUS","ULOCAL","LATCOD", "LONCOD","CONUM","CONAME","OTHER",
                         'X20', 'X21', 'X22', 'X23', 'X24', 'X25', 'X26', 'X27', 'X28', 'X29', 'X30', 'X31', 'X32', 'X34', 'X35', 'X36', 'X37', 'X38', 'X39', 'X40',
                         'X41', 'X42', 'X43', 'X44', 'X45', 'X46', 'X47', 'X48', 'X49', 'X50', 'X51', 'X52', 'X53', 'X54', 'X55', 'X56', 'X57', 'X58', 'X59', 'X50',
                         'X61', 'X62', 'X63', 'X64', 'X65', 'X66', 'X67', 'X68', 'X69', 'X70', 'X71', 'X72', 'X73', 'X74', 'X75', 'X76', 'X77', 'X78', 'X79', 'X80',
                         'X81', 'X82', 'X83', 'X84', 'X85', 'X86', 'X87', 'X88', 'X89', 'X90', 'X81', 'X82', 'X83', 'X84', 'X95', 'X96', 'X97', 'X98', 'X99', 'X100',
                         'X101', 'X102', 'X103', 'X104', 'X105', 'X106', 'X107', 'X108', 'X109', 'X110', 'X111', 'X112', 'X113', 'X114', 'X115', 'X116', 'X117', 'X118', 'X119', 'X120',
                         'X121', 'X122', 'X123', 'X124', 'X125', 'X126', 'X127', 'X128', 'X129', 'X130', 'X131', 'X132', 'X133', 'X134', 'X135', 'X136', 'X137', 'X138', 'X139', 'X140',
                         'X141', 'X142', 'X143', 'X144', 'X145', 'X146', 'X147', 'X148', 'X159', 'X150', 'X151', 'X152', 'X153', 'X154', 'X155', 'X156', 'X157', 'X158', 'X159', 'X160',
                         'X161', 'X162', 'X163', 'X164', 'X165', 'X166', 'X167', 'X168', 'X169', 'X170', 'X171', 'X172', 'X173', 'X174', 'X175', 'X176', 'X177', 'X178', 'X179', 'X180',
                         'X181', 'X182', 'X183', 'X184', 'X185', 'X186', 'X187', 'X188', 'X189', 'X190', 'X191', 'X192', 'X193', 'X194', 'X195', 'X196', 'X197', 'X198', 'X199', 'X200',
                         'X201', 'X202', 'X203', 'X204', 'X205', 'X206', 'X207', 'X208', 'X209', 'X210', 'X211', 'X212', 'X213', 'X214', 'X215', 'X216', 'X217', 'X218', 'X219', 'X220',
                         'X221', 'X222', 'X223', 'X224', 'X225', 'X226', 'X227', 'X228', 'X229', 'X230', 'X231', 'X232', 'X233', 'X234', 'X235', 'X236', 'X237', 'X238', 'X239', 'X240',
                         'X241', 'X242', 'X243', 'X244', 'X245', 'X246', 'X247', 'X248', 'X249', 'X250', 'X251', 'X252', 'X253', 'X254', 'X255', 'X256', 'X257', 'X258', 'X259', 'X260',
                         'X261', 'X262', 'X263', 'X264', 'X265', 'X266', 'X267', 'X268', 'X269', 'X270', 'X271', 'X272', 'X273', 'X274', 'X275', 'X276', 'X277', 'X278', 'X279', 'X280',
                         'X281', 'X282')
# Add YEAR column
ccd_06_kn$YEAR <- 2006
# Gets rid of commas
ccd_06_kn$SCHNAM <- gsub(",", " ", ccd_06_kn$SCHNAM)
ccd_06_kn$LEANM <- gsub(",", " ", ccd_06_kn$LEANM)


# Read in .dat for states O-W
ccd_06_ow <- read_table("data/Sc051aow.dat", col_names = FALSE, col_types = NULL)


# SEPARATES COLUMNS
ccd_06_ow <- separate(ccd_06_ow, X1, c("X1.a","X1.b"), sep = -6) # separates STID from NCESSCH
ccd_06_ow <- separate(ccd_06_ow, X4, c("X4.a","X4.b"), sep = 2) # separates MSTATE from MZIP+MZIP4+LSTREE+LCITY
ccd_06_ow <- separate(ccd_06_ow, X4.b, c("X4.b","X4.c"), sep = 5) # separates MZIP from MZIP4+LSTREE+LCITY
ccd_06_ow <- separate(ccd_06_ow, X5, c("X5.a","X5.b"), sep = 2) # separates MSTATE from MZIP+MZIP4+LSTREE+LCITY
ccd_06_ow <- separate(ccd_06_ow, X5.b, c("X5.b","X5.c"), sep = 5) # separates MZIP from MZIP4+LSTREE+LCITY
ccd_06_ow <- separate(ccd_06_ow, X5.c, c("X5.c","X5.e"), sep = -10) # separates the lat coords from OTHERs
ccd_06_ow <- separate(ccd_06_ow, X5.c, c("X5.c","X5.d"), sep = -2) # separates the lat coords from OTHERs
ccd_06_ow <- separate(ccd_06_ow, X6, c("X6.a","X6.c"), sep = 15) # separates the lon coords and CONUM from OTHERs
ccd_06_ow <- separate(ccd_06_ow, X6.a, c("X6.a","X6.b"), sep = -5) # separates CONUM from lon coords
ccd_06_ow <- separate(ccd_06_ow, X8, c("X8.a","X8.b"), sep = 2) # separates CONUM from lon coords
ccd_06_ow <- separate(ccd_06_ow, X8.b, c("X8.b","X8.c"), sep = 2) # separates CONUM from lon coords
# Turn NA/Missing => -99
ccd_06_ow[ccd_06_ow == -2] <- -99
ccd_06_ow[ccd_06_ow == -1] <- -99
ccd_06_ow[ccd_06_ow == 'M'] <- -99
ccd_06_ow[ccd_06_ow == 'N'] <- -99
# Set COLNAMES
colnames(ccd_06_ow) <- c("NCESSCH", "STID", "SEASCH", "LEANM_SCHNAM", 
                         "MSTATE", "MZIP", "MZIP4_LSTREE","LSTATE","LZIP","LZIP4_TYPE_STATUS", "ULOCAL", "LATCOD", "LONCOD", "CONUM", "CONAME", "OTHER",
                         'X20', 'X21', 'X22', 'X23', 'X24', 'X25', 'X26', 'X27', 'X28', 'X29', 'X30', 'X31', 'X32', 'X34', 'X35', 'X36', 'X37', 'X38', 'X39', 'X40',
                         'X41', 'X42', 'X43', 'X44', 'X45', 'X46', 'X47', 'X48', 'X49', 'X50', 'X51', 'X52', 'X53', 'X54', 'X55', 'X56', 'X57', 'X58', 'X59', 'X50',
                         'X61', 'X62', 'X63', 'X64', 'X65', 'X66', 'X67', 'X68', 'X69', 'X70', 'X71', 'X72', 'X73', 'X74', 'X75', 'X76', 'X77', 'X78', 'X79', 'X80',
                         'X81', 'X82', 'X83', 'X84', 'X85', 'X86', 'X87', 'X88', 'X89', 'X90', 'X81', 'X82', 'X83', 'X84', 'X95', 'X96', 'X97', 'X98', 'X99', 'X100',
                         'X101', 'X102', 'X103', 'X104', 'X105', 'X106', 'X107', 'X108', 'X109', 'X110', 'X111', 'X112', 'X113', 'X114', 'X115', 'X116', 'X117', 'X118', 'X119', 'X120',
                         'X121', 'X122', 'X123', 'X124', 'X125', 'X126', 'X127', 'X128', 'X129', 'X130', 'X131', 'X132', 'X133', 'X134', 'X135', 'X136', 'X137', 'X138', 'X139', 'X140',
                         'X141', 'X142', 'X143', 'X144', 'X145', 'X146', 'X147', 'X148', 'X159', 'X150', 'X151', 'X152', 'X153', 'X154', 'X155', 'X156', 'X157', 'X158', 'X159', 'X160',
                         'X161', 'X162', 'X163', 'X164', 'X165', 'X166', 'X167', 'X168', 'X169', 'X170', 'X171', 'X172', 'X173', 'X174', 'X175', 'X176', 'X177', 'X178', 'X179', 'X180',
                         'X181', 'X182', 'X183', 'X184', 'X185', 'X186', 'X187', 'X188', 'X189', 'X190', 'X191', 'X192', 'X193', 'X194', 'X195', 'X196', 'X197', 'X198', 'X199', 'X200',
                         'X201', 'X202', 'X203', 'X204', 'X205', 'X206', 'X207', 'X208', 'X209', 'X210', 'X211', 'X212', 'X213', 'X214', 'X215', 'X216', 'X217', 'X218', 'X219', 'X220',
                         'X221', 'X222', 'X223', 'X224', 'X225', 'X226', 'X227', 'X228', 'X229', 'X230', 'X231', 'X232', 'X233', 'X234', 'X235', 'X236', 'X237', 'X238', 'X239', 'X240',
                         'X241', 'X242', 'X243', 'X244', 'X245', 'X246', 'X247', 'X248', 'X249', 'X250', 'X251', 'X252', 'X253', 'X254', 'X255', 'X256', 'X257', 'X258', 'X259', 'X260',
                         'X261', 'X262', 'X263', 'X264', 'X265', 'X266', 'X267', 'X268', 'X269', 'X270', 'X271', 'X272', 'X273', 'X274', 'X275', 'X276', 'X277')
# Add YEAR column
ccd_06_ow$YEAR <- 2006
# Gets rid of commas
ccd_06_ow$LEANM_SCHNAM <- gsub(",", " ", ccd_06_ow$LEANM_SCHNAM)


# Merge All of Them Into One
ccd_06 <- merge(ccd_06_ai, ccd_06_kn, all = TRUE)
ccd_06 <- merge(ccd_06, ccd_06_ow, all = TRUE)



# Writes to .csv file
# write.csv(ccd_06, 'cleaned_data/CCD_200506.csv')


# 2004-05 ------------------------------------------------------------------------------
# Read in .dat for states A-I
ccd_05_ai <- read_table("data/Sc041bai.dat", col_names = FALSE, col_types = NULL)


# SEPARATES COLUMNS
ccd_05_ai <- separate(ccd_05_ai, X1, c("X1.a","X1.b"), sep = -4) # separates STID from NCESSCH
ccd_05_ai <- separate(ccd_05_ai, X5, c("X5.a","X5.b"), sep = 10) # separates PHONE from MSTREE
ccd_05_ai <- separate(ccd_05_ai, X7, c("X7.a","X7.b"), sep = 2) # separates LSTATE from LZIP+LZIP4+OTHERS
ccd_05_ai <- separate(ccd_05_ai, X7.b, c("X7.b","X7.c"), sep = 5) # separates LZIP from LZIP4+OTHERS
ccd_05_ai <- separate(ccd_05_ai, X8, c("X8.a","X8.b"), sep = 2) # separates LSTATE from LZIP+LZIP4+OTHERS
ccd_05_ai <- separate(ccd_05_ai, X8.b, c("X8.b","X8.c"), sep = 5) # separates LZIP from LZIP4+OTHERS
ccd_05_ai <- separate(ccd_05_ai, X8.c, c("X8.c","X8.d"), sep = -2) # separates the lat coords from OTHERs
# Turn NA/Missing => -99
ccd_05_ai[ccd_05_ai == -2] <- -99
ccd_05_ai[ccd_05_ai == -1] <- -99
ccd_05_ai[ccd_05_ai == 'M'] <- -99
ccd_05_ai[ccd_05_ai == 'N'] <- -99
# Set COLNAMES
colnames(ccd_05_ai) <- c("NCESSCH", "STID", "SEASCH", "LEANM", "SCHNAM",
                         "PHONE", "MSTREE", "MCITY", "MSTATE","MZIP", "MZIP4_LSTREE",
                         "LSTATE","LZIP","LZIP4_TYPE_STATUS", "ULOCAL",
                         "LATCOD_LONCOD_CONUM_CONAME", "OTHERS",
                         'X20', 'X21', 'X22', 'X23', 'X24', 'X25', 'X26', 'X27', 'X28', 'X29', 'X30', 'X31', 'X32', 'X34', 'X35', 'X36', 'X37', 'X38', 'X39', 'X40',
                         'X41', 'X42', 'X43', 'X44', 'X45', 'X46', 'X47', 'X48', 'X49', 'X50', 'X51', 'X52', 'X53', 'X54', 'X55', 'X56', 'X57', 'X58', 'X59', 'X50',
                         'X61', 'X62', 'X63', 'X64', 'X65', 'X66', 'X67', 'X68', 'X69', 'X70', 'X71', 'X72', 'X73', 'X74', 'X75', 'X76', 'X77', 'X78', 'X79', 'X80',
                         'X81', 'X82', 'X83', 'X84', 'X85', 'X86', 'X87', 'X88', 'X89', 'X90', 'X81', 'X82', 'X83', 'X84', 'X95', 'X96', 'X97', 'X98', 'X99', 'X100',
                         'X101', 'X102', 'X103', 'X104', 'X105', 'X106', 'X107', 'X108', 'X109', 'X110', 'X111', 'X112', 'X113', 'X114', 'X115', 'X116', 'X117', 'X118', 'X119', 'X120',
                         'X121', 'X122', 'X123', 'X124', 'X125', 'X126', 'X127', 'X128', 'X129', 'X130', 'X131', 'X132', 'X133', 'X134', 'X135', 'X136', 'X137', 'X138', 'X139', 'X140',
                         'X141', 'X142', 'X143', 'X144', 'X145', 'X146', 'X147', 'X148', 'X159', 'X150', 'X151', 'X152', 'X153', 'X154', 'X155', 'X156', 'X157', 'X158', 'X159', 'X160',
                         'X161', 'X162', 'X163', 'X164', 'X165', 'X166', 'X167', 'X168', 'X169', 'X170', 'X171', 'X172', 'X173', 'X174', 'X175', 'X176', 'X177', 'X178', 'X179', 'X180',
                         'X181', 'X182', 'X183', 'X184', 'X185', 'X186', 'X187', 'X188', 'X189', 'X190', 'X191', 'X192', 'X193', 'X194', 'X195', 'X196', 'X197', 'X198', 'X199', 'X200',
                         'X201', 'X202', 'X203', 'X204', 'X205', 'X206', 'X207', 'X208', 'X209', 'X210', 'X211', 'X212', 'X213', 'X214', 'X215', 'X216', 'X217', 'X218', 'X219', 'X220',
                         'X221', 'X222', 'X223', 'X224', 'X225', 'X226', 'X227', 'X228', 'X229', 'X230', 'X231', 'X232', 'X233', 'X234', 'X235', 'X236', 'X237', 'X238', 'X239', 'X240',
                         'X241', 'X242', 'X243', 'X244', 'X245', 'X246', 'X247', 'X248', 'X249', 'X250', 'X251', 'X252', 'X253', 'X254', 'X255', 'X256', 'X257', 'X258', 'X259', 'X260',
                         'X261', 'X262', 'X263', 'X264', 'X265', 'X266', 'X267', 'X268', 'X269', 'X270', 'X271', 'X272', 'X273', 'X274', 'X275', 'X276', 'X277', 'X278', 'X279', 'X280',
                         'X281')
# Add YEAR column
ccd_05_ai$YEAR <- 2005
# Gets rid of commas
ccd_05_ai$SCHNAM <- gsub(",", " ", ccd_05_ai$SCHNAM)
ccd_05_ai$LEANM <- gsub(",", " ", ccd_05_ai$LEANM)


# Read in .dat for states K-N
ccd_05_kn <- read_table("data/Sc041bkn.dat", col_names = FALSE, col_types = NULL)

# SEPARATES COLUMNS
ccd_05_kn <- separate(ccd_05_kn, X1, c("X1.a","X1.b"), sep = -6) # separates STID from NCESSCH
ccd_05_kn <- separate(ccd_05_kn, X5, c("X5.a","X5.b"), sep = 10) # separates PHONE from MSTREE
ccd_05_kn <- separate(ccd_05_kn, X6, c("X6.a","X6.b"), sep = 2) # separates MSTATE from MZIP+MZIP4+LSTREE+LCITY
ccd_05_kn <- separate(ccd_05_kn, X6.b, c("X6.b","X6.c"), sep = 5) # separates MSTATE from MZIP+MZIP4+LSTREE+LCIT
ccd_05_kn <- separate(ccd_05_kn, X7, c("X7.a","X7.b"), sep = 2) # separates MSTATE from MZIP+MZIP4+LSTREE+LCITY
ccd_05_kn <- separate(ccd_05_kn, X7.b, c("X7.b","X7.c"), sep = 5) # separates MZIP from MZIP4+LSTREE+LCITY
ccd_05_kn <- separate(ccd_05_kn, X7.c, c("X7.c","X7.d"), sep = -2) # separates MSTATE from MZIP+MZIP4+LSTREE+LCITY
ccd_05_kn <- separate(ccd_05_kn, X8, c("X8.a","X8.b"), sep = 9) # separates 
ccd_05_kn <- separate(ccd_05_kn, X8.b, c("X8.b","X8.d"), sep = 15) # separates 
ccd_05_kn <- separate(ccd_05_kn, X8.b, c("X8.b","X8.c"), sep = -5) # separates 
# Turn NA/Missing => -99
ccd_05_kn[ccd_05_kn == -2] <- -99
ccd_05_kn[ccd_05_kn == -1] <- -99
ccd_05_kn[ccd_05_kn == 'M'] <- -99
ccd_05_kn[ccd_05_kn == 'N'] <- -99
# Set COLNAMES
colnames(ccd_05_kn) <- c("NCESSCH", "STID", "SEASCH", "LEANM", "SCHNAM",
                         "PHONE", "MSTREE_MCITY", "MSTATE","MZIP",
                         "MZIP4_LSTREE_LCITY", "LSTATE","LZIP","LZIP4_TYPE_STATUS","ULOCAL","LATCOD", "LONCOD","CONUM","CONAME","OTHER",
                         'X20', 'X21', 'X22', 'X23', 'X24', 'X25', 'X26', 'X27', 'X28', 'X29', 'X30', 'X31', 'X32', 'X34', 'X35', 'X36', 'X37', 'X38', 'X39', 'X40',
                         'X41', 'X42', 'X43', 'X44', 'X45', 'X46', 'X47', 'X48', 'X49', 'X50', 'X51', 'X52', 'X53', 'X54', 'X55', 'X56', 'X57', 'X58', 'X59', 'X50',
                         'X61', 'X62', 'X63', 'X64', 'X65', 'X66', 'X67', 'X68', 'X69', 'X70', 'X71', 'X72', 'X73', 'X74', 'X75', 'X76', 'X77', 'X78', 'X79', 'X80',
                         'X81', 'X82', 'X83', 'X84', 'X85', 'X86', 'X87', 'X88', 'X89', 'X90', 'X81', 'X82', 'X83', 'X84', 'X95', 'X96', 'X97', 'X98', 'X99', 'X100',
                         'X101', 'X102', 'X103', 'X104', 'X105', 'X106', 'X107', 'X108', 'X109', 'X110', 'X111', 'X112', 'X113', 'X114', 'X115', 'X116', 'X117', 'X118', 'X119', 'X120',
                         'X121', 'X122', 'X123', 'X124', 'X125', 'X126', 'X127', 'X128', 'X129', 'X130', 'X131', 'X132', 'X133', 'X134', 'X135', 'X136', 'X137', 'X138', 'X139', 'X140',
                         'X141', 'X142', 'X143', 'X144', 'X145', 'X146', 'X147', 'X148', 'X159', 'X150', 'X151', 'X152', 'X153', 'X154', 'X155', 'X156', 'X157', 'X158', 'X159', 'X160',
                         'X161', 'X162', 'X163', 'X164', 'X165', 'X166', 'X167', 'X168', 'X169', 'X170', 'X171', 'X172', 'X173', 'X174', 'X175', 'X176', 'X177', 'X178', 'X179', 'X180',
                         'X181', 'X182', 'X183', 'X184', 'X185', 'X186', 'X187', 'X188', 'X189', 'X190', 'X191', 'X192', 'X193', 'X194', 'X195', 'X196', 'X197', 'X198', 'X199', 'X200',
                         'X201', 'X202', 'X203', 'X204', 'X205', 'X206', 'X207', 'X208', 'X209', 'X210', 'X211', 'X212', 'X213', 'X214', 'X215', 'X216', 'X217', 'X218', 'X219', 'X220',
                         'X221', 'X222', 'X223', 'X224', 'X225', 'X226', 'X227', 'X228', 'X229', 'X230', 'X231', 'X232', 'X233', 'X234', 'X235', 'X236', 'X237', 'X238', 'X239', 'X240',
                         'X241', 'X242', 'X243', 'X244', 'X245', 'X246', 'X247', 'X248', 'X249', 'X250', 'X251', 'X252', 'X253', 'X254', 'X255', 'X256', 'X257', 'X258', 'X259', 'X260',
                         'X261', 'X262', 'X263', 'X264', 'X265', 'X266', 'X267', 'X268', 'X269', 'X270', 'X271', 'X272', 'X273', 'X274', 'X275', 'X276', 'X277', 'X278', 'X279', 'X280')
# Add YEAR column
ccd_05_kn$YEAR <- 2005
# Gets rid of commas
ccd_05_kn$SCHNAM <- gsub(",", " ", ccd_05_kn$SCHNAM)
ccd_05_kn$LEANM <- gsub(",", " ", ccd_05_kn$LEANM)

# Read in .dat for states O-W
ccd_05_ow <- read_table("data/Sc041bow.dat", col_names = FALSE, col_types = NULL)


# SEPARATES COLUMNS
ccd_05_ow <- separate(ccd_05_ow, X1, c("X1.a","X1.b"), sep = -6) # separates STID from NCESSCH
ccd_05_ow <- separate(ccd_05_ow, X5, c("X5.a","X5.b"), sep = 2) # separates MSTATE from MZIP+MZIP4+LSTREE+LCITY
ccd_05_ow <- separate(ccd_05_ow, X5.b, c("X5.b","X5.c"), sep = 5) # separates MZIP from MZIP4+LSTREE+LCITY
ccd_05_ow <- separate(ccd_05_ow, X6, c("X6.a","X6.b"), sep = 2) # separates MSTATE from MZIP+MZIP4+LSTREE+LCITY
ccd_05_ow <- separate(ccd_05_ow, X6.b, c("X6.b","X6.c"), sep = 5) # separates MZIP from MZIP4+LSTREE+LCITY
ccd_05_ow <- separate(ccd_05_ow, X6.c, c("X6.c","X6.d"), sep = -2) # separates MZIP from MZIP4+LSTREE+LCITY
ccd_05_ow <- separate(ccd_05_ow, X7, c("X7.a","X7.b"), sep = 9) # separates 
ccd_05_ow <- separate(ccd_05_ow, X7.b, c("X7.b","X7.d"), sep = 15) # separates 
ccd_05_ow <- separate(ccd_05_ow, X7.b, c("X7.b","X7.c"), sep = -5) # separates 
# Turn NA/Missing => -99
ccd_05_ow[ccd_05_ow == -2] <- -99
ccd_05_ow[ccd_05_ow == -1] <- -99
ccd_05_ow[ccd_05_ow == 'M'] <- -99
ccd_05_ow[ccd_05_ow == 'N'] <- -99
# Set COLNAMES
colnames(ccd_05_ow) <- c("NCESSCH", "STID", "SEASCH", "LEANM", "SCHNAM_PHONE_MSTREE_MCITY", 
                         "MSTATE", "MZIP", "MZIP4_LSTREE","LSTATE","LZIP","LZIP4_TYPE_STATUS", "ULOCAL", "LATCOD", "LONCOD", "CONUM", "CONAME", "OTHER",
                         'X20', 'X21', 'X22', 'X23', 'X24', 'X25', 'X26', 'X27', 'X28', 'X29', 'X30', 'X31', 'X32', 'X34', 'X35', 'X36', 'X37', 'X38', 'X39', 'X40',
                         'X41', 'X42', 'X43', 'X44', 'X45', 'X46', 'X47', 'X48', 'X49', 'X50', 'X51', 'X52', 'X53', 'X54', 'X55', 'X56', 'X57', 'X58', 'X59', 'X50',
                         'X61', 'X62', 'X63', 'X64', 'X65', 'X66', 'X67', 'X68', 'X69', 'X70', 'X71', 'X72', 'X73', 'X74', 'X75', 'X76', 'X77', 'X78', 'X79', 'X80',
                         'X81', 'X82', 'X83', 'X84', 'X85', 'X86', 'X87', 'X88', 'X89', 'X90', 'X81', 'X82', 'X83', 'X84', 'X95', 'X96', 'X97', 'X98', 'X99', 'X100',
                         'X101', 'X102', 'X103', 'X104', 'X105', 'X106', 'X107', 'X108', 'X109', 'X110', 'X111', 'X112', 'X113', 'X114', 'X115', 'X116', 'X117', 'X118', 'X119', 'X120',
                         'X121', 'X122', 'X123', 'X124', 'X125', 'X126', 'X127', 'X128', 'X129', 'X130', 'X131', 'X132', 'X133', 'X134', 'X135', 'X136', 'X137', 'X138', 'X139', 'X140',
                         'X141', 'X142', 'X143', 'X144', 'X145', 'X146', 'X147', 'X148', 'X159', 'X150', 'X151', 'X152', 'X153', 'X154', 'X155', 'X156', 'X157', 'X158', 'X159', 'X160',
                         'X161', 'X162', 'X163', 'X164', 'X165', 'X166', 'X167', 'X168', 'X169', 'X170', 'X171', 'X172', 'X173', 'X174', 'X175', 'X176', 'X177', 'X178', 'X179', 'X180',
                         'X181', 'X182', 'X183', 'X184', 'X185', 'X186', 'X187', 'X188', 'X189', 'X190', 'X191', 'X192', 'X193', 'X194', 'X195', 'X196', 'X197', 'X198', 'X199', 'X200',
                         'X201', 'X202', 'X203', 'X204', 'X205', 'X206', 'X207', 'X208', 'X209', 'X210', 'X211', 'X212', 'X213', 'X214', 'X215', 'X216', 'X217', 'X218', 'X219', 'X220',
                         'X221', 'X222', 'X223', 'X224', 'X225', 'X226', 'X227', 'X228', 'X229', 'X230', 'X231', 'X232', 'X233', 'X234', 'X235', 'X236', 'X237', 'X238', 'X239', 'X240',
                         'X241', 'X242', 'X243', 'X244', 'X245', 'X246', 'X247', 'X248', 'X249', 'X250', 'X251', 'X252', 'X253', 'X254', 'X255', 'X256', 'X257', 'X258', 'X259', 'X260',
                         'X261', 'X262', 'X263', 'X264', 'X265', 'X266', 'X267', 'X268', 'X269', 'X270', 'X271', 'X272', 'X273', 'X274', 'X275', 'X276')
# Add YEAR column
ccd_05_ow$YEAR <- 2005
# Gets rid of commas
ccd_05_ow$LEANM <- gsub(",", " ", ccd_05_ow$LEANM)
ccd_05_ow$SCHNAM_PHONE_MSTREE <- gsub(",", " ", ccd_05_ow$SCHNAM_PHONE_MSTREE_MCITY)

# Merge All of Them Into One
ccd_05 <- merge(ccd_05_ai, ccd_05_kn, all = TRUE)
ccd_05 <- merge(ccd_05, ccd_05_ow, all = TRUE)



# Writes to .csv file
# write.csv(ccd_05, 'cleaned_data/CCD_200405.csv')


# 2003-04 ------------------------------------------------------------------------------
# Read in .txt for states A-I
ccd_04_ai <- read_table("data/Sc031aai.txt", col_names = FALSE, col_types = NULL)


# SEPARATES COLUMNS
ccd_04_ai <- separate(ccd_04_ai, X1, c("X1.a","X1.b"), sep = 12) # separates STID from NCESSCH
ccd_04_ai <- separate(ccd_04_ai, X4, c("X4.a","X4.b"), sep = 10) # separates PHONE from MSTREE
ccd_04_ai <- separate(ccd_04_ai, X5, c("X5.a","X5.b"), sep = 2) # separates LSTATE from LZIP+LZIP4+OTHERS
ccd_04_ai <- separate(ccd_04_ai, X5.b, c("X5.b","X5.c"), sep = 5) # separates LZIP from LZIP4+OTHERS
ccd_04_ai <- separate(ccd_04_ai, X6, c("X6.a","X6.b"), sep = 2) # separates LSTATE from LZIP+LZIP4+OTHERS
ccd_04_ai <- separate(ccd_04_ai, X6.b, c("X6.b","X6.c"), sep = 5) # separates
ccd_04_ai <- separate(ccd_04_ai, X6.c, c("X6.c","X6.d"), sep = -2) # separates
ccd_04_ai <- separate(ccd_04_ai, X7, c("X7.a","X7.b"), sep = 9) # separates 
ccd_04_ai <- separate(ccd_04_ai, X7.b, c("X7.b","X7.d"), sep = 15) # separates 
ccd_04_ai <- separate(ccd_04_ai, X7.b, c("X7.b","X7.c"), sep = -5) # separates 
ccd_04_ai <- separate(ccd_04_ai, X9, c("X9.a","X9.b"), sep = 3) # separates 
# Turn NA/Missing => -99
ccd_04_ai[ccd_04_ai == -2] <- -99
ccd_04_ai[ccd_04_ai == -1] <- -99
ccd_04_ai[ccd_04_ai == 'M'] <- -99
ccd_04_ai[ccd_04_ai == 'N'] <- -99
# Set COLNAMES
colnames(ccd_04_ai) <- c("NCESSCH", "STID_SEASCH", "LEANM", "SCHNAM",
                         "PHONE", "MSTREE_MCITY", "MSTATE","MZIP", "MZIP4_LSTREE",
                         "LSTATE","LZIP","LZIP4_TYPE_STATUS", "ULOCAL",
                         "LATCOD", "LONCOD", "CONUM", "CONAME", "OTHERS",
                         'X20', 'X21', 'X22', 'X23', 'X24', 'X25', 'X26', 'X27', 'X28', 'X29', 'X30', 'X31', 'X32', 'X34', 'X35', 'X36', 'X37', 'X38', 'X39', 'X40',
                         'X41', 'X42', 'X43', 'X44', 'X45', 'X46', 'X47', 'X48', 'X49', 'X50', 'X51', 'X52', 'X53', 'X54', 'X55', 'X56', 'X57', 'X58', 'X59', 'X50',
                         'X61', 'X62', 'X63', 'X64', 'X65', 'X66', 'X67', 'X68', 'X69', 'X70', 'X71', 'X72', 'X73', 'X74', 'X75', 'X76', 'X77', 'X78', 'X79', 'X80',
                         'X81', 'X82', 'X83', 'X84', 'X85', 'X86', 'X87', 'X88', 'X89', 'X90', 'X81', 'X82', 'X83', 'X84', 'X95', 'X96', 'X97', 'X98', 'X99', 'X100',
                         'X101', 'X102', 'X103', 'X104', 'X105', 'X106', 'X107', 'X108', 'X109', 'X110', 'X111', 'X112', 'X113', 'X114', 'X115', 'X116', 'X117', 'X118', 'X119', 'X120',
                         'X121', 'X122', 'X123', 'X124', 'X125', 'X126', 'X127', 'X128', 'X129', 'X130', 'X131', 'X132', 'X133', 'X134', 'X135', 'X136', 'X137', 'X138', 'X139', 'X140',
                         'X141', 'X142', 'X143', 'X144', 'X145', 'X146', 'X147', 'X148', 'X159', 'X150', 'X151', 'X152', 'X153', 'X154', 'X155', 'X156', 'X157', 'X158', 'X159', 'X160',
                         'X161', 'X162', 'X163', 'X164', 'X165', 'X166', 'X167', 'X168', 'X169', 'X170', 'X171', 'X172', 'X173', 'X174', 'X175', 'X176', 'X177', 'X178', 'X179', 'X180',
                         'X181', 'X182', 'X183', 'X184', 'X185', 'X186', 'X187', 'X188', 'X189', 'X190', 'X191', 'X192', 'X193', 'X194', 'X195', 'X196', 'X197', 'X198', 'X199', 'X200',
                         'X201', 'X202', 'X203', 'X204', 'X205', 'X206', 'X207', 'X208', 'X209', 'X210', 'X211', 'X212', 'X213', 'X214', 'X215', 'X216', 'X217', 'X218', 'X219', 'X220',
                         'X221', 'X222', 'X223', 'X224', 'X225', 'X226', 'X227', 'X228', 'X229', 'X230', 'X231', 'X232', 'X233', 'X234', 'X235', 'X236', 'X237', 'X238', 'X239', 'X240',
                         'X241', 'X242', 'X243', 'X244', 'X245', 'X246', 'X247', 'X248', 'X249', 'X250', 'X251', 'X252', 'X253', 'X254', 'X255', 'X256', 'X257', 'X258', 'X259', 'X260',
                         'X261', 'X262', 'X263', 'X264', 'X265', 'X266', 'X267', 'X268', 'X269', 'X270', 'X271', 'X272', 'X273', 'X274', 'X275', 'X276', 'X277', 'X278')
# Add YEAR column
ccd_04_ai$YEAR <- 2004
# Gets rid of commas
ccd_04_ai$SCHNAM <- gsub(",", " ", ccd_04_ai$SCHNAM)
ccd_04_ai$LEANM <- gsub(",", " ", ccd_04_ai$LEANM)



# Read in .txt for states K-N
ccd_04_kn <- read_table("data/Sc031akn.txt", col_names = FALSE, col_types = NULL)


# SEPARATES COLUMNS
ccd_04_kn <- separate(ccd_04_kn, X1, c("X1.a","X1.b"), sep = -6) # separates STID from NCESSCH
ccd_04_kn <- separate(ccd_04_kn, X5, c("X5.a","X5.b"), sep = 10) # separates PHONE from MSTREE
ccd_04_kn <- separate(ccd_04_kn, X7, c("X7.a","X7.b"), sep = 2) # separates MSTATE from MZIP+MZIP4+LSTREE+LCITY
ccd_04_kn <- separate(ccd_04_kn, X7.b, c("X7.b","X7.c"), sep = 5) # separates 
ccd_04_kn <- separate(ccd_04_kn, X9, c("X9.a","X9.b"), sep = 2) # separates MSTATE from MZIP+MZIP4+LSTREE+LCITY
ccd_04_kn <- separate(ccd_04_kn, X9.b, c("X9.b","X9.c"), sep = 5) # separates 
ccd_04_kn <- separate(ccd_04_kn, X9.c, c("X9.c","X9.d"), sep = -2) # separates 
ccd_04_kn <- separate(ccd_04_kn, X10, c("X10.a","X10.b"), sep = 9) # separates 
ccd_04_kn <- separate(ccd_04_kn, X10.b, c("X10.b","X10.d"), sep = 15) # separates 
ccd_04_kn <- separate(ccd_04_kn, X10.b, c("X10.b","X10.c"), sep = -5) # separates 
ccd_04_kn <- separate(ccd_04_kn, X13, c("X13.a","X13.b"), sep = 3) # separates 
# Turn NA/Missing => -99
ccd_04_kn[ccd_04_kn == -2] <- -99
ccd_04_kn[ccd_04_kn == -1] <- -99
ccd_04_kn[ccd_04_kn == 'M'] <- -99
ccd_04_kn[ccd_04_kn == 'N'] <- -99
# Set COLNAMES
colnames(ccd_04_kn) <- c("NCESSCH", "STID", "SEASCH", "LEANM", "SCHNAM",
                         "PHONE", "MSTREE", "MCITY", "MSTATE","MZIP",
                         "MZIP4_LSTREE", "LCITY", "LSTATE","LZIP","LZIP4_TYPE_STATUS","ULOCAL","LATCOD", "LONCOD", "CONUM", "CONAME", "OTHER",
                         'X20', 'X21', 'X22', 'X23', 'X24', 'X25', 'X26', 'X27', 'X28', 'X29', 'X30', 'X31', 'X32', 'X34', 'X35', 'X36', 'X37', 'X38', 'X39', 'X40',
                         'X41', 'X42', 'X43', 'X44', 'X45', 'X46', 'X47', 'X48', 'X49', 'X50', 'X51', 'X52', 'X53', 'X54', 'X55', 'X56', 'X57', 'X58', 'X59', 'X50',
                         'X61', 'X62', 'X63', 'X64', 'X65', 'X66', 'X67', 'X68', 'X69', 'X70', 'X71', 'X72', 'X73', 'X74', 'X75', 'X76', 'X77', 'X78', 'X79', 'X80',
                         'X81', 'X82', 'X83', 'X84', 'X85', 'X86', 'X87', 'X88', 'X89', 'X90', 'X81', 'X82', 'X83', 'X84', 'X95', 'X96', 'X97', 'X98', 'X99', 'X100',
                         'X101', 'X102', 'X103', 'X104', 'X105', 'X106', 'X107', 'X108', 'X109', 'X110', 'X111', 'X112', 'X113', 'X114', 'X115', 'X116', 'X117', 'X118', 'X119', 'X120',
                         'X121', 'X122', 'X123', 'X124', 'X125', 'X126', 'X127', 'X128', 'X129', 'X130', 'X131', 'X132', 'X133', 'X134', 'X135', 'X136', 'X137', 'X138', 'X139', 'X140',
                         'X141', 'X142', 'X143', 'X144', 'X145', 'X146', 'X147', 'X148', 'X159', 'X150', 'X151', 'X152', 'X153', 'X154', 'X155', 'X156', 'X157', 'X158', 'X159', 'X160',
                         'X161', 'X162', 'X163', 'X164', 'X165', 'X166', 'X167', 'X168', 'X169', 'X170', 'X171', 'X172', 'X173', 'X174', 'X175', 'X176', 'X177', 'X178', 'X179', 'X180',
                         'X181', 'X182', 'X183', 'X184', 'X185', 'X186', 'X187', 'X188', 'X189', 'X190', 'X191', 'X192', 'X193', 'X194', 'X195', 'X196', 'X197', 'X198', 'X199', 'X200',
                         'X201', 'X202', 'X203', 'X204', 'X205', 'X206', 'X207', 'X208', 'X209', 'X210', 'X211', 'X212', 'X213', 'X214', 'X215', 'X216', 'X217', 'X218', 'X219', 'X220',
                         'X221', 'X222', 'X223', 'X224', 'X225', 'X226', 'X227', 'X228', 'X229', 'X230', 'X231', 'X232', 'X233', 'X234', 'X235', 'X236', 'X237', 'X238', 'X239', 'X240',
                         'X241', 'X242', 'X243', 'X244', 'X245', 'X246', 'X247', 'X248', 'X249', 'X250', 'X251', 'X252', 'X253', 'X254', 'X255', 'X256', 'X257', 'X258', 'X259', 'X260',
                         'X261', 'X262', 'X263', 'X264', 'X265', 'X266', 'X267', 'X268', 'X269', 'X270', 'X271', 'X272', 'X273', 'X274', 'X275', 'X276', 'X277', 'X278', 'X279', 'X280')
# Add YEAR column
ccd_04_kn$YEAR <- 2004
# Gets rid of commas
ccd_04_kn$SCHNAM <- gsub(",", " ", ccd_04_kn$SCHNAM)
ccd_04_kn$LEANM <- gsub(",", " ", ccd_04_kn$LEANM)


# Read in .txt for states O-W
ccd_04_ow <- read_table("data/Sc031aow.txt", col_names = FALSE, col_types = NULL)


# Gets rid of column X4
ccd_04_ow$X4 <- NULL
# SEPARATES COLUMNS
ccd_04_ow <- separate(ccd_04_ow, X1, c("X1.a","X1.b"), sep = -4) # separates STID from NCESSCH
ccd_04_ow <- separate(ccd_04_ow, X6, c("X6.a","X6.b"), sep = 2) # separates MSTATE from MZIP+MZIP4+LSTREE+LCITY
ccd_04_ow <- separate(ccd_04_ow, X6.b, c("X6.b","X6.c"), sep = 5) # separates MZIP from MZIP4+LSTREE+LCITY
ccd_04_ow <- separate(ccd_04_ow, X7, c("X7.a","X7.b"), sep = 2) # separates MSTATE from MZIP+MZIP4+LSTREE+LCITY
ccd_04_ow <- separate(ccd_04_ow, X7.b, c("X7.b","X7.c"), sep = 5) # s
ccd_04_ow <- separate(ccd_04_ow, X7.c, c("X7.c","X7.d"), sep = -2) # s
ccd_04_ow <- separate(ccd_04_ow, X8, c("X8.a","X8.b"), sep = 9) # separates 
ccd_04_ow <- separate(ccd_04_ow, X8.b, c("X8.b","X8.d"), sep = 15) # separates 
ccd_04_ow <- separate(ccd_04_ow, X8.b, c("X8.b","X8.c"), sep = -5) # separates 
# Turn NA/Missing => -99
ccd_04_ow[ccd_04_ow == -2] <- -99
ccd_04_ow[ccd_04_ow == -1] <- -99
ccd_04_ow[ccd_04_ow == 'M'] <- -99
ccd_04_ow[ccd_04_ow == 'N'] <- -99
# Set COLNAMES
colnames(ccd_04_ow) <- c("NCESSCH", "STID", "SEASCH", "LEANM", "SCHNAM_PHONE_MSTREE_MCITY", 
                         "MSTATE", "MZIP", "MZIP4_LSTREE","LSTATE","LZIP","LZIP4_TYPE_STATUS", "ULOCAL", "LATCOD", "LONCOD", "CONUM", "CONAME", "OTHER",
                         'X20', 'X21', 'X22', 'X23', 'X24', 'X25', 'X26', 'X27', 'X28', 'X29', 'X30', 'X31', 'X32', 'X34', 'X35', 'X36', 'X37', 'X38', 'X39', 'X40',
                         'X41', 'X42', 'X43', 'X44', 'X45', 'X46', 'X47', 'X48', 'X49', 'X50', 'X51', 'X52', 'X53', 'X54', 'X55', 'X56', 'X57', 'X58', 'X59', 'X50',
                         'X61', 'X62', 'X63', 'X64', 'X65', 'X66', 'X67', 'X68', 'X69', 'X70', 'X71', 'X72', 'X73', 'X74', 'X75', 'X76', 'X77', 'X78', 'X79', 'X80',
                         'X81', 'X82', 'X83', 'X84', 'X85', 'X86', 'X87', 'X88', 'X89', 'X90', 'X81', 'X82', 'X83', 'X84', 'X95', 'X96', 'X97', 'X98', 'X99', 'X100',
                         'X101', 'X102', 'X103', 'X104', 'X105', 'X106', 'X107', 'X108', 'X109', 'X110', 'X111', 'X112', 'X113', 'X114', 'X115', 'X116', 'X117', 'X118', 'X119', 'X120',
                         'X121', 'X122', 'X123', 'X124', 'X125', 'X126', 'X127', 'X128', 'X129', 'X130', 'X131', 'X132', 'X133', 'X134', 'X135', 'X136', 'X137', 'X138', 'X139', 'X140',
                         'X141', 'X142', 'X143', 'X144', 'X145', 'X146', 'X147', 'X148', 'X159', 'X150', 'X151', 'X152', 'X153', 'X154', 'X155', 'X156', 'X157', 'X158', 'X159', 'X160',
                         'X161', 'X162', 'X163', 'X164', 'X165', 'X166', 'X167', 'X168', 'X169', 'X170', 'X171', 'X172', 'X173', 'X174', 'X175', 'X176', 'X177', 'X178', 'X179', 'X180',
                         'X181', 'X182', 'X183', 'X184', 'X185', 'X186', 'X187', 'X188', 'X189', 'X190', 'X191', 'X192', 'X193', 'X194', 'X195', 'X196', 'X197', 'X198', 'X199', 'X200',
                         'X201', 'X202', 'X203', 'X204', 'X205', 'X206', 'X207', 'X208', 'X209', 'X210', 'X211', 'X212', 'X213', 'X214', 'X215', 'X216', 'X217', 'X218', 'X219', 'X220',
                         'X221', 'X222', 'X223', 'X224', 'X225', 'X226', 'X227', 'X228', 'X229', 'X230', 'X231', 'X232', 'X233', 'X234', 'X235', 'X236', 'X237', 'X238', 'X239', 'X240',
                         'X241', 'X242', 'X243', 'X244', 'X245', 'X246', 'X247', 'X248', 'X249', 'X250', 'X251', 'X252', 'X253', 'X254', 'X255', 'X256', 'X257', 'X258', 'X259', 'X260',
                         'X261', 'X262', 'X263', 'X264', 'X265', 'X266', 'X267', 'X268', 'X269', 'X270', 'X271', 'X272', 'X273', 'X274', 'X275')
# Add YEAR column
ccd_04_ow$YEAR <- 2004
# Gets rid of commas
ccd_04_ow$SCHNAM_PHONE_MSTREE_MCITY <- gsub(",", " ", ccd_04_ow$SCHNAM_PHONE_MSTREE_MCITY)
ccd_04_ow$LEANM <- gsub(",", " ", ccd_04_ow$LEANM)



# Merge All of Them Into One
ccd_04 <- merge(ccd_04_ai, ccd_04_kn, all = TRUE)
ccd_04 <- merge(ccd_04, ccd_04_ow, all = TRUE)


# Writes to .csv file
# write.csv(ccd_04, 'cleaned_data/CCD_200304.csv')


# 2002-03 ------------------------------------------------------------------------------
# Read in .txt for states A-I
ccd_03_ai <- read_table("data/Sc021aai.txt", col_names = FALSE, col_types = NULL)

# Gets rid of column X4
ccd_03_ai$X4 <- NULL
# SEPARATES COLUMNS
ccd_03_ai <- separate(ccd_03_ai, X1, c("X1.a","X1.b"), sep = -4) # separates STID from NCESSCH
ccd_03_ai <- separate(ccd_03_ai, X6, c("X6.a","X6.b"), sep = 10) # separates PHONE from MSTREE
ccd_03_ai <- separate(ccd_03_ai, X7, c("X7.a","X7.b"), sep = 2) # separates MSTATE from MZIP+MZIP4+LSTREE+LCITY
ccd_03_ai <- separate(ccd_03_ai, X7.b, c("X7.b","X7.c"), sep = 6) # separates MZIP from MZIP4+LSTREE+LCITY
ccd_03_ai <- separate(ccd_03_ai, X8, c("X8.a","X8.b"), sep = 2) # separates LSTATE from LZIP+LZIP4+OTHERS
ccd_03_ai <- separate(ccd_03_ai, X8.b, c("X8.b","X8.c"), sep = 5) # separates LZIP from LZIP4+OTHERS
ccd_03_ai <- separate(ccd_03_ai, X8.c, c("X8.c","X8.d"), sep = -2) # separates something
# Turn NA/Missing => -99
ccd_03_ai[ccd_03_ai == -2] <- -99
ccd_03_ai[ccd_03_ai == -1] <- -99
ccd_03_ai[ccd_03_ai == 'M'] <- -99
ccd_03_ai[ccd_03_ai == 'N'] <- -99
# Set COLNAMES
colnames(ccd_03_ai) <- c('NCESSCH', 'STID', 'SEASCH', 'LEANM', 'SCHNAM', 
                         'PHONE', 'MSTREE', 'MCITY', 'MSTATE', 'MZIP', 
                         'MZIP4_LSTREE_LCITY', 'LSTATE', 'LZIP', 'LZIP4_OTHERS',
                         'X9', 'X10', 'X11', 'X12', 'X13', 'X14', 
                         'X15', 'X16', 'X17', 'X18', 'X19', 
                         'X20', 'X21', 'X22', 'X23', 'X24', 
                         'X25', 'X26', 'X27', 'X28', 'X29', 
                         'X30', 'X31', 'X32', 'X34', 'X35', 
                         'X36', 'X37', 'X38', 'X39', 'X40',
                         'X41', 'X42', 'X43')
# Add YEAR column
ccd_03_ai$YEAR <- 2003
# Gets rid of commas
ccd_03_ai$SCHNAM <- gsub(",", " ", ccd_03_ai$SCHNAM)
ccd_03_ai$LEANM <- gsub(",", " ", ccd_03_ai$LEANM)

# Read in .txt for states K-N
ccd_03_kn <- read_table("data/Sc021akn.txt", col_names = FALSE, col_types = NULL)

# SEPARATES COLUMNS
ccd_03_kn <- separate(ccd_03_kn, X1, c("X1.a","X1.b"), sep = -4) # separates STID from NCESSCH
ccd_03_kn <- separate(ccd_03_kn, X5, c("X5.a","X5.b"), sep = 10) # separates PHONE from MSTREE
ccd_03_kn <- separate(ccd_03_kn, X7, c("X7.a","X7.b"), sep = 2) # separates MSTATE from MZIP+MZIP4+LSTREE+LCITY
ccd_03_kn <- separate(ccd_03_kn, X7.b, c("X7.b","X7.c"), sep = 5) # separates MZIP from MZIP4+LSTREE+LCITY
ccd_03_kn <- separate(ccd_03_kn, X9, c("X9.a","X9.b"), sep = 2) # separates LSTATE from LZIP+LZIP4+OTHERS
ccd_03_kn <- separate(ccd_03_kn, X9.b, c("X9.b","X9.c"), sep = 5) # separates LZIP from LZIP4+OTHERS
ccd_03_kn <- separate(ccd_03_kn, X9.c, c("X9.c","X9.d"), sep = -2) # separates something
ccd_03_kn <- separate(ccd_03_kn, X13, c("X13.a","X13.b"), sep = 3) # separates something
ccd_03_kn <- separate(ccd_03_kn, X13.b, c("X13.a","X13.b"), sep = 4) # separates something
# Turn NA/Missing => -99
ccd_03_kn[ccd_03_kn == -2] <- -99
ccd_03_kn[ccd_03_kn == -1] <- -99
ccd_03_kn[ccd_03_kn == 'M'] <- -99
ccd_03_kn[ccd_03_kn == 'N'] <- -99
# Set COLNAMES
colnames(ccd_03_kn) <- c('NCESSCH', 'STID', 'SEASCH', 'LEANM', 'SCHNAM', 
                         'PHONE', 'MSTREE', 'MCITY', 'MSTATE', 'MZIP', 
                         'MZIP4_LSTREE', 'LCITY', 'LSTATE', 'LZIP', 'LZIP4_OTHERS',
                         'X9', 'X10', 'X11', 'X12', 'X13', 'X14', 
                         'X15', 'X16', 'X17', 'X18', 'X19', 
                         'X20', 'X21', 'X22', 'X23', 'X24', 
                         'X25', 'X26', 'X27', 'X28', 'X29', 
                         'X30', 'X31', 'X32', 'X34', 'X35', 
                         'X36', 'X37', 'X38', 'X39', 'X40',
                         'X41', 'X42', 'X43')
# Add YEAR column
ccd_03_kn$YEAR <- 2003
# Gets rid of commas
ccd_03_kn$SCHNAM <- gsub(",", " ", ccd_03_kn$SCHNAM)
ccd_03_kn$LEANM <- gsub(",", " ", ccd_03_kn$LEANM)


# Read in .txt for states O-W
ccd_03_ow <- read_table("data/Sc021aow.txt", col_names = FALSE, col_types = NULL)

# Gets rid of column X4
ccd_03_ow$X5 <- NULL
# SEPARATES COLUMNS
ccd_03_ow <- separate(ccd_03_ow, X1, c("X1.a","X1.b"), sep = -4) # separates STID from NCESSCH
ccd_03_ow <- separate(ccd_03_ow, X6, c("X6.a","X6.b"), sep = 10) # separates PHONE from MSTREE
ccd_03_ow <- separate(ccd_03_ow, X8, c("X8.a","X8.b"), sep = 2) # separates MSTATE from MZIP+MZIP4+LSTREE+LCITY
ccd_03_ow <- separate(ccd_03_ow, X8.b, c("X8.b","X8.c"), sep = 5) # separates MZIP from MZIP4+LSTREE+LCITY
ccd_03_ow <- separate(ccd_03_ow, X10, c("X10.a","X10.b"), sep = 2) # separates LSTATE from LZIP+LZIP4+OTHERS
ccd_03_ow <- separate(ccd_03_ow, X10.b, c("X10.b","X10.c"), sep = 5) # separates LZIP from LZIP4+OTHERS
ccd_03_ow <- separate(ccd_03_ow, X10.c, c("X10.c","X10.d"), sep = -2) # separates something
ccd_03_ow <- separate(ccd_03_ow, X13, c("X13.a","X13.b"), sep = 3) # separates something
# Turn NA/Missing => -99
ccd_03_ow[ccd_03_ow == -2] <- -99
ccd_03_ow[ccd_03_ow == -1] <- -99
ccd_03_ow[ccd_03_ow == 'M'] <- -99
ccd_03_ow[ccd_03_ow == 'N'] <- -99
# Set COLNAMES
colnames(ccd_03_ow) <- c('NCESSCH', 'STID', 'SEASCH', 'LEANM', 'SCHNAM', 
                         'PHONE', 'MSTREE', 'MCITY', 'MSTATE', 'MZIP', 
                         'MZIP4_LSTREE', 'LCITY', 'LSTATE', 'LZIP', 'LZIP4_OTHERS',
                         'X9', 'X10', 'X11', 'X12', 'X13', 'X14', 
                         'X15', 'X16', 'X17', 'X18', 'X19', 
                         'X20', 'X21', 'X22', 'X23', 'X24', 
                         'X25', 'X26', 'X27', 'X28', 'X29', 
                         'X30', 'X31', 'X32', 'X34', 'X35', 
                         'X36', 'X37', 'X38', 'X39', 'X40',
                         'X41', 'X42', 'X43')
# Add YEAR column
ccd_03_ow$YEAR <- 2003
# Gets rid of commas
ccd_03_ow$SCHNAM <- gsub(",", " ", ccd_03_ow$SCHNAM)
ccd_03_ow$LEANM <- gsub(",", " ", ccd_03_ow$LEANM)



# Merge All of Them Into One
ccd_03 <- merge(ccd_03_ai, ccd_03_kn, all = TRUE)
ccd_03 <- merge(ccd_03, ccd_03_ow, all = TRUE)




# Writes to .csv file
# write.csv(ccd_03, 'cleaned_data/CCD_200203.csv')



# 2001-02 ------------------------------------------------------------------------------
# Read in .dat for states A-I
ccd_02_ai <- read_table("data/Sc011aai.dat", col_names = FALSE, col_types = NULL)

# Gets rid of column X4
ccd_02_ai$X4 <- NULL
# SEPARATES COLUMNS
ccd_02_ai <- separate(ccd_02_ai, X1, c("X1.a","X1.b"), sep = -4) # separates STID from NCESSCH
ccd_02_ai <- separate(ccd_02_ai, X6, c("X6.a","X6.b"), sep = 10) # separates PHONE from MSTREE
ccd_02_ai <- separate(ccd_02_ai, X8, c("X8.a","X8.b"), sep = 2) # separates MSTATE from MZIP+MZIP4+LSTREE+LCITY
ccd_02_ai <- separate(ccd_02_ai, X8.b, c("X8.b","X8.c"), sep = 6) # separates MZIP from MZIP4+LSTREE+LCITY
ccd_02_ai <- separate(ccd_02_ai, X9, c("X9.a","X9.b"), sep = 2) # separates LSTATE from LZIP+LZIP4+OTHERS
ccd_02_ai <- separate(ccd_02_ai, X9.b, c("X9.b","X9.c"), sep = 5) # separates LZIP from LZIP4+OTHERS
ccd_02_ai <- separate(ccd_02_ai, X9.c, c("X9.c","X9.d"), sep = -2) # separates something
# Turn NA/Missing => -99
ccd_02_ai[ccd_02_ai == -2] <- -99
ccd_02_ai[ccd_02_ai == -1] <- -99
ccd_02_ai[ccd_02_ai == 'M'] <- -99
ccd_02_ai[ccd_02_ai == 'N'] <- -99
# Set COLNAMES
colnames(ccd_02_ai) <- c('NCESSCH', 'STID', 'SEASCH', 'LEANM', 'SCHNAM', 
                         'PHONE', 'MSTREE', 'MCITY', 'MSTATE', 'MZIP', 
                         'MZIP4_LSTREE_LCITY', 'LSTATE', 'LZIP', 'LZIP4_OTHERS',
                         'X9', 'X10', 'X11', 'X12', 'X13', 'X14', 
                         'X15', 'X16', 'X17', 'X18', 'X19', 
                         'X20', 'X21', 'X22', 'X23', 'X24', 
                         'X25', 'X26', 'X27', 'X28', 'X29', 
                         'X30', 'X31', 'X32', 'X34', 'X35', 
                         'X36', 'X37', 'X38', 'X39', 'X40',
                         'X41', 'X42', 'X43')
# Add YEAR column
ccd_02_ai$YEAR <- 2002
# Gets rid of commas
ccd_02_ai$SCHNAM <- gsub(",", " ", ccd_02_ai$SCHNAM)
ccd_02_ai$LEANM <- gsub(",", " ", ccd_02_ai$LEANM)


# Read in .dat for states K-N
ccd_02_kn <- read_table("data/Sc011akn.dat", col_names = FALSE, col_types = NULL)

# SEPARATES COLUMNS
ccd_02_kn <- separate(ccd_02_kn, X1, c("X1.a","X1.b"), sep = -6) # separates STID from NCESSCH
ccd_02_kn <- separate(ccd_02_kn, X5, c("X5.a","X5.b"), sep = 10) # separates PHONE from MSTREE
ccd_02_kn <- separate(ccd_02_kn, X7, c("X7.a","X7.b"), sep = 2) # separates MSTATE from MZIP+MZIP4+LSTREE+LCITY
ccd_02_kn <- separate(ccd_02_kn, X7.b, c("X7.b","X7.c"), sep = 6) # separates MZIP from MZIP4+LSTREE+LCITY
ccd_02_kn <- separate(ccd_02_kn, X9, c("X9.a","X9.b"), sep = 2) # separates LSTATE from LZIP+LZIP4+OTHERS
ccd_02_kn <- separate(ccd_02_kn, X9.b, c("X9.b","X9.c"), sep = 5) # separates LZIP from LZIP4+OTHERS
ccd_02_kn <- separate(ccd_02_kn, X9.c, c("X9.c","X9.d"), sep = -2) # separates something
# Turn NA/Missing => -99
ccd_02_kn[ccd_02_kn == -2] <- -99
ccd_02_kn[ccd_02_kn == -1] <- -99
ccd_02_kn[ccd_02_kn == 'M'] <- -99
ccd_02_kn[ccd_02_kn == 'N'] <- -99
# Changes COLNAMES
colnames(ccd_02_kn) <- c('NCESSCH', 'STID', 'SEASCH', 'LEANM', 'SCHNAM', 
                         'PHONE', 'MSTREE', 'MCITY', 'MSTATE', 'MZIP', 
                         'MZIP4_LSTREE', 'LCITY', 'LSTATE', 'LZIP', 'LZIP4', 'X16', 'X17')
# Add YEAR column
ccd_02_kn$YEAR <- 2002
# Gets rid of commas
ccd_02_kn$SCHNAM <- gsub(",", " ", ccd_02_kn$SCHNAM)
ccd_02_kn$LEANM <- gsub(",", " ", ccd_02_kn$LEANM)


# Read in .dat for states O-W
ccd_02_ow <- read_table("data/Sc011aow.dat", col_names = FALSE, col_types = NULL)

# SEPARATES COLUMNS
ccd_02_ow <- separate(ccd_02_ow, X1, c("X1.a","X1.b"), sep = -4) # separates STID from NCESSCH
ccd_02_ow <- separate(ccd_02_ow, X5, c("X5.a","X5.b"), sep = 10) # separates PHONE from MSTREE
ccd_02_ow <- separate(ccd_02_ow, X6, c("X6.a","X6.b"), sep = 2) # separates MSTATE from MZIP+MZIP4+LSTREE+LCITY
ccd_02_ow <- separate(ccd_02_ow, X6.b, c("X6.b","X6.c"), sep = 5) # separates MZIP from MZIP4+LSTREE+LCITY
ccd_02_ow <- separate(ccd_02_ow, X7, c("X7.a","X7.b"), sep = 2) # separates LSTATE from LZIP+LZIP4+OTHERS
ccd_02_ow <- separate(ccd_02_ow, X7.b, c("X7.b","X7.c"), sep = 5) # separates LZIP from LZIP4+OTHERS
ccd_02_ow <- separate(ccd_02_ow, X7.c, c("X7.c","X7.d"), sep = -2) # separates something
# Turn NA/Missing => -99
ccd_02_ow[ccd_02_ow == -2] <- -99
ccd_02_ow[ccd_02_ow == -1] <- -99
ccd_02_ow[ccd_02_ow == 'M'] <- -99
ccd_02_ow[ccd_02_ow == 'N'] <- -99
# Changes COLNAMES
colnames(ccd_02_ow) <- c('NCESSCH', 'STID', 'SEASCH', 'LEANM', 'SCHNAM', 
                         'PHONE', 'MSTREE_MCITY', 'MSTATE', 'MZIP', 
                         'MZIP4', 'LSTREE', 'LSTATE', 'LZIP', 'LZIP4', 'X16', 'X17')
# Add YEAR column
ccd_02_ow$YEAR <- 2002
# Gets rid of commas
ccd_02_ow$SCHNAM <- gsub(",", " ", ccd_02_ow$SCHNAM)
ccd_02_ow$LEANM <- gsub(",", " ", ccd_02_ow$LEANM)

# Merge All of Them Into One
ccd_02 <- merge(ccd_02_ai, ccd_02_kn, all = TRUE)
ccd_02 <- merge(ccd_02, ccd_02_ow, all = TRUE)





# Writes to .csv file
# write.csv(ccd_02, 'cleaned_data/CCD_200102.csv')


# 2000-01 ------------------------------------------------------------------------------
# Read in .dat for states A-I
ccd_01_ai <- read_table("data/Sc001aai.dat", col_names = FALSE, col_types = NULL)

# Gets rid of column X4
ccd_01_ai$X4 <- NULL
# SEPARATES COLUMNS
ccd_01_ai <- separate(ccd_01_ai, X1, c("X1.a","X1.b"), sep = -4) # separates STID from NCESSCH
ccd_01_ai <- separate(ccd_01_ai, X6, c("X6.a","X6.b"), sep = 10) # separates PHONE from MSTREE
ccd_01_ai <- separate(ccd_01_ai, X8, c("X8.a","X8.b"), sep = 2) # separates MSTATE from MZIP+MZIP4+LSTREE+LCITY
ccd_01_ai <- separate(ccd_01_ai, X8.b, c("X8.b","X8.c"), sep = 5) # separates MZIP from MZIP4+LSTREE+LCITY
ccd_01_ai <- separate(ccd_01_ai, X9, c("X9.a","X9.b"), sep = 2) # separates LSTATE from LZIP+LZIP4+OTHERS
ccd_01_ai <- separate(ccd_01_ai, X9.b, c("X9.b","X9.c"), sep = 5) # separates LZIP from LZIP4+OTHERS
# Turn NA/Missing => -99
ccd_01_ai[ccd_01_ai == -2] <- -99
ccd_01_ai[ccd_01_ai == -1] <- -99
ccd_01_ai[ccd_01_ai == 'M'] <- -99
ccd_01_ai[ccd_01_ai == 'N'] <- -99
# Changes COLNAMES
colnames(ccd_01_ai) <- c('NCESSCH', 'STID', 'SEASCH', 'LEANM', 'SCHNAM', 
                         'PHONE', 'MSTREE', 'MCITY', 'MSTATE', 'MZIP', 
                         'MZIP4_LSTREE_LCITY', 'LSTATE', 'LZIP', 'LZIP4_OTHERS',
                         'X9', 'X10', 'X11', 'X12', 'X13', 'X14', 
                         'X15', 'X16', 'X17', 'X18', 'X19', 
                         'X20', 'X21', 'X22', 'X23', 'X24', 
                         'X25', 'X26', 'X27', 'X28', 'X29', 
                         'X30', 'X31', 'X32', 'X34', 'X35', 
                         'X36', 'X37', 'X38', 'X39', 'X40',
                         'X41', 'X42', 'X43')
# Add YEAR column
ccd_01_ai$YEAR <- 2001
# Gets rid of commas
ccd_01_ai$SCHNAM <- gsub(",", " ", ccd_01_ai$SCHNAM)
ccd_01_ai$LEANM <- gsub(",", " ", ccd_01_ai$LEANM)


# Read in .dat for states K-N
ccd_01_kn <- read_table("data/Sc001akn.dat", col_names = FALSE, col_types = NULL)

# SEPARATES COLUMNS
ccd_01_kn <- separate(ccd_01_kn, X1, c("X1.a","X1.b"), sep = -6) # separates STID from NCESSCH
ccd_01_kn <- separate(ccd_01_kn, X5, c("X5.a","X5.b"), sep = 10) # separates PHONE from MSTREE
ccd_01_kn <- separate(ccd_01_kn, X7, c("X7.a","X7.b"), sep = 2) # separates MSTATE from MZIP+MZIP4+LSTREE+LCITY
ccd_01_kn <- separate(ccd_01_kn, X7.b, c("X7.b","X7.c"), sep = 5) # separates MZIP from MZIP4+LSTREE+LCITY
ccd_01_kn <- separate(ccd_01_kn, X9, c("X9.a","X9.b"), sep = 2) # separates LSTATE from LZIP+LZIP4+OTHERS
ccd_01_kn <- separate(ccd_01_kn, X9.b, c("X9.b","X9.c"), sep = 5) # separates LZIP from LZIP4+OTHERS
# Turn NA/Missing => -99
ccd_01_kn[ccd_01_kn == -2] <- -99
ccd_01_kn[ccd_01_kn == -1] <- -99
ccd_01_kn[ccd_01_kn == 'M'] <- -99
ccd_01_kn[ccd_01_kn == 'N'] <- -99
# Changes COLNAMES
colnames(ccd_01_kn) <- c('NCESSCH', 'STID', 'SEASCH', 'LEANM', 'SCHNAM', 
                         'PHONE', 'MSTREE', 'MCITY', 'MSTATE', 'MZIP', 
                         'MZIP4_LSTREE', 'LCITY', 'LSTATE', 'LZIP', 'LZIP4', 'X16', 'X17')
# Add YEAR column
ccd_01_kn$YEAR <- 2001
# Gets rid of commas
ccd_01_kn$SCHNAM <- gsub(",", " ", ccd_01_kn$SCHNAM)
ccd_01_kn$LEANM <- gsub(",", " ", ccd_01_kn$LEANM)


# Read in .dat for states O-W
ccd_01_ow <- read_table("data/Sc001aow.dat", col_names = FALSE, col_types = NULL)

# SEPARATES COLUMNS
ccd_01_ow <- separate(ccd_01_ow, X1, c("X1.a","X1.b"), sep = -4) # separates STID from NCESSCH
ccd_01_ow <- separate(ccd_01_ow, X5, c("X5.a","X5.b"), sep = 10) # separates PHONE from MSTREE
ccd_01_ow <- separate(ccd_01_ow, X7, c("X7.a","X7.b"), sep = 2) # separates MSTATE from MZIP+MZIP4+LSTREE+LCITY
ccd_01_ow <- separate(ccd_01_ow, X7.b, c("X7.b","X7.c"), sep = 5) # separates MZIP from MZIP4+LSTREE+LCITY
ccd_01_ow <- separate(ccd_01_ow, X9, c("X9.a","X9.b"), sep = 2) # separates LSTATE from LZIP+LZIP4+OTHERS
ccd_01_ow <- separate(ccd_01_ow, X9.b, c("X9.b","X9.c"), sep = 5) # separates LZIP from LZIP4+OTHERS
# Turn NA/Missing => -99
ccd_01_ow[ccd_01_ow == -2] <- -99
ccd_01_ow[ccd_01_ow == -1] <- -99
ccd_01_ow[ccd_01_ow == 'M'] <- -99
ccd_01_ow[ccd_01_ow == 'N'] <- -99
# Changes COLNAMES
colnames(ccd_01_ow) <- c('NCESSCH', 'STID', 'SEASCH', 'LEANM', 'SCHNAM', 
                         'PHONE', 'MSTREE', 'MCITY', 'MSTATE', 'MZIP', 
                         'MZIP4+LSTREE', 'LCITY', 'LSTATE', 'LZIP', 'LZIP4', 'X16', 'X17')
# Add YEAR column
ccd_01_ow$YEAR <- 2001
# Gets rid of commas
ccd_01_ow$SCHNAM <- gsub(",", " ", ccd_01_ow$SCHNAM)
ccd_01_ow$LEANM <- gsub(",", " ", ccd_01_ow$LEANM)


# Merge All of Them Into One
ccd_01 <- merge(ccd_01_ai, ccd_01_kn, all = TRUE)
ccd_01 <- merge(ccd_01, ccd_01_ow, all = TRUE)




# Writes to .csv file
# write.csv(ccd_01, 'cleaned_data/CCD_200001.csv')



