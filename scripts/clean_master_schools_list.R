#### Center on Reinventing Public Education #### 
# Title: Clean Master School List
# Description: Cleans the Master School List so it's more compatible with the database
# Created by: Kevin Cha on 07-13-17
# Updated by: Kevin Cha on 08-09-17
# Link to Github: https://github.com/CRPE-UWB/Federal
# Notes: Internal FUNCTION is supposed to stay inside the script
#        External FUNCTION will be the one to export to other code

# libraries needed
library(stringr)
library(dplyr)
library(gsubfn)


# FUNCTION (Internal): --------------------------------------------------------------------
# Function 1: Makes the columns to the right number of digits
# warning: turns ncessch into character
fix_ncessch <- function(data) {
  # requires 5 digits
  data$schno <- sprintf("%05d", data$schno)
  
  # requires 7 digits
  data$leaid <- sprintf("%07d", data$leaid)
  
  # should have 12 digits
  data$ncessch <- paste(data$leaid, data$schno, sep = "")
  # nchar(data$ncessch) # run to error check: should all be 12
  
  # turns them back into integers
  # only needed them to be characters in order to combine them
  # data$schno <- as.integer(data$schno)
  # data$leaid <- as.integer(data$leaid)
  
  return(data)
}

# Function 2: gets rid of special symbols like ",", "@", etc
# database won't like it
no_more_special_symbols <- function(data) {
  library(stringr)
  # replaces the commas
  data$leanm <- gsub(",", "", data$leanm)
  data$schnam <- gsub(",", "", data$schnam)
  data$city <- gsub(",", "", data$city)
  # alternative
  data[data == '-'] <- ""
  
  # replace the tilde
  data$leanm <- gsub("~", "-", data$leanm)
  data$schnam <- gsub("~", "-", data$schnam)
  data$city <- gsub("~", "-", data$city)
  # alternative
  data[datqa == '~'] <- "-"
  
  return(data)
}

no_more_special_characters <- function(df_col) {
  # need to replace the chracters
  library(gsubfn)
  # list of special characters to replace with
  unwanted_array <- list(    'Š'='S', 'š'='s', 'Ž'='Z', 'ž'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                            'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                            'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                            'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                            'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y' )
 # replaces the characters
  df_col <- gsubfn(paste(names(unwanted_array),collapse='|'), unwanted_array,df_col)
  
  return(df_col)
}

# FUNCTION (External): --------------------------------------------------------------------
clean_master <- function(df) {
  df <- fix_ncessch(df)
  df <- no_more_special_symbols(df)
  
  df$leanm <- no_more_special_characters(df$leanm)
  # data$schnam <- no_more_special_characters(data$schnam)
  df$city <- no_more_special_characters(df$city)
  
  return(df)
}