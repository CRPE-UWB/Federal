#### Center on Reinventing Public Education #### 
# Description: Cleaning OCR data to find math enrollment by different cities. Looking for originally
#               4 states only, but this version contains all the states.
# Title: Cleaning Massachusetts 
# Created by: Kevin Cha on 03-31-17
# Updated by: Kevin Cha on 08-10-17
# Data from: https://www2.ed.gov/rschstat/catalog/student-demographics.html
# Notes:
# when committing, make sure to not include this dataset b/c its too big
# TODO
# -Make sure variables on top
# -Want to find advanced math (Assumed pre calc, calc) in different cities


# Set Up --------------------------------------------------------------------------------------------------------
rm(list=ls()) 

library(plyr)
library(dplyr)
library(tidyr)
library(data.table)
library(stringr)

# SETWD

setwd("C:/Users/phato_000/Documents/CRPE/enrollment/ocr_clean") #PC
setwd("/Users/crpe/Documents/ocr_clean") #MAC


# Functions --------------------------------------------------------------------------------------------------------
no_more_special_characters <- function(df_col) {
  # need to replace the chracters
  require(gsubfn)
  # make sure the col is character
  df_col <- as.character(df_col)
  # list of special characters to replace with
  unwanted_array = list(    'Š'='S', 'š'='s', 'Ž'='Z', 'ž'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                            'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                            'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                            'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                            'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y' )
  # replaces the characters
  df_col <- gsubfn(paste(names(unwanted_array),collapse='|'), unwanted_array,df_col)
  
  return(df_col)
}

# Lists --------------------------------------------------------------------------------------------------------
# list of columns we want to keep
list_of_wanted_col <- c( 'LEA_STATE', 'LEA_NAME', 'SCH_NAME',
                         'COMBOKEY', 'LEAID', 'SCHID',
                         'JJ', 'NCES_SCHOOL_ID', 'SCH_GRADE_KG',
                         'SCH_GRADE_G01', 'SCH_GRADE_G02', 'SCH_GRADE_G03',
                         'SCH_GRADE_G04', 'SCH_GRADE_G05', 'SCH_GRADE_G06',
                         'SCH_GRADE_G07', 'SCH_GRADE_G08', 'SCH_GRADE_G09',
                         'SCH_GRADE_G10', 'SCH_GRADE_G11', 'SCH_GRADE_G12',
                         'SCH_GRADE_UG', 'TOT_ENR_M', 'TOT_ENR_F',
                         'SCH_MATHCLASSES_ADVM', 'SCH_MATHCLASSES_CALC', 'SCH_MATHENR_ADVM_HI_M', 
                         'SCH_MATHENR_ADVM_HI_F', 'SCH_MATHENR_ADVM_AM_M', 'SCH_MATHENR_ADVM_AM_F',
                         'SCH_MATHENR_ADVM_AS_M', 'SCH_MATHENR_ADVM_AS_F', 'SCH_MATHENR_ADVM_HP_M',
                         'SCH_MATHENR_ADVM_HP_F', 'SCH_MATHENR_ADVM_BL_M', 'SCH_MATHENR_ADVM_BL_F',
                         'SCH_MATHENR_ADVM_WH_M', 'SCH_MATHENR_ADVM_WH_F', 'SCH_MATHENR_ADVM_TR_M',
                         'TOT_MATHENR_ADVM_M', 'TOT_MATHENR_ADVM_F', 'SCH_MATHENR_ADVM_TR_F',
                         'SCH_MATHENR_ADVM_LEP_M', 'SCH_MATHENR_ADVM_LEP_F', 'SCH_MATHENR_ADVM_IDEA_M',
                         'SCH_MATHENR_ADVM_IDEA_F', 'SCH_MATHENR_CALC_HI_M', 'SCH_MATHENR_CALC_HI_F',
                         'SCH_MATHENR_CALC_AM_M', 'SCH_MATHENR_CALC_AM_F', 'SCH_MATHENR_CALC_AS_M',
                         'SCH_MATHENR_CALC_AS_F', 'SCH_MATHENR_CALC_HP_M', 'SCH_MATHENR_CALC_HP_F', 
                         'SCH_MATHENR_CALC_BL_M', 'SCH_MATHENR_CALC_BL_F', 'SCH_MATHENR_CALC_WH_M',
                         'SCH_MATHENR_CALC_WH_F', 'SCH_MATHENR_CALC_TR_M', 'SCH_MATHENR_CALC_TR_F',
                         'TOT_MATHENR_CALC_M', 'TOT_MATHENR_CALC_F', 'SCH_MATHENR_CALC_LEP_M',
                         'SCH_MATHENR_CALC_LEP_F', 'SCH_MATHENR_CALC_IDEA_M', 'SCH_MATHENR_CALC_IDEA_F'
)
# Clean Up --------------------------------------------------------------------------------------------------------
# read in dataset
ocr <- read.csv("data/CRDC2013_14_SCH.csv", stringsAsFactors = FALSE, na.strings = "")
backup <- ocr
ocr <- backup

# Get rid of flag columns (btw flag is used for checkin in datasets, tmyk)
# DEBUG: 1929 => 1928
ocr <- ocr[, -grep("FLAG", colnames(ocr))]
# # DEBUG: 1928 => 1671
ocr <- ocr[, -grep("DSO", colnames(ocr))]

# # Turn Yes/No => 1/0
ocr[ocr == 'YES'] <- 1
ocr[ocr == 'NO'] <- 0
# Make -2 and - 9 => NA
ocr[ocr == -2] <- NA
ocr[ocr == -9] <- NA

# Obtain only columns that have to do with math --------------------------------------------------------------------------------------------------------
# make it so df only has the columns we want to keep
ocr <- ocr %>%
  select(one_of(list_of_wanted_col))

## make total enrollment column (general, advm, calc)
ocr <- ocr %>% 
  mutate(TOT_ENR = TOT_ENR_M + TOT_ENR_F)
ocr <- ocr %>% 
  mutate(TOT_MATHENR_ADVM = TOT_MATHENR_ADVM_M + TOT_MATHENR_ADVM_F)
ocr <- ocr %>% 
  mutate(TOT_MATHENR_CALC = TOT_MATHENR_CALC_M + TOT_MATHENR_CALC_F)

## make percent enrolled in advmath courses columns for each demographic
ocr <- ocr %>% 
  mutate(PCT_MATHENR_ADVM_TOT = round(TOT_MATHENR_ADVM / TOT_ENR, digits=6))
ocr <- ocr %>% 
  mutate(PCT_MATHENR_CALC_TOT = round(TOT_MATHENR_CALC / TOT_ENR, digits=6))

## make percent enrolled in advmath courses columns for each demographic
ocr <- ocr %>% 
  mutate(PCT_MATHENR_ADVM_HI_M = round(SCH_MATHENR_ADVM_HI_M / TOT_ENR, digits=6))
ocr <- ocr %>% 
  mutate(PCT_MATHENR_ADVM_HI_F = round(SCH_MATHENR_ADVM_HI_F / TOT_ENR, digits=6))
ocr <- ocr %>% 
  mutate(PCT_MATHENR_ADVM_AM_M = round(SCH_MATHENR_ADVM_AM_M / TOT_ENR, digits=6))
ocr <- ocr %>% 
  mutate(PCT_MATHENR_ADVM_AM_F = round(SCH_MATHENR_ADVM_AM_F / TOT_ENR, digits=6))
ocr <- ocr %>% 
  mutate(PCT_MATHENR_ADVM_AS_M = round(SCH_MATHENR_ADVM_AS_M / TOT_ENR, digits=6))
ocr <- ocr %>% 
  mutate(PCT_MATHENR_ADVM_AS_F = round(SCH_MATHENR_ADVM_AS_F / TOT_ENR, digits=6))
ocr <- ocr %>% 
  mutate(PCT_MATHENR_ADVM_HP_M = round(SCH_MATHENR_ADVM_HP_M / TOT_ENR, digits=6))
ocr <- ocr %>% 
  mutate(PCT_MATHENR_ADVM_HP_F = round(SCH_MATHENR_ADVM_HP_F / TOT_ENR, digits=6))
ocr <- ocr %>% 
  mutate(PCT_MATHENR_ADVM_BL_M = round(SCH_MATHENR_ADVM_BL_M / TOT_ENR, digits=6))
ocr <- ocr %>% 
  mutate(PCT_MATHENR_ADVM_BL_F = round(SCH_MATHENR_ADVM_BL_F / TOT_ENR, digits=6))
ocr <- ocr %>% 
  mutate(PCT_MATHENR_ADVM_WH_M = round(SCH_MATHENR_ADVM_WH_M / TOT_ENR, digits=6))
ocr <- ocr %>% 
  mutate(PCT_MATHENR_ADVM_WH_F = round(SCH_MATHENR_ADVM_WH_F / TOT_ENR, digits=6))
ocr <- ocr %>% 
  mutate(PCT_MATHENR_ADVM_TR_M = round(SCH_MATHENR_ADVM_TR_M / TOT_ENR, digits=6))
ocr <- ocr %>% 
  mutate(PCT_MATHENR_ADVM_TR_F = round(SCH_MATHENR_ADVM_TR_F / TOT_ENR, digits=6))
ocr <- ocr %>% 
  mutate(PCT_MATHENR_ADVM_LEP_M = round(SCH_MATHENR_ADVM_LEP_M / TOT_ENR, digits=6))
ocr <- ocr %>% 
  mutate(PCT_MATHENR_ADVM_LEP_F = round(SCH_MATHENR_ADVM_LEP_F / TOT_ENR, digits=6))
ocr <- ocr %>% 
  mutate(PCT_MATHENR_ADVM_IDEA_M = round(SCH_MATHENR_ADVM_IDEA_M / TOT_ENR, digits=6))
ocr <- ocr %>% 
  mutate(PCT_MATHENR_ADVM_IDEA_F = round(SCH_MATHENR_ADVM_IDEA_F / TOT_ENR, digits=6))
ocr <- ocr %>% 
  mutate(PCT_MATHENR_CALC_IDEA_M = round(TOT_MATHENR_ADVM_M / TOT_ENR, digits=6))
ocr <- ocr %>% 
  mutate(PCT_MATHENR_CALC_IDEA_F = round(TOT_MATHENR_ADVM_F / TOT_ENR, digits=6))

ocr <- ocr %>% 
  mutate(PCT_MATHENR_CALC_HI_M = round(SCH_MATHENR_CALC_HI_M / TOT_ENR, digits=6))
ocr <- ocr %>% 
  mutate(PCT_MATHENR_CALC_HI_F = round(SCH_MATHENR_CALC_HI_F / TOT_ENR, digits=6))
ocr <- ocr %>% 
  mutate(PCT_MATHENR_CALC_AM_M = round(SCH_MATHENR_CALC_AM_M / TOT_ENR, digits=6))
ocr <- ocr %>% 
  mutate(PCT_MATHENR_CALC_AM_F = round(SCH_MATHENR_CALC_AM_F / TOT_ENR, digits=6))
ocr <- ocr %>% 
  mutate(PCT_MATHENR_CALC_AS_M = round(SCH_MATHENR_CALC_AS_M / TOT_ENR, digits=6))
ocr <- ocr %>% 
  mutate(PCT_MATHENR_CALC_AS_F = round(SCH_MATHENR_CALC_AS_F / TOT_ENR, digits=6))
ocr <- ocr %>% 
  mutate(PCT_MATHENR_CALC_HP_M = round(SCH_MATHENR_CALC_HP_M / TOT_ENR, digits=6))
ocr <- ocr %>% 
  mutate(PCT_MATHENR_CALC_HP_F = round(SCH_MATHENR_CALC_HP_F / TOT_ENR, digits=6))
ocr <- ocr %>% 
  mutate(PCT_MATHENR_CALC_BL_M = round(SCH_MATHENR_CALC_BL_M / TOT_ENR, digits=6))
ocr <- ocr %>% 
  mutate(PCT_MATHENR_CALC_BL_F = round(SCH_MATHENR_CALC_BL_F / TOT_ENR, digits=6))
ocr <- ocr %>% 
  mutate(PCT_MATHENR_CALC_WH_M = round(SCH_MATHENR_CALC_WH_M / TOT_ENR, digits=6))
ocr <- ocr %>% 
  mutate(PCT_MATHENR_CALC_WH_F = round(SCH_MATHENR_CALC_WH_F / TOT_ENR, digits=6))
ocr <- ocr %>% 
  mutate(PCT_MATHENR_CALC_TR_M = round(SCH_MATHENR_CALC_TR_M / TOT_ENR, digits=6))
ocr <- ocr %>% 
  mutate(PCT_MATHENR_CALC_TR_F = round(SCH_MATHENR_CALC_TR_F / TOT_ENR, digits=6))
ocr <- ocr %>% 
  mutate(PCT_MATHENR_CALC_LEP_M = round(SCH_MATHENR_CALC_LEP_M / TOT_ENR, digits=6))
ocr <- ocr %>% 
  mutate(PCT_MATHENR_CALC_LEP_F = round(SCH_MATHENR_CALC_LEP_F / TOT_ENR, digits=6))
ocr <- ocr %>% 
  mutate(PCT_MATHENR_CALC_IDEA_M = round(SCH_MATHENR_CALC_IDEA_M / TOT_ENR, digits=6))
ocr <- ocr %>% 
  mutate(PCT_MATHENR_CALC_IDEA_F = round(SCH_MATHENR_CALC_IDEA_F / TOT_ENR, digits=6))
ocr <- ocr %>% 
  mutate(PCT_MATHENR_CALC_IDEA_M = round(TOT_MATHENR_CALC_M / TOT_ENR, digits=6))
ocr <- ocr %>% 
  mutate(PCT_MATHENR_CALC_IDEA_F = round(TOT_MATHENR_CALC_F / TOT_ENR, digits=6))

# Last Cleaning --------------------------------------------------------------------------------------------------------
# fix NCES_SCHOOL_ID by copying the COMBOKEY
names(ocr)[names(ocr) == 'NCES_SCHOOL_ID'] <- 'NCESSID'
ocr$NCESSID <- ocr$COMBOKEY

ocr$LEA_NAME <- as.character(ocr$LEA_NAME)
ocr$SCH_NAME <- as.character(ocr$SCH_NAME)

# last clean
ocr[is.na(ocr)] <- -99
ocr <- as.data.frame(sapply(ocr, gsub, pattern=",", replacement=""))
ocr <- as.data.frame(sapply(ocr, gsub, pattern="~", replacement="-"))
ocr <- as.data.frame(sapply(ocr, gsub, pattern="ñ", replacement="n"))
ocr <- as.data.frame(sapply(ocr, gsub, pattern="é", replacement="e"))


# Finish 
write.csv(ocr, "clean_data/ocr.csv", row.names = FALSE)

