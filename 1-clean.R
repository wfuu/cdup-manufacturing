#####################
#
# Commodity Flow Survey data only has code, but not name/labels.
#
# This script adds the names of state, industry, goods, 
# and transportation mode.
#
######################

#naming convention: lower case for object names, upper case for column/variable names

# source('0-download.R')

#1. Load CFS file and data dictionaries

library(data.table)
fips.code <- read.csv("FIPS Code.csv")
naics <- read.csv("NAICS.csv")
sctg <- read.csv("SCTG.csv")
mt <- read.csv("MT.csv")
cfs <- fread("2012_cfs.txt")

#2. Recode variables in CFS file

#FIPS codes
cfs$ORIG_STATE_name <- fips.code$State[match(cfs$ORIG_STATE, fips.code$FIPS)]
cfs$DEST_STATE_name <- fips.code$State[match(cfs$DEST_STATE, fips.code$FIPS)]

#NAICS (North American Industry Classification System) codes
cfs$NAICS_name <- naics$Description[match(cfs$NAICS, naics$naics)]

#SCTG (Standard Classification of Transported Goods) codes
cfs$SCTG_name <- sctg$Description[match(cfs$SCTG, sctg$sctg)]

#MT (Mode of Transportation) codes
cfs$MODE_name <- mt$description[match(cfs$MODE, mt$mode_code)]
