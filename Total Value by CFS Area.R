#setwd("~/Documents/School/QMSS II/Projects/Manufacturing GIS")
#naming convention: lower case for object names, upper case for column/variable names

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
cfs$NAICS_name <- naics$Description[match(cfs$NAICS, naics$NAICS)]

#SCTG (Standard Classification of Transported Goods) codes
cfs$SCTG_name <- sctg$Description[match(cfs$SCTG, sctg$SCTG)]

#MT (Mode of Transportation) codes
cfs$MODE_name <- mt$Mode.of.transportation.Description[match(cfs$MODE, mt$Mode.Code)]

#3. Summarise and prepare data for QGIS mapping

#3.1 Select only rows with manufacturing industries
cfs_mf <- cfs[grep("manufacturing", cfs$NAICS_name), ]

#3.2 Select only origins and destinations in the lower 48 states (for better presentation in QGIS)
cfs_48 <- cfs_mf[ORIG_STATE_name != "Alaska" & ORIG_STATE_name != "Hawaii"
                 & DEST_STATE_name != "Alaska" & DEST_STATE_name != "Hawaii", ]

#3.3 Value by Origin: Group rows by the origin CFS Areas and summarise the total shipment value from these origins
#Convert total value to thousand dollars 
library(dplyr)
value_by_orig <- group_by(cfs_48, ORIG_CFS_AREA) %>% 
        summarize(TL_VAL_O = sum(SHIPMT_VALUE/1000))

#3.4 Value by Destination: Group rows by the destination CFS Areas and summarise the total shipment value to these destinations
value_by_dest <- group_by(cfs_48, DEST_CFS_AREA) %>%
        summarize(TL_VAL_D = sum(SHIPMT_VALUE/1000))

#3.5 Check for missing data (since QGIS doesn't handle missing data)
summary(value_by_orig)
summary(value_by_dest)

#3.6 Check and discard the difference in numbers of origin CFS Areas and destination CFS Areas (due to data suppression)
(suprsd <- setdiff(value_by_orig$ORIG_CFS_AREA, value_by_dest$DEST_CFS_AREA))
rm <- with(value_by_orig, which(ORIG_CFS_AREA %in% suprsd))
value_by_orig <- value_by_orig[- rm, ]

#3.6 Combine the data frames and write to .csv (for use in QGIS)
total_value <- cbind(value_by_orig, value_by_dest)
write.csv(total_value, "TL_VAL.csv")

#4. Obtain shapefile containing CFS area codes, edit area codes for data-join later
#4.1 In QGIS, select the lower 48 states 
#4.2 Transform the projection of the map
#4.3 Join the CFS data with the shapefile by CFS area code (first by origin, then by destination)
#4.4 Convert the joined columns to numeric data

#5. Plot gradient graphs of relationships of interest



