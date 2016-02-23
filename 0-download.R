#######################
#
# This script downloads 2012 Commodity Flow Survey shipment level data 
# as well as its associated data dictionary.
#
# It also writes out each data dictionary as a cleaned csv.
#
######################

# install.packages("gdata")
library(gdata)
installXLSXsupport()
options(stringsAsFactors = FALSE)

# Download Survey Data
download.file("http://www.census.gov/econ/cfs/2012/cfs_2012_pumf_csv.zip", destfile="cfs.zip")
unzip("cfs.zip")
file.rename(from = "cfs_2012_pumf_csv.txt", to = "2012_cfs.txt")


# Connect to Data Dictionary
url = "http://www.census.gov/econ/cfs/2012/cfs_2012_pum_file_users_guide_App_A%20(Jun%202015).xlsx"

# Read each sheet 
state_metroarea <- read.xls(url, sheet = 2)
industry_naics <- read.xls(url, sheet = 3)
goods_sctg <- read.xls(url, sheet = 4)
transport_mode <- read.xls(url, sheet = 5)

# Clean state metro area dictionary
names(state_metroarea) <- c("cfs_area","state", "state_cfs", "type", "description")
state_metroarea <- state_metroarea[-c(1,2),]
rownames(state_metroarea) <- NULL
write.csv(state_metroarea, "cfs_dictionary.csv")

# clean industry
names(industry_naics) <- c("naics", "description")
industry_naics <- industry_naics[-1,]
rownames(industry_naics) <- NULL
write.csv(industry_naics, "NAICS.csv")

# clean goods dictionary
goods_sctg <- goods_sctg[-1,]
names(goods_sctg) <- c("sctg", "description", "sctg_group")
rownames(goods_sctg) <- NULL
temp <- as.numeric(goods_sctg$sctg)

goods_sctg$sctg_group <- ifelse(temp < 6 , '01-05',
                                ifelse(temp < 10, '06-09',
                                       ifelse(temp < 15, '10-14',
                                              ifelse(temp < 20, '15-19',
                                                     ifelse(temp < 25, '20-24',
                                                            ifelse(temp < 31, '25-30',
                                                                   ifelse(temp < 35, '31-34',
                                                                          ifelse(temp < 39, '35-38', '39-99'))))))))
                                          
goods_sctg[temp == 0,]$sctg_group <- "00"

write.csv(goods_sctg, "SCTG.csv")

# clean mode of transportation
transport_temp <- transport_mode[23:nrow(transport_mode), ]
transport_mode <- transport_mode[2:22, ]
transport_mode <- transport_mode[ , 1:2]
names(transport_mode) <- c("mode_code", 'description')
write.csv(transport_mode, "MT.csv")

