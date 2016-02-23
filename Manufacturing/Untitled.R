library(reshape)
library(ggplot2)
library(plyr)
detach("package:plyr", unload=TRUE)
library(dplyr)
library(plotly)
library(data.table)
library(readxl)

#Reading in the dataset!
cfs_2012 <- read.csv("commodityflow.txt")

#List of all NAICS codes
naics <- read_excel("naics.xlsx")


#Building our manufacturing dataset - thanks Steve!
Industry <- rep(0,nrow(cfs_2012))
NAICS_data <- cfs_2012$NAICS
for(row in 1:4547661){
  Industry[row] <- naics$industry[naics$code == NAICS_data[row]]
}
cfs_2012$Industry <- Industry
manufacture_cfs <- cfs_2012[grep("manufacturing",cfs_2012$Industry),]

#Basic plot of this data to see the frequency of each type of manufacturing
ggplot(manufacture_cfs, aes(x = manufacture_cfs$Industry)) + geom_bar(stat = "count", fill="#FF9999", colour="black") + theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5, size=9))

#####################################

manufacture_cfs$ORIG_STATE <- as.factor(manufacture_cfs$ORIG_STATE)
manufacture_cfs$DEST_STATE <- as.factor(manufacture_cfs$DEST_STATE)

levels(manufacture_cfs$ORIG_STATE) <- c(NA, "AL"	,
                                        "AK"	,
                                        "AZ"	,
                                        "AR"	,
                                        "CA"	,
                                        "CO"	,
                                        "CT"	,
                                        "DE"	,
                                        "DC"	,
                                        "FL"	,
                                        "GA"	,
                                        "HI"	,
                                        "ID"	,
                                        "IL"	,
                                        "IN"	,
                                        "IA"	,
                                        "KS"	,
                                        "KY"	,
                                        "LA"	,
                                        "ME"	,
                                        "MD"	,
                                        "MA"	,
                                        "MI"	,
                                        "MN"	,
                                        "MS"	,
                                        "MO"	,
                                        "MT"	,
                                        "NE"	,
                                        "NV"	,
                                        "NH"	,
                                        "NJ"	,
                                        "NM"	,
                                        "NY"	,
                                        "NC"	,
                                        "ND"	,
                                        "OH"	,
                                        "OK"	,
                                        "OR"	,
                                        "PA"	,
                                        "RI"	,
                                        "SC"	,
                                        "SD"	,
                                        "TN"	,
                                        "TX"	,
                                        "UT"	,
                                        "VT"	,
                                        "VA"	,
                                        "WA"	,
                                        "WV"	,
                                        "WI"	,
                                        "WY"	)

levels(manufacture_cfs$DEST_STATE) <- c("AL"	,
                                        "AK"	,
                                        "AZ"	,
                                        "AR"	,
                                        "CA"	,
                                        "CO"	,
                                        "CT"	,
                                        "DE"	,
                                        "DC"	,
                                        "FL"	,
                                        "GA"	,
                                        "HI"	,
                                        "ID"	,
                                        "IL"	,
                                        "IN"	,
                                        "IA"	,
                                        "KS"	,
                                        "KY"	,
                                        "LA"	,
                                        "ME"	,
                                        "MD"	,
                                        "MA"	,
                                        "MI"	,
                                        "MN"	,
                                        "MS"	,
                                        "MO"	,
                                        "MT"	,
                                        "NE"	,
                                        "NV"	,
                                        "NH"	,
                                        "NJ"	,
                                        "NM"	,
                                        "NY"	,
                                        "NC"	,
                                        "ND"	,
                                        "OH"	,
                                        "OK"	,
                                        "OR"	,
                                        "PA"	,
                                        "RI"	,
                                        "SC"	,
                                        "SD"	,
                                        "TN"	,
                                        "TX"	,
                                        "UT"	,
                                        "VT"	,
                                        "VA"	,
                                        "WA"	,
                                        "WV"	,
                                        "WI"	,
                                        "WY"	)

manufacture_cfs$MODE <- as.factor(manufacture_cfs$MODE)
levels(manufacture_cfs$MODE) <- c("Mode Suppressed","Single Mode","Truck","For-hire Truck",
                                  "Private Truck", "Rail","Water","Inland Water","Great Lakes",
                                  "Deep Sea","Air (incl. Truck & Air)","Pipeline","Multiple Mode",
                                  "Parcel, USPS, or Courier","Truck and Rail","Truck and Water",
                                  "Rail and Water","Non-Parcel Multimode","Multiple Waterways")

#To get Exporting State as the State Abbreviation
manufacture_cfs$ORIG_STATE <- as.factor(manufacture_cfs$ORIG_STATE)   

ggplot(manufacture_cfs, aes(x = ORIG_STATE, fill = Industry)) + geom_bar() + 
  ggtitle("Manufacturing Industries by Origin State")

ggplot(manufacture_cfs, aes(x = DEST_STATE, fill = Industry)) + geom_bar() + 
  ggtitle("Manufacturing Industries by Destination State")

ggplot(manufacture_cfs, aes(x = DEST_STATE, fill = MODE)) + geom_bar() + 
  ggtitle("Mode of Transportion for Flow of MFG Goods by Destination State") + theme(axis.text.x = element_text(angle = 20, hjust = 1))

ggplot(manufacture_cfs, aes(x = ORIG_STATE, fill = MODE)) + geom_bar() + 
  ggtitle("Mode of Transportion for Flow of MFG Goods by Origin State") + theme(axis.text.x = element_text(angle = 20, hjust = 1))

ggplot(manufacture_cfs, aes(x = Industry, fill = MODE)) + geom_bar() + 
  ggtitle("Mode of Transportion for Flow of MFG Goods by Industry") + theme(axis.text.x = element_text(angle = 20, hjust = 1))


#Most popular NAICS codes - Industry
popNAICS <- manufacture_cfs %>%
  group_by(Industry) %>%
  summarize(
    Count = n(), na.rm=TRUE) %>%
  arrange(desc(Count))

popNAICS

#Most Popular NAICS Codes in Mfg.... FINISH THIS PART SCHINRIA
manufacture_cfs$popNAICS <- rep(NA)
manufacture_cfs$popNAICS <- ifelse(manufacture_cfs$Industry=="Chemical manufacturing", together2.mexico$popNAICS)
manufacture_cfs$popNAICS <- ifelse(together2.mexico$NAICS==333,"Machinery Manufac", together2.mexico$popNAICS)
manufacture_cfs$popNAICS <- ifelse(together2.mexico$NAICS==336,"Transportation Equip Manufac", together2.mexico$popNAICS)
manufacture_cfs$popNAICS <- ifelse(together2.mexico$NAICS==332,"Fabricated Metal Product Manufac", together2.mexico$popNAICS)
manufacture_cfs$popNAICS <- ifelse(together2.mexico$NAICS==326,"Plastics & Rubber Products Manufac", together2.mexico$popNAICS)
manufacture_cfs$popNAICS <- ifelse(together2.mexico$NAICS==334,"Computer & Electronic Product Manufac", together2.mexico$popNAICS)
manufacture_cfs$popNAICS <- ifelse(together2.mexico$NAICS==331,"Primary Metal Manufac", together2.mexico$popNAICS)
manufacture_cfs$popNAICS <- ifelse(together2.mexico$NAICS==335,"Electrical Equip, Appliance & Component Manufac", together2.mexico$popNAICS)
manufacture_cfs$popNAICS <- ifelse(together2.mexico$NAICS==4236,"Electrical & Electronic Goods Merchant Wholesalers", together2.mexico$popNAICS)
manufacture_cfs$popNAICS <- ifelse(together2.mexico$NAICS==322,"Paper Manufac", together2.mexico$popNAICS)
manufacture_cfs_popNAICS <- na.omit(together2.mexico)
##

least.popNAICS <- manufacture_cfs %>%
  group_by(Industry) %>%
  summarize(
    Count = n(), na.rm=TRUE) %>%
  arrange(Count)

least.popNAICS

####

#ggplot(together2, aes(Country, group=SHIPMT_DIST_ROUTED)) + 
geom_bar(aes(colour=SHIPMT_DIST_ROUTED, fill=SHIPMT_DIST_ROUTED), alpha=0.9) +
  xlab("Country") + ylab("Number of Cases") + 
  ggtitle("What Types of Materials are we Exporting to Canada and Mexico?") +
  scale_fill_brewer(palette = "Paired") + scale_colour_brewer(palette = "Paired") + theme(panel.background = element_rect(fill = "lightyellow"))

#ggplot(manufacture_cfs) + geom_bar(aes(x=SHIPMT_DIST_ROUTED), color= "darkgreen", fill="pink") + xlab("Miles") + ylab("Number of Cases") + ggtitle("How Far Are Our Manufacturers Exporting/Importing?")

manufacture_cfs$QUARTER <- as.factor(manufacture_cfs$QUARTER)

levels(manufacture_cfs$QUARTER) <- c("First","Second","Third","Fourth")

ggplot(manufacture_cfs) + geom_bar(aes(x=QUARTER), color= "black", fill="yellow") + 
  ggtitle("What Time of Year Our Manufacturers Busiest?")

ggplot(manufacture_cfs, aes(Industry, group=QUARTER)) + 
  geom_bar(aes(colour=QUARTER, fill=QUARTER), alpha=0.9) +
  xlab("Quarter") + ylab("Number of Cases") + 
  ggtitle("During What Quarters and In What Industries are Our Manufacturers Importing/Exporting?") +
  scale_fill_brewer(palette = "Paired") + scale_colour_brewer(palette = "Paired") + theme(panel.background = element_rect(fill = "lightyellow")) + coord_flip()

#PLOTLY #1
plotly1 <- manufacture_cfs %>% 
  count(Industry, ORIG_STATE, QUARTER) %>% 
  plot_ly(x = ORIG_STATE, y= n, type= "bar", color= QUARTER) %>%
  layout(
    title = "Quarterly Manufacturing Activities by ORIGIN State",
    xaxis = list(title = "State"),
    margin = list(l = 100))

plotly1

#PLOTLY #2
plotly2 <- manufacture_cfs %>% 
  count(Industry, DEST_STATE, QUARTER) %>% 
  plot_ly(x = DEST_STATE, y= n, type= "bar", color= QUARTER) %>%
  layout(
    title = "Quarterly Manufacturing Activities by DESTINATION State",
    xaxis = list(title = "State"),
    margin = list(l = 100))

plotly2

#PLOTLY #3

plotly3 <- manufacture_cfs %>% 
  count(Industry, ORIG_STATE, DEST_STATE, QUARTER, MODE) %>% 
  plot_ly(x = MODE, y= DEST_STATE, type= "bar", color=MODE)

plotly3


####################
#commodityflow$Country <- rep(NA)
#commodityflow$Country <- ifelse(commodityflow$EXPORT_CNTRY=="C","Canada", commodityflow$Country)
#commodityflow$Country <- ifelse(commodityflow$EXPORT_CNTRY=="M","Mexico", commodityflow$Country)

#dim(commodityflow)

#together2 <- commodityflow

#changing class type from character to factor
#together2$Country <- as.factor(together2$Country)

#levels(together2$Country)

#we are only looking at exports from UN into Canada and the Mexico so removing all EXPORT columns for Canada and Mexico
#together2 <- together2[,c(1:36,50:51)]

##Plots

#Counts of how many exports to Canada and Mexico
#total <- together2 %>%
group_by(Country) %>%
  summarize(
    Count = n(), na.rm=TRUE) %>%
  arrange(desc(Count))

#total

#ggplot(together2) + geom_bar(aes(x=Country), color= "black", fill="indianred")

#ggplot(together2, aes(Country, group=HAZMAT)) + 
geom_bar(aes(colour=HAZMAT, fill=HAZMAT), alpha=0.9) +
  xlab("Country") + ylab("Number of Cases") + 
  ggtitle("What Types of Materials are we Exporting to Canada and Mexico?") +
  scale_fill_brewer(palette = "Paired") + scale_colour_brewer(palette = "Paired") + theme(panel.background = element_rect(fill = "lightyellow"))

#summary(together2$NAICS)

#togetherall <- together2

#names(togetherall)

#together2$NAFTA_mex <- as.factor(together2$NAFTA_mex)
#ggplot(together2) + geom_bar(aes(x=NAFTA_mex), color= "black", fill="pink") + 
ggtitle("How many of our U.S. Exports to Mexico are through NAFTA?")

#together2$NAFTA_can <- as.factor(together2$NAFTA_can)
#ggplot(together2) + geom_bar(aes(x=NAFTA_can), color= "black", fill="red") + 
ggtitle("How many of our U.S. Exports to Canada are through NAFTA?")

#summary(together2$NAICS)

#ggplot(together2) + geom_bar(aes(x=NAICS), color= "black", fill="blue") + 
ggtitle("What U.S. Industries are Exporting to Canada and Mexico?") + xlim(310,341) #lowest is 212 though!

#summary(together2)

#ggplot(together2) + geom_bar(aes(x=NAICS), color= "black", fill="blue") + 
ggtitle("What U.S. Industries are Exporting to Canada and Mexico?") + xlim(300,350) #lowest is 212 though!

#ORIGIN STATES (since only exports)

together2$ORIG_STATE <- as.factor(together2$ORIG_STATE)
levels(together2$ORIG_STATE) <- c("AL"	,
                                  "AK"	,
                                  "AZ"	,
                                  "AR"	,
                                  "CA"	,
                                  "CO"	,
                                  "CT"	,
                                  "DE"	,
                                  "DC"	,
                                  "FL"	,
                                  "GA"	,
                                  "HI"	,
                                  "ID"	,
                                  "IL"	,
                                  "IN"	,
                                  "IA"	,
                                  "KS"	,
                                  "KY"	,
                                  "LA"	,
                                  "ME"	,
                                  "MD"	,
                                  "MA"	,
                                  "MI"	,
                                  "MN"	,
                                  "MS"	,
                                  "MO"	,
                                  "MT"	,
                                  "NE"	,
                                  "NV"	,
                                  "NH"	,
                                  "NJ"	,
                                  "NM"	,
                                  "NY"	,
                                  "NC"	,
                                  "ND"	,
                                  "OH"	,
                                  "OK"	,
                                  "OR"	,
                                  "PA"	,
                                  "RI"	,
                                  "SC"	,
                                  "SD"	,
                                  "TN"	,
                                  "TX"	,
                                  "UT"	,
                                  "VT"	,
                                  "VA"	,
                                  "WA"	,
                                  "WV"	,
                                  "WI"	,
                                  "WY"	)