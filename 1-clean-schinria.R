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
fips.code <- read.csv("cfs_dictionary.csv")
naics <- read.csv("NAICS.csv")
sctg <- read.csv("SCTG.csv")
mt <- read.csv("MT.csv")
cfs <- fread("2012_cfs.txt")

#2. Recode variables in CFS file

#FIPS codes for CFS area
cfs$ORIG_CFS_name <- fips.code$description[match(cfs$ORIG_CFS_AREA, fips.code$state_cfs)]
cfs$DEST_CFS_name <- fips.code$description[match(cfs$DEST_CFS_AREA, fips.code$state_cfs)]

#Add Industry
cfs$Industry <- naics$description[match(cfs$NAICS, naics$naics)]

#SCTG (Standard Classification of Transported Goods) codes
cfs$SCTG_name <- sctg$description[match(cfs$SCTG, sctg$sctg)]

#MT (Mode of Transportation) codes
cfs$MODE_name <- mt$description[match(cfs$MODE, mt$mode_code)]

#Subset out Manufacturing
manufacture_cfs <- cfs[grep("manufacturing",cfs$Industry),]

#################################
#~Schinria Analysis Starts Here~#
#################################

#Read in file that will match county FIPS with State abbreviations
state.code <- read.csv("FIPS_Code.csv")

#2. Recode variables in CFS file

#State codes
colnames(state.code) <- c("State", "StateFP", "CountyFP", "County Name", "ClassFP")
manufacture_cfs$ORIG_STATE_name <- state.code$State[match(manufacture_cfs$ORIG_STATE, state.code$StateFP)]
manufacture_cfs$DEST_STATE_name <- state.code$State[match(manufacture_cfs$DEST_STATE, state.code$StateFP)]

#Exports to Canada
canada_exp <- manufacture_cfs[grep("C",manufacture_cfs$EXPORT_CNTRY),]

#Exports to Canada
mexico_exp <- manufacture_cfs[grep("M",manufacture_cfs$EXPORT_CNTRY),]

#All Exports Out of Country
ofc_exp <- rbind(canada_exp, mexico_exp)

#Exports to Canada by Origin State
install.packages(c("choroplethr"))
library(choroplethr)

#Frequency of exports to Canada by state abbreviation
canada_total_map <- as.data.frame(table(canada_exp$ORIG_STATE_name))

#Adding a column "region" with state name
canada_total_map$region <- tolower(state.name[match(canada_total_map$Var1,state.abb)])

#Chorophleth reads frequencies from a "value" column
colnames(canada_total_map)[2] <- "value"

#District of Column is missing, so we add it
canada_total_map$region[which(is.na(canada_total_map$region))] <- "district of columbia"

#Preview some lines of our dataframe
head(canada_total_map)

#Save our image as a png within our working directory
png("US_States_Exports_Canada.png", width=12, height=8, units="in", res=300)
state_choropleth(canada_total_map, title = "2012 Manufacturing Exports to Canada", legend = "Number of Exports")
dev.off()

#Frequency of exports to Mexico by state abbreviation
mexico_total_map <- as.data.frame(table(mexico_exp$ORIG_STATE_name))

#Adding a column "region" with state name
mexico_total_map$region <- tolower(state.name[match(mexico_total_map$Var1,state.abb)])

#Chorophleth reads frequencies from a "value" column
colnames(mexico_total_map)[2] <- "value"

#In the case that istrict of Column is missing, we add it
mexico_total_map$region[which(is.na(mexico_total_map$region))] <- "district of columbia"

#Preview some lines of our dataframe
head(mexico_total_map)

#Save our image as a png within our working directory
png("US_States_Exports_Mexico.png", width=12, height=8, units="in", res=300)
state_choropleth(mexico_total_map, title = "2012 Manufacturing Exports to Mexico", legend = "Number of Exports")
dev.off()

library(dplyr)

#Frequency of exports to Canada by Industry
canada_exp %>%
  group_by(Industry) %>%
  summarize(
    Count = n(), na.rm=TRUE) %>%
  arrange(desc(Count))

#Frequency of exports to Mexico by Industry
mexico_exp %>%
  group_by(Industry) %>%
  summarize(
    Count = n(), na.rm=TRUE) %>%
  arrange(desc(Count))

#Wordcloud for Canada
library(tm)
library(wordcloud)

#Extract industry only
canada_ind <- sapply(canada_exp$Industry, function(row) iconv(row, "latin1", "ASCII", sub=""))

#to get all of the exports together
canada_corp <- paste(unlist(canada_ind), collapse =" ") 

#create a corpus
canada_corp <- Corpus(VectorSource(canada_corp))
canada_corp <- tm_map(canada_corp, PlainTextDocument)
canada_corp <- tm_map(canada_corp, removePunctuation)
canada_corp <- tm_map(canada_corp, stripWhitespace)
canada_corp <- tm_map(canada_corp, stemDocument)

#What words do we want the wordcloud to ignore?
mystopwords <- c("manufacturing","manufactur","product")
canada_corp <- tm_map(canada_corp, removeWords, c(stopwords('english'),mystopwords))

#Save our image as a png within our working directory
png("Mfg_Exports_Canada_Cloud.png", width=12, height=8, units="in", res=300)
wordcloud(canada_corp, max.words=200, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()

#Wordcloud for Mexico

#Extract industry only
mexico_ind <- sapply(mexico_exp$Industry, function(row) iconv(row, "latin1", "ASCII", sub=""))

#to get all of the exports together
mexico_corp <- paste(unlist(mexico_ind), collapse =" ") #to get all of the exports together

#create a corpus
mexico_corp <- Corpus(VectorSource(mexico_corp))
mexico_corp <- tm_map(mexico_corp, PlainTextDocument)
mexico_corp <- tm_map(mexico_corp, removePunctuation)
mexico_corp <- tm_map(mexico_corp, stripWhitespace)
mexico_corp <- tm_map(mexico_corp, stemDocument)

#What words do we want the wordcloud to ignore?
mystopwords <- c("manufacturing","manufactur","product")
mexico_corp <- tm_map(mexico_corp, removeWords, c(stopwords('english'),mystopwords))

#Save our image as a png within our working directory
png("Mfg_Exports_Mexico_Cloud.png", width=12, height=8, units="in", res=300)
wordcloud(mexico_corp, max.words=200, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()

#corpus of both
ofc_corp <- c(mexico_corp, canada_corp)
ofc_corp <- Corpus(VectorSource(ofc_corp))
tdm_ofc <- TermDocumentMatrix(ofc_corp)
tdm_ofc <- as.matrix(tdm_ofc)
colnames(tdm_ofc) <- c("Mexico", "Canada")

#Commonality cloud
png("Mfg_Commonality_Cloud.png", width=12, height=8, units="in", res=300)
commonality.cloud(tdm_ofc, random.order=FALSE, title.size=1.0, max.words=500)
dev.off()


#Comparison cloud
png("Mfg_Comparison_Cloud.png", width=12, height=8, units="in", res=300)
comparison.cloud(tdm_ofc, random.order=FALSE, title.size=1.0, max.words=500)
dev.off()
