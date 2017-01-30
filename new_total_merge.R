library(plyr)
library(dplyr)
library(sp)
library(maptools)
library(raster)


dollarsComma <- function(x){
  x <- round(x, 2)
  x <- prettyNum(x, big.mark = ",")
  x <- paste0("$", x)
  return(x)
}
options(scipen = 999)

##Merge Total Parcel csv generated from REALESTATE to WPRDC parcel/assessment data
load.new_parcel <- read.csv("./test_city_parcels.csv")
centroids <- read.csv("./august_parcelcentroids.csv")
centroids <- subset(centroids, geo_name_cousub == "Pittsburgh city")
new_merged <- merge(load.new_parcel, centroids, by.x = "COUNTY_PIN", by.y = "PIN", all = TRUE)
new_merged <- subset(new_merged, NEIGHBORHOOD != 'MOUNT OLIVER BOROUGH')
new_merged <- subset(new_merged, select = c('COUNTY_PIN', 'CITY_PIN', 'OWNER', 'ADDRESS', 'PROP_ZIP', 'LAST_SALE_DATE', 'BANKRUPT_FLAG', 'NEIGHBORHOOD',
                                               'HOMESTEAD', 'CURRENT_DELQ_TAX', 'PROGRAM_NAME', 'ABATEMENT_LENGTH', 'START_YEAR', 'APPROVED_USER',
                                               'MAPBLOCKLO', 'geo_name_nhood', 'Pgh_Ward', 'x', 'y'))
missing_x <- subset(new_merged, is.na(new_merged$x))

city_assessed <- read.csv('./city_assessed.csv')
new_merged <- merge(new_merged, city_assessed, by.x = "COUNTY_PIN", by.y = "PARID", all.x = TRUE)
new_merged <- subset(new_merged, select = c('COUNTY_PIN', 'CITY_PIN', 'OWNER', 'ADDRESS', 'PROP_ZIP', 'geo_name_nhood', 'Pgh_Ward', 'MUNIDESC',
                                            'TAXDESC', 'OWNERDESC', 'CLASSDESC', 'USEDESC', 'SALEDATE', 'SALEPRICE', 'COUNTYBUILDING', 'COUNTYLAND',
                                            'COUNTYTOTAL', 'BANKRUPT_FLAG', 'HOMESTEAD', 'CURRENT_DELQ_TAX', 'PROGRAM_NAME', 'ABATEMENT_LENGTH', 'START_YEAR',
                                            'APPROVED_USER', 'x', 'y'))




##Drop 12 missing records and change column names
new_merged <- new_merged[-c(1:12), ]
names(new_merged)[names(new_merged) == "CNTY_OWNER_NAME"] <- "OWNER"
names(new_merged)[names(new_merged) == "geo_name_nhood"] <- "Neighborhood"
names(new_merged)[names(new_merged) == "Pgh_Ward"] <- "Ward"
names(new_merged)[names(new_merged) == "city_tax"] <- "City_Tax_Due2017"
names(new_merged)[names(new_merged) == "school_tax"] <- "School_Tax_Due2017"
names(new_merged)[names(new_merged) == "lib_tax"] <- "Library_Tax_Due2017"


##Calculate taxes owed (try to find better way) and add dollars/commas to values

new_merged$city_tax <- ifelse(new_merged$HOMESTEAD == "H", (new_merged$COUNTYTOTAL - 15000)*0.00806, (new_merged$COUNTYTOTAL * 0.00806))
new_merged$school_tax <- ifelse(new_merged$HOMESTEAD == "H", (new_merged$COUNTYTOTAL - 29447)*0.00984, (new_merged$COUNTYTOTAL * 0.00984))
new_merged$lib_tax <- ifelse(new_merged$HOMESTEAD == "H", (new_merged$COUNTYTOTAL - 15000)*0.00025, (new_merged$COUNTYTOTAL * 0.00025))
new_merged$city_tax <- dollarsComma(new_merged$city_tax)
new_merged$school_tax <- dollarsComma(new_merged$school_tax)
new_merged$lib_tax <- dollarsComma(new_merged$lib_tax)
new_merged$SALEPRICE <- dollarsComma(new_merged$SALEPRICE)
new_merged$COUNTYBUILDING <- dollarsComma(new_merged$COUNTYBUILDING)
new_merged$COUNTYLAND <- dollarsComma(new_merged$COUNTYLAND)
new_merged$COUNTYTOTAL <- dollarsComma(new_merged$COUNTYTOTAL)



##Edit tax abatement information
new_merged$PROGRAM_NAME <- as.character(new_merged$PROGRAM_NAME)
new_merged$PROGRAM_NAME[new_merged$PROGRAM_NAME==""] <- "NA"
new_merged$PROGRAM_NAME <- as.factor(new_merged$PROGRAM_NAME)
new_merged$APPROVED_USER <- as.character(new_merged$APPROVED_USER)
new_merged$APPROVED_USER[new_merged$APPROVED_USER==""] <- "NA"
new_merged$APPROVED_USER <- as.factor(new_merged$APPROVED_USER)

##Map city region based on municipal description

new_merged <- transform(new_merged, REGION = as.factor(mapvalues(MUNIDESC, c("1st Ward  - PITTSBURGH", "2nd Ward - PITTSBURGH", "3rd Ward - PITTSBURGH", "4th Ward - PITTSBURGH",
                                                                         "5th Ward - PITTSBURGH", "6th Ward - PITTSBURGH", "7th Ward - PITTSBURGH", "8th Ward - PITTSBURGH",
                                                                         "9th Ward - PITTSBURGH", "10th Ward - PITTSBURGH", "11th Ward - PITTSBURGH", "12th Ward - PITTSBURGH",
                                                                         "13th Ward - PITTSBURGH", "14th Ward - PITTSBURGH", "15th Ward - PITTSBURGH", "16th Ward - PITTSBURGH",
                                                                         "17th Ward - PITTSBURGH", "18th Ward - PITTSBURGH", "19th Ward - PITTSBURGH", "20th Ward - PITTSBURGH",
                                                                         "21st Ward - PITTSBURGH", "22nd Ward - PITTSBURGH", "23rd Ward - PITTSBURGH", "24th Ward - PITTSBURGH",
                                                                         "25th Ward - PITTSBURGH", "26th Ward - PITTSBURGH", "27th Ward - PITTSBURGH", "28th Ward - PITTSBURGH",
                                                                         "29th Ward - PITTSBURGH", "30th Ward - PITTSBURGH", "31st Ward - PITTSBURGH", "32nd Ward - PITTSBURGH"),
                                                             c("East End", "East End", "East End", "East End", "East End", "East End", "East End",
                                                               "East End", "East End", "East End", "East End", "East End", "East End", "East End", "East End",
                                                               "South Hills/Side", "South Hills/Side", "South Hills/Side", "South Hills/Side", "West End",
                                                               "North Side", "North Side", "North Side", "North Side", "North Side", "North Side", "North Side",
                                                               "West End", "South Hills/Side", "South Hills/Side", "South Hills/Side", "South Hills/Side"))))

write.csv(new_merged, './all_city_parcels.csv')

missing_ward <- subset(new_merged, is.na(new_merged$REGION))
write.csv(missing_ward, './missing_ward_parcels.csv')

missing_city_parcel <- subset(new_merged, is.na(new_merged$CITY_PIN))
write.csv(missing_city_parcel, './missing_city_parcelID.csv')

missing_nhood <- subset(new_merged, is.na(new_merged$Neighborhood))

