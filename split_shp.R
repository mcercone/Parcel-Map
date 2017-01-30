library(sp)
library(maptools)
library(raster)
library(dplyr)

#total_parcels <- read.csv('./total_properties_1.csv')
#parcels <- readShapeSpatial("allegheny_county_parcel_boundaries.shp")
#cp_parcel_2 <- merge(parcels, total_parcels, by.x = "pin", by.y = "PARCEL_ID", duplicateGeoms = TRUE)

##Subset City shp into four regional spatialpolygondataframe
#ee_parcel <- subset(cp_parcel_2, REGION == "East End")
#we_parcel <- subset(cp_parcel_2, REGION == "West End")
#ns_parcel <- subset(cp_parcel_2, REGION == "North Side")
#sh_parcel <- subset(cp_parcel_2, REGION == "South Hills/Side")

##Write four regional shp
#writeSpatialShape(ee_parcel, "east_end_parcels")
#writeSpatialShape(we_parcel, "west_end_parcels")
#writeSpatialShape(ns_parcel, "north_side_parcels")
#writeSpatialShape(sh_parcel, "south_hill_parcels")


##Testing files with updated fields -- delete above if app runs correctly
city_parcels <- read.csv('./all_city_parcels.csv')
#names(city_parcels)[names(load.new_parcel) == "COUNTY_PIN"] <- "PIN"
parcels <- readShapeSpatial("allegheny_county_parcel_boundaries.shp")
cp_parcel_2 <- merge(parcels, city_parcels, by.x = "pin", by.y = "COUNTY_PIN", duplicateGeoms = TRUE)
cp_parcel_2 <- subset(cp_parcel_2, !is.na(cp_parcel_2$x))



##Subset City shp into four regional spatialpolygondataframe
ee_parcel <- subset(cp_parcel_2, REGION == "East End")
we_parcel <- subset(cp_parcel_2, REGION == "West End")
ns_parcel <- subset(cp_parcel_2, REGION == "North Side")
sh_parcel <- subset(cp_parcel_2, REGION == "South Hills/Side")



ee_parcel$Neighborhood <- as.character(ee_parcel$Neighborhood)
ee_parcel$Neighborhood <- as.factor(ee_parcel$Neighborhood)
ee_parcel$CURRENT_DELQ_TAX[is.na(ee_parcel$CURRENT_DELQ_TAX)] <- 0
ee_parcel$PROGRAM_NAME <- as.character(ee_parcel$PROGRAM_NAME)
ee_parcel$PROGRAM_NAME[is.na(ee_parcel$PROGRAM_NAME)] <- "No Abatement"
ee_parcel$color_val <- ifelse(ee_parcel$PROGRAM_NAME == "No Abatement", "#ccc7c7", "#19eebe")
ee_parcel$color_val <- ifelse(ee_parcel$OWNER == "URBAN REDEVELOPMENT AUTH OF PITTSBURGH", "#a820b8", ee_parcel$color_val)
ee_parcel$color_val <- ifelse(ee_parcel$OWNER == "CITY OF PITTSBURGH", "#f2f403", ee_parcel$color_val)
ee_parcel$color_val <- ifelse(ee_parcel$CURRENT_DELQ_TAX > 0, "#f20303", ee_parcel$color_val)

ns_parcel$Neighborhood <- as.character(ns_parcel$Neighborhood)
ns_parcel$Neighborhood <- as.factor(ns_parcel$Neighborhood)
ns_parcel$CURRENT_DELQ_TAX[is.na(ns_parcel$CURRENT_DELQ_TAX)] <- 0
ns_parcel$PROGRAM_NAME <- as.character(ns_parcel$PROGRAM_NAME)
ns_parcel$PROGRAM_NAME[is.na(ns_parcel$PROGRAM_NAME)] <- "No Abatement"
ns_parcel$color_val <- ifelse(ns_parcel$PROGRAM_NAME == "No Abatement", "#ccc7c7", "#19eebe")
ns_parcel$color_val <- ifelse(ns_parcel$OWNER == "URBAN REDEVELOPMENT AUTH OF PITTSBURGH", "#a820b8", ns_parcel$color_val)
ns_parcel$color_val <- ifelse(ns_parcel$OWNER == "CITY OF PITTSBURGH", "#f2f403", ns_parcel$color_val)
ns_parcel$color_val <- ifelse(ns_parcel$CURRENT_DELQ_TAX > 0, "#f20303", ns_parcel$color_val)

we_parcel$Neighborhood <- as.character(we_parcel$Neighborhood)
we_parcel$Neighborhood <- as.factor(we_parcel$Neighborhood)
we_parcel$CURRENT_DELQ_TAX[is.na(we_parcel$CURRENT_DELQ_TAX)] <- 0
we_parcel$PROGRAM_NAME <- as.character(we_parcel$PROGRAM_NAME)
we_parcel$PROGRAM_NAME[is.na(we_parcel$PROGRAM_NAME)] <- "No Abatement"
we_parcel$color_val <- ifelse(we_parcel$PROGRAM_NAME == "No Abatement", "#ccc7c7", "#19eebe")
we_parcel$color_val <- ifelse(we_parcel$OWNER == "URBAN REDEVELOPMENT AUTH OF PITTSBURGH", "#a820b8", we_parcel$color_val)
we_parcel$color_val <- ifelse(we_parcel$OWNER == "CITY OF PITTSBURGH", "#f2f403", we_parcel$color_val)
we_parcel$color_val <- ifelse(we_parcel$CURRENT_DELQ_TAX > 0, "#f20303", we_parcel$color_val)

sh_parcel$Neighborhood <- as.character(sh_parcel$Neighborhood)
sh_parcel$Neighborhood <- as.factor(sh_parcel$Neighborhood)
sh_parcel$CURRENT_DELQ_TAX[is.na(sh_parcel$CURRENT_DELQ_TAX)] <- 0
sh_parcel$PROGRAM_NAME <- as.character(sh_parcel$PROGRAM_NAME)
sh_parcel$PROGRAM_NAME[is.na(sh_parcel$PROGRAM_NAME)] <- "No Abatement"
sh_parcel$color_val <- ifelse(sh_parcel$PROGRAM_NAME == "No Abatement", "#ccc7c7", "#19eebe")
sh_parcel$color_val <- ifelse(sh_parcel$OWNER == "URBAN REDEVELOPMENT AUTH OF PITTSBURGH", "#a820b8", sh_parcel$color_val)
sh_parcel$color_val <- ifelse(sh_parcel$OWNER == "CITY OF PITTSBURGH", "#f2f403", sh_parcel$color_val)
sh_parcel$color_val <- ifelse(sh_parcel$CURRENT_DELQ_TAX > 0, "#f20303", sh_parcel$color_val)

bv_par <- subset(sh_parcel, Neighborhood == "Beechview")


##Write four regional shp
writeSpatialShape(ee_parcel, "east_end_parcels")
writeSpatialShape(we_parcel, "west_end_parcels")
writeSpatialShape(ns_parcel, "north_side_parcels")
writeSpatialShape(sh_parcel, "south_hill_parcels")

