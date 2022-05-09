# Clay Ford
# jcf2d@virginia.edu

# getting and munging albemarle county homes data

library(tidyverse)

# Albemarle county
# Office of Geographic Data Services
# https://www.albemarle.org/government/community-development/gis-mapping/gis-data

# Real Estate Information
# Under PARCELS - Primary Card Level Data - this file includes data
# such as year built, finished square footage, number of rooms, and condition. 
# https://gisweb.albemarle.org/gisdata/CAMA/GIS_CardLevelData_new_TXT.zip

setwd("data/")
link <- "https://gisweb.albemarle.org/gisdata/CAMA/GIS_CardLevelData_new_TXT.zip"
download.file(link, destfile = basename(link))
unzip(basename(link)) # extract file to working directory


# Real Estate Information - Parcel Level Data.  This file contains information
# about the parcel itself such as owner information, deed acreage value, and
# assessed value

# https://gisweb.albemarle.org/gisdata/CAMA/GIS_View_Redacted_ParcelInfo_TXT.zip

link2 <- "https://gisweb.albemarle.org/gisdata/CAMA/GIS_View_Redacted_ParcelInfo_TXT.zip"
download.file(link2, destfile = basename(link2))
unzip(basename(link2)) # extract file to working directory

# Other Parcel Characteristics

# This file contains other parcel information that is managed in our development
# tracking system (e.g. Zoning, School Districts, Jurisdictional Areas, etc.).

link3 <- "https://gisweb.albemarle.org/gisdata/CAMA/CityView_View_OtherParcelCharacteristics_TXT.zip"
download.file(link3, destfile = basename(link3))
unzip(basename(link3)) # extract file to working directory

file.remove("GIS_View_Redacted_ParcelInfo_TXT.zip")
file.remove("GIS_CardLevelData_new_TXT.zip")
file.remove("CityView_View_OtherParcelCharacteristics_TXT.zip")

card_level <- read.csv("GIS_CardLevelData_new.txt", 
                       na.strings = "NULL")
parcel_level <- read.csv("GIS_View_Redacted_ParcelInfo.txt", 
                         na.strings = "NULL")
other_parcel <- read.csv("CityView_View_OtherParcelCharacteristics.txt", 
                         na.strings = c("NULL", "N/A"))

# card_level list of variables to keep
vars_card <- c("TMP", "CardNum", "YearBuilt", "YearRemodeled", 
          "UseCode", "Condition", "NumStories", "FinSqFt", "Cooling", 
          "FP_Open", "Bedroom", "FullBath", "HalfBath", "TotalRooms")

# select listed variables, add variable for source
# keep only residential home records (e.g., not businesses, apartment complexes)
# keep only records with CardNum = 1
card <- card_level %>% 
  select(all_of(vars_card)) %>% 
  filter(UseCode %in% c("Duplex", "Single Family", "Single Family-Rental")) %>% 
  filter(CardNum == 1)

# parcel_level list of variables to keep
vars_parcel <- c("ParcelID", "LotSize", "LandValue", "ImprovementsValue", 
          "TotalValue", "LastSalePrice", "LastSaleDate1", 
          "Cards")

# select listed variables for parcel level
# drop records with 2 or cards
# a Parcel of land can have more than one building (ie, card) on it;
# we only want parcels with one building

parcel <- parcel_level %>% 
  select(all_of(vars_parcel)) %>% 
  filter(Cards < 2)

# select other parcel variables
other <- other_parcel %>% 
  select(ParcelID, ESDistrict:HSDistrict, CensusTract) 

# clean up
rm(card_level, parcel_level, other_parcel, vars_card, vars_parcel, link, link2, link3)

# merge all three data sets
homes <- left_join(card, parcel, by = c("TMP" = "ParcelID")) %>% 
  left_join(other, by = c("TMP" = "ParcelID"))

# keep rows with TotalValue not missing and greater than 0
homes <- homes %>% 
  filter(!is.na(TotalValue) & TotalValue > 0)

# keep rows with FinSqFt not missing and greater than 0
homes <- homes %>% 
  filter(!is.na(FinSqFt) & FinSqFt > 0)

# more likely to use this as age than year, create age of home
homes <- homes %>% 
  mutate(Age = 2022 - YearBuilt)

# condition
table(homes$Condition, useNA = "ifany") # re-order levels of factor

# combine unknown and NA into none and relevel
homes <- homes %>% 
  mutate(Condition = if_else(is.na(Condition) | 
                               Condition == "Unknown",
                             "None",
                             Condition))

# set condition to factor
cond_levels <- c("Very Poor", "Poor", "Fair", "Average", "Good", 
                 "Excellent", "None") # define levels/order

homes <- homes %>% 
  mutate(Condition = fct_relevel(Condition, cond_levels))

table(homes$Condition)

# yearremodeled -> remodel indicator
summary(homes$YearRemodeled) # NA not remodeled; not sure about 8
homes <- homes %>% 
  mutate(Remodeled = if_else(!is.na(YearRemodeled), 1, 0))

# drop NumStories and Cards
homes <- homes %>% select(-c(NumStories, Cards))

# cooling
table(homes$Cooling) # fix factor -- assume 00, M1, and NULL are no air
homes <- homes %>% 
  mutate(Cooling = fct_collapse(Cooling, 
                                "No Central Air" = c("00", "M1", "")))


# fp_open (these are characters)
table(homes$FP_Open) # make a binary indicator, 0 and Null are none
homes <- homes %>% 
  mutate(FP = if_else(FP_Open == 0, 0, 1))

homes <- homes %>% 
  select(-c(TMP, CardNum))

rm(cond_levels)

# save everything to working directory
save.image("albemarle_homes_2022.Rdata") 
# load("albemarle_homes_2020.Rdata")

# save just the homes data frame 
saveRDS(homes, file = "albemarle_homes_2022.rds") 
# readRDS("albemarle_homes_2022.rds")

# save a csv file of the homes data
write_csv(homes, file = "albemarle_homes_2022.csv") 

file.remove(c("CityView_View_OtherParcelCharacteristics.txt", 
              "GIS_CardLevelData_new.txt", 
              "GIS_View_Redacted_ParcelInfo.txt"))

# homes <- read_csv("albemarle_homes_2020.csv")
