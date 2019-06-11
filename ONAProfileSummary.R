#Created by: Nilanjana Bhattacharya 
#Title: ONA data clean up and correction to polygon information
#Step 1 (THIS CODE): Run this code on the settlement profile
#Step 2: Open the modified file on QGIS and save as .shp file
#Step 3: Run Settlement Summary code to view indices related to each city

#BASIC PROFILE INFORMATION --------------------------------------------------------------------

rm(list = ls())
setwd("E:\\Open Reblock\\Datasets")
library("tidyverse")
library("stringi")
library("lubridate")

#Enter filename of settlement profile downloaded from ONA
file_input <- ("sierraleaone.csv")  
df <- read.csv(file_input, stringsAsFactors = FALSE)

#GPS COORDINATIONS
dfout <- data.frame(df$section_B._B1_GPS_latitude, df$section_B._B1_GPS_longitude)
colnames(dfout) <- c("lat", "long")
dfout$lat[dfout$lat=="n/a"] = NA
dfout$long[dfout$long=="n/a"] = NA

#Country
arr <- data.frame(str_split_fixed(df$section_B.B3_Country, "_", 2))
dfout$country <- arr[,2]

#Settlement Name
dfout$settlementname <- df$section_B.B7_Settlement_Name_Community

#Area of the settlement
dfout$areaacres <- as.numeric(df$section_B.B2b_Area_acres)

#Shape of the polygon
str1 <- "POLYGON(("
str2 <- "))"
#Manipulating to switch the lat and long variables 
df$verification.A0_Boundary[df$verification.A0_Boundary=="n/a"] <- NA
temp <- str_split(df$verification.A0_Boundary, ";") #CHANGE THE SEPARATOR BASIS THE INPUT FILE
for (i in 1:length(temp)){
  for(j in 1:length(temp[[i]])){
    temp2 <- str_split(temp[[i]][j], " ", 4)
    temp[[i]][j] <- paste(temp2[[1]][2],temp2[[1]][1])
  }
}
for (i in 1:nrow(df)){
  dfout$wkt[i] <- stri_join_list(as.list(temp[[i]]), collapse = ", ")
  dfout$wkt[i] <- paste0(str1, dfout$wkt[i], str2)
}

#Age of settlement 
dfout$age <- year(today()) - year(as.Date(df$section_B.B10b_Year_Established))

#DEMOGRAPHICS --------------------------------------------------------------

#Population Density 
dfout$popdensity <- as.numeric(df$section_C.C14_density)
#Density cannot be 1000 persons per acre, adjust this threshold to remove outliers 
dfout$popdensity[dfout$popdensity>1000] <- NA
dfout$popdensity[dfout$popdensity<0] <- NA

#Household Size
dfout$hholdsize <- as.numeric(df$section_C.C10_Household_Size)

#STRUCTURE DETAILS ----------------------------------------------------------

dfout$permstructures <- 100*as.numeric(df$section_C.C6_Structures_Permanent)/(as.numeric(df$section_C.C6_Structures_Permanent) + as.numeric(df$section_C.C7_Structures_Temporary))
dfout$tempstructures <- 100*as.numeric(df$section_C.C7_Structures_Temporary)/(as.numeric(df$section_C.C6_Structures_Permanent) + as.numeric(df$section_C.C7_Structures_Temporary))
dfout$permstructures[is.na(dfout$permstructures)]=NA
dfout$tempstructures[is.na(dfout$tempstructures)]=NA

##ENVIRONMENT ---------------------------------------------------------------

#Hazards: How many hazards exist in the settlement? 
#Public infrastructure (railway tracks/under power lines/canals/roadside/open drains) or
#Physical (Water body/flood prone area/sloped zone/sinking soil) or
#Garbage dump or
#Industrial dumping area: Isolated for toxic waste area 
dfout$publicgoodhazard <- ifelse(as.logical(df$section_D.D1_Location_Problems.railway_track),1,0)+
  ifelse(as.logical(df$section_D.D1_Location_Problems.under_power_lines),1,0)+
  ifelse(as.logical(df$section_D.D1_Location_Problems.canal),1,0)+
  ifelse(as.logical(df$section_D.D1_Location_Problems.road_side),1,0)+
  ifelse(as.logical(df$section_D.D1_Location_Problems.open_drains),1,0)
dfout$physical <-  ifelse(as.logical(df$section_D.D1_Location_Problems.water_body),1,0)+
  ifelse(as.logical(df$section_D.D1_Location_Problems.flood_prone_area),1,0)+
  ifelse(as.logical(df$section_D.D1_Location_Problems.slope),1,0)+
  ifelse(as.logical(df$section_D.D1_Location_Problems.sinking_soil),1,0)
dfout$garbagedump <- ifelse(as.logical(df$section_D.D1_Location_Problems.garbage_dump),1,0)
dfout$industrial <- ifelse(as.logical(df$section_D.D1_Location_Problems.industrial_hazards),1,0)+
  ifelse(as.logical(df$section_D.D1_Location_Problems.mine_dump),1,0)
  
#Disasters in the last year: Fires, Floods are isolated; Others are clubbed together
#Fires
dfout$firefrequency <- ifelse(df$section_D.D4a_Distaster_Frequency=="more_than_three",3,ifelse(df$section_D.D4a_Distaster_Frequency=="twice",2,ifelse(df$section_D.D4a_Distaster_Frequency=="once",1,0)))
dfout$firestructures <- ifelse(df$section_D.D4a_Structures_Destroyed=="more_than_10",15,ifelse(df$section_D.D4a_Structures_Destroyed=="6_10",8,ifelse(df$section_D.D4a_Structures_Destroyed=="1_5",3,0)))  
#Floods
dfout$floodfrequency <- ifelse(df$section_D.D4b_Distaster_Frequency=="more_than_three",3,ifelse(df$section_D.D4b_Distaster_Frequency=="twice",2,ifelse(df$section_D.D4b_Distaster_Frequency=="once",1,0)))
dfout$floodstructures <- ifelse(df$section_D.D4b_Structures_Destroyed=="more_than_10",15,ifelse(df$section_D.D4b_Structures_Destroyed=="6_10",8,ifelse(df$section_D.D4b_Structures_Destroyed=="1_5",3,0)))  
#Structures destroyed by other disasters
dfout$otherdistasterstructures <- ifelse(df$section_D.D4c_Structures_Destroyed=="more_than_10",15,ifelse(df$section_D.D4c_Structures_Destroyed=="6_10",8,ifelse(df$section_D.D4c_Structures_Destroyed=="1_5",3,0)))+
  ifelse(df$section_D.D4d_Structures_Destroyed=="more_than_10",15,ifelse(df$section_D.D4d_Structures_Destroyed=="6_10",8,ifelse(df$section_D.D4d_Structures_Destroyed=="1_5",3,0)))+
  ifelse(df$section_D.D4e_Structures_Destroyed=="more_than_10",15,ifelse(df$section_D.D4e_Structures_Destroyed=="6_10",8,ifelse(df$section_D.D4e_Structures_Destroyed=="1_5",3,0)))

##ECONOMIC GROWTH-------------------------------------------------------

#MALE JOBS: Construction, Petty trading, Domestic work, Services, Fishing
dfout$malejobconstruction <- ifelse(as.logical(df$section_K.K1_Jobs_Men.construction),1,0)
dfout$malejobpettytrading <- ifelse((ifelse(as.logical(df$section_K.K1_Jobs_Men.hawking),1,0)+
  ifelse(as.logical(df$section_K.K1_Jobs_Men.petty_trading),1,0)+
  ifelse(as.logical(df$section_K.K1_Jobs_Men.food_vendor),1,0)),1,0)
dfout$malejobsdomesticwork <- ifelse((ifelse(as.logical(df$section_K.K1_Jobs_Men.domestic_work),1,0)+
  ifelse(as.logical(df$section_K.K1_Jobs_Men.garderning),1,0)),1,0)
dfout$malejobservices <- ifelse((ifelse(as.logical(df$section_K.K1_Jobs_Men.securtiy),1,0)+
                                       ifelse(as.logical(df$section_K.K1_Jobs_Men.restuarant),1,0)),1,0)
dfout$malejobfishing <- ifelse(as.logical(df$section_K.K1_Jobs_Men.fishing),1,0)

#FEMALE JOBS: Construction, Petty trading, Domestic work, Services, Fishing 
dfout$femalejobconstruction <- ifelse(as.logical(df$section_K.K2_Jobs_Women.construction,1,0))
dfout$femalejobpettytrading <- ifelse((ifelse(as.logical(df$section_K.K2_Jobs_Women.hawking),1,0)+
                                       ifelse(as.logical(df$section_K.K2_Jobs_Women.petty_trading),1,0)+
                                       ifelse(as.logical(df$section_K.K2_Jobs_Women.food_vendor),1,0)),1,0)
dfout$femalejobsdomesticwork <- ifelse((ifelse(as.logical(df$section_K.K2_Jobs_Women.domestic_work),1,0)+
                                        ifelse(as.logical(df$section_K.K2_Jobs_Women.garderning),1,0)),1,0)
dfout$femalejobservices <- ifelse((ifelse(as.logical(df$section_K.K2_Jobs_Women.securtiy),1,0)+
                                   ifelse(as.logical(df$section_K.K2_Jobs_Women.restuarant),1,0)),1,0)
dfout$femalejobfishing <- ifelse(as.logical(df$section_K.K2_Jobs_Women.fishing),1,0)

#BUSINESS ACTIVITY: % of structures used partially or completely for business activity 
df$section_C.C2_Structures_Residential_Business[df$section_C.C2_Structures_Residential_Business=="n/a"]=NA
df$section_C.C3_Structures_Business[df$section_C.C3_Structures_Business=="n/a"]=NA
df$section_C.C5_Structures_Total[df$section_C.C5_Structures_Total=="n/a"]=NA
dfout$businessactivity <- 100*(as.numeric(df$section_C.C2_Structures_Residential_Business) + as.numeric(df$section_C.C3_Structures_Business))/as.numeric(df$section_C.C5_Structures_Total)
dfout$businessactivity[is.na(dfout$businessactivity)]=NA

#INSTITUTIONS: Informal markets, banks, savings groups and shops
dfout$informalmarkets <- ifelse(df$section_N.N3_InformalMarkets_inside == "yes",2,ifelse(df$section_N.N3_InformalMarkets=="yes",1,0))
dfout$banks <- ifelse(df$section_N.N2_Banks_inside =="yes", 2, ifelse(df$section_N.N2_Banks == "yes",1,0))
dfout$savingsgroups <- as.numeric(df$section_P.P10_Savings_Groups_Count)
dfout$shops <- as.numeric(df$section_O.O1_GeneralShops_Count) + 
  as.numeric(df$section_O.O2_FoodShops_Count)+
  as.numeric(df$section_O.O3_ClothingShops_Count)

#TENURE SECURITY---------------------------------------------------------

#Eviction Threat Count: Replace NA with "None" since NA here is not missing data but means none
dfout$EvictThreatCount <- as.numeric(df$section_E.E1B_Evition_Threat_Count)
dfout$EvictThreatCount[df$Evict_Threat_Count>500] <- NA #What is the reasonable number of discrete eviction threats that might happen

#Seriousness of eviction
dfout$PerceivedRisk <- as.factor(df$section_E.E2B_Current_Eviction_Seriousness)
dfout$PerceivedRisk[df$Evict_Serious=="n/a"] <- NA
levels(dfout$PerceivedRisk) <- c(levels(dfout$PerceivedRisk), "No Response")
dfout$PerceivedRisk[dfout$PerceivedRisk==""] <- "No Response"
dfout$PerceivedRisk[is.na(dfout$PerceivedRisk)] <- "No Response"
droplevels(dfout$PerceivedRisk, c("", "n/a"))

#Ownership of property
dfout$ownerprivate <- df$section_B.B13_Ownership_private_owner
dfout$ownergovt <- as.numeric(df$section_B.B13_Ownership_crown_land) + 
  as.numeric(df$section_B.B13_Ownership_municipality) 
dfout$ownerpublicgoodsreserves <- as.numeric(df$section_B.B13_Ownership_railway) +
  as.numeric(df$section_B.B13_Ownership_airport_authority) +
  as.numeric(df$section_B.B13_Ownership_port_trust) +
  as.numeric(df$section_B.B13_Ownership_defense)
dfout$ownercustomary <- as.numeric(df$section_B.B13_Ownership_customary_land) + 
  as.numeric(df$section_B.B13_Ownership_church_land)

#Legal Status: Is this a legal settlement?
dfout$status[df$section_B.B14_Status=="n/a"|df$section_B.B14_Status=="not_applicable"]<-NA
dfout$status[df$section_B.B14_Status=="declared_legal_protected"] <- "Legal"
dfout$status[df$section_B.B14_Status=="resettled"] <- "Resettled"
dfout$status[df$section_B.B14_Status=="undeclared_illegal_unprotected"]<- "Illegal"
dfout$status <- as.factor(dfout$status)

#Renting Usage
#Allocating number values to renting
dfout$rentusage[df$section_C.C13_Renting=="no_rent"]<-0
dfout$rentusage[df$section_C.C13_Renting=="few_rent"]<-15
dfout$rentusage[df$section_C.C13_Renting=="less_rent"]<-25
dfout$rentusage[df$section_C.C13_Renting=="half_rent"]<-50
dfout$rentusage[df$section_C.C13_Renting=="most_rent"]<-75 

#ACCESS TO INFRASTRUCTURE -------------------------------------------

#Access to Improved Water
#100*(Number of households with sustainable access to piped water/total number of households)

#Data cleaning - Access to water
df$section_F.F1_Working <- as.numeric(df$section_F.F1_Working)
df$section_F.F2_Working <- as.numeric(df$section_F.F2_Working)
df$section_F.F3_Working <- as.numeric(df$section_F.F3_Working)
df$section_F.F4_Working <- as.numeric(df$section_F.F4_Working)
df$section_F.F6_Count <- as.numeric(df$section_F.F6_Count)
df$section_C.C9_Households <- as.numeric(df$section_C.C9_Households)

df$section_F.F1_Working[is.na(df$section_F.F1_Working)]<-0
df$section_F.F2_Working[is.na(df$section_F.F2_Working)]<-0
df$section_F.F3_Working[is.na(df$section_F.F3_Working)]<-0
df$section_F.F4_Working[is.na(df$section_F.F4_Working)]<-0
df$section_F.F4_Working[df$section_F.F4_Quality=="not_safe"]<-0
df$section_F.F6_Count[is.na(df$section_F.F6_Count)]<-0
df$section_F.F6_Count[df$section_F.F6_Quality=="not_safe"]<-0

#Assumption: Survey measure of number of individual taps that are working is an approximate measure of access to piped water to a household
#Deviation: Added shared taps in working order/no. of households + Added working number of communal taps/no. of households+
#No. of boreholes with safe water/no. of households + No. of safe springs/no. of households
#Try an alternative calculation of all sources together 
dfout$access_water <- df$section_F.F1_Working + 
  df$section_F.F2_Working/df$section_C.C9_Households + 
  df$section_F.F3_Working/df$section_C.C9_Households + 
  df$section_F.F4_Working/df$section_C.C9_Households +
  df$section_F.F6_Count/df$section_C.C9_Households 
dfout$access_water <- (100*dfout$access_water)/df$section_C.C9_Households #Logging unstandardized value for use in regressions
dfout$access_water[dfout$access_water>100] <- 100

#Access To Sanitation 
#100*(Number of households with improved sanitation/Total no. of households)
#Improved sanitation incl. flush, pour flush, septic tank
#Modification: Individual toilets count as one, shared toilet seats or public toilet seats is discounted by the total no. of households

#Data cleaning - Access to sanitation
#Replace blanks in the database with zeros
df$section_G.G7_Seats <- as.numeric(df$section_G.G7_Seats)
df$section_G.G8_Seats <- as.numeric(df$section_G.G8_Seats)
df$section_G.G9_Seats <- as.numeric(df$section_G.G9_Seats)

df$section_G.G7_Seats[is.na(df$section_G.G7_Seats)]<-0
df$section_G.G8_Seats[is.na(df$section_G.G8_Seats)]<-0
df$section_G.G9_Seats[is.na(df$section_G.G9_Seats)]<-0

#Logical Coefficients (Improved = 1, Unimproved = 0)
#When Toilet types gets inputted as characters
individual <- as.integer(df$section_G.G7_Toilet_Type.chemical=="TRUE"|df$section_G.G7_Toilet_Type.chemical=="True"|
                           df$section_G.G7_Toilet_Type.ecosan=="TRUE"|df$section_G.G7_Toilet_Type.ecosan=="True"|
                           df$section_G.G7_Toilet_Type.pour_flush=="TRUE"|df$section_G.G7_Toilet_Type.pour_flush=="True"|
                           df$section_G.G7_Toilet_Type.flush=="TRUE"|df$section_G.G7_Toilet_Type.flush=="True")

shared <- as.integer(df$section_G.G8_Toilet_Type.chemical=="TRUE"|df$section_G.G8_Toilet_Type.chemical=="True"|
                       df$section_G.G8_Toilet_Type.ecosan=="TRUE"|df$section_G.G8_Toilet_Type.ecosan=="True"|
                       df$section_G.G8_Toilet_Type.pour_flush=="TRUE"|df$section_G.G8_Toilet_Type.pour_flush=="True"|
                       df$section_G.G8_Toilet_Type.flush=="TRUE"|df$section_G.G8_Toilet_Type.flush=="True")

communal <- as.integer(df$section_G.G9_Toilet_Type.chemical=="TRUE"|df$section_G.G9_Toilet_Type.chemical=="True"|
                         df$section_G.G9_Toilet_Type.ecosan=="TRUE"|df$section_G.G9_Toilet_Type.ecosan=="True"|
                         df$section_G.G9_Toilet_Type.pour_flush=="TRUE"|df$section_G.G9_Toilet_Type.pour_flush=="True"|
                         df$section_G.G9_Toilet_Type.flush=="TRUE"|df$section_G.G9_Toilet_Type.flush=="True")

#When toilet types get inputted as logical values
#individual <- as.integer(df$section_G.G7_Toilet_Type.chemical|df$section_G.G7_Toilet_Type.ecosan|df$section_G.G7_Toilet_Type.pour_flush|df$section_G.G7_Toilet_Type.flush)
#shared <- as.integer(df$section_G.G8_Toilet_Type.chemical|df$section_G.G8_Toilet_Type.ecosan|df$section_G.G8_Toilet_Type.pour_flush|df$section_G.G8_Toilet_Type.flush)
#communal <- as.integer(df$section_G.G9_Toilet_Type.chemical|df$section_G.G9_Toilet_Type.ecosan|df$section_G.G9_Toilet_Type.pour_flush|df$section_G.G9_Toilet_Type.flush)

individual[is.na(individual)]<-0
shared[is.na(shared)]<-0
communal[is.na(communal)]<-0

#Counting no. of households with minimum access to improved sanitation 
dfout$improved_sanitation <- df$section_G.G7_Seats*individual + 
  shared*(df$section_G.G8_Seats/df$section_C.C9_Households) + 
  communal*(df$section_G.G9_Seats/df$section_C.C9_Households)
dfout$improved_sanitation <- 100*dfout$improved_sanitation/df$section_C.C9_Households
dfout$improved_sanitation[dfout$improved_sanitation>100] <- 100

#Access to Electricity 
#100*(Number of households with access to the city grid/Total no. of households)
#Convert NA's into zeros
df$section_J.J2_Electricity_Households <- as.numeric(df$section_J.J2_Electricity_Households)
df$section_J.J2_Electricity_Households[is.na(df$section_J.J2_Electricity_Households)]<-0

#Evaluate percentage of households with electricity. Allocate NA when partial data is available or incorrect data
dfout$access_electricity <- case_when (
  (df$section_C.C9_Households < df$section_J.J2_Electricity_Households) ~ NA_real_, 
  (df$section_C.C9_Households==0) ~ NA_real_,
  TRUE ~ 100*df$section_J.J2_Electricity_Households/(df$section_C.C9_Households)
)  
dfout$access_electricity[dfout$access_electricity>100] <- 100

#PRIORITIES ---------------------------------------------------------

#Short term, medium term and long term priorities
df$section_Q.Q1a_Priority_Term <- as.character(ifelse(df$section_Q.Q1a_Priority_Term=="",NA, df$section_Q.Q1a_Priority_Term))
df$section_Q.Q2a_Priority_Term <- as.character(ifelse(df$section_Q.Q2a_Priority_Term=="",NA, df$section_Q.Q2a_Priority_Term))
df$section_Q.Q3a_Priority_Term <- as.character(ifelse(df$section_Q.Q3a_Priority_Term=="",NA, df$section_Q.Q3a_Priority_Term))
df$section_Q.Q4a_Priority_Term <- as.character(ifelse(df$section_Q.Q4a_Priority_Term=="",NA, df$section_Q.Q4a_Priority_Term))
df$section_Q.Q5a_Priority_Term <- as.character(ifelse(df$section_Q.Q5a_Priority_Term=="",NA, df$section_Q.Q5a_Priority_Term))

#Setting weights for short, medium and long term preferences
short <- 10
medium <- 5
long <- 1

df$Priority_Term_1[df$section_Q.Q1a_Priority_Term=="short"]<-short
df$Priority_Term_1[df$section_Q.Q1a_Priority_Term=="medium"|df$section_Q.Q1a_Priority_Term==""]<-medium
df$Priority_Term_1[df$section_Q.Q1a_Priority_Term=="long"]<-long
df$Priority_Term_1[df$section_Q.Q1a_Priority_Term=="n/a"]<-long

df$Priority_Term_2[df$section_Q.Q2a_Priority_Term=="short"]<-short
df$Priority_Term_2[df$section_Q.Q2a_Priority_Term=="medium"|df$section_Q.Q1a_Priority_Term==""]<-medium
df$Priority_Term_2[df$section_Q.Q2a_Priority_Term=="long"]<-long
df$Priority_Term_2[df$section_Q.Q2a_Priority_Term=="n/a"]<-long

df$Priority_Term_3[df$section_Q.Q3a_Priority_Term=="short"]<-short
df$Priority_Term_3[df$section_Q.Q3a_Priority_Term=="medium"|df$section_Q.Q1a_Priority_Term==""]<-medium
df$Priority_Term_3[df$section_Q.Q3a_Priority_Term=="long"]<-long
df$Priority_Term_3[df$section_Q.Q3a_Priority_Term=="n/a"]<-long

df$Priority_Term_4[df$section_Q.Q4a_Priority_Term=="short"]<-short
df$Priority_Term_4[df$section_Q.Q4a_Priority_Term=="medium"|df$section_Q.Q1a_Priority_Term==""]<-medium
df$Priority_Term_4[df$section_Q.Q4a_Priority_Term=="long"]<-long
df$Priority_Term_4[df$section_Q.Q4a_Priority_Term=="n/a"]<-long

df$Priority_Term_5[df$section_Q.Q5a_Priority_Term=="short"]<-short
df$Priority_Term_5[df$section_Q.Q5a_Priority_Term=="medium"|df$section_Q.Q1a_Priority_Term==""]<-medium
df$Priority_Term_5[df$section_Q.Q5a_Priority_Term=="long"]<-long
df$Priority_Term_5[df$section_Q.Q5a_Priority_Term=="n/a"]<-long

#Set up dataframe to record settlement preferences 
#Solving location problems
pr_names <- c("water_drainage", "sanitation_sewage", "tenure_security", "housing", "electricity", "land_tenure", "other")

#Utility scores for electricity per settlement
for (i in c(1:length(pr_names))){
  cn <- as.character(pr_names[i])
  dfout$cn[df$section_Q.Q1_Priority1==cn] <- 10*df$Priority_Term_1[df$section_Q.Q1_Priority1==cn]
  dfout$cn[df$section_Q.Q2_Priority2==cn] <- 8*df$Priority_Term_2[df$section_Q.Q2_Priority2==cn]
  dfout$cn[df$section_Q.Q3_Priority3==cn] <- 6*df$Priority_Term_3[df$section_Q.Q3_Priority3==cn]
  dfout$cn[df$section_Q.Q4_Priority4==cn] <- 4*df$Priority_Term_4[df$section_Q.Q4_Priority4==cn]
  dfout$cn[df$section_Q.Q5_Priority5==cn] <- 2*df$Priority_Term_5[df$section_Q.Q5_Priority5==cn]
  colnames(dfout)[colnames(dfout)=="cn"] <- as.character(pr_names[i])
}

#Combining "tenure_security" and "land_tenure" into a single column
dfout$tenure_security[is.na(dfout$land_tenure)&is.na(dfout$tenure_security)] <- NA
dfout$tenure_security[is.na(dfout$tenure_security)&(!is.na(dfout$land_tenure))] <- dfout$land_tenure[is.na(dfout$tenure_security)&(!is.na(dfout$land_tenure))]
dfout$tenure_security[!is.na(dfout$tenure_security)&(!is.na(dfout$land_tenure))] <- dfout$land_tenure[!is.na(dfout$tenure_security)&(!is.na(dfout$land_tenure))]+
  dfout$tenure_security[!is.na(dfout$tenure_security)&(!is.na(dfout$land_tenure))]
dfout <- dfout[!(names(dfout) %in% c("land_tenure"))]
#Trimming the location problems by removing the duplicate "land tenure"
pr_names <- c("water_drainage", "sanitation_sewage", "tenure_security", "housing", "electricity", "other")

#RESPONSE TIME -----------------------------------------------------

dfout$timefireengine <- ifelse(df$section_I.I9_FireEngine_ResponseTime=="more_than_1_hour", 75,
                               ifelse(df$section_I.I9_FireEngine_ResponseTime=="30_minutes_to_1_hour", 45, 
                                ifelse(df$section_I.I9_FireEngine_ResponseTime=="30_minutes", 30, 
                                      ifelse(df$section_I.I9_FireEngine_ResponseTime=="15_minutes", 15,
                                             ifelse(df$section_I.I9_FireEngine_ResponseTime=="10_minutes", 10,
                                              ifelse(df$section_I.I9_FireEngine_ResponseTime=="5_minutes", 5, NA))))))

dfout$timeambulance <- ifelse(df$section_I.I7_Ambulance_ResponseTime=="more_than_1_hour", 75,
                              ifelse(df$section_I.I7_Ambulance_ResponseTime=="30_minutes_to_1_hour", 45, 
                                     ifelse(df$section_I.I7_Ambulance_ResponseTime=="30_minutes", 30, 
                                            ifelse(df$section_I.I7_Ambulance_ResponseTime=="15_minutes", 15,
                                                   ifelse(df$section_I.I7_Ambulance_ResponseTime=="10_minutes", 10,
                                                          ifelse(df$section_I.I7_Ambulance_ResponseTime=="5_minutes", 5,NA))))))

dfout$timepolice <- ifelse(df$section_N.N8_PoliceStations_min=="more_than_1_hour", 75,
                              ifelse(df$section_N.N8_PoliceStations_min=="30_minutes_to_1_hour", 45, 
                                     ifelse(df$section_N.N8_PoliceStations_min=="30_minutes", 30, 
                                            ifelse(df$section_N.N8_PoliceStations_min=="15_minutes", 15,
                                                   ifelse(df$section_N.N8_PoliceStations_min=="10_minutes", 10,
                                                          ifelse(df$section_N.N8_PoliceStations_min=="5_minutes", 5,NA))))))

#Minutes wait for water
dfout$timewater <- ifelse(df$section_F.F12_Water_CollectionTime=="more_than_1_hour", 75,
                         ifelse(df$section_F.F12_Water_CollectionTime=="30_minutes_to_1_hour", 45, 
                                ifelse(df$section_F.F12_Water_CollectionTime=="30_minutes", 30, 
                                       ifelse(df$section_F.F12_Water_CollectionTime=="15_minutes", 15,
                                              ifelse(df$section_F.F12_Water_CollectionTime=="10_minutes", 10,
                                                     ifelse(df$section_F.F12_Water_CollectionTime=="5_minutes", 5,NA))))))
#Minute wait for toilet use
dfout$timesanitation <- ifelse(df$section_G.G11_Toilet_AverageWait=="more_than_1_hour", 75,
                          ifelse(df$section_G.G11_Toilet_AverageWait=="30_minutes_to_1_hour", 45, 
                                 ifelse(df$section_G.G11_Toilet_AverageWait=="30_minutes", 30, 
                                        ifelse(df$section_G.G11_Toilet_AverageWait=="15_minutes", 15,
                                               ifelse(df$section_G.G11_Toilet_AverageWait=="10_minutes", 10,
                                                      ifelse(df$section_G.G11_Toilet_AverageWait=="5_minutes", 5,NA))))))
#Day interval between garbage collection/disposal
dfout$timegarbage <- 7/as.numeric(df$section_H.H6_Garbage_WeeklyCollections)

#Average response time metrics
dfout$timeemergency <- (dfout$timefireengine + dfout$timeambulance + dfout$timepolice)/3
dfout$publictransit <- 0.5*(as.numeric(df$section_L.L2_WalLTime_Railway)+as.numeric(df$section_L.L3_WalLTime_Bus))
dfout$washaccess <- 0.5*(dfout$timewater + dfout$timesanitation)

#AVERAGE COST ---------------------------------------------------------------

#Cost of transit per day to go to town
#Clean data

df$section_L.L1_Cost_bus <- as.numeric(df$section_L.L1_Cost_bus)
df$section_L.L1_Cost_motorcycles <- as.numeric(df$section_L.L1_Cost_motorcycles)
df$section_L.L1_Cost_private_automobile <- as.numeric(df$section_L.L1_Cost_private_automobile)
df$section_L.L1_Cost_taxi <- as.numeric(df$section_L.L1_Cost_taxi)
df$section_L.L1_Cost_train <- as.numeric(df$section_L.L1_Cost_train)
df$section_L.L1_Cost_other <- as.numeric(df$section_L.L1_Cost_other)

#Average public and private transit costs per settlement
transit <- c("section_L.L1_Cost_bus", "section_L.L1_Cost_bus", "section_L.L1_Cost_taxi", "section_L.L1_Cost_train", "section_L.L1_Cost_other")
private <- c("section_L.L1_Cost_motorcycles", "section_L.L1_Cost_private_automobile")
dfout$costpublictransit <- rowMeans(df[names(df) %in% transit], na.rm = TRUE)
dfout$costprivatetransit <- rowMeans(df[names(df) %in% private], na.rm = TRUE)

#Monthly cost of access to infrastructure 
#Clean data
dfout$costwater <- as.numeric(df$section_F.F11_Water_MonthlyCost)
dfout$costsanitation <- as.numeric(df$section_G.G3_Toilets_Pay)
dfout$costgarbage <- as.numeric(df$section_H.H5_Garbage_Cost)
dfout$costelectricity <- as.numeric(df$section_J.J6_Electricity_MonthlyCost)

#Filtering outliers
dfout$costwater[dfout$costwater>70000]<- NA
dfout$costsanitation[dfout$costsanitation>70000]<- NA
dfout$costgarbage[dfout$costgarbage>70000]<- NA
dfout$costelectricity[dfout$costelectricity>70000] <- NA

#Average cost of infrastructure
dfout$avcostinfra <- rowMeans(dfout[grepl("cost", names(dfout))], na.rm=TRUE)

#PLANNING INPUTS -------------------------------------------------

#Road Type
dfout$roadtype = as.factor(df$section_L.L5_Road_Type)
dfout$roadtype[dfout$roadtype=="n/a"]=NA
droplevels(dfout$roadtype, "n/a")

#Road Network planned?
dfout$roadsplanned = as.factor(df$section_L.L6_Roads_Planned)
dfout$roadsplanned[dfout$roadsplanned=="n/a"]=NA
droplevels(dfout$roadsplanned, "n/a")

#Connectivity
dfout$connectwaterline <- ifelse(df$section_F.F15_Main_Water_Line=="yes",1,ifelse(df$section_F.F15_Main_Water_Line=="no",0,NA))
dfout$connectsewerline <- ifelse(df$section_G.G2_Sewer_Connected=="yes",1, ifelse(df$section_G.G2_Sewer_Connected=="no",0,NA))

#CREATE OUTPUT FILE
country <- "sierraleaone"  
file_output <- paste0(country,"profile.csv")
write.csv(dfout, file_output)
