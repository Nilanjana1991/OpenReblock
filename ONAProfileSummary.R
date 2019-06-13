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
#Public infrastructure (railway tracks/under power lines/roadside)
#Physical (Water body/flood prone area/sinking soil) or
#Solid Waste (Garbage dump or Industrial dumping area or Mining) 
dfout$publicgoodhazard <- ifelse(as.logical(df$section_D.D1_Location_Problems.railway_track),1,0)+
  ifelse(as.logical(df$section_D.D1_Location_Problems.under_power_lines),1,0)+
  ifelse(as.logical(df$section_D.D1_Location_Problems.road_side),1,0)
dfout$physical <-  ifelse(as.logical(df$section_D.D1_Location_Problems.water_body),1,0)+
  ifelse(as.logical(df$section_D.D1_Location_Problems.flood_prone_area),1,0)+
  ifelse(as.logical(df$section_D.D1_Location_Problems.sinking_soil),1,0)
dfout$solidwaste <- ifelse(as.logical(df$section_D.D1_Location_Problems.garbage_dump),1,0) +
                             ifelse(as.logical(df$section_D.D1_Location_Problems.industrial_hazards),1,0)+
                             ifelse(as.logical(df$section_D.D1_Location_Problems.mine_dump),1,0)
  
#Disasters in the last year: Fires, Flood frequency reported; Others are not included 
#Fires
dfout$firefrequency <- ifelse(df$section_D.D4a_Distaster_Frequency=="more_than_three",3,ifelse(df$section_D.D4a_Distaster_Frequency=="twice",2,ifelse(df$section_D.D4a_Distaster_Frequency=="once",1,0)))
#Floods
dfout$floodfrequency <- ifelse(df$section_D.D4b_Distaster_Frequency=="more_than_three",3,ifelse(df$section_D.D4b_Distaster_Frequency=="twice",2,ifelse(df$section_D.D4b_Distaster_Frequency=="once",1,0)))

##ECONOMIC GROWTH-------------------------------------------------------

#MALE JOBS: Construction, Petty trading, Domestic work, Services, Agriculture, Food Vendors
#Construction: Construction OR Masonry OR Tilers OR Electricians
dfout$malejobconst <- ifelse((ifelse(as.logical(df$section_K.K1_Jobs_Men.construction),1,0)+
  ifelse(as.logical(df$section_K.K1_Jobs_Men.masonry),1,0)+
  ifelse(as.logical(df$section_K.K1_Jobs_Men.tilers),1,0)+ 
  ifelse(as.logical(df$section_K.K1_Jobs_Men.electricians),1,0)),1,0)
#Petty Trading: Hawking OR Petty Trading
dfout$malejobpettytrade <- ifelse((ifelse(as.logical(df$section_K.K1_Jobs_Men.hawking),1,0)+
  ifelse(as.logical(df$section_K.K1_Jobs_Men.petty_trading),1,0)),1,0)
#Food Vendors
dfout$malejobfoodvendor <- ifelse(as.logical(df$section_K.K1_Jobs_Men.food_vendor),1,0)
#Services: Security OR Restaurant OR Shopping Centre OR Domestic Work
dfout$malejobserv <- ifelse((ifelse(as.logical(df$section_K.K1_Jobs_Men.domestic_work),1,0)+
  ifelse(as.logical(df$section_K.K1_Jobs_Men.restuarant),1,0)+
           ifelse(as.logical(df$section_K.K1_Jobs_Men.shopping_centre),1,0)+
          ifelse(as.logical(df$section_K.K1_Jobs_Men.securtiy),1,0)),1,0)
#Agriculture: Fishing + Other agri based practices
dfout$malejobagri <- ifelse(as.logical(df$section_K.K1_Jobs_Men.fishing),1,0)

#FEMALE JOBS: Construction, Petty trading, Domestic work, Services, Agriculture, Food Vendors 
#Construction: Construction OR Masonry OR Tilers OR Electricians
dfout$femalejobconst <- ifelse((ifelse(as.logical(df$section_K.K2_Jobs_Women.construction),1,0) +
                               ifelse(as.logical(df$section_K.K2_Jobs_Women.masonry),1,0) +
                               ifelse(as.logical(df$section_K.K2_Jobs_Women.tilers),1,0) + 
                               ifelse(as.logical(df$section_K.K2_Jobs_Women.electricians),1,0)),1,0)
#Petty Trading: Hawking OR Petty Trading
dfout$femalejobpettytrade <- ifelse((ifelse(as.logical(df$section_K.K2_Jobs_Women.hawking),1,0)+
                                    ifelse(as.logical(df$section_K.K2_Jobs_Women.petty_trading),1,0)),1,0)
#Food Vendors
dfout$femalejobfoodvendor <- ifelse(as.logical(df$section_K.K2_Jobs_Women.food_vendor),1,0)
#Services: Security OR Restaurant OR Shopping Centre OR Domestic Work
dfout$femalejobserv <- ifelse((ifelse(as.logical(df$section_K.K2_Jobs_Women.domestic_work),1,0)+
                              ifelse(as.logical(df$section_K.K2_Jobs_Women.restuarant),1,0)+
                              ifelse(as.logical(df$section_K.K2_Jobs_Women.shopping_centre),1,0)+
                              ifelse(as.logical(df$section_K.K2_Jobs_Women.securtiy),1,0)),1,0)
#Agriculture: Fishing + Other agri based practices
dfout$femalejobagri <- ifelse(as.logical(df$section_K.K2_Jobs_Women.fishing),1,0)

#BUSINESS ACTIVITY: % of structures used partially or completely for business activity 
df$section_C.C2_Structures_Residential_Business[df$section_C.C2_Structures_Residential_Business=="n/a"]=NA
df$section_C.C3_Structures_Business[df$section_C.C3_Structures_Business=="n/a"]=NA
df$section_C.C5_Structures_Total[df$section_C.C5_Structures_Total=="n/a"]=NA
dfout$businessactivity <- 100*(as.numeric(df$section_C.C2_Structures_Residential_Business) + as.numeric(df$section_C.C3_Structures_Business))/as.numeric(df$section_C.C5_Structures_Total)
dfout$businessactivity[is.na(dfout$businessactivity)]=NA

#INSTITUTIONS: Informal markets, banks, savings groups and shops
dfout$informalmarkets <- ifelse(df$section_N.N3_InformalMarkets_inside == "yes",2,ifelse(df$section_N.N3_InformalMarkets=="yes",1,0))

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

#DOES THE CITY PROVIDE ANY SERVICES IN SETTLEMENTS -------------------------------------------

#Water Access
dfout$servwater <- ifelse(ifelse((df$section_F.F1_Manager=="Municipality"|df$section_F.F1_Manager=="MUNICIPALITY"|df$section_F.F1_Manager=="municipality"),1,0)+
  ifelse((df$section_F.F2_Manager=="Municipality"|df$section_F.F2_Manager=="MUNICIPALITY"|df$section_F.F2_Manager=="municipality"),1,0)+
  ifelse((df$section_F.F3_Manager=="Municipality"|df$section_F.F3_Manager=="MUNICIPALITY"|df$section_F.F3_Manager=="municipality"),1,0)+
  ifelse((df$section_F.F4_Manager=="Municipality"|df$section_F.F4_Manager=="MUNICIPALITY"|df$section_F.F4_Manager=="municipality"),1,0)+
  ifelse((df$section_F.F6_Manager=="Municipality"|df$section_F.F6_Manager=="MUNICIPALITY"|df$section_F.F6_Manager=="municipality"),1,0)+
  ifelse((df$section_F.F7_Manager=="Municipality"|df$section_F.F7_Manager=="MUNICIPALITY"|df$section_F.F7_Manager=="municipality"),1,0)+
  ifelse((df$section_F.F9_Manager=="Municipality"|df$section_F.F9_Manager=="MUNICIPALITY"|df$section_F.F9_Manager=="municipality"),1,0)+
  ifelse((df$section_F.F1_Supply=="Municipality"|df$section_F.F1_Supply=="MUNICIPALITY"|df$section_F.F1_Supply=="municipality"),1,0)+
  ifelse((df$section_F.F2_Supply=="Municipality"|df$section_F.F2_Supply=="MUNICIPALITY"|df$section_F.F2_Supply=="municipality"),1,0)+
  ifelse((df$section_F.F3_Supply=="Municipality"|df$section_F.F3_Supply=="MUNICIPALITY"|df$section_F.F3_Supply=="municipality"),1,0)+
  ifelse((df$section_F.F3_Supply=="Municipality"|df$section_F.F3_Supply=="MUNICIPALITY"|df$section_F.F3_Supply=="municipality"),1,0)+
  ifelse((df$section_F.F4_Supply=="Municipality"|df$section_F.F4_Supply=="MUNICIPALITY"|df$section_F.F4_Supply=="municipality"),1,0)+
  ifelse((df$section_F.F5_Supply=="Municipality"|df$section_F.F5_Supply=="MUNICIPALITY"|df$section_F.F5_Supply=="municipality"),1,0)+
  ifelse((df$section_F.F9_Supply=="Municipality"|df$section_F.F9_Supply=="MUNICIPALITY"|df$section_F.F9_Supply=="municipality"),1,0),1,0)

#Sanitation
dfout$servsani <- ifelse(ifelse((df$section_G.G7_Manager=="Municipality"|df$section_G.G7_Manager=="MUNICIPALITY"|df$section_G.G7_Manager=="municipality"),1,0)+
                            ifelse((df$section_G.G8_Manager=="Municipality"|df$section_G.G8_Manager=="MUNICIPALITY"|df$section_G.G8_Manager=="municipality"),1,0)+
                            ifelse((df$section_G.G9_Manager=="Municipality"|df$section_G.G9_Manager=="MUNICIPALITY"|df$section_G.G9_Manager=="municipality"),1,0)+
                            ifelse((df$section_G.G10_Manager=="Municipality"|df$section_G.G10_Manager=="MUNICIPALITY"|df$section_G.G10_Manager=="municipality"),1,0),1,0)

#Garbage Disposal 
dfout$servgarbage <- ifelse((df$section_H.H3_Garbage_Collector=="Municipality"|df$section_H.H3_Garbage_Collector=="MUNICIPALITY"|df$section_H.H3_Garbage_Collector=="municipality"),1,0)
  
#Electricity
dfout$servelectric <- ifelse(df$section_J.J1_Electricity_Available=="yes",1,0)



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


#PLANNING INPUTS -------------------------------------------------

#Road Type
dfout$roadtype = as.factor(df$section_L.L5_Road_Type)
dfout$roadtype[dfout$roadtype=="n/a"]=NA
droplevels(dfout$roadtype, "n/a")

#Road Network planned?
dfout$roadsplanned = as.factor(df$section_L.L6_Roads_Planned)
dfout$roadsplanned[dfout$roadsplanned=="n/a"]=NA
droplevels(dfout$roadsplanned, "n/a")


#CREATE OUTPUT FILE
country <- "sierraleaone"  
file_output <- paste0(country,"profile.csv")
write.csv(dfout, file_output)
