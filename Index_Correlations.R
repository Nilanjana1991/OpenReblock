#Created by: Nilanjana Bhattacharya
#Title: Testing correlations between CPI sub - indices and community profile characteristics 
#Abstract: Exploratory effort to check correlations between the sub - indexes of the City Prosperity Index and collective preferences for improvement
#Current Status: 
#This code enumerates the Housing Infrastructure sub index and correlates community preferences with the constituents of HI
#This code includes data from Nigeria, Sierra Leaone, Liberia, South Africa, Ghana, Uganda, Namibia, Senegal, Zimbabwe, Kenya, Tanzania
#Malawi and Botswana have answered a different survey 

#Last updated: 7th March 2019 

#-----------------------------------------------------------
setwd("E:\\Open Reblock")

library("tidyverse")
library("gtools")
library("GGally")
library("outreg")
library("broom")

rm(list=ls())
#Creating database
#List of countries added here
countries <- c("nigeria", "liberia", "sierraleaone", "southafrica", "ghana", "uganda", "senegal", "namibia", "tanzania", "kenya", "zimbabwe")  

filename <- paste0("Datasets\\Settlement_Summary(", countries[1],").csv")
  df <- read.csv(filename)
for (i in c(6:length(countries))){
  filename <- paste0("Datasets\\Settlement_Summary(", countries[i],").csv")
  df_temp <- read.csv(filename)
  df <- rbind(df, df_temp)
}
rm(df_temp)

#DATA REPRESENTATION------------------------------------------------
#Eviction data cleaning
df$Evict_Threat_Count <- as.numeric(df$Evict_Threat_Count)
df$Evict_Threat_Count[df$Evict_Threat_Count>100] <- NA
levels(df$Evict_Serious) <- c(levels(df$Evict_Serious), "No Response")
df$Evict_Serious[df$Evict_Serious==""] <- "No Response"
df$Evict_Serious[is.na(df$Evict_Serious)] <- "No Response"
df$Evict_Serious[df$Evict_Serious=="none"] <- "No Response"
droplevels(df$Evict_Serious, c("", NA, "none"))

#Correcting ranges - cannot be greater than 100%
df$access_water[df$access_water>100] <- NA
df$improved_sanitation[df$improved_sanitation>100] <- NA
df$access_shelter[df$access_shelter>100] <- NA
df$access_electricity[df$access_electricity>100] <- NA
df$liv_area[df$liv_area>100] - NA

#Correcting range of settlement area (cannot be bigger than the size of entire cities)
df$area[df$area>500000] <- NA
#Country Names cleaning 
df$country[df$country=="Africa"]<- "southafrica"
df$country[df$country==" Africa"]<- "southafrica"
droplevels(df$country, c("Africa", " Africa"))

corr_mat<- ggcorr(df[, c(7:ncol(df))], label = TRUE)
ggsave("Viz\\Settlement Correlations.png", corr_mat, width = 16, height = 8)

#ACCESS TO PUBLIC INFRASTRUCTURE AND SERVICES ---------------

#Benchmarks to be included in the measurement of indices
ind_benchmark_names <- c("water","sani","shelter", "electricity", "liv_area", "density") #Add more names to this array

#Benchmarks from World Bank 2014: 
benchmarks <- data.frame(c(50,100), 
                         c(15,100),
                         c(84.8,98.4), 
                         c(7,100),
                         c(1.26, 2.76),
                         c(0,61))
colnames(benchmarks)<- ind_benchmark_names
rownames(benchmarks)<- c("min","max")

#COMPARING ACCESS BY COUNTRY --------------------------------------
sum_ind <- df%>%
  filter(country!="")%>%
  group_by(country)%>%
  summarise(mHI <- mean(HI, na.rm = T),
            mMI <- mean(MI, na.rm = T), 
            mSTI <- mean(STI, na.rm = T),
            mwater <- mean(access_water, na.rm = TRUE), 
            melectricity <- mean(access_electricity, na.rm = TRUE), 
            msanitation <- mean(improved_sanitation, na.rm = TRUE), 
            mliv_area <- mean(liv_area, na.rm = TRUE), 
            mdensity <- mean(pop_density, na.rm = TRUE))
colnames(sum_ind) <- c("Country", "HousingIndex", "MIndex", "SecureTenureIndex", "%Households_WaterAccess", "%Households_ElectricityAccess", "%Households_Improved Sanitation", "%Households_SufficientLiving Area", "Average Population Density")

#Housing Infrastructure Index  
HIplot <- ggplot(sum_ind, aes(x = sum_ind$Country, y = sum_ind$HousingIndex))+
  geom_col()+
  geom_text(aes(label = as.integer(HousingIndex)), nudge_y = -5)+
  labs(x = "Country", y = "Mean Housing Infrastructure Sub - Index")+
  ggtitle("Housing Infrastructure by Country")+
  theme_minimal()
ggsave("Viz\\HI.png", HIplot, width = 15)

sum_ind <- gather(sum_ind, "field", "value", -Country)

sub_ind_plot <- ggplot(data = sum_ind, aes(x = Country, y = value, group = field, colour = as.factor(field)))+
  geom_line(size = 1)+
  labs(y = "Value")+
  ggtitle("Country Comparisons in access to housing infrastructure")
print(sub_ind_plot)
ggsave("Viz\\Country Comparisons of Access.png", sub_ind_plot, width = 20)

#ACCESS TO INFRASTRUCTURE IN INFORMAL SETTLEMENTS -------------------------------
#Access to water
p1 <- ggplot(df, aes(x=log10(access_water+1)))+
  geom_histogram()+
  ggtitle("Access to Water")+
  geom_vline(xintercept = c(log10(benchmarks$water[1]+1),log10(benchmarks$water[2]+1)), linetype="dotted", colour = "red", size = 1.0)+
  geom_vline(xintercept = mean(log10(df$access_water+1), na.rm = TRUE), linetype = "dotted", size = 1.0)+
  xlab("log10(% Households with improved water sources)")
print(p1)
ggsave("Viz\\Access to Water.png", p1)

#Access to improved sanitation
p2 <- ggplot(df, aes(x=log10(improved_sanitation+1)))+
  geom_histogram()+
  ggtitle("Access to Sanitation")+
  geom_vline(xintercept = c(log10(benchmarks$sani[1]+1),log10(benchmarks$sani[2]+1)), linetype="dotted", colour = "red", size = 1.0)+
  geom_vline(xintercept = mean(log(df$improved_sanitation+1), na.rm = TRUE), linetype="dotted", size = 1.0)+
  xlab("log10(% Households with improved sanitation)")
print(p2)
ggsave("Viz\\Access to Improved Sanitation.png", p2)

#Access to improved housing
p3 <- ggplot(df, aes(x=log10(df$access_shelter+1)))+
  geom_histogram()+
  ggtitle("Access to Improved Housing")+
  geom_vline(xintercept = c(log10(benchmarks$shelter[1]+1),log10(benchmarks$shelter[2]+1)), linetype="dotted", colour = "red", size = 1.0)+
  geom_vline(xintercept = mean(log10(df$access_shelter+1), na.rm = TRUE), linetype="dotted", size = 1.0)+
  xlab("Log10(% Households with improved shelters)")
print(p3)
ggsave("Viz\\Access to Improved Housing.png", p3)

#Access to electricity
p4 <- ggplot(df, aes(x=log10(df$access_electricity+1)))+
  geom_histogram()+
  ggtitle("Access to Electricity")+
  geom_vline(xintercept = c(log10(benchmarks$electricity[1]+1),log10(benchmarks$electricity[2]+1)), linetype="dotted", colour = "red", size = 1.0)+
  geom_vline(xintercept = mean(log10(df$access_electricity+1), na.rm = TRUE), linetype="dotted", size = 1.0)+
  xlab("% Households with grid electricity")
print(p4)
ggsave("Viz\\Access to Electricity.png", p4)

#Population Density
p5 <- ggplot(df, aes(x=log10(pop_density+1)))+
  geom_histogram()+
  ggtitle("Population Density")+
  geom_vline(xintercept = c(log10(benchmarks$density[1]+1),log10(benchmarks$density[2]+1)), linetype="dotted", colour = "red", size = 1.0)+
  geom_vline(xintercept = mean(log10(df$pop_density+1), na.rm = TRUE), linetype="dotted", size = 1.0)+
  xlab("Population Density Distribution")
print(p5)
ggsave("Viz\\Population Density.png", p5)

#Sufficient Living Area
p6 <- ggplot(df, aes(x=log10(liv_area+1)))+
  geom_histogram()+
  ggtitle("Sufficient Living Area")+
  geom_vline(xintercept = c(log10(benchmarks$liv_area[1]+1),log10(benchmarks$liv_area[2]+1)), linetype="dotted", colour = "red", size = 1.0)+
  geom_vline(xintercept = mean(log10(df$liv_area+1), na.rm = TRUE), linetype="dotted", size = 1.0)+
  xlab("% Settlements with less than average of 4 people per household")
print(p6)
ggsave("Viz\\Living Area.png", p6)

#SUMMARY PREFERENCES AND INDICES IN INFORMAL SETTLEMENTS ------------------------------------------ 
#Community preferences summarized - Water and tenure security most common top priorities
pr_names <- c("water_drainage", "sanitation_sewage", "tenure_security", "housing", "electricity", "land_tenure", "other")
utility <- df[names(df) %in% pr_names]#check the column numbers
utility <- stack(utility)
utility <- subset(utility, utility$values>30)
q <- ggplot(utility, aes(x = values)) +
  stat_density(aes(group = ind, color = ind), position="identity",geom="line",adjust = 1.5, kernel = "gaussian", size = 1)+
  ggtitle("Community priorities to solve location problems")+
  xlab("Priority Score (Scale: 100 = Highest priority, short term ------------- 0 = Lowest priority, long term)")+
  theme_minimal()
print(q)
ggsave("Viz\\Community Priorities.png", q, width = 10)

#Comparing Infrastructure Index across communities
i_names <- c("HI", "STI")
index <- df[names(df) %in% i_names]
index <- stack(index)
index <- subset(index, !is.na(index$values))
index <- subset(index, !is.infinite(index$values))
p7 <- ggplot(index, aes(x = values)) +
  stat_density(aes(group = ind, color = ind), position="identity",geom="line",adjust = 1, kernel = "gaussian", size = 1)+
  ggtitle("Indices for Settlements")
print(p7)
ggsave("Viz\\Index Distributions.png", p7, width = 10)

#TESTING HYPOTHESES-------------------------------------------

#PERCEIVING THE THREAT OF EVICTION  
#Create dummy variables 
df$evict_high[df$Evict_Serious=="high"]<-1
df$evict_high[df$Evict_Serious!="high"]<-0
df$evict_medium[df$Evict_Serious=="medium"]<-1
df$evict_medium[df$Evict_Serious!="medium"]<-0
df$evict_low[df$Evict_Serious=="low"]<-1
df$evict_low[df$Evict_Serious!="low"]<-0

#INCORPORATING FIXED EFFECTS IN MODELS
df$country <- factor(df$country)

#H1:Actual number of evictions is not correlated to seriousness of evictions
#Probit Model on evict_high versus number of eviction events. p value = 0.421 > 0.05. We cannot ignore the null that these are uncorrelated
reg_evict1 <- glm(evict_high ~ Evict_Threat_Count, 
                  family = binomial(link = "probit"), 
                  data = df)
summary(reg_evict1)
#Probit Model on evict_medium versus number of eviction events. p value = 0.421 > 0.05. We cannot ignore the null that these are uncorrelated
reg_evict2 <- glm(evict_medium ~ Evict_Threat_Count, 
                  family = binomial(link = "probit"), 
                  data = df)
summary(reg_evict2)
#FINDING 1: We find indications that actual number of evictions is conclusively unrelated to number of eviction events 
#H2:How does access to infrastructure and business ownership positively correlate to feeling the threat of eviction?
reg_evict3 <- glm(evict_high ~ Evict_Threat_Count + bus_usage + country + hazardous, 
                  family = binomial(link = "probit"), 
                  data = df)
summary(reg_evict3)
#FINDING 2: Increasing ownership of business within the settlements is correlated strongly with a perception of high eviction threat
#Interestingly, the STI does not show a strong correlation with this

#CORRELATIONS BETWEEN INDICES 
#H3: How are HI and STI correlated? 
reg_HI <- lm(HI ~ STI, data = df)
summary(reg_HI)
#FINDING 3: These indices are highly correlated - R2 is 48.36%. There is a very high degree of statistical significance
reg_MI <- lm(MI ~ STI, data = df)
summary(reg_MI)
#FINDING 4: Very low correlation - R2 is -ve. Coefficient is not statistically significant

#H5: How does HI correlate with its % of access of infrastructure to significant levels? In other words, does it reflect the right levels of access
reg_HI1<- lm(tenure_security ~ access_water + access_electricity + access_shelter + improved_sanitation + pop_density + liv_area, data = df)
summary(reg_HI1)
#FINDING 5: The Housing Infrastructure Index is not correlated significantly to any of the access measures except for access_shelter. Now, this may be because the small variances in the access measures generate very imprecise estimates
#Demonstrating graphically, this is also proven that there is little variation in the other variables
#There is a weak trend that indicates that as access increases, so does HI but very high correlation with access to shelter. Most settlements have <25% water access hence not reflected in HI
p8 <- ggplot(df, aes(HI, y = value, color = variable)) +
  geom_point(aes(y=access_water, col = "Safe water"), alpha = 0.30, size = 2)+
  geom_point(aes(y=access_electricity, col = "Continuous electricity"), alpha = 0.30, size = 2)+ 
  geom_point(aes(y=access_shelter, col = "Permanent shelters"), alpha = 0.30, size = 2)+ 
  geom_point(aes(y=improved_sanitation, col = "Improved sanitation"), alpha = 0.30, size = 2)+ 
  geom_point(aes(y=liv_area, col = "Sufficient Living Area"), alpha = 0.30, size = 2)+ 
  xlab("Housing Infrastructure Index")+
  ylab("% Households with access ")
  ggtitle("HI is correlated to infrastructure access")
print(p8)
ggsave("Viz\\HI Correlated.png", p8, width = 10, height = 8)

#TENURE SECURITY AS A PRIORITY CONTROLLING FOR ACCESS
#H4: STI is not correlated with tenure security. HI is representing (largely) access to safe shelters!
#Correlation between the tenure_security and private ownership is not statistically significant
#There is an indicative correlation between tenure security and legal status as well as hazards at a location. 
reg_ten_sec <- lm(tenure_security ~ HI + legal + hazardous, data = df)
summary(reg_ten_sec)
#When the model is refined to include country effects, R2 goes up but the coefficients to legal and hazardous goes down - 
#This means tenure security largely has country effects
reg_ten_sec1 <- lm(tenure_security ~ HI + legal + hazardous + country + hazardous*country, data = df)
summary(reg_ten_sec1)
fitlist <- list(reg_ten_sec,reg_ten_sec1)
table1 <- outreg(setNames(fitlist, c('Excl. Country Effects','Incl. fixed effects')), displayed = list(pv = TRUE, tv = TRUE))
write.csv(table1, "reg_results1.csv")

#H5: Is HI correlated with area? In other words, are there scaling effects?
reg_scale <- lm(HI ~ legal + area, data = df)
summary(reg_scale)
reg_scale1 <- lm(HI ~ legal + area + country, data = df)
summary(reg_scale1)
fitlist <- list(reg_scale, reg_scale1)
table2 <- outreg(setNames(fitlist, c('Excl. Country Effects','Incl. fixed effects')), displayed = list(pv = TRUE, tv = TRUE))
write.csv(table2, "reg_results2.csv", append = TRUE)

#H6: Is population density representing (largely) access to safe shelters correlated with population density?
#FINDING 5: Even accounting for country effects, higher population density is correlated with lower HI. 
#People move for reasons other than infrastructure access - could be proximity to work etc. 
reg_dens <- lm(pop_density ~ HI, data = df)
summary(reg_dens)
reg_dens1 <- lm(pop_density ~ HI + country, data = df)
summary(reg_dens1)
fitlist <- list(reg_dens, reg_dens1)
table2 <- outreg(setNames(fitlist, c('Excl. Country Effects','Incl. fixed effects')), displayed = list(pv = TRUE, tv = TRUE))
write.csv(table2, "reg_results3.csv")
#H7: Water_drainage is the most common top priority for most communities - is this only affected by access to water and sanitation?
#Hazardous and country effects have significant effects: Nigeria, Uganda, Senegal and Zimbabwe have the most pronounced effects.
#Objectively, access to water and improved sanitation seem to effect less than presence of hazards
reg_dens_wd <- lm(water_drainage ~  access_water + improved_sanitation + hazardous + country, data = df)
summary(reg_dens_wd)
reg_dens_sn <- lm(sanitation_sewage ~  improved_sanitation + hazardous + country, data = df)
summary(reg_dens_sn)

