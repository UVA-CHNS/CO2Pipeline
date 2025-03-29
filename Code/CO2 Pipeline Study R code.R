rm(list = ls())
library(readr)
library(dplyr)
library(glmnet)
library(caret)
library(ggplot2)
library(MASS)
library(factoextra)
library(openxlsx)
library(coefplot)
library(scales)
library(stringr)
library(tibble)
library(patchwork)
library(extrafont)
library(marginaleffects)
library(knitr)
library(margins)
library(margins)
library(boot)


# Current code is using the 2017-2021 data, but it can easily be replaced with other years for comparison.

X2021_demographic <- read_csv("C:/Users/julia/OneDrive - University of Virginia/PhD Stuff/Research/Pipeline/DataClean/current 2017-2021 demographic/2017-2021demographic.csv")
X2022_pipeline <- read_csv("C:/Users/julia/OneDrive - University of Virginia/PhD Stuff/Research/Pipeline/DataClean/2022 pipeline.csv")

Accidents <- read_csv("C:/Users/julia/OneDrive - University of Virginia/PhD Stuff/Research/Pipeline/DataClean/Accidents_Tract.csv")
##Accidents: each row is an accident, and it has the AFFGEOID value for each accident
###will need to append these to the big proposed pipeline datasheet
Proposed <- read_csv("C:/Users/julia/OneDrive - University of Virginia/PhD Stuff/Research/Pipeline/DataClean/Tract_Buffer.csv")
##Proposed pipelines excludes the Trailblazer conversion pipeline
Demographic <- read_csv("C:/Users/julia/OneDrive - University of Virginia/PhD Stuff/Research/Pipeline/DataClean/current 2017-2021 demographic/2017-2021demographic.csv")

Projected <- read_csv("C:/Users/julia/OneDrive - University of Virginia/PhD Stuff/Research/Pipeline/DataClean/Projected_Tract_2030.csv")
DemographicProj <- read_csv("C:/Users/julia/OneDrive - University of Virginia/PhD Stuff/Research/Pipeline/DataClean/current 2017-2021 demographic/2017-2021demographic.csv")

#Step 1: Initial cleaning. Start by removing no area pipeline data and no population demographic data


# Using dataset X2021_demographic
# Change the demographic dataset to have no rows where AON5E001 (total population) is equal to 0
X2021_demographic <- X2021_demographic[X2021_demographic$AON5E001 != 0, ]

# this changed it from 85395 rows to 84524 rows (erased 871 rows)



# Using dataset X2022_pipeline
# Change the pipeline dataset to have no rows where ALAND (land area) is equal to 0
X2022_pipeline <- X2022_pipeline[X2022_pipeline$ALAND != 0, ]

# this changed it from 85185 rows to 85159 rows (erased 26 rows)





#Step 2: Merging X2021_demographic and X2022_pipeline


#This makes a new dataset (FullDataDemogPipe) that joins the demographic and pipeline 
# data for all rows in which the GEO_ID value (from demographic data) matches the AFFGEOID 
# value (from pipeline data. Only GEO_ID variable will be shown now.
FullDataDemogPipe <- inner_join(X2021_demographic, X2022_pipeline, by = c("GEO_ID" = "AFFGEOID"))
#View(FullDataDemogPipe)



#This makes a new dataset (ExtraDemog) that holds the rows/observations in the demographic
# dataset that did not match to any rows/observations in the pipeline dataset.
ExtraDemog<- anti_join(X2021_demographic, X2022_pipeline, by = c("GEO_ID" = "AFFGEOID"))
ExtraDemog$Source <- "Demographic"
# ExtraDemog has stuff that was in the demographic data but not pipeline data


#This makes a new dataset (ExtraPipe) that holds the rows/observations in the pipeline
# dataset that did not match to any rows/observations in the demographic dataset.
ExtraPipe<- anti_join(X2022_pipeline, X2021_demographic, by = c("AFFGEOID" = "GEO_ID"))
ExtraPipe$Source <- "Pipeline"
# ExtraPipe has stuff that was in the pipeline data but not deographic data

#View(ExtraDemog)
#View(ExtraPipe)




#Step 3: Create any new variables necessary to add in later regressions:

#Population Density
# Now create a new column for the variable called `Population Density`, which represents the population 
# density for each row (by dividing the total population by the area for each census tract). This
# may be useful for our regressions later on.

#First change the land area from square meters to square miles
FullDataDemogPipe$ALAND <- FullDataDemogPipe$ALAND / 2.59e+6


FullDataDemogPipe$`Population Density` <- FullDataDemogPipe$AON5E001 / FullDataDemogPipe$ALAND

#FullDataDemogPipe$`Population Denisty` <- FullDataDemogPipe$`Population Density` * 1000

#Rural/`Urban Indicator`
#Create another column that indicates if a tract is rural (equal to zero) or `Urban Indicator` (equal to 1).
# A tract will be considered rural if the population density is less than or equal to 1000 people
# per square mile, and `Urban Indicator` if the population density is greater than 1000 people/sq mile.

FullDataDemogPipe$`Urban Indicator` <- ifelse(FullDataDemogPipe$`Population Density` > 1000, 1, 0)

#Here I'll need to add the 4 type variable with the metropolitan variable (need to figure that out)



#Income and Poverty
#Create a new column, called IncTot, which represents the total number of observations within
# each household income bracket for each census tract.

FullDataDemogPipe$IncTot <- rowSums(FullDataDemogPipe[, c("AOQHE002", "AOQHE003", "AOQHE004", "AOQHE005", "AOQHE006", "AOQHE007", "AOQHE008", "AOQHE009", "AOQHE010", "AOQHE011", "AOQHE012", "AOQHE013", "AOQHE014", "AOQHE015", "AOQHE016", "AOQHE017")])
#this wasn't necessary to do

#Create another column, called `High Income Household Rates`, which represents the percentage of households within each
# census tract that make at least $200K per year

FullDataDemogPipe$`High Income Household Rates` <- (FullDataDemogPipe$AOQHE017 / FullDataDemogPipe$IncTot) * 100


#Create a new column, called PovTot, which represents the total number of observations within
# each poverty ratio bracket for each census tract.
FullDataDemogPipe$PovTot <- rowSums(FullDataDemogPipe[, c("AOXWE002", "AOXWE003", "AOXWE004", "AOXWE005", "AOXWE006", "AOXWE007", "AOXWE008")])
#this wasn't necessary to do

#Create another column, called `Low Poverty Rate`, which represents the percentage of households within each
# census tract that have at a high ratio of income to poverty level (2 or greater)
FullDataDemogPipe$`Low Poverty Rate` <- (FullDataDemogPipe$AOXWE008 / FullDataDemogPipe$PovTot) * 100

#Create another column, called `High Poverty Rate`, which represents the percentage of households within each
# census tract that have at a low ratio of income to poverty level (under 0.5)
FullDataDemogPipe$`High Poverty Rate` <- (FullDataDemogPipe$AOXWE002 / FullDataDemogPipe$PovTot) * 100



#Housing Occupation Rates
#Create a new column, called HouseTot, which represents the total number of observations within
# each housing occupation bracket for each census tract (both renters and owners).
FullDataDemogPipe$HouseTot <- rowSums(FullDataDemogPipe[, c("AOSPE002", "AOSPE003")])
#this wasn't necessary to do

#Create another column, called `Homeownership Rates`, which represents the percentage of households within each
# census tract where housing units are owned (as opposed to being rented).
FullDataDemogPipe$`Homeownership Rates` <- (FullDataDemogPipe$AOSPE002 / FullDataDemogPipe$HouseTot) * 100

#Create another column, called HouseRatio_renter, which represents the percentage of households within each
# census tract where housing units are owned (as opposed to being rented).
FullDataDemogPipe$HouseRatio_renter <- (FullDataDemogPipe$AOSPE003 / FullDataDemogPipe$HouseTot) * 100


#Create a new column, called VacantTot, which represents the total number of observations within
# each housing occupation bracket for each census tract (both vacant and occupied).
FullDataDemogPipe$VacantTot <- rowSums(FullDataDemogPipe[, c("AOSOE002", "AOSOE003")])
#this wasn't necessary to do

#Create another column, called VacantRatio_occ, which represents the percentage of households within each
# census tract where housing units are occupied (as opposed to being vacant).
FullDataDemogPipe$VacantRatio_occ <- (FullDataDemogPipe$AOSOE002 / FullDataDemogPipe$VacantTot) * 100

#Create another column, called `Vacancy Rates`, which represents the percentage of households within each
# census tract where housing units are occupied (as opposed to being vacant).
FullDataDemogPipe$`Vacancy Rates` <- (FullDataDemogPipe$AOSOE003 / FullDataDemogPipe$VacantTot) * 100



#Race
#Create a new column, called RaceTot, which represents the total number of observations within
# each race bracket for each census tract (should be the same as total population for each census tract)
FullDataDemogPipe$RaceTot <- rowSums(FullDataDemogPipe[, c("AON5E002", "AON5E002", "AON5E003", "AON5E004", "AON5E005", "AON5E006", "AON5E007", "AON5E008")])
#Did not include AON5E009 and AON5E010 because they are subsets of AON5E008.

#Create another column, called `White Race`, which represents the percentage of people within each
# census tract that are `White Race` alone
FullDataDemogPipe$`White Race` <- (FullDataDemogPipe$AON5E002 / FullDataDemogPipe$RaceTot) * 100

#Create another column, called `Black Race`, which represents the percentage of people within each
# census tract that are `Black Race` or African American alone
FullDataDemogPipe$`Black Race` <- (FullDataDemogPipe$AON5E003 / FullDataDemogPipe$RaceTot) * 100

#Create another column, called `American Indian Race`, which represents the percentage of people within each
# census tract that are American Indian and Alaska Native alone
FullDataDemogPipe$`American Indian Race` <- (FullDataDemogPipe$AON5E004 / FullDataDemogPipe$RaceTot) * 100

#Create another column, called `Asian Race`, which represents the percentage of people within each
# census tract that are `Asian Race` alone
FullDataDemogPipe$`Asian Race` <- (FullDataDemogPipe$AON5E005 / FullDataDemogPipe$RaceTot) * 100

#Create another column, called `Native Hawaiian Race`, which represents the percentage of people within each
# census tract that are Native Hawaiian and Other Pacific Islander alone
FullDataDemogPipe$`Native Hawaiian Race` <- (FullDataDemogPipe$AON5E006 / FullDataDemogPipe$RaceTot) * 100

#Create another column, called `Other/Unspecified Race`, which represents the percentage of people within each
# census tract that are some other race alone
FullDataDemogPipe$`Other/Unspecified Race` <- (FullDataDemogPipe$AON5E007 / FullDataDemogPipe$RaceTot) * 100

#Create another column, called TwoPlus_Race, which represents the percentage of people within each
# census tract that are two or more races
FullDataDemogPipe$TwoPlus_Race <- (FullDataDemogPipe$AON5E008 / FullDataDemogPipe$RaceTot) * 100



#Educational Attainment
#Create a new column, called EduTot, which represents the total number of observations within
# each education bracket for each census tract (should be the same as total population for each census tract)
FullDataDemogPipe$EduTot <- rowSums(FullDataDemogPipe[, c("AOP8E002", "AOP8E003", "AOP8E004", "AOP8E005", "AOP8E006", "AOP8E007", "AOP8E008", "AOP8E009", "AOP8E010", "AOP8E011", "AOP8E012", "AOP8E013", "AOP8E014", "AOP8E015", "AOP8E016", "AOP8E017", "AOP8E018", "AOP8E019", "AOP8E020", "AOP8E021", "AOP8E022", "AOP8E023", "AOP8E024", "AOP8E025")])

#Create another column, called `Highest Education Level: None`, which represents the percentage of people within each
# census tract that have no completed education
FullDataDemogPipe$`Highest Education Level: None` <- (FullDataDemogPipe$AOP8E002 / FullDataDemogPipe$EduTot) * 100

#Create another column, called `Highest Education Level: High School`, which represents the percentage of people within each
# census tract that have completed regular high school as their highest education level
FullDataDemogPipe$`Highest Education Level: High School` <- (FullDataDemogPipe$AOP8E017 / FullDataDemogPipe$EduTot) * 100

#Create another column, called `Highest Education Level: GED/High School`, which represents the percentage of people within each
# census tract that have completed high school or has a GED/alternative credential as their highest education level
FullDataDemogPipe$`Highest Education Level: GED/High School` <- ((FullDataDemogPipe$AOP8E017 + FullDataDemogPipe$AOP8E018) / FullDataDemogPipe$EduTot) * 100

#Create another column, called `Highest Education Level: Some College`, which represents the percentage of people within each
# census tract that have some college education as their highest education level
FullDataDemogPipe$`Highest Education Level: Some College` <- ((FullDataDemogPipe$AOP8E019 + FullDataDemogPipe$AOP8E020) / FullDataDemogPipe$EduTot) * 100

#Create another column, called `Highest Education Level: Associate or Bachelor`, which represents the percentage of people within each
# census tract that have an Associate's or `Highest Education Level: Bachelor`'s degree their highest education level
FullDataDemogPipe$`Highest Education Level: Associate or Bachelor` <- ((FullDataDemogPipe$AOP8E021 + FullDataDemogPipe$AOP8E022) / FullDataDemogPipe$EduTot) * 100

#Create another column, called `Highest Education Level: Bachelor`, which represents the percentage of people within each
# census tract that have a `Highest Education Level: Bachelor`'s degree their highest education level
FullDataDemogPipe$`Highest Education Level: Bachelor` <- (FullDataDemogPipe$AOP8E022 / FullDataDemogPipe$EduTot) * 100

#Create another column, called `Highest Education Level: Master/Professional/Doctorate`, which represents the percentage of people within each
# census tract that have a Master's, professional school, or Doctorate degree their highest education level
FullDataDemogPipe$`Highest Education Level: Master/Professional/Doctorate` <- ((FullDataDemogPipe$AOP8E023 + FullDataDemogPipe$AOP8E024 + FullDataDemogPipe$AOP8E025) / FullDataDemogPipe$EduTot) * 100




#Variables I have:

##Population density -- `Population Density` (measured as people per square mile)
###As population density increases by one person per square mile, Y variable changes by

##`Urban Indicator` -- `Urban Indicator` (measured as 1 if the population density is greater than 1000 people/sq mile)
###Compared to rural areas, `Urban Indicator` areas have an associated change in the Y variable by

##High income -- `High Income Household Rates` (measured as % of households who have an income of at least $200k)
###As the share of high-income households increases by one percentage point, Y variable changes by

##High poverty -- `High Poverty Rate` (measured as % of population who has an income to poverty level ratio of under 0.5)
###As the share of poor people increases by one percentage point, Y variable changes by

##Low poverty -- `Low Poverty Rate` (measured as % of population who has an income to poverty level ratio of at least 2)
###As the share of non-poor people increases by one percentage point, Y variable changes by

##Homeowners -- `Homeownership Rates` (measured as % of occupied housing units that are owned)
###Within occupied housing units, as the share of homeowners increases by one percentage point, Y variable changes by

##Renters -- HouseRatio_renter (measured as % of occupied housing units that are rented)
###Within occupied housing units, as the share of renters increases by one percentage point, Y variable changes by
####Repetitive, will probably use homeowners

##`Vacancy Rates` -- `Vacancy Rates` (measured as % of housing units that are vacant)
###As the share of vacant housing units increases by one percentage point, Y variable changes by

##Occupancy -- VacantRatio_occ (measured as % of housing units that are occupied)
###As the share of occupied housing units increases by one percentage point, Y variable changes by

##`White Race` -- `White Race` (measured as % of people that are `White Race`)
###As the share of people who are `White Race` alone increases by one percentage point, Y variable changes by

##`Black Race` -- `Black Race` (measured as % of people that are `Black Race`)
###As the share of people who are `Black Race` alone increases by one percentage point, Y variable changes by

##American Indian/Native -- `American Indian Race` (measured as % of people that are American Indian)
###As the share of people who are American Indian alone increases by one percentage point, Y variable changes by

##`Asian Race` -- `Asian Race` (measured as % of people that are `Asian Race`)
###As the share of people who are `Asian Race` alone increases by one percentage point, Y variable changes by

##Native Hawaiian/Pacific Islander -- `Native Hawaiian Race` (measured as % of people that are Native Hawaiian)
###As the share of people who are Native Hawaiian alone increases by one percentage point, Y variable changes by

##Other Race -- `Other/Unspecified Race` (measured as % of people that are another race)
###As the share of people who are another race alone increases by one percentage point, Y variable changes by

##Two or more races -- TwoPlus_Race (measured as % of people that are two or more races)
###As the share of people who are two or more races increases by one percentage point, Y variable changes by

##No education -- `Highest Education Level: None` (measured as % of people that have no completed schooling)
###As the share of people who have no completed schooling increases by one percentage point, Y variable changes by

##High school education -- `Highest Education Level: High School` (measured as % of people that have completed regular high school as their highest education level)
###As the share of people who have completed regular high school as their highest education level increases by one percentage point, Y variable changes by

##High school/GED education -- `Highest Education Level: GED/High School` (measured as % of people that have completed regular high school, or received a GED or alternative credential, as their highest education level)
###As the share of people who have completed regular high school, or received a GED or alternative credential, as their highest education level increases by one percentage point, Y variable changes by

##Some college experience -- `Highest Education Level: Some College` (measured as % of people that have some college experience as their highest education level)
###As the share of people who have some college experience as their highest education level increases by one percentage point, Y variable changes by

##Associate/`Highest Education Level: Bachelor` degree -- `Highest Education Level: Associate or Bachelor` (measured as % of people that have an Associate's or `Highest Education Level: Bachelor`'s degree as their highest education level)
###As the share of people who have an Associate's or `Highest Education Level: Bachelor`'s degree as their highest education level increases by one percentage point, Y variable changes by

##`Highest Education Level: Bachelor` degree -- `Highest Education Level: Bachelor` (measured as % of people that have a `Highest Education Level: Bachelor`'s degree as their highest education level)
###As the share of people who have a `Highest Education Level: Bachelor`'s degree as their highest education level increases by one percentage point, Y variable changes by

##Graduate degree -- `Highest Education Level: Master/Professional/Doctorate` (measured as % of people that have a Master's, professional school, or Doctorate degree as their highest education level)
###As the share of people who have a Master's, professional school, or Doctorate degree as their highest education level increases by one percentage point, Y variable changes by



#I'll also need to add any new variables if I need


#Step 4a: remove any census tracts with missing values of any of the variables of interest

variables_to_check <- c("Trac_Bin", "Population Density", "COUNTY", "Urban Indicator", "High Income Household Rates", 
                        "High Poverty Rate", "Low Poverty Rate", "Homeownership Rates", 
                        "Vacancy Rates", "White Race", "Black Race", 
                        "American Indian Race", "Asian Race", "Native Hawaiian Race", 
                        "Other/Unspecified Race", "Highest Education Level: None", "Highest Education Level: High School", 
                        "Highest Education Level: GED/High School", "Highest Education Level: Some College", "Highest Education Level: Associate or Bachelor", 
                        "Highest Education Level: Bachelor", "Highest Education Level: Master/Professional/Doctorate")

# Remove observations with missing values for specified variables
FullDataDemogPipe <- FullDataDemogPipe[complete.cases(FullDataDemogPipe[, variables_to_check]), ]


#Step 4: Now clean data so that there are only counties that have some census tracts that
# do have pipelines and some census tracts that do not (i.e. within a given county value,
# delete if all rows have a pipeline or no rows have a pipeline).



#First: Get rid of counties with no pipelines in any of the census tracts:

#Start by creating a new dataset (FullData_countyzero) of the counties and states that have their total
# pipeline greater than 0
FullData_countyzero <- FullDataDemogPipe%>%
  group_by(NAMELSADCO, STATE_NAME) %>%
  summarise(total_trac_bin = sum(Trac_Bin))%>%
  filter(total_trac_bin > 0)
#View(FullData_countyzero)
#View(FullDataDemogPipe)

#In both the FullDataDemogPipe and FullData_countyzero, create a new column (CountyState in FullDataDemogPipe
# and CountyStates in FullData_countyzero) that has both the county and state name. This is important because
# some states have counties with the same name, and I needed a way to differentiate them from each other.

FullDataDemogPipe$CountyState <- paste(FullDataDemogPipe$NAMELSADCO, FullDataDemogPipe$STATE_NAME, sep = ", ")
#View(FullDataDemogPipe)

FullData_countyzero$CountyStates <- paste(FullData_countyzero$NAMELSADCO, FullData_countyzero$STATE_NAME, sep = ", ")


#Create another dataset called FullData, that has the rows (census tracts) from FullDataDemogPipe if they
# have a matching value in the FullData_countyzero dataset (looking at TractState and TractStates).
FullData <- FullDataDemogPipe %>%
  filter(CountyState %in% FullData_countyzero$CountyStates)

#View(FullData)


#Create a duplicate dataset (called FullDataExistInc) for later regression use that includes census tracts in 
# counties where all census tracts
FullDataExistInc <- FullData

#Second: Get rid of counties with pipelines in all of the census tracts:


#This makes a new dataset called FullData_countyone, with a new variable (PipeTotal) that counts the 
# total number of observations (census tracts) for each CountyState value, within the FullData dataset.
FullData_countyone <- FullData %>%
  group_by(CountyState) %>%
  summarise(PipeTotal = n())

#View(FullData_countyone)

#This joins the FullData_countyzero and FullData_countyone datasets (based on CountyStates and CountyState). 
# This is useful because now I am able to see the number of pipelines in a CountyState value (total_trac_bin),
# as well as the total number of census tracts for that CountyState value (PipeTotal).
FullData_county <- inner_join(FullData_countyzero, FullData_countyone, by = c("CountyStates" = "CountyState"))
#View(FullData_county)

#This removes the CountyState values where the number of pipelines in a CountyState equals the total number 
# of census tracts for that CountyState value. I am doing this because if they equal the same amount, that
# means that every census tract within a CountyState value has a pipeline in it, and we want to remove those.
FullData_county <- FullData_county %>%
  filter(PipeTotal != total_trac_bin)

#Now, FullData_county only has CountyState values where not every census tract has a pipeline in it. So, I
# can keep the observations in FullData where the CountyState value is present in the CountyStates variable
# in the FullData_county dataset.
FullData <- FullData %>%
  filter(CountyState %in% FullData_county$CountyStates)




#Step 4b: add remaining variables related to accidents. Do for both FullData and FullDataExist datasets.



##Accidents dataset: each row is an accident, and it has the AFFGEOID value for each accident
###will need to append these to the big proposed pipeline datasheet... something like a code where
### it makes a new column in the big proposed datasheet and then it looks to see if the AFFGEOID 
### matches between big proposed datasheet and the accidents datasheet and if there is a match, set 
### the new column value equal to one for that AFFGEOID, and if not, set zero (for the question of
### if there is a correlation between if there's an accident and if there's a proposed pipeline)

##Border dataset: each row is a census tract, can append it to the proposed datasheet the same way 
### as existing datasheet earlier

##correlation between if there is an accident in a census tract and if there is a proposed pipeline
##correlation between the number of accidents in a census tract and if there is a proposed pipeline
##correlation between distance from an accident and if there is a proposed pipeline
##correlation between number of days since an accident in a census tract and if there is a proposed pipeline






###Make a column (called Prev_Acc) that looks at the accidents list and if there are any accidents
### for a given census tract, it gives the value of 1 for Prev_Acc, and if there are no accidents,
### it gives the value zero.

#View(FullData)
#View(FullDataExistInc)
#View(Accidents)

####This creates a new datatset called Prev_Acc_Count with a new variable (AccTotal) that counts the 
# total number of observations (accidents) for each AFFGEOID value, within the Accidents dataset.
Prev_Acc_Count_CT <- Accidents %>%
  group_by(AFFGEOID) %>%
  summarise(AccTotal = n(), LOCAL_DAYSSINCE = LOCAL_DAYSSINCE[which.min(LOCAL_DAYSSINCE)])
#View(Prev_Acc_Count_CT)



# Merge datasets based on GEO ID.
merged_data_CT <- merge(Prev_Acc_Count_CT, FullData, by.x = "AFFGEOID", by.y = "GEO_ID", all.y = TRUE)
merged_data_CT$AccTotal <- ifelse(is.na(merged_data_CT$AccTotal), 0, merged_data_CT$AccTotal)
###Only 69 accidents

# Do same for inc dataset
merged_data_CT_inc <- merge(Prev_Acc_Count_CT, FullDataExistInc, by.x = "AFFGEOID", by.y = "GEO_ID", all.y = TRUE)
merged_data_CT_inc$AccTotal <- ifelse(is.na(merged_data_CT_inc$AccTotal), 0, merged_data_CT_inc$AccTotal)
###86 accidents in this, compared to 90 in total

#View(merged_data_CT)
#View(merged_data_CT_inc)

#Create another column, called Prev_Acc, that indicates if a tract is has any accidents in its state 
# (equal to 1), or not (equal to 0).

merged_data_CT$Prev_Acc <- ifelse(merged_data_CT$AccTotal > 0, 1, 0)
merged_data_CT_inc$Prev_Acc <- ifelse(merged_data_CT_inc$AccTotal > 0, 1, 0)


###Make a column (called AccTotal) that looks at the accidents list and counts the number of accidents
### for each given census tract. It returns the number of accidents for each census tract, and if there
### are no census tracts/accidents (for the missing census tracts), it gives the value zero.
#####already did this abovr


###Create a new column called DaysSinceAcc that counts the number of days it has been (from December 31, 2023)
### since the last accident in each census tract has taken place.

####I did this in excel (in the Accidents dataset), and then added it to the datasets above

merged_data_CT$DaysSinceAcc <- merged_data_CT$LOCAL_DAYSSINCE
merged_data_CT_inc$DaysSinceAcc <- merged_data_CT_inc$LOCAL_DAYSSINCE


FullData <- merged_data_CT
FullDataExistInc <- merged_data_CT_inc


##This includes only the CTs of each dataset that have a pipeline.
FullDataAcc <- FullData[FullData$Trac_Bin == 1, ]
FullDataAccInc <- FullDataExistInc[FullDataExistInc$Trac_Bin == 1, ]



#Step 5a: descriptive statistics

##Whole US
##Census Tracts in Restricted Sample
mean(FullDataDemogPipe$AON5E001,na.rm = TRUE) ##Population
mean(FullDataDemogPipe$`Population Density`, na.rm = TRUE)
mean(FullDataDemogPipe$`Urban Indicator`, na.rm = TRUE)
mean(FullDataDemogPipe$`High Income Household Rates`, na.rm = TRUE)
mean(FullDataDemogPipe$`High Poverty Rate`, na.rm = TRUE)
mean(FullDataDemogPipe$`Low Poverty Rate`, na.rm = TRUE)
mean(FullDataDemogPipe$`Homeownership Rates`, na.rm = TRUE)
mean(FullDataDemogPipe$`Vacancy Rates`, na.rm = TRUE)
mean(FullDataDemogPipe$`White Race`, na.rm = TRUE)
mean(FullDataDemogPipe$`Black Race`, na.rm = TRUE)
mean(FullDataDemogPipe$`American Indian Race`, na.rm = TRUE)
mean(FullDataDemogPipe$`Asian Race`, na.rm = TRUE)
mean(FullDataDemogPipe$`Native Hawaiian Race`, na.rm = TRUE)
mean(FullDataDemogPipe$`Other/Unspecified Race`, na.rm = TRUE)
mean(FullDataDemogPipe$`Highest Education Level: None`, na.rm = TRUE)
mean(FullDataDemogPipe$`Highest Education Level: High School`, na.rm = TRUE)
mean(FullDataDemogPipe$`Highest Education Level: GED/High School`, na.rm = TRUE)
mean(FullDataDemogPipe$`Highest Education Level: Some College`, na.rm = TRUE)
mean(FullDataDemogPipe$`Highest Education Level: Associate or Bachelor`, na.rm = TRUE)
mean(FullDataDemogPipe$`Highest Education Level: Bachelor`, na.rm = TRUE)
mean(FullDataDemogPipe$`Highest Education Level: Master/Professional/Doctorate`, na.rm = TRUE)


##Census Tracts in Restricted Sample
mean(FullData$AON5E001,na.rm = TRUE) ##Population
mean(FullData$`Population Density`, na.rm = TRUE)
mean(FullData$`Urban Indicator`, na.rm = TRUE)
mean(FullData$`High Income Household Rates`, na.rm = TRUE)
mean(FullData$`High Poverty Rate`, na.rm = TRUE)
mean(FullData$`Low Poverty Rate`, na.rm = TRUE)
mean(FullData$`Homeownership Rates`, na.rm = TRUE)
mean(FullData$`Vacancy Rates`, na.rm = TRUE)
mean(FullData$`White Race`, na.rm = TRUE)
mean(FullData$`Black Race`, na.rm = TRUE)
mean(FullData$`American Indian Race`, na.rm = TRUE)
mean(FullData$`Asian Race`, na.rm = TRUE)
mean(FullData$`Native Hawaiian Race`, na.rm = TRUE)
mean(FullData$`Other/Unspecified Race`, na.rm = TRUE)
mean(FullData$`Highest Education Level: None`, na.rm = TRUE)
mean(FullData$`Highest Education Level: High School`, na.rm = TRUE)
mean(FullData$`Highest Education Level: GED/High School`, na.rm = TRUE)
mean(FullData$`Highest Education Level: Some College`, na.rm = TRUE)
mean(FullData$`Highest Education Level: Associate or Bachelor`, na.rm = TRUE)
mean(FullData$`Highest Education Level: Bachelor`, na.rm = TRUE)
mean(FullData$`Highest Education Level: Master/Professional/Doctorate`, na.rm = TRUE)

mean(FullData$Prev_Acc, na.rm = TRUE)
mean(FullData$AccTotal, na.rm = TRUE)
mean(FullData$DaysSinceAcc, na.rm = TRUE)
sum(!is.na(FullData$DaysSinceAcc))


##Census Tracts in Restricted Sample - INC
mean(FullDataExistInc$AON5E001,na.rm = TRUE) ##Population
mean(FullDataExistInc$`Population Density`, na.rm = TRUE)
mean(FullDataExistInc$`Urban Indicator`, na.rm = TRUE)
mean(FullDataExistInc$`High Income Household Rates`, na.rm = TRUE)
mean(FullDataExistInc$`High Poverty Rate`, na.rm = TRUE)
mean(FullDataExistInc$`Low Poverty Rate`, na.rm = TRUE)
mean(FullDataExistInc$`Homeownership Rates`, na.rm = TRUE)
mean(FullDataExistInc$`Vacancy Rates`, na.rm = TRUE)
mean(FullDataExistInc$`White Race`, na.rm = TRUE)
mean(FullDataExistInc$`Black Race`, na.rm = TRUE)
mean(FullDataExistInc$`American Indian Race`, na.rm = TRUE)
mean(FullDataExistInc$`Asian Race`, na.rm = TRUE)
mean(FullDataExistInc$`Native Hawaiian Race`, na.rm = TRUE)
mean(FullDataExistInc$`Other/Unspecified Race`, na.rm = TRUE)
mean(FullDataExistInc$`Highest Education Level: None`, na.rm = TRUE)
mean(FullDataExistInc$`Highest Education Level: High School`, na.rm = TRUE)
mean(FullDataExistInc$`Highest Education Level: GED/High School`, na.rm = TRUE)
mean(FullDataExistInc$`Highest Education Level: Some College`, na.rm = TRUE)
mean(FullDataExistInc$`Highest Education Level: Associate or Bachelor`, na.rm = TRUE)
mean(FullDataExistInc$`Highest Education Level: Bachelor`, na.rm = TRUE)
mean(FullDataExistInc$`Highest Education Level: Master/Professional/Doctorate`, na.rm = TRUE)

mean(FullDataExistInc$Prev_Acc, na.rm = TRUE)
mean(FullDataExistInc$AccTotal, na.rm = TRUE)
mean(FullDataExistInc$DaysSinceAcc, na.rm = TRUE)
sum(!is.na(FullDataExistInc$DaysSinceAcc))



#Step 5a1: PCA

# Create a new dataset with selected variables
FullDataExistInc_simple <- FullDataExistInc[, c("Trac_Bin", "Population Density", "COUNTY", "Urban Indicator", "High Income Household Rates", 
                                                "High Poverty Rate", "Low Poverty Rate", "Homeownership Rates", 
                                                "Vacancy Rates", "White Race", "Black Race", 
                                                "American Indian Race", "Asian Race", "Native Hawaiian Race", 
                                                "Other/Unspecified Race", "Highest Education Level: None", "Highest Education Level: High School", 
                                                "Highest Education Level: GED/High School", "Highest Education Level: Some College", "Highest Education Level: Associate or Bachelor", 
                                                "Highest Education Level: Bachelor", "Highest Education Level: Master/Professional/Doctorate")]
#View(FullDataExistInc_simple)


#changes variables from num to integers
# Convert numeric variables to integer in the dataset
FullDataExistInc_simple$Trac_Bin <- as.integer(FullDataExistInc_simple$Trac_Bin)
FullDataExistInc_simple$`Urban Indicator` <- as.integer(FullDataExistInc_simple$`Urban Indicator`)
FullDataExistInc_simple$`High Income Household Rates` <- as.integer(FullDataExistInc_simple$`High Income Household Rates`)
FullDataExistInc_simple$`High Poverty Rate` <- as.integer(FullDataExistInc_simple$`High Poverty Rate`)
FullDataExistInc_simple$`Low Poverty Rate` <- as.integer(FullDataExistInc_simple$`Low Poverty Rate`)
FullDataExistInc_simple$`Homeownership Rates` <- as.integer(FullDataExistInc_simple$`Homeownership Rates`)
FullDataExistInc_simple$`Vacancy Rates` <- as.integer(FullDataExistInc_simple$`Vacancy Rates`)
FullDataExistInc_simple$`White Race` <- as.integer(FullDataExistInc_simple$`White Race`)
FullDataExistInc_simple$`Black Race` <- as.integer(FullDataExistInc_simple$`Black Race`)
FullDataExistInc_simple$`Asian Race` <- as.integer(FullDataExistInc_simple$`Asian Race`)
FullDataExistInc_simple$`American Indian Race` <- as.integer(FullDataExistInc_simple$`American Indian Race`)
FullDataExistInc_simple$`Native Hawaiian Race` <- as.integer(FullDataExistInc_simple$`Native Hawaiian Race`)
FullDataExistInc_simple$`Other/Unspecified Race` <- as.integer(FullDataExistInc_simple$`Other/Unspecified Race`)
FullDataExistInc_simple$`Highest Education Level: None` <- as.integer(FullDataExistInc_simple$`Highest Education Level: None`)
FullDataExistInc_simple$`Highest Education Level: High School` <- as.integer(FullDataExistInc_simple$`Highest Education Level: High School`)
FullDataExistInc_simple$`Highest Education Level: GED/High School` <- as.integer(FullDataExistInc_simple$`Highest Education Level: GED/High School`)
FullDataExistInc_simple$`Highest Education Level: Some College` <- as.integer(FullDataExistInc_simple$`Highest Education Level: Some College`)
FullDataExistInc_simple$`Highest Education Level: Associate or Bachelor` <- as.integer(FullDataExistInc_simple$`Highest Education Level: Associate or Bachelor`)
FullDataExistInc_simple$`Highest Education Level: Bachelor` <- as.integer(FullDataExistInc_simple$`Highest Education Level: Bachelor`)
FullDataExistInc_simple$`Highest Education Level: Master/Professional/Doctorate` <- as.integer(FullDataExistInc_simple$`Highest Education Level: Master/Professional/Doctorate`)
FullDataExistInc_simple$`Population Density` <- as.integer(FullDataExistInc_simple$`Population Density`)


str(FullDataExistInc_simple)


PCA_exist <- na.omit(FullDataExistInc_simple)

#Run PCA
PCA_exist <- prcomp(PCA_exist, scale=TRUE)

#Summary of analysis
summary(PCA_exist)

#Components
names(PCA_exist)

#std dev of components
PCA_exist$sdev

#eigenvectors
PCA_exist$rotation

#std dev and mean
PCA_exist$center
PCA_exist$scale

#Principal component scores
PCA_exist$x

#plot of variance
fviz_eig(PCA_exist, addlabels = TRUE)

#default settings
fviz_pca_biplot(PCA_exist)
#with labeled variables
fviz_pca_biplot(PCA_exist, label="var")







#Step 5b: Machine learning to determine the best variables for regression

##make sure Trac_Bin is numeric
#FullDataExistInc$Trac_Bin <- as.numeric(FullDataExistInc$Trac_Bin)


# Convert character variables to factors
#character_vars <- sapply(FullDataExistInc, is.character)
#FullDataExistInc[, character_vars] <- lapply(FullDataExistInc[, character_vars], as.factor)

##Split data into training and testing sets for model evaluation
#set.seed(123)  # Set seed for reproducibility
#index <- createDataPartition(FullDataExistInc$Trac_Bin, p = 0.8, list = FALSE)
#train_data <- FullDataExistInc[index, ]
#test_data <- FullDataExistInc[-index, ]


# Fit the model
#cv_model <- cv.glmnet(x, y, alpha = 1)

##Plot to identify optimal lambda value
#plot(cv_model)

# Choose lambda that minimizes error (where the curve levels off or starts to increase.)
#lasso_model <- cv_model$glmnet.fit

# Predictions on the test set
#x_test <- model.matrix(~ `Urban Indicator` + `High Income Household Rates` + `High Poverty Rate` + `Low Poverty Rate` + `Homeownership Rates` + `Vacancy Rates` + `White Race` + `Black Race` + `Asian Race` + `American Indian Race` + `Native Hawaiian Race` + `Other/Unspecified Race` + `Highest Education Level: None` + `Highest Education Level: High School` + `Highest Education Level: GED/High School` + `Highest Education Level: Some College` + `Highest Education Level: Associate or Bachelor` + `Highest Education Level: Bachelor` + `Highest Education Level: Master/Professional/Doctorate` + `Population Density`, data = test_data)[, -1]
#predictions <- predict(lasso_model, newx = x_test, s = cv_model$lambda.min)



#results <- data.frame(Actual = test_data$Trac_Bin, Predicted = predictions)
##View(results)

#confusion_matrix <- confusionMatrix(table(results$Predicted, results$Actual))
##View(confusion_matrix)







####YOUTUBE VIDEO: LASSO REGRESSION

####EXISTING DATA
##make sure Trac_Bin is numeric
train_FullDataExistInc <- FullDataExistInc
train_FullDataExistInc$Trac_Bin <- as.factor(FullDataExistInc$Trac_Bin)


## Partition the data (80% training, 20% testing)
set.seed(123)
index <- createDataPartition(train_FullDataExistInc$Trac_Bin, p=0.8, list=FALSE, times=1)
train_FDExistInc <- train_FullDataExistInc[index,]
test_FDExistInc <- train_FullDataExistInc[-index,]

## Define k-fold cross-validation (10-fold cross-validation) framework
ctrlspecs <- trainControl(method="cv", number=10, savePredictions="all")

## Create vector of potential lambda values
lambda_vector <- 10^seq(3, -3, length=500)

## Define alpha values for elastic net regularization (mixing parameter)
alpha_values <- seq(0, 1, by=0.1)

## Define tuning grid with lambda and alpha values
tuning_grid <- expand.grid(alpha = 1, lambda = lambda_vector)

## Specify LASSO regression model to be estimated using training data
lassomodel1 <- train(Trac_Bin ~ `Urban Indicator` + `High Income Household Rates` + `High Poverty Rate` + `Low Poverty Rate` + `Homeownership Rates` + `Vacancy Rates` + `White Race` + `Black Race` + `Asian Race` + `American Indian Race` + `Native Hawaiian Race` + `Other/Unspecified Race` + `Highest Education Level: None` + `Highest Education Level: High School` + `Highest Education Level: GED/High School` + `Highest Education Level: Some College` + `Highest Education Level: Associate or Bachelor` + `Highest Education Level: Bachelor` + `Highest Education Level: Master/Professional/Doctorate` + `Population Density`,
                     data=train_FDExistInc,
                     preProcess = c("center", "scale"),
                     method="glmnet",
                     trControl=ctrlspecs,
                     tuneGrid=tuning_grid,
                     na.action=na.omit)


## Print best lambda and alpha values
lassomodel1$bestTune
lassomodel1$bestTune$lambda


#LASSO regression model coefficients
coef(lassomodel1$finalModel, lassomodel1$bestTune$lambda)
round(coef(lassomodel1$finalModel, lassomodel1$bestTune$lambda), 3)

#Plot log(lambda) and RMSE
plot(lassomodel1$results$lambda,
     lassomodel1$results$RMSE,
     xlab="log(lambda)",
     ylab="RMSE")


##Variable importance
varImp(lassomodel1)


#Data visualization of variable importance
#install.packages("ggplot2")
ggplot(varImp(lassomodel1))


#Model Prediction
predictions1 <- predict(lassomodel1, newdata=test_FDExistInc)

##Model performance/accuracy

# Convert factors to numeric if necessary
if(is.factor(predictions1))
  predictions1 <- as.numeric(as.character(predictions1))
if(is.factor(test_FDExistInc$Trac_Bin))
  test_FDExistInc$Trac_Bin <- as.numeric(as.character(test_FDExistInc$Trac_Bin))


mod1perform <- data.frame(RMSE=RMSE(predictions1, test_FDExistInc$Trac_Bin),
                          Rsquared=R2(predictions1, test_FDExistInc$Trac_Bin))

#View(mod1perform)
#a small: r square of 0.01
#a medium: 0.09
#a large: 0.25


ggplot(varImp(lassomodel1)) + labs(x="Demographic Variable") #existing


###Removing ``
ggplot(varImp(lassomodel1), aes(x = Variable, y = Importance)) +
  geom_bar(stat = "identity") +
  labs(x = "Demographic Variable", y = "Importance") +
  scale_x_discrete(labels = function(x) gsub("`", "", x))



### Removing 0 importance variables

# Extract variable importance as a data frame
varImp_data <- varImp(lassomodel1)$importance

# Add row names as a new column for variable names
varImp_data <- varImp_data %>%
  rownames_to_column(var = "Variable")

# Filter out variables with zero importance
filtered_data <- varImp_data %>%
  filter(Overall > 0)

# Plot
ggplot(filtered_data, aes(x = reorder(Variable, Overall), y = Overall)) +
  geom_bar(stat = "identity") +
  labs(x = "Demographic Variable", y = "Importance") +
  scale_x_discrete(labels = function(x) gsub("`", "", x)) + coord_flip()


#####NEED TO DO WITH ACCIDENTS OUTCOMES





#Step 5: Running logistic regressions

##Strube's variables
model1 <- glm(Trac_Bin ~ `Urban Indicator` + `High Poverty Rate` + `Black Race`  +
                `White Race` + `Other/Unspecified Race` + `Highest Education Level: None` + `Highest Education Level: High School` +
                `Highest Education Level: GED/High School` + `Highest Education Level: Some College` + `Highest Education Level: Bachelor` + `Highest Education Level: Master/Professional/Doctorate` + as.factor(COUNTY) + `Population Density`,
              data = FullData, family = binomial)

summary(model1)
nobs(model1)


##Model with all variables without interaction
model2 <- glm(Trac_Bin ~ `Urban Indicator` + `High Income Household Rates` + `Low Poverty Rate` + `Homeownership Rates` + 
                `Vacancy Rates` + `White Race` + `Black Race` + `Asian Race` + `American Indian Race` +
                `Highest Education Level: GED/High School` + `Highest Education Level: Some College` + `Highest Education Level: Associate or Bachelor` + `Highest Education Level: Master/Professional/Doctorate` + `Population Density` + as.factor(COUNTY),
              data = FullData, family = binomial)

summary(model2)
nobs(model2)


model02 <- glm(Trac_Bin ~ `Urban Indicator` + `High Poverty Rate` + 
                 `White Race` + `Black Race` + `American Indian Race` + `Other/Unspecified Race` + `Highest Education Level: None` +
                 `Highest Education Level: GED/High School` + `Highest Education Level: Bachelor` + `Highest Education Level: Master/Professional/Doctorate` + as.factor(COUNTY),
               data = FullDataExistInc, family = binomial)

summary(model02)
nobs(model02)


###Pov_Ratio_low (neg to pos) and Race_Ratio_`White Race` (pos to neg) changed signs/directions between model 1 and model 2. Could be because of OVB in model 1,
### or collinearity in model 2. To test this, try adding one variable at a time.
#Model 1: high pov (-)
#Model 2: high inc (-), high pov (+), low pov (+)
##Model 2a: high inc (-)
##Model 2b: high pov (+)
##Model 2c: low pov (+)
##Model 2d: high pov (+), low pov (+)
##Lkely just OVB

model2a <- glm(Trac_Bin ~ `Urban Indicator` + `High Income Household Rates` + `Homeownership Rates` + 
                 `Vacancy Rates` + `White Race` + `Black Race` + `Asian Race` + `American Indian Race` + `Native Hawaiian Race` + `Other/Unspecified Race` + `Highest Education Level: None` + `Highest Education Level: High School` +
                 `Highest Education Level: GED/High School` + `Highest Education Level: Some College` + `Highest Education Level: Associate or Bachelor` + `Highest Education Level: Bachelor` + `Highest Education Level: Master/Professional/Doctorate` + as.factor(COUNTY) + `Population Density`,
               data = FullData, family = binomial)

summary(model2a)
nobs(model2a)


model2b <- glm(Trac_Bin ~ `Urban Indicator` + `High Poverty Rate` + `Homeownership Rates` + 
                 `Vacancy Rates` + `White Race` + `Black Race` + `Asian Race` + `American Indian Race` + `Native Hawaiian Race` + `Other/Unspecified Race` + `Highest Education Level: None` + `Highest Education Level: High School` +
                 `Highest Education Level: GED/High School` + `Highest Education Level: Some College` + `Highest Education Level: Associate or Bachelor` + `Highest Education Level: Bachelor` + `Highest Education Level: Master/Professional/Doctorate` + as.factor(COUNTY) + `Population Density`,
               data = FullData, family = binomial)

summary(model2b)
nobs(model2b)


model2c <- glm(Trac_Bin ~ `Urban Indicator` + `Low Poverty Rate` + `Homeownership Rates` + 
                 `Vacancy Rates` + `White Race` + `Black Race` + `Asian Race` + `American Indian Race` + `Native Hawaiian Race` + `Other/Unspecified Race` + `Highest Education Level: None` + `Highest Education Level: High School` +
                 `Highest Education Level: GED/High School` + `Highest Education Level: Some College` + `Highest Education Level: Associate or Bachelor` + `Highest Education Level: Bachelor` + `Highest Education Level: Master/Professional/Doctorate` + as.factor(COUNTY) + `Population Density`,
               data = FullData, family = binomial)

summary(model2c)
nobs(model2c)


model2d <- glm(Trac_Bin ~ `Urban Indicator` + `Low Poverty Rate` + `High Poverty Rate` + `Homeownership Rates` + 
                 `Vacancy Rates` + `White Race` + `Black Race` + `Asian Race` + `American Indian Race` + `Native Hawaiian Race` + `Other/Unspecified Race` + `Highest Education Level: None` + `Highest Education Level: High School` +
                 `Highest Education Level: GED/High School` + `Highest Education Level: Some College` + `Highest Education Level: Associate or Bachelor` + `Highest Education Level: Bachelor` + `Highest Education Level: Master/Professional/Doctorate` + as.factor(COUNTY) + `Population Density`,
               data = FullData, family = binomial)

summary(model2d)
nobs(model2d)



##Model with all variables and interaction term with `Urban Indicator` and house ownership
model3 <- glm(Trac_Bin ~ `Urban Indicator` + `High Income Household Rates` + `High Poverty Rate` + `Low Poverty Rate` + `Homeownership Rates` + 
                `Vacancy Rates` + `White Race` + `Black Race` + `American Indian Race` + `Asian Race` +
                `Native Hawaiian Race` + `Other/Unspecified Race` + `Highest Education Level: None` + `Highest Education Level: High School` +
                `Highest Education Level: GED/High School` + `Highest Education Level: Some College` + `Highest Education Level: Associate or Bachelor` + `Highest Education Level: Bachelor` + `Highest Education Level: Master/Professional/Doctorate` + `Urban Indicator`:`Homeownership Rates` + as.factor(COUNTY) + `Population Density`,
              data = FullData, family = binomial)

summary(model3)
nobs(model3)




##More interactions
#model4 <- glm(Trac_Bin ~ `Urban Indicator` + `High Income Household Rates` + `High Poverty Rate` + `Low Poverty Rate` + `Homeownership Rates` +
#             `Vacancy Rates` + `White Race` + `Black Race` + `American Indian Race` +
#            `Asian Race` + `Native Hawaiian Race` + `Other/Unspecified Race` + `Highest Education Level: None` +
#           `Highest Education Level: High School` + `Highest Education Level: GED/High School` + `Highest Education Level: Some College` + `Highest Education Level: Associate or Bachelor` +
#          `Highest Education Level: Bachelor` + `Highest Education Level: Master/Professional/Doctorate` + 
#         `Urban Indicator`:`Homeownership Rates` + `Urban Indicator`:`Highest Education Level: Bachelor` + `Urban Indicator`:`White Race` +
#               as.factor(COUNTY), 
#             data = FullData, 
#             family = binomial)

#summary(model4)
#nobs(model4)




##Accident Models with all variables without interaction (inc dataset)

model_prevacc <- glm(Prev_Acc ~ `Urban Indicator` + `High Income Household Rates` + `High Poverty Rate` + `Low Poverty Rate` + `Homeownership Rates` + 
                       `Vacancy Rates` + `White Race` + `Black Race` + `Asian Race` + `American Indian Race` + `Native Hawaiian Race` + `Other/Unspecified Race` + `Highest Education Level: None` + `Highest Education Level: High School` +
                       `Highest Education Level: GED/High School` + `Highest Education Level: Some College` + `Highest Education Level: Associate or Bachelor` + `Highest Education Level: Bachelor` + `Highest Education Level: Master/Professional/Doctorate` + as.factor(COUNTY) + `Population Density`,
                     data = FullDataAccInc, family = binomial)

summary(model_prevacc)
nobs(model_prevacc)



model_prevacc1 <- glm(Prev_Acc ~ `Urban Indicator` + `High Poverty Rate`  + `Homeownership Rates` + 
                        `White Race` + `Black Race` + `American Indian Race` + `Highest Education Level: None` + `Highest Education Level: High School` +
                        `Highest Education Level: Bachelor` + `Highest Education Level: Master/Professional/Doctorate` + as.factor(COUNTY) + `Population Density`,
                      data = FullDataAccInc, family = binomial)

summary(model_prevacc1)
nobs(model_prevacc1)


model_acctot <- lm(AccTotal ~ `Urban Indicator` + `High Income Household Rates` + `High Poverty Rate` + `Low Poverty Rate` + `Homeownership Rates` + 
                     `Vacancy Rates` + `White Race` + `Black Race` + `Asian Race` + `American Indian Race` + `Native Hawaiian Race` + `Other/Unspecified Race` + 
                     `Highest Education Level: None` + `Highest Education Level: High School` + `Highest Education Level: GED/High School` + `Highest Education Level: Some College` + `Highest Education Level: Associate or Bachelor` + `Highest Education Level: Bachelor` + `Highest Education Level: Master/Professional/Doctorate` + 
                     as.factor(COUNTY) + `Population Density`,
                   data = FullDataAccInc)

summary(model_acctot)
nobs(model_acctot)



model_dayssince <- lm(DaysSinceAcc ~ `Urban Indicator` + `High Income Household Rates` + `High Poverty Rate` + `Low Poverty Rate` + `Homeownership Rates` + 
                        `Vacancy Rates` + `White Race` + `Black Race` + `Asian Race` + `American Indian Race` + `Native Hawaiian Race` + `Other/Unspecified Race` + 
                        `Highest Education Level: None` + `Highest Education Level: High School` + `Highest Education Level: GED/High School` + `Highest Education Level: Some College` + `Highest Education Level: Associate or Bachelor` + `Highest Education Level: Bachelor` + `Highest Education Level: Master/Professional/Doctorate` + 
                        as.factor(COUNTY) + `Population Density`,
                      data = FullDataExistInc)

summary(model_dayssince)
nobs(model_dayssince)



#Step 5.1 log regressions with inc dataset

##Strube's variables
incmodel1 <- glm(Trac_Bin ~ `Urban Indicator` + `High Poverty Rate` + `Black Race`  +
                   `White Race` + `Other/Unspecified Race` + `Highest Education Level: None` + `Highest Education Level: High School` +
                   `Highest Education Level: GED/High School` + `Highest Education Level: Some College` + `Highest Education Level: Bachelor` + `Highest Education Level: Master/Professional/Doctorate` + as.factor(COUNTY) + `Population Density`,
                 data = FullDataExistInc, family = binomial)

summary(incmodel1)
nobs(incmodel1)


##Model with all variables without interaction
####USED IN PAPER

incmodel2 <- glm(Trac_Bin ~ `Urban Indicator` + `High Income Household Rates` + `Low Poverty Rate` + `Homeownership Rates` + 
                   `Vacancy Rates` + `White Race` + `Black Race` + `Asian Race` + `American Indian Race` +
                   `Highest Education Level: GED/High School` + `Highest Education Level: Some College` + `Highest Education Level: Associate or Bachelor` + `Highest Education Level: Master/Professional/Doctorate` + `Population Density` + as.factor(COUNTY),
                 data = FullDataExistInc, family = binomial)


summary(incmodel2)
nobs(incmodel2)



###MARGINAL EFFECT

# Rename variables in the dataset
FullDataExistInc_ME <- FullDataExistInc
colnames(FullDataExistInc_ME) <- gsub("[:/ ]", "_", colnames(FullDataExistInc))


FullDataExistInc_ME$COUNTY <- as.factor(FullDataExistInc_ME$COUNTY)

# Refit the model with updated variable names
incmodel2_ME <- glm(Trac_Bin ~ Urban_Indicator + High_Income_Household_Rates + 
                      Low_Poverty_Rate + Homeownership_Rates + Vacancy_Rates +
                      White_Race + Black_Race + Asian_Race + American_Indian_Race +
                      Highest_Education_Level__GED_High_School + 
                      Highest_Education_Level__Some_College + 
                      Highest_Education_Level__Associate_or_Bachelor + 
                      Highest_Education_Level__Master_Professional_Doctorate + 
                      Population_Density + COUNTY,
                    data = FullDataExistInc_ME, family = binomial)



##MARGINAL EFFECT

existavemargeffect <- margins(incmodel2_ME, variables = c("Urban_Indicator", "High_Income_Household_Rates", 
                                                          "Low_Poverty_Rate", "Homeownership_Rates", "Vacancy_Rates", "White_Race", "Black_Race", 
                                                          "Asian_Race", "American_Indian_Race",                                                      
                                                          "Highest_Education_Level__GED_High_School", 
                                                          "Highest_Education_Level__Some_College", 
                                                          "Highest_Education_Level__Associate_or_Bachelor", 
                                                          "Highest_Education_Level__Master_Professional_Doctorate", 
                                                          "Population_Density"))
summary(existavemargeffect)




#### Compute STANDARD DEVIATION

# Convert the marginal effects to a data frame
exist_marginal_effects_df <- as.data.frame(existavemargeffect)

# Keep only numeric columns
exist_numeric_marginal_effects <- exist_marginal_effects_df[sapply(exist_marginal_effects_df, is.numeric)]

# Select the specific variables you're interested in
exist_selected_vars <- c("dydx_High_Income_Household_Rates", "dydx_Urban_Indicator", 
                         "dydx_Population_Density", "dydx_Homeownership_Rates")

# Calculate the standard deviation for these selected variables
exist_std_dev_selected <- sapply(exist_numeric_marginal_effects[exist_selected_vars], sd, na.rm = TRUE)

# Print standard deviations of the selected marginal effects
print(exist_std_dev_selected)









incmodel2a <- glm(Trac_Bin ~ `Urban Indicator` + `High Income Household Rates` + `Low Poverty Rate` + `Homeownership Rates` + 
                    `Vacancy Rates` + `White Race` + `Black Race` + `Asian Race` + `American Indian Race` +
                    `Highest Education Level: GED/High School` + `Highest Education Level: Some College` + `Highest Education Level: Associate or Bachelor` + `Highest Education Level: Master/Professional/Doctorate` + `Population Density` + `Population Density`:`Highest Education Level: GED/High School`+ as.factor(COUNTY),
                  data = FullDataExistInc, family = binomial)


summary(incmodel2a)
nobs(incmodel2a)




##Model with all variables and interaction term with `Urban Indicator` and house ownership
incmodel3 <- glm(Trac_Bin ~ `Urban Indicator` + `High Income Household Rates` + `Low Poverty Rate` + `Homeownership Rates` + 
                   `Vacancy Rates` + `White Race` + `Black Race` + `Asian Race` + `American Indian Race` +
                   `Highest Education Level: GED/High School` + `Highest Education Level: Some College` + `Highest Education Level: Associate or Bachelor` + `Highest Education Level: Master/Professional/Doctorate` + `Population Density` + `Population Density`:`Homeownership Rates` + as.factor(COUNTY),
                 data = FullDataExistInc, family = binomial)

summary(incmodel3)
nobs(incmodel3)




###PLOTS:


# Make predictions
FullDataExistInc$predictions <- predict(incmodel2, type = "response")


# Create scatterplots for each predictor against the predicted probabilities
ggplot(FullDataExistInc, aes(x = `Urban Indicator`, y = predictions)) +
  geom_point() +
  geom_smooth(method = "glm") +
  labs(x = "`Urban Indicator` Indicator", y = "Predicted Probability of a Pipline") +
  theme_minimal()

ggplot(FullDataExistInc, aes(x = `High Income Household Rates`, y = predictions)) +
  geom_point() +
  geom_smooth(method = "glm") +
  labs(x = "Share Households with Income >= $200K (%)", y = "Predicted Probability of a Pipline") +
  #ggtitle("Relationship between `High Income Household Rates` and Predicted Probability") +
  theme_minimal()


ggplot(FullDataExistInc, aes(x = `Population Density`, y = predictions)) +
  geom_point() +
  geom_smooth(method = "glm") +
  labs(x = "Population Density (people per square mile)", y = "Predicted Probability of a Pipline") +
  #ggtitle("Relationship between `Low Poverty Rate` and Predicted Probability") +
  theme_minimal() +
  ylim(0, NA) + xlim(0, 10000)

ggplot(FullDataExistInc, aes(x = `Homeownership Rates`, y = predictions)) +
  geom_point() +
  geom_smooth(method = "glm") +
  labs(x = "Share of Occupied Housing that is Owned (%)", y = "Predicted Probability of a Pipline") +
  #ggtitle("Relationship between `Homeownership Rates` and Predicted Probability") +
  theme_minimal()


##Different format: 

ggplot(FullDataExistInc, aes(x = `Urban Indicator`, y = predictions)) +
  geom_jitter(height = 0.05, alpha=0.1) +
  geom_smooth(method = "glm", method.arg = list(family="binomial")) +
  labs(x = "`Urban Indicator` Indicator", y = "Predicted Probability of a Pipline") +
  #ggtitle("Relationship between `Homeownership Rates` and Predicted Probability") +
  theme_minimal() + 
  ylim(0, 1) + xlim(0, 1)


ggplot(FullDataExistInc, aes(x = `High Income Household Rates`, y = predictions)) +
  geom_jitter(height = 0.05, alpha=0.1) +
  geom_smooth(method = "glm", method.arg = list(family="binomial")) +
  labs(x = "Share Households with Income >= $200K", y = "Predicted Probability of a Pipline") +
  #ggtitle("Relationship between `Homeownership Rates` and Predicted Probability") +
  theme_minimal() + 
  ylim(0, 1) 


ggplot(FullDataExistInc, aes(x = `Population Density`, y = predictions)) +
  geom_jitter(height = 0.05, alpha=0.1) +
  geom_smooth(method = "glm", method.arg = list(family="binomial")) +
  labs(x = "Population Density (people per square mile)", y = "Predicted Probability of a Pipline") +
  #ggtitle("Relationship between `Homeownership Rates` and Predicted Probability") +
  theme_minimal() + 
  ylim(0, 1) 


ggplot(FullDataExistInc, aes(x = `Homeownership Rates`, y = predictions)) +
  geom_jitter(height = 0.05, alpha=0.1) +
  geom_smooth(method = "glm", method.arg = list(family="binomial")) +
  labs(x = "Share of Occupied Housing that is Owned", y = "Predicted Probability of a Pipline") +
  #ggtitle("Relationship between `Homeownership Rates` and Predicted Probability") +
  theme_minimal() + 
  ylim(0, 1) 


##First format with slope:
`Exist Urban Indicator` <- ggplot(FullDataExistInc, aes(x = `Urban Indicator`, y = predictions)) +
  geom_point() +
  geom_smooth(method = "glm", method.arg = list(family="binomial")) +
  labs(x = "`Urban Indicator` Indicator", y = "Predicted Probability of a Pipeline") +
  theme_minimal()

# Calculate the slope of the line
slope <- coef(summary(glm(predictions ~ `Urban Indicator`, data = FullDataExistInc)))[2,1]

# Add the slope to the plot
`Exist Urban Indicator` + geom_text(x = max(FullDataExistInc$`Urban Indicator`), y = max(FullDataExistInc$predictions), 
                                    label = paste("Slope:", round(slope, 2)), hjust = 1, vjust = 1, size = 4, color = "red")




`Exist High Income Household Rates` <- ggplot(FullDataExistInc, aes(x = `High Income Household Rates`, y = predictions)) +
  geom_point() +
  geom_smooth(method = "glm", method.arg = list(family="binomial")) +
  labs(x = "`High Income Household Rates`", y = "Predicted Probability of a Pipeline") +
  theme_minimal()

# Calculate the slope of the line
slope <- coef(summary(glm(predictions ~ `High Income Household Rates`, data = FullDataExistInc)))[2,1]

# Add the slope to the plot
`Exist High Income Household Rates` + geom_text(x = max(FullDataExistInc$`High Income Household Rates`), y = max(FullDataExistInc$predictions), 
                                                label = paste("Slope:", round(slope, 2)), hjust = 1, vjust = 1, size = 4, color = "red")





###Coefficient plot of the logistic regression model.


#Scale around zero
existcoef_table <- summary(incmodel2)$coefficients
existcoef_df <- data.frame(
  variable = rownames(existcoef_table),
  coefficient = existcoef_table[, 1],
  std_error = existcoef_table[, 2]
)

# Filter out the county fixed effects
existcoef_df <- existcoef_df[!grepl("COUNTY", existcoef_df$variable), ]

# Determine the x-axis limits based on the coefficients and standard errors
x_min <- min(existcoef_df$coefficient - 2 * existcoef_df$std_error)
x_max <- max(existcoef_df$coefficient + 2 * existcoef_df$std_error)
x_range <- abs(x_max - x_min)
x_min <- x_min - 0.2 * x_range
x_max <- x_max + 0.2 * x_range

# Create the coefficient plot with 95% confidence intervals
ggplot(existcoef_df, aes(x = coefficient, y = reorder(variable, coefficient))) +
  geom_point() +
  geom_errorbarh(aes(xmin = coefficient - 1.96 * std_error, xmax = coefficient + 1.96 * std_error), height = 0.2) +
  labs(x = "Coefficient", y = "Variable") +
  scale_x_continuous(limits = c(x_min, x_max)) +
  theme_minimal()


# Filter out variables other than `Urban Indicator`, `Population Density`, `Homeownership Rates`, and `High Income Household Rates`
existcoef_df <- existcoef_df[existcoef_df$variable %in% c("`Urban Indicator`", "`Population Density`", "`Homeownership Rates`", "`High Income Household Rates`"), ]

# Determine the x-axis limits based on the coefficients and standard errors
x_min <- min(existcoef_df$coefficient - 2 * existcoef_df$std_error)
x_max <- max(existcoef_df$coefficient + 2 * existcoef_df$std_error)
x_range <- abs(x_max - x_min)
x_min <- x_min - 0.2 * x_range
x_max <- x_max + 0.2 * x_range

# Create the coefficient plot with 95% confidence intervals
ggplot(existcoef_df, aes(x = coefficient, y = reorder(variable, coefficient))) +
  geom_point() +
  geom_errorbarh(aes(xmin = coefficient - 1.96 * std_error, xmax = coefficient + 1.96 * std_error), height = 0.2) +
  labs(x = "Coefficient", y = "Variable") +
  scale_x_continuous(limits = c(x_min, x_max)) +
  theme_minimal()


existcoef_df$variable <- stringr::str_wrap(existcoef_df$variable, width = 13)


# Now plot
ggplot(existcoef_df, aes(x = coefficient, y = reorder(variable, coefficient))) +
  geom_point(size = 1.8, color="chartreuse4") +
  geom_errorbarh(aes(xmin = coefficient - 1.96 * std_error, xmax = coefficient + 1.96 * std_error), height = 0.2, size = .75, color="chartreuse4") +
  labs(x = "Coefficient", y = "Variable") +
  scale_x_continuous(limits = c(x_min, x_max)) +
  theme_minimal() +
  theme(
    panel.border = element_rect(fill = NA, color = "gray", size = 1),
    panel.grid.major = element_line(color = "lightgray", size = 0.25),
    panel.grid.minor = element_line(color = "lightgray", size = 0.25),
    #panel.grid.major.x = element_blank(), # Remove vertical major gridlines
    panel.grid.minor.x = element_blank(), # Remove vertical minor gridlines
    axis.ticks.length = unit(-0.25, "cm") # Adjust the length of axis ticks
  ) +
  scale_y_discrete(labels = function(labels) str_replace_all(labels, "`", ""))





###TRY to do coefficient plot with MARGINAL EFFECTS

# Extract marginal effects as a dataframe
existmarg_df <- summary(existavemargeffect)

# Rename columns for consistency
existmarg_df <- data.frame(
  variable = existmarg_df$factor,  # Variable names
  coefficient = existmarg_df$AME*100,  # Average Marginal Effect
  std_error = existmarg_df$SE*100 # Standard Error
)


existmarg_df$variable <- str_replace_all(existmarg_df$variable, "_", " ")


# Filter for the selected variables
selected_vars <- c("Urban Indicator", "Population Density", "Homeownership Rates", "High Income Household Rates")
existmarg_df <- existmarg_df[existmarg_df$variable %in% selected_vars, ]

# Determine the x-axis limits based on marginal effects and standard errors
x_min <- min(existmarg_df$coefficient - 2 * existmarg_df$std_error)
x_max <- max(existmarg_df$coefficient + 2 * existmarg_df$std_error)
x_range <- abs(x_max - x_min)
x_min <- x_min - 0.2 * x_range
x_max <- x_max + 0.2 * x_range

existmarg_df$variable <- stringr::str_wrap(existmarg_df$variable, width = 13)
# Create the filtered marginal effects plot
ggplot(existmarg_df, aes(x = coefficient, y = reorder(variable, coefficient))) +
  geom_point(size = 1.8, color = "chartreuse4") +
  geom_errorbarh(aes(xmin = coefficient - 1.96 * std_error, xmax = coefficient + 1.96 * std_error), 
                 height = 0.2, size = 0.75, color = "chartreuse4") +
  labs(x = "Marginal Effect", y = "Variable") +
  scale_x_continuous(limits = c(x_min, x_max)) +
  theme_minimal() +
  theme(
    panel.border = element_rect(fill = NA, color = "gray", size = 1),
    panel.grid.major = element_line(color = "lightgray", size = 0.25),
    panel.grid.minor = element_line(color = "lightgray", size = 0.25),
    panel.grid.minor.x = element_blank(), 
    axis.ticks.length = unit(-0.25, "cm"),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(margin = margin(t = 14), size=14),
    axis.title.y = element_text(size=14),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  scale_y_discrete(labels = function(labels) str_replace_all(labels, "`", ""))


#save high resolution
ggsave("C:/Users/julia/OneDrive - University of Virginia/PhD Stuff/Research/Pipeline/Paper Revisions/Final Submissions/exist_marginal_effects_plot.png", 
       plot = last_plot(), 
       width = 9, height = 6, units = "in", 
       dpi = 300)


# Specify the file path where you want to save the Excel file
file_path <- "C:/Users/julia/OneDrive - University of Virginia/PhD Stuff/Research/Pipeline/FullDataExistInc.xlsx"

# Export the dataset to an Excel file
write.xlsx(FullDataExistInc, file_path, rowNames = TRUE)

# Provide a message indicating the export is complete
cat("Dataset exported to", file_path, "\n")







##cluster standard errors at the county level to account for the correlation of errors among tracts in the same county.




##will need to do same data cleaning


##Accidents dataset: each row is an accident, and it has the AFFGEOID value for each accident
###will need to append these to the big proposed pipeline datasheet... something like a code where
### it makes a new column in the big proposed datasheet and then it looks to see if the AFFGEOID 
### matches between big proposed datasheet and the accidents datasheet and if there is a match, set 
### the new column value equal to one for that AFFGEOID, and if not, set zero (for the question of
### if there is a correlation between if there's an accident and if there's a proposed pipeline)

##Border dataset: each row is a census tract, can append it to the proposed datatsheet the same way 
### as exisiting datasheet earlier

##correlation between if there is an accident in a census tract and if there is a proposed pipeline
##correlation between the number of accidents in a census tract and if there is a proposed pipeline
##correlation between distance from an accident and if there is a proposed pipeline
##correlation between number of days since an accident in a census tract and if there is a proposed pipeline


##For proposed pipelines analysis:
###Make a column (called Prev_Acc) that looks at the accidents list and if there are any accidents
### for a given census tract, it gives the value of 1 for Prev_Acc, and if there are no accidents,
### it gives the value zero.

###Make a column (called Num_Acc) that looks at the accidents list and counts the number of accidents
### for each given census tract. It returns the number of accidents for each census tract, and if there
### are no census tracts/accidents (for the missing census tracts), it gives the value zero.

###


###Create a new dataset that mimics the original pipeline accident dataset. Use the column 
### called DaysSinceAcc that counts the number of years it has been (from December 31, 2023)
### since each accident took place



##PROPOSED PIPELINES

#Step 1: Initial cleaning. Start by removing no area pipeline data and no population demographic data


# Using dataset Demographic
# Change the demographic dataset to have no rows where AON5E001 (total population) is equal to 0
Demographic <- Demographic[Demographic $AON5E001 != 0, ]

# went from 85395 to 84524



# Using dataset Proposed
# Change the pipeline dataset to have no rows where ALAND (land area) is equal to 0
Proposed <- Proposed[Proposed$ALAND != 0, ]

# went from 85185 to 85159 observations.





#Step 2: Merging X2021_demographic and Proposed


#This makes a new dataset (FullDataProposedDemogPipe) that joins the demographic and pipeline 
# data for all rows in which the GEO_ID value (from demographic data) matches the AFFGEOID 
# value (from pipeline data). Only GEO_ID variable will be shown now.
FullDataProposedDemogPipe <- inner_join(Demographic, Proposed, by = c("GEO_ID" = "AFFGEOID"))
#View(FullDataProposedDemogPipe)



#This makes a new dataset (ExtraDemog) that holds the rows/observations in the demographic
# dataset that did not match to any rows/observations in the pipeline dataset.
ExtraPropDemog<- anti_join(Demographic, Proposed, by = c("GEO_ID" = "AFFGEOID"))
ExtraPropDemog$Source <- "Demographic"
# ExtraDemog has stuff that was in the demographic data but not pipeline data


#This makes a new dataset (ExtraPipe) that holds the rows/observations in the pipeline
# dataset that did not match to any rows/observations in the demographic dataset.
ExtraPropPipe<- anti_join(Proposed, Demographic, by = c("AFFGEOID" = "GEO_ID"))
ExtraPropPipe$Source <- "Pipeline"
# ExtraPipe has stuff that was in the pipeline data but not deographic data

#View(ExtraPropDemog)
#View(ExtraPropPipe)




#Step 3: Create any new variables necessary to add in later regressions:

#Population Density
# Now create a new column for the variable called `Population Density`, which represents the population 
# density for each row (by dividing the total population by the area for each census tract). This
# may be useful for our regressions later on.

#First change the land area from square meters to square miles
FullDataProposedDemogPipe$ALAND <- FullDataProposedDemogPipe$ALAND / 2.59e+6


FullDataProposedDemogPipe$`Population Density` <- FullDataProposedDemogPipe$AON5E001 / FullDataProposedDemogPipe$ALAND

#Rural/`Urban Indicator`
#Create another column that indicates if a tract is rural (equal to zero) or `Urban Indicator` (equal to 1).
# A tract will be considered rural if the population density is less than or equal to 1000 people
# per square mile, and `Urban Indicator` if the population density is greater than 1000 people/sq mile.

FullDataProposedDemogPipe$`Urban Indicator` <- ifelse(FullDataProposedDemogPipe$`Population Density` > 1000, 1, 0)

#Here I'll need to add the 4 type variable with the metropolitan variable (need to figure that out)



#Income and Poverty
#Create a new column, called IncTot, which represents the total number of observations within
# each household income bracket for each census tract.

FullDataProposedDemogPipe$IncTot <- rowSums(FullDataProposedDemogPipe[, c("AOQHE002", "AOQHE003", "AOQHE004", "AOQHE005", "AOQHE006", "AOQHE007", "AOQHE008", "AOQHE009", "AOQHE010", "AOQHE011", "AOQHE012", "AOQHE013", "AOQHE014", "AOQHE015", "AOQHE016", "AOQHE017")])
#this wasn't necessary to do

#Create another column, called `High Income Household Rates`, which represents the percentage of households within each
# census tract that make at least $200K per year

FullDataProposedDemogPipe$`High Income Household Rates` <- (FullDataProposedDemogPipe$AOQHE017 / FullDataProposedDemogPipe$IncTot) * 100


#Create a new column, called PovTot, which represents the total number of observations within
# each poverty ratio bracket for each census tract.
FullDataProposedDemogPipe$PovTot <- rowSums(FullDataProposedDemogPipe[, c("AOXWE002", "AOXWE003", "AOXWE004", "AOXWE005", "AOXWE006", "AOXWE007", "AOXWE008")])
#this wasn't necessary to do

#Create another column, called `Low Poverty Rate`, which represents the percentage of households within each
# census tract that have at a high ratio of income to poverty level (2 or greater)
FullDataProposedDemogPipe$`Low Poverty Rate` <- (FullDataProposedDemogPipe$AOXWE008 / FullDataProposedDemogPipe$PovTot) * 100

#Create another column, called `High Poverty Rate`, which represents the percentage of households within each
# census tract that have at a low ratio of income to poverty level (under 0.5)
FullDataProposedDemogPipe$`High Poverty Rate` <- (FullDataProposedDemogPipe$AOXWE002 / FullDataProposedDemogPipe$PovTot) * 100



#Housing Occupation Rates
#Create a new column, called HouseTot, which represents the total number of observations within
# each housing occupation bracket for each census tract (both renters and owners).
FullDataProposedDemogPipe$HouseTot <- rowSums(FullDataProposedDemogPipe[, c("AOSPE002", "AOSPE003")])
#this wasn't necessary to do

#Create another column, called `Homeownership Rates`, which represents the percentage of households within each
# census tract where housing units are owned (as opposed to being rented).
FullDataProposedDemogPipe$`Homeownership Rates` <- (FullDataProposedDemogPipe$AOSPE002 / FullDataProposedDemogPipe$HouseTot) * 100

#Create another column, called HouseRatio_renter, which represents the percentage of households within each
# census tract where housing units are owned (as opposed to being rented).
FullDataProposedDemogPipe$HouseRatio_renter <- (FullDataProposedDemogPipe$AOSPE003 / FullDataProposedDemogPipe$HouseTot) * 100


#Create a new column, called VacantTot, which represents the total number of observations within
# each housing occupation bracket for each census tract (both vacant and occupied).
FullDataProposedDemogPipe$VacantTot <- rowSums(FullDataProposedDemogPipe[, c("AOSOE002", "AOSOE003")])
#this wasn't necessary to do

#Create another column, called VacantRatio_occ, which represents the percentage of households within each
# census tract where housing units are occupied (as opposed to being vacant).
FullDataProposedDemogPipe$VacantRatio_occ <- (FullDataProposedDemogPipe$AOSOE002 / FullDataProposedDemogPipe$VacantTot) * 100

#Create another column, called `Vacancy Rates`, which represents the percentage of households within each
# census tract where housing units are occupied (as opposed to being vacant).
FullDataProposedDemogPipe$`Vacancy Rates` <- (FullDataProposedDemogPipe$AOSOE003 / FullDataProposedDemogPipe$VacantTot) * 100



#Race
#Create a new column, called RaceTot, which represents the total number of observations within
# each race bracket for each census tract (should be the same as total population for each census tract)
FullDataProposedDemogPipe$RaceTot <- rowSums(FullDataProposedDemogPipe[, c("AON5E002", "AON5E002", "AON5E003", "AON5E004", "AON5E005", "AON5E006", "AON5E007", "AON5E008")])
#Did not include AON5E009 and AON5E010 because they are subsets of AON5E008.

#Create another column, called `White Race`, which represents the percentage of people within each
# census tract that are `White Race` alone
FullDataProposedDemogPipe$`White Race` <- (FullDataProposedDemogPipe$AON5E002 / FullDataProposedDemogPipe$RaceTot) * 100

#Create another column, called `Black Race`, which represents the percentage of people within each
# census tract that are `Black Race` or African American alone
FullDataProposedDemogPipe$`Black Race` <- (FullDataProposedDemogPipe$AON5E003 / FullDataProposedDemogPipe$RaceTot) * 100

#Create another column, called `American Indian Race`, which represents the percentage of people within each
# census tract that are American Indian and Alaska Native alone
FullDataProposedDemogPipe$`American Indian Race` <- (FullDataProposedDemogPipe$AON5E004 / FullDataProposedDemogPipe$RaceTot) * 100

#Create another column, called `Asian Race`, which represents the percentage of people within each
# census tract that are `Asian Race` alone
FullDataProposedDemogPipe$`Asian Race` <- (FullDataProposedDemogPipe$AON5E005 / FullDataProposedDemogPipe$RaceTot) * 100

#Create another column, called `Native Hawaiian Race`, which represents the percentage of people within each
# census tract that are Native Hawaiian and Other Pacific Islander alone
FullDataProposedDemogPipe$`Native Hawaiian Race` <- (FullDataProposedDemogPipe$AON5E006 / FullDataProposedDemogPipe$RaceTot) * 100

#Create another column, called `Native Hawaiian Race`, which represents the percentage of people within each
# census tract that are Native Hawaiian and Other Pacific Islander alone
FullDataProposedDemogPipe$`Native Hawaiian Race` <- (FullDataProposedDemogPipe$AON5E006 / FullDataProposedDemogPipe$RaceTot) * 100

#Create another column, called `Other/Unspecified Race`, which represents the percentage of people within each
# census tract that are some other race alone
FullDataProposedDemogPipe$`Other/Unspecified Race` <- (FullDataProposedDemogPipe$AON5E007 / FullDataProposedDemogPipe$RaceTot) * 100

#Create another column, called TwoPlus_Race, which represents the percentage of people within each
# census tract that are two or more races
FullDataProposedDemogPipe$TwoPlus_Race <- (FullDataProposedDemogPipe$AON5E008 / FullDataProposedDemogPipe$RaceTot) * 100



#Educational Attainment
#Create a new column, called EduTot, which represents the total number of observations within
# each education bracket for each census tract (should be the same as total population for each census tract)
FullDataProposedDemogPipe$EduTot <- rowSums(FullDataProposedDemogPipe[, c("AOP8E002", "AOP8E003", "AOP8E004", "AOP8E005", "AOP8E006", "AOP8E007", "AOP8E008", "AOP8E009", "AOP8E010", "AOP8E011", "AOP8E012", "AOP8E013", "AOP8E014", "AOP8E015", "AOP8E016", "AOP8E017", "AOP8E018", "AOP8E019", "AOP8E020", "AOP8E021", "AOP8E022", "AOP8E023", "AOP8E024", "AOP8E025")])

#Create another column, called `Highest Education Level: None`, which represents the percentage of people within each
# census tract that have no completed education
FullDataProposedDemogPipe$`Highest Education Level: None` <- (FullDataProposedDemogPipe$AOP8E002 / FullDataProposedDemogPipe$EduTot) * 100

#Create another column, called `Highest Education Level: High School`, which represents the percentage of people within each
# census tract that have completed regular high school as their highest education level
FullDataProposedDemogPipe$`Highest Education Level: High School` <- (FullDataProposedDemogPipe$AOP8E017 / FullDataProposedDemogPipe$EduTot) * 100

#Create another column, called `Highest Education Level: GED/High School`, which represents the percentage of people within each
# census tract that have completed high school or has a GED/alternative credential as their highest education level
FullDataProposedDemogPipe$`Highest Education Level: GED/High School` <- ((FullDataProposedDemogPipe$AOP8E017 + FullDataProposedDemogPipe$AOP8E018) / FullDataProposedDemogPipe$EduTot) * 100

#Create another column, called `Highest Education Level: Some College`, which represents the percentage of people within each
# census tract that have some college education as their highest education level
FullDataProposedDemogPipe$`Highest Education Level: Some College` <- ((FullDataProposedDemogPipe$AOP8E019 + FullDataProposedDemogPipe$AOP8E020) / FullDataProposedDemogPipe$EduTot) * 100

#Create another column, called `Highest Education Level: Associate or Bachelor`, which represents the percentage of people within each
# census tract that have an Associate's or `Highest Education Level: Bachelor`'s degree their highest education level
FullDataProposedDemogPipe$`Highest Education Level: Associate or Bachelor` <- ((FullDataProposedDemogPipe$AOP8E021 + FullDataProposedDemogPipe$AOP8E022) / FullDataProposedDemogPipe$EduTot) * 100

#Create another column, called `Highest Education Level: Bachelor`, which represents the percentage of people within each
# census tract that have a `Highest Education Level: Bachelor`'s degree their highest education level
FullDataProposedDemogPipe$`Highest Education Level: Bachelor` <- (FullDataProposedDemogPipe$AOP8E022 / FullDataProposedDemogPipe$EduTot) * 100

#Create another column, called `Highest Education Level: Master/Professional/Doctorate`, which represents the percentage of people within each
# census tract that have a Master's, professional school, or Doctorate degree their highest education level
FullDataProposedDemogPipe$`Highest Education Level: Master/Professional/Doctorate` <- ((FullDataProposedDemogPipe$AOP8E023 + FullDataProposedDemogPipe$AOP8E024 + FullDataProposedDemogPipe$AOP8E025) / FullDataProposedDemogPipe$EduTot) * 100





#Variables I have:

##Population density -- `Population Density` (measured as people per square mile)
###As population density increases by one person per square mile, Y variable changes by

##`Urban Indicator` -- `Urban Indicator` (measured as 1 if the population density is greater than 1000 people/sq mile)
###Compared to rural areas, `Urban Indicator` areas have an associated change in the Y variable by

##High income -- `High Income Household Rates` (measured as % of households who have an income of at least $200k)
###As the share of high-income households increases by one percentage point, Y variable changes by

##High poverty -- `High Poverty Rate` (measured as % of population who has an income to poverty level ratio of under 0.5)
###As the share of poor people increases by one percentage point, Y variable changes by

##Low poverty -- `Low Poverty Rate` (measured as % of population who has an income to poverty level ratio of at least 2)
###As the share of non-poor people increases by one percentage point, Y variable changes by

##Homeowners -- `Homeownership Rates` (measured as % of occupied housing units that are owned)
###Within occupied housing units, as the share of homeowners increases by one percentage point, Y variable changes by

##Renters -- HouseRatio_renter (measured as % of occupied housing units that are rented)
###Within occupied housing units, as the share of renters increases by one percentage point, Y variable changes by
####Repetitive, will probably use homeowners

##`Vacancy Rates` -- `Vacancy Rates` (measured as % of housing units that are vacant)
###As the share of vacant housing units increases by one percentage point, Y variable changes by

##Occupancy -- VacantRatio_occ (measured as % of housing units that are occupied)
###As the share of occupied housing units increases by one percentage point, Y variable changes by

##`White Race` -- `White Race` (measured as % of people that are `White Race`)
###As the share of people who are `White Race` alone increases by one percentage point, Y variable changes by

##`Black Race` -- `Black Race` (measured as % of people that are `Black Race`)
###As the share of people who are `Black Race` alone increases by one percentage point, Y variable changes by

##American Indian/Native -- `American Indian Race` (measured as % of people that are American Indian)
###As the share of people who are American Indian alone increases by one percentage point, Y variable changes by

##`Asian Race` -- `Asian Race` (measured as % of people that are `Asian Race`)
###As the share of people who are `Asian Race` alone increases by one percentage point, Y variable changes by

##Native Hawaiian/Pacific Islander -- `Native Hawaiian Race` (measured as % of people that are Native Hawaiian)
###As the share of people who are Native Hawaiian alone increases by one percentage point, Y variable changes by

##Other Race -- `Other/Unspecified Race` (measured as % of people that are another race)
###As the share of people who are another race alone increases by one percentage point, Y variable changes by

##Two or more races -- TwoPlus_Race (measured as % of people that are two or more races)
###As the share of people who are two or more races increases by one percentage point, Y variable changes by

##No education -- `Highest Education Level: None` (measured as % of people that have no completed schooling)
###As the share of people who have no completed schooling increases by one percentage point, Y variable changes by

##High school education -- `Highest Education Level: High School` (measured as % of people that have completed regular high school as their highest education level)
###As the share of people who have completed regular high school as their highest education level increases by one percentage point, Y variable changes by

##High school/GED education -- `Highest Education Level: GED/High School` (measured as % of people that have completed regular high school, or received a GED or alternative credential, as their highest education level)
###As the share of people who have completed regular high school, or received a GED or alternative credential, as their highest education level increases by one percentage point, Y variable changes by

##Some college experience -- `Highest Education Level: Some College` (measured as % of people that have some college experience as their highest education level)
###As the share of people who have some college experience as their highest education level increases by one percentage point, Y variable changes by

##Associate/`Highest Education Level: Bachelor` degree -- `Highest Education Level: Associate or Bachelor` (measured as % of people that have an Associate's or `Highest Education Level: Bachelor`'s degree as their highest education level)
###As the share of people who have an Associate's or `Highest Education Level: Bachelor`'s degree as their highest education level increases by one percentage point, Y variable changes by

##`Highest Education Level: Bachelor` degree -- `Highest Education Level: Bachelor` (measured as % of people that have a `Highest Education Level: Bachelor`'s degree as their highest education level)
###As the share of people who have a `Highest Education Level: Bachelor`'s degree as their highest education level increases by one percentage point, Y variable changes by

##Graduate degree -- `Highest Education Level: Master/Professional/Doctorate` (measured as % of people that have a Master's, professional school, or Doctorate degree as their highest education level)
###As the share of people who have a Master's, professional school, or Doctorate degree as their highest education level increases by one percentage point, Y variable changes by



#I'll also need to add any new variables if I need


#Step 4a: remove any census tracts with missing values of any of the variables of interest

variables_to_check_prop <- c("Trac_Bin", "Population Density", "COUNTY", "Urban Indicator", "High Income Household Rates", 
                             "High Poverty Rate", "Low Poverty Rate", "Homeownership Rates", 
                             "Vacancy Rates", "White Race", "Black Race", 
                             "American Indian Race", "Asian Race", "Native Hawaiian Race", 
                             "Other/Unspecified Race", "Highest Education Level: None", "Highest Education Level: High School", 
                             "Highest Education Level: GED/High School", "Highest Education Level: Some College", "Highest Education Level: Associate or Bachelor", 
                             "Highest Education Level: Bachelor", "Highest Education Level: Master/Professional/Doctorate")

# Remove observations with missing values for specified variables
FullDataProposedDemogPipe <- FullDataProposedDemogPipe[complete.cases(FullDataProposedDemogPipe[, variables_to_check_prop]), ]



#Step 4: Now clean data so that there are only counties that have some census tracts that
# do have pipelines and some census tracts that do not (i.e. within a given county value,
# delete if all rows have a pipeline or no rows have a pipeline).



#First: Get rid of counties with no pipelines in any of the census tracts:

#Start by creating a new dataset (FullData_countyzero) of the counties and states that have their total
# pipeline greater than 0
FullDataProp_countyzero <- FullDataProposedDemogPipe%>%
  group_by(NAMELSADCO, STATE_NAME) %>%
  summarise(total_trac_bin = sum(Trac_Bin))%>%
  filter(total_trac_bin > 0)
#View(FullDataProp_countyzero)
#View(FullDataProposedDemogPipe)

#In both the FullDataDemogPipe and FullData_countyzero, create a new column (CountyState in FullDataDemogPipe
# and CountyStates in FullData_countyzero) that has both the county and state name. This is important because
# some states have counties with the same name, and I needed a way to differentiate them from each other.

FullDataProposedDemogPipe$CountyState <- paste(FullDataProposedDemogPipe$NAMELSADCO, FullDataProposedDemogPipe$STATE_NAME, sep = ", ")
#View(FullDataDemogPipe)

FullDataProp_countyzero$CountyStates <- paste(FullDataProp_countyzero$NAMELSADCO, FullDataProp_countyzero$STATE_NAME, sep = ", ")


#Create another dataset called FullData, that has the rows (census tracts) from FullDataDemogPipe if they
# have a matching value in the FullData_countyzero dataset (looking at TractState and TractStates).
FullDataProposed <- FullDataProposedDemogPipe %>%
  filter(CountyState %in% FullDataProp_countyzero$CountyStates)

#View(FullDataProposed)


#Create a duplicate dataset (called FullDataPropInc) for later regression use that includes census tracts in 
# counties where all census tracts
FullDataPropInc <- FullDataProposed


#Second: Get rid of counties with pipelines in all of the census tracts:


#This makes a new dataset called FullData_countyone, with a new variable (PipeTotal) that counts the 
# total number of observations (census tracts) for each CountyState value, within the FullData dataset.
FullDataProp_countyone <- FullDataProposed %>%
  group_by(CountyState) %>%
  summarise(PipeTotal = n())

#View(FullDataProp_countyone)

#This joins the FullData_countyzero and FullData_countyone datasets (based on CountyStates and CountyState). 
# This is useful because now I am able to see the number of pipelines in a CountyState value (total_trac_bin),
# as well as the total number of census tracts for that CountyState value (PipeTotal).
FullDataProp_county <- inner_join(FullDataProp_countyzero, FullDataProp_countyone, by = c("CountyStates" = "CountyState"))
#View(FullDataProp_county)

#This removes the CountyState values where the number of pipelines in a CountyState equals the total number 
# of census tracts for that CountyState value. I am doing this because if they equal the same amount, that
# means that every census tract within a CountyState value has a pipeline in it, and we want to remove those.
FullDataProp_county <- FullDataProp_county %>%
  filter(PipeTotal != total_trac_bin)

#Now, FullData_county only has CountyState values where not every census tract has a pipeline in it. So, I
# can keep the observations in FullData where the CountyState value is present in the CountyStates variable
# in the FullData_county dataset.
FullDataProposed <- FullDataProposed %>%
  filter(CountyState %in% FullDataProp_county$CountyStates)




#Step 4b: add remaining variables related to accidents. Just doing this for FullDataProposed dataset, so if I do
# end up using FullDataPropInc, have to do this for that dataset too.
## did it for inc too



##Accidents dataset: each row is an accident, and it has the AFFGEOID value for each accident
###will need to append these to the big proposed pipeline datasheet... something like a code where
### it makes a new column in the big proposed datasheet and then it looks to see if the AFFGEOID 
### matches between big proposed datasheet and the accidents datasheet and if there is a match, set 
### the new column value equal to one for that AFFGEOID, and if not, set zero (for the question of
### if there is a correlation between if there's an accident and if there's a proposed pipeline)

##Border dataset: each row is a census tract, can append it to the proposed datasheet the same way 
### as existing datasheet earlier

##correlation between if there is an accident in a census tract and if there is a proposed pipeline
##correlation between the number of accidents in a census tract and if there is a proposed pipeline
##correlation between distance from an accident and if there is a proposed pipeline
##correlation between number of days since an accident in a census tract and if there is a proposed pipeline






###Make a column (called Prev_Acc) that looks at the accidents list and if there are any accidents
### for a given census tract, it gives the value of 1 for Prev_Acc, and if there are no accidents,
### it gives the value zero.

#View(FullDataProposed)
#View(FullDataPropInc)
#View(Accidents)

####did this already: This creates a new datatset called Prev_Acc_Count with a new variable (AccTotal) that counts the 
# total number of observations (accidents) for each AFFGEOID value, within the Accidents dataset.
#Prev_Acc_Count_CT <- Accidents %>%
#group_by(AFFGEOID) %>%
#summarise(AccTotal = n(), LOCAL_DAYSSINCE = LOCAL_DAYSSINCE[which.min(LOCAL_DAYSSINCE)])
##View(Prev_Acc_Count_CT)

###PROBLEM: there are no accidents in the same census tract as the proposed pipelines. Expand to statewide.
Prev_Acc_Count_ST <- Accidents %>%
  group_by(STATE_NAME) %>%
  summarise(AccTotal = n(), LOCAL_DAYSSINCE = LOCAL_DAYSSINCE[which.min(LOCAL_DAYSSINCE)])
#View(Prev_Acc_Count_ST)


# Merge datasets based on STATE_NAME.
merged_data_ST <- merge(Prev_Acc_Count_ST, FullDataProposed, by.x = "STATE_NAME", by.y = "STATE_NAME", all.y = TRUE)
merged_data_ST$AccTotal <- ifelse(is.na(merged_data_ST$AccTotal), 0, merged_data_ST$AccTotal)
# Do same for inc dataset
merged_data_ST_inc <- merge(Prev_Acc_Count_ST, FullDataPropInc, by.x = "STATE_NAME", by.y = "STATE_NAME", all.y = TRUE)
merged_data_ST_inc$AccTotal <- ifelse(is.na(merged_data_ST_inc$AccTotal), 0, merged_data_ST_inc$AccTotal)

#View(merged_data_ST)
#View(merged_data_ST_inc)

#Create another column, called Prev_Acc, that indicates if a tract is has any accidents in its state 
# (equal to 1), or not (equal to 0).

merged_data_ST$Prev_Acc <- ifelse(merged_data_ST$AccTotal > 0, 1, 0)
merged_data_ST_inc$Prev_Acc <- ifelse(merged_data_ST_inc$AccTotal > 0, 1, 0)


###Make a column (called AccTotal) that looks at the accidents list and counts the number of accidents
### for each given census tract. It returns the number of accidents for each census tract, and if there
### are no census tracts/accidents (for the missing census tracts), it gives the value zero.


###PROBLEM: again, there are no accidents in the same census tract as the proposed pipelines. Expand to statewide.

##Use AccTotal variable



###Create a new column called DaysSinceAcc that counts the number of days it has been (from December 31, 2023)
### since the last accident in each state has taken place.

####BELOW: none is necessary (with relation to DaysSinceAcc). I did it in the Accidents excel file (column called LOCAL_DAYSSINCE)

####This creates a new datatset called Date_Acc with only the variables 
#### "AFFGEOID", "STATE_NAME", "LOCAL_DATETIME" from the Accidents dataset

#Date_Acc <- Accidents[, c("AFFGEOID", "STATE_NAME", "LOCAL_DATETIME", "LOCAL_DAYSSINCE")]
##View(Date_Acc)

####Next, I looked at the STATE_NAME and AccTotal columns in merged_data_ST (and merged_data_ST_inc) and saw that 
#### only Colorado, Wyoming, and North Dakota had accidents in them. Thus, only keep states: Colorado, North Dakota, Wyoming.

#Date_Acc <- Date_Acc %>%
#  filter(STATE_NAME %in% c("Colorado", "Wyoming", "North Dakota"))

###Looking at Date_Acc, I found the following:
#### In Colorado: last accident on 10/27/2021
#### 795 days from 12/31/2023
#### In North Dakota: last (only) accident on 9/25/2014
#### 3384 days from 12/31/2023
#### In Wyoming: last accident on 5/23/2022
#### 587 days from 12/31/2023

####Add these values to the dataset
#merged_data_ST <- merged_data_ST %>%
#  mutate(DaysSinceAcc = case_when(
#    STATE_NAME == "Colorado" ~ 795,
#    STATE_NAME == "North Dakota" ~ 3384,
#    STATE_NAME == "Wyoming" ~ 587,
#    TRUE ~ NA_real_  # Default value for other cases
#  ))

#merged_data_ST_inc <- merged_data_ST_inc %>%
#  mutate(DaysSinceAcc = case_when(
#    STATE_NAME == "Colorado" ~ 795,
#    STATE_NAME == "North Dakota" ~ 3384,
#    STATE_NAME == "Wyoming" ~ 587,
#    TRUE ~ NA_real_  # Default value for other cases
#  ))

merged_data_ST$DaysSinceAcc <- merged_data_ST$LOCAL_DAYSSINCE
merged_data_ST_inc$DaysSinceAcc <- merged_data_ST_inc$LOCAL_DAYSSINCE

FullDataProposed <- merged_data_ST
FullDataPropInc <- merged_data_ST_inc




#Step 5a: descriptive statistics

##Whole US
##Census Tracts in (Proposed) Restricted Sample
mean(FullDataProposedDemogPipe$AON5E001,na.rm = TRUE) ##Population
mean(FullDataProposedDemogPipe$`Population Density`, na.rm = TRUE)
mean(FullDataProposedDemogPipe$`Urban Indicator`, na.rm = TRUE)
mean(FullDataProposedDemogPipe$`High Income Household Rates`, na.rm = TRUE)
mean(FullDataProposedDemogPipe$`High Poverty Rate`, na.rm = TRUE)
mean(FullDataProposedDemogPipe$`Low Poverty Rate`, na.rm = TRUE)
mean(FullDataProposedDemogPipe$`Homeownership Rates`, na.rm = TRUE)
mean(FullDataProposedDemogPipe$`Vacancy Rates`, na.rm = TRUE)
mean(FullDataProposedDemogPipe$`White Race`, na.rm = TRUE)
mean(FullDataProposedDemogPipe$`Black Race`, na.rm = TRUE)
mean(FullDataProposedDemogPipe$`American Indian Race`, na.rm = TRUE)
mean(FullDataProposedDemogPipe$`Asian Race`, na.rm = TRUE)
mean(FullDataProposedDemogPipe$`Native Hawaiian Race`, na.rm = TRUE)
mean(FullDataProposedDemogPipe$`Other/Unspecified Race`, na.rm = TRUE)
mean(FullDataProposedDemogPipe$`Highest Education Level: None`, na.rm = TRUE)
mean(FullDataProposedDemogPipe$`Highest Education Level: High School`, na.rm = TRUE)
mean(FullDataProposedDemogPipe$`Highest Education Level: GED/High School`, na.rm = TRUE)
mean(FullDataProposedDemogPipe$`Highest Education Level: Some College`, na.rm = TRUE)
mean(FullDataProposedDemogPipe$`Highest Education Level: Associate or Bachelor`, na.rm = TRUE)
mean(FullDataProposedDemogPipe$`Highest Education Level: Bachelor`, na.rm = TRUE)
mean(FullDataProposedDemogPipe$`Highest Education Level: Master/Professional/Doctorate`, na.rm = TRUE)


##Census Tracts in Proposed Restricted Sample
mean(FullDataProposed$AON5E001,na.rm = TRUE) ##Population
mean(FullDataProposed$`Population Density`, na.rm = TRUE)
mean(FullDataProposed$`Urban Indicator`, na.rm = TRUE)
mean(FullDataProposed$`High Income Household Rates`, na.rm = TRUE)
mean(FullDataProposed$`High Poverty Rate`, na.rm = TRUE)
mean(FullDataProposed$`Low Poverty Rate`, na.rm = TRUE)
mean(FullDataProposed$`Homeownership Rates`, na.rm = TRUE)
mean(FullDataProposed$`Vacancy Rates`, na.rm = TRUE)
mean(FullDataProposed$`White Race`, na.rm = TRUE)
mean(FullDataProposed$`Black Race`, na.rm = TRUE)
mean(FullDataProposed$`American Indian Race`, na.rm = TRUE)
mean(FullDataProposed$`Asian Race`, na.rm = TRUE)
mean(FullDataProposed$`Native Hawaiian Race`, na.rm = TRUE)
mean(FullDataProposed$`Other/Unspecified Race`, na.rm = TRUE)
mean(FullDataProposed$`Highest Education Level: None`, na.rm = TRUE)
mean(FullDataProposed$`Highest Education Level: High School`, na.rm = TRUE)
mean(FullDataProposed$`Highest Education Level: GED/High School`, na.rm = TRUE)
mean(FullDataProposed$`Highest Education Level: Some College`, na.rm = TRUE)
mean(FullDataProposed$`Highest Education Level: Associate or Bachelor`, na.rm = TRUE)
mean(FullDataProposed$`Highest Education Level: Bachelor`, na.rm = TRUE)
mean(FullDataProposed$`Highest Education Level: Master/Professional/Doctorate`, na.rm = TRUE)

mean(FullDataProposed$Prev_Acc, na.rm = TRUE)
mean(FullDataProposed$AccTotal, na.rm = TRUE)
mean(FullDataProposed$DaysSinceAcc, na.rm = TRUE)
sum(!is.na(FullDataProposed$DaysSinceAcc))


##Census Tracts in Proposed Restricted Sample - INC
mean(FullDataPropInc$AON5E001,na.rm = TRUE) ##Population
mean(FullDataPropInc$`Population Density`, na.rm = TRUE)
mean(FullDataPropInc$`Urban Indicator`, na.rm = TRUE)
mean(FullDataPropInc$`High Income Household Rates`, na.rm = TRUE)
mean(FullDataPropInc$`High Poverty Rate`, na.rm = TRUE)
mean(FullDataPropInc$`Low Poverty Rate`, na.rm = TRUE)
mean(FullDataPropInc$`Homeownership Rates`, na.rm = TRUE)
mean(FullDataPropInc$`Vacancy Rates`, na.rm = TRUE)
mean(FullDataPropInc$`White Race`, na.rm = TRUE)
mean(FullDataPropInc$`Black Race`, na.rm = TRUE)
mean(FullDataPropInc$`American Indian Race`, na.rm = TRUE)
mean(FullDataPropInc$`Asian Race`, na.rm = TRUE)
mean(FullDataPropInc$`Native Hawaiian Race`, na.rm = TRUE)
mean(FullDataPropInc$`Other/Unspecified Race`, na.rm = TRUE)
mean(FullDataPropInc$`Highest Education Level: None`, na.rm = TRUE)
mean(FullDataPropInc$`Highest Education Level: High School`, na.rm = TRUE)
mean(FullDataPropInc$`Highest Education Level: GED/High School`, na.rm = TRUE)
mean(FullDataPropInc$`Highest Education Level: Some College`, na.rm = TRUE)
mean(FullDataPropInc$`Highest Education Level: Associate or Bachelor`, na.rm = TRUE)
mean(FullDataPropInc$`Highest Education Level: Bachelor`, na.rm = TRUE)
mean(FullDataPropInc$`Highest Education Level: Master/Professional/Doctorate`, na.rm = TRUE)

mean(FullDataPropInc$Prev_Acc, na.rm = TRUE)
mean(FullDataPropInc$AccTotal, na.rm = TRUE)
mean(FullDataPropInc$DaysSinceAcc, na.rm = TRUE)
sum(!is.na(FullDataPropInc$DaysSinceAcc))


######Compare to existing, should be diff -- they are (not for whole US but for restricted)!



#Step 5b: run ttests to see if restricted existing and restricted proposed are statistically different:

t.test(FullData$AON5E001, FullDataProposed$AON5E001) #Population
t.test(FullData$`Population Density`, FullDataProposed$`Population Density`) #Population Density
t.test(FullData$`Urban Indicator`, FullDataProposed$`Urban Indicator`) #`Urban Indicator`
t.test(FullData$`High Income Household Rates`, FullDataProposed$`High Income Household Rates`) #High Income HH
t.test(FullData$`High Poverty Rate`, FullDataProposed$`High Poverty Rate`) #High Poverty
t.test(FullData$`Low Poverty Rate`, FullDataProposed$`Low Poverty Rate`) #Low Poverty
t.test(FullData$`Homeownership Rates`, FullDataProposed$`Homeownership Rates`) #Home Ownership
t.test(FullData$`Vacancy Rates`, FullDataProposed$`Vacancy Rates`) #`Vacancy Rates` Rates
t.test(FullData$`White Race`, FullDataProposed$`White Race`) #`White Race`
t.test(FullData$`Black Race`, FullDataProposed$`Black Race`) #`Black Race`
t.test(FullData$`American Indian Race`, FullDataProposed$`American Indian Race`) #Native American
t.test(FullData$`Asian Race`, FullDataProposed$`Asian Race`) #`Asian Race`
t.test(FullData$`Native Hawaiian Race`, FullDataProposed$`Native Hawaiian Race`) #Pacific Islander
t.test(FullData$`Other/Unspecified Race`, FullDataProposed$`Other/Unspecified Race`) #Other Race
t.test(FullData$`Highest Education Level: None`, FullDataProposed$`Highest Education Level: None`) #No school
t.test(FullData$`Highest Education Level: High School`, FullDataProposed$`Highest Education Level: High School`) #HS
t.test(FullData$`Highest Education Level: GED/High School`, FullDataProposed$`Highest Education Level: GED/High School`) #GED/HS
t.test(FullData$`Highest Education Level: Some College`, FullDataProposed$`Highest Education Level: Some College`) #Some College Experience
t.test(FullData$`Highest Education Level: Associate or Bachelor`, FullDataProposed$`Highest Education Level: Associate or Bachelor`) #Assoc/Bach
t.test(FullData$`Highest Education Level: Bachelor`, FullDataProposed$`Highest Education Level: Bachelor`) #`Highest Education Level: Bachelor`
t.test(FullData$`Highest Education Level: Master/Professional/Doctorate`, FullDataProposed$`Highest Education Level: Master/Professional/Doctorate`) #Graduate


#Step 5c: run ttests to see if (INC) restricted existing and restricted proposed are statistically different:

t.test(FullDataExistInc$AON5E001, FullDataPropInc$AON5E001) #Population
t.test(FullDataExistInc$`Population Density`, FullDataPropInc$`Population Density`) #Population Density
t.test(FullDataExistInc$`Urban Indicator`, FullDataPropInc$`Urban Indicator`) #`Urban Indicator`
t.test(FullDataExistInc$`High Income Household Rates`, FullDataPropInc$`High Income Household Rates`) #High Income HH
t.test(FullDataExistInc$`High Poverty Rate`, FullDataPropInc$`High Poverty Rate`) #High Poverty
t.test(FullDataExistInc$`Low Poverty Rate`, FullDataPropInc$`Low Poverty Rate`) #Low Poverty
t.test(FullDataExistInc$`Homeownership Rates`, FullDataPropInc$`Homeownership Rates`) #Home Ownership
t.test(FullDataExistInc$`Vacancy Rates`, FullDataPropInc$`Vacancy Rates`) #`Vacancy Rates` Rates
t.test(FullDataExistInc$`White Race`, FullDataPropInc$`White Race`) #`White Race`
t.test(FullDataExistInc$`Black Race`, FullDataPropInc$`Black Race`) #`Black Race`
t.test(FullDataExistInc$`American Indian Race`, FullDataPropInc$`American Indian Race`) #Native American
t.test(FullDataExistInc$`Asian Race`, FullDataPropInc$`Asian Race`) #`Asian Race`
t.test(FullDataExistInc$`Native Hawaiian Race`, FullDataPropInc$`Native Hawaiian Race`) #Pacific Islander
t.test(FullDataExistInc$`Other/Unspecified Race`, FullDataPropInc$`Other/Unspecified Race`) #Other Race
t.test(FullDataExistInc$`Highest Education Level: None`, FullDataPropInc$`Highest Education Level: None`) #No school
t.test(FullDataExistInc$`Highest Education Level: High School`, FullDataPropInc$`Highest Education Level: High School`) #HS
t.test(FullDataExistInc$`Highest Education Level: GED/High School`, FullDataPropInc$`Highest Education Level: GED/High School`) #GED/HS
t.test(FullDataExistInc$`Highest Education Level: Some College`, FullDataPropInc$`Highest Education Level: Some College`) #Some College Experience
t.test(FullDataExistInc$`Highest Education Level: Associate or Bachelor`, FullDataPropInc$`Highest Education Level: Associate or Bachelor`) #Assoc/Bach
t.test(FullDataExistInc$`Highest Education Level: Bachelor`, FullDataPropInc$`Highest Education Level: Bachelor`) #`Highest Education Level: Bachelor`
t.test(FullDataExistInc$`Highest Education Level: Master/Professional/Doctorate`, FullDataPropInc$`Highest Education Level: Master/Professional/Doctorate`) #Graduate


#Step 5d: run ttests to see if the original samples are statistically different from the INC samples

##for existing:
t.test(FullDataExistInc$AON5E001, FullData$AON5E001) #Population
t.test(FullDataExistInc$`Population Density`, FullData$`Population Density`) #Population Density
t.test(FullDataExistInc$`Urban Indicator`, FullData$`Urban Indicator`) #`Urban Indicator`
t.test(FullDataExistInc$`High Income Household Rates`, FullData$`High Income Household Rates`) #High Income HH
t.test(FullDataExistInc$`High Poverty Rate`, FullData$`High Poverty Rate`) #High Poverty
t.test(FullDataExistInc$`Low Poverty Rate`, FullData$`Low Poverty Rate`) #Low Poverty
t.test(FullDataExistInc$`Homeownership Rates`, FullData$`Homeownership Rates`) #Home Ownership
t.test(FullDataExistInc$`Vacancy Rates`, FullData$`Vacancy Rates`) #`Vacancy Rates` Rates
t.test(FullDataExistInc$`White Race`, FullData$`White Race`) #`White Race`
t.test(FullDataExistInc$`Black Race`, FullData$`Black Race`) #`Black Race`
t.test(FullDataExistInc$`American Indian Race`, FullData$`American Indian Race`) #Native American
t.test(FullDataExistInc$`Asian Race`, FullData$`Asian Race`) #`Asian Race`
t.test(FullDataExistInc$`Native Hawaiian Race`, FullData$`Native Hawaiian Race`) #Pacific Islander
t.test(FullDataExistInc$`Other/Unspecified Race`, FullData$`Other/Unspecified Race`) #Other Race
t.test(FullDataExistInc$`Highest Education Level: None`, FullData$`Highest Education Level: None`) #No school
t.test(FullDataExistInc$`Highest Education Level: High School`, FullData$`Highest Education Level: High School`) #HS
t.test(FullDataExistInc$`Highest Education Level: GED/High School`, FullData$`Highest Education Level: GED/High School`) #GED/HS
t.test(FullDataExistInc$`Highest Education Level: Some College`, FullData$`Highest Education Level: Some College`) #Some College Experience
t.test(FullDataExistInc$`Highest Education Level: Associate or Bachelor`, FullData$`Highest Education Level: Associate or Bachelor`) #Assoc/Bach
t.test(FullDataExistInc$`Highest Education Level: Bachelor`, FullData$`Highest Education Level: Bachelor`) #`Highest Education Level: Bachelor`
t.test(FullDataExistInc$`Highest Education Level: Master/Professional/Doctorate`, FullData$`Highest Education Level: Master/Professional/Doctorate`) #Graduate


##for proposed:
t.test(FullDataProposed$AON5E001, FullDataPropInc$AON5E001) #Population
t.test(FullDataProposed$`Population Density`, FullDataPropInc$`Population Density`) #Population Density
t.test(FullDataProposed$`Urban Indicator`, FullDataPropInc$`Urban Indicator`) #`Urban Indicator`
t.test(FullDataProposed$`High Income Household Rates`, FullDataPropInc$`High Income Household Rates`) #High Income HH
t.test(FullDataProposed$`High Poverty Rate`, FullDataPropInc$`High Poverty Rate`) #High Poverty
t.test(FullDataProposed$`Low Poverty Rate`, FullDataPropInc$`Low Poverty Rate`) #Low Poverty
t.test(FullDataProposed$`Homeownership Rates`, FullDataPropInc$`Homeownership Rates`) #Home Ownership
t.test(FullDataProposed$`Vacancy Rates`, FullDataPropInc$`Vacancy Rates`) #`Vacancy Rates` Rates
t.test(FullDataProposed$`White Race`, FullDataPropInc$`White Race`) #`White Race`
t.test(FullDataProposed$`Black Race`, FullDataPropInc$`Black Race`) #`Black Race`
t.test(FullDataProposed$`American Indian Race`, FullDataPropInc$`American Indian Race`) #Native American
t.test(FullDataProposed$`Asian Race`, FullDataPropInc$`Asian Race`) #`Asian Race`
t.test(FullDataProposed$`Native Hawaiian Race`, FullDataPropInc$`Native Hawaiian Race`) #Pacific Islander
t.test(FullDataProposed$`Other/Unspecified Race`, FullDataPropInc$`Other/Unspecified Race`) #Other Race
t.test(FullDataProposed$`Highest Education Level: None`, FullDataPropInc$`Highest Education Level: None`) #No school
t.test(FullDataProposed$`Highest Education Level: High School`, FullDataPropInc$`Highest Education Level: High School`) #HS
t.test(FullDataProposed$`Highest Education Level: GED/High School`, FullDataPropInc$`Highest Education Level: GED/High School`) #GED/HS
t.test(FullDataProposed$`Highest Education Level: Some College`, FullDataPropInc$`Highest Education Level: Some College`) #Some College Experience
t.test(FullDataProposed$`Highest Education Level: Associate or Bachelor`, FullDataPropInc$`Highest Education Level: Associate or Bachelor`) #Assoc/Bach
t.test(FullDataProposed$`Highest Education Level: Bachelor`, FullDataPropInc$`Highest Education Level: Bachelor`) #`Highest Education Level: Bachelor`
t.test(FullDataProposed$`Highest Education Level: Master/Professional/Doctorate`, FullDataPropInc$`Highest Education Level: Master/Professional/Doctorate`) #Graduate



#Step 5e: LASSO regression

###LASSO

####PROPOSED DATA
##make sure Trac_Bin is numeric
train_FullDataPropInc <- FullDataPropInc
train_FullDataPropInc$Trac_Bin <- as.factor(FullDataPropInc$Trac_Bin)


## Partition the data (80% training, 20% testing)
set.seed(124)
index <- createDataPartition(train_FullDataPropInc$Trac_Bin, p=0.8, list=FALSE, times=1)
train_FDPropInc <- train_FullDataPropInc[index,]
test_FDPropInc <- train_FullDataPropInc[-index,]

## Define k-fold cross-validation (10-fold cross-validation) framework
ctrlspecs <- trainControl(method="cv", number=10, savePredictions="all")

## Create vector of potential lambda values
lambda_vector <- 10^seq(3, -3, length=500)

## Define alpha values for elastic net regularization (mixing parameter)
alpha_values <- seq(0, 1, by=0.1)

## Define tuning grid with lambda and alpha values
tuning_grid <- expand.grid(alpha = 1, lambda = lambda_vector)

## Specify LASSO regression model to be estimated using training data
lassomodel2 <- train(Trac_Bin ~ `Urban Indicator` + `High Income Household Rates` + `High Poverty Rate` + `Low Poverty Rate` + `Homeownership Rates` + `Vacancy Rates` + `White Race` + `Black Race` + `Asian Race` + `American Indian Race` + `Native Hawaiian Race` + `Other/Unspecified Race` + `Highest Education Level: None` + `Highest Education Level: High School` + `Highest Education Level: GED/High School` + `Highest Education Level: Some College` + `Highest Education Level: Associate or Bachelor` + `Highest Education Level: Bachelor` + `Highest Education Level: Master/Professional/Doctorate` + `Population Density`,
                     data=train_FDPropInc,
                     preProcess = c("center", "scale"),
                     method="glmnet",
                     trControl=ctrlspecs,
                     tuneGrid=tuning_grid,
                     na.action=na.omit)


## Print best lambda and alpha values
lassomodel2$bestTune
lassomodel2$bestTune$lambda


#LASSO regression model coefficients
coef(lassomodel2$finalModel, lassomodel2$bestTune$lambda)
round(coef(lassomodel2$finalModel, lassomodel2$bestTune$lambda), 3)

#Plot log(lambda) and RMSE
plot(lassomodel2$results$lambda,
     lassomodel2$results$RMSE,
     xlab="log(lambda)",
     ylab="RMSE")


##Variable importance
varImp(lassomodel2)


#Data visualization of variable importance
#install.packages("ggplot2")
ggplot(varImp(lassomodel2))


#Model Prediction
predictions2 <- predict(lassomodel2, newdata=test_FDPropInc)

##Model performance/accuracy

# Convert factors to numeric if necessary
if(is.factor(predictions2))
  predictions2 <- as.numeric(as.character(predictions2))
if(is.factor(test_FDPropInc$Trac_Bin))
  test_FDPropInc$Trac_Bin <- as.numeric(as.character(test_FDPropInc$Trac_Bin))


mod2perform <- data.frame(RMSE=RMSE(predictions2, test_FDPropInc$Trac_Bin),
                          Rsquared=R2(predictions2, test_FDPropInc$Trac_Bin))

#View(mod2perform)
#a small: r square of 0.01
#a medium: 0.09
#a large: 0.25


ggplot(varImp(lassomodel2)) #proposed


ggplot(varImp(lassomodel2)) + labs(x="Demographic Variable") #existing


###Removing ``
ggplot(varImp(lassomodel2), aes(x = Variable, y = Importance)) +
  geom_bar(stat = "identity") +
  labs(x = "Demographic Variable", y = "Importance") +
  scale_x_discrete(labels = function(x) gsub("`", "", x))


###Remove 0 importance


# Extract variable importance as a data frame
varImp_data2 <- varImp(lassomodel2)$importance

# Add row names as a new column for variable names
varImp_data2 <- varImp_data2 %>%
  rownames_to_column(var = "Variable")

# Filter out variables with zero importance
filtered_data2 <- varImp_data2 %>%
  filter(Overall > 0)

# Plot
ggplot(filtered_data2, aes(x = reorder(Variable, Overall), y = Overall)) +
  geom_bar(stat = "identity") +
  labs(x = "Demographic Variable", y = "Importance") +
  scale_x_discrete(labels = function(x) gsub("`", "", x)) + coord_flip()





#Step 5: Running logistic regressions

##Strube's variables
Propmodel1 <- glm(Trac_Bin ~ `Urban Indicator` + `High Poverty Rate` + `Black Race`  +
                    `White Race` + `Other/Unspecified Race` + `Highest Education Level: None` + `Highest Education Level: High School` +
                    `Highest Education Level: GED/High School` + `Highest Education Level: Some College` + `Highest Education Level: Bachelor` + `Highest Education Level: Master/Professional/Doctorate` + as.factor(COUNTY) + `Population Density`,
                  data = FullDataProposed, family = binomial)

summary(Propmodel1)
nobs(Propmodel1)


##Model with all variables without interaction
Propmodel2 <- glm(Trac_Bin ~ `Urban Indicator` + `High Income Household Rates` + `High Poverty Rate` + `Low Poverty Rate` + `Homeownership Rates` + 
                    `Vacancy Rates` + `White Race` + `Black Race` + `Asian Race` + `American Indian Race` + `Native Hawaiian Race` + `Other/Unspecified Race` + `Highest Education Level: None` + `Highest Education Level: High School` +
                    `Highest Education Level: GED/High School` + `Highest Education Level: Some College` + `Highest Education Level: Associate or Bachelor` + `Highest Education Level: Bachelor` + `Highest Education Level: Master/Professional/Doctorate` + as.factor(COUNTY) + `Population Density`,
                  data = FullDataProposed, family = binomial)

summary(Propmodel2)
nobs(Propmodel2)




#Propmodel2a <- glm(Trac_Bin ~ `Urban Indicator` + `High Income Household Rates` + `Homeownership Rates` + 
#                 `Vacancy Rates` + `White Race` + `Black Race` + `Asian Race` + `American Indian Race` + `Native Hawaiian Race` + `Other/Unspecified Race` + `Highest Education Level: None` + `Highest Education Level: High School` +
#                `Highest Education Level: GED/High School` + `Highest Education Level: Some College` + `Highest Education Level: Associate or Bachelor` + `Highest Education Level: Bachelor` + `Highest Education Level: Master/Professional/Doctorate` + as.factor(COUNTY) + `Population Density`,
#             data = FullDataProposed, family = binomial)

#summary(Propmodel2a)
#nobs(Propmodel2a)


#Propmodel2b <- glm(Trac_Bin ~ `Urban Indicator` + `High Poverty Rate` + `Homeownership Rates` + 
#               `Vacancy Rates` + `White Race` + `Black Race` + `Asian Race` + `American Indian Race` + `Native Hawaiian Race` + `Other/Unspecified Race` + `Highest Education Level: None` + `Highest Education Level: High School` +
#               `Highest Education Level: GED/High School` + `Highest Education Level: Some College` + `Highest Education Level: Associate or Bachelor` + `Highest Education Level: Bachelor` + `Highest Education Level: Master/Professional/Doctorate` + as.factor(COUNTY) + `Population Density`,
#            data = FullDataProposed, family = binomial)

#summary(Propmodel2b)
#nobs(Propmodel2b)


#Propmodel2c <- glm(Trac_Bin ~ `Urban Indicator` + `Low Poverty Rate` + `Homeownership Rates` + 
#           `Vacancy Rates` + `White Race` + `Black Race` + `Asian Race` + `American Indian Race` + `Native Hawaiian Race` + `Other/Unspecified Race` + `Highest Education Level: None` + `Highest Education Level: High School` +
#            `Highest Education Level: GED/High School` + `Highest Education Level: Some College` + `Highest Education Level: Associate or Bachelor` + `Highest Education Level: Bachelor` + `Highest Education Level: Master/Professional/Doctorate` + as.factor(COUNTY) + `Population Density`,
#        data = FullDataProposed, family = binomial)

#summary(Propmodel2c)
#nobs(Propmodel2c)


#Propmodel2d <- glm(Trac_Bin ~ `Urban Indicator` + `Low Poverty Rate` + `High Poverty Rate` + `Homeownership Rates` + 
#               `Vacancy Rates` + `White Race` + `Black Race` + `Asian Race` + `American Indian Race` + `Native Hawaiian Race` + `Other/Unspecified Race` + `Highest Education Level: None` + `Highest Education Level: High School` +
#              `Highest Education Level: GED/High School` + `Highest Education Level: Some College` + `Highest Education Level: Associate or Bachelor` + `Highest Education Level: Bachelor` + `Highest Education Level: Master/Professional/Doctorate` + as.factor(COUNTY) + `Population Density`,
#           data = FullDataProposed, family = binomial)

#summary(Propmodel2d)
#nobs(Propmodel2d)



##Model with all variables and interaction term with `Urban Indicator` and house ownership
Propmodel3 <- glm(Trac_Bin ~ `Urban Indicator` + `High Income Household Rates` + `High Poverty Rate` + `Low Poverty Rate` + `Homeownership Rates` + 
                    `Vacancy Rates` + `White Race` + `Black Race` + `American Indian Race` + `Asian Race` +
                    `Native Hawaiian Race` + `Other/Unspecified Race` + `Highest Education Level: None` + `Highest Education Level: High School` +
                    `Highest Education Level: GED/High School` + `Highest Education Level: Some College` + `Highest Education Level: Associate or Bachelor` + `Highest Education Level: Bachelor` + `Highest Education Level: Master/Professional/Doctorate` + `Urban Indicator`:`Homeownership Rates` + as.factor(COUNTY) + `Population Density`,
                  data = FullDataProposed, family = binomial)

summary(Propmodel3)
nobs(Propmodel3)





##Model with accident variables
Propmodel4 <- glm(Trac_Bin ~ Prev_Acc + AccTotal + as.factor(COUNTY) + `Population Density`,
                  data = FullDataProposed, family = binomial)

summary(Propmodel4)
nobs(Propmodel4)



Propmodel5 <- glm(Trac_Bin ~ `Urban Indicator` + `High Income Household Rates` + `High Poverty Rate` + `Low Poverty Rate` + `Homeownership Rates` + 
                    `Vacancy Rates` + `White Race` + `Black Race` + `Native Hawaiian Race`+ `Other/Unspecified Race` + `Highest Education Level: None` + `Highest Education Level: High School` +
                    `Highest Education Level: GED/High School` + `Highest Education Level: Some College` + `Highest Education Level: Bachelor` + `Highest Education Level: Master/Professional/Doctorate` + Prev_Acc + AccTotal + as.factor(COUNTY) + `Population Density`,
                  data = FullDataProposed, family = binomial)

summary(Propmodel5)
nobs(Propmodel5)


Propmodel6 <- glm(Trac_Bin ~ DaysSinceAcc + as.factor(COUNTY) + `Population Density`,
                  data = FullDataProposed, family = binomial)

summary(Propmodel6)
nobs(Propmodel6)

#`Urban Indicator` + `High Income Household Rates` + `High Poverty Rate` + `Low Poverty Rate` + `Homeownership Rates` + 
#`Vacancy Rates` + `White Race` + `Black Race` + `Native Hawaiian Race`+ `Other/Unspecified Race` + `Highest Education Level: None` + `Highest Education Level: High School` +
# `Highest Education Level: GED/High School` + `Highest Education Level: Some College` + `Highest Education Level: Bachelor` + `Highest Education Level: Master/Professional/Doctorate` + Prev_Acc + AccTotal +



#Step 5.1: Running logistic regressions - INC

##Strube's variables
incPropmodel1 <- glm(Trac_Bin ~ `Urban Indicator` + `High Poverty Rate` + `Black Race`  +
                       `White Race` + `Other/Unspecified Race` + `Highest Education Level: None` + `Highest Education Level: High School` +
                       `Highest Education Level: GED/High School` + `Highest Education Level: Some College` + `Highest Education Level: Bachelor` + `Highest Education Level: Master/Professional/Doctorate` + as.factor(COUNTY) + `Population Density`,
                     data = FullDataPropInc, family = binomial)

summary(incPropmodel1)
nobs(incPropmodel1)



##One used in paper
##Model with all variables without interaction
incPropmodel2 <- glm(Trac_Bin ~ `High Income Household Rates` + `High Poverty Rate` + `Homeownership Rates` + 
                       `Vacancy Rates` + `Black Race` + `Asian Race` + `American Indian Race` + `Native Hawaiian Race` 
                     + `Other/Unspecified Race` + `Highest Education Level: None` + `Highest Education Level: High School` +
                       `Highest Education Level: Some College` + `Highest Education Level: Associate or Bachelor` + `Highest Education Level: Bachelor` + `Population Density` + as.factor(COUNTY),
                     data = FullDataPropInc, family = binomial)

summary(incPropmodel2)
nobs(incPropmodel2)


##MARGINAL EFFECT

# Rename variables in the dataset
FullDataPropInc_ME <- FullDataPropInc
colnames(FullDataPropInc_ME) <- gsub("[:/ ]", "_", colnames(FullDataPropInc))


FullDataPropInc_ME$COUNTY <- as.factor(FullDataPropInc_ME$COUNTY)

# Refit the model with updated variable names
propincmodel2_ME <- glm(Trac_Bin ~ High_Income_Household_Rates + 
                          High_Poverty_Rate + 
                          Homeownership_Rates + 
                          Vacancy_Rates + 
                          Black_Race + 
                          Asian_Race + 
                          American_Indian_Race + 
                          Native_Hawaiian_Race + 
                          Other_Unspecified_Race + 
                          Highest_Education_Level__None + 
                          Highest_Education_Level__High_School + 
                          Highest_Education_Level__Some_College + 
                          Highest_Education_Level__Associate_or_Bachelor + 
                          Highest_Education_Level__Bachelor + 
                          Population_Density + COUNTY,
                        data = FullDataPropInc_ME, family = binomial)



##MARGINAL EFFECT


propavemargeffect <- margins(propincmodel2_ME, variables = c("High_Income_Household_Rates",
                                                             "High_Poverty_Rate",
                                                             "Homeownership_Rates",
                                                             "Vacancy_Rates",
                                                             "Black_Race",
                                                             "Asian_Race",
                                                             "American_Indian_Race",
                                                             "Native_Hawaiian_Race",
                                                             "Other_Unspecified_Race",
                                                             "Highest_Education_Level__None",
                                                             "Highest_Education_Level__High_School",
                                                             "Highest_Education_Level__Some_College",
                                                             "Highest_Education_Level__Associate_or_Bachelor",
                                                             "Highest_Education_Level__Bachelor",
                                                             "Population_Density"))
summary(propavemargeffect)


#### Compute STANDARD DEVIATION

# Convert the marginal effects to a data frame
prop_marginal_effects_df <- as.data.frame(propavemargeffect)

# Keep only numeric columns
prop_numeric_marginal_effects <- prop_marginal_effects_df[sapply(prop_marginal_effects_df, is.numeric)]

# Select the specific variables you're interested in
prop_selected_vars <- c("dydx_Black_Race", "dydx_Highest_Education_Level__High_School", 
                        "dydx_Population_Density", "dydx_Homeownership_Rates", "dydx_Highest_Education_Level__Associate_or_Bachelor")

# Calculate the standard deviation for these selected variables
prop_std_dev_selected <- sapply(prop_numeric_marginal_effects[prop_selected_vars], sd, na.rm = TRUE)

# Print standard deviations of the selected marginal effects
print(prop_std_dev_selected)






##Model with all variables and interaction term with `Urban Indicator` and house ownership
incPropmodel3 <- glm(Trac_Bin ~ `Urban Indicator` + `High Income Household Rates` + `High Poverty Rate` + `Low Poverty Rate` + `Homeownership Rates` + 
                       `Vacancy Rates` + `White Race` + `Black Race` + `American Indian Race` + `Asian Race` +
                       `Native Hawaiian Race` + `Other/Unspecified Race` + `Highest Education Level: None` + `Highest Education Level: High School` +
                       `Highest Education Level: GED/High School` + `Highest Education Level: Some College` + `Highest Education Level: Associate or Bachelor` + `Highest Education Level: Bachelor` + `Highest Education Level: Master/Professional/Doctorate` + `Urban Indicator`:`Homeownership Rates` + as.factor(COUNTY) + `Population Density`,
                     data = FullDataPropInc, family = binomial)

summary(incPropmodel3)
nobs(incPropmodel3)





##GRAPHS
###Coefficient plot of the logistic regression model.


#Scale around zero
Propcoef_table <- summary(incPropmodel2)$coefficients
Propcoef_df <- data.frame(
  variable = rownames(Propcoef_table),
  coefficient = Propcoef_table[, 1],
  std_error = Propcoef_table[, 2]
)

# Filter out the county fixed effects
Propcoef_df <- Propcoef_df[!grepl("COUNTY", Propcoef_df$variable), ]

# Determine the x-axis limits based on the coefficients and standard errors
x_min <- min(Propcoef_df$coefficient - 2 * Propcoef_df$std_error)
x_max <- max(Propcoef_df$coefficient + 2 * Propcoef_df$std_error)
x_range <- abs(x_max - x_min)
x_min <- x_min - 0.2 * x_range
x_max <- x_max + 0.2 * x_range

# Create the coefficient plot with 95% confidence intervals
ggplot(Propcoef_df, aes(x = coefficient, y = reorder(variable, coefficient))) +
  geom_point() +
  geom_errorbarh(aes(xmin = coefficient - 1.96 * std_error, xmax = coefficient + 1.96 * std_error), height = 0.2) +
  labs(x = "Coefficient", y = "Variable") +
  scale_x_continuous(limits = c(x_min, x_max)) +
  theme_minimal()


# Filter out variables other than the specified ones
Propcoef_df <- Propcoef_df[Propcoef_df$variable %in% c("`Population Density`", "`Homeownership Rates`", "`Black Race`", "`Other/Unspecified Race`", "`Highest Education Level: High School`", "`Highest Education Level: Associate or Bachelor`"), ]

# Determine the x-axis limits based on the coefficients and standard errors
x_min <- min(Propcoef_df$coefficient - 2 * Propcoef_df$std_error)
x_max <- max(Propcoef_df$coefficient + 2 * Propcoef_df$std_error)
x_range <- abs(x_max - x_min)
x_min <- x_min - 0.2 * x_range
x_max <- x_max + 0.2 * x_range

# Create the coefficient plot with 95% confidence intervals
ggplot(Propcoef_df, aes(x = coefficient, y = reorder(variable, coefficient))) +
  geom_point() +
  geom_errorbarh(aes(xmin = coefficient - 1.96 * std_error, xmax = coefficient + 1.96 * std_error), height = 0.2) +
  labs(x = "Coefficient", y = "Variable") +
  scale_x_continuous(limits = c(x_min, x_max)) +
  theme_minimal()

Propcoef_df$variable <- stringr::str_wrap(Propcoef_df$variable, width = 14)

# Now plot
ggplot(Propcoef_df, aes(x = coefficient, y = reorder(variable, coefficient))) +
  geom_point(size=1.8, color="brown2") +
  geom_errorbarh(aes(xmin = coefficient - 1.96 * std_error, xmax = coefficient + 1.96 * std_error), height = 0.2, size=0.75, color="brown2") +
  labs(x = "Coefficient", y = "Variable") +
  scale_x_continuous(limits = c(x_min, x_max)) +
  theme_minimal()+
  theme(
    panel.border = element_rect(fill = NA, color = "gray", size = 1),
    panel.grid.major = element_line(color = "lightgray", size = 0.25),
    panel.grid.minor = element_line(color = "lightgray", size = 0.25),
    #panel.grid.major.x = element_blank(), # Remove vertical major gridlines
    panel.grid.minor.x = element_blank(), # Remove vertical minor gridlines
    axis.ticks.length = unit(-0.25, "cm") # Adjust the length of axis ticks
  ) +
  scale_y_discrete(labels = function(labels) str_replace_all(labels, "`", ""))




###TRY to do coefficient plot with MARGINAL EFFECTS

# Extract marginal effects as a dataframe
propmarg_df <- summary(propavemargeffect)

# Rename columns for consistency
propmarg_df <- data.frame(
  variable = propmarg_df$factor,  # Variable names
  coefficient = propmarg_df$AME*100,  # Average Marginal Effect
  std_error = propmarg_df$SE*100 # Standard Error
)


propmarg_df$variable <- str_replace_all(propmarg_df$variable, "_", " ")
propmarg_df$variable <- str_replace(propmarg_df$variable, "Highest Education Level  High School", "Highest Education Level: High School")
propmarg_df$variable <- str_replace(propmarg_df$variable, "Highest Education Level  Associate or Bachelor", "Highest Education Level: Associate or Bachelor")


# Filter for the selected variables
selected_vars <- c("Black Race", "Highest Education Level: High School",  "Highest Education Level: Associate or Bachelor", "Homeownership Rates", "Population Density")
propmarg_df <- propmarg_df[propmarg_df$variable %in% selected_vars, ]


# Determine the x-axis limits based on marginal effects and standard errors
x_min <- min(propmarg_df$coefficient - 2 * propmarg_df$std_error)
x_max <- max(propmarg_df$coefficient + 2 * propmarg_df$std_error)
x_range <- abs(x_max - x_min)
x_min <- x_min - 0.2 * x_range
x_max <- x_max + 0.2 * x_range

propmarg_df$variable <- stringr::str_wrap(propmarg_df$variable, width = 14)
# Create the filtered marginal effects plot
ggplot(propmarg_df, aes(x = coefficient, y = reorder(variable, coefficient))) +
  geom_point(size = 1.8, color = "brown2") +
  geom_errorbarh(aes(xmin = coefficient - 1.96 * std_error, xmax = coefficient + 1.96 * std_error), 
                 height = 0.2, size = 0.75, color = "brown2") +
  labs(x = "Marginal Effect", y = "Variable") +
  scale_x_continuous(limits = c(x_min, x_max)) +
  theme_minimal() +
  theme(
    panel.border = element_rect(fill = NA, color = "gray", size = 1),
    panel.grid.major = element_line(color = "lightgray", size = 0.25),
    panel.grid.minor = element_line(color = "lightgray", size = 0.25),
    panel.grid.minor.x = element_blank(), 
    axis.ticks.length = unit(-0.25, "cm"),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(margin = margin(t = 14), size=14),
    axis.title.y = element_text(size=14),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  scale_y_discrete(labels = function(labels) str_replace_all(labels, "`", ""))


#save high resolution
ggsave("C:/Users/julia/OneDrive - University of Virginia/PhD Stuff/Research/Pipeline/Paper Revisions/Final Submissions/proposed_marginal_effects_plot.png", 
       plot = last_plot(), 
       width = 9, height = 6, units = "in", 
       dpi = 300)






#EXPORT
FullDataPropInc$predictions <- predict(incPropmodel2, type = "response")

# Specify the file path where you want to save the Excel file
file_path <- "C:/Users/julia/OneDrive - University of Virginia/PhD Stuff/Research/Pipeline/FullDataPropInc.xlsx"

# Export the dataset to an Excel file
write.xlsx(FullDataPropInc, file_path, rowNames = TRUE)

# Provide a message indicating the export is complete
cat("Dataset exported to", file_path, "\n")






##PROJECTED PIPELINES

#Step 1: Initial cleaning. Start by removing no area pipeline data and no population demographic data


# Using dataset Demographic
# Change the demographic dataset to have no rows where AON5E001 (total population) is equal to 0
DemographicProj <- DemographicProj[DemographicProj $AON5E001 != 0, ]

# went from 85395 to 84524



# Using dataset Proposed
# Change the pipeline dataset to have no rows where ALAND (land area) is equal to 0
Projected <- Projected[Projected$ALAND != 0, ]

# went from 85185 to 85159 observations.





#Step 2: Merging X2021_demographic and Proposed


#This makes a new dataset (FullDataProjDemogPipe) that joins the demographic and pipeline 
# data for all rows in which the GEO_ID value (from demographic data) matches the AFFGEOID 
# value (from pipeline data). Only GEO_ID variable will be shown now.
FullDataProjDemogPipe <- inner_join(DemographicProj, Projected, by = c("GEO_ID" = "AFFGEOID"))
#View(FullDataProjDemogPipe)



#This makes a new dataset (ExtraDemog) that holds the rows/observations in the demographic
# dataset that did not match to any rows/observations in the pipeline dataset.
ExtraProjDemog<- anti_join(DemographicProj, Projected, by = c("GEO_ID" = "AFFGEOID"))
ExtraProjDemog$Source <- "Demographic"
# ExtraDemog has stuff that was in the demographic data but not pipeline data


#This makes a new dataset (ExtraPipe) that holds the rows/observations in the pipeline
# dataset that did not match to any rows/observations in the demographic dataset.
ExtraProjPipe<- anti_join(Projected, DemographicProj, by = c("AFFGEOID" = "GEO_ID"))
ExtraProjPipe$Source <- "Pipeline"
# ExtraPipe has stuff that was in the pipeline data but not demographic data

#View(ExtraProjDemog)
#View(ExtraProjPipe)




#Step 3: Create any new variables necessary to add in later regressions:

#Population Density
# Now create a new column for the variable called `Population Density`, which represents the population 
# density for each row (by dividing the total population by the area for each census tract). This
# may be useful for our regressions later on.

#First change the land area from square meters to square miles
FullDataProjDemogPipe$ALAND <- FullDataProjDemogPipe$ALAND / 2.59e+6


FullDataProjDemogPipe$`Population Density` <- FullDataProjDemogPipe$AON5E001 / FullDataProjDemogPipe$ALAND

#Rural/`Urban Indicator`
#Create another column that indicates if a tract is rural (equal to zero) or `Urban Indicator` (equal to 1).
# A tract will be considered rural if the population density is less than or equal to 1000 people
# per square mile, and `Urban Indicator` if the population density is greater than 1000 people/sq mile.

FullDataProjDemogPipe$`Urban Indicator` <- ifelse(FullDataProjDemogPipe$`Population Density` > 1000, 1, 0)

#Here I'll need to add the 4 type variable with the metropolitan variable (need to figure that out)



#Income and Poverty
#Create a new column, called IncTot, which represents the total number of observations within
# each household income bracket for each census tract.

FullDataProjDemogPipe$IncTot <- rowSums(FullDataProjDemogPipe[, c("AOQHE002", "AOQHE003", "AOQHE004", "AOQHE005", "AOQHE006", "AOQHE007", "AOQHE008", "AOQHE009", "AOQHE010", "AOQHE011", "AOQHE012", "AOQHE013", "AOQHE014", "AOQHE015", "AOQHE016", "AOQHE017")])
#this wasn't necessary to do

#Create another column, called `High Income Household Rates`, which represents the percentage of households within each
# census tract that make at least $200K per year

FullDataProjDemogPipe$`High Income Household Rates` <- (FullDataProjDemogPipe$AOQHE017 / FullDataProjDemogPipe$IncTot) * 100


#Create a new column, called PovTot, which represents the total number of observations within
# each poverty ratio bracket for each census tract.
FullDataProjDemogPipe$PovTot <- rowSums(FullDataProjDemogPipe[, c("AOXWE002", "AOXWE003", "AOXWE004", "AOXWE005", "AOXWE006", "AOXWE007", "AOXWE008")])
#this wasn't necessary to do

#Create another column, called `Low Poverty Rate`, which represents the percentage of households within each
# census tract that have at a high ratio of income to poverty level (2 or greater)
FullDataProjDemogPipe$`Low Poverty Rate` <- (FullDataProjDemogPipe$AOXWE008 / FullDataProjDemogPipe$PovTot) * 100

#Create another column, called `High Poverty Rate`, which represents the percentage of households within each
# census tract that have at a low ratio of income to poverty level (under 0.5)
FullDataProjDemogPipe$`High Poverty Rate` <- (FullDataProjDemogPipe$AOXWE002 / FullDataProjDemogPipe$PovTot) * 100



#Housing Occupation Rates
#Create a new column, called HouseTot, which represents the total number of observations within
# each housing occupation bracket for each census tract (both renters and owners).
FullDataProjDemogPipe$HouseTot <- rowSums(FullDataProjDemogPipe[, c("AOSPE002", "AOSPE003")])
#this wasn't necessary to do

#Create another column, called `Homeownership Rates`, which represents the percentage of households within each
# census tract where housing units are owned (as opposed to being rented).
FullDataProjDemogPipe$`Homeownership Rates` <- (FullDataProjDemogPipe$AOSPE002 / FullDataProjDemogPipe$HouseTot) * 100

#Create another column, called HouseRatio_renter, which represents the percentage of households within each
# census tract where housing units are owned (as opposed to being rented).
FullDataProjDemogPipe$HouseRatio_renter <- (FullDataProjDemogPipe$AOSPE003 / FullDataProjDemogPipe$HouseTot) * 100


#Create a new column, called VacantTot, which represents the total number of observations within
# each housing occupation bracket for each census tract (both vacant and occupied).
FullDataProjDemogPipe$VacantTot <- rowSums(FullDataProjDemogPipe[, c("AOSOE002", "AOSOE003")])
#this wasn't necessary to do

#Create another column, called VacantRatio_occ, which represents the percentage of households within each
# census tract where housing units are occupied (as opposed to being vacant).
FullDataProjDemogPipe$VacantRatio_occ <- (FullDataProjDemogPipe$AOSOE002 / FullDataProjDemogPipe$VacantTot) * 100

#Create another column, called `Vacancy Rates`, which represents the percentage of households within each
# census tract where housing units are occupied (as opposed to being vacant).
FullDataProjDemogPipe$`Vacancy Rates` <- (FullDataProjDemogPipe$AOSOE003 / FullDataProjDemogPipe$VacantTot) * 100



#Race
#Create a new column, called RaceTot, which represents the total number of observations within
# each race bracket for each census tract (should be the same as total population for each census tract)
FullDataProjDemogPipe$RaceTot <- rowSums(FullDataProjDemogPipe[, c("AON5E002", "AON5E002", "AON5E003", "AON5E004", "AON5E005", "AON5E006", "AON5E007", "AON5E008")])
#Did not include AON5E009 and AON5E010 because they are subsets of AON5E008.

#Create another column, called `White Race`, which represents the percentage of people within each
# census tract that are `White Race` alone
FullDataProjDemogPipe$`White Race` <- (FullDataProjDemogPipe$AON5E002 / FullDataProjDemogPipe$RaceTot) * 100

#Create another column, called `Black Race`, which represents the percentage of people within each
# census tract that are `Black Race` or African American alone
FullDataProjDemogPipe$`Black Race` <- (FullDataProjDemogPipe$AON5E003 / FullDataProjDemogPipe$RaceTot) * 100

#Create another column, called `American Indian Race`, which represents the percentage of people within each
# census tract that are American Indian and Alaska Native alone
FullDataProjDemogPipe$`American Indian Race` <- (FullDataProjDemogPipe$AON5E004 / FullDataProjDemogPipe$RaceTot) * 100

#Create another column, called `Asian Race`, which represents the percentage of people within each
# census tract that are `Asian Race` alone
FullDataProjDemogPipe$`Asian Race` <- (FullDataProjDemogPipe$AON5E005 / FullDataProjDemogPipe$RaceTot) * 100

#Create another column, called `Native Hawaiian Race`, which represents the percentage of people within each
# census tract that are Native Hawaiian and Other Pacific Islander alone
FullDataProjDemogPipe$`Native Hawaiian Race` <- (FullDataProjDemogPipe$AON5E006 / FullDataProjDemogPipe$RaceTot) * 100

#Create another column, called `Native Hawaiian Race`, which represents the percentage of people within each
# census tract that are Native Hawaiian and Other Pacific Islander alone
FullDataProjDemogPipe$`Native Hawaiian Race` <- (FullDataProjDemogPipe$AON5E006 / FullDataProjDemogPipe$RaceTot) * 100

#Create another column, called `Other/Unspecified Race`, which represents the percentage of people within each
# census tract that are some other race alone
FullDataProjDemogPipe$`Other/Unspecified Race` <- (FullDataProjDemogPipe$AON5E007 / FullDataProjDemogPipe$RaceTot) * 100

#Create another column, called TwoPlus_Race, which represents the percentage of people within each
# census tract that are two or more races
FullDataProjDemogPipe$TwoPlus_Race <- (FullDataProjDemogPipe$AON5E008 / FullDataProjDemogPipe$RaceTot) * 100



#Educational Attainment
#Create a new column, called EduTot, which represents the total number of observations within
# each education bracket for each census tract (should be the same as total population for each census tract)
FullDataProjDemogPipe$EduTot <- rowSums(FullDataProjDemogPipe[, c("AOP8E002", "AOP8E003", "AOP8E004", "AOP8E005", "AOP8E006", "AOP8E007", "AOP8E008", "AOP8E009", "AOP8E010", "AOP8E011", "AOP8E012", "AOP8E013", "AOP8E014", "AOP8E015", "AOP8E016", "AOP8E017", "AOP8E018", "AOP8E019", "AOP8E020", "AOP8E021", "AOP8E022", "AOP8E023", "AOP8E024", "AOP8E025")])

#Create another column, called `Highest Education Level: None`, which represents the percentage of people within each
# census tract that have no completed education
FullDataProjDemogPipe$`Highest Education Level: None` <- (FullDataProjDemogPipe$AOP8E002 / FullDataProjDemogPipe$EduTot) * 100

#Create another column, called `Highest Education Level: High School`, which represents the percentage of people within each
# census tract that have completed regular high school as their highest education level
FullDataProjDemogPipe$`Highest Education Level: High School` <- (FullDataProjDemogPipe$AOP8E017 / FullDataProjDemogPipe$EduTot) * 100

#Create another column, called `Highest Education Level: GED/High School`, which represents the percentage of people within each
# census tract that have completed high school or has a GED/alternative credential as their highest education level
FullDataProjDemogPipe$`Highest Education Level: GED/High School` <- ((FullDataProjDemogPipe$AOP8E017 + FullDataProjDemogPipe$AOP8E018) / FullDataProjDemogPipe$EduTot) * 100

#Create another column, called `Highest Education Level: Some College`, which represents the percentage of people within each
# census tract that have some college education as their highest education level
FullDataProjDemogPipe$`Highest Education Level: Some College` <- ((FullDataProjDemogPipe$AOP8E019 + FullDataProjDemogPipe$AOP8E020) / FullDataProjDemogPipe$EduTot) * 100

#Create another column, called `Highest Education Level: Associate or Bachelor`, which represents the percentage of people within each
# census tract that have an Associate's or `Highest Education Level: Bachelor`'s degree their highest education level
FullDataProjDemogPipe$`Highest Education Level: Associate or Bachelor` <- ((FullDataProjDemogPipe$AOP8E021 + FullDataProjDemogPipe$AOP8E022) / FullDataProjDemogPipe$EduTot) * 100

#Create another column, called `Highest Education Level: Bachelor`, which represents the percentage of people within each
# census tract that have a `Highest Education Level: Bachelor`'s degree their highest education level
FullDataProjDemogPipe$`Highest Education Level: Bachelor` <- (FullDataProjDemogPipe$AOP8E022 / FullDataProjDemogPipe$EduTot) * 100

#Create another column, called `Highest Education Level: Master/Professional/Doctorate`, which represents the percentage of people within each
# census tract that have a Master's, professional school, or Doctorate degree their highest education level
FullDataProjDemogPipe$`Highest Education Level: Master/Professional/Doctorate` <- ((FullDataProjDemogPipe$AOP8E023 + FullDataProjDemogPipe$AOP8E024 + FullDataProjDemogPipe$AOP8E025) / FullDataProjDemogPipe$EduTot) * 100





#Variables I have:

##Population density -- `Population Density` (measured as people per square mile)
###As population density increases by one person per square mile, Y variable changes by

##`Urban Indicator` -- `Urban Indicator` (measured as 1 if the population density is greater than 1000 people/sq mile)
###Compared to rural areas, `Urban Indicator` areas have an associated change in the Y variable by

##High income -- `High Income Household Rates` (measured as % of households who have an income of at least $200k)
###As the share of high-income households increases by one percentage point, Y variable changes by

##High poverty -- `High Poverty Rate` (measured as % of population who has an income to poverty level ratio of under 0.5)
###As the share of poor people increases by one percentage point, Y variable changes by

##Low poverty -- `Low Poverty Rate` (measured as % of population who has an income to poverty level ratio of at least 2)
###As the share of non-poor people increases by one percentage point, Y variable changes by

##Homeowners -- `Homeownership Rates` (measured as % of occupied housing units that are owned)
###Within occupied housing units, as the share of homeowners increases by one percentage point, Y variable changes by

##Renters -- HouseRatio_renter (measured as % of occupied housing units that are rented)
###Within occupied housing units, as the share of renters increases by one percentage point, Y variable changes by
####Repetitive, will probably use homeowners

##`Vacancy Rates` -- `Vacancy Rates` (measured as % of housing units that are vacant)
###As the share of vacant housing units increases by one percentage point, Y variable changes by

##Occupancy -- VacantRatio_occ (measured as % of housing units that are occupied)
###As the share of occupied housing units increases by one percentage point, Y variable changes by

##`White Race` -- `White Race` (measured as % of people that are `White Race`)
###As the share of people who are `White Race` alone increases by one percentage point, Y variable changes by

##`Black Race` -- `Black Race` (measured as % of people that are `Black Race`)
###As the share of people who are `Black Race` alone increases by one percentage point, Y variable changes by

##American Indian/Native -- `American Indian Race` (measured as % of people that are American Indian)
###As the share of people who are American Indian alone increases by one percentage point, Y variable changes by

##`Asian Race` -- `Asian Race` (measured as % of people that are `Asian Race`)
###As the share of people who are `Asian Race` alone increases by one percentage point, Y variable changes by

##Native Hawaiian/Pacific Islander -- `Native Hawaiian Race` (measured as % of people that are Native Hawaiian)
###As the share of people who are Native Hawaiian alone increases by one percentage point, Y variable changes by

##Other Race -- `Other/Unspecified Race` (measured as % of people that are another race)
###As the share of people who are another race alone increases by one percentage point, Y variable changes by

##Two or more races -- TwoPlus_Race (measured as % of people that are two or more races)
###As the share of people who are two or more races increases by one percentage point, Y variable changes by

##No education -- `Highest Education Level: None` (measured as % of people that have no completed schooling)
###As the share of people who have no completed schooling increases by one percentage point, Y variable changes by

##High school education -- `Highest Education Level: High School` (measured as % of people that have completed regular high school as their highest education level)
###As the share of people who have completed regular high school as their highest education level increases by one percentage point, Y variable changes by

##High school/GED education -- `Highest Education Level: GED/High School` (measured as % of people that have completed regular high school, or received a GED or alternative credential, as their highest education level)
###As the share of people who have completed regular high school, or received a GED or alternative credential, as their highest education level increases by one percentage point, Y variable changes by

##Some college experience -- `Highest Education Level: Some College` (measured as % of people that have some college experience as their highest education level)
###As the share of people who have some college experience as their highest education level increases by one percentage point, Y variable changes by

##Associate/`Highest Education Level: Bachelor` degree -- `Highest Education Level: Associate or Bachelor` (measured as % of people that have an Associate's or `Highest Education Level: Bachelor`'s degree as their highest education level)
###As the share of people who have an Associate's or `Highest Education Level: Bachelor`'s degree as their highest education level increases by one percentage point, Y variable changes by

##`Highest Education Level: Bachelor` degree -- `Highest Education Level: Bachelor` (measured as % of people that have a `Highest Education Level: Bachelor`'s degree as their highest education level)
###As the share of people who have a `Highest Education Level: Bachelor`'s degree as their highest education level increases by one percentage point, Y variable changes by

##Graduate degree -- `Highest Education Level: Master/Professional/Doctorate` (measured as % of people that have a Master's, professional school, or Doctorate degree as their highest education level)
###As the share of people who have a Master's, professional school, or Doctorate degree as their highest education level increases by one percentage point, Y variable changes by



#I'll also need to add any new variables if I need


#Step 4a: remove any census tracts with missing values of any of the variables of interest

variables_to_check_proj <- c("Trac_Bin", "Population Density", "COUNTY", "Urban Indicator", "High Income Household Rates", 
                             "High Poverty Rate", "Low Poverty Rate", "Homeownership Rates", 
                             "Vacancy Rates", "White Race", "Black Race", 
                             "American Indian Race", "Asian Race", "Native Hawaiian Race", 
                             "Other/Unspecified Race", "Highest Education Level: None", "Highest Education Level: High School", 
                             "Highest Education Level: GED/High School", "Highest Education Level: Some College", "Highest Education Level: Associate or Bachelor", 
                             "Highest Education Level: Bachelor", "Highest Education Level: Master/Professional/Doctorate")

# Remove observations with missing values for specified variables
FullDataProjDemogPipe <- FullDataProjDemogPipe[complete.cases(FullDataProjDemogPipe[, variables_to_check_proj]), ]



#Step 4: Now clean data so that there are only counties that have some census tracts that
# do have pipelines and some census tracts that do not (i.e. within a given county value,
# delete if all rows have a pipeline or no rows have a pipeline).



#First: Get rid of counties with no pipelines in any of the census tracts:

#Start by creating a new dataset (FullData_countyzero) of the counties and states that have their total
# pipeline greater than 0
FullDataProj_countyzero <- FullDataProjDemogPipe%>%
  group_by(NAMELSADCO, STATE_NAME) %>%
  summarise(total_trac_bin = sum(Trac_Bin))%>%
  filter(total_trac_bin > 0)
#View(FullDataProj_countyzero)
#View(FullDataProjDemogPipe)

#In both the FullDataDemogPipe and FullData_countyzero, create a new column (CountyState in FullDataDemogPipe
# and CountyStates in FullData_countyzero) that has both the county and state name. This is important because
# some states have counties with the same name, and I needed a way to differentiate them from each other.

FullDataProjDemogPipe$CountyState <- paste(FullDataProjDemogPipe$NAMELSADCO, FullDataProjDemogPipe$STATE_NAME, sep = ", ")
#View(FullDataDemogPipe)

FullDataProj_countyzero$CountyStates <- paste(FullDataProj_countyzero$NAMELSADCO, FullDataProj_countyzero$STATE_NAME, sep = ", ")


#Create another dataset called FullData, that has the rows (census tracts) from FullDataDemogPipe if they
# have a matching value in the FullData_countyzero dataset (looking at TractState and TractStates).
FullDataProj <- FullDataProjDemogPipe %>%
  filter(CountyState %in% FullDataProj_countyzero$CountyStates)

#View(FullDataProj)


#Create a duplicate dataset (called FullDataPropInc) for later regression use that includes census tracts in 
# counties where all census tracts
FullDataProjInc <- FullDataProj


#Second: Get rid of counties with pipelines in all of the census tracts:


#This makes a new dataset called FullData_countyone, with a new variable (PipeTotal) that counts the 
# total number of observations (census tracts) for each CountyState value, within the FullData dataset.
FullDataProj_countyone <- FullDataProj %>%
  group_by(CountyState) %>%
  summarise(PipeTotal = n())

#View(FullDataProj_countyone)

#This joins the FullData_countyzero and FullData_countyone datasets (based on CountyStates and CountyState). 
# This is useful because now I am able to see the number of pipelines in a CountyState value (total_trac_bin),
# as well as the total number of census tracts for that CountyState value (PipeTotal).
FullDataProj_county <- inner_join(FullDataProj_countyzero, FullDataProj_countyone, by = c("CountyStates" = "CountyState"))
#View(FullDataProj_county)

#This removes the CountyState values where the number of pipelines in a CountyState equals the total number 
# of census tracts for that CountyState value. I am doing this because if they equal the same amount, that
# means that every census tract within a CountyState value has a pipeline in it, and we want to remove those.
FullDataProj_county <- FullDataProj_county %>%
  filter(PipeTotal != total_trac_bin)

#Now, FullData_county only has CountyState values where not every census tract has a pipeline in it. So, I
# can keep the observations in FullData where the CountyState value is present in the CountyStates variable
# in the FullData_county dataset.
FullDataProj <- FullDataProj %>%
  filter(CountyState %in% FullDataProj_county$CountyStates)








#Step 5a: descriptive statistics

##Whole US
##Census Tracts in (Projected) Restricted Sample
mean(FullDataProjDemogPipe$AON5E001,na.rm = TRUE) ##Population
mean(FullDataProjDemogPipe$`Population Density`, na.rm = TRUE)
mean(FullDataProjDemogPipe$`Urban Indicator`, na.rm = TRUE)
mean(FullDataProjDemogPipe$`High Income Household Rates`, na.rm = TRUE)
mean(FullDataProjDemogPipe$`High Poverty Rate`, na.rm = TRUE)
mean(FullDataProjDemogPipe$`Low Poverty Rate`, na.rm = TRUE)
mean(FullDataProjDemogPipe$`Homeownership Rates`, na.rm = TRUE)
mean(FullDataProjDemogPipe$`Vacancy Rates`, na.rm = TRUE)
mean(FullDataProjDemogPipe$`White Race`, na.rm = TRUE)
mean(FullDataProjDemogPipe$`Black Race`, na.rm = TRUE)
mean(FullDataProjDemogPipe$`American Indian Race`, na.rm = TRUE)
mean(FullDataProjDemogPipe$`Asian Race`, na.rm = TRUE)
mean(FullDataProjDemogPipe$`Native Hawaiian Race`, na.rm = TRUE)
mean(FullDataProjDemogPipe$`Other/Unspecified Race`, na.rm = TRUE)
mean(FullDataProjDemogPipe$`Highest Education Level: None`, na.rm = TRUE)
mean(FullDataProjDemogPipe$`Highest Education Level: High School`, na.rm = TRUE)
mean(FullDataProjDemogPipe$`Highest Education Level: GED/High School`, na.rm = TRUE)
mean(FullDataProjDemogPipe$`Highest Education Level: Some College`, na.rm = TRUE)
mean(FullDataProjDemogPipe$`Highest Education Level: Associate or Bachelor`, na.rm = TRUE)
mean(FullDataProjDemogPipe$`Highest Education Level: Bachelor`, na.rm = TRUE)
mean(FullDataProjDemogPipe$`Highest Education Level: Master/Professional/Doctorate`, na.rm = TRUE)


##Census Tracts in Projected Restricted Sample
mean(FullDataProj$AON5E001,na.rm = TRUE) ##Population
mean(FullDataProj$`Population Density`, na.rm = TRUE)
mean(FullDataProj$`Urban Indicator`, na.rm = TRUE)
mean(FullDataProj$`High Income Household Rates`, na.rm = TRUE)
mean(FullDataProj$`High Poverty Rate`, na.rm = TRUE)
mean(FullDataProj$`Low Poverty Rate`, na.rm = TRUE)
mean(FullDataProj$`Homeownership Rates`, na.rm = TRUE)
mean(FullDataProj$`Vacancy Rates`, na.rm = TRUE)
mean(FullDataProj$`White Race`, na.rm = TRUE)
mean(FullDataProj$`Black Race`, na.rm = TRUE)
mean(FullDataProj$`American Indian Race`, na.rm = TRUE)
mean(FullDataProj$`Asian Race`, na.rm = TRUE)
mean(FullDataProj$`Native Hawaiian Race`, na.rm = TRUE)
mean(FullDataProj$`Other/Unspecified Race`, na.rm = TRUE)
mean(FullDataProj$`Highest Education Level: None`, na.rm = TRUE)
mean(FullDataProj$`Highest Education Level: High School`, na.rm = TRUE)
mean(FullDataProj$`Highest Education Level: GED/High School`, na.rm = TRUE)
mean(FullDataProj$`Highest Education Level: Some College`, na.rm = TRUE)
mean(FullDataProj$`Highest Education Level: Associate or Bachelor`, na.rm = TRUE)
mean(FullDataProj$`Highest Education Level: Bachelor`, na.rm = TRUE)
mean(FullDataProj$`Highest Education Level: Master/Professional/Doctorate`, na.rm = TRUE)

#mean(FullDataProposed$Prev_Acc, na.rm = TRUE)
#mean(FullDataProposed$AccTotal, na.rm = TRUE)
#mean(FullDataProposed$DaysSinceAcc, na.rm = TRUE)
#sum(!is.na(FullDataProposed$DaysSinceAcc))


##Census Tracts in Projected Restricted Sample - INC
mean(FullDataProjInc$AON5E001,na.rm = TRUE) ##Population
mean(FullDataProjInc$`Population Density`, na.rm = TRUE)
mean(FullDataProjInc$`Urban Indicator`, na.rm = TRUE)
mean(FullDataProjInc$`High Income Household Rates`, na.rm = TRUE)
mean(FullDataProjInc$`High Poverty Rate`, na.rm = TRUE)
mean(FullDataProjInc$`Low Poverty Rate`, na.rm = TRUE)
mean(FullDataProjInc$`Homeownership Rates`, na.rm = TRUE)
mean(FullDataProjInc$`Vacancy Rates`, na.rm = TRUE)
mean(FullDataProjInc$`White Race`, na.rm = TRUE)
mean(FullDataProjInc$`Black Race`, na.rm = TRUE)
mean(FullDataProjInc$`American Indian Race`, na.rm = TRUE)
mean(FullDataProjInc$`Asian Race`, na.rm = TRUE)
mean(FullDataProjInc$`Native Hawaiian Race`, na.rm = TRUE)
mean(FullDataProjInc$`Other/Unspecified Race`, na.rm = TRUE)
mean(FullDataProjInc$`Highest Education Level: None`, na.rm = TRUE)
mean(FullDataProjInc$`Highest Education Level: High School`, na.rm = TRUE)
mean(FullDataProjInc$`Highest Education Level: GED/High School`, na.rm = TRUE)
mean(FullDataProjInc$`Highest Education Level: Some College`, na.rm = TRUE)
mean(FullDataProjInc$`Highest Education Level: Associate or Bachelor`, na.rm = TRUE)
mean(FullDataProjInc$`Highest Education Level: Bachelor`, na.rm = TRUE)
mean(FullDataProjInc$`Highest Education Level: Master/Professional/Doctorate`, na.rm = TRUE)

#mean(FullDataPropInc$Prev_Acc, na.rm = TRUE)
#mean(FullDataPropInc$AccTotal, na.rm = TRUE)
#mean(FullDataPropInc$DaysSinceAcc, na.rm = TRUE)
#sum(!is.na(FullDataPropInc$DaysSinceAcc))


######Compare to existing, should be diff -- they are (not for whole US but for restricted)!



#Step 5b: run ttests to see if restricted US and restricted projected are statistically different:

t.test(FullData$AON5E001, FullDataProj$AON5E001) #Population
t.test(FullData$`Population Density`, FullDataProj$`Population Density`) #Population Density
t.test(FullData$`Urban Indicator`, FullDataProj$`Urban Indicator`) #`Urban Indicator`
t.test(FullData$`High Income Household Rates`, FullDataProj$`High Income Household Rates`) #High Income HH
t.test(FullData$`High Poverty Rate`, FullDataProj$`High Poverty Rate`) #High Poverty
t.test(FullData$`Low Poverty Rate`, FullDataProj$`Low Poverty Rate`) #Low Poverty
t.test(FullData$`Homeownership Rates`, FullDataProj$`Homeownership Rates`) #Home Ownership
t.test(FullData$`Vacancy Rates`, FullDataProj$`Vacancy Rates`) #`Vacancy Rates` Rates
t.test(FullData$`White Race`, FullDataProj$`White Race`) #`White Race`
t.test(FullData$`Black Race`, FullDataProj$`Black Race`) #`Black Race`
t.test(FullData$`American Indian Race`, FullDataProj$`American Indian Race`) #Native American
t.test(FullData$`Asian Race`, FullDataProj$`Asian Race`) #`Asian Race`
t.test(FullData$`Native Hawaiian Race`, FullDataProj$`Native Hawaiian Race`) #Pacific Islander
t.test(FullData$`Other/Unspecified Race`, FullDataProj$`Other/Unspecified Race`) #Other Race
t.test(FullData$`Highest Education Level: None`, FullDataProj$`Highest Education Level: None`) #No school
t.test(FullData$`Highest Education Level: High School`, FullDataProj$`Highest Education Level: High School`) #HS
t.test(FullData$`Highest Education Level: GED/High School`, FullDataProj$`Highest Education Level: GED/High School`) #GED/HS
t.test(FullData$`Highest Education Level: Some College`, FullDataProj$`Highest Education Level: Some College`) #Some College Experience
t.test(FullData$`Highest Education Level: Associate or Bachelor`, FullDataProj$`Highest Education Level: Associate or Bachelor`) #Assoc/Bach
t.test(FullData$`Highest Education Level: Bachelor`, FullDataProj$`Highest Education Level: Bachelor`) #`Highest Education Level: Bachelor`
t.test(FullData$`Highest Education Level: Master/Professional/Doctorate`, FullDataProj$`Highest Education Level: Master/Professional/Doctorate`) #Graduate


#Step 5c: run ttests to see if (INC) restricted US and restricted proposed are statistically different:

t.test(FullDataProjDemogPipe$AON5E001, FullDataProjInc$AON5E001) #Population
t.test(FullDataProjDemogPipe$`Population Density`, FullDataProjInc$`Population Density`) #Population Density
t.test(FullDataProjDemogPipe$`Urban Indicator`, FullDataProjInc$`Urban Indicator`) #`Urban Indicator`
t.test(FullDataProjDemogPipe$`High Income Household Rates`, FullDataProjInc$`High Income Household Rates`) #High Income HH
t.test(FullDataProjDemogPipe$`High Poverty Rate`, FullDataProjInc$`High Poverty Rate`) #High Poverty
t.test(FullDataProjDemogPipe$`Low Poverty Rate`, FullDataProjInc$`Low Poverty Rate`) #Low Poverty
t.test(FullDataProjDemogPipe$`Homeownership Rates`, FullDataProjInc$`Homeownership Rates`) #Home Ownership
t.test(FullDataProjDemogPipe$`Vacancy Rates`, FullDataProjInc$`Vacancy Rates`) #`Vacancy Rates` Rates
t.test(FullDataProjDemogPipe$`White Race`, FullDataProjInc$`White Race`) #`White Race`
t.test(FullDataProjDemogPipe$`Black Race`, FullDataProjInc$`Black Race`) #`Black Race`
t.test(FullDataProjDemogPipe$`American Indian Race`, FullDataProjInc$`American Indian Race`) #Native American
t.test(FullDataProjDemogPipe$`Asian Race`, FullDataProjInc$`Asian Race`) #`Asian Race`
t.test(FullDataProjDemogPipe$`Native Hawaiian Race`, FullDataProjInc$`Native Hawaiian Race`) #Pacific Islander
t.test(FullDataProjDemogPipe$`Other/Unspecified Race`, FullDataProjInc$`Other/Unspecified Race`) #Other Race
t.test(FullDataProjDemogPipe$`Highest Education Level: None`, FullDataProjInc$`Highest Education Level: None`) #No school
t.test(FullDataProjDemogPipe$`Highest Education Level: High School`, FullDataProjInc$`Highest Education Level: High School`) #HS
t.test(FullDataProjDemogPipe$`Highest Education Level: GED/High School`, FullDataProjInc$`Highest Education Level: GED/High School`) #GED/HS
t.test(FullDataProjDemogPipe$`Highest Education Level: Some College`, FullDataProjInc$`Highest Education Level: Some College`) #Some College Experience
t.test(FullDataProjDemogPipe$`Highest Education Level: Associate or Bachelor`, FullDataProjInc$`Highest Education Level: Associate or Bachelor`) #Assoc/Bach
t.test(FullDataProjDemogPipe$`Highest Education Level: Bachelor`, FullDataProjInc$`Highest Education Level: Bachelor`) #`Highest Education Level: Bachelor`
t.test(FullDataProjDemogPipe$`Highest Education Level: Master/Professional/Doctorate`, FullDataProjInc$`Highest Education Level: Master/Professional/Doctorate`) #Graduate



##comp existing to projected:
t.test(FullDataExistInc$AON5E001, FullDataProjInc$AON5E001) #Population
t.test(FullDataExistInc$`Population Density`, FullDataProjInc$`Population Density`) #Population Density
t.test(FullDataExistInc$`Urban Indicator`, FullDataProjInc$`Urban Indicator`) #`Urban Indicator`
t.test(FullDataExistInc$`High Income Household Rates`, FullDataProjInc$`High Income Household Rates`) #High Income HH
t.test(FullDataExistInc$`High Poverty Rate`, FullDataProjInc$`High Poverty Rate`) #High Poverty
t.test(FullDataExistInc$`Low Poverty Rate`, FullDataProjInc$`Low Poverty Rate`) #Low Poverty
t.test(FullDataExistInc$`Homeownership Rates`, FullDataProjInc$`Homeownership Rates`) #Home Ownership
t.test(FullDataExistInc$`Vacancy Rates`, FullDataProjInc$`Vacancy Rates`) #`Vacancy Rates` Rates
t.test(FullDataExistInc$`White Race`, FullDataProjInc$`White Race`) #`White Race`
t.test(FullDataExistInc$`Black Race`, FullDataProjInc$`Black Race`) #`Black Race`
t.test(FullDataExistInc$`American Indian Race`, FullDataProjInc$`American Indian Race`) #Native American
t.test(FullDataExistInc$`Asian Race`, FullDataProjInc$`Asian Race`) #`Asian Race`
t.test(FullDataExistInc$`Native Hawaiian Race`, FullDataProjInc$`Native Hawaiian Race`) #Pacific Islander
t.test(FullDataExistInc$`Other/Unspecified Race`, FullDataProjInc$`Other/Unspecified Race`) #Other Race
t.test(FullDataExistInc$`Highest Education Level: None`, FullDataProjInc$`Highest Education Level: None`) #No school
t.test(FullDataExistInc$`Highest Education Level: High School`, FullDataProjInc$`Highest Education Level: High School`) #HS
t.test(FullDataExistInc$`Highest Education Level: GED/High School`, FullDataProjInc$`Highest Education Level: GED/High School`) #GED/HS
t.test(FullDataExistInc$`Highest Education Level: Some College`, FullDataProjInc$`Highest Education Level: Some College`) #Some College Experience
t.test(FullDataExistInc$`Highest Education Level: Associate or Bachelor`, FullDataProjInc$`Highest Education Level: Associate or Bachelor`) #Assoc/Bach
t.test(FullDataExistInc$`Highest Education Level: Bachelor`, FullDataProjInc$`Highest Education Level: Bachelor`) #`Highest Education Level: Bachelor`
t.test(FullDataExistInc$`Highest Education Level: Master/Professional/Doctorate`, FullDataProjInc$`Highest Education Level: Master/Professional/Doctorate`) #Graduate



##comp proposed to projected:
t.test(FullDataPropInc$AON5E001, FullDataProjInc$AON5E001) #Population
t.test(FullDataPropInc$`Population Density`, FullDataProjInc$`Population Density`) #Population Density
t.test(FullDataPropInc$`Urban Indicator`, FullDataProjInc$`Urban Indicator`) #`Urban Indicator`
t.test(FullDataPropInc$`High Income Household Rates`, FullDataProjInc$`High Income Household Rates`) #High Income HH
t.test(FullDataPropInc$`High Poverty Rate`, FullDataProjInc$`High Poverty Rate`) #High Poverty
t.test(FullDataPropInc$`Low Poverty Rate`, FullDataProjInc$`Low Poverty Rate`) #Low Poverty
t.test(FullDataPropInc$`Homeownership Rates`, FullDataProjInc$`Homeownership Rates`) #Home Ownership
t.test(FullDataPropInc$`Vacancy Rates`, FullDataProjInc$`Vacancy Rates`) #`Vacancy Rates` Rates
t.test(FullDataPropInc$`White Race`, FullDataProjInc$`White Race`) #`White Race`
t.test(FullDataPropInc$`Black Race`, FullDataProjInc$`Black Race`) #`Black Race`
t.test(FullDataPropInc$`American Indian Race`, FullDataProjInc$`American Indian Race`) #Native American
t.test(FullDataPropInc$`Asian Race`, FullDataProjInc$`Asian Race`) #`Asian Race`
t.test(FullDataPropInc$`Native Hawaiian Race`, FullDataProjInc$`Native Hawaiian Race`) #Pacific Islander
t.test(FullDataPropInc$`Other/Unspecified Race`, FullDataProjInc$`Other/Unspecified Race`) #Other Race
t.test(FullDataPropInc$`Highest Education Level: None`, FullDataProjInc$`Highest Education Level: None`) #No school
t.test(FullDataPropInc$`Highest Education Level: High School`, FullDataProjInc$`Highest Education Level: High School`) #HS
t.test(FullDataPropInc$`Highest Education Level: GED/High School`, FullDataProjInc$`Highest Education Level: GED/High School`) #GED/HS
t.test(FullDataPropInc$`Highest Education Level: Some College`, FullDataProjInc$`Highest Education Level: Some College`) #Some College Experience
t.test(FullDataPropInc$`Highest Education Level: Associate or Bachelor`, FullDataProjInc$`Highest Education Level: Associate or Bachelor`) #Assoc/Bach
t.test(FullDataPropInc$`Highest Education Level: Bachelor`, FullDataProjInc$`Highest Education Level: Bachelor`) #`Highest Education Level: Bachelor`
t.test(FullDataPropInc$`Highest Education Level: Master/Professional/Doctorate`, FullDataProjInc$`Highest Education Level: Master/Professional/Doctorate`) #Graduate


#Step 5d: run ttests to see if the original samples are statistically different from the INC samples

##for existing:
t.test(FullDataExistInc$AON5E001, FullData$AON5E001) #Population
t.test(FullDataExistInc$`Population Density`, FullData$`Population Density`) #Population Density
t.test(FullDataExistInc$`Urban Indicator`, FullData$`Urban Indicator`) #`Urban Indicator`
t.test(FullDataExistInc$`High Income Household Rates`, FullData$`High Income Household Rates`) #High Income HH
t.test(FullDataExistInc$`High Poverty Rate`, FullData$`High Poverty Rate`) #High Poverty
t.test(FullDataExistInc$`Low Poverty Rate`, FullData$`Low Poverty Rate`) #Low Poverty
t.test(FullDataExistInc$`Homeownership Rates`, FullData$`Homeownership Rates`) #Home Ownership
t.test(FullDataExistInc$`Vacancy Rates`, FullData$`Vacancy Rates`) #`Vacancy Rates` Rates
t.test(FullDataExistInc$`White Race`, FullData$`White Race`) #`White Race`
t.test(FullDataExistInc$`Black Race`, FullData$`Black Race`) #`Black Race`
t.test(FullDataExistInc$`American Indian Race`, FullData$`American Indian Race`) #Native American
t.test(FullDataExistInc$`Asian Race`, FullData$`Asian Race`) #`Asian Race`
t.test(FullDataExistInc$`Native Hawaiian Race`, FullData$`Native Hawaiian Race`) #Pacific Islander
t.test(FullDataExistInc$`Other/Unspecified Race`, FullData$`Other/Unspecified Race`) #Other Race
t.test(FullDataExistInc$`Highest Education Level: None`, FullData$`Highest Education Level: None`) #No school
t.test(FullDataExistInc$`Highest Education Level: High School`, FullData$`Highest Education Level: High School`) #HS
t.test(FullDataExistInc$`Highest Education Level: GED/High School`, FullData$`Highest Education Level: GED/High School`) #GED/HS
t.test(FullDataExistInc$`Highest Education Level: Some College`, FullData$`Highest Education Level: Some College`) #Some College Experience
t.test(FullDataExistInc$`Highest Education Level: Associate or Bachelor`, FullData$`Highest Education Level: Associate or Bachelor`) #Assoc/Bach
t.test(FullDataExistInc$`Highest Education Level: Bachelor`, FullData$`Highest Education Level: Bachelor`) #`Highest Education Level: Bachelor`
t.test(FullDataExistInc$`Highest Education Level: Master/Professional/Doctorate`, FullData$`Highest Education Level: Master/Professional/Doctorate`) #Graduate


##for projected:
t.test(FullDataProj$AON5E001, FullDataProjInc$AON5E001) #Population
t.test(FullDataProj$`Population Density`, FullDataProjInc$`Population Density`) #Population Density
t.test(FullDataProj$`Urban Indicator`, FullDataProjInc$`Urban Indicator`) #`Urban Indicator`
t.test(FullDataProj$`High Income Household Rates`, FullDataProjInc$`High Income Household Rates`) #High Income HH
t.test(FullDataProj$`High Poverty Rate`, FullDataProjInc$`High Poverty Rate`) #High Poverty
t.test(FullDataProj$`Low Poverty Rate`, FullDataProjInc$`Low Poverty Rate`) #Low Poverty
t.test(FullDataProj$`Homeownership Rates`, FullDataProjInc$`Homeownership Rates`) #Home Ownership
t.test(FullDataProj$`Vacancy Rates`, FullDataProjInc$`Vacancy Rates`) #`Vacancy Rates` Rates
t.test(FullDataProj$`White Race`, FullDataProjInc$`White Race`) #`White Race`
t.test(FullDataProj$`Black Race`, FullDataProjInc$`Black Race`) #`Black Race`
t.test(FullDataProj$`American Indian Race`, FullDataProjInc$`American Indian Race`) #Native American
t.test(FullDataProj$`Asian Race`, FullDataProjInc$`Asian Race`) #`Asian Race`
t.test(FullDataProj$`Native Hawaiian Race`, FullDataProjInc$`Native Hawaiian Race`) #Pacific Islander
t.test(FullDataProj$`Other/Unspecified Race`, FullDataProjInc$`Other/Unspecified Race`) #Other Race
t.test(FullDataProj$`Highest Education Level: None`, FullDataProjInc$`Highest Education Level: None`) #No school
t.test(FullDataProj$`Highest Education Level: High School`, FullDataProjInc$`Highest Education Level: High School`) #HS
t.test(FullDataProj$`Highest Education Level: GED/High School`, FullDataProjInc$`Highest Education Level: GED/High School`) #GED/HS
t.test(FullDataProj$`Highest Education Level: Some College`, FullDataProjInc$`Highest Education Level: Some College`) #Some College Experience
t.test(FullDataProj$`Highest Education Level: Associate or Bachelor`, FullDataProjInc$`Highest Education Level: Associate or Bachelor`) #Assoc/Bach
t.test(FullDataProj$`Highest Education Level: Bachelor`, FullDataProjInc$`Highest Education Level: Bachelor`) #`Highest Education Level: Bachelor`
t.test(FullDataProj$`Highest Education Level: Master/Professional/Doctorate`, FullDataProjInc$`Highest Education Level: Master/Professional/Doctorate`) #Graduate






#Step 5e: LASSO regression


####PROJECTED DATA
##make sure Trac_Bin is numeric
train_FullDataProjInc <- FullDataProjInc
train_FullDataProjInc$Trac_Bin <- as.factor(FullDataProjInc$Trac_Bin)


## Partition the data (80% training, 20% testing)
set.seed(125)
index <- createDataPartition(train_FullDataProjInc$Trac_Bin, p=0.8, list=FALSE, times=1)
train_FDProjInc <- train_FullDataProjInc[index,]
test_FDProjInc <- train_FullDataProjInc[-index,]

## Define k-fold cross-validation (10-fold cross-validation) framework
ctrlspecs <- trainControl(method="cv", number=10, savePredictions="all")

## Create vector of potential lambda values
lambda_vector <- 10^seq(3, -3, length=500)

## Define alpha values for elastic net regularization (mixing parameter)
alpha_values <- seq(0, 1, by=0.1)

## Define tuning grid with lambda and alpha values
tuning_grid <- expand.grid(alpha = 1, lambda = lambda_vector)

## Specify LASSO regression model to be estimated using training data
lassomodel3 <- train(Trac_Bin ~ `Urban Indicator` + `High Income Household Rates` + `High Poverty Rate` + `Low Poverty Rate` + `Homeownership Rates` + `Vacancy Rates` + `White Race` + `Black Race` + `Asian Race` + `American Indian Race` + `Native Hawaiian Race` + `Other/Unspecified Race` + `Highest Education Level: None` + `Highest Education Level: High School` + `Highest Education Level: GED/High School` + `Highest Education Level: Some College` + `Highest Education Level: Associate or Bachelor` + `Highest Education Level: Bachelor` + `Highest Education Level: Master/Professional/Doctorate` + `Population Density`,
                     data=train_FDProjInc,
                     preProcess = c("center", "scale"),
                     method="glmnet",
                     trControl=ctrlspecs,
                     tuneGrid=tuning_grid,
                     na.action=na.omit)


## Print best lambda and alpha values
lassomodel3$bestTune
lassomodel3$bestTune$lambda


#LASSO regression model coefficients
coef(lassomodel3$finalModel, lassomodel3$bestTune$lambda)
round(coef(lassomodel3$finalModel, lassomodel3$bestTune$lambda), 3)

#Plot log(lambda) and RMSE
plot(lassomodel3$results$lambda,
     lassomodel3$results$RMSE,
     xlab="log(lambda)",
     ylab="RMSE")


##Variable importance
varImp(lassomodel3)


#Data visualization of variable importance
#install.packages("ggplot2")
ggplot(varImp(lassomodel3))


#Model Prediction
predictions3 <- predict(lassomodel3, newdata=test_FDProjInc)

##Model performance/accuracy

# Convert factors to numeric if necessary
if(is.factor(predictions3))
  predictions3 <- as.numeric(as.character(predictions3))
if(is.factor(test_FDProjInc$Trac_Bin))
  test_FDProjInc$Trac_Bin <- as.numeric(as.character(test_FDProjInc$Trac_Bin))


mod3perform <- data.frame(RMSE=RMSE(predictions3, test_FDProjInc$Trac_Bin),
                          Rsquared=R2(predictions3, test_FDProjInc$Trac_Bin))

#View(mod3perform)
#a small: r square of 0.01
#a medium: 0.09
#a large: 0.25


ggplot(varImp(lassomodel3)) #projected



ggplot(varImp(lassomodel3)) + labs(x="Demographic Variable") #existing


###Removing ``
ggplot(varImp(lassomodel3), aes(x = Variable, y = Importance)) +
  geom_bar(stat = "identity") +
  labs(x = "Demographic Variable", y = "Importance") +
  scale_x_discrete(labels = function(x) gsub("`", "", x))


###Remove 0 importance variables


# Extract variable importance as a data frame
varImp_data3 <- varImp(lassomodel3)$importance

# Add row names as a new column for variable names
varImp_data3 <- varImp_data3 %>%
  rownames_to_column(var = "Variable")

# Filter out variables with zero importance
filtered_data3 <- varImp_data3 %>%
  filter(Overall > 0)

# Plot
ggplot(filtered_data3, aes(x = reorder(Variable, Overall), y = Overall)) +
  geom_bar(stat = "identity") +
  labs(x = "Demographic Variable", y = "Importance") +
  scale_x_discrete(labels = function(x) gsub("`", "", x)) + coord_flip()



###Make one importance graph:

# Extract variable importance and add model labels
lasso1_data <- varImp(lassomodel1)$importance %>%
  rownames_to_column(var = "Variable") %>%
  mutate(Model = "lassomodel1")

lasso2_data <- varImp(lassomodel2)$importance %>%
  rownames_to_column(var = "Variable") %>%
  mutate(Model = "lassomodel2")

lasso3_data <- varImp(lassomodel3)$importance %>%
  rownames_to_column(var = "Variable") %>%
  mutate(Model = "lassomodel3")

# Combine the datasets
combined_data <- bind_rows(lasso1_data, lasso2_data, lasso3_data)

# Plot all data together
ggplot(combined_data, aes(x = Variable, y = Overall, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.8) +
  # Add vertical lines
  geom_vline(xintercept = seq(1.5, length(unique(combined_data$Variable)) - 0.5, by = 1), 
             color = "lightgray", linetype = "dashed", size = 0.5) +
  labs(x = "Demographic Variable", y = "Importance", fill = "Model") +
  scale_fill_manual(values = c("lassomodel1" = "chartreuse4", 
                               "lassomodel2" = "brown2", 
                               "lassomodel3" = "purple3")) +
  theme_minimal() +
  scale_x_discrete(labels = function(x) gsub("`", "", x), expand = expansion(mult = c(0.00001, 0.00001))) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        panel.grid.major.x = element_blank())


# Reorder the Variable column based on lassomodel1's importance
combined_data <- combined_data %>%
  mutate(Variable = factor(Variable, 
                           levels = combined_data %>%
                             filter(Model == "lassomodel1") %>%
                             arrange(desc(Overall)) %>%
                             pull(Variable)))

# Plot with reordered x variables
ggplot(combined_data, aes(x = Variable, y = Overall, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.8) +
  # Add vertical lines
  geom_vline(xintercept = seq(1.5, length(unique(combined_data$Variable)) - 0.5, by = 1), 
             color = "lightgray", linetype = "dashed", size = 0.5) +
  labs(x = "Demographic Variable", y = "Importance", fill = "Model") +
  scale_fill_manual(values = c("lassomodel1" = "chartreuse4", 
                               "lassomodel2" = "brown2", 
                               "lassomodel3" = "purple3")) +
  theme_minimal() +
  scale_x_discrete(labels = function(x) gsub("`", "", x), expand = expansion(mult = c(0.00001, 0.00001))) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())


##Change labels and font: 

ggplot(combined_data, aes(x = Variable, y = Overall, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.8) +
  # Add vertical lines
  geom_vline(xintercept = seq(1.5, length(unique(combined_data$Variable)) - 0.5, by = 1), 
             color = "lightgray", linetype = "dashed", size = 0.5) +
  labs(x = "Demographic Variable", y = "Importance", fill = "Pipeline Network") + # Rename legend title
  scale_fill_manual(
    values = c("lassomodel1" = "chartreuse4", 
               "lassomodel2" = "brown2", 
               "lassomodel3" = "purple3"),
    labels = c("lassomodel1" = "Existing", 
               "lassomodel2" = "Proposed", 
               "lassomodel3" = "2030 Projected") # Rename legend labels
  ) +
  theme_minimal() +
  scale_x_discrete(labels = function(x) gsub("`", "", x), expand = expansion(mult = c(0.00001, 0.00001))) +
  theme(
    axis.text.x = element_text(hjust = 1, size = 12, family = "Arial"),
    axis.text.y = element_text(size = 12, family = "Arial"),
    axis.title.x = element_text(size = 12, family = "Arial"),
    axis.title.y = element_text(size = 12, family = "Arial"),
    #legend.title = element_text(size = 24, family = "Arial"), # Set font for legend title
    legend.text = element_text(size = 12, family = "Arial"),  # Set font for legend labels
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  coord_flip()





ggplot(combined_data, aes(x = Variable, y = Overall, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.8) +
  # Add solid vertical lines
  geom_vline(xintercept = seq(1.5, length(unique(combined_data$Variable)) - 0.5, by = 1), 
             color = "lightgray", linetype = "solid", size = 0.25) +
  labs(x = "Demographic Variable", y = "Importance", fill = "Pipeline Network") + # Rename legend title
  scale_fill_manual(
    values = c("lassomodel1" = "chartreuse4", 
               "lassomodel2" = "brown2", 
               "lassomodel3" = "purple3"),
    labels = c("lassomodel1" = "Existing", 
               "lassomodel2" = "Proposed", 
               "lassomodel3" = "2030 Projected") # Rename legend labels
  ) +
  theme_minimal() +
  scale_x_discrete(labels = function(x) gsub("`", "", x), expand = expansion(mult = c(0.00001, 0.00001))) +
  theme(
    axis.text.x = element_text(hjust = 1, size = 12, family = "Arial"),
    axis.text.y = element_text(size = 12, family = "Arial"),
    axis.title.x = element_text(size = 12, family = "Arial"),
    axis.title.y = element_text(size = 12, family = "Arial"),
    legend.text = element_text(size = 12, family = "Arial"),  # Set font for legend labels
    panel.grid.major.x = element_line(color = "white"), # Keep gray horizontal gridlines i want
    panel.grid.major.y = element_blank(),              # Remove gray horizontal gridlines
    panel.grid.minor = element_blank()                 # Remove minor gridlines
  ) +
  coord_flip()





# Filter data to remove variables where none of the three models have importance >= 20
filtered_data <- combined_data %>%
  group_by(Variable) %>%
  filter(any(Overall >= 10)) %>%
  ungroup()

#Remove other/unspecified race
unique(filtered_data$Variable)

filtered_data <- filtered_data %>%
  filter(Variable != "`Other/Unspecified Race`")

# Plot the filtered data
ggplot(filtered_data, aes(x = Variable, y = Overall, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.8) +
  # Add solid vertical lines
  geom_vline(xintercept = seq(1.5, length(unique(filtered_data$Variable)) - 0.5, by = 1), 
             color = "lightgray", linetype = "solid", size = 0.25) +
  labs(x = "Demographic Variable", y = "Importance", fill = "Pipeline Network") + # Rename legend title
  scale_fill_manual(
    values = c("lassomodel1" = "chartreuse4", 
               "lassomodel2" = "brown2", 
               "lassomodel3" = "purple3"),
    labels = c("lassomodel1" = "Existing", 
               "lassomodel2" = "Proposed", 
               "lassomodel3" = "2030 Projected") # Rename legend labels
  ) +
  theme_minimal() +
  scale_x_discrete(labels = function(x) gsub("`", "", x), expand = expansion(mult = c(0.00001, 0.00001))) +
  theme(
    axis.text.x = element_text(hjust = 1, size = 12, family = "Arial"),
    axis.text.y = element_text(size = 12, family = "Arial"),
    axis.title.x = element_text(size = 12, family = "Arial"),
    axis.title.y = element_text(size = 12, family = "Arial"),
    legend.text = element_text(size = 12, family = "Arial"),  # Set font for legend labels
    panel.grid.major.x = element_line(color = "white"), # Keep gray horizontal gridlines
    panel.grid.major.y = element_blank(),              # Remove gray horizontal gridlines
    panel.grid.minor = element_blank()                 # Remove minor gridlines
  ) +
  coord_flip()





##Change y axis for long variables to be on 2+ lines
ggplot(filtered_data, aes(x = Variable, y = Overall, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.8) +
  # Add solid vertical lines
  geom_vline(xintercept = seq(1.5, length(unique(filtered_data$Variable)) - 0.5, by = 1), 
             color = "lightgray", linetype = "solid", size = 0.25) +
  labs(x = "Demographic Variable", y = "Importance", fill = "Pipeline Network") + # Rename legend title
  scale_fill_manual(
    values = c("lassomodel1" = "chartreuse4", 
               "lassomodel2" = "brown2", 
               "lassomodel3" = "purple3"),
    labels = c("lassomodel1" = "Existing", 
               "lassomodel2" = "Proposed", 
               "lassomodel3" = "2030 Projected") # Rename legend labels
  ) +
  theme_minimal() +
  scale_x_discrete(
    labels = function(x) str_wrap(gsub("`", "", x), width = 20), # Wrap labels longer than 20 characters
    expand = expansion(mult = c(0.00001, 0.00001))
  ) +
  theme(
    axis.text.x = element_text(hjust = 1, size = 12, family = "Arial"),
    axis.text.y = element_text(size = 12, family = "Arial"),
    axis.title.x = element_text(size = 12, family = "Arial"),
    axis.title.y = element_text(size = 12, family = "Arial"),
    legend.text = element_text(size = 12, family = "Arial"),  # Set font for legend labels
    legend.position = c(0.85, 0.9), 
    legend.background = element_rect(fill = "white", color = "black"),
    panel.grid.major.x = element_line(color = "white"), # Keep gray horizontal gridlines
    panel.grid.major.y = element_blank(),              # Remove gray horizontal gridlines
    panel.grid.minor = element_blank()                 # Remove minor gridlines
  ) +
  coord_flip()






#Step 5: Running logistic regressions

##Strube's variables
Projmodel1 <- glm(Trac_Bin ~ `Urban Indicator` + `High Poverty Rate` + `Black Race`  +
                    `White Race` + `Other/Unspecified Race` + `Highest Education Level: None` + `Highest Education Level: High School` +
                    `Highest Education Level: GED/High School` + `Highest Education Level: Some College` + `Highest Education Level: Bachelor` + `Highest Education Level: Master/Professional/Doctorate` + as.factor(COUNTY) + `Population Density`,
                  data = FullDataProj, family = binomial)

summary(Projmodel1)
nobs(Projmodel1)


##Model with all variables without interaction
Projmodel2 <- glm(Trac_Bin ~ `Urban Indicator` + `High Income Household Rates` + `High Poverty Rate` + `Low Poverty Rate` + `Homeownership Rates` + 
                    `Vacancy Rates` + `White Race` + `Black Race` + `Asian Race` + `American Indian Race` + `Native Hawaiian Race` + `Other/Unspecified Race` + `Highest Education Level: None` + `Highest Education Level: High School` +
                    `Highest Education Level: GED/High School` + `Highest Education Level: Some College` + `Highest Education Level: Associate or Bachelor` + `Highest Education Level: Bachelor` + `Highest Education Level: Master/Professional/Doctorate` + as.factor(COUNTY) + `Population Density`,
                  data = FullDataProj, family = binomial)

summary(Projmodel2)
nobs(Projmodel2)




#Propmodel2a <- glm(Trac_Bin ~ `Urban Indicator` + `High Income Household Rates` + `Homeownership Rates` + 
#                 `Vacancy Rates` + `White Race` + `Black Race` + `Asian Race` + `American Indian Race` + `Native Hawaiian Race` + `Other/Unspecified Race` + `Highest Education Level: None` + `Highest Education Level: High School` +
#                `Highest Education Level: GED/High School` + `Highest Education Level: Some College` + `Highest Education Level: Associate or Bachelor` + `Highest Education Level: Bachelor` + `Highest Education Level: Master/Professional/Doctorate` + as.factor(COUNTY) + `Population Density`,
#             data = FullDataProposed, family = binomial)

#summary(Propmodel2a)
#nobs(Propmodel2a)


#Propmodel2b <- glm(Trac_Bin ~ `Urban Indicator` + `High Poverty Rate` + `Homeownership Rates` + 
#               `Vacancy Rates` + `White Race` + `Black Race` + `Asian Race` + `American Indian Race` + `Native Hawaiian Race` + `Other/Unspecified Race` + `Highest Education Level: None` + `Highest Education Level: High School` +
#               `Highest Education Level: GED/High School` + `Highest Education Level: Some College` + `Highest Education Level: Associate or Bachelor` + `Highest Education Level: Bachelor` + `Highest Education Level: Master/Professional/Doctorate` + as.factor(COUNTY) + `Population Density`,
#            data = FullDataProposed, family = binomial)

#summary(Propmodel2b)
#nobs(Propmodel2b)


#Propmodel2c <- glm(Trac_Bin ~ `Urban Indicator` + `Low Poverty Rate` + `Homeownership Rates` + 
#           `Vacancy Rates` + `White Race` + `Black Race` + `Asian Race` + `American Indian Race` + `Native Hawaiian Race` + `Other/Unspecified Race` + `Highest Education Level: None` + `Highest Education Level: High School` +
#            `Highest Education Level: GED/High School` + `Highest Education Level: Some College` + `Highest Education Level: Associate or Bachelor` + `Highest Education Level: Bachelor` + `Highest Education Level: Master/Professional/Doctorate` + as.factor(COUNTY) + `Population Density`,
#        data = FullDataProposed, family = binomial)

#summary(Propmodel2c)
#nobs(Propmodel2c)


#Propmodel2d <- glm(Trac_Bin ~ `Urban Indicator` + `Low Poverty Rate` + `High Poverty Rate` + `Homeownership Rates` + 
#               `Vacancy Rates` + `White Race` + `Black Race` + `Asian Race` + `American Indian Race` + `Native Hawaiian Race` + `Other/Unspecified Race` + `Highest Education Level: None` + `Highest Education Level: High School` +
#              `Highest Education Level: GED/High School` + `Highest Education Level: Some College` + `Highest Education Level: Associate or Bachelor` + `Highest Education Level: Bachelor` + `Highest Education Level: Master/Professional/Doctorate` + as.factor(COUNTY) + `Population Density`,
#           data = FullDataProposed, family = binomial)

#summary(Propmodel2d)
#nobs(Propmodel2d)



##Model with all variables and interaction term with `Urban Indicator` and house ownership
Projmodel3 <- glm(Trac_Bin ~ `Urban Indicator` + `High Income Household Rates` + `High Poverty Rate` + `Low Poverty Rate` + `Homeownership Rates` + 
                    `Vacancy Rates` + `White Race` + `Black Race` + `American Indian Race` + `Asian Race` +
                    `Native Hawaiian Race` + `Other/Unspecified Race` + `Highest Education Level: None` + `Highest Education Level: High School` +
                    `Highest Education Level: GED/High School` + `Highest Education Level: Some College` + `Highest Education Level: Associate or Bachelor` + `Highest Education Level: Bachelor` + `Highest Education Level: Master/Professional/Doctorate` + `Urban Indicator`:`Homeownership Rates` + as.factor(COUNTY) + `Population Density`,
                  data = FullDataProj, family = binomial)

summary(Projmodel3)
nobs(Projmodel3)





##Model with accident variables
#Propmodel4 <- glm(Trac_Bin ~ Prev_Acc + AccTotal + as.factor(COUNTY) + `Population Density`,
#data = FullDataProposed, family = binomial)

#summary(Propmodel4)
#nobs(Propmodel4)



#Propmodel5 <- glm(Trac_Bin ~ `Urban Indicator` + `High Income Household Rates` + `High Poverty Rate` + `Low Poverty Rate` + `Homeownership Rates` + 
#  `Vacancy Rates` + `White Race` + `Black Race` + `Native Hawaiian Race`+ `Other/Unspecified Race` + `Highest Education Level: None` + `Highest Education Level: High School` +
#  `Highest Education Level: GED/High School` + `Highest Education Level: Some College` + `Highest Education Level: Bachelor` + `Highest Education Level: Master/Professional/Doctorate` + Prev_Acc + AccTotal + as.factor(COUNTY) + `Population Density`,
# data = FullDataProposed, family = binomial)

#summary(Propmodel5)
#nobs(Propmodel5)


#Propmodel6 <- glm(Trac_Bin ~ DaysSinceAcc + as.factor(COUNTY) + `Population Density`,
#                  data = FullDataProposed, family = binomial)

#summary(Propmodel6)
#nobs(Propmodel6)

#`Urban Indicator` + `High Income Household Rates` + `High Poverty Rate` + `Low Poverty Rate` + `Homeownership Rates` + 
#`Vacancy Rates` + `White Race` + `Black Race` + `Native Hawaiian Race`+ `Other/Unspecified Race` + `Highest Education Level: None` + `Highest Education Level: High School` +
# `Highest Education Level: GED/High School` + `Highest Education Level: Some College` + `Highest Education Level: Bachelor` + `Highest Education Level: Master/Professional/Doctorate` + Prev_Acc + AccTotal +



#Step 5.1: Running logistic regressions - INC

##Strube's variables
incProjmodel1 <- glm(Trac_Bin ~ `Urban Indicator` + `High Poverty Rate` + `Black Race`  +
                       `White Race` + `Other/Unspecified Race` + `Highest Education Level: None` + `Highest Education Level: High School` +
                       `Highest Education Level: GED/High School` + `Highest Education Level: Some College` + `Highest Education Level: Bachelor` + `Highest Education Level: Master/Professional/Doctorate` + as.factor(COUNTY) + `Population Density`,
                     data = FullDataProjInc, family = binomial)

summary(incProjmodel1)
nobs(incProjmodel1)


##ONE USED IN STUDY
##Model with all variables without interaction
incProjmodel2 <- glm(Trac_Bin ~ `Urban Indicator` + `High Income Household Rates` + `High Poverty Rate` + `Low Poverty Rate` + `Homeownership Rates` + 
                       `Vacancy Rates` + `Black Race` + `Asian Race` + `American Indian Race` + `Native Hawaiian Race` + 
                       `Highest Education Level: None` + `Highest Education Level: High School` + `Highest Education Level: Some College` + `Highest Education Level: Bachelor` + `Population Density` + as.factor(COUNTY), data = FullDataProjInc, family = binomial)

summary(incProjmodel2)
nobs(incProjmodel2)


##MARGINAL EFFECT

# Rename variables in the dataset
FullDataProjInc_ME <- FullDataProjInc
colnames(FullDataProjInc_ME) <- gsub("[:/ ]", "_", colnames(FullDataProjInc))


FullDataProjInc_ME$COUNTY <- as.factor(FullDataProjInc_ME$COUNTY)

# Refit the model with updated variable names
projincmodel2_ME <- glm(Trac_Bin ~ Urban_Indicator + 
                          High_Income_Household_Rates + 
                          High_Poverty_Rate + 
                          Low_Poverty_Rate + 
                          Homeownership_Rates + 
                          Vacancy_Rates + 
                          Black_Race + 
                          Asian_Race + 
                          American_Indian_Race + 
                          Native_Hawaiian_Race + 
                          Highest_Education_Level__None + 
                          Highest_Education_Level__High_School + 
                          Highest_Education_Level__Some_College + 
                          Highest_Education_Level__Bachelor + 
                          Population_Density + COUNTY,
                        data = FullDataProjInc_ME, family = binomial)



##MARGINAL EFFECT


projavemargeffect <- margins(projincmodel2_ME, variables = c("Urban_Indicator", 
                                                             "High_Income_Household_Rates", 
                                                             "High_Poverty_Rate", 
                                                             "Low_Poverty_Rate", 
                                                             "Homeownership_Rates", 
                                                             "Vacancy_Rates", 
                                                             "Black_Race", 
                                                             "Asian_Race", 
                                                             "American_Indian_Race", 
                                                             "Native_Hawaiian_Race", 
                                                             "Highest_Education_Level__None", 
                                                             "Highest_Education_Level__High_School", 
                                                             "Highest_Education_Level__Some_College", 
                                                             "Highest_Education_Level__Bachelor", 
                                                             "Population_Density"))
summary(projavemargeffect)


# Convert the marginal effects to a data frame
proj_marginal_effects_df <- as.data.frame(projavemargeffect)

# Keep only numeric columns
proj_numeric_marginal_effects <- proj_marginal_effects_df[sapply(proj_marginal_effects_df, is.numeric)]

# Select the specific variables you're interested in
proj_selected_vars <- c("dydx_High_Income_Household_Rates", "dydx_Urban_Indicator", 
                        "dydx_Population_Density", "dydx_Highest_Education_Level__High_School", 
                        "dydx_Highest_Education_Level__Some_College", "dydx_Black_Race")

# Calculate the standard deviation for these selected variables
proj_std_dev_selected <- sapply(proj_numeric_marginal_effects[proj_selected_vars], sd, na.rm = TRUE)

# Print standard deviations of the selected marginal effects
print(proj_std_dev_selected)




##Model with all variables and interaction term with `Urban Indicator` and house ownership
incProjmodel3 <- glm(Trac_Bin ~ `Urban Indicator` + `High Income Household Rates` + `High Poverty Rate` + `Low Poverty Rate` + `Homeownership Rates` + 
                       `Vacancy Rates` + `White Race` + `Black Race` + `American Indian Race` + `Asian Race` +
                       `Native Hawaiian Race` + `Other/Unspecified Race` + `Highest Education Level: None` + `Highest Education Level: High School` +
                       `Highest Education Level: GED/High School` + `Highest Education Level: Some College` + `Highest Education Level: Associate or Bachelor` + `Highest Education Level: Bachelor` + `Highest Education Level: Master/Professional/Doctorate` + `Urban Indicator`:`Homeownership Rates` + as.factor(COUNTY) + `Population Density`,
                     data = FullDataProjInc, family = binomial)

summary(incProjmodel3)
nobs(incProjmodel3)







##GRAPHS
###Coefficient plot of the logistic regression model.


#Scale around zero
Projcoef_table <- summary(incProjmodel2)$coefficients
Projcoef_df <- data.frame(
  variable = rownames(Projcoef_table),
  coefficient = Projcoef_table[, 1],
  std_error = Projcoef_table[, 2]
)

# Filter out the county fixed effects
Projcoef_df <- Projcoef_df[!grepl("COUNTY", Projcoef_df$variable), ]

# Determine the x-axis limits based on the coefficients and standard errors
x_min <- min(Projcoef_df$coefficient - 2 * Projcoef_df$std_error)
x_max <- max(Projcoef_df$coefficient + 2 * Projcoef_df$std_error)
x_range <- abs(x_max - x_min)
x_min <- x_min - 0.2 * x_range
x_max <- x_max + 0.2 * x_range

# Create the coefficient plot with 95% confidence intervals
ggplot(Projcoef_df, aes(x = coefficient, y = reorder(variable, coefficient))) +
  geom_point() +
  geom_errorbarh(aes(xmin = coefficient - 1.96 * std_error, xmax = coefficient + 1.96 * std_error), height = 0.2) +
  labs(x = "Coefficient", y = "Variable") +
  scale_x_continuous(limits = c(x_min, x_max)) +
  theme_minimal()


# Filter out variables other than the specified ones
Projcoef_df <- Projcoef_df[Projcoef_df$variable %in% c("`Urban Indicator`", "`Population Density`", "`Black Race`", "`High Income Household Rates`", "`Highest Education Level: Some College`", "`Highest Education Level: High School`"), ]

# Determine the x-axis limits based on the coefficients and standard errors
x_min <- min(Projcoef_df$coefficient - 2 * Projcoef_df$std_error)
x_max <- max(Projcoef_df$coefficient + 2 * Projcoef_df$std_error)
x_range <- abs(x_max - x_min)
x_min <- x_min - 0.2 * x_range
x_max <- x_max + 0.2 * x_range

# Create the coefficient plot with 95% confidence intervals
ggplot(Projcoef_df, aes(x = coefficient, y = reorder(variable, coefficient))) +
  geom_point() +
  geom_errorbarh(aes(xmin = coefficient - 1.96 * std_error, xmax = coefficient + 1.96 * std_error), height = 0.2) +
  labs(x = "Coefficient", y = "Variable") +
  scale_x_continuous(limits = c(x_min, x_max)) +
  theme_minimal()

Projcoef_df$variable <- stringr::str_wrap(Projcoef_df$variable, width = 14)

# Now plot
ggplot(Projcoef_df, aes(x = coefficient, y = reorder(variable, coefficient))) +
  geom_point(size=1.8, color="purple3") +
  geom_errorbarh(aes(xmin = coefficient - 1.96 * std_error, xmax = coefficient + 1.96 * std_error), height = 0.2, size=0.75, color="purple3") +
  labs(x = "Coefficient", y = "Variable") +
  scale_x_continuous(limits = c(x_min, x_max)) +
  theme_minimal() +
  theme(
    panel.border = element_rect(fill = NA, color = "gray", size = 1),
    panel.grid.major = element_line(color = "lightgray", size = 0.25),
    panel.grid.minor = element_line(color = "lightgray", size = 0.25),
    #panel.grid.major.x = element_blank(), # Remove vertical major gridlines
    panel.grid.minor.x = element_blank(), # Remove vertical minor gridlines
    axis.ticks.length = unit(-0.25, "cm") # Adjust the length of axis ticks
  ) +
  scale_y_discrete(labels = function(labels) str_replace_all(labels, "`", ""))





###TRY to plot coefficient plot with MARGINAL EFFECT
# Extract marginal effects as a dataframe
projmarg_df <- summary(projavemargeffect)

# Rename columns for consistency
projmarg_df <- data.frame(
  variable = projmarg_df$factor,  # Variable names
  coefficient = projmarg_df$AME*100,  # Average Marginal Effect
  std_error = projmarg_df$SE*100 # Standard Error
)

projmarg_df$variable <- str_replace_all(projmarg_df$variable, "_", " ")
projmarg_df$variable <- str_replace(projmarg_df$variable, "Highest Education Level  High School", "Highest Education Level: High School")
projmarg_df$variable <- str_replace(projmarg_df$variable, "Highest Education Level  Some College", "Highest Education Level: Some College")

# Filter for the selected variables
selected_vars <- c("Black Race", "Highest Education Level: High School",  "Highest Education Level: Some College", "High Income Household Rates", "Population Density", "Urban Indicator")
projmarg_df <- projmarg_df[projmarg_df$variable %in% selected_vars, ]

# Determine the x-axis limits based on marginal effects and standard errors
x_min <- min(projmarg_df$coefficient - 2 * projmarg_df$std_error)
x_max <- max(projmarg_df$coefficient + 2 * projmarg_df$std_error)
x_range <- abs(x_max - x_min)
x_min <- x_min - 0.2 * x_range
x_max <- x_max + 0.2 * x_range

projmarg_df$variable <- stringr::str_wrap(projmarg_df$variable, width = 14)

# Create the filtered marginal effects plot
ggplot(projmarg_df, aes(x = coefficient, y = reorder(variable, coefficient))) +
  geom_point(size = 1.8, color = "purple3") +
  geom_errorbarh(aes(xmin = coefficient - 1.96 * std_error, xmax = coefficient + 1.96 * std_error), 
                 height = 0.2, size = 0.75, color = "purple3") +
  labs(x = "Marginal Effect", y = "Variable") +
  scale_x_continuous(limits = c(x_min, x_max)) +
  theme_minimal() +
  theme(
    panel.border = element_rect(fill = NA, color = "gray", size = 1),
    panel.grid.major = element_line(color = "lightgray", size = 0.25),
    panel.grid.minor = element_line(color = "lightgray", size = 0.25),
    panel.grid.minor.x = element_blank(), 
    axis.ticks.length = unit(-0.25, "cm"),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(margin = margin(t = 14), size=14),
    axis.title.y = element_text(size=14),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  scale_y_discrete(labels = function(labels) str_replace_all(labels, "`", ""))


#save high resolution
ggsave("C:/Users/julia/OneDrive - University of Virginia/PhD Stuff/Research/Pipeline/Paper Revisions/Final Submissions/projected_marginal_effects_plot.png", 
       plot = last_plot(), 
       width = 9, height = 6, units = "in", 
       dpi = 300)



#EXPORT
FullDataProjInc$predictions <- predict(incProjmodel2, type = "response")

# Specify the file path where you want to save the Excel file
file_path <- "C:/Users/julia/OneDrive - University of Virginia/PhD Stuff/Research/Pipeline/FullDataProjInc.xlsx"

# Export the dataset to an Excel file
write.xlsx(FullDataProjInc, file_path, rowNames = TRUE)

# Provide a message indicating the export is complete
cat("Dataset exported to", file_path, "\n")



####Combine 3 coefficient plots

# First, combine all the data into one dataframe with an additional 'Model' column
combined_coef_df <- bind_rows(
  mutate(existcoef_df, Model = "Existing"),
  mutate(Propcoef_df, Model = "Proposed"),
  mutate(Projcoef_df, Model = "2030 Projected")
)

# Now, create the plot with all three datasets combined
ggplot(combined_coef_df, aes(x = coefficient, y = reorder(variable, coefficient), color = Model)) +
  geom_point(size = 1.8) +
  geom_errorbarh(aes(xmin = coefficient - 1.96 * std_error, xmax = coefficient + 1.96 * std_error), 
                 height = 0.2, size = 0.75) +
  labs(x = "Coefficient", y = "Variable", title = "Model Coefficients") +
  scale_color_manual(values = c("Existing" = "chartreuse4", 
                                "Proposed" = "brown2", 
                                "2030 Projected" = "purple3")) +
  scale_x_continuous(limits = c(x_min, x_max)) +
  theme_minimal() +
  theme(
    panel.border = element_rect(fill = NA, color = "gray", size = 1),
    panel.grid.major = element_line(color = "lightgray", size = 0.25),
    panel.grid.minor.x = element_blank(),
    axis.ticks.length = unit(-0.25, "cm"),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  ) +
  scale_y_discrete(labels = function(labels) str_replace_all(labels, "`", ""))







# First, make sure 'urban' coefficient is in 'existcoef_df' or another relevant dataset
# Ensure the 'urban' coefficient from lassomodel1 is included in 'existcoef_df'
# Then combine all datasets into one
combined_coef_df <- bind_rows(
  mutate(existcoef_df, Model = "Existing"),  # Include all coefficients from lassomodel1
  mutate(Propcoef_df, Model = "Proposed"),
  mutate(Projcoef_df, Model = "2030 Projected")
)

# Create the plot with all coefficients, including 'urban' from lassomodel1
ggplot(combined_coef_df, aes(x = coefficient, y = reorder(variable, coefficient), color = Model)) +
  geom_point(size = 1.8) +
  geom_errorbarh(aes(xmin = coefficient - 1.96 * std_error, xmax = coefficient + 1.96 * std_error), 
                 height = 0.2, size = 0.75) +
  labs(x = "Coefficient", y = "Variable", title = "Model Coefficients (Including Urban from lassomodel1)") +
  scale_color_manual(values = c("Existing" = "chartreuse4", 
                                "Proposed" = "brown2", 
                                "2030 Projected" = "purple3")) +
  scale_x_continuous(limits = c(x_min, x_max)) +
  theme_minimal() +
  theme(
    panel.border = element_rect(fill = NA, color = "gray", size = 1),
    panel.grid.major = element_line(color = "lightgray", size = 0.25),
    panel.grid.minor.x = element_blank(),
    axis.ticks.length = unit(-0.25, "cm"),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  ) +
  scale_y_discrete(labels = function(labels) str_replace_all(labels, "`", ""))



# Remove rows with missing values in important columns
combined_coef_df <- combined_coef_df %>%
  filter(!is.na(coefficient) & !is.na(std_error) & !is.na(variable))

# Adjust x-axis limits if necessary
x_min <- min(combined_coef_df$coefficient, na.rm = TRUE) - 1
x_max <- max(combined_coef_df$coefficient, na.rm = TRUE) + 1

# Alternatively, you could remove coefficients outside the limits
combined_coef_df <- combined_coef_df %>%
  filter(coefficient >= x_min & coefficient <= x_max)

ggplot(combined_coef_df, aes(x = coefficient, y = reorder(variable, coefficient), color = Model)) +
  geom_point(size = 1.8) +
  geom_errorbarh(aes(xmin = coefficient - 1.96 * std_error, xmax = coefficient + 1.96 * std_error), 
                 height = 0.2, size = 0.75) +
  labs(x = "Coefficient", y = "Variable", title = "Model Coefficients (Including Urban from lassomodel1)") +
  scale_color_manual(values = c("Existing" = "chartreuse4", 
                                "Proposed" = "brown2", 
                                "2030 Projected" = "purple3")) +
  scale_x_continuous(limits = c(x_min, x_max)) +
  theme_minimal() +
  theme(
    panel.border = element_rect(fill = NA, color = "gray", size = 1),
    panel.grid.major = element_line(color = "lightgray", size = 0.25),
    panel.grid.minor.x = element_blank(),
    axis.ticks.length = unit(-0.25, "cm"),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  ) +
  scale_y_discrete(labels = function(labels) str_replace_all(labels, "`", ""))






# Remove rows where the 'variable' is "Other/Unspecified Race"

unique(combined_coef_df$variable)

combined_coef_df <- combined_coef_df %>%
  filter(variable != "`Other/Unspecified\nRace`")

combined_coef_df <- combined_coef_df %>%
  filter(variable != "`Urban\nIndicator`")

# Plot again
ggplot(combined_coef_df, aes(x = coefficient, y = reorder(variable, coefficient), color = Model)) +
  geom_point(size = 1.8) +
  geom_errorbarh(aes(xmin = coefficient - 1.96 * std_error, xmax = coefficient + 1.96 * std_error), 
                 height = 0.2, size = 0.75) +
  labs(x = "Coefficient", y = "Variable", title = "Model Coefficients (Including Urban from lassomodel1)") +
  scale_color_manual(values = c("Existing" = "chartreuse4", 
                                "Proposed" = "brown2", 
                                "2030 Projected" = "purple3")) +
  scale_x_continuous(limits = c(x_min, x_max)) +
  theme_minimal() +
  theme(
    panel.border = element_rect(fill = NA, color = "gray", size = 1),
    panel.grid.major = element_line(color = "lightgray", size = 0.25),
    panel.grid.minor.x = element_blank(),
    axis.ticks.length = unit(-0.25, "cm"),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  ) +
  scale_y_discrete(labels = function(labels) str_replace_all(labels, "`", ""))






###Final plot

ggplot(combined_coef_df, aes(x = coefficient, y = reorder(variable, coefficient), color = Model)) +
  geom_point(size = 1.8, position = position_dodge(width = 0.5)) +  # Dodging points to avoid overlap
  geom_errorbarh(aes(xmin = coefficient - 1.96 * std_error, xmax = coefficient + 1.96 * std_error), 
                 height = 0.2, size = 0.75, position = position_dodge(width = 0.5)) +
  labs(x = "Coefficient", y = "Variable", color = "Pipeline Network") +
  scale_color_manual(values = c("Existing" = "chartreuse4", 
                                "Proposed" = "brown2", 
                                "2030 Projected" = "purple3")) +
  scale_x_continuous(limits = c(-0.2, 0.2)) +
  theme_minimal() +  # Set base font to Arial and size to 12
  theme(
    panel.border = element_rect(fill = NA, color = "gray", size = 1),
    panel.grid.major = element_line(color = "lightgray", size = 0.25),
    panel.grid.minor.x = element_blank(),
    axis.ticks.length = unit(-0.25, "cm"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 12, hjust = 0.5)
  ) +
  scale_y_discrete(labels = function(labels) str_replace_all(labels, "`", "")) +
  scale_color_manual(
    values = c("Existing" = "chartreuse4", 
               "Proposed" = "brown2", 
               "2030 Projected" = "purple3"),
    breaks = c("Existing", "Proposed", "2030 Projected")  # Explicitly specify order here
  )



##Better font:

ggplot(combined_coef_df, aes(x = coefficient, y = reorder(variable, coefficient), color = Model)) +
  geom_point(size = 1.8, position = position_dodge(width = 0.5)) +  # Dodging points to avoid overlap
  geom_errorbarh(aes(xmin = coefficient - 1.96 * std_error, xmax = coefficient + 1.96 * std_error), 
                 height = 0.2, size = 0.75, position = position_dodge(width = 0.5)) +
  labs(x = "Coefficient", y = "Variable", color = "Pipeline Network") +
  scale_color_manual(values = c("Existing" = "chartreuse4", 
                                "Proposed" = "brown2", 
                                "2030 Projected" = "purple3")) +
  scale_x_continuous(limits = c(-0.2, 0.2)) +  # Adjust x-axis limits
  theme_minimal(base_family = "Arial", base_size = 14) +  # Set font to Arial and base size to 14
  theme(
    panel.border = element_rect(fill = NA, color = "gray", size = 1),
    panel.grid.major = element_line(color = "lightgray", size = 0.25),
    panel.grid.minor.x = element_blank(),
    axis.ticks.length = unit(-0.25, "cm"),
    legend.title = element_text(size = 16),  # Larger legend title
    legend.text = element_text(size = 14),  # Larger legend text
    legend.position = c(0.85, 0.15), 
    legend.background = element_rect(fill = "white", color = "black"),
    axis.text = element_text(size = 14),    # Larger axis text
    axis.title = element_text(size = 16),   # Larger axis titles
    plot.title = element_text(size = 16, hjust = 0.5)  # Larger centered title
  ) +
  scale_y_discrete(labels = function(labels) str_replace_all(labels, "`", "")) +
  scale_color_manual(
    values = c("Existing" = "chartreuse4", 
               "Proposed" = "brown2", 
               "2030 Projected" = "purple3"),
    breaks = c("Existing", "Proposed", "2030 Projected")  # Explicitly specify order
  )
