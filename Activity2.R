#Activity 2 and Homework 2
#####Activity 2------

#prompt 1 and 
# install.packages(c("dplyr","lubridate"))
library(dplyr)
library(lubridate)

StreamH <- read.csv("/cloud/project/activtiy02/stream_gauge.csv")
siteInfo <- read.csv("/cloud/project/activtiy02/site_info.csv")


floods <- full_join(StreamH, 
                    siteInfo,
                    by="siteID")
#the type of join matters because depending on how you use your join it will dictate what decisions are in one table and not in anohter table. The SiteID appears in both tables so the type of join matters less.  
#designating one or more columns that have matching values in the tables
head(floods)

#Prompt 2
floods$dateF <- ymd_hm(floods$datetime, tz="America/New_York")

#prompt 3
#What was the earliest date that each river reached the flood stage?
earliestdate <- floods %>%
  group_by(names) %>%
  filter(gheight.ft >= flood.ft) %>%
  summarise(min.date = min(dateF))

  




######group presentation (select function)-----
date <- select(floods,"names", "gheight.ft", "flood.ft")

VariableA <- select(floods, names, gheight.ft ,datetime) #Select the variables names, gheight.ft, and datetime from VariableA
VariableB <-  select(floods, gheight.ft:dateF) #Select all variables from gheight.ft to dateF from VariableB
VariableC <-  select(floods, -datetime, -siteID) #Exclude dateime and siteID from the dataset from VariableC	
VariableD <-  select(floods, gheight.ft:dateF, -moderate.ft) #select all variables from gheight.ft to dateF but excluse moderate.ft from VariableD
VariableE <- select(floods, contains("e"), -agency) #select all variables containing the letter e but exclude agency from VariableE
VariableF <- select(floods,(!(gheight.ft:dateF))) #select all variables NOT included in gheight.ft to dateF


######group presentation (hist function)-----
fisheating_creek = floods[floods$names == "FISHEATING CREEK AT PALMDALE",]
hist(fisheating_creek$gheight.ft, 
     main = "Fisheating Creek Water Height", 
     xlab = "Water Height (ft)", 
     col = "red", 
     border = "black")

unique(floods$names)

Santa.Fe.River <- floods[4266:6473,]
hist(Santa.Fe.River$gheight.ft, 
     main = "Santa Fe River", 
     xlab = "Height (ft)", col = "green", 
     border = "blue", 
     xlim = c(4,10), 
     ylim = c(0,200))


######group presentation (mutate function)-----
#mutate(dataframe, newvariable = existingvariable*2)

floods_mutated <- mutate (floods,
                          stage_meters = gheight.ft*0.3048, 
                          percent_flood = (gheight.ft/major.ft)*100)

######group presentation (ifelse function)-----
floods$test <- ifelse(is.na(floods$gheight.ft), 1,0)






######Homework 2 Part 2-----

#Question 1
fisheating_creek = floods[floods$names == "FISHEATING CREEK AT PALMDALE",]
plot(fisheating_creek$dateF, fisheating_creek$gheight.ft,
     main = "Fisheating Creek Stream Stage", 
     xlab = "Date",
     ylab = "Stage", 
     col = "blue")

peace_river = floods[floods$names == "PEACE RIVER AT US 17 AT ZOLFO SPRINGS",]
plot(peace_river$dateF, peace_river$gheight.ft,
     main = "Peace River Stream Stage", 
     xlab = "Date", 
     ylab = "Stage", 
     col = "khaki")

santafe_river=floods[floods$names == "SANTA FE RIVER NEAR FORT WHITE",]
plot(santafe_river$dateF, santafe_river$gheight.ft,
     main = "Santa Fe River Stream Stangle",
     xlab = "Date",
     ylab = "Stage", 
     col = "green")

#Question 2
#What was the earliest date of occurrence for each flood category in each river?
floodstage <- floods %>%
  group_by(names) %>%
  filter(gheight.ft >= flood.ft) %>%
  summarise(min.date = min(dateF))


Moderatestage <- floods %>%
  group_by(names) %>%
  filter(gheight.ft >= moderate.ft) %>%
  summarise(min.date = min(dateF))

Majorstage <- floods %>%
  group_by(names) %>%
  filter(gheight.ft >= major.ft) %>%
  summarise(min.date = min(dateF))

#Question 3

MaxExceedence <- floods %>%
  group_by(names) %>%
  filter(gheight.ft >= major.ft) %>%
  summarise(major.flood.level = mean(major.ft), max.height = max(gheight.ft))



