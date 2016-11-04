# CKME 136 - Capstone Project for Data Analytics
# ID   : 
# Name : Gagan Minhas
# ==============================================================

# Load Libraries
# ==============================================================
library(magrittr)
library(dplyr)
library(rpart)
library(tree)
library(party)
# Load Death Records Data 
# ==============================================================
File <- 'C:/Users/Gagan/Desktop/DeathRecords.csv'
DeathRecordsOrig <- read.csv(File, header = TRUE)

# Acquire Basic Information from Data
# ==============================================================
numAttributes <- ncol(DeathRecords)
numObservations <- nrow(DeathRecords)

# Clean the Data
# 1. Remove non-essential attributes
# 2a. Combine columns that distribute same data on unique scales
# 2b. Remove Attributes that are repeated under different scales
# 4. Remove Death Records of non-adults
# 5. Convert attribute to correct data type
# ==============================================================

DeathRecords$Education2003Revision[DeathRecords$Education2003Revision == 0 & 
                                     DeathRecords$Education1989Revision %in% 
                                     c(0,1,2,3,4,5,6,7,8)] <- 1
DeathRecords$Education2003Revision[DeathRecords$Education2003Revision == 0 & 
                                     DeathRecords$Education1989Revision %in% 
                                     c(9,10,11)] <- 2
DeathRecords$Education2003Revision[DeathRecords$Education2003Revision == 0 & 
                                     DeathRecords$Education1989Revision == 12] <- 3
DeathRecords$Education2003Revision[DeathRecords$Education2003Revision == 0 & 
                                     DeathRecords$Education1989Revision %in% 
                                     c(13,14,15)] <- 4
DeathRecords$Education2003Revision[DeathRecords$Education2003Revision == 0 & 
                                     DeathRecords$Education1989Revision == 16] <- 6
DeathRecords$Education2003Revision[DeathRecords$Education2003Revision == 0 & 
                                     DeathRecords$Education1989Revision == 17] <- 7
DeathRecords$Education2003Revision[DeathRecords$Education2003Revision == 0 & 
                                     DeathRecords$Education1989Revision == 99] <- 9

dropSameAttributes <- c("Education1989Revision","EducationReportingFlag","AgeType",
                        "Age","AgeSubstitutionFlag","AgeRecode52","AgeRecode12",
                        "CauseRecode358","CauseRecode113","Icd10Code","MannerOfDeath",
                        "RaceImputationFlag","RaceRecode3","RaceRecode5",
                        "HispanicOrigin")

dropNonEssAttributes <- c("Id","CurrentDataYear","Autopsy","NumberOfEntityAxisConditions",
                          "NumberOfRecordAxisConditions","MethodOfDisposition")

dropNonAdultAttributes <- c("InfantAgeRecode22","InfantCauseRecode10","InfantCauseRecode130")
dropAttributes <- c(dropNonEssAttributes,dropSameAttributes,dropNonAdultAttributes)
DeathRecords <- DeathRecords[,!(names(DeathRecords) %in% dropAttributes)]

DeathRecords <- DeathRecords[DeathRecords$AgeRecode27>=10,]
DeathRecords <- DeathRecords[DeathRecords$CauseRecode39 !=35,]
numAttributesClean <- ncol(DeathRecords)
numObservationsClean <- nrow(DeathRecords)

DeathRecords$ResidentStatus <- as.factor(DeathRecords$ResidentStatus)
levels(DeathRecords$ResidentStatus) <- c("Residents","Intrastate Residents","Interstate Residents",
                                         "Foreign Residents")
DeathRecords$Education2003Revision <- as.factor(DeathRecords$Education2003Revision)
levels(DeathRecords$Education2003Revision) <- c("8th Grade or less","9-12 Grade, no diploma","HS Grad or GED",
                                                "Some College","Associate Degree","Bachelor Degree","Master Degree",
                                                "Doctorate/Professional Degree","Unknown")
DeathRecords$MonthOfDeath <- as.factor(DeathRecords$MonthOfDeath)
levels(DeathRecords$MonthOfDeath) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
DeathRecords$PlaceOfDeathAndDecedentsStatus <- as.factor(DeathRecords$PlaceOfDeathAndDecedentsStatus)
levels(DeathRecords$PlaceOfDeathAndDecedentsStatus) <- c("Hospital - Inpatient","Hospital - Outpatient","Hospital - DoA","Decendent Home",
                          "Hospice Facility","Nursing Home","Other","Place of Death Unknown")
DeathRecords$DayOfWeekOfDeath <- as.factor(DeathRecords$DayOfWeekOfDeath)
levels(DeathRecords$DayOfWeekOfDeath) <- c("Sun","Mon","Tue","Wed","Thu","Fri","Sat","Unknown")
DeathRecords$ActivityCode <- as.factor(DeathRecords$ActivityCode)
levels(DeathRecords$ActivityCode) <- c("Sports","Leisure","Working","Other Work","Resting","Other","Unspecified",
                                       "Not Applicable")
DeathRecords$PlaceOfInjury <- as.factor(DeathRecords$PlaceOfInjury)
levels(DeathRecords$PlaceOfInjury) <- c("Home","Residential","School","Sports","Street","Trade","Construction","Farm",
                                        "Other Specified","Unspecified","Cause of Death not W00-Y34")
DeathRecords$CauseRecode39 <- as.factor(DeathRecords$CauseRecode39)
levels(DeathRecords$CauseRecode39) <- c("TB","Syph","HIV","MNStomach","MNColon","MNPancreas",
                                       "MNTrachea","MNBreast","MNcervix","MNProstate","MNUrine",
                                       "NHLymphoma","Leuk","OMN","Diabetes","Alz","HTHD","IHD",
                                       "OHD","HTRD","CVD","Atherosc","OCS","InfPneu","CLRD","PepUlcer",
                                       "LD","Neph","Pregna","Perinatal","Congenital","Abnormal","Other",
                                       "Vehicle","Accident","Suicide","Homicide","OEC")
DeathRecords$Race <- as.factor(DeathRecords$Race)
levels(DeathRecords$Race) <- c("Other","White","Black","American Indian","Chinese","Japanese","Hawaiian","Filipino","Other Asian",
                               "Asian Indian","Korean","Samoan","Vietnamese","Guamanian","Other Asian*","Combined Asian")
DeathRecords$BridgedRaceFlag <- as.factor(DeathRecords$BridgedRaceFlag)
levels(DeathRecords$BridgedRaceFlag) <- c("Not Bridged","Is Bridged")
DeathRecords$HispanicOriginRaceRecode <- as.factor(DeathRecords$HispanicOriginRaceRecode)
levels(DeathRecords$HispanicOriginRaceRecode) <- c("Mexican","Puerto Rican","Cuban","Central/South American","Other Hispanic",
                                                   "Non-hispanic white","Non-hispanic black","Non-hispanic other","Unknown")


# Explore Trends in the Data
# ==============================================================

SummaryStatistics <- summary(DeathRecords)
attach(DeathRecords)
hist(AgeRecode27)
hist(ResidentStatus)
hist(Education2003Revision)
hist(MonthOfDeath)
barplot(table(Sex), xlab = "Sex", ylab = "Number of Deaths", main = "Deaths by Sex")
barplot(table(CauseRecode39[Sex == "M"]))
barplot(table(CauseRecode39))

CauseCount <- group_by(DeathRecords, CauseRecode39) %>% summarize(count = n()) %>% arrange(desc(count))
barplot(CauseCount$count, names.arg = CauseCount$CauseRecode39, las = 2)
CauseCountF <- group_by(DeathRecords[DeathRecords$Sex == "F",], CauseRecode39) %>% summarize(count = n()) %>% arrange(desc(count))
barplot(CauseCountF$count, names.arg = CauseCountF$CauseRecode39, las = 2)
CauseCountM <- group_by(DeathRecords[DeathRecords$Sex == "M",], CauseRecode39) %>% summarize(count = n()) %>% arrange(desc(count))
barplot(CauseCountM$count, names.arg = CauseCountM$CauseRecode39, las = 2)
CauseCountFR <- group_by(DeathRecords[DeathRecords$ResidentStatus == "Foreign Residents",], CauseRecode39) %>% summarize(count = n()) %>% arrange(desc(count))
barplot(CauseCountFR$count, names.arg = CauseCountFR$CauseRecode39, las = 2)



# Build a simple Decision Tree Model
# ====================================================================

# rpart
set.seed(1)
train <- sample(1:nrow(DeathRecords), 0.7 * nrow(DeathRecords))

DeathRecords %>%
  filter(ID %in% sample(unique(ID), ceiling(0.7*length(unique(ID)))))


DeathRecordTree <- rpart(CauseRecode39 ~ PlaceOfInjury+ActivityCode+InjuryAtWork+PlaceOfDeathAndDecedentsStatus+
                           AgeRecode27+ResidentStatus, data = DeathRecords[train,], method = "class")
DeathRecordPred <- predict(DeathRecordTree, DeathRecords[-train,], type = "class")
table(DeathRecordPred, DeathRecords[-train,]$CauseRecode39)

# tree - limits categorical variable to 32 levels
tree.DeathRecords <- tree(CauseRecode39 ~ AgeRecode27 + ResidentStatus + Race + MaritalStatus + ActivityCode + MonthOfDeath,
                          data = DeathRecords[train,])

# party - takes a long time
TreeParty <- ctree(CauseRecode39 ~ ., data = DeathRecords[train,])
