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
levels(DeathRecords$Education2003Revision) <- c("<=8th Grade","9-12 Grade","HS Grad/GED",
                                                "Some College","Associate Degree","Bachelor Degree","Master Degree",
                                                "Doctorate Degree","Unknown")
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
levels(DeathRecords$Race) <- c("Other","White","Black","US Indian","Chinese","Japanese","Hawaiian","Filipino","Other Asian",
                               "Asian Indian","Korean","Samoan","Vietnamese","Guamanian","Other Asian*","All Asian")
DeathRecords$BridgedRaceFlag <- as.factor(DeathRecords$BridgedRaceFlag)
levels(DeathRecords$BridgedRaceFlag) <- c("Not Bridged","Is Bridged")
DeathRecords$HispanicOriginRaceRecode <- as.factor(DeathRecords$HispanicOriginRaceRecode)
levels(DeathRecords$HispanicOriginRaceRecode) <- c("Mexican","Puerto Rican","Cuban","Central/South American","Other Hispanic",
                                                   "Non-hispanic white","Non-hispanic black","Non-hispanic other","Unknown")


# Explore Trends in the Data
# Summary Statistics & Specific subgroups related to Cause of Death
# ==============================================================

SummaryStatistics <- summary(DeathRecords)
attach(DeathRecords)

SexPlot <- barplot(table(Sex), xlab = "Sex", ylab = "Number of Deaths", main = "Deaths by Sex",
        ylim = c(0, 1400000))
text(x = SexPlot, y = table(Sex), label = table(Sex), pos = 3, cex = 0.8, col = "red")

EduCount <- group_by(DeathRecords, Education2003Revision) %>% summarize(count = n()) %>% arrange(desc(count))
mp <- barplot(EduCount$count, main = "Effect of Education on Death", 
               xlim = c(0,1.2e6), las = 2, horiz = TRUE, axisnames = FALSE)
mtext(text = EduCount$Education2003Revision, side = 2, at = mp+0.75, line =-20, padj = par(las = 2))

CauseCount <- group_by(DeathRecords, CauseRecode39) %>% summarize(count = n()) %>% arrange(desc(count))
barplot(CauseCount$count[1:10], names.arg = CauseCount$CauseRecode39[1:10], las = 2,
        ylab = "Number of Deaths", main = "Cause of Death", cex.axis = 0.8, cex.names = 0.8)

barplot(table(DeathRecords$Race), cex.names = 1, line = -0.6, main = "Death by Race", ylim = c(0,2500000))

CauseCountF <- group_by(DeathRecords[DeathRecords$Sex == "F",], CauseRecode39) %>% summarize(count = n()) %>% arrange(desc(count))
barplot(CauseCountF$count[1:10], names.arg = CauseCountF$CauseRecode39[1:10], las = 2,
        main = "Top 10 Cause of Death in Females", ylim = c(0,300000))
CauseCountM <- group_by(DeathRecords[DeathRecords$Sex == "M",], CauseRecode39) %>% summarize(count = n()) %>% arrange(desc(count))
barplot(CauseCountM$count[1:10], names.arg = CauseCountM$CauseRecode39[1:10], las = 2,
        main = "Top 10 Cause of Death in Males", ylim = c(0,250000))

CauseCountj <- group_by(DeathRecords[DeathRecords$MonthOfDeath == "Jan",], CauseRecode39) %>% summarize(count = n()) %>% arrange(desc(count))
barplot(CauseCountj$count[1:10], names.arg = CauseCountj$CauseRecode39[1:10], las = 2,
        main = "Top 10 Cause of Death in January", ylim = c(0,50000))

CauseCountJ <- group_by(DeathRecords[DeathRecords$MonthOfDeath == "Jul",], CauseRecode39) %>% summarize(count = n()) %>% arrange(desc(count))
barplot(CauseCountJ$count[1:10], names.arg = CauseCountJ$CauseRecode39[1:10], las = 2,
        main = "Top 10 Cause of Death in July", ylim = c(0,50000))

CauseCountW <- group_by(DeathRecords[DeathRecords$HispanicOriginRaceRecode == "Non-hispanic white",], CauseRecode39) %>% summarize(count = n()) %>% arrange(desc(count))
barplot(CauseCountW$count[1:10], names.arg = CauseCountW$CauseRecode39[1:10], las = 2,
        main = "Top 10 Cause of Death Caucasian")

CauseCountMe <- group_by(DeathRecords[DeathRecords$HispanicOriginRaceRecode == "Mexican",], CauseRecode39) %>% summarize(count = n()) %>% arrange(desc(count))
barplot(CauseCountMe$count[1:10], names.arg = CauseCountMe$CauseRecode39[1:10], las = 2,
        main = "Top 10 Cause of Death Mexicans", ylim = c(0,20000))

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
