# CKME 136 - Capstone Project for Data Analytics
# ID   : 500786799
# Name : Gagan Minhas
# ==============================================================

# Load Libraries
# ==============================================================
library(magrittr)
library(dplyr)
library(rpart)
library(tree)
library(party)
library(RWeka)
library(mlbench)
library(caret)
library(caretEnsemble) # requires package 'chron'
library(c50)
library(gbm)
# Load Death Records Data 
# ==============================================================
File <- 'C:/Users/CKME999/Downloads/DeathRecords.csv'
DeathRecords <- read.csv(File, header = TRUE)
rm(File)

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


# CauseRecode358, CauseRecode113
dropSameAttributes <- c("Education1989Revision","EducationReportingFlag","AgeType",
                        "Age","AgeSubstitutionFlag","AgeRecode52","AgeRecode12",
                        "Icd10Code","MannerOfDeath","RaceImputationFlag",
                        "RaceRecode3","RaceRecode5","HispanicOrigin")



dropNonEssAttributes <- c("Id","CurrentDataYear","Autopsy","NumberOfEntityAxisConditions",
                          "NumberOfRecordAxisConditions","MethodOfDisposition")

dropNonAdultAttributes <- c("InfantAgeRecode22","InfantCauseRecode10","InfantCauseRecode130")
dropAttributes <- c(dropNonEssAttributes,dropSameAttributes,dropNonAdultAttributes)
DeathRecords <- DeathRecords[,!(names(DeathRecords) %in% dropAttributes)]
rm(dropAttributes,dropNonAdultAttributes,dropNonEssAttributes,dropSameAttributes)

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
DeathRecords$Race <- as.factor(DeathRecords$Race)
levels(DeathRecords$Race) <- c("Other","White","Black","US Indian","Chinese","Japanese","Hawaiian","Filipino","Other Asian",
                               "Asian Indian","Korean","Samoan","Vietnamese","Guamanian","Other Asian*","All Asian")
DeathRecords$BridgedRaceFlag <- as.factor(DeathRecords$BridgedRaceFlag)
levels(DeathRecords$BridgedRaceFlag) <- c("Not Bridged","Is Bridged")
DeathRecords$HispanicOriginRaceRecode <- as.factor(DeathRecords$HispanicOriginRaceRecode)
levels(DeathRecords$HispanicOriginRaceRecode) <- c("Mexican","Puerto Rican","Cuban","Central/South American","Other Hispanic",
                                                   "Non-hispanic white","Non-hispanic black","Non-hispanic other","Unknown")

# CauseRecode358
a<-sort(table(DeathRecords$CauseRecode358),decreasing=TRUE)[1:20]
DeathRecords358<- DeathRecords[DeathRecords$CauseRecode358 %in% c(189,159,238,214,230,228,327,257,104,81,88,207,23,420),]
DeathRecords358<-DeathRecords358[,!names(DeathRecords358) %in% c("CauseRecode113","CauseRecode39")]
DeathRecords358$CauseRecode358 <- as.factor(DeathRecords358$CauseRecode358)
levels(DeathRecords358$CauseRecode358) <- c("Septicemia","Colon Cancer","Pancreatic Cancer","Breast Cancer","Diabetes","Alzheimers",
                                            "HT Heart Disease","Athero CVD","Conduction Disorders","Congestive Heart Failure","Stroke",
                                            "Pneumonia","Renal Failure","Accidental Poisoning-drugs")
# sample 2k of each and try decision tree
set.seed(1)
n <- 31700 #ideally 31700
train358 <-     (c(sample(which(DeathRecords358$CauseRecode358 == "Septicemia"),n),
                   sample(which(DeathRecords358$CauseRecode358 == "Colon Cancer"),n),
                   sample(which(DeathRecords358$CauseRecode358 == "Pancreatic Cancer"),n),
                   sample(which(DeathRecords358$CauseRecode358 == "Breast Cancer"),n),
                   sample(which(DeathRecords358$CauseRecode358 == "Diabetes"),n),
                   sample(which(DeathRecords358$CauseRecode358 == "Alzheimers"),n),
                   sample(which(DeathRecords358$CauseRecode358 == "HT Heart Disease"),n),
                   sample(which(DeathRecords358$CauseRecode358 == "Athero CVD"),n),
                   sample(which(DeathRecords358$CauseRecode358 == "Conduction Disorders"),n),
                   sample(which(DeathRecords358$CauseRecode358 == "Congestive Heart Failure"),n),
                   sample(which(DeathRecords358$CauseRecode358 == "Stroke"),n),
                   sample(which(DeathRecords358$CauseRecode358 == "Pneumonia"),n),
                   sample(which(DeathRecords358$CauseRecode358 == "Renal Failure"),n),
                   sample(which(DeathRecords358$CauseRecode358 == "Accidental Poisoning-drugs"),n)))


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

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes


# rpart
rpart_tree <- rpart(CauseRecode358 ~ ., data = DeathRecords358[train358,], method = "class")
rpart_pred <- predict(rpart_tree, DeathRecords358[-train358,-c(12)], type = "class")
rpart_results <- table(rpart_pred, DeathRecords358[-train358,]$CauseRecode)


rpart_accuracy <- sum(diag(rpart_results)) / sum(rpart_results)
rpart_precision <- diag(rpart_results) / apply(rpart_results, 2, sum)
rpart_recall <- diag(rpart_results) / apply(rpart_results, 1, sum)

macro_rpart_precision <- mean(rpart_precision)
macro_rpart_recall <- mean(rpart_recall)




# tree - limits categorical variable to 32 levels
tree.DeathRecords <- tree(CauseRecode358 ~ .,data = DeathRecords358[train358,])
tree_pred <- predict(tree.DeathRecords, DeathRecords358[-train358,-c(12)],type="class")
tree_results <- table(tree_pred, DeathRecords358[-train358,]$CauseRecode358)

tree_accuracy <- sum(diag(tree_results)) / sum(tree_results)
tree_precision <- diag(tree_results) / apply(tree_results, 2, sum)
tree_recall <- diag(tree_results) / apply(tree_results, 1, sum)

macro_tree_precision <- mean(tree_precision)
macro_tree_recall <- mean(tree_recall)

# rweka
fit <- J48(CauseRecode358 ~., data = DeathRecords358[train358,])
pred <- predict(fit, DeathRecords358[-train358,-c(12)])
result <- table(pred, DeathRecords358[-train358,]$CauseRecode358)

accuracy <- sum(diag(result)) / sum(result)
precision <- diag(result) / apply(result, 2, sum)
recall <- diag(result) / apply(result, 1, sum)

macro_precision <- mean(precision)
macro_recall <- mean(recall)


# Ensemble Methods
# 1. Boosting - C5.0 & Stochastic Gradient Boosting
# 2. Bagging - Random Forest
# ===============================================================

controlparam <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
metric <- "Accuracy"

# C5.0
set.seed(seed)
fit.c50 <- train(CauseRecode358~., data = DeathRecords358[train358,], method="C5.0", 
                 metric=metric, trControl=controlparam)
predc50 <- predict(fit.c50, DeathRecords358[-train358,-c(12)])
resultc50 <- table(predc50, DeathRecords358[-train358,]$CauseRecode358)

accuracyc50 <- sum(diag(resultc50)) / sum(resultc50)
precisionc50 <- diag(resultc50) / apply(resultc50, 2, sum)
recallc50 <- diag(resultc50) / apply(resultc50, 1, sum)

macro_precision <- mean(precisionc50)
macro_recall <- mean(recallc50)

# Stochastic Gradient Boosting
set.seed(seed)
fit.gbm <- train(CauseRecode358~., data = DeathRecords358[train358,], method="gbm", 
                 metric=metric, trControl=controlparam, verbose=FALSE)

predgbm <- predict(fit.gbm, DeathRecords358[-train358,-c(12)])
resultgbm <- table(predgbm, DeathRecords358[-train358,]$CauseRecode358)

accuracygbm <- sum(diag(resultgbm)) / sum(resultgbm)
precisiongbm <- diag(resultgbm) / apply(resultgbm, 2, sum)
recallgbm <- diag(resultgbm) / apply(resultgbm, 1, sum)

macro_precision <- mean(precisiongbm)
macro_recall <- mean(recallgbm)

# summarize results
boosting_results <- resamples(list(c5.0=fit.c50, gbm=fit.gbm))
summary(boosting_results)
dotplot(boosting_results)
