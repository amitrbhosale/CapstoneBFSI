# CredX - Acquisition Risk Analysis Capstone Project.

#Installing Required Packages
install.packages("rstudioapi")
install.packages("ggplot2")
install.packages("stringr")
install.packages("Information")
install.packages("MASS")
library(MASS)
install.packages("car")
install.packages("Rcpp")
library(car)
library(Information)
library(rstudioapi)
library(ggplot2)
library(stringr)
#Set working directory to directory of the file

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Read the datasets

Demographic_data <- read.csv("Demographic data.csv")

# Demographic data contains customer level information (age, gender, income, marital status, etc)

View(Demographic_data)

Credit_Bureau_data <- read.csv("Credit Bureau data.csv")

# Credit Bureau Data contains information taken from credit bureau. ('number of times 30 DPD or worse in last 3/6/12 months', 'outstanding balance', 'number of trades', etc.) 

View(Credit_Bureau_data)

# Data Cleaning - Demographic data file.

summary(Demographic_data)

# Identify the null values or na values.

sum(is.na(Demographic_data))
# 1428 null values found in the Demographic dataset.

sapply(Demographic_data, function(x) sum(is.na(x)))

#Checking unique Application IDs
nrow(Demographic_data)
length(unique(Demographic_data$Application.ID))

# Looking from the data, there are duplicate Application IDs in the dataset

nrow(Demographic_data[duplicated(Demographic_data$Application.ID),])

# As the number of duplicates are very less compared to the size of the dataset, removing the duplicate records

# Removing the duplicate records
Demographic_data <- Demographic_data[!duplicated(Demographic_data$Application.ID),]

# Looking at the Demographic dataset, there are 3 areas in No.of.Dependents where the value is NA, we are replacing it with 0.

#Demographic_data$No.of.dependents[which(is.na(Demographic_data$No.of.dependents))] <- 0
# We need to verify the NA logic for number of dependents from WoE data before we can mark it as 0

#Converting data to category variables.

Demographic_data$No.of.dependents <- as.factor(Demographic_data$No.of.dependents)
Demographic_data$Performance.Tag <- as.factor(Demographic_data$Performance.Tag)
summary(Demographic_data)

sapply(Credit_Bureau_data, function(x) sum(is.na(x)))
nrow(Credit_Bureau_data)
length(unique(Credit_Bureau_data$Application.ID))

nrow(Credit_Bureau_data[duplicated(Credit_Bureau_data$Application.ID),])

# As the number of duplicates are very less compared to the size of the dataset, removing the duplicate records

Credit_Bureau_data <- Credit_Bureau_data[!duplicated(Credit_Bureau_data$Application.ID),]

merged_df <- merge(Demographic_data,Credit_Bureau_data,by.x = "Application.ID", by.y = "Application.ID")


nrow(merged_df)

#Need to perform WOE and IV Analysis, The cleaning would be done using the WOE transformation.

# As there are two attributes in the merged data frame "merge_df" (Performance.Tag.x and Performance.Tag.y), we are checking if both carry the same data.

# Below function returns the total occurrences if the values are exactly matching including the NAs

sum(ifelse(merged_df$Performance.Tag.x == merged_df$Performance.Tag.y, 1,0)  | ifelse(is.na(merged_df$Performance.Tag.x) & is.na(merged_df$Performance.Tag.y), 1,0))

# Since, the above sum matches the row count of the merged data frame, we can conclude that both columns are identical and one column can be removed.

# Removing the redundant column
merged_df$Performance.Tag.x <- NULL

# Fixing the negative values for some variables which are outliers.

quantile(merged_df$Age,seq(0,1,0.01))

# We can see a jump in the Age variable from -3 to 27 and Age cannot be negative, therefore we are treating this outlier.

merged_df$Age <- ifelse(merged_df$Age<27, 27, merged_df$Age)

#Checking if the Income variable has outliers.

quantile(merged_df$Income, seq(0,1,0.01))

# We can see a jump in the Income variable from -0.5 to 4.5 and Income cannot be negative, therefore we are treating this as an outlier.

merged_df$Income <- ifelse(merged_df$Income<4.5, 4.5, merged_df$Income)

# Checking if the Outstanding.Balance variable has outliers
quantile(merged_df$Outstanding.Balance, seq(0,1,0.01),na.rm = TRUE)

outlier <- boxplot.stats(merged_df$Outstanding.Balance)
outlier$out

#EDA to find the important variables

#Filtering only defaulters data for univariate analysis
Defaulters <- subset(merged_df, merged_df$Performance.Tag.y==1)

ggplot(Defaulters, aes(x=Gender))+geom_bar(stat = "count")

ggplot(Defaulters, aes(x=Marital.Status..at.the.time.of.application.))+geom_bar(stat = "count")

ggplot(Defaulters, aes(x=factor(No.of.dependents)))+geom_bar(stat = "count")

#Need to perform WOE and IV Analysis for demographic data

summary(Demographic_data$Performance.Tag)

#Remove NAs from Dependant Variable as it won't allow execution of IV functions.
traindata_demographic <- subset(Demographic_data, is.na(Demographic_data$Performance.Tag)==FALSE)

for (i in 1:nrow(traindata_demographic)) {
  ifelse(traindata_demographic$Performance.Tag[i]==0,traindata_demographic$Performance.Tag[i] <- 1,traindata_demographic$Performance.Tag[i] <- 0)
}

traindata_demographic$Performance.Tag <- as.numeric(as.character(traindata_demographic$Performance.Tag))

# Generate InfoTables for the variables
IV_demographic <- create_infotables(traindata_demographic,y="Performance.Tag",parallel = TRUE, ncore = 4)

IV_demographic$Summary

#Adding a bar graph to see the important variablles based on IV value
All_IV_dem <- data.frame(IV_demographic$Summary)
All_IV_dem$Variable <- factor(All_IV_dem$Variable, levels = All_IV_dem$Variable[order(-All_IV_dem$IV)])
ggplot(All_IV_dem, aes(x=All_IV_dem$Variable,y=All_IV_dem$IV))+geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

sapply(Demographic_data, function(x) sum(is.na(x)))

#Only No.of.dependents has 3 NA values and hence replacing those 3 values by nearest value according to IV
which(is.na(Demographic_data$No.of.dependents))
Demographic_data$No.of.dependents[which(is.na(Demographic_data$No.of.dependents))] <- 1 

#----------------Model Building for Demographic Data set-------------------------
#---------------------------------------------------------    

# splitting into train and test data

set.seed(1)

