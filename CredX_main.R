# CredX - Acquisition Risk Analysis Capstone Project.

#Installing Required Packages
install.packages("rstudioapi")
install.packages("ggplot2")
install.packages("stringr")
install.packages("Information")
install.packages("MASS")
install.packages("car")
install.packages("e1071")
install.packages("caret", dependencies = c("Depends", "Suggests"))
install.packages("cowplot")
install.packages("GGally")
library(Information)
library(rstudioapi)
library(ggplot2)
library(stringr)
library(MASS)
library(car)
library(e1071)
library(caret)
library(cowplot)
library(caTools)

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

#Need to perform WOE and IV Analysis

summary(merged_df$Performance.Tag.y)

length(which(merged_df$Performance.Tag.y==1))
length(which(merged_df$Performance.Tag.y==0))

#Swapping 0 and 1 values for performance tag column since 1 should denote good and 0 should denote bad
for (i in 1:nrow(merged_df)) {
  ifelse(merged_df$Performance.Tag.y[i]==0,merged_df$Performance.Tag.y[i] <- 1,merged_df$Performance.Tag.y[i] <- 0)
}
length(which(merged_df$Performance.Tag.y==1))
length(which(merged_df$Performance.Tag.y==0))


#Remove NAs from Dependant Variable as it won't allow execution of IV functions.
traindata <- subset(merged_df, is.na(merged_df$Performance.Tag.y)==FALSE)

traindata$Performance.Tag.y <- as.numeric(traindata$Performance.Tag.y)

# Generate InfoTables for the variables
IV <- create_infotables(traindata,y="Performance.Tag.y",parallel = TRUE, ncore = 4)

IV$Summary

#Adding a bar graph to see the important variablles based on IV value
All_IVs <- data.frame(IV$Summary)
All_IVs$Variable <- factor(All_IVs$Variable, levels = All_IVs$Variable[order(-All_IVs$IV)])
ggplot(All_IVs, aes(x=All_IVs$Variable,y=All_IVs$IV))+geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + geom_hline(yintercept = 0.1, color = "red")  + xlab("Predictor Variable") + ylab("IV") + ggtitle("IV for Predictor variable")

predictor_variables <- data.frame(IV$Summary)
predictor_variables <- subset(predictor_variables,predictor_variables$IV >0.1)

predictor_variables

sapply(merged_df, function(x) sum(is.na(x)))


IV$Tables$Avgas.CC.Utilization.in.last.12.months
val <- IV$Tables$Avgas.CC.Utilization.in.last.12.months$WOE[which(IV$Tables$Avgas.CC.Utilization.in.last.12.months$Avgas.CC.Utilization.in.last.12.months == "NA")]
napos <- which(IV$Tables$Avgas.CC.Utilization.in.last.12.months$Avgas.CC.Utilization.in.last.12.months == "NA")

repos <- which.min(abs(IV$Tables$Avgas.CC.Utilization.in.last.12.months$WOE[-napos] - val)) + 1 # Adding 1 as first row is excluded

#Replacing NA value with highest value in the bucket available at position repos
#Getting the highest value as mentioned earlier

IV$Tables$Avgas.CC.Utilization.in.last.12.months[repos,1]

a <- str_locate(IV$Tables$Avgas.CC.Utilization.in.last.12.months[repos,1],",")[1]
b <- str_locate(IV$Tables$Avgas.CC.Utilization.in.last.12.months[repos,1],"]")[1]
replacement <- as.numeric(substr(IV$Tables$Avgas.CC.Utilization.in.last.12.months[repos,1],a+1,b-1))

#Replacing the value where NA's are located in the original dataset
merged_df$Avgas.CC.Utilization.in.last.12.months[which(is.na(merged_df$Avgas.CC.Utilization.in.last.12.months))] <- replacement
sum(is.na(merged_df$Avgas.CC.Utilization.in.last.12.months))
# Checking the second predictor variable No.of.trades.opened.in.last.12.months

IV$Tables$No.of.trades.opened.in.last.12.months


# Checking the third predictor variable No.of.PL.trades.opened.in.last.12.months

IV$Tables$No.of.PL.trades.opened.in.last.12.months

# Checking the fourth predictor variable 

IV$Tables$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.

# Checking the fifth predictor variable Outstanding.Balance

IV$Tables$Outstanding.Balance
val <- IV$Tables$Outstanding.Balance$WOE[which(IV$Tables$Outstanding.Balance$Outstanding.Balance == "NA")]
napos <- which(IV$Tables$Outstanding.Balance$Outstanding.Balance == "NA")

repos <- which.min(abs(IV$Tables$Outstanding.Balance$WOE[-napos] - val)) + 1 #Adding 1 as first row is excluded

#Replacing NA value with highest value in the bucket available at position repos
#Getting the highest value as mentioned earlier

IV$Tables$Outstanding.Balance[repos,1]

a <- str_locate(IV$Tables$Outstanding.Balance[repos,1],",")[1]
b <- str_locate(IV$Tables$Outstanding.Balance[repos,1],"]")[1]
replacement <- as.numeric(substr(IV$Tables$Outstanding.Balance[repos,1],a+1,b-1))

#Replacing the value where NA's are located in the original dataset
merged_df$Outstanding.Balance[which(is.na(merged_df$Outstanding.Balance))] <- replacement


# Checking another predictor variable No.of.times.30.DPD.or.worse.in.last.6.months
IV$Tables$No.of.times.30.DPD.or.worse.in.last.6.months


# Checking another predictor variable Total.No.of.Trades

IV$Tables$Total.No.of.Trades

# Checking another predictor variable No.of.PL.trades.opened.in.last.6.months

IV$Tables$No.of.PL.trades.opened.in.last.6.months

# Checking another predictor variable No.of.times.90.DPD.or.worse.in.last.12.months

IV$Tables$No.of.times.90.DPD.or.worse.in.last.12.months

# Checking another predictor variable No.of.times.60.DPD.or.worse.in.last.6.months

IV$Tables$No.of.times.60.DPD.or.worse.in.last.6.months

# Checking another predictor variable No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.

IV$Tables$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.

# Checking another predictor variable No.of.times.30.DPD.or.worse.in.last.12.months

IV$Tables$No.of.times.30.DPD.or.worse.in.last.12.months

#Checking another predictor variable No.of.trades.opened.in.last.6.months

IV$Tables$No.of.trades.opened.in.last.6.months
val <- IV$Tables$No.of.trades.opened.in.last.6.months$WOE[which(IV$Tables$No.of.trades.opened.in.last.6.months$No.of.trades.opened.in.last.6.months == "NA")]
napos <- which(IV$Tables$No.of.trades.opened.in.last.6.months$No.of.trades.opened.in.last.6.months == "NA")

repos <- which.min(abs(IV$Tables$No.of.trades.opened.in.last.6.months$WOE[-napos] - val)) + 1 #Adding 1 as first row is excluded

#Replacing NA value with highest value in the bucket available at position repos
#Getting the highest value as mentioned earlier

IV$Tables$No.of.trades.opened.in.last.6.months[repos,1]

a <- str_locate(IV$Tables$No.of.trades.opened.in.last.6.months[repos,1],",")[1]
b <- str_locate(IV$Tables$No.of.trades.opened.in.last.6.months[repos,1],"]")[1]
replacement <- as.numeric(substr(IV$Tables$No.of.trades.opened.in.last.6.months[repos,1],a+1,b-1))

#Replacing the value where NA's are located in the original dataset
merged_df$No.of.trades.opened.in.last.6.months[which(is.na(merged_df$No.of.trades.opened.in.last.6.months))] <- replacement


# Checking another predictor variable No.of.times.60.DPD.or.worse.in.last.12.months

IV$Tables$No.of.times.60.DPD.or.worse.in.last.12.months

# Checking another predictor variable No.of.times.90.DPD.or.worse.in.last.6.months

IV$Tables$No.of.times.90.DPD.or.worse.in.last.6.months

traindata <- subset(merged_df, is.na(merged_df$Performance.Tag.y)==FALSE)
# Create a dataframe with the important variables identified and the dependant variable

impvar_df <- traindata[,c(as.vector(predictor_variables$Variable),"Performance.Tag.y")]


# WOE replacement in the impvar_df to be carried out.

# woe_replace <- function(dataframe, IV, predictor_var)
# {
#   df <- dataframe
#   IV <- IV
#   rows_df <- nrow(df)
#   cols_df <- ncol(df)
#   #IVTable <- 0
#   predictor <- predictor_var
#   # For every column, perform the following actions
#   for ( i in 1:cols_df-1)
#   {
#     colname <- colnames(df[i])
#     num <- which(names(IV$Tables)==colname)
#     IVTable <- data.frame(IV$Tables[[num]])
#     if(is.factor(df[,i])==FALSE) #Numeric column
#     {
#       IVTable$min1 <- str_locate(IVTable[colname],"\\[")[,1]
#       IVTable$min2 <- str_locate(IVTable[colname],"\\,")[,1]
#       
#       IVTable$min <- as.numeric(substr(IVTable[colname],IVTable$min1+1, IVTable$min2-1))
#       
#       IVTable$max1 <- str_locate(IVTable[colname],"\\,")[,1]
#       IVTable$max2 <- str_locate(IVTable[colname],"\\]")[,1]
#       
#       IVTable$max <- as.numeric(substr(IVTable[colname], IVTable$max1+1, IVTable$max2-1))
#       
#       
#       # Perform following action for every row - for replacement
#       for(j in 1: rows_df)
#       {
#         val <- df[j,i]
#         woepos <- which(val>= IVTable$min && val <= IVTable$max)
#         woeval <- IVTable$WOE[woepos]
#         #Replacement
#         df[j,i] <- woeval
#       }
#     }
#     
#   }
#   return(df)
# }
# 
# testing <- woe_replace(impvar_df,IV,predictor_variables$Variable)


# Replacing the woe values for predictor variables

rows_df <- nrow(impvar_df)

#Avgas.CC.Utilization.in.last.12.months

      IVTable <- data.frame(IV$Tables$Avgas.CC.Utilization.in.last.12.months)

      IVTable$min1 <- str_locate(IVTable$Avgas.CC.Utilization.in.last.12.months,"\\[")[,1]
      IVTable$min2 <- str_locate(IVTable$Avgas.CC.Utilization.in.last.12.months,"\\,")[,1]

      IVTable$min <- as.numeric(substr(IVTable$Avgas.CC.Utilization.in.last.12.months,IVTable$min1+1, IVTable$min2-1))

      IVTable$max1 <- str_locate(IVTable$Avgas.CC.Utilization.in.last.12.months,"\\,")[,1]
      IVTable$max2 <- str_locate(IVTable$Avgas.CC.Utilization.in.last.12.months,"\\]")[,1]

      IVTable$max <- as.numeric(substr(IVTable$Avgas.CC.Utilization.in.last.12.months, IVTable$max1+1, IVTable$max2-1))

# Perform following action for every row - for replacement
   for(j in 1: rows_df)
       {
         val <- impvar_df$Avgas.CC.Utilization.in.last.12.months[j]
         woepos <- which(val>= IVTable$min & val <= IVTable$max)
         woeval <- as.numeric(IVTable$WOE[woepos])
         #Replacement
         impvar_df$Avgas.CC.Utilization.in.last.12.months[j] <- woeval
       }

      sum(is.na(impvar_df$Avgas.CC.Utilization.in.last.12.months))

#No.of.trades.opened.in.last.12.months
      
      IVTable <- data.frame(IV$Tables$No.of.trades.opened.in.last.12.months)
      
      IVTable$min1 <- str_locate(IVTable$No.of.trades.opened.in.last.12.months,"\\[")[,1]
      IVTable$min2 <- str_locate(IVTable$No.of.trades.opened.in.last.12.months,"\\,")[,1]
      
      IVTable$min <- as.numeric(substr(IVTable$No.of.trades.opened.in.last.12.months,IVTable$min1+1, IVTable$min2-1))
      
      IVTable$max1 <- str_locate(IVTable$No.of.trades.opened.in.last.12.months,"\\,")[,1]
      IVTable$max2 <- str_locate(IVTable$No.of.trades.opened.in.last.12.months,"\\]")[,1]
      
      IVTable$max <- as.numeric(substr(IVTable$No.of.trades.opened.in.last.12.months, IVTable$max1+1, IVTable$max2-1))
      
      # Perform following action for every row - for replacement
      for(j in 1: rows_df)
      {
        val <- impvar_df$No.of.trades.opened.in.last.12.months[j]
        woepos <- which(val>= IVTable$min & val <= IVTable$max)
        woeval <- as.numeric(IVTable$WOE[woepos])
        #Replacement
        impvar_df$No.of.trades.opened.in.last.12.months[j] <- woeval
      }
      
      sum(is.na(impvar_df$No.of.trades.opened.in.last.12.months))
      
      
#No.of.PL.trades.opened.in.last.12.months
      
      IVTable <- data.frame(IV$Tables$No.of.PL.trades.opened.in.last.12.months)
      
      IVTable$min1 <- str_locate(IVTable$No.of.PL.trades.opened.in.last.12.months,"\\[")[,1]
      IVTable$min2 <- str_locate(IVTable$No.of.PL.trades.opened.in.last.12.months,"\\,")[,1]
      
      IVTable$min <- as.numeric(substr(IVTable$No.of.PL.trades.opened.in.last.12.months,IVTable$min1+1, IVTable$min2-1))
      
      IVTable$max1 <- str_locate(IVTable$No.of.PL.trades.opened.in.last.12.months,"\\,")[,1]
      IVTable$max2 <- str_locate(IVTable$No.of.PL.trades.opened.in.last.12.months,"\\]")[,1]
      
      IVTable$max <- as.numeric(substr(IVTable$No.of.PL.trades.opened.in.last.12.months, IVTable$max1+1, IVTable$max2-1))
      
      # Perform following action for every row - for replacement
      for(j in 1: rows_df)
      {
        val <- impvar_df$No.of.PL.trades.opened.in.last.12.months[j]
        woepos <- which(val>= IVTable$min & val <= IVTable$max)
        woeval <- as.numeric(IVTable$WOE[woepos])
        #Replacement
        impvar_df$No.of.PL.trades.opened.in.last.12.months[j] <- woeval
      }
      
      sum(is.na(impvar_df$No.of.PL.trades.opened.in.last.12.months))
      
#No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.
      
      IVTable <- data.frame(IV$Tables$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)
      
      IVTable$min1 <- str_locate(IVTable$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.,"\\[")[,1]
      IVTable$min2 <- str_locate(IVTable$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.,"\\,")[,1]
      
      IVTable$min <- as.numeric(substr(IVTable$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.,IVTable$min1+1, IVTable$min2-1))
      
      IVTable$max1 <- str_locate(IVTable$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.,"\\,")[,1]
      IVTable$max2 <- str_locate(IVTable$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.,"\\]")[,1]
      
      IVTable$max <- as.numeric(substr(IVTable$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., IVTable$max1+1, IVTable$max2-1))
      
      # Perform following action for every row - for replacement
      for(j in 1: rows_df)
      {
        val <- impvar_df$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.[j]
        woepos <- which(val>= IVTable$min & val <= IVTable$max)
        woeval <- as.numeric(IVTable$WOE[woepos])
        #Replacement
        impvar_df$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.[j] <- woeval
      }
      
      sum(is.na(impvar_df$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.))
      
#Outstanding.Balance
      
      IVTable <- data.frame(IV$Tables$Outstanding.Balance)
      
      IVTable$min1 <- str_locate(IVTable$Outstanding.Balance,"\\[")[,1]
      IVTable$min2 <- str_locate(IVTable$Outstanding.Balance,"\\,")[,1]
      
      IVTable$min <- as.numeric(substr(IVTable$Outstanding.Balance,IVTable$min1+1, IVTable$min2-1))
      
      IVTable$max1 <- str_locate(IVTable$Outstanding.Balance,"\\,")[,1]
      IVTable$max2 <- str_locate(IVTable$Outstanding.Balance,"\\]")[,1]
      
      IVTable$max <- as.numeric(substr(IVTable$Outstanding.Balance, IVTable$max1+1, IVTable$max2-1))
      
      # Perform following action for every row - for replacement
      for(j in 1: rows_df)
      {
        val <- impvar_df$Outstanding.Balance[j]
        woepos <- which(val>= IVTable$min & val <= IVTable$max)
        woeval <- as.numeric(IVTable$WOE[woepos])
        #Replacement
        impvar_df$Outstanding.Balance[j] <- woeval
      }
      
      sum(is.na(impvar_df$Outstanding.Balance))
      

#No.of.times.30.DPD.or.worse.in.last.6.months
      
      IVTable <- data.frame(IV$Tables$No.of.times.30.DPD.or.worse.in.last.6.months)
      
      IVTable$min1 <- str_locate(IVTable$No.of.times.30.DPD.or.worse.in.last.6.months,"\\[")[,1]
      IVTable$min2 <- str_locate(IVTable$No.of.times.30.DPD.or.worse.in.last.6.months,"\\,")[,1]
      
      IVTable$min <- as.numeric(substr(IVTable$No.of.times.30.DPD.or.worse.in.last.6.months,IVTable$min1+1, IVTable$min2-1))
      
      IVTable$max1 <- str_locate(IVTable$No.of.times.30.DPD.or.worse.in.last.6.months,"\\,")[,1]
      IVTable$max2 <- str_locate(IVTable$No.of.times.30.DPD.or.worse.in.last.6.months,"\\]")[,1]
      
      IVTable$max <- as.numeric(substr(IVTable$No.of.times.30.DPD.or.worse.in.last.6.months, IVTable$max1+1, IVTable$max2-1))
      
      # Perform following action for every row - for replacement
      for(j in 1: rows_df)
      {
        val <- impvar_df$No.of.times.30.DPD.or.worse.in.last.6.months[j]
        woepos <- which(val>= IVTable$min & val <= IVTable$max)
        woeval <- as.numeric(IVTable$WOE[woepos])
        #Replacement
        impvar_df$No.of.times.30.DPD.or.worse.in.last.6.months[j] <- woeval
      }
      
      sum(is.na(impvar_df$No.of.times.30.DPD.or.worse.in.last.6.months))
      
#Total.No.of.Trades
      
      IVTable <- data.frame(IV$Tables$Total.No.of.Trades)
      
      IVTable$min1 <- str_locate(IVTable$Total.No.of.Trades,"\\[")[,1]
      IVTable$min2 <- str_locate(IVTable$Total.No.of.Trades,"\\,")[,1]
      
      IVTable$min <- as.numeric(substr(IVTable$Total.No.of.Trades,IVTable$min1+1, IVTable$min2-1))
      
      IVTable$max1 <- str_locate(IVTable$Total.No.of.Trades,"\\,")[,1]
      IVTable$max2 <- str_locate(IVTable$Total.No.of.Trades,"\\]")[,1]
      
      IVTable$max <- as.numeric(substr(IVTable$Total.No.of.Trades, IVTable$max1+1, IVTable$max2-1))
      
      # Perform following action for every row - for replacement
      for(j in 1: rows_df)
      {
        val <- impvar_df$Total.No.of.Trades[j]
        woepos <- which(val>= IVTable$min & val <= IVTable$max)
        woeval <- as.numeric(IVTable$WOE[woepos])
        #Replacement
        impvar_df$Total.No.of.Trades[j] <- woeval
      }
      
      sum(is.na(impvar_df$Total.No.of.Trades))
      
#No.of.PL.trades.opened.in.last.6.months
      
      IVTable <- data.frame(IV$Tables$No.of.PL.trades.opened.in.last.6.months)
      
      IVTable$min1 <- str_locate(IVTable$No.of.PL.trades.opened.in.last.6.months,"\\[")[,1]
      IVTable$min2 <- str_locate(IVTable$No.of.PL.trades.opened.in.last.6.months,"\\,")[,1]
      
      IVTable$min <- as.numeric(substr(IVTable$No.of.PL.trades.opened.in.last.6.months,IVTable$min1+1, IVTable$min2-1))
      
      IVTable$max1 <- str_locate(IVTable$No.of.PL.trades.opened.in.last.6.months,"\\,")[,1]
      IVTable$max2 <- str_locate(IVTable$No.of.PL.trades.opened.in.last.6.months,"\\]")[,1]
      
      IVTable$max <- as.numeric(substr(IVTable$No.of.PL.trades.opened.in.last.6.months, IVTable$max1+1, IVTable$max2-1))
      
      # Perform following action for every row - for replacement
      for(j in 1: rows_df)
      {
        val <- impvar_df$No.of.PL.trades.opened.in.last.6.months[j]
        woepos <- which(val>= IVTable$min & val <= IVTable$max)
        woeval <- as.numeric(IVTable$WOE[woepos])
        #Replacement
        impvar_df$No.of.PL.trades.opened.in.last.6.months[j] <- woeval
      }
      
      sum(is.na(impvar_df$No.of.PL.trades.opened.in.last.6.months))
      
#No.of.times.90.DPD.or.worse.in.last.12.months
      
      IVTable <- data.frame(IV$Tables$No.of.times.90.DPD.or.worse.in.last.12.months)
      
      IVTable$min1 <- str_locate(IVTable$No.of.times.90.DPD.or.worse.in.last.12.months,"\\[")[,1]
      IVTable$min2 <- str_locate(IVTable$No.of.times.90.DPD.or.worse.in.last.12.months,"\\,")[,1]
      
      IVTable$min <- as.numeric(substr(IVTable$No.of.times.90.DPD.or.worse.in.last.12.months,IVTable$min1+1, IVTable$min2-1))
      
      IVTable$max1 <- str_locate(IVTable$No.of.times.90.DPD.or.worse.in.last.12.months,"\\,")[,1]
      IVTable$max2 <- str_locate(IVTable$No.of.times.90.DPD.or.worse.in.last.12.months,"\\]")[,1]
      
      IVTable$max <- as.numeric(substr(IVTable$No.of.times.90.DPD.or.worse.in.last.12.months, IVTable$max1+1, IVTable$max2-1))
      
      # Perform following action for every row - for replacement
      for(j in 1: rows_df)
      {
        val <- impvar_df$No.of.times.90.DPD.or.worse.in.last.12.months[j]
        woepos <- which(val>= IVTable$min & val <= IVTable$max)
        woeval <- as.numeric(IVTable$WOE[woepos])
        #Replacement
        impvar_df$No.of.times.90.DPD.or.worse.in.last.12.months[j] <- woeval
      }
      
      sum(is.na(impvar_df$No.of.times.90.DPD.or.worse.in.last.12.months))

#No.of.times.60.DPD.or.worse.in.last.12.months
      
      IVTable <- data.frame(IV$Tables$No.of.times.60.DPD.or.worse.in.last.12.months)
      
      IVTable$min1 <- str_locate(IVTable$No.of.times.60.DPD.or.worse.in.last.12.months,"\\[")[,1]
      IVTable$min2 <- str_locate(IVTable$No.of.times.60.DPD.or.worse.in.last.12.months,"\\,")[,1]
      
      IVTable$min <- as.numeric(substr(IVTable$No.of.times.60.DPD.or.worse.in.last.12.months,IVTable$min1+1, IVTable$min2-1))
      
      IVTable$max1 <- str_locate(IVTable$No.of.times.60.DPD.or.worse.in.last.12.months,"\\,")[,1]
      IVTable$max2 <- str_locate(IVTable$No.of.times.60.DPD.or.worse.in.last.12.months,"\\]")[,1]
      
      IVTable$max <- as.numeric(substr(IVTable$No.of.times.60.DPD.or.worse.in.last.12.months, IVTable$max1+1, IVTable$max2-1))
      
      # Perform following action for every row - for replacement
      for(j in 1: rows_df)
      {
        val <- impvar_df$No.of.times.60.DPD.or.worse.in.last.12.months[j]
        woepos <- which(val>= IVTable$min & val <= IVTable$max)
        woeval <- as.numeric(IVTable$WOE[woepos])
        #Replacement
        impvar_df$No.of.times.60.DPD.or.worse.in.last.12.months[j] <- woeval
      }
      
      sum(is.na(impvar_df$No.of.times.60.DPD.or.worse.in.last.12.months))  
      

#No.of.times.60.DPD.or.worse.in.last.6.months
      
      IVTable <- data.frame(IV$Tables$No.of.times.60.DPD.or.worse.in.last.6.months)
      
      IVTable$min1 <- str_locate(IVTable$No.of.times.60.DPD.or.worse.in.last.6.months,"\\[")[,1]
      IVTable$min2 <- str_locate(IVTable$No.of.times.60.DPD.or.worse.in.last.6.months,"\\,")[,1]
      
      IVTable$min <- as.numeric(substr(IVTable$No.of.times.60.DPD.or.worse.in.last.6.months,IVTable$min1+1, IVTable$min2-1))
      
      IVTable$max1 <- str_locate(IVTable$No.of.times.60.DPD.or.worse.in.last.6.months,"\\,")[,1]
      IVTable$max2 <- str_locate(IVTable$No.of.times.60.DPD.or.worse.in.last.6.months,"\\]")[,1]
      
      IVTable$max <- as.numeric(substr(IVTable$No.of.times.60.DPD.or.worse.in.last.6.months, IVTable$max1+1, IVTable$max2-1))
      
      # Perform following action for every row - for replacement
      for(j in 1: rows_df)
      {
        val <- impvar_df$No.of.times.60.DPD.or.worse.in.last.6.months[j]
        woepos <- which(val>= IVTable$min & val <= IVTable$max)
        woeval <- as.numeric(IVTable$WOE[woepos])
        #Replacement
        impvar_df$No.of.times.60.DPD.or.worse.in.last.6.months[j] <- woeval
      }
      
      sum(is.na(impvar_df$No.of.times.60.DPD.or.worse.in.last.6.months))       
      
#No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.
      
      IVTable <- data.frame(IV$Tables$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)
      
      IVTable$min1 <- str_locate(IVTable$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.,"\\[")[,1]
      IVTable$min2 <- str_locate(IVTable$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.,"\\,")[,1]
      
      IVTable$min <- as.numeric(substr(IVTable$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.,IVTable$min1+1, IVTable$min2-1))
      
      IVTable$max1 <- str_locate(IVTable$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.,"\\,")[,1]
      IVTable$max2 <- str_locate(IVTable$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.,"\\]")[,1]
      
      IVTable$max <- as.numeric(substr(IVTable$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans., IVTable$max1+1, IVTable$max2-1))
      
      # Perform following action for every row - for replacement
      for(j in 1: rows_df)
      {
        val <- impvar_df$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.[j]
        woepos <- which(val>= IVTable$min & val <= IVTable$max)
        woeval <- as.numeric(IVTable$WOE[woepos])
        #Replacement
        impvar_df$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.[j] <- woeval
      }
      
      sum(is.na(impvar_df$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)) 

#No.of.times.30.DPD.or.worse.in.last.12.months
      
      IVTable <- data.frame(IV$Tables$No.of.times.30.DPD.or.worse.in.last.12.months)
      
      IVTable$min1 <- str_locate(IVTable$No.of.times.30.DPD.or.worse.in.last.12.months,"\\[")[,1]
      IVTable$min2 <- str_locate(IVTable$No.of.times.30.DPD.or.worse.in.last.12.months,"\\,")[,1]
      
      IVTable$min <- as.numeric(substr(IVTable$No.of.times.30.DPD.or.worse.in.last.12.months,IVTable$min1+1, IVTable$min2-1))
      
      IVTable$max1 <- str_locate(IVTable$No.of.times.30.DPD.or.worse.in.last.12.months,"\\,")[,1]
      IVTable$max2 <- str_locate(IVTable$No.of.times.30.DPD.or.worse.in.last.12.months,"\\]")[,1]
      
      IVTable$max <- as.numeric(substr(IVTable$No.of.times.30.DPD.or.worse.in.last.12.months, IVTable$max1+1, IVTable$max2-1))
      
      # Perform following action for every row - for replacement
      for(j in 1: rows_df)
      {
        val <- impvar_df$No.of.times.30.DPD.or.worse.in.last.12.months[j]
        woepos <- which(val>= IVTable$min & val <= IVTable$max)
        woeval <- as.numeric(IVTable$WOE[woepos])
        #Replacement
        impvar_df$No.of.times.30.DPD.or.worse.in.last.12.months[j] <- woeval
      }
      
      sum(is.na(impvar_df$No.of.times.30.DPD.or.worse.in.last.12.months))
      
      
#No.of.trades.opened.in.last.6.months
      
      IVTable <- data.frame(IV$Tables$No.of.trades.opened.in.last.6.months)
      
      IVTable$min1 <- str_locate(IVTable$No.of.trades.opened.in.last.6.months,"\\[")[,1]
      IVTable$min2 <- str_locate(IVTable$No.of.trades.opened.in.last.6.months,"\\,")[,1]
      
      IVTable$min <- as.numeric(substr(IVTable$No.of.trades.opened.in.last.6.months,IVTable$min1+1, IVTable$min2-1))
      
      IVTable$max1 <- str_locate(IVTable$No.of.trades.opened.in.last.6.months,"\\,")[,1]
      IVTable$max2 <- str_locate(IVTable$No.of.trades.opened.in.last.6.months,"\\]")[,1]
      
      IVTable$max <- as.numeric(substr(IVTable$No.of.trades.opened.in.last.6.months, IVTable$max1+1, IVTable$max2-1))
      
      # Perform following action for every row - for replacement
      for(j in 1: rows_df)
      {
        val <- impvar_df$No.of.trades.opened.in.last.6.months[j]
        woepos <- which(val>= IVTable$min & val <= IVTable$max)
        woeval <- as.numeric(IVTable$WOE[woepos])
        #Replacement
        impvar_df$No.of.trades.opened.in.last.6.months[j] <- woeval
      }
      
      sum(is.na(impvar_df$No.of.trades.opened.in.last.6.months))
      
      
#No.of.times.90.DPD.or.worse.in.last.6.months
      
      IVTable <- data.frame(IV$Tables$No.of.times.90.DPD.or.worse.in.last.6.months)
      
      IVTable$min1 <- str_locate(IVTable$No.of.times.90.DPD.or.worse.in.last.6.months,"\\[")[,1]
      IVTable$min2 <- str_locate(IVTable$No.of.times.90.DPD.or.worse.in.last.6.months,"\\,")[,1]
      
      IVTable$min <- as.numeric(substr(IVTable$No.of.times.90.DPD.or.worse.in.last.6.months,IVTable$min1+1, IVTable$min2-1))
      
      IVTable$max1 <- str_locate(IVTable$No.of.times.90.DPD.or.worse.in.last.6.months,"\\,")[,1]
      IVTable$max2 <- str_locate(IVTable$No.of.times.90.DPD.or.worse.in.last.6.months,"\\]")[,1]
      
      IVTable$max <- as.numeric(substr(IVTable$No.of.times.90.DPD.or.worse.in.last.6.months, IVTable$max1+1, IVTable$max2-1))
      
      # Perform following action for every row - for replacement
      for(j in 1: rows_df)
      {
        val <- impvar_df$No.of.times.90.DPD.or.worse.in.last.6.months[j]
        woepos <- which(val>= IVTable$min & val <= IVTable$max)
        woeval <- as.numeric(IVTable$WOE[woepos])
        #Replacement
        impvar_df$No.of.times.90.DPD.or.worse.in.last.6.months[j] <- woeval
      }
      
      sum(is.na(impvar_df$No.of.times.90.DPD.or.worse.in.last.6.months)) 
      
      
# Modelling Logistic regression
      
########################################################################
      # splitting the data between train and test
      set.seed(100)
      
      indices = sample.split(impvar_df$Performance.Tag.y, SplitRatio = 0.7)
      
      train = impvar_df[indices,]
      
      test = impvar_df[!(indices),]
      
      
      #Initial model
      model_1 = glm(Performance.Tag.y ~ ., data = train, family = "binomial")
      summary(model_1)
      
# Using StepAIC function
model_2<- stepAIC(model_1, direction="both")

summary(model_2)
vif(model_2)
