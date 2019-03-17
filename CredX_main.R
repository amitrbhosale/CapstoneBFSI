#############################################################################################
##                  CredX - Acquisition Risk Analysis Capstone Project                     ##
#############################################################################################

# Environment Clean up
rm(list = ls())

# Installing Necessary Packages
install.packages("rstudioapi")
install.packages("ggplot2")
install.packages("stringr")
install.packages("Information")
install.packages("MASS")
install.packages("car")
install.packages("e1071")
install.packages("ModelMetrics")
install.packages("generics")
install.packages("gower")
install.packages("caret", dependencies = c("Depends", "Suggests"))
install.packages("caTools")
install.packages("cowplot")
install.packages("GGally")
install.packages("gridExtra")
install.packages("purrr")
install.packages("h2o") #For multi-threading
install.packages("rpart")
install.packages("rattle")
install.packages("rpart.plot")
install.packages("randomForest")
install.packages("DMwR")

# Import Nessesary Libraries 
library(DMwR)
library(randomForest)
library(rpart.plot)
library(Information)
library(rstudioapi)
library(ggplot2)
library(stringr)
library(MASS)
library(car)
library(e1071)
library(ModelMetrics)
library(h2o)
library(generics)
library(gower)
library(caret)
library(caTools)
library(cowplot)
library(caTools)
library(gridExtra)
library(purrr)
h2o.init(nthreads = 4)
library(rpart)
library(rattle)

# Set Working Directory to Directory of the File
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Function for Distribution of Categorical Variables 
univariate_categorical <- function(dataset,var,var_name){
  dataset %>% ggplot(aes(x = as.factor(var))) +
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
    scale_y_continuous(labels = scales::percent) +
    labs(title = var_name, y = "Percent", x = var_name)+theme(
      axis.text.y=element_blank(), axis.ticks=element_blank(),
      axis.title.y=element_blank(),axis.text.x = element_text(angle = 60, hjust = 1)
    ) 
}

# Function for Distribution of Continuous Variables
univariate_continuous <- function(dataset,var,var_name){
  dataset %>% ggplot(aes(x = (var))) +
    geom_histogram(breaks=seq(min(var), max(var), by=1),col="red", aes(fill=..count..)) +
    scale_fill_gradient("Count", low="green", high="red")+
    labs(title = var_name, y = "Count", x = var_name)
  
}

# WOE replacement in the impvar_df to be carried out, we are defining woe_replace function.
woe_replace <- function(dataframe, IV)
{
  df <- dataframe
  IV <- IV
  rows_df <- nrow(df)
  cols_df <- ncol(df)
  #IVTable <- 0
  
  # For every column, perform the following actions
  for ( i in 1:(cols_df - 1))  # cols_df-1
  {
    colname <- colnames(df[i])
    ans <- map_df(IV$Tables, ~as.data.frame(.x), .id="test")
    loc <- which(ans["test"]==colname)
    IVTable <- subset(ans[loc,])
    if(is.factor(df[,i])==FALSE) #Numeric columns
    {
      IVTable$min1 <- str_locate(IVTable[,colname],"\\[")[,1]
      IVTable$min2 <- str_locate(IVTable[,colname],"\\,")[,1]
      
      IVTable$min <- as.numeric(substr(IVTable[,colname],IVTable$min1+1, IVTable$min2-1))
      
      IVTable$max1 <- str_locate(IVTable[,colname],"\\,")[,1]
      IVTable$max2 <- str_locate(IVTable[,colname],"\\]")[,1]
      
      IVTable$max <- as.numeric(substr(IVTable[,colname], IVTable$max1+1, IVTable$max2-1))
      
      
      # Perform following action for every row - for replacement
      for(j in 1: rows_df)
      {
        val <- as.numeric(df[j,i])
        woepos <- which(val>= IVTable$min & val<= IVTable$max)
        woeval <- IVTable$WOE[woepos]
        #Replacement
        df[j,i] <- woeval
      }
    }
    
  }
  return(df)
}




# Read the Demographic Datasets
Demographic_data <- read.csv("Demographic data.csv")

# View Demographic Dataset 
View(Demographic_data)

###############################################################################################
###############################################################################################
# Variable						              # Description                                             #
###############################################################################################
# Application ID				            # Unique ID of the customers                              #
# Age							                  # Age of customer                                         #
# Gender                		        # Gender of customer                                      #
# Marital Status                    # Marital status of customer (at the time of application) #
# No of dependents                  # No. of childrens of customers                           #
# Income                            # Income of customers                                     #
# Education                         # Education of customers                                  #
# Profession                        # Profession of customers                                 #
# Type of residence                 # Type of residence of customers                          #
# No of months in current residence # No of months in current residence of customers          #
# No of months in current company   # No of months in current company of customers            #
# Performance Tag                   # Status of customer performance (1 represents "Default") #
###############################################################################################

# Checking Dimension of Demographic Dataset.
dim(Demographic_data) 
# 71295 rows X 12 columns

# Exploring Demographic Dataset.
summary(Demographic_data)

# Identify the Null Values or NA Values.
sum(is.na(Demographic_data))
# There are Total 1428 Null Values found in the Demographic Dataset.

# Checking which Column has NA or NULL Values in the Demographic Dataset.
sapply(Demographic_data, function(x) sum(is.na(x)))
# Performance Tag has 1425 & No.of.dependents has 3 NA or Null Value  

# Checking unique Application IDs
length(unique(Demographic_data$Application.ID))
# Total Number of Rows are 71295 and Unique Application IDs are 71292 which mean there are 3 duplicate records in Demographic Dataset.

# Looking for Duplicate Application IDs in the Demographic Dataset.
sum(duplicated(Demographic_data$Application.ID))
Demographic_data_Duplicate_Id <- Demographic_data$Application.ID[duplicated(Demographic_data$Application.ID)]
# Three Duplicate Records found in Demographic Dataset. Application Id:765011468 , 653287861 , 671989187
Demographic_data_Duplicate_Record <- Demographic_data[Demographic_data$Application.ID %in% Demographic_data_Duplicate_Id,]
# As the number of duplicates are very less compared to the size of the dataset, removing the duplicate records

# Removing the Duplicate Records
Demographic_data <- Demographic_data[!duplicated(Demographic_data$Application.ID),]

# Removing Outliers from Demographic Dataset
# Fixing the Negative Values for Age Column which are Outliers.
quantile(Demographic_data$Age,seq(0,1,0.01))

# Box Plot for Variable Age
boxplot(Demographic_data$Age)

# Checking Which Value is Outlier
boxplot.stats(Demographic_data$Age)$out

# We can see a jump in the Age variable from -3 to 27 and Age cannot be negative, therefore we are treating this outlier.
Demographic_data$Age[which(Demographic_data$Age < 27)]<-27

# Validating whether Outlier was Treated Properly or Not
boxplot(Demographic_data$Age)

# Checking if the No of dependents variable has Outlier
quantile(Demographic_data$No.of.dependents,seq(0,1,0.01),na.rm = TRUE)

# Box Plot for Variable No of dependents
boxplot(Demographic_data$No.of.dependents)

# Checking Which Value is Outlier
boxplot.stats(Demographic_data$No.of.dependents)$out

# Checking if the Income variable has Outliers.
quantile(Demographic_data$Income, seq(0,1,0.01))

# Box Plot for Variable Income
boxplot(Demographic_data$Income)

# Checking Which Value is Outlier
boxplot.stats(Demographic_data$Income)$out

# We can see a jump in the Income variable from -0.5 to 4.5 and Income cannot be negative, therefore we are treating this as an outlier.
Demographic_data$Income[which(Demographic_data$Income < 4.5)]<-4.5

# Validating whether Outlier was Treated Properly or Not
boxplot(Demographic_data$Income)

# Checking if the No of month in current residence variable has Outlier
quantile(Demographic_data$No.of.months.in.current.residence,seq(0,1,0.01))

# Box Plot for Variable No of month in current residence
boxplot(Demographic_data$No.of.months.in.current.residence)

# Checking Which Value is Outlier
boxplot.stats(Demographic_data$No.of.months.in.current.residence)$out

# Checking if the No of month in current company variable has Outlier
quantile(Demographic_data$No.of.months.in.current.company,seq(0,1,0.01))

# Box Plot for Variable No of month in current company
boxplot(Demographic_data$No.of.months.in.current.company)

# Checking Which Value is Outlier
boxplot.stats(Demographic_data$No.of.months.in.current.company)$out

# We can see a jump in the No of month in current company variable from 98 to 133, therefore we are treating this as an outlier.
Demographic_data$No.of.months.in.current.company[which(Demographic_data$No.of.months.in.current.company > 98)]<-98

# Validating whether Outlier was Treated Properly or Not
boxplot(Demographic_data$No.of.months.in.current.company)

# Converting data to category variables.
#Demographic_data$No.of.dependents <- as.factor(Demographic_data$No.of.dependents)
#Demographic_data$Performance.Tag <- as.factor(Demographic_data$Performance.Tag)
#summary(Demographic_data)

# Univariate Analysis
univariate_continuous(Demographic_data,Demographic_data$Age,"Age Distribution")
# 76% is Male Population and 24% is Female Population
univariate_categorical(Demographic_data,Demographic_data$Gender,"Gender Distribution")
# High Distribution of Age Ranging from 24 to 65 Years 
univariate_categorical(Demographic_data,Demographic_data$Marital.Status..at.the.time.of.application.,"Maritial Status Distribution")
# 85% of Population are Married and 15% Population are Single
univariate_categorical(Demographic_data,Demographic_data$No.of.dependents,"Dependents Distribution")
# Dependent 1 has 21.6%, Dependent 2 has 21.4%, Dependent 3 has 22.8%, Dependent 4 has 17.1%, Dependent 1 has 17%
univariate_continuous(Demographic_data,Demographic_data$Income,"Income Distribution")
# High Distribution of Income Ranging from 0 to 60
univariate_categorical(Demographic_data,Demographic_data$Education,"Education Distribution")
# 35% are Professional 34% are Masters 25% are Bachelor 6.4% are PHD and Others are 0.2%
univariate_categorical(Demographic_data,Demographic_data$Profession,"Profession Distribution")
# 57% are SAL 23.2% are SE_PROF 20.1% are SE
univariate_categorical(Demographic_data,Demographic_data$Type.of.residence,"Residence Type Distribution")
# 75% are Rented 20% are Owned 2.6% are Living with Parents 2.3% are Company Provided 0.3% are Others
univariate_continuous(Demographic_data,Demographic_data$No.of.months.in.current.residence,"Current Residence Distribution")
# High Distribution of Current Residence Ranging from 0 to 120
univariate_continuous(Demographic_data,Demographic_data$No.of.months.in.current.company,"Current Company Distribution")
# High Distribution of Current Company Ranging from 0 to 75
univariate_categorical(Demographic_data,Demographic_data$Performance.Tag,"Performance Distribution")
# 94% are 0 4% are 1 and 2% are NA




# Read the Credit Bureau Datasets
Credit_Bureau_data <- read.csv("Credit Bureau data.csv")

# View Credit Bureau Dataset 
View(Credit_Bureau_data)

##################################################################################################################################################
# Variable						                                           # Description                                                                   #
##################################################################################################################################################
# Application ID				                                         # Customer application ID 														                           #                          
# No of times 90 DPD or worse in last 6 months                   # Number of times customer has not payed dues since 90days in last 6 months	   #
# No of times 60 DPD or worse in last 6 months                   # Number of times customer has not payed dues since 60 days last 6 months       #
# No of times 30 DPD or worse in last 6 months                   # Number of times customer has not payed dues since 30 days days last 6 months  #
# No of times 90 DPD or worse in last 12 months                  # Number of times customer has not payed dues since 90 days days last 12 months #
# No of times 60 DPD or worse in last 12 months                  # Number of times customer has not payed dues since 60 days days last 12 months #
# No of times 30 DPD or worse in last 12 months                  # Number of times customer has not payed dues since 30 days days last 12 months #
# Avgas CC Utilization in last 12 months                         # Average utilization of credit card by customer                                #
# No of trades opened in last 6 months                           # Number of times the customer has done the trades in last 6 months             #
# No of trades opened in last 12 months                          # Number of times the customer has done the trades in last 12 months            #
# No of PL trades opened in last 6 months  	                     # No of PL trades in last 6 month  of customer                                  #
# No of PL trades opened in last 12 months                       # No of PL trades in last 12 month  of customer                                 #
# No of Inquiries in last 6 months (excluding home & auto loans) # Number of times the customers has inquired in last 6 months                   #
# No of Inquiries in last 12 months (excluding home & auto loans)# Number of times the customers has inquired in last 12 months                  #
# Presence of open home loan									                   # Is the customer has home loan (1 represents "Yes")                            #
# Outstanding Balance											                       # Outstanding balance of customer                                               #
# Total No of Trades											                       # Number of times the customer has done total trades                            #
# Presence of open auto loan                                     # Is the customer has auto loan (1 represents "Yes")                            #
# Performance Tag												                         # Status of customer performance (" 1 represents "Default")			         #
##################################################################################################################################################

# Checking Dimension of Credit Bureau Dataset.
dim(Credit_Bureau_data) 
# 71295 rows X 19 columns

# Exploring Credit Bureau Dataset.
summary(Credit_Bureau_data)

# Identify the Null Values or NA Values.
sum(is.na(Credit_Bureau_data))
# There are Total 3028 Null Values found in the Credit Bureau Dataset.

# Checking which Column has NA or NULL Values in the Credit Bureau Dataset.
sapply(Credit_Bureau_data, function(x) sum(is.na(x)))
# Avgas CC Utilization in last 12 months has 1058 NA or Null Value, 
# No of trades opened in last 6 months 1 NA or Null Value,
# Presence of open home loan has 272 NA or Null Value,
# Outstanding Balance has 272 NA or Null Value, 
# Performance Tag has 1425 NA or Null Value

# Checking unique Application IDs
length(unique(Credit_Bureau_data$Application.ID))
# Total Number of Rows are 71295 and Unique Application IDs are 71292 which mean there are 3 duplicate records in Credit Bureau Dataset.

# Looking for Duplicate Application IDs in the Credit Bureau Dataset.
sum(duplicated(Credit_Bureau_data$Application.ID))
Credit_Bureau_data_Duplicate_Id <- Credit_Bureau_data$Application.ID[duplicated(Credit_Bureau_data$Application.ID)]
# Three Duplicate Records found in Credit Bureau Dataset. Application Id:765011468 , 653287861 , 671989187
Credit_Bureau_data_Duplicate_Record <- Credit_Bureau_data[Credit_Bureau_data$Application.ID %in% Credit_Bureau_data_Duplicate_Id,]
# As the number of duplicates are very less compared to the size of the dataset, removing the duplicate records

# Removing the Duplicate Records
Credit_Bureau_data <- Credit_Bureau_data[!duplicated(Credit_Bureau_data$Application.ID),]

# Checking if the No of times 90 DPD or worse in last 6 months variable has Outlier
quantile(Credit_Bureau_data$No.of.times.90.DPD.or.worse.in.last.6.months,seq(0,1,0.01))

# Box Plot for Variable No of times 90 DPD or worse in last 6 months
boxplot(Credit_Bureau_data$No.of.times.90.DPD.or.worse.in.last.6.months)

# Checking Which Value is Outlier
boxplot.stats(Credit_Bureau_data$No.of.times.90.DPD.or.worse.in.last.6.months)$out
# 1 2 3 are the Outlier that we see here

# Checking if the No of times 60 DPD or worse in last 6 months variable has Outlier
quantile(Credit_Bureau_data$No.of.times.60.DPD.or.worse.in.last.6.months,seq(0,1,0.01))

# Box Plot for Variable No of times 60 DPD or worse in last 6 months
boxplot(Credit_Bureau_data$No.of.times.60.DPD.or.worse.in.last.6.months)

# Checking Which Value is Outlier
boxplot.stats(Credit_Bureau_data$No.of.times.60.DPD.or.worse.in.last.6.months)$out
# 3 4 5 Are the Outlier we se here

# Checking if the No of times 30 DPD or worse in last 6 months variable has Outlier
quantile(Credit_Bureau_data$No.of.times.30.DPD.or.worse.in.last.6.months,seq(0,1,0.01))

# Box Plot for Variable No of times 30 DPD or worse in last 6 months
boxplot(Credit_Bureau_data$No.of.times.30.DPD.or.worse.in.last.6.months)

# Checking Which Value is Outlier
boxplot.stats(Credit_Bureau_data$No.of.times.30.DPD.or.worse.in.last.6.months)$out
# 3 4 5 6 7 Are the Outlier that we see here

# Checking if the No of times 90 DPD or worse in last 12 months variable has Outlier
quantile(Credit_Bureau_data$No.of.times.90.DPD.or.worse.in.last.12.months,seq(0,1,0.01))

# Box Plot for Variable No of times 90 DPD or worse in last 12 months
boxplot(Credit_Bureau_data$No.of.times.90.DPD.or.worse.in.last.12.months)

# Checking Which Value is Outlier
boxplot.stats(Credit_Bureau_data$No.of.times.90.DPD.or.worse.in.last.12.months)$out
# 3 4 5 Are the Outlier that we see here

# Checking if the No of times 60 DPD or worse in last 12 months variable has Outlier
quantile(Credit_Bureau_data$No.of.times.60.DPD.or.worse.in.last.12.months,seq(0,1,0.01))

# Box Plot for Variable No of times 60 DPD or worse in last 12 months
boxplot(Credit_Bureau_data$No.of.times.60.DPD.or.worse.in.last.12.months)

# Checking Which Value is Outlier
boxplot.stats(Credit_Bureau_data$No.of.times.60.DPD.or.worse.in.last.12.months)$out
# 3 4 5 6 7 Are the Outlier that we see here

# Checking if the No of times 30 DPD or worse in last 12 months variable has Outlier
quantile(Credit_Bureau_data$No.of.times.30.DPD.or.worse.in.last.12.months,seq(0,1,0.01))

# Box Plot for Variable No of times 30 DPD or worse in last 12 months
boxplot(Credit_Bureau_data$No.of.times.30.DPD.or.worse.in.last.12.months)

# Checking Which Value is Outlier
boxplot.stats(Credit_Bureau_data$No.of.times.30.DPD.or.worse.in.last.12.months)$out
# 3 4 5 6 7 8 9 Are the Outlier that we see here

# Checking if the Avgas CC Utilization in last 12 months variable has Outlier
quantile(Credit_Bureau_data$Avgas.CC.Utilization.in.last.12.months,seq(0,1,0.01),na.rm = TRUE)

# Box Plot for Variable Avgas CC Utilization in last 12 months
boxplot(Credit_Bureau_data$Avgas.CC.Utilization.in.last.12.months)

# Checking Which Value is Outlier
boxplot.stats(Credit_Bureau_data$Avgas.CC.Utilization.in.last.12.months)$out
# 103 104 105 106 107 108 109 110 111 112 113 Are the Outlier that we see here

# Checking if the No of trades opened in last 6 months variable has Outlier
quantile(Credit_Bureau_data$No.of.trades.opened.in.last.6.months,seq(0,1,0.01),na.rm = TRUE)

# Box Plot for Variable No of trades opened in last 6 months
boxplot(Credit_Bureau_data$No.of.trades.opened.in.last.6.months)

# Checking Which Value is Outlier
boxplot.stats(Credit_Bureau_data$No.of.trades.opened.in.last.6.months)$out
# 7 8 9 10 11 12 Are the Outlier that we see here

# Checking if the No of trades opened in last 12 months variable has Outlier
quantile(Credit_Bureau_data$No.of.trades.opened.in.last.12.months,seq(0,1,0.01))

# Box Plot for Variable No of trades opened in last 12 months
boxplot(Credit_Bureau_data$No.of.trades.opened.in.last.12.months)

# Checking Which Value is Outlier
boxplot.stats(Credit_Bureau_data$No.of.trades.opened.in.last.12.months)$out
# 20 21 22 23 24 25 26 27 28 Are the Outlier that we see here

# Checking if the No of PL trades opened in last 6 months variable has Outlier
quantile(Credit_Bureau_data$No.of.PL.trades.opened.in.last.6.months,seq(0,1,0.01))

# Box Plot for Variable No of PL trades opened in last 6 months
boxplot(Credit_Bureau_data$No.of.PL.trades.opened.in.last.6.months)

# Checking Which Value is Outlier
boxplot.stats(Credit_Bureau_data$No.of.PL.trades.opened.in.last.6.months)$out
# 6 Are the Outlier that we see here

# Checking if the No of PL trades opened in last 12 months variable has Outlier
quantile(Credit_Bureau_data$No.of.PL.trades.opened.in.last.12.months,seq(0,1,0.01))

# Box Plot for Variable No of PL trades opened in last 12 months
boxplot(Credit_Bureau_data$No.of.PL.trades.opened.in.last.12.months)

# Checking Which Value is Outlier
boxplot.stats(Credit_Bureau_data$No.of.PL.trades.opened.in.last.12.months)$out
# 11 12 Are the Outlier that we see here

# Checking if the No of Inquiries in last 6 months (excluding home & auto loans) variable has Outlier
quantile(Credit_Bureau_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.,seq(0,1,0.01))

# Box Plot for Variable No of Inquiries in last 6 months (excluding home & auto loans)
boxplot(Credit_Bureau_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)

# Checking Which Value is Outlier
boxplot.stats(Credit_Bureau_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)$out
# 8 9 10 Are the Outlier that we see here

# Checking if the No of Inquiries in last 12 months (excluding home & auto loans) variable has Outlier
quantile(Credit_Bureau_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.,seq(0,1,0.01))

# Box Plot for Variable No of Inquiries in last 12 months (excluding home & auto loans)
boxplot(Credit_Bureau_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)

# Checking Which Value is Outlier
boxplot.stats(Credit_Bureau_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)$out
# 13 14 15 16 17 18 19 20 Are the Outlier that we see here

# Checking if the Presence of open home loan variable has Outlier
quantile(Credit_Bureau_data$Presence.of.open.home.loan,seq(0,1,0.01),na.rm = TRUE)

# Box Plot for Variable Presence of open home loan
boxplot(Credit_Bureau_data$Presence.of.open.home.loan)

# Checking Which Value is Outlier
boxplot.stats(Credit_Bureau_data$Presence.of.open.home.loan)$out

# Checking if the Outstanding.Balance variable has outliers
quantile(Credit_Bureau_data$Outstanding.Balance, seq(0,1,0.01),na.rm = TRUE)

# Box Plot for Variable Outstanding Balance
boxplot(Credit_Bureau_data$Outstanding.Balance)

# Checking Which Value is Outlier
boxplot.stats(Credit_Bureau_data$Outstanding.Balance)$out

# Checking if the Total No of Trades variable has outliers
quantile(Credit_Bureau_data$Total.No.of.Trades, seq(0,1,0.01))

# Box Plot for Variable Total No of Trades
boxplot(Credit_Bureau_data$Total.No.of.Trades)

# Checking Which Value is Outlier
boxplot.stats(Credit_Bureau_data$Total.No.of.Trades)$out
# 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 Are the Outlier that we see here

# Checking if the Presence of open auto loan variable has outliers
quantile(Credit_Bureau_data$Presence.of.open.auto.loan, seq(0,1,0.01))

# Box Plot for Variable Presence of open auto loan
boxplot(Credit_Bureau_data$Presence.of.open.auto.loan)

# Checking Which Value is Outlier
boxplot.stats(Credit_Bureau_data$Presence.of.open.auto.loan)$out
# 1 is the Outlier that we see here
# We cannot Treat above Column from Credit_Bureau_data in Outlier Treatment to avoid Information Loss 

# Univariate Analysis
univariate_categorical(Credit_Bureau_data,Credit_Bureau_data$No.of.times.90.DPD.or.worse.in.last.6.months,"90 DPD Worse in Last 6 Months")
univariate_categorical(Credit_Bureau_data,Credit_Bureau_data$No.of.times.60.DPD.or.worse.in.last.6.months,"60 DPD Worse in Last 6 Months")
univariate_categorical(Credit_Bureau_data,Credit_Bureau_data$No.of.times.30.DPD.or.worse.in.last.6.months,"30 DPD Worse in Last 6 Months")
univariate_categorical(Credit_Bureau_data,Credit_Bureau_data$No.of.times.90.DPD.or.worse.in.last.12.months,"90 DPD Worse in Last 12 Months")
univariate_categorical(Credit_Bureau_data,Credit_Bureau_data$No.of.times.60.DPD.or.worse.in.last.12.months,"60 DPD Worse in Last 12 Months")
univariate_categorical(Credit_Bureau_data,Credit_Bureau_data$No.of.times.30.DPD.or.worse.in.last.12.months,"30 DPD Worse in Last 12 Months")
univariate_categorical(Credit_Bureau_data,Credit_Bureau_data$Avgas.CC.Utilization.in.last.12.months,"Avg CC Utilization in Last 12 Months")
univariate_categorical(Credit_Bureau_data,Credit_Bureau_data$No.of.trades.opened.in.last.6.months,"Trade Opened in Last 6 Months")
univariate_categorical(Credit_Bureau_data,Credit_Bureau_data$No.of.trades.opened.in.last.12.months,"Trade Opened in Last 12 Months")
univariate_categorical(Credit_Bureau_data,Credit_Bureau_data$No.of.PL.trades.opened.in.last.6.months,"PL Trade Opened in Last 6 Months")
univariate_categorical(Credit_Bureau_data,Credit_Bureau_data$No.of.PL.trades.opened.in.last.12.months,"PL Trade Opened in Last 12 Months")
univariate_categorical(Credit_Bureau_data,Credit_Bureau_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.,"Inquires of Home and Auto Loan in Last 6 Months")
univariate_categorical(Credit_Bureau_data,Credit_Bureau_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.,"Inquires of Home and Auto Loan in Last 12 Months")
univariate_categorical(Credit_Bureau_data,Credit_Bureau_data$Presence.of.open.home.loan,"Presence of Home Loan")
univariate_continuous(Credit_Bureau_data,Credit_Bureau_data$Total.No.of.Trades,"Number of Trades")
univariate_categorical(Credit_Bureau_data,Credit_Bureau_data$Presence.of.open.auto.loan,"Presence of Auto Loan")
# Merging Demographic Dataset and Credit Bureau Dataset
merged_df <- merge(Demographic_data,Credit_Bureau_data,by.x = "Application.ID", by.y = "Application.ID")

# Checking Number of Rows in Final Merged Dataset
nrow(merged_df)

# As there are two attributes in the merged data frame "merge_df" (Performance.Tag.x and Performance.Tag.y), we are checking if both carry the same data.
Validation <- merged_df$Performance.Tag.x - merged_df$Performance.Tag.y

# Below function returns the total occurrences if the values are exactly matching including the NAs

sum(ifelse(merged_df$Performance.Tag.x == merged_df$Performance.Tag.y, 1,0)  | ifelse(is.na(merged_df$Performance.Tag.x) & is.na(merged_df$Performance.Tag.y), 1,0))

# Since, the above sum matches the row count of the merged data frame, we can conclude that both columns are identical and one column can be removed.

# Removing the redundant column
merged_df$Performance.Tag.x <- NULL

####################################################################################################################################
#EDA to find the important variables

#Filtering only defaulters data for univariate analysis
#Defaulters <- subset(merged_df, merged_df$Performance.Tag.y==1)

#ggplot(Defaulters, aes(x=Gender))+geom_bar(stat = "count")

#ggplot(Defaulters, aes(x=Marital.Status..at.the.time.of.application.))+geom_bar(stat = "count")

#ggplot(Defaulters, aes(x=factor(No.of.dependents)))+geom_bar(stat = "count")

#Finding no of NAs in EDA_data dataframe
#sum(is.na(EDA_data$Performance.Tag.y))

#Removing records with Performance tag as NA since EDA need to be done on defaulted or non defaulted records
#EDA_data <- subset(EDA_data,!is.na(EDA_data$Performance.Tag.y))

#ggplot(EDA_data, aes(x=Gender,fill=factor(Performance.Tag.y)))+geom_bar(position = "fill")

#ggplot(EDA_data, aes(x=EDA_data$Marital.Status..at.the.time.of.application.,fill=factor(Performance.Tag.y)))+geom_bar(position = "fill")

#ggplot(EDA_data, aes(x=EDA_data$Marital.Status..at.the.time.of.application.,fill=factor(Performance.Tag.y)))+geom_bar(position = "fill")

#ggplot(EDA_data, aes(x=factor(EDA_data$No.of.dependents),fill=factor(Performance.Tag.y)))+geom_bar(position = "fill")

#ggplot(EDA_data, aes(x=factor(EDA_data$Education),fill=factor(Performance.Tag.y)))+geom_bar(position = "fill")

#ggplot(EDA_data, aes(x=factor(EDA_data$Profession),fill=factor(Performance.Tag.y)))+geom_bar(position = "fill")

#ggplot(EDA_data, aes(x=factor(EDA_data$Education),fill=factor(Performance.Tag.y)))+geom_bar(position = "fill")

#ggplot(EDA_data, aes(x=factor(EDA_data$Type.of.residence),fill=factor(Performance.Tag.y)))+geom_bar(position = "fill")

#ggplot(EDA_data, aes(x=factor(EDA_data$Presence.of.open.home.loan),fill=factor(Performance.Tag.y)))+geom_bar(position = "fill")

#ggplot(EDA_data, aes(x=factor(EDA_data$Presence.of.open.auto.loan),fill=factor(Performance.Tag.y)))+geom_bar(position = "fill")

#Based on EDA done for categorical variables in above graphs, there is no specific attribute which has high impact on performance and hence all are included in WOE.

#Taking only defaulters data for EDA on continuous values attributes
#EDA_data_defaulted <- subset(EDA_data,EDA_data$Performance.Tag.y==1)

#ggplot(EDA_data_defaulted) +  geom_histogram(mapping = aes(x = EDA_data_defaulted$Age), binwidth = 5)
#The defaulters ratio is more between age range 30 to 50

#ggplot(EDA_data_defaulted) +  geom_histogram(mapping = aes(x = EDA_data_defaulted$Income), binwidth = 5)
#It is found that as the income keeps increasing, the deault rate keeps decreasing

#ggplot(EDA_data_defaulted) +  geom_histogram(mapping = aes(x = EDA_data_defaulted$No.of.months.in.current.residence), binwidth = 5)
#There is a slight reduce in defaulters ratio as the no of months in current residence increases

#ggplot(EDA_data_defaulted) +  geom_histogram(mapping = aes(x = EDA_data_defaulted$No.of.months.in.current.company), binwidth = 5)
#Defaulters reduce as the no of months in current company increases

#ggplot(EDA_data_defaulted) +  geom_histogram(mapping = aes(x = EDA_data_defaulted$No.of.times.30.DPD.or.worse.in.last.6.months), binwidth = 2)

#ggplot(EDA_data_defaulted) +  geom_histogram(mapping = aes(x = EDA_data_defaulted$No.of.times.30.DPD.or.worse.in.last.12.months), binwidth = 2)

#ggplot(EDA_data_defaulted) +  geom_histogram(mapping = aes(x = EDA_data_defaulted$No.of.times.60.DPD.or.worse.in.last.6.months), binwidth = 2)

#ggplot(EDA_data_defaulted) +  geom_histogram(mapping = aes(x = EDA_data_defaulted$No.of.times.60.DPD.or.worse.in.last.12.months), binwidth = 2)

#ggplot(EDA_data_defaulted) +  geom_histogram(mapping = aes(x = EDA_data_defaulted$No.of.times.90.DPD.or.worse.in.last.6.months), binwidth = 2)

#ggplot(EDA_data_defaulted) +  geom_histogram(mapping = aes(x = EDA_data_defaulted$No.of.times.90.DPD.or.worse.in.last.12.months), binwidth = 2)
#Looks like all the above 6 attributes related to DPD are important factors since the defaulters count has a declining trend as the value increases

#ggplot(EDA_data_defaulted) +  geom_histogram(mapping = aes(x = EDA_data_defaulted$Avgas.CC.Utilization.in.last.12.months), binwidth = 10)
#The number od defaulters decrease as the value Average CC utilization for past 12 month increases

#ggplot(EDA_data_defaulted) +  geom_histogram(mapping = aes(x = EDA_data_defaulted$No.of.trades.opened.in.last.6.months), binwidth = 1)
#The defaulters count increases until the no of trades done on cc in last 6 months = 3 and then reduces                                                                                                                                                        

#ggplot(EDA_data_defaulted) +  geom_histogram(mapping = aes(x = EDA_data_defaulted$No.of.trades.opened.in.last.12.months), binwidth = 1)
#The defaulters count increases until the no of trades done on cc in last 12 months = 10 and then reduces                                                                                                                                                        

#ggplot(EDA_data_defaulted) +  geom_histogram(mapping = aes(x = EDA_data_defaulted$No.of.PL.trades.opened.in.last.6.months), binwidth = 1)
#The defaulters count increases until the no of PL trades done on cc in last 6 months = 3 and then reduces                                                                                                                                                        

#ggplot(EDA_data_defaulted) +  geom_histogram(mapping = aes(x = EDA_data_defaulted$No.of.PL.trades.opened.in.last.12.months), binwidth = 1)
#The defaulters count increases until the no of PL trades done on cc in last 12 months = 6 and then reduces                                                                                                                                                        

#ggplot(EDA_data_defaulted) +  geom_histogram(mapping = aes(x = EDA_data_defaulted$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.), binwidth = 1)
#The defaulters count increases until the no of inquiries in last 6 months = 3 and then reduces                                                                                                                                                        

#ggplot(EDA_data_defaulted) +  geom_histogram(mapping = aes(x = EDA_data_defaulted$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.), binwidth = 1)
#The defaulters count increases until the no of inquiries in last 12 months = 4 and then reduces                                                                                                                                                        

#ggplot(EDA_data_defaulted) +  geom_histogram(mapping = aes(x = EDA_data_defaulted$Outstanding.Balance), binwidth = 500000)
#The defaulters count increases until outstanding balance is 1M and then reduces                                                                                                                                                        

#ggplot(EDA_data_defaulted) +  geom_histogram(mapping = aes(x = EDA_data_defaulted$Total.No.of.Trades), binwidth = 1)
#The defaulters count is maximum between 5 to 10 for total no of trades done and decreases gradually after that                                                                                                                                                        

#From above EDA it is found that all the attributes has impact on performance tag of a customer and hence all the attributes are considered for WOE and IV
##############################################################################################################################################
#------------------------------------END OF EDA------------------------------------------------------------------
# Multivariate Analysis
ggplot(merged_df, aes(x = Avgas.CC.Utilization.in.last.12.months, y = No.of.PL.trades.opened.in.last.12.months, group=Performance.Tag.y, color=Performance.Tag.y))+
  geom_line(stat='summary', fun.y='mean') +  
  geom_point(stat='summary', fun.y='mean')
# No of PL-trades opened is relatively higher for default users.

ggplot(data=merged_df, aes(x=No.of.times.90.DPD.or.worse.in.last.6.months, y=Avgas.CC.Utilization.in.last.12.months, group=Performance.Tag.y, color=Performance.Tag.y))+ 
  geom_line(stat='summary', fun.y='mean') + 
  geom_point(stat='summary', fun.y='mean') 
# For default users Avg-CC-utilization is overall higher , Also CC-usage is going high with increasing DPD values.

ggplot(data=merged_df, aes(x=Total.No.of.Trades, y=Outstanding.Balance, group=Performance.Tag.y, color=Performance.Tag.y))+ 
  geom_line(stat='summary', fun.y='mean') + 
  geom_point(stat='summary', fun.y='mean')
# Total no of trades is overall in higherno nos for default users. 
# Also outstanding balance is relatively higher for most of default users.

ggplot(data=merged_df, aes(x=Income, y=Outstanding.Balance, group=Performance.Tag.y, color=Performance.Tag.y))+ 
  geom_line(stat='summary', fun.y='mean')  
# For defaulters Outstanding balance is higher.
# No upward/downward trend for outstanding balance with increasing income.
# If outstanding is more than 12.5lakh its a matter of concern.

ggplot(merged_df, aes(x = Income, y = Avgas.CC.Utilization.in.last.12.months, group=Performance.Tag.y, color=Performance.Tag.y))+
  geom_line(stat='summary', fun.y='mean') +  
  geom_point(stat='summary', fun.y='mean')
# With increasing income avg-cc-usage decreases for whole population.
#  If avg cc usage is >40 for a low income, >30 for middle income, >25 for higher income,they should be looked at.

ggplot(data=merged_df, aes(x=Income, y=No.of.times.90.DPD.or.worse.in.last.6.months, group=Performance.Tag.y, color=Performance.Tag.y))+ 
  geom_line(stat='summary', fun.y='mean') + 
  geom_point(stat='summary', fun.y='mean')
# With increasing Income, DPD nos are decreasing. 
# Also for defaulting users DPD nos are way higher.
#  High no of defaulters are in lower to medium income range. 

ggplot(data=merged_df, aes(x=Income, y=No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., group=Performance.Tag.y, color=Performance.Tag.y))+ 
  geom_line(stat='summary', fun.y='mean') + 
  geom_point(stat='summary', fun.y='mean') 
# With increase in income no of inquiries are decreasing for non defaulters.
# With increase in income no of inquiries relatively higher for defaulters.

ggplot(data=merged_df, aes(x=No.of.dependents, y=Income, group=Performance.Tag.y, color=Performance.Tag.y))+ 
  geom_line(stat='summary', fun.y='mean') + 
  geom_point(stat='summary', fun.y='mean') 
# Income per no of dependants is very low for defaulters comapared to non-defaulters. 

ggplot(data=merged_df, aes(x=No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., y=Total.No.of.Trades , group=Performance.Tag.y, color=Performance.Tag.y))+ 
  geom_line(stat='summary', fun.y='mean') + 
  geom_point(stat='summary', fun.y='mean') 
#  With increasing no of inquiries in last 12months, 
#  total no of trades increases, then gradually it becomes constant.
#  for default users total no of trades is higher.

# WOE The weight of evidence tells the predictive power of an independent variable in relation to the dependent variable.
# IV measures the strength of that relationship.
# Information Value	 Predictive Power
#    < 0.02	         useless for prediction
#  0.02 to 0.1	     Weak predictor
#  0.1 to 0.3	       Medium predictor
#  0.3 to 0.5	       Strong predictor
#    >0.5	           Suspicious or too good to be true


#Need to perform WOE and IV Analysis

summary(merged_df$Performance.Tag.y)

# Checking the Count of 1 in Performance Tag Column
length(which(merged_df$Performance.Tag.y==1))
# 2947

# Checking the Count of 0 in Performance Tag Column
length(which(merged_df$Performance.Tag.y==0))
# 66920

#Swapping 0 and 1 values for performance tag column since 1 should denote good and 0 should denote bad
for (i in 1:nrow(merged_df)) {
  ifelse(merged_df$Performance.Tag.y[i]==0,merged_df$Performance.Tag.y[i] <- 1,merged_df$Performance.Tag.y[i] <- 0)
}
#Validating Swap Loop
length(which(merged_df$Performance.Tag.y==1))
# 66920
length(which(merged_df$Performance.Tag.y==0))
# 2947

#Remove NAs from Dependant Variable as it won't allow execution of IV functions.
traindata <- subset(merged_df, is.na(merged_df$Performance.Tag.y)==FALSE)

# Converting Performance Tag Column to Numeric
traindata$Performance.Tag.y <- as.numeric(traindata$Performance.Tag.y)

# Generate InfoTables for the variables
IV <- create_infotables(traindata,y="Performance.Tag.y",parallel = TRUE, ncore = 4)

# Getting Summary of IV Dataframe
IV$Summary

#Adding a bar graph to see the important variablles based on IV value
All_IVs <- data.frame(IV$Summary)
All_IVs$Variable <- factor(All_IVs$Variable, levels = All_IVs$Variable[order(-All_IVs$IV)])

# Plotting IV for Predictor Variable
ggplot(All_IVs, aes(x=All_IVs$Variable,y=All_IVs$IV))+geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + geom_hline(yintercept = 0.1, color = "red")  + xlab("Predictor Variable") + ylab("IV") + ggtitle("IV for Predictor variable")

# Copying IV$Summary to Predictor Variable 
predictor_variables <- data.frame(IV$Summary)

######################################################################################
#                     Correlation Analysis                                           #
######################################################################################
# Correlation Analysis to be Done for for all Numer ic Variable inorder to eleminate Income and other correlation element

#Subsetting Predictor Variable which are above Threshold Line (i.e. > 0.1)
predictor_variables <- subset(predictor_variables,predictor_variables$IV >0.1)

# Printing Predictor Variables
predictor_variables
#Avgas.CC.Utilization.in.last.12.months
#No.of.trades.opened.in.last.12.months
#No.of.PL.trades.opened.in.last.12.months
#No.of.Inquiries.in.last.12.months..excluding.home...auto.loans
#Outstanding.Balance
#No.of.times.30.DPD.or.worse.in.last.6.months
#Total.No.of.Trades
#No.of.PL.trades.opened.in.last.6.months
#No.of.times.90.DPD.or.worse.in.last.12.months
#No.of.times.60.DPD.or.worse.in.last.6.months
#No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.
#No.of.times.30.DPD.or.worse.in.last.12.months
#No.of.trades.opened.in.last.6.months
#No.of.times.60.DPD.or.worse.in.last.12.months
#No.of.times.90.DPD.or.worse.in.last.6.months

# Checking Number of NA present in Merged Dataset
sapply(merged_df, function(x) sum(is.na(x)))

# We are checking the NA values in the dataset, once the NA value is found, we are replacing it with the highest bin value
# from the IV table where the WOE values is closest to the WOE value for NA bin. The process is repeated for all 
# important predictor columns where the NA exists.

# Replacing NA value with WOE for Avgas.CC.Utilization.in.last.12.months Field
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

# Validating whether NA`s are treated properly or not`
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

# Checking sixth predictor variable No.of.times.30.DPD.or.worse.in.last.6.months
IV$Tables$No.of.times.30.DPD.or.worse.in.last.6.months

# Checking seventh predictor variable Total.No.of.Trades
IV$Tables$Total.No.of.Trades

# Checking eigth predictor variable No.of.PL.trades.opened.in.last.6.months
IV$Tables$No.of.PL.trades.opened.in.last.6.months

# Checking nineth predictor variable No.of.times.90.DPD.or.worse.in.last.12.months
IV$Tables$No.of.times.90.DPD.or.worse.in.last.12.months

# Checking tenth predictor variable No.of.times.60.DPD.or.worse.in.last.6.months
IV$Tables$No.of.times.60.DPD.or.worse.in.last.6.months

# Checking eleventh predictor variable No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.
IV$Tables$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.

# Checking twelveth predictor variable No.of.times.30.DPD.or.worse.in.last.12.months
IV$Tables$No.of.times.30.DPD.or.worse.in.last.12.months

# Checking thirteenth predictor variable No.of.trades.opened.in.last.6.months
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

# Checking fourteenth predictor variable No.of.times.60.DPD.or.worse.in.last.12.months
IV$Tables$No.of.times.60.DPD.or.worse.in.last.12.months

# Checking fifteenth predictor variable No.of.times.90.DPD.or.worse.in.last.6.months
IV$Tables$No.of.times.90.DPD.or.worse.in.last.6.months

# Updating traindata frame
traindata <- merged_df
# Create a dataframe with the important variables identified and the dependant variable

impvar_df <- traindata[,c(as.vector(predictor_variables$Variable),"Performance.Tag.y")]


# Once the NAs are treated, we are replacing the actual values in the dataset with the corresponding WOE values using the function.

# Replacement using woe_replace function
impvar_df <- woe_replace(impvar_df,IV)

# Modelling Logistic regression
      
########################################################################
# splitting the data between train and test
set.seed(100)

impvar_without_NA_df <- subset(impvar_df,is.na(impvar_df$Performance.Tag.y)==FALSE)

indices = sample.split(impvar_without_NA_df$Performance.Tag.y, SplitRatio = 0.7)

train = impvar_df[indices,]

test = impvar_df[!(indices),]


#Initial model
model_1 = glm(Performance.Tag.y ~ ., data = train, family = "binomial")
summary(model_1)


# Using StepAIC function
model_2<- stepAIC(model_1, direction="both")

summary(model_2)
vif(model_2)


final_model <- model_2

p1 <- plot_infotables(IV, IV$Summary$Variable[1],same_scales = TRUE)
p2 <- plot_infotables(IV, IV$Summary$Variable[2],same_scales = TRUE)
p3 <- plot_infotables(IV, IV$Summary$Variable[4],same_scales = TRUE)
p4 <- plot_infotables(IV, IV$Summary$Variable[6],same_scales = TRUE)

grid.arrange(p1,p2,p3,p4)

#---------------------------------------------------------    

# Predicting probabilities of defaulting for the test data

predictions_logit <- predict(final_model, newdata = test[, -16], type = "response")
summary(predictions_logit)

#--------------------------------------------------------- 

## Model Evaluation: Logistic Regression

# Let's use the probability cutoff of 90.5%.

predicted_Performance_tag <- factor(ifelse(predictions_logit >= 0.905, "no", "yes"))
test$Performance.Tag.y <- factor(ifelse(test$Performance.Tag.y ==1, "no","yes"))

# Creating confusion matrix for identifying the model evaluation.

conf <- confusionMatrix(predicted_Performance_tag, test$Performance.Tag.y, positive = "no")

conf

#-----------------------------------------------------------

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_Performance_tag <- factor(ifelse(predictions_logit >= cutoff, "no", "yes"))
  conf <- confusionMatrix(predicted_Performance_tag, test$Performance.Tag.y, positive = "no")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

#---------------------------------------------------------  
# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.

s = seq(0.905,0.99,length=100)

OUT = matrix(0,100,3)

for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0.905,1,length=5),seq(0.905,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(x="topright",0.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

#---------------------------------------------------------    
cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]

# Let's choose a cutoff value of 95.3% for final model

predicted_Performance_tag <- factor(ifelse(predictions_logit >= .953, "no", "yes"))

conf_final <- confusionMatrix(predicted_Performance_tag, test$Performance.Tag.y, positive = "no")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

#----------------------------End_of_model_buiding----------------------------------------

#----------------------------Creation of application scorecard for merged dataset--------

Application_Card_Merged <- impvar_df

predictions_logit <- predict(final_model, newdata = Application_Card_Merged[, -16], type = "response")
predicted_Performance_tag <- factor(ifelse(predictions_logit >= 0.953, "no", "yes"))

# Appending the probabilities and response variables to the test data

Application_Card_Merged$predicted_probs <- predictions_logit

Application_Card_Merged$predicted_Performance_tag <- predicted_Performance_tag

Application_Card_Merged$predict_NonDefault <- Application_Card_Merged$predicted_probs

Application_Card_Merged$predict_Default <- 1 - Application_Card_Merged$predict_NonDefault

Application_Card_Merged$odds <-  log(Application_Card_Merged$predict_NonDefault/Application_Card_Merged$predict_Default)

# Score = Offset + ( Factor * log(odds) )
# Factor = PDO/ln(2)
# Offset = Score-(Factor*log(odds))
# PDO = 20, Base Score=400, odds = 10

Factor = 20/log(2)
#28.8539

Offset = 400 - (28.8539*log(10))

Application_Card_Merged$Score = Offset + (Factor*Application_Card_Merged$odds)

#Calculating the cut off score for application score

cutoff_odds <- log(0.953/(1-0.953))
cutoff_score <- Offset + (Factor*cutoff_odds)
cutoff_score
#Cut off Score is 420.39

#-------------------------------Demographic data WOE, IV and model building-------
#Need to perform WOE and IV Analysis

summary(Demographic_data$Performance.Tag)

length(which(Demographic_data$Performance.Tag==1))
length(which(Demographic_data$Performance.Tag==0))

Demographic_data$Performance.Tag <- factor(as.character(Demographic_data$Performance.Tag))

#Swapping 0 and 1 values for performance tag column since 1 should denote good and 0 should denote bad
for (i in 1:nrow(Demographic_data)) {
  ifelse(Demographic_data$Performance.Tag[i]==0,Demographic_data$Performance.Tag[i] <- 1,Demographic_data$Performance.Tag[i] <- 0)
}
length(which(Demographic_data$Performance.Tag==1))
length(which(Demographic_data$Performance.Tag==0))

#Remove NAs from Dependant Variable as it won't allow execution of IV functions.
traindata_dem <- subset(Demographic_data, is.na(Demographic_data$Performance.Tag)==FALSE)

traindata_dem$Performance.Tag <- as.numeric(as.character(traindata_dem$Performance.Tag))

# Generate InfoTables for the variables
IV_dem <- create_infotables(traindata_dem,y="Performance.Tag",parallel = TRUE, ncore = 4)

IV_dem$Summary

#Adding a bar graph to see the important variablles based on IV value
All_IV_dem <- data.frame(IV_dem$Summary)
All_IV_dem$Variable <- factor(All_IV_dem$Variable, levels = All_IV_dem$Variable[order(-All_IV_dem$IV)])
ggplot(All_IV_dem, aes(x=All_IV_dem$Variable,y=All_IV_dem$IV))+geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + geom_hline(yintercept = 0.1, color = "red")  + xlab("Predictor Variable") + ylab("IV") + ggtitle("IV for Predictor variable")

#Since the IV values of all attributes lie below 0.1, we are considering weak indicators i.e 0.02 to 0.1 for model building
predictor_variables_dem <- data.frame(IV_dem$Summary)
predictor_variables_dem <- subset(predictor_variables_dem,predictor_variables_dem$IV >0.02)

ggplot(All_IV_dem, aes(x=All_IV_dem$Variable,y=All_IV_dem$IV))+geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + geom_hline(yintercept = 0.02, color = "red")  + xlab("Predictor Variable") + ylab("IV") + ggtitle("IV for Predictor variable")

predictor_variables_dem

sapply(Demographic_data, function(x) sum(is.na(x)))

# Checking the first predictor variable No.of.months.in.current.residence

IV_dem$Tables$No.of.months.in.current.residence

# Checking the second predictor variable Income

IV_dem$Tables$Income

# Checking the third predictor variable No.of.months.in.current.company

IV_dem$Tables$No.of.months.in.current.company

traindata_dem <- Demographic_data

# Create a dataframe with the important variables identified and the dependant variable

impvar_dem_df <- traindata_dem[,c(as.vector(predictor_variables_dem$Variable),"Performance.Tag")]

# WOE replacement in the impvar_df to be carried out.

# Replacing the woe values for predictor variables using the function

impvar_dem_df <- woe_replace(impvar_dem_df,IV_dem)

# Modelling Logistic regression for Demographic dataset

########################################################################
# splitting the data between train and test
set.seed(100)

impvar_dem_without_NA_df <- subset(impvar_dem_df,is.na(impvar_dem_df$Performance.Tag)==FALSE)

indices = sample.split(impvar_dem_without_NA_df$Performance.Tag, SplitRatio = 0.7)

train = impvar_dem_without_NA_df[indices,]

test = impvar_dem_without_NA_df[!(indices),]

#Initial model
model_1 = glm(Performance.Tag ~ ., data = train, family = "binomial")
summary(model_1)

# Using StepAIC function
model_2<- stepAIC(model_1, direction="both")

summary(model_2)
vif(model_2)

final_model_dem <- model_2

p1_dem <- plot_infotables(IV_dem, IV_dem$Summary$Variable[1],same_scales = TRUE)
p2_dem <- plot_infotables(IV_dem, IV_dem$Summary$Variable[2],same_scales = TRUE)
p3_dem <- plot_infotables(IV_dem, IV_dem$Summary$Variable[3],same_scales = TRUE)
grid.arrange(p1_dem,p2_dem, p3_dem)

#---------------------------------------------------------    

# Predicting probabilities of defaulting for the test data

predictions_logit_dem <- predict(final_model_dem, newdata = test[, -4], type = "response")
summary(predictions_logit_dem)

#--------------------------------------------------------- 

## Model Evaluation: Logistic Regression

# Let's use the probability cutoff of 93%.

predicted_Performance_tag <- factor(ifelse(predictions_logit_dem >= 0.93, "no", "yes"))
test$Performance.Tag <- factor(ifelse(test$Performance.Tag ==1, "no","yes"))

# Creating confusion matrix for identifying the model evaluation.

conf_dem <- confusionMatrix(predicted_Performance_tag, test$Performance.Tag, positive = "no")

conf_dem

#-----------------------------------------------------------

# Let's find out the optimal probalility cutoff 

perform_fn_dem <- function(cutoff) 
{
  predicted_Performance_tag <- factor(ifelse(predictions_logit_dem >= cutoff, "no", "yes"))
  conf <- confusionMatrix(predicted_Performance_tag, test$Performance.Tag, positive = "no")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

#---------------------------------------------------------  
# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.

s = seq(0.93,0.99,length=100)

OUT = matrix(0,100,3)

for(i in 1:100)
{
  OUT[i,] = perform_fn_dem(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0.93,1,length=5),seq(0.93,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(x="topright",0.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

#---------------------------------------------------------    
cutoff_dem <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]

# Let's choose a cutoff value of 95.66% for final model

predicted_Performance_tag <- factor(ifelse(predictions_logit_dem >= .9566, "no", "yes"))

conf_final <- confusionMatrix(predicted_Performance_tag, test$Performance.Tag, positive = "no")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

#----------------------------End_of_model_buiding----------------------------------------

#----------------------------Creation of application scorecard for demographic dataset--------

Application_Card_dem_Merged <- impvar_dem_df

predictions_logit_dem <- predict(final_model_dem, newdata = Application_Card_dem_Merged[, -4], type = "response")
predicted_Performance_tag <- factor(ifelse(predictions_logit_dem >= 0.9566, "no", "yes"))

# Appending the probabilities and response variables to the test data

Application_Card_dem_Merged$predicted_probs <- predictions_logit_dem

Application_Card_dem_Merged$predicted_Performance_tag <- predicted_Performance_tag

Application_Card_dem_Merged$predict_NonDefault <- Application_Card_dem_Merged$predicted_probs

Application_Card_dem_Merged$predict_Default <- 1 - Application_Card_dem_Merged$predict_NonDefault

Application_Card_dem_Merged$Odds <-  log(Application_Card_dem_Merged$predict_NonDefault/Application_Card_dem_Merged$predict_Default)  

# Score = Offset + ( Factor * log(odds) )
# Factor = PDO/ln(2)
# Offset = Score-(Factor*log(odds))
# PDO = 20, Base Score=400, odds = 10

Factor = 20/log(2)
#28.8539

Offset = 400 - (28.8539*log(10))

  Application_Card_dem_Merged$Score = Offset + (Factor*Application_Card_dem_Merged$Odds)  

#Calculating the cut off score for application score

cutoff_odds_dem <- log(0.9566/(1-0.9566))
cutoff_score_dem <- Offset + (Factor*cutoff_odds_dem)
cutoff_score_dem
#Cut off Score is 422.8044

#---------------End of application Score card code for Demographic data----------------

## Model Building- Model 2: Decision Tree

set.seed(100)

impvar_without_NA_df <- subset(impvar_df,is.na(impvar_df$Performance.Tag.y)==FALSE)

indices = sample.split(impvar_without_NA_df$Performance.Tag.y, SplitRatio = 0.7)

train_dt = impvar_without_NA_df[indices,]

test_dt = impvar_without_NA_df[!(indices),]

nrow(train_dt)/nrow(impvar_without_NA_df)

nrow(test_dt)/nrow(impvar_without_NA_df)

#---------------------------------------------------------    

# building a tree with arbitrary minsplit and cp
tree_1 <-  rpart(Performance.Tag.y ~ ., data=train_dt,control=rpart.control(minsplit=5,cp=0.0001),method = "class")
plot(tree_1)

# This is clearly an overfitted tree

# Increasing the minsplit two fold to 10 
tree_2 <-  rpart(Performance.Tag.y ~ ., data=train_dt,control=rpart.control(minsplit=10,cp=0.0001),method = "class")
plot(tree_2)

# This one is better, but still looks a little too complex

fancyRpartPlot(tree_2)

# Listing the variables by importance:
tree_2$variable.importance

# Increasing the minsplit two fold to 15 
tree_3 <-  rpart(Performance.Tag.y ~ ., data=train_dt,control=rpart.control(minsplit=15,cp=0.0001),method = "class")
plot(tree_3)

# Increasing the minsplit for tree_6
tree_4 <- rpart(Performance.Tag.y ~ ., data=train_dt,control=rpart.control(minsplit=18,cp=0.0001),method = "class")
plot(tree_4)
fancyRpartPlot(tree_4)
tree_4$variable.importance

#---------------------------------------------------------    
## Model Evaluation for tree_3 and tree_3
# using test data from now on
# tree_4
tree_3_pred <- predict(tree_3, test_dt[, -16],type = "class")
tree_3_pred <- factor(ifelse(tree_3_pred==1,"no","yes"))
test_dt$Performance.Tag.y <- factor(ifelse(test_dt$Performance.Tag.y==1,"no","yes"))
confusionMatrix(tree_3_pred, test_dt[, 16], positive = "no")

# Accuracy is 95.65%, sensitivity is 99.85%, specificity is 0.3%

# tree_4
tree_4_pred <- predict(tree_4, test_dt[, -16], type = "class")
tree_4_pred <- factor(ifelse(tree_4_pred==1,"no","yes"))
confusionMatrix(tree_4_pred, test_dt[, 16], positive = "no")

# Accuracy is 95.73%, sensitivity is 99.99%, specificity is 0%

# the model is much better than logistic regression since the accuracy increases significantly.
# But we should need to build a random forest model since specificity is affected immensely.

#Lets balance the data using SMOTE and try building the model for the balanced dataset

summary(factor(train_dt$Performance.Tag.y))
train_dt$Performance.Tag.y <- as.factor(train_dt$Performance.Tag.y)

Smoted_train_dt <- SMOTE(Performance.Tag.y~.,data = train_dt,perc.over = 120,perc.under = 200)

summary(Smoted_train_dt$Performance.Tag.y)

tree_5 <- rpart(Performance.Tag.y~.,data = Smoted_train_dt,control = rpart.control(minsplit = 15,cp=0.0001),method = "class")

tree_5_pred <- predict(tree_5,test_dt[,-16],type = "class")
tree_5_pred <- factor(ifelse(tree_5_pred==1,"no","yes"))
Smoted_train_dt$Performance.Tag.y <- factor(ifelse(Smoted_train_dt$Performance.Tag.y==1,"no","yes"))
confusionMatrix(tree_5_pred,test_dt[,16],positive = "no")

#Accuracy = 0.70
#Sensitivity = 0.71
#Specificity = 0.43
#Even theough accuracy decreases, the model provides a better specificity compared to model built on unbalanced data
#---------------------------------------------------------    

#Model Building - Random Forest

# Spliting the data in 70:30 ratio

set.seed(100)

impvar_without_NA_df$Performance.Tag.y <- as.factor(ifelse(impvar_without_NA_df$Performance.Tag.y ==1,"no","yes"))
split_indices <- sample.split(impvar_without_NA_df$Performance.Tag.y, SplitRatio = 0.70)

train_rf <- impvar_without_NA_df[split_indices, ]

test_rf <- impvar_without_NA_df[!split_indices, ]

nrow(train_rf)/nrow(impvar_without_NA_df)

nrow(test_rf)/nrow(impvar_without_NA_df)

#---------------------------------------------------------    

# Building the model 

Credit_rf <- randomForest(Performance.Tag.y ~., data = train_rf, proximity = F, do.trace = T, mtry = 5)

# Predict response for test data

rf_pred <- predict(Credit_rf, test_rf[, -16], type = "prob")

#---------------------------------------------------------    

# Cutoff for randomforest to assign yes or no

perform_fn_rf <- function(cutoff) 
{
  predicted_Performance_tag <- as.factor(ifelse(rf_pred[, 1] >= cutoff, "no", "yes"))
  conf <- confusionMatrix(predicted_Performance_tag, test_rf$Performance.Tag.y, positive = "no")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT_rf <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT_rf) <- c("sensitivity", "specificity", "accuracy")
  return(OUT_rf)
}

#---------------------------------------------------------    

# creating cutoff values from 0.001 to 0.698 for plotting and initialising a matrix of size 1000x4
s = seq(.96,.99,length=100)

OUT_rf = matrix(0,100,3)

# calculate the sens, spec and acc for different cutoff values

for(i in 1:100)
{
  OUT_rf[i,] = perform_fn_rf(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs

plot(s, OUT_rf[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0.96,0.99,length=5),seq(0.96,0.99,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT_rf[,2],col="darkgreen",lwd=2)
lines(s,OUT_rf[,3],col=4,lwd=2)
box()

#legend(x="topright",0.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff_rf <- s[which(abs(OUT_rf[,1]-OUT_rf[,2])<0.01)]
OUT_rf
# The plot shows that cutoff value of around 98.3% optimises sensitivity and accuracy

predicted_Performance_tag <- factor(ifelse(rf_pred[, 1] >= 0.97, "no", "yes"))

conf_forest <- confusionMatrix(predicted_Performance_tag, test_rf[, 16], positive = "no")

conf_forest

# Sensitivity
conf_forest$byClass[1]
#0.6867

# Specificity 
conf_forest$byClass[2]
#0.5169

# Accuracy 
conf_forest$overall[1]
#0.67

# Final RF important variables
importance <- Credit_rf$importance 

importance <- data.frame(importance)


#-----------------Random forest model for Balanced data-------------------

set.seed(100)

impvar_without_NA_df <- subset(impvar_df,is.na(impvar_df$Performance.Tag.y)==FALSE)

impvar_without_NA_df$Performance.Tag.y <- as.factor(ifelse(impvar_without_NA_df$Performance.Tag.y ==1,"no","yes"))
split_indices <- sample.split(impvar_without_NA_df$Performance.Tag.y, SplitRatio = 0.70)

train_rf <- impvar_without_NA_df[split_indices, ]

test_rf <- impvar_without_NA_df[!split_indices, ]

nrow(train_rf)/nrow(impvar_without_NA_df)

nrow(test_rf)/nrow(impvar_without_NA_df)

#balncing the data using SMOTE function

summary(factor(train_rf$Performance.Tag.y))
train_rf$Performance.Tag.y <- as.factor(train_rf$Performance.Tag.y)

Smoted_train_rf <- SMOTE(Performance.Tag.y~.,data = train_rf,perc.over = 100,perc.under = 200,k=20)

summary(Smoted_train_rf$Performance.Tag.y)

tuneRF(Smoted_train_rf[,-16],Smoted_train_rf[,16])

# Building the model 

Credit_rf <- randomForest(Performance.Tag.y ~., data = Smoted_train_rf, proximity = F, do.trace = T, mtry = 6,nodesize=42)

# Predict response for test data

rf_pred <- predict(Credit_rf, test_rf[, -16], type = "prob")

#---------------------------------------------------------    

# Cutoff for randomforest to assign yes or no

perform_fn_rf <- function(cutoff) 
{
  predicted_Performance_tag <- as.factor(ifelse(rf_pred[, 1] >= cutoff, "no", "yes"))
  conf <- confusionMatrix(predicted_Performance_tag, test_rf$Performance.Tag.y, positive = "no")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT_rf <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT_rf) <- c("sensitivity", "specificity", "accuracy")
  return(OUT_rf)
}

#---------------------------------------------------------    

# creating cutoff values from 0.001 to 0.698 for plotting and initialising a matrix of size 1000x4
s = seq(.01,.99,length=100)

OUT_rf = matrix(0,100,3)

# calculate the sens, spec and acc for different cutoff values

for(i in 1:100)
{
  OUT_rf[i,] = perform_fn_rf(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs

plot(s, OUT_rf[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0.01,0.99,length=5),seq(0.01,0.99,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT_rf[,2],col="darkgreen",lwd=2)
lines(s,OUT_rf[,3],col=4,lwd=2)
box()

#legend(x="topright",0.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff_rf <- s[which(abs(OUT_rf[,1]-OUT_rf[,2])<0.01)]

# The plot shows that cutoff value of around 62% optimises sensitivity and accuracy

predicted_Performance_tag <- factor(ifelse(rf_pred[, 1] >= 0.62, "no", "yes"))

conf_forest <- confusionMatrix(predicted_Performance_tag, test_rf[, 16], positive = "no")

conf_forest

# Sensitivity
conf_forest$byClass[1]
#0.624

# Specificity 
conf_forest$byClass[2]
#0.630

# Accuracy 
conf_forest$overall[1]
#0.624

# Final RF important variables
importance <- Credit_rf$importance 

importance <- data.frame(importance)


#------------Application Scorecard for balanced data random forest model------------------

Application_Card_rf <- impvar_df

predictions_rf <- predict(Credit_rf, newdata = Application_Card_rf[, -16], type = "prob")
predicted_Performance_tag <- factor(ifelse(predictions_rf[,1] >= 0.62, "no", "yes"))

# Appending the probabilities and response variables to the test data

Application_Card_rf$predicted_probs <- predictions_rf

Application_Card_rf$predicted_Performance_tag <- predicted_Performance_tag

Application_Card_rf$predict_NonDefault <- Application_Card_rf$predicted_probs

Application_Card_rf$predict_Default <- 1 - Application_Card_rf$predict_NonDefault

Application_Card_rf$odds <-  log(Application_Card_rf$predict_NonDefault/Application_Card_rf$predict_Default)

# Score = Offset + ( Factor * log(odds) )
# Factor = PDO/ln(2)
# Offset = Score-(Factor*log(odds))
# PDO = 20, Base Score=400, odds = 10

Factor = 20/log(2)
#28.8539

Offset = 400 - (28.8539*log(10))

Application_Card_rf$Score = Offset + (Factor*Application_Card_rf$odds)

#Calculating the cut off score for application score

cutoff_odds <- log(0.62/(1-0.62))
cutoff_score <- Offset + (Factor*cutoff_odds)
cutoff_score
#Cut off Score is 347.68

#-----------------------------------------------------------------------------------------------------------
# Modelling Logistic regression for balanced data

########################################################################
# splitting the data between train and test
set.seed(100)

impvar_without_NA_df <- subset(impvar_df,is.na(impvar_df$Performance.Tag.y)==FALSE)

indices = sample.split(impvar_without_NA_df$Performance.Tag.y, SplitRatio = 0.7)

train = impvar_without_NA_df[indices,]

test = impvar_without_NA_df[!(indices),]

#balncing the data using SMOTE function

summary(factor(train$Performance.Tag.y))
train$Performance.Tag.y <- as.factor(train$Performance.Tag.y)

Smoted_train_lg <- SMOTE(Performance.Tag.y~.,data = train,perc.over = 100,perc.under = 200,k=20)

summary(Smoted_train_lg$Performance.Tag.y)

#Initial model
model_1 = glm(Performance.Tag.y ~ ., data = Smoted_train_lg, family = "binomial")
summary(model_1)

# Using StepAIC function
model_2<- stepAIC(model_1, direction="both")

summary(model_2)
vif(model_2)

# Removing No.of.PL.trades.opened.in.last.12.months due to high vif and relatively high p-value
model_3 <- glm(formula = Performance.Tag.y ~ Avgas.CC.Utilization.in.last.12.months + 
                 No.of.trades.opened.in.last.12.months + No.of.PL.trades.opened.in.last.12.months + 
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                 Outstanding.Balance + No.of.times.30.DPD.or.worse.in.last.6.months + 
                 No.of.times.90.DPD.or.worse.in.last.12.months + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
                 No.of.times.60.DPD.or.worse.in.last.12.months, 
               family = "binomial", data = Smoted_train_lg)
summary(model_3)
vif(model_3)

# Removing No.of.PL.trades.opened.in.last.12.months due to high p-value and VIF

model_4 <- glm(formula = Performance.Tag.y ~ Avgas.CC.Utilization.in.last.12.months + 
                 No.of.trades.opened.in.last.12.months + 
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                 Outstanding.Balance + No.of.times.30.DPD.or.worse.in.last.6.months + 
                 No.of.times.90.DPD.or.worse.in.last.12.months + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
                 No.of.times.60.DPD.or.worse.in.last.12.months, 
               family = "binomial", data = Smoted_train_lg)
summary(model_4)
vif(model_4)

# Removing No.of.trades.opened.in.last.12.months due to high p-value and VIF

model_5 <- glm(formula = Performance.Tag.y ~ Avgas.CC.Utilization.in.last.12.months + 
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                 Outstanding.Balance + No.of.times.30.DPD.or.worse.in.last.6.months + 
                 No.of.times.90.DPD.or.worse.in.last.12.months + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
                 No.of.times.60.DPD.or.worse.in.last.12.months, 
               family = "binomial", data = Smoted_train_lg)
summary(model_5)
vif(model_5)

# Removing No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. due to high p-value and VIF

model_6 <- glm(formula = Performance.Tag.y ~ Avgas.CC.Utilization.in.last.12.months + 
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                 Outstanding.Balance + No.of.times.30.DPD.or.worse.in.last.6.months + 
                 No.of.times.90.DPD.or.worse.in.last.12.months + 
                 No.of.times.60.DPD.or.worse.in.last.12.months, 
               family = "binomial", data = Smoted_train_lg)
summary(model_6)
vif(model_6)


# Removing No.of.times.60.DPD.or.worse.in.last.12.months due to high p-value and VIF

model_7 <- glm(formula = Performance.Tag.y ~ Avgas.CC.Utilization.in.last.12.months + 
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                 Outstanding.Balance + No.of.times.30.DPD.or.worse.in.last.6.months + 
                 No.of.times.90.DPD.or.worse.in.last.12.months, 
               family = "binomial", data = Smoted_train_lg)

summary(model_7)
vif(model_7)

# Removing No.of.times.90.DPD.or.worse.in.last.12.months due to high p-value and VIF

model_8 <- glm(formula = Performance.Tag.y ~ Avgas.CC.Utilization.in.last.12.months + 
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                 Outstanding.Balance + No.of.times.30.DPD.or.worse.in.last.6.months, 
               family = "binomial", data = Smoted_train_lg)

summary(model_8)
vif(model_8)


final_model <- model_8

#---------------------------------------------------------    

# Predicting probabilities of defaulting for the test data

predictions_logit <- predict(final_model, newdata = test[, -16], type = "response")
summary(predictions_logit)

#--------------------------------------------------------- 

## Model Evaluation: Logistic Regression

# # Let's use the probability cutoff of 90.5%.
# #### Error here. need to investigate
# predicted_Performance_tag <- factor(ifelse(predictions_logit >= 0.905, "no", "yes"))
# test$Performance.Tag.y <- factor(ifelse(test$Performance.Tag.y ==1, "no","yes"))
# summary(test$Performance.Tag.y)
# summary(predicted_Performance_tag)
# # Creating confusion matrix for identifying the model evaluation.
# 
# conf <- confusionMatrix(predicted_Performance_tag, test$Performance.Tag.y, positive = "no")
# 
# conf
# 
# #-----------------------------------------------------------
# 
# # Let's find out the optimal probalility cutoff 
# 
# perform_fn <- function(cutoff) 
# {
#   predicted_Performance_tag <- factor(ifelse(predictions_logit >= cutoff, "no", "yes"))
#   conf <- confusionMatrix(predicted_Performance_tag, test$Performance.Tag.y, positive = "no")
#   acc <- conf$overall[1]
#   sens <- conf$byClass[1]
#   spec <- conf$byClass[2]
#   out <- t(as.matrix(c(sens, spec, acc))) 
#   colnames(out) <- c("sensitivity", "specificity", "accuracy")
#   return(out)
# }
# 
# #---------------------------------------------------------  
# # Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.
# 
# s = seq(0.001,0.99,length=100)
# 
# OUT = matrix(0,100,3)
# 
# for(i in 1:100)
# {
#   OUT[i,] = perform_fn(s[i])
# } 
# 
# #---------------------------------------------------------    
# 
# # plotting cutoffs 
# plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
# axis(1,seq(0.001,1,length=5),seq(0.001,1,length=5),cex.lab=1.5)
# axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
# lines(s,OUT[,2],col="darkgreen",lwd=2)
# lines(s,OUT[,3],col=4,lwd=2)
# box()
# legend(x="topright",0.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))
# 
# #---------------------------------------------------------    
# cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]
# 
# # Let's choose a cutoff value of 46% for final model
# 
# predicted_Performance_tag <- factor(ifelse(predictions_logit >= .46, "no", "yes"))
# 
# conf_final <- confusionMatrix(predicted_Performance_tag, test$Performance.Tag.y, positive = "no")
# 
# acc <- conf_final$overall[1]
# 
# sens <- conf_final$byClass[1]
# 
# spec <- conf_final$byClass[2]
# 
# acc
# 
# sens
# 
# spec

#----------------------------End_of_model_buiding----------------------------------------


h2o.shutdown(prompt = FALSE)
