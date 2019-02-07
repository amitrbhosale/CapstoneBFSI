# CredX - Acquisition Risk Analysis Capstone Project.

#Installing Required Packages
install.packages("rstudioapi")
install.packages("ggplot2")
install.packages("Information")
install.packages("stringr")
install.packages("fuzzyjoin")
library("Information")
library(rstudioapi)
library(ggplot2)
library(stringr)
library(fuzzyjoin)

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

#Since No of dependents has only 6 levels including NA, we are considering it as categorical attribute and setting the NA to 9999
Demographic_data$No.of.dependents[which(is.na(Demographic_data$No.of.dependents))] <- 9999

#Converting data to category variables.

Demographic_data$No.of.dependents <- as.factor(Demographic_data$No.of.dependents)
Demographic_data$Performance.Tag <- as.factor(Demographic_data$Performance.Tag)
summary(Demographic_data)

sapply(Credit_Bureau_data, function(x) sum(is.na(x)))
#No.of.trades.opened.in.last.6.months, Avgas.CC.Utilization.in.last.12.months, Presence.of.open.home.loan, Outstanding.Balance have NA values
#All the above mentioned variables are of type integer.

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

#Finding Outliers

#For Age
boxplot(merged_df$Age)
#There are some values which are negative
quantile(merged_df$Age, probs = seq(0,1,0.01))
#Can't find any specific value which can be replaced for negative value
#Setting the records with negative age value to 18 since it is the min age required
merged_df$Age[which(merged_df$Age < 18)] <- 18

#For Income
boxplot(merged_df$Income)
which(merged_df$Income < 0)
#There are some values which are negative
quantile(merged_df$Income, probs = seq(0,1,0.01))
#Setting the negative value to 4.5 based on output from quantile function
merged_df$Income[which(merged_df$Income < 0)] <- 4.5

#For No.of.months.in.current.residence
boxplot(merged_df$No.of.months.in.current.residence)
which(merged_df$No.of.months.in.current.residence <= 0)

#For No.of.months.in.current.residence
boxplot(merged_df$No.of.months.in.current.company)
quantile(merged_df$No.of.months.in.current.company, probs = seq(0,1,0.01))
#Not replacing the outlier since it is a possible scenario (133 months)

#For No.of.times.90.DPD.or.worse.in.last.6.months
boxplot(merged_df$No.of.times.90.DPD.or.worse.in.last.6.months)
which(merged_df$No.of.times.90.DPD.or.worse.in.last.6.months < 0)
#There are no values which are negative
sum(is.na(merged_df$No.of.times.90.DPD.or.worse.in.last.6.months))
#There are no NA Values
quantile(merged_df$No.of.times.90.DPD.or.worse.in.last.6.months, probs = seq(0,1,0.01))

#For No.of.times.60.DPD.or.worse.in.last.6.months
boxplot(merged_df$No.of.times.60.DPD.or.worse.in.last.6.months)
which(merged_df$No.of.times.60.DPD.or.worse.in.last.6.months < 0)
#There are no values which are negative
sum(is.na(merged_df$No.of.times.60.DPD.or.worse.in.last.6.months))
#There are no NA Values
quantile(merged_df$No.of.times.60.DPD.or.worse.in.last.6.months, probs = seq(0,1,0.01))

#For No.of.times.30.DPD.or.worse.in.last.6.months
boxplot(merged_df$No.of.times.30.DPD.or.worse.in.last.6.months)
which(merged_df$No.of.times.30.DPD.or.worse.in.last.6.months < 0)
#There are no values which are negative
sum(is.na(merged_df$No.of.times.30.DPD.or.worse.in.last.6.months))
#There are no NA Values
quantile(merged_df$No.of.times.30.DPD.or.worse.in.last.6.months, probs = seq(0,1,0.01))

#For No.of.times.90.DPD.or.worse.in.last.12.months
boxplot(merged_df$No.of.times.90.DPD.or.worse.in.last.12.months)
which(merged_df$No.of.times.90.DPD.or.worse.in.last.12.months < 0)
#There are no values which are negative
sum(is.na(merged_df$No.of.times.90.DPD.or.worse.in.last.12.months))
#There are no NA Values
quantile(merged_df$No.of.times.90.DPD.or.worse.in.last.12.months, probs = seq(0,1,0.01))

#For No.of.times.60.DPD.or.worse.in.last.12.months
boxplot(merged_df$No.of.times.60.DPD.or.worse.in.last.12.months)
which(merged_df$No.of.times.60.DPD.or.worse.in.last.12.months < 0)
#There are no values which are negative
sum(is.na(merged_df$No.of.times.60.DPD.or.worse.in.last.12.months))
#There are no NA Values
quantile(merged_df$No.of.times.60.DPD.or.worse.in.last.12.months, probs = seq(0,1,0.01))

#For No.of.times.30.DPD.or.worse.in.last.12.months
boxplot(merged_df$No.of.times.30.DPD.or.worse.in.last.12.months)
which(merged_df$No.of.times.30.DPD.or.worse.in.last.12.months < 0)
#There are no values which are negative
sum(is.na(merged_df$No.of.times.30.DPD.or.worse.in.last.12.months))
#There are no NA Values
quantile(merged_df$No.of.times.30.DPD.or.worse.in.last.12.months, probs = seq(0,1,0.01))

#For Avgas.CC.Utilization.in.last.12.months
boxplot(merged_df$Avgas.CC.Utilization.in.last.12.months)
which(merged_df$Avgas.CC.Utilization.in.last.12.months < 0)
#There are no values which are negative
sum(is.na(merged_df$Avgas.CC.Utilization.in.last.12.months))
#There are records with NA Values
quantile(merged_df$Avgas.CC.Utilization.in.last.12.months[!is.na(merged_df$Avgas.CC.Utilization.in.last.12.months)] , probs = seq(0,1,0.01))
#Setting NA values to 90 according to data obtained from quantile
merged_df$Avgas.CC.Utilization.in.last.12.months[is.na(merged_df$Avgas.CC.Utilization.in.last.12.months)] <- 90

#For No.of.trades.opened.in.last.6.months
boxplot(merged_df$No.of.trades.opened.in.last.6.months)
which(merged_df$No.of.trades.opened.in.last.6.months < 0)
#There are no values which are negative
sum(is.na(merged_df$No.of.trades.opened.in.last.6.months))
#There is one record with NA Values
quantile(merged_df$No.of.trades.opened.in.last.6.months[!is.na(merged_df$No.of.trades.opened.in.last.6.months)] , probs = seq(0,1,0.01))
#Setting NA values to 9 according to data obtained from quantile
merged_df$No.of.trades.opened.in.last.6.months[is.na(merged_df$No.of.trades.opened.in.last.6.months)] <- 9

#For No.of.trades.opened.in.last.12.months
boxplot(merged_df$No.of.trades.opened.in.last.12.months)
which(merged_df$No.of.trades.opened.in.last.12.months < 0)
#There are no values which are negative
sum(is.na(merged_df$No.of.trades.opened.in.last.12.months))
#There are no records with NA Values
quantile(merged_df$No.of.trades.opened.in.last.6.months[!is.na(merged_df$No.of.trades.opened.in.last.6.months)] , probs = seq(0,1,0.01))

#For No.of.PL.trades.opened.in.last.6.months
boxplot(merged_df$No.of.PL.trades.opened.in.last.6.months)
which(merged_df$No.of.PL.trades.opened.in.last.6.months < 0)
#There are no values which are negative
sum(is.na(merged_df$No.of.PL.trades.opened.in.last.6.months))
#There are no records with NA Values
quantile(merged_df$No.of.trades.opened.in.last.6.months , probs = seq(0,1,0.01))

#For No.of.PL.trades.opened.in.last.12.months
boxplot(merged_df$No.of.PL.trades.opened.in.last.12.months)
which(merged_df$No.of.PL.trades.opened.in.last.12.months < 0)
#There are no values which are negative
sum(is.na(merged_df$No.of.PL.trades.opened.in.last.12.months))
#There are no records with NA Values
quantile(merged_df$No.of.PL.trades.opened.in.last.12.months , probs = seq(0,1,0.01))

#For No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.
boxplot(merged_df$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)
which(merged_df$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. < 0)
#There are no values which are negative
sum(is.na(merged_df$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.))
#There are no records with NA Values
quantile(merged_df$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. , probs = seq(0,1,0.01))

#For No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.
boxplot(merged_df$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)
which(merged_df$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. < 0)
#There are no values which are negative
sum(is.na(merged_df$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.))
#There are no records with NA Values
quantile(merged_df$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. , probs = seq(0,1,0.01))

#For Outstanding.Balance
boxplot(merged_df$Outstanding.Balance)
which(merged_df$Outstanding.Balance < 0)
#There are no values which are negative
sum(is.na(merged_df$Outstanding.Balance))
#There are records with NA Values
quantile(merged_df$Outstanding.Balance[!is.na(merged_df$Outstanding.Balance)] , probs = seq(0,1,0.01))
mean(merged_df$Outstanding.Balance[!is.na(merged_df$Outstanding.Balance)])
median(merged_df$Outstanding.Balance[!is.na(merged_df$Outstanding.Balance)])
#Generally replacing it with median value is a better approach and hence replacing NAs by 774994.5
merged_df$Outstanding.Balance[is.na(merged_df$Outstanding.Balance)] <- 774994.5

#For Total.No.of.Trades
boxplot(merged_df$Total.No.of.Trades)
which(merged_df$Total.No.of.Trades < 0)
#There are no values which are negative
sum(is.na(merged_df$Total.No.of.Trades))
#There are no records with NA Values
quantile(merged_df$Outstanding.Balance[!is.na(merged_df$Outstanding.Balance)] , probs = seq(0,1,0.01))

sapply(merged_df, function(x) sum(is.na(x)))

#------------------------------------------EDA to find the important variables-----------------------------------

#Filtering only defaulters data
EDA_data <- merged_df

#Finding no of NAs in EDA_data dataframe
sum(is.na(EDA_data$Performance.Tag.y))

#Removing records with Performance tag as NA since EDA need to be done on defaulted or non defaulted records
EDA_data <- subset(EDA_data,!is.na(EDA_data$Performance.Tag.y))

ggplot(EDA_data, aes(x=Gender,fill=factor(Performance.Tag.y)))+geom_bar(position = "fill")

ggplot(EDA_data, aes(x=EDA_data$Marital.Status..at.the.time.of.application.,fill=factor(Performance.Tag.y)))+geom_bar(position = "fill")

ggplot(EDA_data, aes(x=factor(EDA_data$No.of.dependents),fill=factor(Performance.Tag.y)))+geom_bar(position = "fill")

ggplot(EDA_data, aes(x=factor(EDA_data$Education),fill=factor(Performance.Tag.y)))+geom_bar(position = "fill")

ggplot(EDA_data, aes(x=factor(EDA_data$Profession),fill=factor(Performance.Tag.y)))+geom_bar(position = "fill")

ggplot(EDA_data, aes(x=factor(EDA_data$Type.of.residence),fill=factor(Performance.Tag.y)))+geom_bar(position = "fill")

ggplot(EDA_data, aes(x=factor(EDA_data$Presence.of.open.home.loan),fill=factor(Performance.Tag.y)))+geom_bar(position = "fill")

ggplot(EDA_data, aes(x=factor(EDA_data$Presence.of.open.auto.loan),fill=factor(Performance.Tag.y)))+geom_bar(position = "fill")

#Based on EDA done for categorical variables in above graphs, there is no specific attribute which has high impact on performance and hence all are included in WOE.

#Taking only defaulters data for EDA on continuous values attributes
EDA_data_defaulted <- subset(EDA_data,EDA_data$Performance.Tag.y==1)

ggplot(EDA_data_defaulted) +  geom_histogram(mapping = aes(x = EDA_data_defaulted$Age), binwidth = 5)
#The defaulters ratio is more between age range 30 to 50

ggplot(EDA_data_defaulted) +  geom_histogram(mapping = aes(x = EDA_data_defaulted$Income), binwidth = 5)
#It is found that as the income keeps increasing, the deault rate keeps decreasing

ggplot(EDA_data_defaulted) +  geom_histogram(mapping = aes(x = EDA_data_defaulted$No.of.months.in.current.residence), binwidth = 5)
#There is a slight reduce in defaulters ratio as the no of months in current residence increases

ggplot(EDA_data_defaulted) +  geom_histogram(mapping = aes(x = EDA_data_defaulted$No.of.months.in.current.company), binwidth = 5)
#Defaulters reduce as the no of months in current company increases

ggplot(EDA_data_defaulted) +  geom_histogram(mapping = aes(x = EDA_data_defaulted$No.of.times.30.DPD.or.worse.in.last.6.months), binwidth = 2)

ggplot(EDA_data_defaulted) +  geom_histogram(mapping = aes(x = EDA_data_defaulted$No.of.times.30.DPD.or.worse.in.last.12.months), binwidth = 2)

ggplot(EDA_data_defaulted) +  geom_histogram(mapping = aes(x = EDA_data_defaulted$No.of.times.60.DPD.or.worse.in.last.6.months), binwidth = 2)

ggplot(EDA_data_defaulted) +  geom_histogram(mapping = aes(x = EDA_data_defaulted$No.of.times.60.DPD.or.worse.in.last.12.months), binwidth = 2)

ggplot(EDA_data_defaulted) +  geom_histogram(mapping = aes(x = EDA_data_defaulted$No.of.times.90.DPD.or.worse.in.last.6.months), binwidth = 2)

ggplot(EDA_data_defaulted) +  geom_histogram(mapping = aes(x = EDA_data_defaulted$No.of.times.90.DPD.or.worse.in.last.12.months), binwidth = 2)
#Looks like all the above 6 attributes related to DPD are important factors since the defaulters count has a declining trend as the value increases

ggplot(EDA_data_defaulted) +  geom_histogram(mapping = aes(x = EDA_data_defaulted$Avgas.CC.Utilization.in.last.12.months), binwidth = 10)
#The number od defaulters decrease as the value Average CC utilization for past 12 month increases

ggplot(EDA_data_defaulted) +  geom_histogram(mapping = aes(x = EDA_data_defaulted$No.of.trades.opened.in.last.6.months), binwidth = 1)
#The defaulters count increases until the no of trades done on cc in last 6 months = 3 and then reduces                                                                                                                                                        

ggplot(EDA_data_defaulted) +  geom_histogram(mapping = aes(x = EDA_data_defaulted$No.of.trades.opened.in.last.12.months), binwidth = 1)
#The defaulters count increases until the no of trades done on cc in last 12 months = 10 and then reduces                                                                                                                                                        

ggplot(EDA_data_defaulted) +  geom_histogram(mapping = aes(x = EDA_data_defaulted$No.of.PL.trades.opened.in.last.6.months), binwidth = 1)
#The defaulters count increases until the no of PL trades done on cc in last 6 months = 3 and then reduces                                                                                                                                                        

ggplot(EDA_data_defaulted) +  geom_histogram(mapping = aes(x = EDA_data_defaulted$No.of.PL.trades.opened.in.last.12.months), binwidth = 1)
#The defaulters count increases until the no of PL trades done on cc in last 12 months = 6 and then reduces                                                                                                                                                        

ggplot(EDA_data_defaulted) +  geom_histogram(mapping = aes(x = EDA_data_defaulted$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.), binwidth = 1)
#The defaulters count increases until the no of inquiries in last 6 months = 3 and then reduces                                                                                                                                                        

ggplot(EDA_data_defaulted) +  geom_histogram(mapping = aes(x = EDA_data_defaulted$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.), binwidth = 1)
#The defaulters count increases until the no of inquiries in last 12 months = 4 and then reduces                                                                                                                                                        

ggplot(EDA_data_defaulted) +  geom_histogram(mapping = aes(x = EDA_data_defaulted$Outstanding.Balance), binwidth = 500000)
#The defaulters count increases until outstanding balance is 1M and then reduces                                                                                                                                                        

ggplot(EDA_data_defaulted) +  geom_histogram(mapping = aes(x = EDA_data_defaulted$Total.No.of.Trades), binwidth = 1)
#The defaulters count is maximum between 5 to 10 for total no of trades done and decreases gradually after that                                                                                                                                                        

#From above EDA it is found that all the attributes has impact on performance tag of a customer and hence all the attributes are considered for WOE and IV

#------------------------------------END OF EDA------------------------------------------------------------------

#Calculation of WOE and finding the important attributes based on IV

summary(merged_df$Performance.Tag.y)

#Remove NAs from Dependant Variable as it won't allow execution of IV functions.
WOE_Data <- subset(merged_df, is.na(merged_df$Performance.Tag.y)==FALSE)
WOE_Data$Performance.Tag.y <- as.numeric(WOE_Data$Performance.Tag.y)

#Converting categorical variables to factor
WOE_Data$Presence.of.open.auto.loan <- factor(WOE_Data$Presence.of.open.auto.loan)
WOE_Data$Presence.of.open.home.loan <- factor(WOE_Data$Presence.of.open.home.loan)

#Dataset without application ID column
Temp_woe <- WOE_Data[,!names(WOE_Data) %in% c("Application.ID")]

# Generate InfoTables for the variables
IV <- create_infotables(Temp_woe,y="Performance.Tag.y", parallel = TRUE)

IV$Summary
#It is evident from summary of IV table that following attributes are not significant and can be rejected in the model
#25                                      Presence.of.open.home.loan 1.761939e-02
#2                                                              Age 3.350241e-03
#5                                                 No.of.dependents 2.653501e-03
#8                                                       Profession 2.219893e-03
#28                                      Presence.of.open.auto.loan 1.658061e-03
#9                                                Type.of.residence 9.198065e-04
#7                                                        Education 7.825416e-04
#3                                                           Gender 3.258695e-04
#4                      Marital.Status..at.the.time.of.application. 9.473857e-05

# Plot IVs for prominent variables obtained from IV's summary
plot_infotables(IV, IV$Summary$Variable[5],same_scales = TRUE)
plot_infotables(IV, IV$Summary$Variable[9:13],same_scales = TRUE)
plot_infotables(IV, IV$Summary$Variable[14:17],same_scales = TRUE)
plot_infotables(IV, IV$Summary$Variable[18:22],same_scales = TRUE)
plot_infotables(IV, IV$Summary$Variable[23],same_scales = TRUE)
plot_infotables(IV, IV$Summary$Variable[25,26],same_scales = TRUE)
#It is found that all th above variables are monotonic. i.e either growing or decresing with the groupings.

# Get WOE values for all 10 bins for Age variable
IV$Tables

#Function to replace column values with WOE value
woe_replace <- function(df_orig, IV) {
  df <- cbind(df_orig)
  df_clmtyp <- data.frame(clmtyp = sapply(df, class))
  df_col_typ <-
    data.frame(clmnm = colnames(df), clmtyp = df_clmtyp$clmtyp)
  for (rownm in 1:nrow(df_col_typ)) {
    colmn_nm <- toString(df_col_typ[rownm, "clmnm"])    
    if(colmn_nm %in% names(IV$Tables)){
      column_woe_df <- cbind(data.frame(IV$Tables[[toString(df_col_typ[rownm, "clmnm"])]]))
      if (df_col_typ[rownm, "clmtyp"] == "factor" | df_col_typ[rownm, "clmtyp"] == "character") {
        df <-
          dplyr::inner_join(
            df,
            column_woe_df[,c(colmn_nm,"WOE")],
            by = colmn_nm,
            type = "inner",
            match = "all"
          )
        df[colmn_nm]<-NULL
        colnames(df)[colnames(df)=="WOE"]<-colmn_nm
      } else if (df_col_typ[rownm, "clmtyp"] == "numeric" | df_col_typ[rownm, "clmtyp"] == "integer") {
        column_woe_df$lv<-as.numeric(str_sub(
          column_woe_df[,colmn_nm],
          regexpr("\\[", column_woe_df[,colmn_nm]) + 1,
          regexpr(",", column_woe_df[,colmn_nm]) - 1
        ))
        column_woe_df$uv<-as.numeric(str_sub(
          column_woe_df[,colmn_nm],
          regexpr(",", column_woe_df[,colmn_nm]) + 1,
          regexpr("\\]", column_woe_df[,colmn_nm]) - 1
        ))
        column_woe_df[colmn_nm]<-NULL      
        column_woe_df<-column_woe_df[,c("lv","uv","WOE")]      
        colnames(df)[colnames(df)==colmn_nm]<-"WOE_temp2381111111111111697"      
        df <-
          fuzzy_inner_join(
            df,
            column_woe_df[,c("lv","uv","WOE")],
            by = c("WOE_temp2381111111111111697"="lv","WOE_temp2381111111111111697"="uv"),
            match_fun=list(`>=`,`<=`) 
          )      
        df["WOE_temp2381111111111111697"]<-NULL      
        df["lv"]<-NULL      
        df["uv"]<-NULL      
        colnames(df)[colnames(df)=="WOE"]<-colmn_nm      
      }}
  }
  return(df)
}

#Calling the above function to replace values with WOE

#Obtaining a data frame with only important variables (obtained from IV)
temp <- WOE_Data[,-c(2:5,7:9,25,28)]

WOE_Data_final <- woe_replace(temp, IV)
#WOE_Data_final gives the final model after replacing the values with corresponding WOE values

str(WOE_Data_final)

#---------------------------------------------------------------------------------------------------------------#

#-----------------------------------------Obtaing WOE FInal data for demographic data------------------------------------

#Setting the negative values to null

Demographic_data_final <- Demographic_data

#Data cleaning for NA records

#For Age
boxplot(Demographic_data_final$Age)
#There are some values which are negative
quantile(Demographic_data_final$Age, probs = seq(0,1,0.01))
#Can't find any specific value which can be replaced for negative value
#Setting the records with negative age value to 18 since it is the min age required
Demographic_data_final$Age[which(Demographic_data_final$Age < 18)] <- 18

#For Income
boxplot(Demographic_data_final$Income)
which(Demographic_data_final$Income < 0)
#There are some values which are negative
quantile(Demographic_data_final$Income, probs = seq(0,1,0.01))
#Setting the negative value to 4.5 based on output from quantile function
Demographic_data_final$Income[which(Demographic_data_final$Income < 0)] <- 4.5

#------------------------------------------EDA to find the important variables-----------------------------------

#Filtering only defaulters data
EDA_data_demographic <- Demographic_data_final

#Finding no of NAs in EDA_data dataframe
sum(is.na(EDA_data_demographic$Performance.Tag))

#Removing records with Performance tag as NA since EDA need to be done on defaulted or non defaulted records
EDA_data_demographic <- subset(EDA_data_demographic,!is.na(EDA_data_demographic$Performance.Tag))

ggplot(EDA_data_demographic, aes(x=Gender,fill=factor(Performance.Tag)))+geom_bar(position = "fill")

ggplot(EDA_data_demographic, aes(x=EDA_data_demographic$Marital.Status..at.the.time.of.application.,fill=factor(Performance.Tag)))+geom_bar(position = "fill")

ggplot(EDA_data_demographic, aes(x=factor(EDA_data_demographic$No.of.dependents),fill=factor(Performance.Tag)))+geom_bar(position = "fill")

ggplot(EDA_data_demographic, aes(x=factor(EDA_data_demographic$Education),fill=factor(Performance.Tag)))+geom_bar(position = "fill")

ggplot(EDA_data_demographic, aes(x=factor(EDA_data_demographic$Profession),fill=factor(Performance.Tag)))+geom_bar(position = "fill")

ggplot(EDA_data_demographic, aes(x=factor(EDA_data_demographic$Type.of.residence),fill=factor(Performance.Tag)))+geom_bar(position = "fill")

#Based on EDA done for categorical variables in above graphs, there is no specific attribute which has high impact on performance and hence all are included in WOE.

#Taking only defaulters data for EDA on continuous values attributes
EDA_data_demographic_defaulted <- subset(EDA_data_demographic,EDA_data_demographic$Performance.Tag==1)

ggplot(EDA_data_demographic_defaulted) +  geom_histogram(mapping = aes(x = EDA_data_demographic_defaulted$Age), binwidth = 5)
#The defaulters ratio is more between age range 30 to 50

ggplot(EDA_data_demographic_defaulted) +  geom_histogram(mapping = aes(x = EDA_data_demographic_defaulted$Income), binwidth = 5)
#It is found that as the income keeps increasing, the deault rate keeps decreasing

ggplot(EDA_data_demographic_defaulted) +  geom_histogram(mapping = aes(x = EDA_data_demographic_defaulted$No.of.months.in.current.residence), binwidth = 5)
#There is a slight reduce in defaulters ratio as the no of months in current residence increases

ggplot(EDA_data_demographic_defaulted) +  geom_histogram(mapping = aes(x = EDA_data_demographic_defaulted$No.of.months.in.current.company), binwidth = 5)
#Defaulters reduce as the no of months in current company increases

#From above EDA it is found that all the attributes has impact on performance tag of a customer and hence all the attributes are considered for WOE and IV

#------------------------------------END OF EDA for Demographic data------------------------------------------------------------------

#Calculation of WOE and finding the important attributes based on IV

summary(Demographic_data_final$Performance.Tag)

#Remove NAs from Dependant Variable as it won't allow execution of IV functions.
WOE_Data_Demographic <- subset(Demographic_data_final, is.na(Demographic_data_final$Performance.Tag)==FALSE)
WOE_Data_Demographic$Performance.Tag <- as.numeric(as.character(WOE_Data_Demographic$Performance.Tag))

#Dataset without application ID column
Temp_demographic_woe <- WOE_Data[,!names(WOE_Data_Demographic) %in% c("Application.ID")]

# Generate InfoTables for the variables
IV_Demographic <- create_infotables(WOE_Data_Demographic,y="Performance.Tag", parallel = TRUE)

IV_Demographic$Summary
#All the values lie above 0.02 except for Application_ID and hence are comsidered for modelling

# Plot IVs for prominent variables obtained from IV's summary

plot_infotables(IV_Demographic, IV_Demographic$Summary$Variable[2:5],same_scales = TRUE)
plot_infotables(IV_Demographic, IV_Demographic$Summary$Variable[6:9],same_scales = TRUE)
plot_infotables(IV_Demographic, IV_Demographic$Summary$Variable[10:11],same_scales = TRUE)

#Even thogh some attributes like Age , No-of_dependents dont show monotonic behaviour, they are considered based on IV value

# Get WOE values for all 10 bins
IV_Demographic$Tables

#Function to replace column values with WOE value
woe_replace <- function(df_orig, IV) {
  df <- cbind(df_orig)
  df_clmtyp <- data.frame(clmtyp = sapply(df, class))
  df_col_typ <-
    data.frame(clmnm = colnames(df), clmtyp = df_clmtyp$clmtyp)
  for (rownm in 1:nrow(df_col_typ)) {
    colmn_nm <- toString(df_col_typ[rownm, "clmnm"])    
    if(colmn_nm %in% names(IV$Tables)){
      column_woe_df <- cbind(data.frame(IV$Tables[[toString(df_col_typ[rownm, "clmnm"])]]))
      if (df_col_typ[rownm, "clmtyp"] == "factor" | df_col_typ[rownm, "clmtyp"] == "character") {
        df <-
          dplyr::inner_join(
            df,
            column_woe_df[,c(colmn_nm,"WOE")],
            by = colmn_nm,
            type = "inner",
            match = "all"
          )
        df[colmn_nm]<-NULL
        colnames(df)[colnames(df)=="WOE"]<-colmn_nm
      } else if (df_col_typ[rownm, "clmtyp"] == "numeric" | df_col_typ[rownm, "clmtyp"] == "integer") {
        column_woe_df$lv<-as.numeric(str_sub(
          column_woe_df[,colmn_nm],
          regexpr("\\[", column_woe_df[,colmn_nm]) + 1,
          regexpr(",", column_woe_df[,colmn_nm]) - 1
        ))
        column_woe_df$uv<-as.numeric(str_sub(
          column_woe_df[,colmn_nm],
          regexpr(",", column_woe_df[,colmn_nm]) + 1,
          regexpr("\\]", column_woe_df[,colmn_nm]) - 1
        ))
        column_woe_df[colmn_nm]<-NULL      
        column_woe_df<-column_woe_df[,c("lv","uv","WOE")]      
        colnames(df)[colnames(df)==colmn_nm]<-"WOE_temp2381111111111111697"      
        df <-
          fuzzy_inner_join(
            df,
            column_woe_df[,c("lv","uv","WOE")],
            by = c("WOE_temp2381111111111111697"="lv","WOE_temp2381111111111111697"="uv"),
            match_fun=list(`>=`,`<=`) 
          )      
        df["WOE_temp2381111111111111697"]<-NULL      
        df["lv"]<-NULL      
        df["uv"]<-NULL      
        colnames(df)[colnames(df)=="WOE"]<-colmn_nm      
      }}
  }
  return(df)
}

#Calling the above function to replace values with WOE

#Obtaining a data frame with only important variables (obtained from IV)
temp_demographic <- WOE_Data_Demographic

WOE_Data_Demographic_final <- woe_replace(temp_demographic, IV_Demographic)
#WOE_Data_Demographic_final gives the final model after replacing the values with corresponding WOE values

str(WOE_Data_Demographic_final)
