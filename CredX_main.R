# CredX - Acquisition Risk Analysis Capstone Project.

#Installing Required Packages
install.packages("rstudioapi")
install.packages("ggplot2")
library(rstudioapi)
library(ggplot2)

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

#------------------------------------------EDA to find the important variables-----------------------------------

#Filtering only defaulters data
EDA_data <- merged_df

#Finding no of NAs in EDA_data dataframe
sum(is.na(EDA_data$Performance.Tag.y))

#Removing records with Performance tag as NA since EDA need to be done on defaulted or non defaulted records
EDA_data <- subset(EDA_data,!is.na(EDA_data$Performance.Tag.y))

ggplot(EDA_data, aes(x=Gender,fill=factor(Performance.Tag.y)))+geom_bar(position = "fill")

ggplot(EDA_data, aes(x=EDA_data$Marital.Status..at.the.time.of.application.,fill=factor(Performance.Tag.y)))+geom_bar(position = "fill")

ggplot(EDA_data, aes(x=EDA_data$Marital.Status..at.the.time.of.application.,fill=factor(Performance.Tag.y)))+geom_bar(position = "fill")

ggplot(EDA_data, aes(x=factor(EDA_data$No.of.dependents),fill=factor(Performance.Tag.y)))+geom_bar(position = "fill")

ggplot(EDA_data, aes(x=factor(EDA_data$Education),fill=factor(Performance.Tag.y)))+geom_bar(position = "fill")

ggplot(EDA_data, aes(x=factor(EDA_data$Profession),fill=factor(Performance.Tag.y)))+geom_bar(position = "fill")

ggplot(EDA_data, aes(x=factor(EDA_data$Education),fill=factor(Performance.Tag.y)))+geom_bar(position = "fill")

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

#Need to perform WOE and IV Analysis


install.packages("Information")
library("Information")

summary(merged_df$Performance.Tag.y)

#Remove NAs from Dependant Variable as it won't allow execution of IV functions.
traindata <- subset(merged_df, is.na(merged_df$Performance.Tag.y)==FALSE)

traindata$Performance.Tag.y <- as.numeric(traindata$Performance.Tag.y)
# Generate InfoTables for the variables
IV <- create_infotables(traindata,y="Performance.Tag.y")

# Plot IVs for first 4 variables
plot_infotables(IV, IV$Summary$Variable[1:4],same_scales = TRUE)

# Get WOE values for all 10 bins for Age variable

IV$Tables$Age$WOE

# Extract variable names
names <- names(IV$Tables)

plots <- list()
for (i in 1:length(names)){
  plots[[i]] <- plot_infotables(IV, names[i])
}
# Showing the top 28 variables
plots[1:28]
