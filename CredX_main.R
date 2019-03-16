# CredX - Acquisition Risk Analysis Capstone Project.

#Installing Required Packages
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
library(DMwR)

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
#------------------------------------------EDA to find the important variables-----------------------------------

#Filtering only defaulters data
EDA_data <- merged_df

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

# Checking predictor variable No.of.months.in.current.residenc

IV$Tables$No.of.months.in.current.residenc

# Checking the predictor variable Income

IV$Tables$Income

#Saving the merged dataframe in traindata
traindata <- merged_df

# Create a dataframe with the important variables identified and the dependant variable

impvar_df <- traindata[,c(as.vector(predictor_variables$Variable),"Performance.Tag.y")]

# WOE replacement in the impvar_df to be carried out, we are defining woe_replace function.

#impvar_df$Income <- ceiling(impvar_df$Income)
#impvar_df$Income <- as.integer(impvar_df$Income)

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

# Removing Total.No.of.Trades due to high vif and relatively high p-value
model_3 <- glm(Performance.Tag.y ~ Avgas.CC.Utilization.in.last.12.months + 
                 No.of.trades.opened.in.last.12.months + No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                 Outstanding.Balance + No.of.times.30.DPD.or.worse.in.last.6.months, 
               family = "binomial", data = train)
summary(model_3)
vif(model_3)

# Removing No.of.trades.opened.in.last.12.months due to high p-value

model_4 <- glm(Performance.Tag.y ~ Avgas.CC.Utilization.in.last.12.months + 
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                 Outstanding.Balance + No.of.times.30.DPD.or.worse.in.last.6.months, 
               family = "binomial", data = train)
summary(model_4)
vif(model_4)

final_model <- model_4

p1 <- plot_infotables(IV, IV$Summary$Variable[1],same_scales = TRUE)
p2 <- plot_infotables(IV, IV$Summary$Variable[4],same_scales = TRUE)
p3 <- plot_infotables(IV, IV$Summary$Variable[5],same_scales = TRUE)
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

# Checking if the Income variable has outliers.

quantile(Demographic_data$Income, seq(0,1,0.01))

# We can see a jump in the Income variable from -0.5 to 4.5 and Income cannot be negative, therefore we are treating this as an outlier.

Demographic_data$Income <- ifelse(Demographic_data$Income<4.5, 4.5, Demographic_data$Income)

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

p1_dem <- plot_infotables(IV_dem, IV_dem$Summary$Variable[10],same_scales = TRUE)
p2_dem <- plot_infotables(IV_dem, IV_dem$Summary$Variable[6],same_scales = TRUE)

grid.arrange(p1_dem,p2_dem)

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
#Specificity = 0.38
#Even theough accuracy decreases, the model provides a better specificity compared to model built on unbalanced data
#---------------------------------------------------------    

#Model Building - Random Forest

# Spliting the data in 70:30 ratio

set.seed(101)

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

# The plot shows that cutoff value of around 98.3% optimises sensitivity and accuracy

predicted_Performance_tag <- factor(ifelse(rf_pred[, 1] >= 0.97, "no", "yes"))

conf_forest <- confusionMatrix(predicted_Performance_tag, test_rf[, 16], positive = "no")

conf_forest

# Sensitivity
conf_forest$byClass[1]
#0.6867

# Specificity 
conf_forest$byClass[2]
#0.4875

# Accuracy 
conf_forest$overall[1]
#0.67

# Final RF important variables
importance <- Credit_rf$importance 

importance <- data.frame(importance)
h2o.shutdown(prompt = FALSE) 

#------------------------------------------------------------------------------------------------
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
                 No.of.PL.trades.opened.in.last.12.months + 
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                 Outstanding.Balance + No.of.times.30.DPD.or.worse.in.last.6.months + 
                 No.of.PL.trades.opened.in.last.6.months + No.of.times.60.DPD.or.worse.in.last.6.months, 
               family = "binomial", data = Smoted_train_lg)
summary(model_3)
vif(model_3)

# Removing No.of.times.60.DPD.or.worse.in.last.6.months due to high p-value and VIF

model_4 <- glm(formula = Performance.Tag.y ~ Avgas.CC.Utilization.in.last.12.months + 
                 No.of.PL.trades.opened.in.last.12.months + 
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                 Outstanding.Balance + No.of.times.30.DPD.or.worse.in.last.6.months + 
                 No.of.PL.trades.opened.in.last.6.months , 
               family = "binomial", data = Smoted_train_lg)
summary(model_4)
vif(model_4)

# Removing No.of.PL.trades.opened.in.last.12.months due to high p-value and VIF

model_5 <- glm(formula = Performance.Tag.y ~ Avgas.CC.Utilization.in.last.12.months +
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                 Outstanding.Balance + No.of.times.30.DPD.or.worse.in.last.6.months + 
                 No.of.PL.trades.opened.in.last.6.months , 
               family = "binomial", data = Smoted_train_lg)
summary(model_5)
vif(model_5)

# Removing No.of.PL.trades.opened.in.last.6.months due to high p-value and VIF

model_6 <- glm(formula = Performance.Tag.y ~ Avgas.CC.Utilization.in.last.12.months +
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                 Outstanding.Balance + No.of.times.30.DPD.or.worse.in.last.6.months, 
               family = "binomial", data = Smoted_train_lg)
summary(model_6)
vif(model_6)

final_model <- model_6

#---------------------------------------------------------    

# Predicting probabilities of defaulting for the test data

predictions_logit <- predict(final_model, newdata = test[, -16], type = "response")
summary(predictions_logit)

#--------------------------------------------------------- 

## Model Evaluation: Logistic Regression

# Let's use the probability cutoff of 90.5%.

predicted_Performance_tag <- factor(ifelse(predictions_logit >= 0.50, "no", "yes"))
test$Performance.Tag.y <- factor(ifelse(test$Performance.Tag.y ==1, "no","yes"))
summary(test$Performance.Tag.y)
summary(predicted_Performance_tag)
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

s = seq(0.001,0.99,length=100)

OUT = matrix(0,100,3)

for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0.001,1,length=5),seq(0.001,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(x="topright",0.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

#---------------------------------------------------------    
cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]

# Let's choose a cutoff value of 46% for final model

predicted_Performance_tag <- factor(ifelse(predictions_logit >= .46, "no", "yes"))

conf_final <- confusionMatrix(predicted_Performance_tag, test$Performance.Tag.y, positive = "no")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

#----------------------------End_of_LR_model_buiding for balanced data----------------------------

#-----------------------------Random forest model for Balanced data-------------------------------

set.seed(100)

impvar_without_NA_df <- subset(impvar_df,is.na(impvar_df$Performance.Tag.y)==FALSE)

cor_data <- impvar_without_NA_df[,-16]
correlation_matrix<- cor(cor_data)

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

Credit_rf <- randomForest(Performance.Tag.y ~., data = Smoted_train_rf, proximity = F, 
                          do.trace = T, mtry = 1,nodesize=20,ntree=500,importance=TRUE)

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
OUT_rf
#legend(x="topright",0.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff_rf <- s[which(abs(OUT_rf[,1]-OUT_rf[,2])<0.01)]

# The plot shows that cutoff value of around 50.5% optimises sensitivity and accuracy

predicted_Performance_tag <- factor(ifelse(rf_pred[, 1] >= 0.505, "no", "yes"))

conf_forest <- confusionMatrix(predicted_Performance_tag, test_rf[, 16], positive = "no")

conf_forest
# Sensitivity
conf_forest$byClass[1]
#approx 67%

# Specificity 
conf_forest$byClass[2]
#approx 62%

# Accuracy 
conf_forest$overall[1]
#approx 67%

# Final RF important variables
importance <- Credit_rf$importance 

importance <- data.frame(importance)
h2o.shutdown(prompt = FALSE)

# -------------------------------------Model Evaluation----------------------------------

# Appending the probabilities and performance tag variables to the test data

test_rf$predicted_probs <- rf_pred[, 1]

test_rf$predicted_Performance_tag <- predicted_Performance_tag

#---------------------------------------------------------    

# Creating new dataframe "test_predictions_rf"

test_predictions_rf <- test_rf[, c("Performance.Tag.y", "predicted_probs", "predicted_Performance_tag")]

#sorting the probabilities in decreasing order 
test_predictions_rf <- test_predictions_rf[order(test_predictions_rf$predicted_probs, decreasing = T), ]

summary(test_predictions_rf$Performance.Tag.y)
summary(test_predictions_rf$predicted_Performance_tag)

# plotting the lift chart

# Loading dplyr package 
require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob, groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups)))
  return(gaintable)
}

# Create a Table of cumulative gain and lift 

test_predictions_rf$Performance.Tag.y <- as.factor(ifelse(test_predictions_rf$Performance.Tag.y=="yes",0,1))
test_predictions_rf$predicted_Performance_tag <- as.factor(ifelse(test_predictions_rf$predicted_Performance_tag=="yes",0,1))

summary(test_predictions_rf$Performance.Tag.y)
summary(test_predictions_rf$predicted_Performance_tag)

LG = lift(test_predictions_rf$predicted_Performance_tag, test_predictions_rf$Performance.Tag.y, groups = 10)

# Gain Chart 

plot(LG$bucket,LG$Gain,col="red",type="l",main="Gain Chart",xlab="% of total targeted",ylab = "% of positive Response")

# Lift Chart 

plot(LG$bucket,LG$Cumlift,col="red",type="l",main="lift Chart",xlab="% of total targeted",ylab = "Lift")

# Total Cost incur throught direct telemarketing 

# Let's say if you have spent 1Re for each customer
View(LG)

# The Cumulative Lift of 1.5 for top two deciles,
# means that when selecting 60% of the records based on the model, 
# one can expect 1.5 times the total number of targets (events) found by randomly 
# selecting 50%-of-records without a model. In terms of defaulter model, 
# we can say we can cover 1.5 times the number of defaulters by selecting only 60% of the
# customers based on the model as compared to 20% customer selection randomly.

### Analyzing the Charts: Cumulative gains and lift charts are a graphical 
# representation of the advantage of using a predictive model to choose which 
# customers to contact. The lift chart shows how much more likely we are to receive
# respondents than if we contact a random sample of customers. For example,
# by contacting only 10% of customers based on the predictive model we will reach 
# 1.5 times as many respondents as if we use no model.

#------------------------model evaluation for entire dataset with performance tag values--------------------------------
# Predict response for test data

test <- impvar_without_NA_df

rf_pred <- predict(Credit_rf, test[, -16], type = "prob")

predicted_Performance_tag <- factor(ifelse(rf_pred[, 1] >= 0.505, "no", "yes"))

# Appending the probabilities and performance tag variables to the test data

test$predicted_probs <- rf_pred[, 1]

test$predicted_Performance_tag <- predicted_Performance_tag

#---------------------------------------------------------    

# Creating new dataframe "test_predictions_rf"

test_predictions_rf <- test[, c("Performance.Tag.y", "predicted_probs", "predicted_Performance_tag")]

#sorting the probabilities in decreasing order 
test_predictions_rf <- test_predictions_rf[order(test_predictions_rf$predicted_probs, decreasing = T), ]

summary(test_predictions_rf$Performance.Tag.y)
summary(test_predictions_rf$predicted_Performance_tag)

# plotting the lift chart

# Loading dplyr package 
require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob, groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups)))
  return(gaintable)
}

# Create a Table of cumulative gain and lift 

test_predictions_rf$Performance.Tag.y <- as.factor(ifelse(test_predictions_rf$Performance.Tag.y=="yes",0,1))
test_predictions_rf$predicted_Performance_tag <- as.factor(ifelse(test_predictions_rf$predicted_Performance_tag=="yes",0,1))

summary(test_predictions_rf$Performance.Tag.y)
summary(test_predictions_rf$predicted_Performance_tag)

LG = lift(test_predictions_rf$predicted_Performance_tag, test_predictions_rf$Performance.Tag.y, groups = 10)

# Gain Chart 

plot(LG$bucket,LG$Gain,col="red",type="l",main="Gain Chart",xlab="% of total targeted",ylab = "% of positive Response")

# Lift Chart 

plot(LG$bucket,LG$Cumlift,col="red",type="l",main="lift Chart",xlab="% of total targeted",ylab = "Lift")

# Total Cost incur throught direct telemarketing 

# Let's say if you have spent 1Re for each customer
View(LG)

#------------Application Scorecard for balanced data random forest model------------------

Application_Card_rf <- impvar_df

predictions_rf <- predict(Credit_rf, newdata = Application_Card_rf[, -16], type = "prob")
predicted_Performance_tag <- factor(ifelse(predictions_rf[,1] >= 0.505, "no", "yes"))

# Appending the probabilities and response variables to the test data

Application_Card_rf$predicted_probs <- predictions_rf

Application_Card_rf$predicted_Performance_tag <- predicted_Performance_tag

Application_Card_rf$predict_NonDefault <- Application_Card_rf$predicted_probs[,1]

Application_Card_rf$predict_Default <- 1 - Application_Card_rf$predict_NonDefault

Application_Card_rf$odds <-  log(Application_Card_rf$predict_NonDefault/Application_Card_rf$predict_Default)

# Score = Offset + ( Factor * log(odds) )
# Factor = PDO/ln(2)
# Offset = Score-(Factor*log(odds))
# PDO = 20, Base Score=400, odds = 10

Factor = 20/log(2)
#28.8539

Offset = 400 - (Factor*log(10))

Application_Card_rf$Score <- Offset + (Factor*Application_Card_rf$odds)

#Calculating the cut off score for application score

cutoff_odds <- log(0.505/(1-0.505))
cutoff_score <- Offset + (Factor*cutoff_odds)
cutoff_score
#Cut off Score is 334.138

quantile(Application_Card_rf$Score, seq(0,1,0.01))

#Application_Card_rf$Score[which(Application_Card_rf$Score==(1/0))] <- max(Application_Card_rf$Score[which(Application_Card_rf$Score != (1/0))])

Application_Card_rf$Performance.Tag.y <- ifelse(Application_Card_rf$Performance.Tag.y==0,"yes",ifelse(is.na(Application_Card_rf$Performance.Tag.y),"NA","no"))

#Histogram for Application Scores
ggplot(Application_Card_rf) +  geom_histogram(mapping = aes(x = Application_Card_rf$Score), binwidth = 2)

#Boxplot for predicted performance Tag
boxplot(Application_Card_rf$Score ~ Application_Card_rf$predicted_Performance_tag,
        Application_Card_rf,xlab="Non-Default or Default",ylab="Score",main="Plot for predicted performance")

#Boxplot for performance Tag
boxplot(Application_Card_rf$Score ~ Application_Card_rf$Performance.Tag.y ,
        Application_Card_rf,xlab="Non-Default or Default",ylab="Score",main="Plot for performance tag")

Scorescard_without_na <- subset(Application_Card_rf,is.na(Application_Card_rf$Performance.Tag.y)==FALSE)
Scorescard_without_na$Performance.Tag.y <- as.factor(Scorescard_without_na$Performance.Tag.y)
Scorescard_without_na$predicted_Performance_tag <- as.factor(Scorescard_without_na$predicted_Performance_tag)

confusionMatrix(Scorescard_without_na$predicted_Performance_tag,Scorescard_without_na$Performance.Tag.y)

#Revenue loss occurs when actual bad customers are predicted as good customers by model
#Totally 22324 customers are identified as good by the model even though they are bad customers
#if revenue from each member is Rs 1, then
Total_revenue_expected <- 69867
Revenue_gained_without_model <- 66920
Revenue_gain_precentage_without_model <- (Revenue_gained_without_model/Total_revenue_expected)*100
Revenue_gained_by_model <- 45715
Revenue_gain_precentage_with_model <- (Revenue_gained_by_model/Total_revenue_expected)*100
Revenue_gain_difference <- Revenue_gain_precentage_with_model-Revenue_gain_precentage_without_model
#Revenue gained is 30.3% percent less by using the model. Hence there is a revenue loss of 30.3 % more using the model

#Calculating credit loss
#credit loss is defined as loss incurred by amount not repayed by cutomer or amount not sanctioned for customers who will probably repay the amount
#lets assume amount approved for each customer is Re 1

Total_approved_without_model <- 69687
loss_without_model <- 2947
Credit_loss_without_model <- (2947/69687)*100

Total_approved_with_model <- 45715
#but there are 1119 customers who are actually good customers but classified as bad
loss_with_model <- 1119
Credit_loss_with_model <- (1119/45715)*100

#Credit loss saved
Credit_loss_without_model - Credit_loss_with_model

#---------------------------------------------------------    


