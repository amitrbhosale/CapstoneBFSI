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

traindata <- merged_df
# Create a dataframe with the important variables identified and the dependant variable

impvar_df <- traindata[,c(as.vector(predictor_variables$Variable),"Performance.Tag.y")]

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
        val <- df[j,i]
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


h2o.shutdown(prompt = FALSE)
