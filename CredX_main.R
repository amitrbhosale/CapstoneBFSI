# CredX - Acquisition Risk Analysis Capstone Project.

# Read the datasets
library(readr)

Demographic_data <- read_csv("Demographic data.csv")

# Demographic data contains customer level information (age, gender, income, marital status, etc)

View(Demographic_data)

Credit_Bureau_data <- read_csv("Credit Bureau data.csv")

# Credit Bureau Data contains information taken from credit bureau. ('number of times 30 DPD or worse in last 3/6/12 months', 'outstanding balance', 'number of trades', etc.) 

View(Credit_Bureau_data)

