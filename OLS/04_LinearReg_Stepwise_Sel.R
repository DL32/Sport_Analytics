# 20630 - Introduction to Sports Analytics
# Group 2 Project 3 - Supervised Machine Learning Model
# Working on the baseline model: Stepwise Regression

# clear environment & set working directory
rm(list=ls()) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Package import
listofpackages <- c("quantmod","PerformanceAnalytics","ellipse","reshape2","ggplot2", "rvest",
                    "stringr", "dygraphs", "dplyr","forecast", "aod","readr","rvest","lubridate", 
                    "xml2", "caret", "olsrr", "pls", "stats", "fastDummies", "pdp", 'car')

# Uploading libraries
for (j in listofpackages){
  if(sum(installed.packages()[, 1] == j) == 0) {
    install.packages(j)
  }
  library(j, character.only = T)
}

### 1. PREPROCESSING & INVESTIGATION###

#Load Data (both train and test)
train_basket <- read.csv('../Dataset/Final Datasets/Final_data_Bplayers_2000_TRAIN.csv')
test_basket <- read.csv('../Dataset/Final Datasets/Final_data_Bplayers_2000_TEST.csv', encoding = "latin1")

#Creating and applying a function for common pre-treatment of train and test
pre_treat <- function(dataset){
  
  #Create dummies for the role
  dataset$pos = lapply(dataset$pos, function(x) if (nchar(x)>2) {x=unlist(str_split(x, ",", simplify = TRUE)[1,1])} else {x})
  dataset$pos = unlist(dataset[,"pos"])
  dataset <- dummy_cols(dataset, select_columns = 'pos')
  
  #Let's drop columns we won't use
  drops <- c("season","Player",'tm','lg','Salary_Cap','Salary', 'pos', 'Image_Link', "contract_type")
  dataset = dataset[ , !(names(dataset) %in% drops)]
  
  #We replace NA with 0
  dataset[is.na(dataset)] = 0
  
  dataset
}

train_basket = pre_treat(train_basket)
test_basket = pre_treat(test_basket)

#Divide X and Y in test
test_basket_x = subset(test_basket, select = -Salary_Cap_Perc) # feature and target array
test_basket_y = test_basket[, "Salary_Cap_Perc"]


### 2. REGRESSION & PREDICTION###

#Simple Linear Regression
lm_model <- lm(Salary_Cap_Perc ~ ., data=train_basket)

#Forward Stepwise Regression
Forw_reg <- ols_step_forward_p(lm_model, penter = 0.05) #I want a p-value of at least 0.05
Forw_reg

#Backward Stepwise Regression: Doesn't work for aliased coefficients
Back_reg <- ols_step_backward_p(lm_model, prem = 0.05) #I want a p-value of at least 0.05
Back_reg

#Bi-directional Stepwise regression: Same results as forward
Both_reg <- ols_step_both_p(lm_model, penter = 0.05, prem = 0.05)
Both_reg

#Forward Stepwise Regression::Using AIC
Forw_reg2 <- ols_step_forward_aic(lm_model, progress = TRUE) # Enter predictors based on AIC
Forw_reg2

#File of investigation for what concerns Stepwise variable reduction techniques
