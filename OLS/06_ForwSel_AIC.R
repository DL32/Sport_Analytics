# 20630 - Introduction to Sports Analytics
# Group 2 Project 3 - Supervised Machine Learning Model
# Working on the baseline model: Forward Selection variables removal (based on AIC)

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
  
  #Select variables included by Forward Selection procedure
  dataset = subset(dataset, select = c(Salary_Cap_Perc,pts, bird_rights, age, maximum_contract, drb,rookie_contract, two_pa, fta,US_Player,ts_pct, gs, super_max_contract,ast,pos_PG,ast_pct,pf,
                                       blk, tov_pct, stl, mp, usg_pct, dbpm, fg_pct, g,trb_pct, two_p_pct, pos_SG, pos_SF,blk_pct, tov, trb, stl_pct,ft, three_p_pct, pos_C,pos_PF ) )
  
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

#Predictions
predictions_1 <- predict(lm_model, newdata=test_basket_x)


### 3. ANALYSIS OF RESULTS ###

#RMSE on test set
RMSE_01 <- sqrt(mean((test_basket_y - predictions_1)^2))
cat('The root mean square error of the test data is ', round(RMSE_01,3),'\n')
#RMSE is 0.042 

#R-squared
rsq_01 <- (cor(predictions_1, test_basket$Salary_Cap_Perc))^2
cat('The R-square of the test data is ', round(rsq_01,3), '\n')
#R-squared is 0.655 