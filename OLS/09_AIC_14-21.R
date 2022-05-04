# 20630 - Introduction to Sports Analytics
# Group 2 Project 3 - Supervised Machine Learning Model
# Working on Reducing The Number of Seasons - Selection Criteria: Forward Selection variables removal (based on AIC)

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

### 1. PREPROCESSING & INVESTIGATION ###

#Load Data (both train and test)
train_basket <- read.csv('../Dataset/Final Datasets/Final_data_Bplayers_2000_TRAIN.csv')
test_basket <- read.csv('../Dataset/Final Datasets/Final_data_Bplayers_2000_TEST.csv', encoding = "latin1")


#Creating and applying a function for common pre-treatment of train and test
pre_treat <- function(dataset){
  
  #Create dummies for the role
  dataset$pos = lapply(dataset$pos, function(x) if (nchar(x)>2) {x=unlist(str_split(x, ",", simplify = TRUE)[1,1])} else {x})
  dataset$pos = unlist(dataset[,"pos"])
  dataset <- dummy_cols(dataset, select_columns = 'pos')
  
  #Selecting Seasons
  dataset <- dataset %>% filter(season == "2014-15"| season == "2015-16" | season == "2016-17" | season == "2017-18" | season == "2018-19" | season == "2019-20"| season == "2020-21") 
  
  #Let's drop columns we won't use
  drops <- c("season","Player",'tm','lg','Salary_Cap','Salary', 'pos', 'Image_Link', "contract_type")
  dataset = dataset[ , !(names(dataset) %in% drops)]
  
  #We replace NA with 0
  dataset[is.na(dataset)] = 0
  
  #Removing variables excluded by Forward Selection procedure
  dataset = subset(dataset, select = c(Salary_Cap_Perc,pts, bird_rights, age, maximum_contract, drb,rookie_contract, gs, pf, three_p_pct,ast, ast_pct, 
                                       super_max_contract,vorp, pos_PG, super_max_contract, US_Player, pos_SG, trb_pct, pos_SF, three_par, two_p_pct,tov, tov_pct, fta) )
  
  dataset
}



## SEASONS 2014-15 to 2020-21

#Train & Test with Variable Selection.
train_basket = pre_treat(train_basket)
test_basket = pre_treat(test_basket)

#Divide X and Y in test
test_basket_x = subset(test_basket, select = -Salary_Cap_Perc) # feature and target array
test_basket_y = test_basket[, "Salary_Cap_Perc"]


###INVESTIGATING WHAT'S RELEVANT###
set.seed(123)
#Simple Linear Regression
lm_model <- lm(Salary_Cap_Perc ~ ., data=train_basket)

#Forward Stepwise Regression::Using AIC
#Forw_reg <- ols_step_forward_aic(lm_model, progress = TRUE) 
#Forw_reg


### 2. REGRESSION & PREDICTION###
set.seed(123)
#Simple Linear Regression 
lm_model2 <- lm(Salary_Cap_Perc ~ ., data=train_basket)

#Predictions
predictions <- predict(lm_model2, newdata=test_basket_x)


### 3. ANALYSIS OF RESULTS ###

#RMSE on test set
RMSE <- sqrt(mean((test_basket_y - predictions)^2))
cat('The root mean square error of the test data is ', round(RMSE,3),'\n')
#RMSE is 0.04 (Best for now)

#R-squared
rsq <- (cor(predictions, test_basket$Salary_Cap_Perc))^2
cat('The R-square of the test data is ', round(rsq,3), '\n')
#R-squared is 0.715