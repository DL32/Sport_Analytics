# 20630 - Introduction to Sports Analytics
# Group 2 Project 3 - Supervised Machine Learning Model
# Working on Reducing The Number of Seasons - Generating 4 Datasets with 5 subsequent seasons each. Selection Criteria: Forward Selection variables removal (based on AIC)

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

### 1. PREPROCESSING & INVESTIGATION --> General process adapted for the 4 different scenarios. This version is the one referring to Q4 model. No need to Run it.###

#Load Data (both train and test)
train_basket <- read.csv('../Sportylytics-main/Final_data_Bplayers_2000_TRAIN.csv')
test_basket <- read.csv('../Sportylytics-main/Final_data_Bplayers_2000_TEST.csv')

#Creating and applying a function for common pre-treatment of train and test
pre_treat <- function(dataset){
  
  #Create dummies for the role
  dataset$pos = lapply(dataset$pos, function(x) if (nchar(x)>2) {x=unlist(str_split(x, ",", simplify = TRUE)[1,1])} else {x})
  dataset$pos = unlist(dataset[,"pos"])
  dataset <- dummy_cols(dataset, select_columns = 'pos')
  
  
  #Selecting Seasons
  dataset <- dataset %>% filter(season == "2014-15"| season == "2015-16" | season == "2016-17" | season == "2017-18" | season == "2018-19" | season == "2019-20" ) 
  
  #Let's drop columns we won't use
  drops <- c("season","Player",'tm','lg','Salary_Cap','Salary', 'pos', 'Image_Link')
  dataset = dataset[ , !(names(dataset) %in% drops)]
  
  #We replace NA with 0
  dataset[is.na(dataset)] = 0
  
  #Removing variables excluded by Forward Selection procedure
  dataset = subset(dataset, select = c(Salary_Cap_Perc, pts, bird_rights, maximum_contract, age, gs, drb, three_p_pct, pf, mp, rookie_contract, super_max_contract, pos_C,
                                       pos_PF, fta, fga, US_Player, pos_PG, ast, ast_pct, trb_pct, stl, stl_pct, ft_pct, three_pa, tov_pct, usg_pct) )
  
  dataset
}



## a. SEASONS 1999-00 to 2003-04

#Train & Test for the above_mentioned 5 seasons with Variable Selection.
train_basket_Q1 = pre_treat(train_basket)
test_basket_Q1 = pre_treat(test_basket)
#In particular, vars left are: Salary_Cap_Perc, pts, bird_rights, blk_pct, rookie_contract, US_Player, tov, efg_pct, ft_pct, pos_SG


#Divide X and Y in test
test_basket_Q1_x = subset(test_basket_Q1, select = -Salary_Cap_Perc) # feature and target array
test_basket_Q1_y = test_basket_Q1[, "Salary_Cap_Perc"]


### a1. INVESTIGATING WHAT'S RELEVANT###

#Simple Linear Regression
lm_model_Q1 <- lm(Salary_Cap_Perc ~ ., data=train_basket_Q1)

#Forward Stepwise Regression::Using AIC
Forw_reg <- ols_step_forward_aic(lm_model_Q1, progress = TRUE) 
Forw_reg
#9 Vars Left


### a2. REGRESSION & PREDICTION###

#Simple Linear Regression with 12 Vars
lm_model2_Q1 <- lm(Salary_Cap_Perc ~ ., data=train_basket_Q1)

#Predictions
predictions_Q1 <- predict(lm_model2_Q1, newdata=test_basket_Q1_x)


### a3. ANALYSIS OF RESULTS ###

#RMSE on test set
RMSE_Q1 <- sqrt(mean((test_basket_Q1_y - predictions_Q1)^2))
cat('The root mean square error of the test data is ', round(RMSE_Q1,3),'\n')
#RMSE is 0.035 (Best for now)

#R-squared
rsq_Q1 <- (cor(predictions_Q1, test_basket_Q1$Salary_Cap_Perc))^2
cat('The R-square of the test data is ', round(rsq_Q1,3), '\n')
#R-squared is 0.212


#Saving our datasets for future usage
write.csv(train_basket_Q1, file = "train_basket_Q1.csv", row.names = FALSE)
write.csv(test_basket_Q1, file = "test_basket_Q1.csv", row.names = FALSE)

#Use this code to upload the datasets when needed
#train_basket_Q1 <- read.csv(file = 'train_basket_Q1.csv')
#test_basket_Q1 <- read.csv(file = 'test_basket_Q1.csv')



## b. SEASONS 2004-05 to 2008-09

#Train & Test for the above_mentioned 5 seasons with Variable Selection.
train_basket_Q2 = pre_treat(train_basket)
test_basket_Q2 = pre_treat(test_basket)
#In particular, vars left are: Salary_Cap_Perc, pts, rookie_contract, drb, age, bird_rights, g, tov, US_Player, ft_pct, stl, ast, blk,
#                              ts_pct, pos_PG, fta, ws, vorp, orb, orb_pct, ast_pct, blk_pct, pos_PF


#Divide X and Y in test
test_basket_Q2_x = subset(test_basket_Q2, select = -Salary_Cap_Perc) # feature and target array
test_basket_Q2_y = test_basket_Q2[, "Salary_Cap_Perc"]


### b1. INVESTIGATING WHAT'S RELEVANT###

#Simple Linear Regression
lm_model_Q2 <- lm(Salary_Cap_Perc ~ ., data=train_basket_Q2)

#Forward Stepwise Regression::Using AIC
Forw_reg_Q2 <- ols_step_forward_aic(lm_model_Q2, progress = TRUE) 
Forw_reg_Q2
#22 Vars Left


### b2. REGRESSION & PREDICTION###

#Simple Linear Regression with 12 Vars
lm_model2_Q2 <- lm(Salary_Cap_Perc ~ ., data=train_basket_Q2)

#Predictions
predictions_Q2 <- predict(lm_model2_Q2, newdata=test_basket_Q2_x)


### b3. ANALYSIS OF RESULTS ###

#RMSE on test set
RMSE_Q2 <- sqrt(mean((test_basket_Q2_y - predictions_Q2)^2))
cat('The root mean square error of the test data is ', round(RMSE_Q2,3),'\n')
#RMSE is 0.041

#R-squared
rsq_Q2 <- (cor(predictions_Q2, test_basket_Q2$Salary_Cap_Perc))^2
cat('The R-square of the test data is ', round(rsq_Q2,3), '\n')
#R-squared is 0.53



#Saving our datasets for future usage
write.csv(train_basket_Q2, file = "train_basket_Q2.csv", row.names = FALSE)
write.csv(test_basket_Q2, file = "test_basket_Q2.csv", row.names = FALSE)

#Use this code to upload the datasets when needed
#train_basket_Q2 <- read.csv(file = 'train_basket_Q2.csv')
#test_basket_Q2 <- read.csv(file = 'test_basket_Q2.csv')




## c. SEASONS 2009-10 to 2013-14

#Train & Test for the above_mentioned 5 seasons with Variable Selection.
train_basket_Q3 = pre_treat(train_basket)
test_basket_Q3 = pre_treat(test_basket)
#In particular, vars left are: Salary_Cap_Perc, fg, rookie_contract, bird_rights, age, fta, blk, g, US_Player, maximum_contract, ts_pct,
#                              drb, trb, pf, ast, pos_PG, usg_pct, bpm


#Divide X and Y in test
test_basket_Q3_x = subset(test_basket_Q3, select = -Salary_Cap_Perc) # feature and target array
test_basket_Q3_y = test_basket_Q3[, "Salary_Cap_Perc"]


### c1. INVESTIGATING WHAT'S RELEVANT###

#Simple Linear Regression
lm_model_Q3 <- lm(Salary_Cap_Perc ~ ., data=train_basket_Q3)

#Forward Stepwise Regression::Using AIC
Forw_reg_Q3 <- ols_step_forward_aic(lm_model_Q3, progress = TRUE) 
Forw_reg_Q3
#18 Vars Left


### c2. REGRESSION & PREDICTION###

#Simple Linear Regression with 12 Vars
lm_model2_Q3 <- lm(Salary_Cap_Perc ~ ., data=train_basket_Q3)

#Predictions
predictions_Q3 <- predict(lm_model2_Q3, newdata=test_basket_Q3_x)


### c3. ANALYSIS OF RESULTS ###

#RMSE on test set
RMSE_Q3 <- sqrt(mean((test_basket_Q3_y - predictions_Q3)^2))
cat('The root mean square error of the test data is ', round(RMSE_Q3,3),'\n')
#RMSE is 0.043

#R-squared
rsq_Q3 <- (cor(predictions_Q3, test_basket_Q3$Salary_Cap_Perc))^2
cat('The R-square of the test data is ', round(rsq_Q3,3), '\n')
#R-squared is 0.637


#Saving our datasets for future usage
write.csv(train_basket_Q3, file = "train_basket_Q3.csv", row.names = FALSE)
write.csv(test_basket_Q3, file = "test_basket_Q3.csv", row.names = FALSE)

#Use this code to upload the datasets when needed
#train_basket_Q3 <- read.csv(file = 'train_basket_Q3.csv')
#test_basket_Q3 <- read.csv(file = 'test_basket_Q3.csv')




## d. SEASONS 2014-15 to 2019-20

#Train & Test for the above_mentioned 5 seasons with Variable Selection.
train_basket_Q4 = pre_treat(train_basket)
test_basket_Q4 = pre_treat(test_basket)
#In particular, vars left are: Salary_Cap_Perc, pts, bird_rights, maximum_contract, age, gs, drb, three_p_pct, pf, mp, rookie_contract, super_max_contract, pos_C,
#                              pos_PF, fta, fga, US_Player, pos_PG, ast, ast_pct, trb_pct, stl, stl_pct, ft_pct, three_pa, tov_pct, usg_pct


#Divide X and Y in test
test_basket_Q4_x = subset(test_basket_Q4, select = -Salary_Cap_Perc) # feature and target array
test_basket_Q4_y = test_basket_Q4[, "Salary_Cap_Perc"]


### d1. INVESTIGATING WHAT'S RELEVANT###

#Simple Linear Regression
lm_model_Q4 <- lm(Salary_Cap_Perc ~ ., data=train_basket_Q4)

#Forward Stepwise Regression::Using AIC
Forw_reg_Q4 <- ols_step_forward_aic(lm_model_Q4, progress = TRUE) 
Forw_reg_Q4
#27 Vars Left


### d2. REGRESSION & PREDICTION###

#Simple Linear Regression with 12 Vars
lm_model2_Q4 <- lm(Salary_Cap_Perc ~ ., data=train_basket_Q4)

#Predictions
predictions_Q4 <- predict(lm_model2_Q4, newdata=test_basket_Q4_x)


### d3. ANALYSIS OF RESULTS ###

#RMSE on test set
RMSE_Q4 <- sqrt(mean((test_basket_Q4_y - predictions_Q4)^2))
cat('The root mean square error of the test data is ', round(RMSE_Q4,3),'\n')
#RMSE is 0.041

#R-squared
rsq_Q4 <- (cor(predictions_Q4, test_basket_Q4$Salary_Cap_Perc))^2
cat('The R-square of the test data is ', round(rsq_Q4,3), '\n')
#R-squared is 0.699 (WOOOOOOW!)


#Saving our datasets for future usage
write.csv(train_basket_Q4, file = "train_basket_Q4.csv", row.names = FALSE)
write.csv(test_basket_Q4, file = "test_basket_Q4.csv", row.names = FALSE)

#Use this code to upload the datasets when needed
#train_basket_Q4 <- read.csv(file = 'train_basket_Q4.csv')
#test_basket_Q4 <- read.csv(file = 'test_basket_Q4.csv')
