# 20630 - Introduction to Sports Analytics
# Group 2 Project 3 - Supervised Machine Learning Model
# Applying PCA to the dataset of the seasons 2014-15 onwards

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

### 1. PREPROCESSING & INVESTIGATION 

#Load Data (both train and test)
train_basket <- read.csv('../Sportylytics-main/train_basket_Q4.csv')
test_basket <- read.csv('../Sportylytics-main/test_basket_Q4.csv')

#Creating and applying a function for common pre-treatment of train and test
pre_treat <- function(dataset){
  
  #Create dummies for the role
  dataset$pos = lapply(dataset$pos, function(x) if (nchar(x)>2) {x=unlist(str_split(x, ",", simplify = TRUE)[1,1])} else {x})
  dataset$pos = unlist(dataset[,"pos"])
  dataset <- dummy_cols(dataset, select_columns = 'pos')
  
  #Let's drop columns we won't use
  drops <- c("season","Player",'tm','lg','Salary_Cap','Salary', 'pos', 'Image_Link')
  dataset = dataset[ , !(names(dataset) %in% drops)]
  
  #We replace NA with 0
  dataset[is.na(dataset)] = 0

  dataset
}


#Divide X and Y in test
test_basket_Q4_x = subset(test_basket, select = -Salary_Cap_Perc) # feature and target array
test_basket_Q4_y = test_basket[, "Salary_Cap_Perc"]


#Fit the PCR model
set.seed(42) #make this example reproducible
model <- pcr(Salary_Cap_Perc ~ ., data=train_basket, scale=TRUE, validation="CV")

summary(model)
#Here, RMSEP explains the RMSE on the basis of the # of components chosen
#And, Training tells the % of variance explained

#visualize cross-validation plots
validationplot(model)
validationplot(model, val.type="MSEP")
validationplot(model, val.type="R2")
#--> 15 out of 53 components explain most of what we are interested in

#Prediction
pcr_pred <- predict(model, test_basket, ncomp=15)


#RMSE on test set 
RMSE_01 <- sqrt(mean((test_basket_Q4_y - pcr_pred)^2))
cat('The root mean square error of the test data is ', round(RMSE_01,3),'\n')
#RMSE is 0.041

#R-squared 
rsq_01 <- (cor(pcr_pred, test_basket$Salary_Cap_Perc))^2
cat('The R-square of the test data is ', round(rsq_01,3), '\n')
#R-squared is 0.697