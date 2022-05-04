##SCOPE: REDUCING MULTICOLLINEARITY --> Excluding vars with VIF > 10
## BUT FOUND THE NEED TO APPLY PCA! SOME VARIABLES ARE PERFECTLY CORRELATED

# clear environment & set working directory
rm(list=ls()) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Package import
listofpackages <- c("quantmod","PerformanceAnalytics","ellipse","reshape2","ggplot2", "rvest",
                    "stringr", "dygraphs", "dplyr","forecast", "aod","readr","rvest","lubridate", 
                    "xml2", "caret", "stats", "fastDummies", "pdp", 'car')

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
  
  ##**Modifications because of high correlation
  dataset = subset(dataset, select = -c(fg, fga, two_pa, efg_pct, ws, drb_pct, bpm, three_pa, fta, drb, two_p, ows, mp, per) )
  
  dataset
}


train_basket = pre_treat(train_basket)
test_basket = pre_treat(test_basket)

#Divide X and Y in test
test_basket_x = subset(test_basket, select = -Salary_Cap_Perc) # feature and target array
test_basket_y = test_basket[, "Salary_Cap_Perc"]


#UNDERSTANDING CORRELATION (exclusion of variables >90% correlated)
res <- cor(test_basket_x, method = c("pearson"))
round(res, 2)
#First round of exclusions : fg, fga, two_pa, efg_pct, ws, drb_pct, bpm --> Function modified**
#Second round of exclusions : three_pa, fta, drb, two_p, ows, mp, per  --> Function modified**

col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = res, col = col, symm = TRUE)


#with(train_basket , table( category, SubCategory) )

### 2. REGRESSION & PREDICTION###

#Simple Linear Regression 
lm_model <- lm(Salary_Cap_Perc ~ ., data=train_basket)
lm_model

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

#No significant improvements