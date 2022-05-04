# 20630 - Introduction to Sports Analytics
# Group 2 Project 3 - Supervised Machine Learning Model
# Working on the baseline model: PCA


# clear environment & set working directory
rm(list=ls()) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Package import
listofpackages <- c("quantmod","PerformanceAnalytics","ellipse","reshape2","ggplot2", "rvest",
                    "stringr", "dygraphs", "dplyr","forecast", "aod","readr","rvest","lubridate", 
                    "xml2", "caret", "pls", "stats", "fastDummies", "pdp", 'car')

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

### 2. INVESTIGATION & PREDICTION###

#PCA investigation
pca <- prcomp(train_basket, scale = TRUE)
#reverse the signs (because in R by default the negative eigenvectors are referred to)
pca$rotation <- -1*pca$rotation

#calculate total variance explained by each principal component
pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100,1)
barplot(pca.var.per, main="Scree Plot", xlab="Principal Component",
        ylab= "Percent Variation")

#Cumulative explained variance
pca.cum.ev <- cumsum(pca$sdev^2 / sum(pca$sdev^2))
barplot(pca.cum.ev, main="Scree Plot", xlab="Principal Component",
        ylab= "Percent Variation")



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
#--> 18 out of 56 components explain most of what we are interested in

#Prediction
pcr_pred <- predict(model, test_basket, ncomp=18)


### 3. ANALYSIS OF RESULTS ###

#RMSE on test set 
RMSE_01 <- sqrt(mean((test_basket_y - pcr_pred)^2))
cat('The root mean square error of the test data is ', round(RMSE_01,3),'\n')
#RMSE is 0.042

#R-squared 
rsq_01 <- (cor(pcr_pred, test_basket$Salary_Cap_Perc))^2
cat('The R-square of the test data is ', round(rsq_01,3), '\n')
#R-squared is 0.646

#No improvements in terms of performances, but more robust and reliable model