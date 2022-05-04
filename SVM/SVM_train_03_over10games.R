# 20630 - Introduction to Sports Analytics
# Group 2 Project 3 - Supervised Machine Learning Model
# Model: Support Vector Machine
# Seasons: 1999-2021

#Clear environment & set working directory
rm(list=ls()) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Package download & import
listofpackages <- c("quantmod","PerformanceAnalytics","ellipse","reshape2","ggplot2", "rvest",
                    "stringr", "dygraphs", "dplyr","forecast", "aod","readr","rvest","lubridate", 
                    "xml2", "caret", "gbm", "fastDummies", "pdp", "e1071", "kernlab")

for (j in listofpackages){
  if(sum(installed.packages()[, 1] == j) == 0) {
    install.packages(j)
  }
  library(j, character.only = T)
}



### 1. PREPROCESSING ###

#Load Data (both train and test)
train_basket <- read.csv('../Dataset/Final Datasets/Final_data_Bplayers_2000_TRAIN.csv')
test_basket <- read.csv('../Dataset/Final Datasets/Final_data_Bplayers_2000_TEST.csv')

#We drop players that played less than 20 games in a season
train_basket = subset(train_basket, g >= 20)
test_basket = subset(test_basket, g >= 20)

#Creating and applying a function for common pre-treatment of train and test and selecting seasons
pre_treat <- function(dataset){
  
  #Create dummies for the role
  dataset$pos = lapply(dataset$pos, function(x) if (nchar(x)>2) {x=unlist(str_split(x, ",", simplify = TRUE)[1,1])} else {x})
  dataset$pos = unlist(dataset[,"pos"])
  dataset <- dummy_cols(dataset, select_columns = 'pos')
  
  #Selecting Seasons
  dataset <- dataset %>% filter(season == "2014-15"| season == "2015-16" | season == "2016-17" | season == "2017-18" | season == "2018-19" | season == "2019-20" | season == "2020-21")
  
  #Let's drop columns we won't use
  drops <- c("season","Player",'tm','lg','Salary_Cap','Salary', 'pos', 'Image_Link')
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

#Cross validation

set.seed(123)

trctrl <- trainControl(method = "cv", number = 5,)



### 2. MODELS ###

#Different kernel options for SVM, let's see how they perform

svm_radial <- train(Salary_Cap_Perc ~ ., data = train_basket, method = 'svmRadial', 
                    preProcess = c("center", "scale"), trCtrl = trctrl)

print(svm_radial)
# C     RMSE        Rsquared   MAE       
# 1.00  0.03784008  0.7618318  0.02554107


svm_linear <- train(Salary_Cap_Perc ~ ., data = train_basket, method = 'svmLinear',
                    preProcess = c("center", "scale"), trCtrl = trctrl)

print(svm_linear)
# RMSE        Rsquared   MAE       
# 0.04148102  0.7247262  0.02818456


svm_poly <- train(Salary_Cap_Perc ~ ., data = train_basket, method = 'svmPoly', 
                  preProcess = c("center", "scale"), trCtrl = trctrl)

print(svm_poly)
# degree  scale  C     RMSE        Rsquared   MAE       
# 2       0.010  0.50  0.03727653  0.7720585  0.02521112

#Save model
saveRDS(svm_radial, file = "svm_radial_over20games.Rds")
saveRDS(svm_poly, file = "svm_poly_over20games.Rds")

#Load model
# svm_radial <- readRDS(file = "svm_radial_over20games.Rds")
# svm_poly <- readRDS(file = "svm_poly_over20games.Rds")

#Predict using different kernels
pred_radial = predict(svm_radial, newdata=test_basket_x)
pred_linear = predict(svm_linear, newdata=test_basket_x)
pred_poly = predict(svm_poly, newdata=test_basket_x)

#RMSE
RMSE_radial = sqrt(mean((test_basket_y - pred_radial)^2))
RMSE_linear = sqrt(mean((test_basket_y - pred_linear)^2))
RMSE_poly = sqrt(mean((test_basket_y - pred_poly)^2))

print(c(RMSE_radial, RMSE_linear, RMSE_poly))
# Radial: 0.03576529
# Linear: 0.03880282
# Poly:   0.03407730 

cor(test_basket_y, pred_radial)^2  # 0.7956651
cor(test_basket_y, pred_linear)^2  # 0.7583031
cor(test_basket_y, pred_poly)^2    # 0.8126302

### Best RMSE:
### Poly: 0.03407730



### 3. HYPER-PARAMETERS TUNING (ALL VARIABLES) ###

tuneGrid_radial <- expand.grid(C = c(0.10, 0.25, 0.5, 0.75, 1, 1.25, 1.5), sigma = c(0.001, 0.005, 0.01, 0.015))
tuneGrid_poly <- expand.grid(degree = c(1, 2, 3, 5), scale = c(0.001, 0.003, 0.005, 0.01), C = c(0.10, 0.25, 0.5, 0.75, 1, 1.25, 1.5))

svm_radial_tuned <- train(Salary_Cap_Perc ~ ., data = train_basket, method = 'svmRadial', 
                    preProcess = c("center", "scale"), trCtrl = trctrl, 
                    tuneGrid = tuneGrid_radial)

svm_poly_tuned <- train(Salary_Cap_Perc ~ ., data = train_basket, method = 'svmPoly', 
                          preProcess = c("center", "scale"), trCtrl = trctrl, 
                          tuneGrid = tuneGrid_poly)

print(svm_radial_tuned)
# C     sigma  RMSE        Rsquared   MAE       
# 1.50  0.005  0.03861715  0.7592025  0.02633224

print(svm_poly_tuned)
# degree  scale  C     RMSE        Rsquared   MAE
# 2       0.005  1.50  0.03744592  0.7711941  0.02529181

#Save model
saveRDS(svm_radial_tuned, file = "svm_radial_tuned_over20games.Rds")
saveRDS(svm_poly_tuned, file = "svm_poly_tuned_over20games.Rds")

#Load model
# svm_radial_tuned <- readRDS(file = "svm_radial_tuned_over20games.Rds")
# svm_poly_tuned <- readRDS(file = "svm_poly_tuned_over20games.Rds")

#Prediction
pred_radial_tuned = predict(svm_radial_tuned, newdata=test_basket_x)
pred_poly_tuned = predict(svm_poly_tuned, newdata=test_basket_x)

RMSE_radial_tuned = sqrt(mean((test_basket_y - pred_radial_tuned)^2))
RMSE_poly_tuned = sqrt(mean((test_basket_y - pred_poly_tuned)^2))

print(c(RMSE_radial, RMSE_radial_tuned))  # Radial:         0.03576529          
                                          # Radial (tuned): 0.03409257
print(c(RMSE_poly, RMSE_poly_tuned))      # Poly:           0.03407730 
                                          # Poly (tuned):   0.03404425  

cor(test_basket_y, pred_radial_tuned)^2   # 0.8130788
cor(test_basket_y, pred_poly_tuned)^2     # 0.8127785 

### Best RMSE:
### Poly (tuned): 0.03404425 



### 4. RESULTS ANALYSIS ###

best_model <- readRDS(file = "svm_poly_tuned_over20games.Rds")
final_test_basket <- read.csv('../Dataset/Final Datasets/Final_data_Bplayers_2000_TEST.csv')
final_test_basket = pre_treat(final_test_basket)
final_test_basket = subset(final_test_basket, g >= 20)

pred_basket_y = predict(best_model, newdata=test_basket_x)
RMSE = sqrt(mean((test_basket_y - pred_basket_y)^2))
cat('The root mean square error of the test data is ', round(RMSE,6),'\n')
# The root mean square error of the test data is  0.031959

rsq <- (cor(pred_basket_y, test_basket$Salary_Cap_Perc))^2
cat('The R-square of the test data is ', round(rsq,6), '\n')
# The R-square of the test data is  0.824852

# Visualize the model, actual and predicted data
x_ax = 1:length(pred_basket_y)
plot(x_ax, test_basket_y, col="blue", pch=20, cex=.9)
lines(x_ax, pred_basket_y, col="red", pch=20, cex=.9) 

# Let's analyse predictions 
final_test_basket$Prediction = pred_basket_y
final_test_basket$Pred_Diff = final_test_basket$Salary_Cap_Perc - final_test_basket$Prediction

# Let's see which are the contract connected with the highest/lowest difference with Real %
maxdata = aggregate(data = final_test_basket, Pred_Diff~contract_type, FUN = mean)

p <- ggplot(data=maxdata, aes(x=contract_type, y=Pred_Diff)) +
     geom_bar(stat="identity", fill="steelblue")+
     theme(axis.text.x = element_text(angle = 45, vjust = 0.95, hjust=1))
p
