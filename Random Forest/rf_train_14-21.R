# 20630 - Introduction to Sports Analytics
# Group 2 Project 3 - Supervised Machine Learning Model
# Model X: Random Forest

# clear environment & set working directory
rm(list=ls()) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Package import
listofpackages <- c("quantmod","PerformanceAnalytics","ellipse","reshape2","ggplot2", "rvest",
                    "stringr", "dygraphs", "dplyr","forecast", "aod","readr","rvest","lubridate", 
                    "xml2", "caret", "gbm", "fastDummies", "pdp","randomForest")

# Uploading libraries
for (j in listofpackages){
  if(sum(installed.packages()[, 1] == j) == 0) {
    install.packages(j)
  }
  library(j, character.only = T)
}

### 1. PREPROCESSING ###

#Load Data (both train and test)
train_basket <- read.csv('../Dataset/Final Datasets/Final_data_Bplayers_2000_TRAIN.csv')
test_basket <- read.csv('../Dataset/Final Datasets/Final_data_Bplayers_2000_TEST.csv',encoding = "latin1")
final_test_basket = test_basket %>% filter(season == "2014-15"| season == "2015-16" | season == "2016-17" | season == "2017-18" | season == "2018-19" | season == "2019-20" | season == "2020-21")

#Creating and applying a function for common pre-treatment of train and test
pre_treat <- function(dataset){
  
  #Create dummies for position
  dataset$pos = lapply(dataset$pos, function(x) if (nchar(x)>2) {x=unlist(str_split(x, ",", simplify = TRUE)[1,1])} else {x})
  dataset$pos = unlist(dataset[,"pos"])
  dataset <- dummy_cols(dataset, select_columns = 'pos')
  
  #Selecting Seasons
  dataset <- dataset %>% filter(season == "2014-15"| season == "2015-16" | season == "2016-17" | season == "2017-18" | season == "2018-19" | season == "2019-20" | season == "2020-21")
 
   #drop columns we won't use
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

##### TRAINING #####
### Random Forest ###
set.seed(123)
rf = randomForest(train_basket$Salary_Cap_Perc~. ,
                  data = train_basket)


#saveRDS(rf, file = "rf_simple_14-21.Rds")
#Load the model
model1 <- readRDS(file = "rf_simple_14-21.Rds")
print(model1)
model1_pred = predict(model1, test_basket_x)

rmse_model1 = sqrt(mean((test_basket_y - model1_pred)^2))
cat('The root mean square error of the test data is ', round(rmse_model1,6),'\n')
#rmse is 0.035103
rsq <- (cor(model1_pred, test_basket$Salary_Cap_Perc))^2
cat('The R-square of the test data is ', round(rsq,6), '\n')
#R is 0.787022

#Tuning the model

hyper_grid <- expand.grid(
  n_tree = c(500,1000,1500,2000),
  optimal_m = 0,               # a place to dump results
  min_RMSE = 0                     # a place to dump results
)

# grid search 
for(i in 1:nrow(hyper_grid)) {
  
  print(i)
  
  # reproducibility
  set.seed(123)
  
  # train model
  t = tuneRF(train_basket[,-4], train_basket[,4], 
             stepFactor = 0.5,
             plot = TRUE, 
             mtryStart = 1,
             ntreeTry=hyper_grid$n_tree[i], #didn't improve much increasing further the size
             trace = TRUE,
             improve = 0.01)
  
  # add min training error and trees to grid
  hyper_grid$optimal_m[i] <- t[t[, 2] == min(t[, 2]), 1]
  hyper_grid$min_RMSE[i] <- min(t[,2])
}

hyper_grid %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10)


#n_tree optimal_m    min_RMSE
#1   2000        32 0.001372958
#2   1500        32 0.001373978
#3   1000        32 0.001375052
#4    500        32 0.001378118


# Train on the best model
set.seed(123)
rf_final = randomForest(train_basket$Salary_Cap_Perc~. ,
                        data = train_basket,
                        ntree = 2000,
                        mtry = 32,
                        importance = TRUE)

saveRDS(rf_final, file = "rf_final_14-21.Rds")
##### END OF TRAINING #####

# Load the model
model <- readRDS(file = "rf_final_14-21.Rds")
model_pred = predict(model, test_basket_x)

rmse_model = sqrt(mean((test_basket_y - model_pred)^2))
cat('The root mean square error of the test data is ', round(rmse_model,6),'\n')
#rmse is 0.034891 
rsq <- (cor(model_pred, test_basket$Salary_Cap_Perc))^2
cat('The R-square of the test data is ', round(rsq,6), '\n')
#R is 0.787836

###  ANALYSIS OF RESULTS ###

# visualize the model, actual and predicted data
x_ax = 1:length(model_pred)
plot(x_ax, test_basket_y, col="blue", pch=20, cex=.9)
lines(x_ax, model_pred, col="red", pch=20, cex=.9) 

#Evaluate variable importance
importance(model)
varImpPlot(model, n.var = 10)


#Let's analyse predictions
final_test_basket = final_test_basket  %>% filter(season == "2014-15"| season == "2015-16" | season == "2016-17" | season == "2017-18" | season == "2018-19" | season == "2019-20" | season == "2020-21")
#Dummy pos
final_test_basket$pos_eval = lapply(final_test_basket$pos, function(x) if (nchar(x)>2) {x=unlist(str_split(x, ",", simplify = TRUE)[1,1])} else {x})
final_test_basket$pos_eval = unlist(final_test_basket[,"pos_eval"])

final_test_basket$Prediction = model_pred
final_test_basket$Pred_Diff = final_test_basket$Salary_Cap_Perc - final_test_basket$Prediction
#High if diff >0, low o.w.
final_test_basket$High_Low = lapply(final_test_basket$Pred_Diff, function(x) if (x>0) {x = "Higher"} else {x = "Lower"})
final_test_basket$High_Low = unlist(final_test_basket[,"High_Low"])

#Barplot to see who is higher in occurrences (if higher or lower)
ggplot(final_test_basket, aes(x=High_Low, fill=pos_eval)) +
  geom_bar(stat="count", width=0.6, position=position_dodge())+
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, position = position_dodge(0.6), color="white")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.95, hjust=1))

#Let's see which are the contract connected with the highest/lowest difference with Real %
maxdata = aggregate(data = final_test_basket, Pred_Diff~contract_type+pos_eval, FUN = mean)
ggplot(maxdata, aes(x=contract_type, y=Pred_Diff, fill=pos_eval)) +
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.95, hjust=1))

#Barplot to analyze differences across Nationality
ggplot(final_test_basket, aes(x=US_Player, fill=High_Low)) +
  geom_bar(stat="count", width=0.6, position=position_dodge())+
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, position = position_dodge(0.6), color="white")+
  theme(axis.text.x = element_text(angle = 45))
#Increase of 8% in underpayment if you're from US!

#Barplot to analyze Age
ggplot(final_test_basket, aes(x=age, fill=High_Low)) +
  geom_bar(stat="count", width=0.6, position=position_dodge())+
  theme(axis.text.x = element_text(angle = 45))
#Trend of underpayment tends to decrease as players become older! Great finding

#Now we see which are the players with highest/lowest difference
plotbest_worst <- function(dataset){
  dat = data.frame()
  for (i in 1:10) {
    name = dataset[i, "Player"]
    sal_perc = dataset[i, "Salary_Cap_Perc"]
    pr = dataset[i, "Prediction"]
    season = dataset[i,"season"]
    to_app = data.frame(Name = paste(name, season, sep = " "),Salary_Cap_Perc = "True", Amount = sal_perc)
    to_app = rbind(to_app, data.frame(Name = paste(name, season, sep = " "),Salary_Cap_Perc = "Predicted", Amount = pr))
    dat = rbind(dat, to_app)
  }
  ggplot(data=dat, aes(x=Name, y=Amount, fill=Salary_Cap_Perc)) +
    geom_bar(stat="identity", position=position_dodge())+
    scale_fill_brewer(palette="Paired")+
    theme(axis.text.x = element_text(angle = 65, vjust = 0.95, hjust=1))
}
final_test_basket = subset(final_test_basket, final_test_basket$g > 20)
plotbest_worst(final_test_basket[order(final_test_basket$Pred_Diff),]) #Underpaid
plotbest_worst(final_test_basket[order(-final_test_basket$Pred_Diff),]) #Overpaid

