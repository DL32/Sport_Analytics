# 20630 - Introduction to Sports Analytics
# Group 2 Project 3 - Supervised Machine Learning Model
# Model 2: K-Nearest Neighbor

# clear environment & set working directory
rm(list=ls()) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Package import
my_packages <- c("quantmod","PerformanceAnalytics","ellipse","reshape2","ggplot2", "rvest", "stringr",
                 "dygraphs", "dplyr","forecast", "aod","readr","rvest","lubridate", "xml2", "caret", "fastDummies")
lapply(my_packages, require, character.only = TRUE)

# use following code to read models
model_1 <- readRDS(file = "knn_1.Rds")
model_2 <- readRDS(file = 'knn_2.Rds')
model_3 <- readRDS(file = 'knn_3.Rds')
model_4 <- readRDS(file = 'knn_4.Rds')
model_5 <- readRDS(file = 'knn_5.Rds')

#Load Data (both train and test)
train_basket <- read.csv('../Dataset/Final\ Datasets/Final_data_Bplayers_2000_TRAIN.csv')
test_basket <- read.csv('../Dataset/Final\ Datasets/Final_data_Bplayers_2000_TEST.csv')
final_test_basket = test_basket %>% filter(season == "2014-15"| season == "2015-16" | season == "2016-17" | season == "2017-18" | season == "2018-19" | season == "2019-20" | season == "2020-21")


pre_treat <- function(dataset){
  
  #Create dummies for position
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

train_basket_tr = pre_treat(train_basket)
test_basket_tr = pre_treat(test_basket)


#Splitting off independent variables
test_basket_x = subset(test_basket_tr, select = -Salary_Cap_Perc) # feature and target array
test_basket_y = test_basket_tr[, "Salary_Cap_Perc"]

################################
# Training of Models           #
# Consider computational costs #
################################
#training a simple k=5 model without any dimensionality reduction techniques
model_1 = knnreg( Salary_Cap_Perc ~ ., data=train_basket)
model_1

#more complex model that tries different k's
model_2 <- train(
  Salary_Cap_Perc ~.,
  data = train_basket,
  method = 'knn')
model_2



#next, let's try a model where (as a pre-processing step), we standardize and center our features
model_3 <- train(
  Salary_Cap_Perc ~.,
  data = train_basket,
  preProcess = c("center", "scale"),
  method = 'knn')



ctrl <- trainControl(method="repeatedcv",repeats = 3)

model_4 <- train(
  Salary_Cap_Perc ~.,
  data = train_basket,
  trControl = ctrl,
  preProcess = c("center", "scale"),
  method = 'kknn',
  tuneLength = 20)

####################
# End of Training  #
####################

model_1
model_2
plot(model_2)
model_3
plot(model_3)
model_4
plot(model_4)

#predict using different models
predictions_1 <- predict(model_1, newdata=test_basket_x)
predictions_2 <- predict(model_2, newdata=test_basket_x)
predictions_3 <- predict(model_3, newdata=test_basket_x)
predictions_4 <- predict(model_4, newdata=test_basket_x)

# Root Mean Standard Error on Test Set
RMSE_01 <- sqrt(mean((test_basket_y - predictions_1)^2)) # 0.04957655
RMSE_02 <- sqrt(mean((test_basket_y - predictions_2)^2)) # 0.04813333
RMSE_03 <- sqrt(mean((test_basket_y - predictions_3)^2)) # 0.03634415
RMSE_04 <- sqrt(mean((test_basket_y - predictions_4)^2)) # 0.03469860

print(c(RMSE_01, RMSE_02, RMSE_03, RMSE_04))

cor(test_basket_y, predictions_1) ^ 2 #0.5106181
cor(test_basket_y, predictions_2) ^ 2 #0.5367857
cor(test_basket_y, predictions_3) ^ 2 #0.736463
cor(test_basket_y, predictions_4) ^ 2 #0.7593972

#plot best model against truth
x_ax = 1:length(predictions_4)
plot(x_ax, test_basket_y, col="blue", pch=20, cex=.9)
lines(x_ax, predictions_4, col="red", pch=20, cex=.9) 


## RUN THE BEST MODEL ON THE LAST 7 SEASONS ##
#Preprocessing
pre_treat_7 <- function(dataset){
  
  #Create dummies for position
  dataset$pos = lapply(dataset$pos, function(x) if (nchar(x)>2) {x=unlist(str_split(x, ",", simplify = TRUE)[1,1])} else {x})
  dataset$pos = unlist(dataset[,"pos"])
  dataset <- dummy_cols(dataset, select_columns = 'pos')
  
  #Selecting Seasons
  dataset <- dataset %>% filter(season == "2014-15"| season == "2015-16" | season == "2016-17" | season == "2017-18" | season == "2018-19" | season == "2019-20"| season == "2020-21") 
  
  #Let's drop columns we won't use
  drops <- c("season","Player",'tm','lg','Salary_Cap','Salary', 'pos', 'Image_Link')
  dataset = dataset[ , !(names(dataset) %in% drops)]
  
  #We replace NA with 0
  dataset[is.na(dataset)] = 0
  
  dataset
}

train_basket_7 = pre_treat_7(train_basket)
test_basket_7 = pre_treat_7(test_basket)

#Divide test set in X and Y
test_basket7_x = subset(test_basket_7, select = -Salary_Cap_Perc)
test_basket7_y = test_basket_7[, "Salary_Cap_Perc"]


#### TRAINING ####
#run best model on just the last 7 years of data
model_5 <- train(
  Salary_Cap_Perc ~.,
  data = train_basket_7,
  trControl = ctrl,
  preProcess = c("center", "scale"),
  method = 'kknn',
  tuneLength = 20)

#### END OF TRAINING ####

model_5
plot(model_5)

predictions_5 <- predict(model_5, newdata=test_basket7_x)
RMSE_05 <- sqrt(mean((test_basket7_y - predictions_5)^2)) # 0.03316408
cor(test_basket7_y, predictions_5) ^ 2 #0.8081245


#Let's analyse predictions
final_test_basket$Prediction = predictions_5
final_test_basket$Pred_Diff = final_test_basket$Salary_Cap_Perc - final_test_basket$Prediction
final_test_basket$pos_eval = lapply(final_test_basket$pos, function(x) if (nchar(x)>2) {x=unlist(str_split(x, ",", simplify = TRUE)[1,1])} else {x})
final_test_basket$pos_eval = unlist(final_test_basket[,"pos_eval"])

#Let's see which are the contract connected with the highest/lowest difference with Real %
maxdata = aggregate(data = final_test_basket, Pred_Diff~contract_type+pos_eval, FUN = mean)
ggplot(maxdata, aes(x=contract_type, y=Pred_Diff, fill=pos_eval)) +
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.95, hjust=1))

#Barplot to see who is higher in occurrencies (if higher or lower)
final_test_basket$High_Low = lapply(final_test_basket$Pred_Diff, function(x) if (x>0) {x = "Higher"} else {x = "Lower"})
final_test_basket$High_Low = unlist(final_test_basket[,"High_Low"])

ggplot(final_test_basket, aes(x=High_Low, fill=pos_eval)) +
  geom_bar(stat="count", width=0.6, position=position_dodge())+
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, position = position_dodge(0.6), color="white")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.95, hjust=1))

#Barplot to analyze differences across Nationality
ggplot(final_test_basket, aes(x=US_Player, fill=High_Low)) +
  geom_bar(stat="count", width=0.6, position=position_dodge())+
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, position = position_dodge(0.6), color="white")+
  theme(axis.text.x = element_text(angle = 45))
#Increase of 3.34% in underpayment if you're from US!

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
    to_app = data.frame(Name = name,Salary_Cap_Perc = "True", Amount = sal_perc)
    to_app = rbind(to_app, data.frame(Name = name,Salary_Cap_Perc = "Predicted", Amount = pr))
    dat = rbind(dat, to_app)
  }
  ggplot(data=dat, aes(x=Name, y=Amount, fill=Salary_Cap_Perc)) +
    geom_bar(stat="identity", position=position_dodge())+
    scale_fill_brewer(palette="Paired")+
    theme(axis.text.x = element_text(angle = 45, vjust = 0.95, hjust=1))
}

plotbest_worst(final_test_basket[order(-final_test_basket$Pred_Diff),])
plotbest_worst(final_test_basket[order(final_test_basket$Pred_Diff),])


#save models so we don't have to retrain every time
saveRDS(model_1, file = "knn_1.Rds")
saveRDS(model_2, file = "knn_2.Rds")
saveRDS(model_3, file = "knn_3.Rds")
saveRDS(model_4, file = "knn_4.Rds")
saveRDS(model_5, file = "knn_5.Rds")



