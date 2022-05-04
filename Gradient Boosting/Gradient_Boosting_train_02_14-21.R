# 20630 - Introduction to Sports Analytics
# Group 2 Project 3 - Supervised Machine Learning Model
# Model 3: Gradient Boosting

# clear environment & set working directory
rm(list=ls()) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Package import
listofpackages <- c("quantmod","PerformanceAnalytics","ellipse","reshape2","ggplot2", "rvest",
                    "stringr", "dygraphs", "dplyr","forecast", "aod","readr","rvest","lubridate", 
                    "xml2", "caret", "gbm", "fastDummies", "pdp")

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
test_basket <- read.csv('../Dataset/Final Datasets/Final_data_Bplayers_2000_TEST.csv')
final_test_basket = test_basket
#Selecting Seasons
final_test_basket <- final_test_basket %>% filter(season == "2014-15"| season == "2015-16" | season == "2016-17" | season == "2017-18" | season == "2018-19" | season == "2019-20" | season == "2020-21") 

#Creating and applying a function for common pre-treatment of train and test
pre_treat <- function(dataset){
  
  #Create dummies for the role
  dataset$pos = lapply(dataset$pos, function(x) if (nchar(x)>2) {x=unlist(str_split(x, ",", simplify = TRUE)[1,1])} else {x})
  dataset$pos = unlist(dataset[,"pos"])
  dataset <- dummy_cols(dataset, select_columns = 'pos')
  
  #Selecting Seasons
  dataset <- dataset %>% filter(season == "2014-15"| season == "2015-16" | season == "2016-17" | season == "2017-18" | season == "2018-19" | season == "2019-20" | season == "2020-21") 
  
  #Let's drop columns we won't use
  drops <- c("season","Player",'tm','lg','Salary_Cap','Salary', 'pos', 'Image_Link', 'contract_type')
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

### 2. TUNING OF GRADIENT BOOSTING MODEL ###

# randomize data
random_index <- sample(1:nrow(train_basket), nrow(train_basket))
random_basket_train <- train_basket[random_index, ]

hyper_grid <- expand.grid(
  shrinkage = c(.01, .1, .3),
  interaction.depth = c(3, 5, 7),
  n.minobsinnode = c(5, 10),
  bag.fraction = c(.65, .8), 
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0                     # a place to dump results
)

# grid search 
for(i in 1:nrow(hyper_grid)) {
  
  print(i)
  
  # reproducibility
  set.seed(123)
  
  # train model
  gbm_tune <- gbm(
    formula = Salary_Cap_Perc ~ .,
    distribution = "gaussian",
    cv.folds = 5,
    data = random_basket_train,
    n.trees = 2500,
    interaction.depth = hyper_grid$interaction.depth[i],
    shrinkage = hyper_grid$shrinkage[i],
    n.minobsinnode = hyper_grid$n.minobsinnode[i],
    bag.fraction = hyper_grid$bag.fraction[i],
    train.fraction = .8,
    n.cores = NULL, # will use all cores by default
    verbose = FALSE
  )
  
  # add min training error and trees to grid
  hyper_grid$optimal_trees[i] <- which.min(gbm_tune$valid.error)
  hyper_grid$min_RMSE[i] <- sqrt(min(gbm_tune$valid.error))
}

hyper_grid %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10)

#      shrinkage  interaction.depth   n.minobsinnode   bag.fraction    optimal_trees    min_RMSE
#1       0.01             7                  5              0.65           1519        0.03383620
#2       0.01             7                  10             0.65           1229        0.03393017
#3       0.01             7                  5              0.80           1930        0.03400956
#4       0.10             5                  5              0.6            66          0.03404039
#5       0.01             5                  5              0.80           1958        0.03408783
#6       0.10             5                  10             0.65           244         0.03415903
#7       0.10             7                  5              0.65           101         0.03417233
#8       0.01             5                  5              0.65           1228        0.03421585
#9       0.01             7                  10             0.80           1338        0.03423356
#10      0.01             5                  10             0.65           1414        0.03436837

### 3. TRAINING ON THE BEST MODEL ###

set.seed(123)

# train GBM model
gbm_final <- gbm(
  formula = Salary_Cap_Perc ~ .,
  distribution = "gaussian",
  data = train_basket,
  n.trees = 1519,
  cv.folds = 5,
  interaction.depth = 7,
  shrinkage = 0.1,
  n.minobsinnode = 5,
  bag.fraction = .65, 
  train.fraction = 1,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
) 

saveRDS(gbm_final, file = "gbm_final_14-21.Rds")

### 4. PREDICTION ###

# use following code to read models
gbm_final <- readRDS(file = "gbm_final_14-21.Rds")

pred_basket_y = predict.gbm(gbm_final, test_basket_x)

### 5. ANALYSIS OF RESULTS ###
RMSE = sqrt(mean((test_basket_y - pred_basket_y)^2))
cat('The root mean square error of the test data is ', round(RMSE,6),'\n')
#RMSE is 0.035693

rsq <- (cor(pred_basket_y, test_basket$Salary_Cap_Perc))^2
cat('The R-square of the test data is ', round(rsq,6), '\n')
#R-square is 0.776824 

gbm.perf(gbm_final, method = "cv")

# visualize the model, actual and predicted data
x_ax = 1:length(pred_basket_y)
plot(x_ax, test_basket_y, col="blue", pch=20, cex=.9)
lines(x_ax, pred_basket_y, col="red", pch=20, cex=.9) 

#Relative influence
summary(gbm_final, cBars = 10, method = relative.influence, las = 2)

#Partial dependence
gbm_final %>%
  partial(pred.var = "mp", n.trees = gbm_final$n.trees, grid.resolution = 100) %>%
  autoplot(rug = TRUE, train = train_basket)

#Let's analyse predictions 
final_test_basket$Prediction = pred_basket_y
final_test_basket$Pred_Diff = final_test_basket$Salary_Cap_Perc - final_test_basket$Prediction
final_test_basket$pos_eval = lapply(final_test_basket$pos, function(x) if (nchar(x)>2) {x=unlist(str_split(x, ",", simplify = TRUE)[1,1])} else {x})
final_test_basket$pos_eval = unlist(final_test_basket[,"pos_eval"])

#Let's see which are the contract connected with the highest/lowest difference with Real %
maxdata = aggregate(data = final_test_basket, Pred_Diff~contract_type+pos_eval, FUN = mean)
ggplot(maxdata, aes(x=contract_type, y=Pred_Diff, fill=pos_eval)) +
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.95, hjust=1))
#TEAM OPTION has the highest difference (that means they're the players that do not
#repay the expenditure)
#MINIMUM has the lowest difference (that means they tend to have players that play
#well with low expenditure)

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
#Increase of 6.42% in underpayment if you're from US!

#Barplot to analyze Age
ggplot(final_test_basket, aes(x=age, fill=High_Low)) +
  geom_bar(stat="count", width=0.6, position=position_dodge())+
  theme(axis.text.x = element_text(angle = 45))
#Trend of underpayment tends to decrease as players become older! Great finding

#Now we see which are the players with highest/lowest difference
plotbest_worst <- function(dataset, number){
  dat = data.frame()
  for (i in 1:number) {
    name = paste(dataset[i, "Player"],dataset[i, "season"])
    sal_perc = dataset[i, "Salary_Cap_Perc"]
    pr = dataset[i, "Prediction"]
    to_app = data.frame(Name = name,Salary_Cap_Perc = "True", Amount = sal_perc)
    to_app = rbind(to_app, data.frame(Name = name,Salary_Cap_Perc = "Predicted", Amount = pr))
    dat = rbind(dat, to_app)
  }
  print(dat)
  ggplot(data=dat, aes(x=Name, y=Amount, fill=Salary_Cap_Perc)) +
    geom_bar(stat="identity", position=position_dodge())+
    scale_fill_brewer(palette="Paired")+
    theme(axis.text.x = element_text(angle = 45, vjust = 0.95, hjust=1))
}

plotbest_worst(final_test_basket[order(-final_test_basket$Pred_Diff),],10)
plotbest_worst(final_test_basket[order(final_test_basket$Pred_Diff),],10)
#Now we deal with variable importance comparing it to model with all seasons
gbm_final <- readRDS(file = "gbm_final.Rds")
pred_basket_y = predict.gbm(gbm_final, test_basket_x)
total_imp = summary(gbm_final, cBars = 10, method = relative.influence, las = 2)

gbm_final <- readRDS(file = "gbm_final_14-21.Rds")
pred_basket_y = predict.gbm(gbm_final, test_basket_x)
season_imp = summary(gbm_final, cBars = 10, method = relative.influence, las = 2)

comparison = merge(total_imp, season_imp, by = "var")
colnames(comparison) = c("var","imp_total","imp_1421")
comparison$perc_diff = ((comparison$imp_1421*100)/comparison$imp_total)
comparison = subset(comparison, imp_total != 0)

plotbest_worst2 <- function(dataset){
  dat = data.frame()
  for (i in 1:10) {
    name = dataset[i, "var"]
    imp_tot = dataset[i, "imp_total"]
    imp_1421 = dataset[i, "imp_1421"]
    to_app = data.frame(Name = name,Imp = "Total", Amount = imp_tot)
    to_app = rbind(to_app, data.frame(Name = name,Imp = "1421", Amount = imp_1421))
    dat = rbind(dat, to_app)
  }
  ggplot(data=dat, aes(x=Name, y=Amount, fill=Imp)) +
    geom_bar(stat="identity", position=position_dodge())+
    scale_fill_brewer(palette="Paired")+
    theme(axis.text.x = element_text(angle = 45, vjust = 0.95, hjust=1))
}

plotbest_worst2(comparison[order(-comparison$perc_diff),])
plotbest_worst2(comparison[order(comparison$perc_diff),])
#Variable that are increasing the most their variable importance are THREE POINT PERCENTAGE
#(with an 11x increase), followed by Box plus/minus (8x) and Defensive win Share (7.9x)
#Variable that are decreasing the most their variable importance are ROOKIE CONTRACT 
#(0.14x), followed by two_points (0.36x) and Minutes_Played (0.39x)