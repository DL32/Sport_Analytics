#Let's initialize our script
rm(list=ls()) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#List of packages we will need
listofpackages <- c("quantmod","PerformanceAnalytics","ellipse","reshape2","ggplot2", "rvest", "stringr",
                    "dygraphs", "dplyr","forecast", "aod","readr","rvest","lubridate", "xml2", "bbr", "caret")

for (j in listofpackages){
  if(sum(installed.packages()[, 1] == j) == 0) {
    install.packages(j)
  }
  library(j, character.only = T)
}

#Let's import the different datasets
players_1 = read.csv("Source Datasets/data_Bplayers_2000_TRAIN.csv")
players_2 = read.csv("Source Datasets/data_Bplayers_2000_TEST.csv")
contracts = read.csv("Source Datasets/spotrac_contracts_encoded.csv")

#We drop possible duplicates from all of them
players_1 = players_1 %>% distinct()
players_2 = players_2 %>% distinct()
contracts = contracts %>% distinct()

merger <- function(dataset){
  merged_data = merge(dataset, contracts[,c("player_name","season","contract_type","rookie_contract","bird_rights","maximum_contract","super_max_contract")], by.x = c("Player","season"), by.y = c("player_name","season"), all.x = TRUE)
  merged_data[c("rookie_contract","bird_rights","maximum_contract","super_max_contract")][is.na(merged_data[c("rookie_contract","bird_rights","maximum_contract","super_max_contract")])] <- 0
  merged_data[c("contract_type")][is.na(merged_data[c("contract_type")])] <- "Undefined"
  merged_data
}

players_1_fin = merger(players_1)  
players_2_fin = merger(players_2) 

write.csv(players_1_fin,'Final Datasets/Final_data_Bplayers_2000_TRAIN.csv', row.names=FALSE)
write.csv(players_2_fin,'Final Datasets/Final_data_Bplayers_2000_TEST.csv', row.names=FALSE)