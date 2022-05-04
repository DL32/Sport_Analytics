#clear the environment 
rm(list=ls()) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

listofpackages <- c("quantmod","PerformanceAnalytics","ellipse","reshape2","ggplot2", "rvest", "stringr",
                    "dygraphs", "dplyr","forecast", "aod","readr","rvest","lubridate", "xml2", "bbr", "caret")

#Uploading libraries
for (j in listofpackages){
  if(sum(installed.packages()[, 1] == j) == 0) {
    install.packages(j)
  }
  library(j, character.only = T)
}

###RETRIEVE SLUGS
diz = c("A","B","C","D","E","F","G","H","I","J","K","L","M",
        "N","O","P","Q","R","S","T","U","V","W","X","Y","Z")

#Build first dataframe
data_slug = data.frame()

#Remove basketball players that started before 2000
for (i in 1:(length(diz))){
  pla = get_players(diz[i])
  pla = subset(pla, from >= 2000)
  pla = pla[,c("player","slug")]
  data_slug <- rbind(data_slug, pla)
}

###SET OF CRUCIAL FUNCTIONS
maybe_as_numeric <- function(x) {
  # tries to make numeric columns numeric (from char)
  numeric_x <- suppressWarnings(as.numeric(x))
  if (!all(is.na(numeric_x))) x <- numeric_x
  x
}
empty_string_to_na <- function(x) {
  # sometimes (especially old datasets), empty values are ""
  if (class(x) == "character") {
    res <- ifelse(x == "", NA, x)
  } else {
    res <- x
  }
  res
}
clean_colnames <- function(df) {
  # clean up column names for a data frame
  stopifnot(is.data.frame(df))
  df <- df[!(names(df) == "Rk")] # remove "Rank" column
  names(df) <- gsub("\\.", "_pct", names(df))
  names(df) <- gsub("X2", "two_", names(df))
  names(df) <- gsub("X3", "three_", names(df))
  names(df) <- tolower(names(df))
  df
}
#Most important one to scrape players
get_player_data_pvt <- function(Nome_Giocatore, slug, tabella) {
  stopifnot(is.character(slug))
  initial <- substr(slug, 1, 1)
  url <- paste0("http://www.basketball-reference.com/players/",
                initial, "/", 
                slug, '.html')
  html <- xml2::read_html(url)
  
  nodes_nat <- rvest::html_nodes(html, "span")
  if (nchar(str_to_title(html_text(nodes_nat)[14])) == 2){
    nationality = str_to_title(html_text(nodes_nat)[14])
  }
  else {
    nationality = str_to_title(html_text(nodes_nat)[15])
  }
  
  node <- rvest::html_node(html, paste0("table#", tabella))
  table <- rvest::html_table(node, header = TRUE)
  table$slug <- slug
  
  converted <- lapply(table, empty_string_to_na)
  converted <- as.data.frame(converted, stringsAsFactors = FALSE)
  
  # ensure player name is the first column
  num_cols <- ncol(converted)
  reordered_df <- converted[, c(num_cols, 1:(num_cols - 1))]
  reordered_df <- clean_colnames(reordered_df)
  
  # strip out summary rows
  career_row <- which(reordered_df$season == 'Career')
  clean_df <- reordered_df[-c(career_row:nrow(reordered_df)), ]
  
  stats_sal <- url %>%
    read_html %>%
    html_nodes(xpath = '//comment()') %>%
    html_text() %>%
    paste(collapse='') %>%
    read_html() %>%
    html_node('table#all_salaries') %>%
    html_table()
  
  final_tab = merge(clean_df, stats_sal[,c("Season", "Salary")], by.x = "season", by.y = "Season")
  final_tab[,c("slug")] <- NULL
  final_tab$Player = Nome_Giocatore
  final_tab$Image_Link = html %>% html_nodes("[itemscope='image']") %>% html_attr("src") 
  final_tab$Nationality = nationality
  final_tab$US_Player = 0
  final_tab$US_Player[final_tab$Nationality %in% c('Us')] <- 1
  final_tab <- final_tab %>% relocate(Player, .before = season)
  final_tab
}
#Function to keep only TOT row (when a player played for more than a team in a season)
funz_TOT <- function(dataset,colonna1, colonna2, valore){
  new_dataset = data.frame()
  for (i in unique(dataset[,c(colonna1)])){
    if (dim(subset(aa, dataset[,c(colonna1)] == i))[1] == 1){
      new_dataset = rbind(new_dataset, subset(dataset, dataset[,c(colonna1)] == i))
    } else {
      new_dataset = rbind(new_dataset, subset(dataset, dataset[,c(colonna1)] == i & dataset[,c(colonna2)] == valore))
    }
  }
  new_dataset
}

###START SCRAPING

#Useful for range(len(...))
ran = dim(data_slug)[1]

#Create dataframe
data_giocat_totale = data.frame()

#Start the scrape
for (i in 1:ran){
  
  print(i)
  
  tryCatch({
    
    #Take per-game statistics and advanced statistics for each player
    aa = get_player_data_pvt(data_slug[i,c("player")],data_slug[i,c("slug")],"advanced")
    bb = get_player_data_pvt(data_slug[i,c("player")],data_slug[i,c("slug")],"per_game")
    
    #Take only TOT values for season with more than a club  
    aa_1 = funz_TOT(aa, "season", "tm", "TOT")
    bb_1 = funz_TOT(bb, "season", "tm", "TOT")
    #Take the largest salary in season with more than one club
    aa_1 <- merge(aa_1, aggregate(data = aa_1, Salary~season, FUN = max), by=c("season","Salary"))
    bb_1 <- merge(bb_1, aggregate(data = bb_1, Salary~season, FUN = max), by=c("season","Salary"))
    
    #Remove total points per season, you already have the per-game one
    aa_1$mp <- NULL
    
    #Remove problematic columns from advanced statistics dataset
    col_n = colnames(aa_1)
    col_n = col_n[sapply(col_n , nchar) > 30]
    for (i in 1:(length(col_n))){
      aa_1[,c(col_n[i])] <- NULL
    }
    
    #Merge the two dataset of per_game and advanced statistics
    total1 <- merge(aa_1, bb_1,by=c("Player","Nationality","Image_Link","US_Player","season","age","tm","lg","pos","g","Salary"))
    data_giocat_totale <- rbind(data_giocat_totale, total1)
    
  }, error = function(e){})
  
}

#Let's import cap history
cap_history = read.csv("Cap_History.csv", sep=";")

#I insert the Cap value for that season in our main dataset
new = merge(data_giocat_totale, cap_history, by.x = "season", by.y = "Season")

#Rename rows
rownames(new) = 1:nrow(new)

#I transform in integer the Salary columns (e.g. from "$ 10,457,100" to 10457100)
new[c("Salary_Cap","Salary")] <- lapply(new[c("Salary_Cap","Salary")], function(x) as.integer(gsub('[$,]', '', x)))

#Calculate minimum for every year
mindata = aggregate(data = new, Salary~season, FUN = min)

#I substitute the "$ <Minimum" with the minimum for that specific season
for (i in 1:dim(new)[1]){
  if (is.na(new[i, "Salary"]) == TRUE) {
    new[i, "Salary"] = subset(mindata, season == new[i, "season"])['Salary']
  }
}

#I create the percentage column
new['Salary_Cap_Perc'] = new["Salary"] / new["Salary_Cap"]

#Rearrange the dataset
new <- new %>% relocate(Salary_Cap_Perc,
                        Salary_Cap,
                        .before = Salary)

#Split in TRAIN-TEST (using 80%-20% ratio)
dt = sort(sample(nrow(new), nrow(new)*.8))
Basket_Player_Train <- new[dt,]
Basket_Player_Test <- new[-dt,]

Basket_Player_Test$Nationality <- NULL
Basket_Player_Train$Nationality <- NULL

Basket_Player_Train <- subset(Basket_Player_Train, season != "2021-22")

#Saving our final CSVs
write.csv(Basket_Player_Train,'data_Bplayers_2000_TRAIN.csv', row.names=FALSE)
write.csv(Basket_Player_Test,'data_Bplayers_2000_TEST.csv', row.names=FALSE)