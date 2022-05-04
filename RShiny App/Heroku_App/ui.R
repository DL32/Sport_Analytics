# 20630 - Introduction to Sports Analytics
# Group 2 Project 3 - Supervised Machine Learning Model
# RShiny App

# clear environment & set working directory
rm(list=ls())

# Package import
listofpackages <- c("shiny","priceR",'rstudioapi')

# Uploading libraries
for (j in listofpackages){
  if(sum(installed.packages()[, 1] == j) == 0) {
    install.packages(j)
  }
  library(j, character.only = T)
}

#Load Data (both train and test)
urlfile_test<-'https://raw.githubusercontent.com/jvschlierf/Sportylytics/main/RShiny%20App/Heroku_App/With_predictions.csv'
test_basket <- read.csv(urlfile_test, encoding = "latin1", stringsAsFactors = FALSE)

ui <- fluidPage(
  
  headerPanel("Predicted Salary for Player"),
  
  sidebarPanel(
    selectInput("variable1", "Player:", choices = c('', test_basket$Player)),
    selectInput("variable2", "Season:", NULL)
  ),
  
  mainPanel(
    
    htmlOutput("logo"),
    h1(textOutput("title")),
    div(style="display: inline-block;vertical-align:right; width: 200px;", htmlOutput("picture")),
    div(style="display: inline-block;vertical-align:right; width: 300px;", h2(textOutput("caption0"))),
    tags$head(tags$style("#caption0{color: red; font-size: 40px; font-style: italic;}")),
    h4(textOutput("caption4")),
    h3(textOutput("caption")),
    h3(textOutput("caption1")),
    h5(textOutput("caption2")),
    dataTableOutput("table1"),
    tags$style(type="text/css", '#table1 tfoot {display:none;}'),
    h5(textOutput("caption3")),
    dataTableOutput("table2"),
    tags$style(type="text/css", '#table2 tfoot {display:none;}')
    
  )
)