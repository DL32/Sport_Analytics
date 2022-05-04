# 20630 - Introduction to Sports Analytics
# Group 2 Project 3 - Supervised Machine Learning Model
# RShiny App

# clear environment & set working directory
rm(list=ls()) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Package import
listofpackages <- c("quantmod","PerformanceAnalytics","ellipse","reshape2","ggplot2", "rvest",
                    "stringr", "dygraphs", "dplyr","forecast", "aod","readr","rvest","lubridate", 
                    "xml2", "caret", "gbm", "fastDummies", "pdp", "shiny", "priceR")

# Uploading libraries
for (j in listofpackages){
  if(sum(installed.packages()[, 1] == j) == 0) {
    install.packages(j)
  }
  library(j, character.only = T)
}

#Load Data (both train and test)
train_basket <- read.csv('../Dataset/Final Datasets/Final_data_Bplayers_2000_TRAIN.csv')
test_basket <- read.csv('../Dataset/Final Datasets/Final_data_Bplayers_2000_TEST.csv')

test_players = test_basket$Player
test_season = test_basket$season
test_link = test_basket$Image_Link
test_capofyear = test_basket$Salary_Cap
test_sal = test_basket$Salary
test_ctype = test_basket$contract_type

#Creating and applying a function for common pre-treatment of train and test
pre_treat <- function(dataset){
  
  #Create dummies for the role
  dataset$pos = lapply(dataset$pos, function(x) if (nchar(x)>2) {x=unlist(str_split(x, ",", simplify = TRUE)[1,1])} else {x})
  dataset$pos = unlist(dataset[,"pos"])
  dataset <- dummy_cols(dataset, select_columns = 'pos')
  
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

gbm_final <- readRDS(file = "gbm_final.Rds")
pred_basket_y = predict.gbm(gbm_final, test_basket_x)

test_basket$Salary_Cap_Perc_Pred = pred_basket_y
test_basket$Salary_int = test_sal
test_basket$Season = test_season
test_basket$Player = test_players
test_basket$Link = test_link
test_basket$Cap = test_capofyear
test_basket$Contract_Type = test_ctype

write.csv(test_basket, "Heroku_App/With_predictions.csv", row.names=FALSE)

### START OF APP ###

shinyApp(
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
  ),
  
  server <- function(input, output, session) {
    
    output$logo <- renderText({
      c('<img src="',"https://d2p3bygnnzw9w3.cloudfront.net/req/202203311/logos/bbr-logo.svg",'">')
    })
    
    output$title <- renderText({
      paste("Comparison between Real Salary and Predicted Salary")
    })
    
    observe({
      x <- input$variable1
      
      # Can also set the label and select items
      updateSelectInput(session, "variable2",
                        label = "Season:",
                        choices = c('', subset(test_basket, Player==x)$Season)
      )
    })
    
    observe({
      
      x1 <- input$variable1
      x2 <- input$variable2
      
      if (x2 == '') {formulaText0 <- reactive({''})
                     formulaText <- reactive({''})
                     formulaText1 <- reactive({''})}
      else {
        
        formulaText0 <- reactive({
          if (length(subset(test_basket, Player==x1 & Season==x2)$Salary_Cap_Perc_Pred)==0){
            paste("")
          } else {
            paste(x1, ", ", x2, sep = "")
          }
        })
        
        formulaText <- reactive({
          if (length(subset(test_basket, Player==x1 & Season==x2)$Salary_Cap_Perc_Pred)==0){
            paste("")
          } else {
          paste(paste("Predicted Salary Cap % is ", format(round(subset(test_basket, Player==x1 & Season==x2)$Salary_Cap_Perc_Pred*100, 3), nsmall = 3),"% ", sep = ""),paste("(", 
                format_dollars(subset(test_basket, Player==x1 & Season==x2)$Salary_Cap_Perc_Pred*subset(test_basket, Player==x1 & Season==x2)$Cap), ")", sep = ""))
          }
        })
        
        formulaText1 <- reactive({
          if (length(subset(test_basket, Player==x1 & Season==x2)$Salary_Cap_Perc)==0){
            paste("")
          } else {
            paste(paste("Real Salary Cap % is ",format(round(subset(test_basket, Player==x1 & Season==x2)$Salary_Cap_Perc*100, 3), nsmall = 3),"% ", sep = ""), paste("(", 
                  format_dollars(subset(test_basket, Player==x1 & Season==x2)$Salary_int), ")", sep = ""))
          }
        })
        
      }
        
      output$caption0 <- renderText({
        formulaText0()
      })
      
      output$caption <- renderText({
          formulaText()
      })
        
      output$caption1 <- renderText({
          formulaText1()
      })
      
      output$picture <- renderText({
        c('<img src="',unique(subset(test_basket, Player==x1 & Season==x2)$Link),'">')
      })
      
      if (x2 == '') {formulaText2 <- reactive({''})
                     formulaText3 <- reactive({''})
                     formulaText4 <- reactive({''})} else {
      
      formulaText2 <- reactive({
        if (length(subset(test_basket, Player==x1 & Season==x2)$Salary_Cap_Perc_Pred)==0){
          paste("")
        } else {
          paste("Per-Game statistics for", x1, "in season", x2)
        }
      })
      
      formulaText3 <- reactive({
        if (length(subset(test_basket, Player==x1 & Season==x2)$Salary_Cap_Perc_Pred)==0){
          paste("")
        } else {
          paste("Advanced statistics for", x1, "in season", x2)
        }
      })
      
      formulaText4 <- reactive({
        if (length(subset(test_basket, Player==x1 & Season==x2)$Salary_Cap_Perc_Pred)==0){
          paste("")
        } else {
          paste("Cap of the year:", format_dollars(subset(test_basket, Player==x1 & Season==x2)$Cap),". Contract type:", subset(test_basket, Player==x1 & Season==x2)$Contract_Type)
        }
      })
      
      }
      
      output$caption4 <- renderText({
        formulaText4()
      })
      
      output$caption2 <- renderText({
        formulaText2()
      })
      
      if (length(subset(test_basket, Player==x1 & Season==x2)$Salary_Cap_Perc_Pred)==0){
        output$table1 <- NULL
      } else {
        output$table1 <- renderDataTable(subset(test_basket, Player==x1 & Season==x2)[,c(3,25:48)], options = list(dom = 'ft', searching = FALSE, info = FALSE))
      }
      
      output$caption3 <- renderText({
        formulaText3()
      })
      
      if (length(subset(test_basket, Player==x1 & Season==x2)$Salary_Cap_Perc_Pred)==0){
        output$table2 <- NULL
      } else {
        output$table2 <- renderDataTable(subset(test_basket, Player==x1 & Season==x2)[,c(5:24)], options = list(dom = 'ft', searching = FALSE, info = FALSE))
      }
      
    })
    
    autoInvalidate <- reactiveTimer(10000)
    observe({
      autoInvalidate()
      cat(".")
    })
  }
)