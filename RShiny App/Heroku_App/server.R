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