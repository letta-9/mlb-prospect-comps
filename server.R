library(shiny)
library(readr)
library(dplyr)
library(DT)
library(shinyBS)
library(shinyjs)

position_prospects <- read_csv("position_prospects.csv")
pitcher_prospects <- read_csv("pitcher_prospects.csv")
display_prospects <- read_csv("display_prospects.csv")

shinyServer(function(input, output){
  
  output$display_prospects <- renderDT(
    if (input$teamSort == "Top 100") {
      display_prospects
    } else {
      display <- subset(display_prospects, display_prospects$Org == input$teamSort)
    },
    options = list(pageLength = 100),
    rownames = FALSE,
    selection = "single",
  )
  
  output$test <- renderPrint({
    s = input$display_prospects_rows_selected
    display_prospects$Name[[s]]
      
  })
  
  observeEvent(input$display_prospects_rows_selected, {
    showModal(modalDialog(
      title = "Somewhat important message",
      "PLAYER COMP HERE",
      easyClose = TRUE,
      footer = NULL
    ))
  
  
  }
  
)})