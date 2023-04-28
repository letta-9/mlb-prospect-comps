library(shiny)
library(readr)
library(dplyr)
library(DT)
library(plyr)
library(shinyBS)
library(shinydashboard)


shinyUI(

  fluidPage(
    
    titlePanel("MLB PROSPECT COMPS"),
              column(12,
                     br(),
                     h4('Select a Prospect from Table Below and Click to Produce an MLB Comparison'),
                     actionButton("compare", "Submit", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                     br(),
                     br(),
                     br(),
                     DTOutput("display_prospects")
              ),
              

              bsModal("modal", "Big League Comps", trigger = 'compare', size = "large",
                      fluidRow(
                        column(4),
                        column(3, htmlOutput("pros_mug")),
                        column(3, htmlOutput("mlb_mug")),
                        column(2)
                      ),
                      br(),
                      fluidRow(
                        column(3),
                        column(3, verbatimTextOutput('prospect')),
                        column(3, verbatimTextOutput('comp')),
                        column(3)
                      ),
                      br(),
                      fluidRow(
                        column(4),
                        column(4,tableOutput("tbl")),
                        column(4)
                      ),
                      br(),
                      fluidRow(
                        column(5),
                        column(4,uiOutput("card")),
                        column(3)
                      ),
                      br(),
                      fluidRow(
                        column(5),
                        column(4,uiOutput("highlight")),
                        column(3)
                      )
              ),
              h6("*Based on Frangraph's 2023 Preseason Top 100*")
          )
      )
  
            
        
