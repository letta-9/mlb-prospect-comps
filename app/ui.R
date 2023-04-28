library(shiny)
library(readr)
library(dplyr)
library(DT)
library(plyr)
library(shinyBS)
library(shinydashboard)

#Import CSV files

#display_prospects <- read_csv('pros_display.csv')
#headshots <- read_csv("mlb_headshots.csv")

# #Clean prospects file
# 
# prospects$Age <- floor(prospects$Age)
# prospects$W <- plyr::round_any(prospects$W, 5, f=ceiling)
# prospects$H <- gsub(" ","", prospects$H)
# prospects$Class <- ifelse(prospects$Pos == 'SP' | prospects$Pos == 'MIRP'| prospects$Pos == 'SIRP', 'Pit', 'Pos')
# display_prospects <- prospects %>% select(Top100, Name, Pos, Org, Age, H, W, B, T)

shinyUI(

  fluidPage(
    
    titlePanel("MLB PROSPECT COMPS"),
              actionButton("compare", "Compare"),
              DTOutput("display_prospects"),
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
              )
          )
      )
  
            
        
