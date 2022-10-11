library(shiny)
library(readr)
library(dplyr)
library(DT)
library(plyr)
library(shinyBS)
library(shinydashboard)

#Import CSV files

prospects <- read_csv("prospects_clean.csv")

#Clean prospects file

prospects$Age <- floor(prospects$Age)
prospects$W <- plyr::round_any(prospects$W, 5, f=ceiling)
prospects$H <- gsub(" ","", prospects$H)
prospects$Class <- ifelse(prospects$Pos == 'SP' | prospects$Pos == 'MIRP'| prospects$Pos == 'SIRP', 'Pit', 'Pos')
display_prospects <- prospects %>% select(Top100, Name, Pos, Org, Age, H, W, B, T)

shinyUI(

  fluidPage(
    
    titlePanel("MLB PROSPECT COMPS"),
              actionButton("compare", "Compare"),
              DTOutput("display_prospects"),
              bsModal("modal", "Big League Comps", "compare", size = "large",
                      dataTableOutput("tbl"),
                      br(),
                      uiOutput("card"),
                      br(),
                      uiOutput("highlight"))

        )

)
  
            
        
