library(shiny)
library(readr)
library(dplyr)
library(DT)

position_prospects <- read_csv("position_prospects.csv")
pitcher_prospects <- read_csv("pitcher_prospects.csv")
display_prospects <- read_csv("display_prospects.csv")

teams <- arrange(display_prospects, Org)
teams <- unique(teams$Org)
teams <- c("Top 100", teams)

shinyUI(fluidPage(
  titlePanel("MLB PROSPECT COMPS"),
  
  mainPanel((""),
            selectInput("teamSort", "Sort by Team", choices = teams, selected = "Top 100"),
            DTOutput("display_prospects")),
  )
)