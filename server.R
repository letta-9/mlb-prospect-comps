library(dplyr)
library(DT)
library(plyr)
library(shinyBS)

#Import CSV files

prospects <- read_csv("prospect_master_090522.csv")
mlb_bios <- read_csv("mlb_bios.csv")

#Clean prospects file

prospects$Age <- floor(prospects$Age)
prospects$W <- plyr::round_any(prospects$W, 5, f=ceiling)
prospects$H <- gsub(" ","", prospects$H)
prospects$Class <- ifelse(prospects$Pos == 'SP' | prospects$Pos == 'MIRP'| prospects$Pos == 'SIRP', 'Pit', 'Pos')

#Clean MLB bios file

mlb_bios$W <- plyr::round_any(mlb_bios$W, 5, f=ceiling)
mlb_bios$Class <- ifelse(mlb_bios$Pos == 'SP' | mlb_bios$Pos == 'RP', 'Pit', 'Pos')

#Create df for front page display

display_prospects <- prospects %>% select(Top100, Name, Pos, Org, Age, H, W, B, T)

#Create df for body type comp

pros_body_comp <- prospects %>% select(Name, Class, H, W)
mlb_body_comp <- mlb_bios %>% select(Name, Class, H, W)
body_comp <- merge(pros_body_comp, mlb_body_comp, by = c('Class','H','W'), all=TRUE)
body_comp <- subset(body_comp, !is.na(Name.x))
body_comp[is.na(body_comp)] <- 'No Simliar Player'


library(shiny)
library(readr)


shinyServer(function(input, output){
  
  
  output$display_prospects <- renderDT(
    display_prospects,
    options = list(pageLength = 1500),
    rownames = FALSE,
    selection = "single",
  )
  

  selected_player = reactive({
    x = isolate(display_prospects)
    x = x[input$display_prospects_rows_selected,]
    x$Name
  })
  
  selected_data = reactive({
    body_comp %>% filter(Name.x == selected_player())
  })
  
  modal_display = reactive({
    body = selected_data()[1,]
    body$Cat <- "Body Type"    
    body = body %>% select(Cat,Name.x,Name.y)

  })
  

  output$tbl <- renderDT(
    modal_display()
  )


  
  })
  