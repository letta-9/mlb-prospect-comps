library(rvest)
library(RSelenium)
library(dplyr)
library(glue)
library(readr)
library(plyr)
library(tidyr)
library(stringi)
library(data.table)

####################
# IMPORT CSV FILES #
####################

prospects <- read_csv('prospects_clean.csv') #From fangraphs the board / mlb.com. Cleaned in prospects.R
mlb_bios <- read_csv('mlb_bios_clean.csv') #From baseballr. Cleaned in batters.R
batters <- read_csv('batters_clean.csv') #From baseballr. Cleaned in batters.R
pitchers <- read_csv('pitchers_clean.csv') #Raw CSV exported from baseball savant pitch arsenal page. baseballr for pitch movement. Cleaned in pitchers.R


############################################
# CREATE DATA TABLE FOR FRONT PAGE DISPLAY #
############################################

display_prospects <- prospects %>% select(Top100, Name, Pos, Org, Age, H, W, B, T)

#################
# SHINY SERVER #
################

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
  
  
  modal_display = reactive({
    modal <- data.frame(matrix(ncol=13, nrow=1))
    colnames(modal) <- c('Name', 'Body', 'Hit', 'Game', 'Raw', 'Spd', 'Fld', 'Arm', 'FB', 'SL', 'CB', 'CH','CMD')

    selected_data <- prospects %>% filter(Name == selected_player())

    modal$Name <- selected

    if (selected_data$Class == 'Position'){
      modal$Hit <- dplyr::select(sample_n(merge(selected_data, batters, by=c('Hit','B')),1), Name.y)
      modal$Game <- dplyr::select(sample_n(merge(selected_data, batters, by=c('Game','B')),1), Name.y)
      modal$Raw <- dplyr::select(sample_n(merge(selected_data, batters, by=c('Raw','B')),1), Name.y)
      modal$Spd <- dplyr::select(sample_n(merge(selected_data, batters, by=('Spd')),1), Name.y)
      modal$Fld <- dplyr::select(sample_n(merge(selected_data, batters, by=c('Pos','Fld')),1), Name.y)
    } 

    if (selected_data$Class == 'Catcher'){
      modal$Hit <- dplyr::select(sample_n(merge(selected_data, batters, by=c('Hit','B')),1), Name.y)
      modal$Game <- dplyr::select(sample_n(merge(selected_data, batters, by=c('Game','B')),1), Name.y)
      modal$Raw <- dplyr::select(sample_n(merge(selected_data, batters, by=c('Raw','B')),1), Name.y)
      modal$Spd <- dplyr::select(sample_n(merge(selected_data, batters, by=('Spd')),1), Name.y)
      modal$Arm <- dplyr::select(sample_n(merge(selected_data, batters, by=c('Pos','Arm')),1), Name.y)
    } 

    if (selected_data$Class == 'Pitcher'){
      modal$FB <- dplyr::select(sample_n(merge(selected_data, pitchers, by=c('T','FB')),1), Name.y)
      modal$SL <- dplyr::select(sample_n(merge(selected_data, pitchers, by=c('T','SL')),1), Name.y)
      modal$CB <- dplyr::select(sample_n(merge(selected_data, pitchers, by=c('T','CB')),1), Name.y)
      modal$CH <- dplyr::select(sample_n(merge(selected_data, pitchers, by=c('T','CH')),1), Name.y) 
    } 

    modal <- t(modal)
    modal <- na.omit(modal)
  })

  
  output$tbl <- renderDataTable(
    modal_display()
  )

  output$card <- renderUI({
    name_url <-  gsub(" ","+", selected_player())
    url <- sprintf("https://www.ebay.com/sch/i.html?_from=R40&_trksid=p2380057.m570.l1313&_nkw=%s+1st+bowman+chrome+auto+psa+10&_sacat=0", name_url)
    tagList(a("1st Bowman Rookie Card", href=url))
  })

  })
  
