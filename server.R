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

prospects <- read_csv('prospects_clean.csv') #From fangraphs the board. Cleaned in prospects.R
mlb_bios <- read_csv('mlb_bios_clean.csv') #From baseballr. Cleaned in batters.R
batters <- read_csv('batters_clean.csv') #From baseballr. Cleaned in batters.R
pitchers <- read_csv('pitchers_clean.csv') #Raw CSV exported from baseball savant pitch arsenal page. baseballr for pitch movement. Cleaned in pitchers.R
headshots <- read_csv("headshots.csv")

joe_random <- "https://img.mlbstatic.com/mlb-photos/image/upload/d_people:generic:headshot:67:current.png/w_213,q_auto:best/v1/people/150397/headshot/67/current" 

############################################
# CREATE DATA TABLE FOR FRONT PAGE DISPLAY #
############################################

display_prospects <- prospects %>% select(Top100, Name, Pos, Org, Age, Ht, Wt, B, T)
display_prospects <- display_prospects %>% arrange(Top100)

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
    x = x$Name
  })


  selected_class = reactive({
    y = prospects %>% filter(Name == selected_player())
    y = y$Class
  })
  
  url = reactive({
    src = headshots['url'][headshots['name'] == selected_player()]
  })
  
  
  modal_display = reactive({
    modal <- data.frame(matrix(ncol=13, nrow=1))
    colnames(modal) <- c('Name', 'Body', 'Hit', 'Game_Power', 'Raw_Power', 'Spd', 'Fld', 'Arm', 'FB', 'SL', 'CB', 'CH','CMD')
    
    selected_data <- prospects %>% filter(Name == selected_player())
    print(selected_player())
    print(selected_data)

    
    if (selected_class() == 'Position'){
      modal$Name <- selected_player()
      modal$Body <- dplyr::select(sample_n(merge(selected_data, batters, by=c('Bio.Class','H','W')),1), Name.y)      
      modal$Hit <- dplyr::select(sample_n(merge(selected_data, batters, by=c('Hit','B')),1), Name.y)
      modal$Game_Power <- dplyr::select(sample_n(merge(selected_data, batters, by=c('Game','B')),1), Name.y)
      modal$Raw_Power <- dplyr::select(sample_n(merge(selected_data, batters, by=c('Raw')),1), Name.y)
      modal$Spd <- dplyr::select(sample_n(merge(selected_data, batters, by=('Spd')),1), Name.y)
      modal$Fld <- dplyr::select(sample_n(merge(selected_data, batters, by=c('Pos','Fld')),1), Name.y)
    } 
    
    if (selected_class() == 'Catcher'){
      modal$Name <- selected_player()
      modal$Body <- dplyr::select(sample_n(merge(selected_data, batters, by=c('Bio.Class','H','W')),1), Name.y)
      modal$Hit <- dplyr::select(sample_n(merge(selected_data, batters, by=c('Hit','B')),1), Name.y)
      modal$Game_Power <- dplyr::select(sample_n(merge(selected_data, batters, by=c('Game','B')),1), Name.y)
      modal$Raw_Power <- dplyr::select(sample_n(merge(selected_data, batters, by=c('Raw','B')),1), Name.y)
      modal$Spd <- dplyr::select(sample_n(merge(selected_data, batters, by=('Spd')),1), Name.y)
      modal$Arm <- dplyr::select(sample_n(merge(selected_data, batters, by=c('Pos','Arm')),1), Name.y)
    } 
    
    if (selected_class() == 'Pitcher'){
      modal$Name <- selected_player()
      modal$Body <- dplyr::select(sample_n(merge(selected_data, mlb_bios, by=c('Bio.Class','H','W')),1), Name.y)
      modal$FB <- dplyr::select(sample_n(merge(selected_data, pitchers, by=c('T','FB')),1), Name.y)
      modal$SL <- dplyr::select(sample_n(merge(selected_data, pitchers, by=c('T','SL')),1), Name.y)
      modal$CB <- dplyr::select(sample_n(merge(selected_data, pitchers, by=c('T','CB')),1), Name.y)
      modal$CH <- dplyr::select(sample_n(merge(selected_data, pitchers, by=c('T','CH')),1), Name.y) 
    } 
    
    modal <- t(modal)
    modal <- na.omit(modal)
    modal <- modal[-1,]
    #names(modal) <- 'Grade Comp'
    

  })
  
  mlb_comp = reactive({
    selected_data <- prospects %>% filter(Name == selected_player())
    
    if (selected_data$Class == 'Pitcher'){
      
      pit_ovl <- pitchers %>% filter(pitchers$T == selected_data$T)
      
      selected_data <- selected_data[c(1,17,18,19,20,7,8)]
      selected_vec <- as.numeric(selected_data[,-1])
      
      pit_ovl <- pit_ovl[c(1,2,3,4,5,8,9)]
      pit_mat <- data.matrix(pit_ovl[,-1])
      
      ovl <- abs(sweep(pit_mat, 2, selected_vec))
      ovl <- cbind(ovl, rowSums(ovl))
      ovl <- cbind(pit_ovl[,1], ovl)
      colnames(ovl)[6] <- 'CV'
      ovl <- data.frame(ovl)
      ovl <- ovl %>% arrange(CV)
      ovl <- ovl[1,]
      comp <- ovl[1,1]
      print(comp)
    } else {
      bat_ovl <- batters %>% filter(batters$Pos == selected_data$Pos,
                                    batters$B == selected_data$B,
                                    batters$T == selected_data$T)
      bat_ovl[is.na(bat_ovl)] <- 0
      
      selected_data <- selected_data[c(1,11,12,13,14,15,16,7,8)]
      selected_vec <- as.numeric(selected_data[,-1])
      
      bat_ovl <- bat_ovl[c(1,4,5,6,7,8,9,12,13)]
      bat_mat <- data.matrix(bat_ovl[,-1])
      
      ovl <- abs(sweep(bat_mat, 2, selected_vec))
      ovl <- cbind(ovl, rowSums(ovl))
      ovl <- cbind(bat_ovl[,1], ovl)
      colnames(ovl)[10] <- 'CV'
      ovl <- ovl %>% arrange(CV)
      ovl <- ovl[1,]
      comp <- ovl[1,1]
      print(comp) 
    }
  })



  output$pros_mug <- renderText({c('<img src="',url(),'" height="80" width="80" align ="center">')})
  
  output$mlb_mug <- renderText({c('<img src="',joe_random,'" height="80" width="60" align ="center">')})

  output$prospect <- renderText(selected_player())
  
  output$comp <- renderText(mlb_comp())  


  output$tbl <- renderTable(
    modal_display(),
    rownames = TRUE,
  )
  
  output$card <- renderUI({
    name_url <-  gsub(" ","+", selected_player())
    url <- sprintf("https://www.ebay.com/sch/i.html?_from=R40&_nkw=%s+1st+bowman+chrome+auto+psa+10&_sacat=0&LH_Auction=1&_sop=1", name_url)
    tagList(a("ROOKIE CARD", href=url, target="_blank"))
  })
  
  output$highlight <- renderUI({
    name_url <-  gsub(" ","+", selected_player())
    url <- sprintf("https://www.youtube.com/results?search_query=%s+highlights+", name_url)
    tagList(a("HIGHLIGHTS", href=url, target="_blank"))
  })
  

  
  
})