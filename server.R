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


prospects <- read_csv("prospect_master_090522.csv") #From fangraphs the board / mlb.com
mlb_bios <- read_csv("mlb_bios.csv") #From data.R
batters <- read_csv("mlb_batters.csv") #From data.R
pros_cat <- read.csv('prospect_catchers.csv')



########################
# CLEAN PROSPECTS DATA #
########################


prospects$Age <- floor(prospects$Age)

prospects$W <- plyr::round_any(prospects$W, 5, f=ceiling)
prospects$Class <- ifelse(prospects$Pos == 'SP' | prospects$Pos == 'MIRP'| prospects$Pos == 'SIRP', 'Pitcher', 'Position')

tool_list <- c('Hit','Game','Raw','Spd','Fld','FB','SL','CB','CH','CMD')
for (a in tool_list){
  prospects <- prospects %>% separate(a, c(paste('c',a),a), " / ")
}


prospects$Hit <-as.numeric(prospects$Hit)
prospects$Hit <- plyr::round_any(prospects$Hit, 10, f = ceiling)
prospects$Game <-as.numeric(prospects$Game)
prospects$Game <- plyr::round_any(prospects$Game, 10, f = ceiling)
prospects$Raw <-as.numeric(prospects$Raw)
prospects$Raw <- plyr::round_any(prospects$Raw, 10, f = ceiling)
prospects$Spd <-as.numeric(prospects$Spd)
prospects$Spd <- plyr::round_any(prospects$Spd, 10, f = ceiling)
prospects$Fld <-as.numeric(prospects$Fld)
prospects$Fld <- plyr::round_any(prospects$Fld, 10, f = ceiling)

pros_cat <- pros_cat %>% filter(Pos == 'C') %>% select(Name, Pos, Arm)

############################################
# CREATE DATA TABLE FOR FRONT PAGE DISPLAY #
############################################

display_prospects <- prospects %>% select(Top100, Name, Pos, Org, Age, H, W, B, T)

#########################
# CREATE COMP VARIABLES #
#########################

pros_body_comp <- prospects %>% select(Name, Class, H, W)
mlb_body_comp <- mlb_bios %>% select(Name, Class, H, W)
body_comp <- merge(pros_body_comp, mlb_body_comp, by = c('Class','H','W'), all=TRUE)
body_comp <- subset(body_comp, !is.na(Name.x))
body_comp[is.na(body_comp)] <- 'No Simliar Player'


pros_hit_comp <- prospects %>% select(Name, Hit)
mlb_hit_comp <- batters %>% select(Name, Hit)
hit_comp <- merge(pros_hit_comp, mlb_hit_comp, by = 'Hit', all=TRUE)

pros_game_comp <- prospects %>% select(Name, Game)
mlb_game_comp <- batters %>% select(Name, Game)
game_comp <- merge(pros_game_comp, mlb_game_comp, by = 'Game', all=TRUE)
 
pros_raw_comp <- prospects %>% select(Name, Raw)
mlb_raw_comp <- batters %>% select(Name, Raw)
raw_comp <- merge(pros_raw_comp, mlb_raw_comp, by = 'Raw', all=TRUE)
 
pros_spd_comp <- prospects %>% select(Name, Spd)
mlb_spd_comp <- batters %>% select(Name, Spd)
spd_comp <- merge(pros_spd_comp, mlb_spd_comp, by = 'Spd', all=TRUE)

pros_fld_comp <- prospects %>% select(Name, Pos, Fld)
mlb_fld_comp <- batters %>% select(Name, Pos, Fld)
fld_comp <- merge(pros_fld_comp, mlb_fld_comp, by = c('Pos','Fld'), all=TRUE)




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
  
  selected_body = reactive({
    body_comp %>% filter(Name.x == selected_player())
  })
  
  selected_hit = reactive({
    hit_comp %>% filter(Name.x == selected_player())
  })
  
  selected_game = reactive({
    game_comp %>% filter(Name.x == selected_player())
  })
  
  selected_raw = reactive({
    raw_comp %>% filter(Name.x == selected_player())
  })
  
  selected_spd = reactive({
    spd_comp %>% filter(Name.x == selected_player())
  })
  
  selected_fld = reactive({
    fld_comp %>% filter(Name.x == selected_player())
  })
  

  modal_display = reactive({
    body = selected_body()[1,]
    body$Cat <- "Body Type"    
    body = body %>% select(Cat,Name.x,Name.y)
    
    hit = selected_hit()[1,]
    hit$Cat <- "Hit"    
    hit = hit %>% select(Cat,Name.x,Name.y)

    game = selected_game()[1,]
    game$Cat <- "Game Power"    
    game = game %>% select(Cat,Name.x,Name.y)
    
    raw = selected_raw()[1,]
    raw$Cat <- "Raw Power"    
    raw = raw %>% select(Cat,Name.x,Name.y)    
    
    spd = selected_spd()[1,]
    spd$Cat <- "Speed"    
    spd = spd %>% select(Cat,Name.x,Name.y)
    
    fld = selected_fld()[1,]
    fld$Cat <- "Fielding"
    fld = fld %>% select(Cat,Name.x,Name.y)
    
    display <- rbind(body, hit, game, raw, spd, fld)
    
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
  