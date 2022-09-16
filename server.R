library(rvest)
library(RSelenium)
library(dplyr)
library(glue)
library(readr)
library(plyr)
library(tidyr)
library(stringi)
library(data.table)

# IMPORT CSV FILES #

prospects <- read_csv("prospect_master_090522.csv")
mlb_bios <- read_csv("mlb_bios.csv")
batters <- read_csv("mlb_batting.csv")
fielders <- read_csv("mlb_fielding.csv")

# CLEAN PROSPECTS DATA #

prospects$Age <- floor(prospects$Age)
prospects$W <- plyr::round_any(prospects$W, 5, f=ceiling)
prospects$H <- gsub(" ","", prospects$H)
prospects$Class <- ifelse(prospects$Pos == 'SP' | prospects$Pos == 'MIRP'| prospects$Pos == 'SIRP', 'Pit', 'Pos')

prospects <- prospects %>% separate(Hit, c("cHit","Hit"), "/")
prospects$Hit <- gsub(" ","", prospects$Hit)
prospects <- prospects %>% separate(Game, c("cGame","Game"), "/")
prospects$Game <- gsub(" ","", prospects$Game)
prospects <- prospects %>% separate(Raw, c("cRaw","Raw"), "/")
prospects$Raw <- gsub(" ","", prospects$Raw)
prospects <- prospects %>% separate(Spd, c("cSpd","Spd"), "/")
prospects$Spd <- gsub(" ","", prospects$Spd)
prospects <- prospects %>% separate(Fld, c("cFld","Fld"), "/")
prospects$Fld <- gsub(" ","", prospects$Fld)

# CLEAN MLB BIOS DATA #

mlb_bios$W <- plyr::round_any(mlb_bios$W, 5, f=ceiling)
mlb_bios$Class <- ifelse(mlb_bios$Pos == 'SP' | mlb_bios$Pos == 'RP', 'Pit', 'Pos')


# CREATE DATA TABLE FOR FRONT PAGE DISPLAY #

display_prospects <- prospects %>% select(Top100, Name, Pos, Org, Age, H, W, B, T)


# CLEAN MLB BATTING # MIN 200 PA for 3/4 LAST SEASONS

names(batters) <- c('Last','First','ID','Year','HR','AVG','G','EV','SPD')
batters$Name <- paste(batters$First, batters$Last)

batters <- batters %>% group_by(Name) %>% filter(n()>2)
batters$HR <- ((batters$HR / batters$G) * 162)
batters <- batters %>% select(Name, G, AVG, HR, EV, SPD)

games <- batters[c(1,2)]
games <- aggregate(games$G, by=list(Name=games$Name), FUN=sum)

batters <- aggregate(batters, list(Name = batters$Name), mean)
batters <- batters[,-2]



# BODY TYPE #

pros_body_comp <- prospects %>% select(Name, Class, H, W)
mlb_body_comp <- mlb_bios %>% select(Name, Class, H, W)
body_comp <- merge(pros_body_comp, mlb_body_comp, by = c('Class','H','W'), all=TRUE)
body_comp <- subset(body_comp, !is.na(Name.x))
body_comp[is.na(body_comp)] <- 'No Simliar Player'


# CLEAN FIELDING STATS #

fielders <- fielders %>% select(last_name, first_name, primary_pos_formatted, outs_above_average)
fielders$Name <- paste(fielders$first_name, fielders$last_name)
fielders <- fielders[c(3,4,5)]
fielders <- fielders %>% relocate(Name, .before = primary_pos_formatted)
names(fielders) = c("Name","Pos","OAA")
fielders$Pos <- gsub(" ","", fielders$Pos)
fielders$OAA <- gsub(" ","", fielders$OAA)
fielders$Name <- iconv(fielders$Name, from = "UTF-8", to = "ASCII//TRANSLIT")



fielders <- merge(fielders, games, by = "Name")
fielders$OAA <- as.numeric(fielders$OAA)
fielders$OAA162 <- (fielders$OAA / fielders$x) * 162


for (i in 2:5){
  n <- c('Hit', 'Game', 'Raw', 'Speed')
  
  
  tool_std <- sd(batters[,i])
  tool_mean <- mean(batters[,i])
  
  tool_breaks = c((tool_mean - (2*tool_std)),(tool_mean - tool_std), tool_mean, (tool_mean + tool_std),(tool_mean + (2*tool_std)))
  
  g <- findInterval(batters[,i], tool_breaks)
  g <- factor(g)
  levels(g) <- c("30","40","50","60","70","80")
  
  batters <- cbind(batters, g)
  
}

names(batters) <- c("Name", "G", "AVG","HR","EV","SPD","Hit","Game","Raw","Spd")


positions <- c("1B", "2B", "SS", "3B", "LF", "CF", "RF")
fielders_blank <- data.frame(matrix(ncol=6, nrow=0))

for (j in positions) {
  x <- fielders %>% filter(Pos == j)
  fld_mean <- mean(x$OAA162)
  fld_std <- sd(x$OAA162)
  fld_breaks = c((fld_mean - (2*fld_std)),(fld_mean - fld_std), fld_mean, (fld_mean + fld_std),(fld_mean + (2*fld_std)))
  
  Fld <- findInterval(x$OAA162, fld_breaks)
  Fld <- factor(Fld)
  levels(Fld) <- c("30","40","50","60","70","80")
  
  x <- cbind(x, Fld)
  
  fielders_blank <- rbindlist(list(fielders_blank,x), fill = TRUE)
  
}

fielders <- fielders_blank[,c(7,8,12)]
batters <- merge(batters, fielders, by="Name")
names(batters) <- c("Name","G","AVG","HR","EV","SPD","Hit","Game","Raw","Spd","Pos","Fld")
batters <- batters %>% relocate(Pos, .before = G)




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
  