library(dplyr)
library(DT)
library(plyr)
library(shinyBS)
library(tidyr)

#Import csv files

prospects <- read_csv("prospect_master_090522.csv")
mlb_bios <- read_csv("mlb_bios.csv")
batters <- read_csv("batting_grades.csv")

#Clean prospects file

prospects$Age <- floor(prospects$Age)
prospects$W <- plyr::round_any(prospects$W, 5, f=ceiling)
prospects$H <- gsub(" ","", prospects$H)
prospects$Class <- ifelse(prospects$Pos == 'SP' | prospects$Pos == 'MIRP'| prospects$Pos == 'SIRP', 'Pit', 'Pos')
prospects <- prospects %>% separate(Game, c("cGame","Game"), "/")
prospects$Game <- gsub(" ","", prospects$Game)

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



batters <- batters %>% group_by(Name) %>% filter(n()==3)
batters$HR162 <- round(((batters$HR / batters$G) * 162), 0)
game <- aggregate(batters$HR162, list(Name = batters$Name), mean)
names(game)[2] <- 'HR'
game$HR <- round(game$HR, 0)
game_std <- round(sd(game$HR), 0)
game_mean <- round(mean(game$HR), 0)

breaks = c((game_mean - (2*game_std)),(game_mean - game_std), game_mean, (game_mean + game_std),(game_mean + (2*game_std)))

game$Game <- findInterval(game$HR, breaks)
game$Game <- factor(game$Game)
levels(game$Game) <- c("30","40","50","60","70","80")

pros_game_comp <- prospects %>% select(Name, Game)
mlb_game_comp <- game %>% select(Name, Game)
game_comp <- merge(pros_game_comp, mlb_game_comp, by = 'Game', all=TRUE)






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
  
  selected_game = reactive({
    game_comp %>% filter(Name.x == selected_player())
  })
  
  modal_display = reactive({
    body = selected_body()[1,]
    body$Cat <- "Body Type"    
    body = body %>% select(Cat,Name.x,Name.y)

    game = selected_game()[1,]
    game$Cat <- "Game Power"    
    game = game %>% select(Cat,Name.x,Name.y)
    
    display <- rbind(body, game)
    

  })
  
  
  output$tbl <- renderDT(
    modal_display()
  )

  })
  