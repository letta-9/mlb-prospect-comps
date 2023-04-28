library(dplyr)
#library(glue)
library(readr)
#library(plyr)
#library(tidyr)
#library(stringi)
#library(data.table)

####################
# IMPORT CSV FILES #
####################

mlb_bios <- read_csv('mlb_bios_clean.csv')
mlb_batters <- read_csv('mlb_batters_clean.csv')
mlb_pitchers <- read_csv('mlb_pitchers_clean.csv')

pros_bios <- read_csv('pros_bios_clean.csv')
pros_batters <- read_csv('pros_batters_clean.csv')
pros_pitchers <- read_csv('pros_pitchers_clean.csv')

display_prospects <- read_csv('pros_display.csv')
display_prospects$Name <- iconv(display_prospects$Name, from = "latin1", to = "ASCII//TRANSLIT")

pros_mugs <- read_csv("pros_headshots.csv")
mlb_mugs <- read_csv("mlb_headshots.csv")
joe_random <- "https://img.mlbstatic.com/mlb-photos/image/upload/d_people:generic:headshot:67:current.png/w_213,q_auto:best/v1/people/150397/headshot/67/current" 

####################
# Merge CSV FILES #
####################

mlb <- merge(mlb_batters, mlb_pitchers, by=c('Name','Arch'), all = TRUE)
mlb <- merge(mlb_bios, mlb, by='Name')
mlb$Team <- paste0('(',mlb$Team,')')
mlb$Name <- paste(mlb$Name, mlb$Team, sep = ' ')

pros <- merge(pros_batters, pros_pitchers, by=c('Name','Arch'), all = TRUE)
pros <- merge(pros_bios, pros, by='Name')

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

  
  mlb_comp = reactive({
    selected_data <- pros %>% filter(Name == selected_player())
    
    if (selected_data$Pos.Class == 'Position'){
      comp_group <- mlb %>% filter(Pos.Class == selected_data$Pos.Class &
                                     Pos.Group == selected_data$Pos.Group &
                                     Arch == selected_data$Arch &
                                     B == selected_data$B)
      
      rownames(comp_group) <- comp_group$Name
      comp_group <- comp_group %>% select(H, W, HIT, PWR, RAW, BAT_CTRL, DISC, FLD, SPD)
      
      rownames(selected_data) <- selected_data$Name
      selected_data <- selected_data %>% select(H, W, HIT, PWR, RAW, BAT_CTRL, DISC, FLD, SPD)
      
    } else {
      comp_group <- mlb %>% filter(Arch == selected_data$Arch &
                                     T == selected_data$B &
                                     FB.Type == selected_data$FB.Type)
      
      rownames(comp_group) <- comp_group$Name
      comp_group <- comp_group %>% select(H, W, FB, SL, CB, CH, CMD)
      
      comp_group[is.na(comp_group)] <- 0
      
      rownames(selected_data) <- selected_data$Name
      selected_data <- selected_data %>% select(H, W, FB, SL, CB, CH, CMD)
      
      selected_data[is.na(selected_data)] <- 0
      
    }
    
    for (i in 1:nrow(comp_group)){
      comp_group[i,] <- abs(comp_group[i,] - selected_data)
    }
    
    comp_group$Ovl <- rowSums(comp_group)
    comp_group$Name <- rownames(comp_group)
    rownames(comp_group) <- NULL
    comp_group <- comp_group %>% relocate(Name, .before = H)
    comp_group <- comp_group %>% arrange(Ovl)
    comp_group <- comp_group %>% filter(comp_group$Ovl == comp_group[1,ncol(comp_group)])
    
    comp_data <- comp_group[sample(1:nrow(comp_group),1),]
    
    comp <- comp_data[1,1]
    comp <- gsub("\\s*\\([^\\)]+\\)","", comp)
    print(comp)
    #perc_match <- ((500 - comp_data$Ovl) / 500) * 100
  })
  
  mlb_url = reactive({
    src = mlb_mugs['url'][mlb_mugs['name'] == mlb_comp()]
  })
  
  
  pros_url = reactive({
    src = pros_mugs['url'][pros_mugs['name'] == selected_player()]
  })

  output$pros_mug <- renderText({c('<img src="',joe_random,'" height="110" width="80" align ="center">')})

  output$mlb_mug <- renderText({c('<img src="',mlb_url(),'" height="110" width="80" align ="center">')})

  output$prospect <- renderText(selected_player())

  output$comp <- renderText(mlb_comp())


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