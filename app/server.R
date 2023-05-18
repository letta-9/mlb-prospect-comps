library(dplyr)
library(readr)
library(ggplot2)

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
  
  
  #######################
  # VARIABLES
  #######################
  
  output$display_prospects <- renderDT(
    display_prospects,
    options = list(pageLength = 20),
    rownames = FALSE,
    selection = "single",
  )
  
  
  selected_player = reactive({
    x = isolate(display_prospects)
    x = x[input$display_prospects_rows_selected,]
    x = x$Name
  })
  
  
  selected_data = reactive({
    y <- pros %>% filter(Name == selected_player())
    
    if (y$Pos.Class == 'Position'){
      rownames(y) <- y$Name
      y <- y %>% select(H, W, HIT, PWR, RAW, BAT_CTRL, DISC, FLD, SPD)
    } else {
      rownames(y) <- y$Name
      y <- y %>% select(H, W, FB, SL, CB, CH, CMD)
      y[is.na(y)] <- 0
    }
    print(y)
  })
  
  
  
  mlb_comp = reactive({
    z <- pros %>% filter(Name == selected_player())
    
    if (z$Pos.Class == 'Position'){
      comp_group <- mlb %>% filter(Pos.Class == z$Pos.Class &
                                     Pos.Group == z$Pos.Group &
                                     Arch == z$Arch &
                                     B == z$B)
      
      rownames(comp_group) <- comp_group$Name
      comp_group <- comp_group %>% select(H, W, HIT, PWR, RAW, BAT_CTRL, DISC, FLD, SPD)
    } else {
      comp_group <- mlb %>% filter(Arch == z$Arch &
                                     T == z$B &
                                     FB.Type == z$FB.Type)
      
      rownames(comp_group) <- comp_group$Name
      comp_group <- comp_group %>% select(H, W, FB, SL, CB, CH, CMD)
      comp_group[is.na(comp_group)] <- 0
    }
    
    for (i in 1:nrow(comp_group)){
      comp_group[i,] <- abs(comp_group[i,] - selected_data())
    }
    
    print(comp_group)
    
    comp_group$Ovl <- rowSums(comp_group)
    comp_group$Name <- rownames(comp_group)
    comp_group <- comp_group %>% arrange(Ovl)
    rownames(comp_group) <- comp_group$Name
    comp_group <- comp_group %>% select(-last_col())
    #print(comp_group)
    comp_group <- comp_group %>% filter(comp_group$Ovl == comp_group[1,ncol(comp_group)])
    
    
    comp_data <- comp_group[sample(1:nrow(comp_group),1),]
    mlb_comp <- rownames(comp_data)
    #comp <- gsub("\\s*\\([^\\)]+\\)","", comp)
    #perc_match <- ((500 - comp_data$Ovl) / 500) * 100
    print(mlb_comp)
  })
  
  mlb_comp_display = reactive({
    mlb_comp_display <- gsub("\\s*\\([^\\)]+\\)","", mlb_comp())
  })
  
  
  
  mlb_url = reactive({
    src = mlb_mugs['url'][mlb_mugs['name'] == mlb_comp_display()]
  })
  
  
  pros_url = reactive({
    src = pros_mugs['url'][pros_mugs['name'] == selected_player()]
  })
  
  
  chart_data = reactive({
    comp_tools <- mlb %>% filter(Name == mlb_comp())
    
    if (comp_tools$Pos.Class == 'Position'){
      rownames(comp_tools) <- comp_tools$Name
      comp_tools <- comp_tools %>% select(H, W, HIT, PWR, RAW, BAT_CTRL, DISC, FLD, SPD)
    } else {
      rownames(comp_tools) <- comp_tools$Name
      comp_tools <- comp_tools %>% select(H, W, FB, SL, CB, CH, CMD)
    }
    chart_data <- rbind(selected_data(), comp_tools) 
    
    chart_data <- t(chart_data)
    chart_data <- data.frame(chart_data) #############################
    colnames(chart_data) <- c(selected_player(), mlb_comp())
    chart_data$Tool <- rownames(chart_data)
    rownames(chart_data) <- NULL
    
    
    pros_chart <- chart_data[,c(1,3)]
    pros_chart$Player <- colnames(pros_chart)[1]
    colnames(pros_chart)[1] <- 'Grade'
    
    comp_chart <- chart_data[,c(2,3)]
    comp_chart$Player <- colnames(comp_chart)[1]
    colnames(comp_chart)[1] <- 'Grade'
    
    chart_data <- rbind(pros_chart, comp_chart)
  })
  
  #######################
  # OUTPUTS
  #######################
  
  output$pros_mug <- renderText({c('<img src="',joe_random,'" height="110" width="80" align ="center">')})
  
  
  output$mlb_mug <- renderText({c('<img src="',mlb_url(),'" height="110" width="80" align ="center">')})
  
  
  output$prospect <- renderText(selected_player())
  
  
  output$comp <- renderText(mlb_comp_display())

  
  output$compPlot = renderPlot({
    if (pros$Pos.Class[pros$Name == selected_player()] == 'Position'){
      ggplot(chart_data(), aes(x=factor(Tool, levels=c('H', 'W', 'HIT', 'PWR', 'RAW', 'BAT_CTRL', 'DISC', 'FLD', 'SPD')), y=Grade, group=Player, fill=Player))+
               geom_bar(position = 'dodge', stat = 'identity')+
               scale_y_continuous(expand=c(0,0))+
               xlab('Tools')+
               theme_classic()

    } else {
      ggplot(chart_data(), aes(x=factor(Tool, levels=c('H', 'W', 'FB', 'SL', 'CB', 'CH', 'CMD')), y=Grade, group=Player, fill=Player))+
        geom_bar(position = 'dodge', stat = 'identity')+
        scale_y_continuous(expand=c(0,0))+
        xlab('Tools')+
        theme_classic()
    }
  })

  
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