library(dplyr)
library(readr)
library(ggplot2)


mlb_bios <- read_csv('mlb_bios_clean.csv')
mlb_batters <- read_csv('mlb_batters_clean.csv')
mlb_pitchers <- read_csv('mlb_pitchers_clean.csv')

pros_bios <- read_csv('pros_bios_clean.csv')
pros_batters <- read_csv('pros_batters_clean.csv')
pros_pitchers <- read_csv('pros_pitchers_clean.csv')

mlb <- merge(mlb_batters, mlb_pitchers, by=c('Name','Arch'), all = TRUE)
mlb <- merge(mlb_bios, mlb, by='Name')
mlb$Team <- paste0('(',mlb$Team,')')
mlb$Name <- paste(mlb$Name, mlb$Team, sep = ' ')

pros <- merge(pros_batters, pros_pitchers, by=c('Name','Arch'), all = TRUE)
pros <- merge(pros_bios, pros, by='Name')

selected <- "Logan O'Hoppe"
#selected <- "Jack Leiter"

selected_data <- pros %>% filter(Name == selected)

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
comp_group <- comp_group %>% arrange(Ovl)
comp_group <- comp_group %>% filter(comp_group$Ovl == comp_group[1,ncol(comp_group)])


comp_data <- comp_group[sample(1:nrow(comp_group),1),]
comp <- rownames(comp_data)
perc_match <- ((500 - comp_data$Ovl) / 500) * 100


## Create Chart ##

comp_tools <- mlb %>% filter(Name == comp)

if (comp_tools$Pos.Class == 'Position'){
  rownames(comp_tools) <- comp_tools$Name
  comp_tools <- comp_tools %>% select(H, W, HIT, PWR, RAW, BAT_CTRL, DISC, FLD, SPD)
} else {
  rownames(comp_tools) <- comp_tools$Name
  comp_tools <- comp_tools %>% select(Name, H, W, FB, SL, CB, CH, CMD)
}

chart_data <- rbind(selected_data, comp_tools)
chart_data <- t(chart_data)
chart_data <- data.frame(chart_data)
colnames(chart_data) <- c(selected, comp)
chart_data$Tool <- rownames(chart_data)
rownames(chart_data) <- NULL

pros_chart <- chart_data[,c(1,3)]
pros_chart$Player <- colnames(pros_chart)[1]
colnames(pros_chart)[1] <- 'Grade'

comp_chart <- chart_data[,c(2,3)]
comp_chart$Player <- colnames(comp_chart)[1]
colnames(comp_chart)[1] <- 'Grade'

chart_data <- rbind(pros_chart, comp_chart)

if (pros$Pos.Class[pros$Name == selected] == 'Position'){
  
  ggplot(chart_data, aes(x=factor(Tool, levels=c('H', 'W', 'HIT', 'PWR', 'RAW', 'BAT_CTRL', 'DISC', 'FLD', 'SPD')), y=Grade, group=Player, fill=Player))+
           geom_bar(position = 'dodge', stat = 'identity')+
           scale_y_continuous(expand=c(0,0))+
           xlab('Tools')+
           theme_classic()
       
} else {
  ggplot(chart_data, aes(x=factor(Tool, levels=c('H', 'W', 'FB', 'SL', 'CB', 'CH', 'CMD')), y=Grade, group=Player, fill=Player))+
    geom_bar(position = 'dodge', stat = 'identity')+
    scale_y_continuous(expand=c(0,0))+
    xlab('Tools')+
    theme_classic()
  
}

print(comp)
print(perc_match)