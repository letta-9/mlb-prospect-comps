library(baseballr)
library(dplyr)
library(plyr)
library(tidyr)
library(readr)

prospects <- read_csv('prospects_raw.csv') #From fangraphs THE BOARD / mlb.com
pros_cat <- read.csv('prospect_catchers_raw.csv') #From fangraphs THE BOARD

#######################
# CLEAN PROSPECT DATA #
#######################

prospects$Age <- floor(prospects$Age)

prospects$W <- plyr::round_any(prospects$W, 10, round)
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
prospects$FB <-as.numeric(prospects$FB)
prospects$FB <- plyr::round_any(prospects$FB, 10, f = ceiling)
prospects$SL <-as.numeric(prospects$SL)
prospects$SL <- plyr::round_any(prospects$SL, 10, f = ceiling)
prospects$CB <-as.numeric(prospects$CB)
prospects$CB <- plyr::round_any(prospects$CB, 10, f = ceiling)
prospects$CH <-as.numeric(prospects$CH)
prospects$CH <- plyr::round_any(prospects$CH, 10, f = ceiling)

prospects <- merge(prospects, pros_cat, by='Name')
prospects$Class[prospects$Pos.x == 'C'] <- 'Catcher'

prospects <- prospects %>% select(c(1,2,3,39,4,5,6,7,8,9,11,15,17,19,21,45,24,26,28,30,32))
colnames(prospects)[3] <- 'Pos'
colnames(prospects)[6] <- 'Age'

prospects <- cbind(prospects, prospects$Class)
colnames(prospects)[22] <- 'Bio.Class'
prospects$Bio.Class[prospects$Bio.Class == 'Catcher'] <- 'Position'

for (i in 7:8){
  body_std <- sd(prospects[,i])
  body_mean <- mean(prospects[,i])
  
  body_breaks = c((body_mean - (2*body_std)),(body_mean - body_std), body_mean, (body_mean + body_std),(body_mean + (2*body_std)))
  
  t <- findInterval(prospects[,i], body_breaks)
  t <- factor(t)
  
  levels(t) <- c(30,40,50,60,70,80)
  
  prospects <- cbind(prospects, t)
}

#mlb_bios <- mlb_bios[c(1,2,3,6,7,8,9)]
#names(mlb_bios) <- c('Name', 'Pos', 'Bio.Class','B','T','H','W')


write_csv(prospects, 'prospects_clean.csv')

