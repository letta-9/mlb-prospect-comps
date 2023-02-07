library(baseballr)
library(dplyr)
library(plyr)
library(tidyr)
library(readr)

#Last Updated 10/10/22
prospects <- read_csv('prospects_raw.csv') #From fangraphs THE BOARD - Summary, Scoutin Position, Scouting Pitching. Remove space in Top 100 Column
pros_arm <- read.csv('prospect_arm_raw.csv') #From fangraphs THE BOARD - Physical Attributes


#######################
# CLEAN PROSPECT DATA #
#######################

prospects$Age <- floor(prospects$Age)

prospects$Class <- ifelse(prospects$Pos == 'SP' | prospects$Pos == 'MIRP'| prospects$Pos == 'SIRP', 'Pitcher', 'Position')

tool_list <- c('Hit','Game','Raw','Spd','Fld','FB','SL','CB','CH','CMD')
for (a in tool_list){
  prospects <- prospects %>% separate(a, c(paste('c',a),a), " / ")
}

prospects <- merge(prospects, pros_arm, by='Name')

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
prospects$Arm <-as.numeric(prospects$Arm)
prospects$Arm <- plyr::round_any(prospects$Arm, 10, f = ceiling)
prospects$FB <-as.numeric(prospects$FB)
prospects$FB <- plyr::round_any(prospects$FB, 10, f = ceiling)
prospects$SL <-as.numeric(prospects$SL)
prospects$SL <- plyr::round_any(prospects$SL, 10, f = ceiling)
prospects$CB <-as.numeric(prospects$CB)
prospects$CB <- plyr::round_any(prospects$CB, 10, f = ceiling)
prospects$CH <-as.numeric(prospects$CH)
prospects$CH <- plyr::round_any(prospects$CH, 10, f = ceiling)
prospects$CMD <-as.numeric(prospects$CMD)
prospects$CMD <- plyr::round_any(prospects$CMD, 10, f = ceiling)

prospects$Class[prospects$Pos.x == 'C'] <- 'Catcher'

prospects <- prospects %>% select(c(1,2,3,39,4,5,6,7,8,9,11,15,17,19,21,45,24,26,28,30,32))
colnames(prospects)[3] <- 'Pos'
colnames(prospects)[6] <- 'Age'

prospects <- cbind(prospects, prospects$Class)
colnames(prospects)[22] <- 'Bio.Class'
prospects$Bio.Class[prospects$Bio.Class == 'Catcher'] <- 'Position'

prospects <-cbind(prospects, prospects$H)
prospects <-cbind(prospects, prospects$W)
prospects <- prospects %>% separate(H, c('Ft','In'), "' ")
prospects$In <- sub('"','',prospects$In)
prospects$Ft <- as.numeric(prospects$Ft)
prospects$In <- as.numeric(prospects$In)
prospects$H <- (prospects$Ft * 12) + prospects$In
prospects <- prospects %>% relocate(H, .before = W)



for (i in 9:10){
  body_std <- sd(prospects[,i])
  body_mean <- mean(prospects[,i])
  
  body_breaks = c((body_mean - (2*body_std)),(body_mean - body_std), body_mean, (body_mean + body_std),(body_mean + (2*body_std)))
  
  t <- findInterval(prospects[,i], body_breaks)
  t <- factor(t)
  
  levels(t) <- c(30,40,50,60,70,80)
  
  prospects <- cbind(prospects, t)
}

prospects <- prospects[c(1,2,3,4,5,6,27,28,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26)]
colnames(prospects)[7] <- 'H'
colnames(prospects)[8] <- 'W'
colnames(prospects)[23] <- 'Ht'
colnames(prospects)[24] <- 'Wt'

#####################
#Add Pos Class column
#####################

prospects <- prospects %>% 
  mutate(
    Pos.Class = case_when(
      Pos == 'CF' | Pos == 'LF' | Pos == 'RF' ~ 'OF',
      Pos == '2B' | Pos == 'SS' ~ 'MI',
      Pos == '3B' | Pos == '1B' ~ 'CI',      
      Pos == 'C' ~ 'C',
      Pos == 'DH' ~ 'DH',
      Pos == 'SP' ~ 'SP',
      Pos == 'MIRP' | Pos == 'SIRP' ~ 'RP',
    )
  )

#####################
#Add Pos Archetype column
#####################

prospects$Arch <- NA

prospects <- prospects %>% 
  mutate(
    Arch = case_when(
      Class == 'Pitcher' & FB >= 70 & CMD >= 40  ~ 'VELO',
      Class == 'Pitcher' & FB < 70 & CMD >= 60  ~ 'CTRL',
      Class == 'Pitcher' & FB < 70 & (SL >= 70 | CB >= 70 | CH >= 70)  ~ 'BREAK',
      Class == 'Pitcher' & FB < 70 & CMD < 40 ~ 'B-WLD',
      Class == 'Pitcher' & FB >= 70 & CMD < 40 ~ 'V-WLD',
      Class == 'Pitcher' & is.na(Arch) ~ 'BLNC',
      Bio.Class == 'Position' & Game >= 60 & (Game - Hit) >= 20  ~ 'PWR',
      Bio.Class == 'Position' & (Hit - Game) >= 20  ~ 'CON',
      Bio.Class == 'Position' & Fld > Hit & Fld > Game & Fld > Raw & Hit < 50 & Game < 50 ~ 'FLD',
      Bio.Class == 'Position' & Spd >= 70 & Hit < 50 & Game < 50 ~ 'SPD',
      Bio.Class == 'Position' & Hit >= 60 & Game >= 60 & Fld >= 60 & Arm >= 60 & Fld >= 60 ~ '5-T',
      Bio.Class == 'Position' & is.na(Arch) ~ 'BLNC'
    )
  )


write_csv(prospects, 'prospects_clean.csv') #Move to app folder

