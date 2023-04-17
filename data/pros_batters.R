#library(baseballr)
library(dplyr)
library(plyr)
library(tidyr)
library(readr)

#Last Updated 02/22/23
pros_batters <- read_csv('csv/pros_batters_raw.csv') # Fangraphs THE BOARD: Scouting Only, Scouting - Position
pros_batters <- pros_batters %>% arrange(`Top 100`)

tool_list <- c('Hit','Game','Raw','Spd','Fld')
for (a in tool_list){
  pros_batters <- pros_batters %>% separate(a, c(paste('c',a),a), " / ")
}

pros_batters <- pros_batters %>% select(Name, Hit, Game, Raw, `Bat Ctrl`, `Pitch Sel`, Fld, Spd)
names(pros_batters) <- c('Name', 'HIT', 'PWR', 'RAW', 'BAT_CTRL', 'DISC', 'FLD', 'SPD')

pros_batters <- pros_batters %>% mutate_at(c('HIT', 'PWR', 'RAW', 'BAT_CTRL', 'DISC', 'FLD', 'SPD'), as.numeric)

# for (i in 2:8){
#   pros_batters[,i] <- plyr::round_any(pros_batters[,i], 10, f = ceiling)
# }

pros_batters$HIT <- plyr::round_any(pros_batters$HIT, 10, f = ceiling)
pros_batters$PWR <- plyr::round_any(pros_batters$PWR, 10, f = ceiling)
pros_batters$RAW <- plyr::round_any(pros_batters$RAW, 10, f = ceiling)
pros_batters$BAT_CTRL <- plyr::round_any(pros_batters$BAT_CTRL, 10, f = ceiling)
pros_batters$DISC <- plyr::round_any(pros_batters$DISC, 10, f = ceiling)
pros_batters$FLD <- plyr::round_any(pros_batters$FLD, 10, f = ceiling)
pros_batters$SPD <- plyr::round_any(pros_batters$SPD, 10, f = ceiling)


write_csv(pros_batters, 'csv/pros_batters_clean.csv')


# #####################
# #Add Pos Archetype column
# #####################
# 
# pros_batters$Arch <- NA
# 
# pros_batters <- pros_batters %>% 
#   mutate(
#     Arch = case_when(
#       Class == 'Pitcher' & FB >= 70 & CMD >= 40  ~ 'VELO',
#       Class == 'Pitcher' & FB < 70 & CMD >= 60  ~ 'CTRL',
#       Class == 'Pitcher' & FB < 70 & (SL >= 70 | CB >= 70 | CH >= 70)  ~ 'BREAK',
#       Class == 'Pitcher' & FB < 70 & CMD < 40 ~ 'B-WLD',
#       Class == 'Pitcher' & FB >= 70 & CMD < 40 ~ 'V-WLD',
#       Class == 'Pitcher' & is.na(Arch) ~ 'BLNC',
#       Bio.Class == 'Position' & Game >= 60 & (Game - Hit) >= 20  ~ 'PWR',
#       Bio.Class == 'Position' & (Hit - Game) >= 20  ~ 'CON',
#       Bio.Class == 'Position' & Fld > Hit & Fld > Game & Fld > Raw & Hit < 50 & Game < 50 ~ 'FLD',
#       Bio.Class == 'Position' & Spd >= 70 & Hit < 50 & Game < 50 ~ 'SPD',
#       Bio.Class == 'Position' & Hit >= 60 & Game >= 60 & Fld >= 60 & Arm >= 60 & Fld >= 60 ~ '5-T',
#       Bio.Class == 'Position' & is.na(Arch) ~ 'BLNC'
#     )
#   )
# 
# 
# write_csv(pros_batters, 'pros_batters_clean.csv') #Move to app folder
# 
