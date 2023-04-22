library(dplyr)
library(plyr)
library(tidyr)
library(readr)

#Last Updated 02/22/23
pros_batters <- read_csv('raw csv/pros_batters_raw.csv') # Fangraphs THE BOARD: Scouting Only, Scouting - Position
pros_batters <- pros_batters %>% arrange(`Top 100`)

tool_list <- c('Hit','Game','Raw','Spd','Fld')
for (a in tool_list){
  pros_batters <- pros_batters %>% separate(a, c(paste('c',a),a), " / ")
}

pros_batters <- pros_batters %>% select(Name, Hit, Game, Raw, `Bat Ctrl`, `Pitch Sel`, Fld, Spd)
names(pros_batters) <- c('Name', 'HIT', 'PWR', 'RAW', 'BAT_CTRL', 'DISC', 'FLD', 'SPD')

pros_batters <- pros_batters %>% mutate_at(c('HIT', 'PWR', 'RAW', 'BAT_CTRL', 'DISC', 'FLD', 'SPD'), as.numeric)

pros_batters$HIT <- plyr::round_any(pros_batters$HIT, 10, f = ceiling)
pros_batters$PWR <- plyr::round_any(pros_batters$PWR, 10, f = ceiling)
pros_batters$RAW <- plyr::round_any(pros_batters$RAW, 10, f = ceiling)
pros_batters$BAT_CTRL <- plyr::round_any(pros_batters$BAT_CTRL, 10, f = ceiling)
pros_batters$DISC <- plyr::round_any(pros_batters$DISC, 10, f = ceiling)
pros_batters$FLD <- plyr::round_any(pros_batters$FLD, 10, f = ceiling)
pros_batters$SPD <- plyr::round_any(pros_batters$SPD, 10, f = ceiling)

pros_batters$Arch <- NA

pros_batters <- pros_batters %>%
  mutate(
    Arch = case_when(
      PWR >= 60 & (PWR - HIT) >= 20  ~ 'PWR',
      (HIT - PWR) >= 20  ~ 'CON',
      FLD > HIT & FLD > PWR & FLD > RAW & HIT < 50 & PWR < 50 ~ 'FLD',
      SPD >= 70 & HIT <= 50 & PWR <= 50 ~ 'SPD',
      HIT >= 70 & PWR >= 70 & FLD >= 60 & SPD >= 60 ~ '5-T',
      is.na(Arch) ~ 'BLNC'
    )
  )

pros_batters <- pros_batters %>% relocate(Arch, .before = HIT)

pros_batters <- lapply(pros_batters, as.character)
pros_batters <- data.frame(pros_batters)


write_csv(pros_batters, 'app/pros_batters_clean.csv')
