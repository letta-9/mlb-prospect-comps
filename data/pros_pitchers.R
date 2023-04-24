#library(baseballr)
library(dplyr)
library(plyr)
library(tidyr)
library(readr)

# Imports
pros_pitchers <- read.csv('raw csv/pros_pitchers_raw.csv')

# Clean
pros_pitchers <- pros_pitchers %>% select(Name, FB.Type, FB, SL, CB, CH, CMD)

tool_list <- c('FB','SL','CB','CH','CMD')
for (i in tool_list){
  pros_pitchers <- pros_pitchers %>% separate(i, c(paste('G',i),i), " / ")
}

pros_pitchers <- pros_pitchers %>% select(Name, FB.Type, FB, SL, CB, CH, CMD)

pros_pitchers <- pros_pitchers %>% mutate_at(c('FB','SL','CB','CH','CMD'), as.numeric)
for (i in 3:7){
  pros_pitchers[,i] <- plyr::round_any(pros_pitchers[,i], 10, f = ceiling)
}

pros_pitchers <- pros_pitchers %>%
  mutate(
    FB.Type = case_when(
      FB.Type == 'Sink' | FB.Type == 'Sink/Tail' ~ 'Sinker',
      FB.Type == 'Cut' | FB.Type == 'Cut/Rise' | FB.Type == 'Cut/Downhi' | FB.Type == 'Cut+Sink'  ~ 'Cutter',
    )
  )

pros_pitchers$FB.Type[is.na(pros_pitchers$FB.Type)] <- '4-Seamer'

pros_pitchers$Has.FB <- !is.na(pros_pitchers$FB)
pros_pitchers$Has.SL <- !is.na(pros_pitchers$SL)
pros_pitchers$Has.CB <- !is.na(pros_pitchers$CB)
pros_pitchers$Has.CH <- !is.na(pros_pitchers$CH)

pros_pitchers <- pros_pitchers %>% mutate_all(as.character)

for (i in 3:7){
  pros_pitchers[,i] <- as.numeric(as.character(pros_pitchers[,i]))
}


pros_pitchers$Arch <- NA

pros_pitchers <- pros_pitchers %>%
  mutate(
    Arch = case_when(
      FB >= 70 & CMD >= 40  ~ 'VELO',
      FB < 70 & CMD >= 60  ~ 'CTRL',
      FB < 70 & (SL >= 70 | CB >= 70 | CH >= 70)  ~ 'BREAK',
      FB < 70 & CMD < 40 ~ 'B-WLD',
      FB >= 70 & CMD < 40 ~ 'V-WLD',
      is.na(Arch) ~ 'BLNC',
    )
  )

pros_pitchers <- pros_pitchers %>% relocate(Arch, .before = FB.Type)


write_csv(pros_pitchers, 'app/pros_pitchers_clean.csv')
