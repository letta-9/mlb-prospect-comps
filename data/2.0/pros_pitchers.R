#library(baseballr)
library(dplyr)
library(plyr)
library(tidyr)
library(readr)

# Imports
pros_pitchers <- read.csv('pros_pitchers.csv')

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
