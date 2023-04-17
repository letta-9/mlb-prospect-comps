library(baseballr)
library(dplyr)
library(plyr)
library(tidyr)
library(readr)

pros_bios <- read_csv('csv/pros_bios_raw.csv') # From Fangraphs THE BOARD (Scouting Only) - Summary

pros_bios$Bio.Class <- NA

pros_bios <- pros_bios %>%
  mutate(
    Bio.Class = case_when(
      Pos == 'SP' | Pos == 'SIRP' | Pos == 'MIRP'  ~ 'Pitcher',
    )
  )

pros_bios$Bio.Class[is.na(pros_bios$Bio.Class)] <- 'Position'
pros_bios <- pros_bios %>% select(Name, Bio.Class, Pos, B, T, Wt, Ht)


# Convert height to inches
pros_bios <- cbind(pros_bios, pros_bios$Ht)
names(pros_bios)[8] <- 'H'
pros_bios <- pros_bios %>% separate(H, c('Ft','In'), "' ")
pros_bios$In <- sub('"','',pros_bios$In)
pros_bios$Ft <- as.numeric(pros_bios$Ft)
pros_bios$In <- as.numeric(pros_bios$In)
pros_bios$H <- (pros_bios$Ft * 12) + pros_bios$In
names(pros_bios)[6] <- 'W'
pros_bios <- pros_bios %>% select('Name', 'Bio.Class', 'Pos', 'B', 'T', 'H', 'W')


#CONVERT TO 20-80 SCALE
for (i in 6:7){
  body_std <- sd(pros_bios[,i])
  body_mean <- mean(pros_bios[,i])
  body_breaks = c((body_mean - (2*body_std)),(body_mean - body_std), body_mean, (body_mean + body_std),(body_mean + (2*body_std)))
  t <- findInterval(pros_bios[,i], body_breaks)
  t <- factor(t)
  levels(t) <- c(30,40,50,60,70,80)
  pros_bios <- cbind(pros_bios, t)
}

names(pros_bios) <- c('Name', 'Bio.Class', 'POS', 'B', 'T', 'Height', 'Weight', 'H', 'W')
pros_bios <- pros_bios %>% select('Name', 'Bio.Class', 'POS', 'B', 'T', 'H', 'W')

write_csv(pros_bios, 'csv/pros_bios_clean.csv')
