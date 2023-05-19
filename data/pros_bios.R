library(dplyr)
library(plyr)
library(tidyr)
library(readr)

pros_bios <- read_csv('raw csv/pros_bios_raw.csv') # From Fangraphs THE BOARD (Scouting Only) - Summary
pros_bios$Name <- iconv(pros_bios$Name, to = "ASCII//TRANSLIT")

pros_bios$Pos.Class <- NA

pros_bios <- pros_bios %>%
  mutate(
    Pos.Class = case_when(
      Pos == 'SP' | Pos == 'SIRP' | Pos == 'MIRP'  ~ 'Pitcher',
    )
  )

pros_bios$Pos[pros_bios$Pos == 'SP'] <- 'P'


pros_bios$Pos.Class[is.na(pros_bios$Pos.Class)] <- 'Position'
pros_bios <- pros_bios %>% select(Name, Pos.Class, Pos, B, T, Wt, Ht)


# Convert height to inches
pros_bios <- cbind(pros_bios, pros_bios$Ht)
names(pros_bios)[8] <- 'H'
pros_bios <- pros_bios %>% separate(H, c('Ft','In'), "' ")
pros_bios$In <- sub('"','',pros_bios$In)
pros_bios$Ft <- as.numeric(pros_bios$Ft)
pros_bios$In <- as.numeric(pros_bios$In)
pros_bios$H <- (pros_bios$Ft * 12) + pros_bios$In
names(pros_bios)[6] <- 'W'
pros_bios <- pros_bios %>% select('Name', 'Pos.Class', 'Pos', 'B', 'T', 'H', 'W')


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

names(pros_bios) <- c('Name', 'Pos.Class', 'Pos', 'B', 'T', 'Height', 'Weight', 'H', 'W')

pros_bios <- pros_bios %>%
  mutate(
    Pos.Group = case_when(
      Pos == 'CF' | Pos == 'LF' | Pos == 'RF' ~ 'OF',
      Pos == '2B' | Pos == 'SS' ~ 'MI',
      Pos == '3B' | Pos == '1B' ~ 'CI',
      Pos == 'C' ~ 'C',
      Pos == 'DH' ~ 'DH',
      Pos == 'P' ~ 'P',
      Pos == 'SIRP' ~ 'P',
      Pos == 'MIRP' ~ 'P'
    )
  )

pros_bios <- pros_bios %>% select('Name', 'Pos.Class', 'Pos.Group', 'Pos', 'B', 'T', 'H', 'W')

write_csv(pros_bios, 'app/pros_bios_clean.csv')
