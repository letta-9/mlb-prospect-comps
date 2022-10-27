library(baseballr)
library(dplyr)
library(plyr)
library(tidyr)
library(readr)

pit_mov <- read.csv('pitchers_raw.csv') #Fome baseball savant export csv
mlb_bios <- read_csv('mlb_bios_clean.csv') #From baseballr. Cleaned in batters.R


#############################
# CLEAN MLB PITCHER DATA #
#############################

# mlb_bios <- mlb_sports_players(sport_id = 1, season = 2022)
# mlb_bios$weight <- plyr::round_any(mlb_bios$weight, 5, f=ceiling)
# mlb_bios$primary_position_type <- ifelse(mlb_bios$primary_position_type != 'Pitcher', 'Position', mlb_bios$primary_position_type)
# mlb_bios <- mlb_bios %>% select(full_name, primary_position_abbreviation, primary_position_type, height, weight, bat_side_code, pitch_hand_code)
# names(mlb_bios) <- c('Name', 'Pos', 'Class','H','W','B','T')

pit_mov$first_name <- sub(' ','',pit_mov$first_name)
pit_mov$last_name <- sub(' ','',pit_mov$last_name)

pit_mov$Name <- paste(pit_mov$first_name, pit_mov$last_name)
pit_mov$Name <- iconv(pit_mov$Name, from = "UTF-8", to = "ASCII//TRANSLIT")
pit_mov <- pit_mov %>% select(c(25, 7, 14, 8, 17, 21))
names(pit_mov) <- c('Name', 'T', 'Pitch', 'Vel', 'vMov', 'hMov')
pit_mov$Pitch[pit_mov$Pitch == '4-Seamer'] <- 'FB'
pit_mov$Pitch[pit_mov$Pitch == 'Slider'] <- 'SL'
pit_mov$Pitch[pit_mov$Pitch == 'Curveball'] <- 'CB'
pit_mov$Pitch[pit_mov$Pitch == 'Changeup'] <- 'CH'
pit_mov$Pitch[pit_mov$Pitch == 'Splitter'] <- 'SPL'
pit_mov$Pitch[pit_mov$Pitch == 'Sinker'] <- 'SNK'
pit_mov$Pitch[pit_mov$Pitch == 'Cutter'] <- 'CUT'


for (i in 4:6){
  pit_std <- sd(pit_mov[,i])
  pit_mean <- mean(pit_mov[,i])

  pit_breaks = c((pit_mean - (2*pit_std)),(pit_mean - pit_std), pit_mean, (pit_mean + pit_std),(pit_mean + (2*pit_std)))

  p <- findInterval(pit_mov[,i], pit_breaks)
  p <- factor(p)

  levels(p) <- c(30, 40, 50, 60, 70, 80)

  pit_mov <- cbind(pit_mov, p)

}

names(pit_mov) <- c('Name', 'T', 'Pitch', 'Vel', 'vMov', 'hMov', 'gVel','gVMov','gHMov')
pit_mov <- pit_mov %>% select(c(1,2,3,7,8,9))
pit_mov$gVel <- (as.integer(pit_mov$gVel) + 2) * 10
pit_mov$gHMov <- (as.integer(pit_mov$gHMov) + 2) * 10
pit_mov$gVMov <- (as.integer(pit_mov$gVMov) + 2) * 10

pit_mov$Grade <- ifelse(pit_mov$Pitch == 'FB', (pit_mov$gVel + ((pit_mov$gVMov + pit_mov$gHMov)/2))/2, NA)
pit_mov$Grade <- ifelse(pit_mov$Pitch == 'CH', (pit_mov$gVMov + pit_mov$gHMov)/2, pit_mov$Grade)
pit_mov$Grade <- ifelse(pit_mov$Pitch == 'SL', (pit_mov$gVel + pit_mov$gVMov + pit_mov$gHMov)/3, pit_mov$Grade)
pit_mov$Grade <- ifelse(pit_mov$Pitch == 'CB', (pit_mov$gVMov + pit_mov$gHMov)/2, pit_mov$Grade)
pit_mov$Grade <- ifelse(pit_mov$Pitch == 'CUT', (pit_mov$gVel + pit_mov$gVMov + pit_mov$gHMov)/3, pit_mov$Grade)
pit_mov$Grade <- ifelse(pit_mov$Pitch == 'SPL', (pit_mov$gVel + pit_mov$gVMov + pit_mov$gHMov)/3, pit_mov$Grade)
pit_mov$Grade <- ifelse(pit_mov$Pitch == 'SNK', (pit_mov$gVel + pit_mov$gVMov + pit_mov$gHMov)/3, pit_mov$Grade)

pit_mov$Grade <- round_any(pit_mov$Grade, 10)
pit_mov <- pit_mov %>% select(c(1,2,3,7))

pit_mov <- reshape(pit_mov, timevar = 'Pitch', idvar = c('Name','T'), direction = 'wide')
names(pit_mov) <- c('Name', 'T', 'CUT', 'FB', 'SNK', 'SL', 'CB','CH','SPL')
pit_mov[is.na(pit_mov)] <- 0
pit_mov$Class <- 'Pitcher'

pit_mov$FB <- pmax(pit_mov$CUT, pit_mov$FB, pit_mov$SNK)


pit_mov <- merge(pit_mov, mlb_bios, by='Name')

pit_mov <- pit_mov[c(1,4,6,7,8,2,10,15,16)]

colnames(pit_mov)[6] <- 'T'

write_csv(pit_mov, 'pitchers_clean.csv')

