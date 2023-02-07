library(baseballr)
library(dplyr)
library(plyr)
library(tidyr)
library(readr)

mlb_pitchers <- read.csv('pitchers_raw.csv') #CSV From https://baseballsavant.mlb.com/leaderboard/pitch-movement
mlb_bios <- read_csv('mlb_bios_clean.csv') #From batters.R


#############################
# CLEAN MLB PITCHER DATA #
#############################

mlb_pitchers$first_name <- sub(' ','',mlb_pitchers$first_name)
mlb_pitchers$last_name <- sub(' ','',mlb_pitchers$last_name)

mlb_pitchers$Name <- paste(mlb_pitchers$first_name, mlb_pitchers$last_name)
mlb_pitchers$Name <- iconv(mlb_pitchers$Name, from = "UTF-8", to = "ASCII//TRANSLIT")
mlb_pitchers <- mlb_pitchers %>% select(c(25, 7, 14, 8, 17, 21))
names(mlb_pitchers) <- c('Name', 'T', 'Pitch', 'Vel', 'vMov', 'hMov')
mlb_pitchers$Pitch[mlb_pitchers$Pitch == '4-Seamer'] <- 'FB'
mlb_pitchers$Pitch[mlb_pitchers$Pitch == 'Slider'] <- 'SL'
mlb_pitchers$Pitch[mlb_pitchers$Pitch == 'Curveball'] <- 'CB'
mlb_pitchers$Pitch[mlb_pitchers$Pitch == 'Changeup'] <- 'CH'
mlb_pitchers$Pitch[mlb_pitchers$Pitch == 'Splitter'] <- 'SPL'
mlb_pitchers$Pitch[mlb_pitchers$Pitch == 'Sinker'] <- 'SNK'
mlb_pitchers$Pitch[mlb_pitchers$Pitch == 'Cutter'] <- 'CUT'


for (i in 4:6){
  pit_std <- sd(mlb_pitchers[,i])
  pit_mean <- mean(mlb_pitchers[,i])

  pit_breaks = c((pit_mean - (2*pit_std)),(pit_mean - pit_std), pit_mean, (pit_mean + pit_std),(pit_mean + (2*pit_std)))

  p <- findInterval(mlb_pitchers[,i], pit_breaks)
  p <- factor(p)

  levels(p) <- c(30, 40, 50, 60, 70, 80)

  mlb_pitchers <- cbind(mlb_pitchers, p)

}

names(mlb_pitchers) <- c('Name', 'T', 'Pitch', 'Vel', 'vMov', 'hMov', 'gVel','gVMov','gHMov')
mlb_pitchers <- mlb_pitchers %>% select(c(1,2,3,7,8,9))
mlb_pitchers$gVel <- (as.integer(mlb_pitchers$gVel) + 2) * 10
mlb_pitchers$gHMov <- (as.integer(mlb_pitchers$gHMov) + 2) * 10
mlb_pitchers$gVMov <- (as.integer(mlb_pitchers$gVMov) + 2) * 10

mlb_pitchers$Grade <- ifelse(mlb_pitchers$Pitch == 'FB', (mlb_pitchers$gVel + ((mlb_pitchers$gVMov + mlb_pitchers$gHMov)/2))/2, NA)
mlb_pitchers$Grade <- ifelse(mlb_pitchers$Pitch == 'CH', (mlb_pitchers$gVMov + mlb_pitchers$gHMov)/2, mlb_pitchers$Grade)
mlb_pitchers$Grade <- ifelse(mlb_pitchers$Pitch == 'SL', (mlb_pitchers$gVel + mlb_pitchers$gVMov + mlb_pitchers$gHMov)/3, mlb_pitchers$Grade)
mlb_pitchers$Grade <- ifelse(mlb_pitchers$Pitch == 'CB', (mlb_pitchers$gVMov + mlb_pitchers$gHMov)/2, mlb_pitchers$Grade)
mlb_pitchers$Grade <- ifelse(mlb_pitchers$Pitch == 'CUT', (mlb_pitchers$gVel + mlb_pitchers$gVMov + mlb_pitchers$gHMov)/3, mlb_pitchers$Grade)
mlb_pitchers$Grade <- ifelse(mlb_pitchers$Pitch == 'SPL', (mlb_pitchers$gVel + mlb_pitchers$gVMov + mlb_pitchers$gHMov)/3, mlb_pitchers$Grade)
mlb_pitchers$Grade <- ifelse(mlb_pitchers$Pitch == 'SNK', (mlb_pitchers$gVel + mlb_pitchers$gVMov + mlb_pitchers$gHMov)/3, mlb_pitchers$Grade)

mlb_pitchers$Grade <- round_any(mlb_pitchers$Grade, 10)
mlb_pitchers <- mlb_pitchers %>% select(c(1,2,3,7))

mlb_pitchers <- reshape(mlb_pitchers, timevar = 'Pitch', idvar = c('Name','T'), direction = 'wide')
names(mlb_pitchers) <- c('Name', 'T', 'CUT', 'FB', 'SNK', 'SL', 'CB','CH','SPL')
mlb_pitchers[is.na(mlb_pitchers)] <- 0
mlb_pitchers$Class <- 'Pitcher'

mlb_pitchers$FB <- pmax(mlb_pitchers$CUT, mlb_pitchers$FB, mlb_pitchers$SNK)


mlb_pitchers <- merge(mlb_pitchers, mlb_bios, by='Name')

mlb_pitchers <- mlb_pitchers[c(1,4,6,7,8,2,10,15,16)]

colnames(mlb_pitchers)[6] <- 'T'

#####################
#Add CMD column
#####################

CMD_raw <- mlb_stats(stat_type = 'season', player_pool = 'all', stat_group = 'pitching', season = '2022')
CMD_raw <- CMD_raw %>% filter(number_of_pitches >= 250) %>% select(player_full_name, walks_per9inn)

names(CMD_raw) <- c('Name','BB')

CMD_raw$BB <- as.numeric(CMD_raw$BB)
CMD_raw$BB <- 100 - CMD_raw$BB

cmd_std <- sd(CMD_raw$BB)
cmd_mean <- mean(CMD_raw$BB)
cmd_breaks = c((cmd_mean - (2*cmd_std)),(cmd_mean - cmd_std), cmd_mean, (cmd_mean + cmd_std),(cmd_mean + (2*cmd_std)))
CMD <- findInterval(CMD_raw$BB, cmd_breaks)
CMD <- factor(CMD)
levels(CMD) <- c(30,40,50,60,70,80)

CMD_raw <- cbind(CMD_raw, CMD)
CMD_raw <- CMD_raw[,c(1,3)]

mlb_pitchers <- merge(mlb_pitchers, CMD_raw, by = 'Name')
mlb_pitchers <- mlb_pitchers %>% relocate(CMD, .before = T)
mlb_pitchers$CMD <- (as.numeric(mlb_pitchers$CMD) + 2) * 10

#####################
#Add Achetype column
#####################

mlb_pitchers$Arch <- NA

mlb_pitchers <- mlb_pitchers %>%
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



write_csv(mlb_pitchers, 'pitchers_clean.csv')

