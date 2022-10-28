library(rvest)
library(RSelenium)
library(dplyr)
library(glue)
library(readr)
library(plyr)
library(tidyr)
library(stringi)
library(data.table)
library(XML)
library(baseballr)



###################################################################################################################################################
# MLB BIOS #
###################################################################################################################################################

mlb_bios <- mlb_sports_players(sport_id = 1, season = 2022) #BASEBALL R

mlb_bios$primary_position_type <- ifelse(mlb_bios$primary_position_type != 'Pitcher', 'Position', mlb_bios$primary_position_type)
mlb_bios <- mlb_bios %>% select(full_name, primary_position_abbreviation, primary_position_type, height, weight, bat_side_code, pitch_hand_code)
names(mlb_bios) <- c('Name', 'Pos', 'Bio.Class','H','W','B','T')
mlb_bios <- mlb_bios %>% separate(H, c('Ft','In'), "' ")
mlb_bios$In <- sub('"','',mlb_bios$In)
mlb_bios$Ft <- as.numeric(mlb_bios$Ft)
mlb_bios$In <- as.numeric(mlb_bios$In)
mlb_bios$H <- (mlb_bios$Ft * 12) + mlb_bios$In
mlb_bios <- mlb_bios[c(1,2,3,9,6,7,8)]
mlb_bios <- data.frame(mlb_bios)


#CONVERT TO 20-80 SCALE
for (i in 4:5){
  body_std <- sd(mlb_bios[,i])
  body_mean <- mean(mlb_bios[,i])

  body_breaks = c((body_mean - (2*body_std)),(body_mean - body_std), body_mean, (body_mean + body_std),(body_mean + (2*body_std)))

  t <- findInterval(mlb_bios[,i], body_breaks)
  t <- factor(t)

  levels(t) <- c(30,40,50,60,70,80)

  mlb_bios <- cbind(mlb_bios, t)
}

mlb_bios <- mlb_bios[c(1,2,3,6,7,8,9)]
names(mlb_bios) <- c('Name', 'Pos', 'Bio.Class','B','T','H','W')

#write_csv(mlb_bios, 'mlb_bios_clean.csv')


###################################################################################################################################################
# MLB BATTERS #
###################################################################################################################################################

#BASEBALL R FOR HR, AVG (2022)
mlb_batters <- mlb_stats(stat_type = 'season', player_pool = 'all', stat_group = 'hitting', season = '2022')
mlb_batters <- mlb_batters %>% filter(plate_appearances > 200)

#BASEBALL R FOR EV (2022)
mlb_ev <- statcast_leaderboards(leaderboard = "exit_velocity_barrels", year = 2022, min_pa = 100)
mlb_ev$player_full_name <- paste(mlb_ev$first_name, mlb_ev$last_name)

#BASEBALL R FOR OAA (2022)
mlb_oaa <- statcast_leaderboards(leaderboard = "outs_above_average", year = 2022, min_field = 50)
mlb_oaa$player_full_name <- paste(mlb_oaa$first_name, mlb_oaa$last_name)

#BASEBALL R FOR SPD (2022)
mlb_spd <- statcast_leaderboards(leaderboard = "sprint_speed", year = 2022)
mlb_spd$player_full_name <- paste(mlb_spd$first_name, mlb_spd$last_name)

#BASEBALL R FOR CATCHER ARM (2022)
mlb_cat <- statcast_leaderboards(leaderboard = "pop_time", year = 2022, min_field = "q")
mlb_cat <- mlb_cat %>% select(catcher,maxeff_arm_2b_3b_sba)
mlb_cat$Pos <- 'C'
colnames(mlb_cat)[1:2] <- c('Name','Arm')
mlb_cat$Name <- iconv(mlb_cat$Name, from = "UTF-8", to = "ASCII//TRANSLIT")


#BASEBALL R FOR STATS OF PAST 3 YEARS
years <- c('2021','2020','2019')

for (b in years){
  w <- mlb_stats(stat_type = 'season', player_pool = 'all', stat_group = 'hitting', season = b)
  w <- w %>% filter(plate_appearances > 200)
  mlb_batters <- rbind(mlb_batters, w)
  
  y <- statcast_leaderboards(leaderboard = "outs_above_average", year = b, min_field = 50)
  y$player_full_name <- paste(y$first_name, y$last_name)
  mlb_oaa <- rbind(mlb_oaa, y)
  
  z <- statcast_leaderboards(leaderboard = "sprint_speed", year = b)
  z$player_full_name <- paste(z$first_name, z$last_name)
  mlb_spd <- rbind(mlb_spd, z)
  
}


mlb_batters <- mlb_batters %>% group_by(player_full_name) %>% filter(n()>2)
mlb_batters$avg <- as.numeric(mlb_batters$avg)
mlb_batters$home_runs <- as.numeric(mlb_batters$home_runs)
mlb_batters$HR.162 <- (mlb_batters$home_runs / mlb_batters$games_played) *162

games <- mlb_batters[c(41,5)]
games <- aggregate(games$games_played, by=list(Name=games$player_full_name), FUN=sum)


all_positions <- mlb_bios[,1:2]

mlb_batters <- aggregate(mlb_batters, list(Name = mlb_batters$player_full_name), mean)
mlb_batters <- mlb_batters %>% select(Name, avg, HR.162)
mlb_batters <- subset(mlb_batters, !is.na(HR.162))

colnames(mlb_ev)[20] <- 'Name'
mlb_ev <- mlb_ev %>% select(Name, avg_hit_speed)
mlb_ev$Name <- iconv(mlb_ev$Name, from = "UTF-8", to = "ASCII//TRANSLIT")


mlb_spd <- mlb_spd %>% group_by(player_full_name) %>% filter(n()>2)
mlb_spd <- aggregate(mlb_spd, list(Name = mlb_spd$player_full_name), mean)
mlb_spd <- mlb_spd %>% select(Name, sprint_speed)
mlb_spd$Name <- iconv(mlb_spd$Name, from = "UTF-8", to = "ASCII//TRANSLIT")


mlb_batters <- merge(mlb_batters, mlb_ev, by = 'Name')
mlb_batters <- merge(mlb_batters, mlb_spd, by = 'Name')

for (i in 2:5){
  tool_std <- sd(mlb_batters[,i])
  tool_mean <- mean(mlb_batters[,i])
  
  tool_breaks = c((tool_mean - (2*tool_std)),(tool_mean - tool_std), tool_mean, (tool_mean + tool_std),(tool_mean + (2*tool_std)))
  
  g <- findInterval(mlb_batters[,i], tool_breaks)
  g <- factor(g)
  
  levels(g) <- c(30,40,50,60,70,80)
  
  
  mlb_batters <- cbind(mlb_batters, g)
  
}

mlb_batters <- mlb_batters %>% dplyr::rename('Hit' = 6,
                                             'Game' = 7,
                                             'Raw' = 8,
                                             'Spd' = 9)

######################
# CREATE FIELDER TABLE
######################


mlb_oaa <- mlb_oaa %>% group_by(player_full_name) %>% filter(n()>2)
mlb_oaa <- mlb_oaa %>% select(player_full_name, outs_above_average)
mlb_oaa$OAA <- as.numeric(mlb_oaa$outs_above_average)
mlb_oaa <- aggregate(mlb_oaa$outs_above_average, list(Name = mlb_oaa$player_full_name), FUN=sum)
names(mlb_oaa) = c("Name","OAA")
mlb_oaa$Name <- iconv(mlb_oaa$Name, from = "UTF-8", to = "ASCII//TRANSLIT")
mlb_oaa <- merge(mlb_oaa, all_positions, by = 'Name')
mlb_oaa <- merge(mlb_oaa, games, by = 'Name')
mlb_oaa$OAA <- as.numeric(mlb_oaa$OAA)
mlb_oaa$OAA162 <- (mlb_oaa$OAA / mlb_oaa$x) * 162


positions <- c("1B", "2B", "SS", "3B", "LF", "CF", "RF")
mlb_oaa_blank <- data.frame(matrix(ncol=6, nrow=0))

for (j in positions) {
  x <- mlb_oaa %>% filter(Pos == j)
  fld_mean <- mean(x$OAA162)
  fld_std <- sd(x$OAA162)
  fld_breaks = c((fld_mean - (2*fld_std)),(fld_mean - fld_std), fld_mean, (fld_mean + fld_std),(fld_mean + (2*fld_std)))
  
  Fld <- findInterval(x$OAA162, fld_breaks)
  Fld <- factor(Fld)
  levels(Fld) <- c(30,40,50,60,70,80)
  
  x <- cbind(x, Fld)
  
  mlb_oaa_blank <- rbindlist(list(mlb_oaa_blank,x), fill = TRUE)
  
}

#################
# CATCHER TABLE #
#################

cat_std <- sd(mlb_cat$Arm)
cat_mean <- mean(mlb_cat$Arm)

cat_breaks = c((cat_mean - (2*cat_std)),(cat_mean - cat_std), cat_mean, (cat_mean + cat_std),(cat_mean + (2*cat_std)))

m <- findInterval(mlb_cat$Arm, cat_breaks)
m <- factor(m)


levels(m) <- c(30,40,50,60,70,80)

mlb_cat <- cbind(mlb_cat, m)
mlb_cat <- mlb_cat[,c(1,3,4)]
colnames(mlb_cat)[3] <- 'Arm'

mlb_batters[is.na(mlb_batters)] <- 0

#############
# FINAL CLEAN
#############

mlb_oaa <- mlb_oaa_blank[,c(7,12)]
mlb_batters <- merge(mlb_batters, mlb_oaa, by="Name", all = TRUE)
mlb_batters <- merge(mlb_batters, mlb_cat, by = 'Name', all = TRUE)
mlb_batters <- merge(mlb_batters, mlb_bios, by = 'Name')
mlb_batters <- mlb_batters[c(1,13,14,6,7,8,9,10,12,15,16,17,18)]
colnames(mlb_batters)[2] <- 'Pos'
#mlb_batters$Pos[is.na(mlb_batters$Pos) & !is.na(mlb_batters$Arm)] <- 'C'
#mlb_batters$Bio.Class[!is.na(mlb_batters$Arm)] <- 'Position'
#mlb_batters <- cbind(mlb_batters, mlb_batters$Bio.Class)
mlb_batters$Class <- 'Position'
mlb_batters$Class[!is.na(mlb_batters$Arm)] <- 'Catcher'

#####################
#Add Pos Class column
#####################

mlb_batters <- mlb_batters %>%
  mutate(
    Pos.Class = case_when(
      Pos == 'CF' | Pos == 'LF' | Pos == 'RF' ~ 'OF',
      Pos == '2B' | Pos == 'SS' ~ 'MI',
      Pos == '3B' | Pos == '1B' ~ 'CI',
      Pos == 'C' ~ 'C',
      Pos == 'DH' ~ 'DH',
    )
  )

write_csv(mlb_batters, 'batters_clean.csv')


###################################################################################################################################################
# MLB PITCHERS #
###################################################################################################################################################

pit_mov <- read.csv('pitchers_raw.csv') #Fome baseball savant export csv


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
