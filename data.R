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



#########################
# CREATE MLB BIOS TABLE #
#########################


mlb_bios <- mlb_sports_players(sport_id = 1, season = 2022)
mlb_bios$weight <- plyr::round_any(mlb_bios$weight, 5, f=ceiling)
mlb_bios$primary_position_type <- ifelse(mlb_bios$primary_position_type != 'Pitcher', 'Position', mlb_bios$primary_position_type)
mlb_bios <- mlb_bios %>% select(full_name, primary_position_abbreviation, primary_position_type, height, weight, bat_side_code, pitch_hand_code)
names(mlb_bios) <- c('Name', 'Pos', 'Class','H','W','B','T')


############################
# CREATE MLB BATTERS TABLE #
############################


mlb_batters <- mlb_stats(stat_type = 'season', player_pool = 'all', stat_group = 'hitting', season = '2022')
mlb_batters <- mlb_batters %>% filter(plate_appearances > 200)

mlb_ev <- statcast_leaderboards(leaderboard = "exit_velocity_barrels", year = 2022, min_pa = 100)
mlb_ev$player_full_name <- paste(mlb_ev$first_name, mlb_ev$last_name)

mlb_oaa <- statcast_leaderboards(leaderboard = "outs_above_average", year = 2022, min_field = 50)
mlb_oaa$player_full_name <- paste(mlb_oaa$first_name, mlb_oaa$last_name)

mlb_spd <- statcast_leaderboards(leaderboard = "sprint_speed", year = 2022)
mlb_spd$player_full_name <- paste(mlb_spd$first_name, mlb_spd$last_name)

mlb_cat <- statcast_leaderboards(leaderboard = "pop_time", year = 2022, min_field = "q")
mlb_cat <- mlb_cat %>% select(catcher,maxeff_arm_2b_3b_sba)
mlb_cat$Pos <- 'C'
colnames(mlb_cat)[1:2] <- c('Name','Arm')
mlb_cat$Name <- iconv(mlb_cat$Name, from = "UTF-8", to = "ASCII//TRANSLIT")

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
mlb_batters$at_bats_per_home_run <- as.numeric(mlb_batters$at_bats_per_home_run)

games <- mlb_batters[c(41,5)]
games <- aggregate(games$games_played, by=list(Name=games$player_full_name), FUN=sum)

all_positions <- mlb_bios[,1:2]

mlb_batters <- aggregate(mlb_batters, list(Name = mlb_batters$player_full_name), mean)
mlb_batters <- mlb_batters %>% select(Name, avg, at_bats_per_home_run)
mlb_batters <- subset(mlb_batters, !is.na(at_bats_per_home_run))

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

 if (i == 3){
   levels(g) <- c("80","70","60","50","40","30")
 } else {
   levels(g) <- c("30","40","50","60","70","80")
 }

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
 levels(Fld) <- c("30","40","50","60","70","80")

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

#############
# FINAL CLEAN
#############

mlb_oaa <- mlb_oaa_blank[,c(7,12)]
mlb_batters <- merge(mlb_batters, mlb_oaa, by="Name", all = TRUE)
mlb_batters <- merge(mlb_batters, mlb_bios, by = 'Name')
mlb_batters <- merge(mlb_batters, mlb_cat, by = 'Name', all = TRUE)
mlb_batters <- mlb_batters[c(1,6,7,8,9,10,11,12,15,16,18)]
mlb_batters <- mlb_batters %>% relocate(Pos.x, .before = Hit)
mlb_batters <- mlb_batters %>% relocate(Class, .before = Hit)
mlb_batters <- mlb_batters %>% relocate(Arm, .before = B)
colnames(mlb_batters)[2] <- 'Pos'



write_csv(mlb_batters, 'C:/Users/mattb/Documents/PROJECTS/PROSPECT COMPS/Shiny/MLB Prospect Comps/mlb_batters.csv')


