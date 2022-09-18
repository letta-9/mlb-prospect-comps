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

write_csv(mlb_bios, 'mlb_bios_test.csv')



############################
# CREATE MLB BATTERS TABLE #
############################


mlb_batters <- mlb_stats(stat_type = 'season', player_pool = 'all', stat_group = 'hitting', season = '2022')
mlb_batters <- mlb_batters %>% filter(plate_appearances > 200)

mlb_ev <- statcast_leaderboards(leaderboard = "exit_velocity_barrels", year = 2022, min_pa = 200)
mlb_ev$player_full_name <- paste(mlb_ev$first_name, mlb_ev$last_name)

mlb_oaa <- statcast_leaderboards(leaderboard = "outs_above_average", year = 2022, min_field = 'q')
mlb_oaa$player_full_name <- paste(mlb_oaa$first_name, mlb_oaa$last_name)

mlb_spd <- statcast_leaderboards(leaderboard = "sprint_speed", year = 2022, min_pa = 200)
mlb_spd$player_full_name <- paste(mlb_spd$first_name, mlb_spd$last_name)

years <- c('2021','2020','2019')

for (b in years){
  w <- mlb_stats(stat_type = 'season', player_pool = 'all', stat_group = 'hitting', season = b)
  w <- w %>% filter(plate_appearances > 200)
  mlb_batters <- rbind(mlb_batters, w)
  
  x <- statcast_leaderboards(leaderboard = "exit_velocity_barrels", year = b, min_pa = 200)
  x$player_full_name <- paste(x$first_name, x$last_name)
  mlb_ev <- rbind(mlb_ev, x)
  
  y <- statcast_leaderboards(leaderboard = "outs_above_average", year = b, min_field = 'q')
  y$player_full_name <- paste(y$first_name, y$last_name)
  mlb_oaa <- rbind(mlb_oaa, y)
  
  z <- statcast_leaderboards(leaderboard = "sprint_speed", year = b, min_pa = 200)
  z$player_full_name <- paste(z$first_name, z$last_name)
  mlb_spd <- rbind(mlb_spd, z)
  
}


mlb_batters <- mlb_batters %>% group_by(player_full_name) %>% filter(n()>2)
mlb_batters$avg <- as.numeric(mlb_batters$avg)
mlb_batters$at_bats_per_home_run <- as.numeric(mlb_batters$at_bats_per_home_run)
mlb_batters <- aggregate(mlb_batters, list(Name = mlb_batters$player_full_name), mean)
mlb_batters <- mlb_batters %>% select(Name, avg, at_bats_per_home_run)
mlb_batters <- subset(mlb_batters, !is.na(at_bats_per_home_run))

mlb_ev <- mlb_ev %>% group_by(player_full_name) %>% filter(n()>2)
mlb_ev <- aggregate(mlb_ev, list(Name = mlb_ev$player_full_name), mean)
mlb_ev <- mlb_ev %>% select(Name, avg_hit_speed)

mlb_oaa <- mlb_oaa %>% group_by(player_full_name) %>% filter(n()>2)
mlb_oaa <- aggregate(mlb_oaa, list(Name = mlb_oaa$player_full_name), mean)
mlb_oaa <- mlb_oaa %>% select(Name, outs_above_average)

mlb_spd <- mlb_spd %>% group_by(player_full_name) %>% filter(n()>2)
mlb_spd <- aggregate(mlb_spd, list(Name = mlb_spd$player_full_name), mean)
mlb_spd <- mlb_spd %>% select(Name, sprint_speed)


mlb_batters <- merge(mlb_batters, mlb_ev, by = 'Name')
mlb_batters <- merge(mlb_batters, mlb_spd, by = 'Name')
mlb_batters <- merge(mlb_batters, mlb_oaa, by = 'Name')


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

mlb_batters <- mlb_batters %>% dplyr::rename('Hit' = 7,
                                         'Game' = 8,
                                         'Raw' = 9,
                                         'Spd' = 10)


write_csv(mlb_batters, 'mlb_batters_test.csv')





# # CLEAN FIELDING STATS #
# 
# fielders <- fielders %>% select(last_name, first_name, primary_pos_formatted, outs_above_average)
# fielders$Name <- paste(fielders$first_name, fielders$last_name)
# fielders <- fielders[c(3,4,5)]
# fielders <- fielders %>% relocate(Name, .before = primary_pos_formatted)
# names(fielders) = c("Name","Pos","OAA")
# fielders$Pos <- gsub(" ","", fielders$Pos)
# fielders$OAA <- gsub(" ","", fielders$OAA)
# fielders$Name <- iconv(fielders$Name, from = "UTF-8", to = "ASCII//TRANSLIT")
# 
# 
# 
# fielders <- merge(fielders, games, by = "Name")
# fielders$OAA <- as.numeric(fielders$OAA)
# fielders$OAA162 <- (fielders$OAA / fielders$x) * 162
# 
# 
# for (i in 2:5){
#   n <- c('Hit', 'Game', 'Raw', 'Speed')
#   
#   
#   tool_std <- sd(batters[,i])
#   tool_mean <- mean(batters[,i])
#   
#   tool_breaks = c((tool_mean - (2*tool_std)),(tool_mean - tool_std), tool_mean, (tool_mean + tool_std),(tool_mean + (2*tool_std)))
#   
#   g <- findInterval(batters[,i], tool_breaks)
#   g <- factor(g)
#   levels(g) <- c("30","40","50","60","70","80")
#   
#   batters <- cbind(batters, g)
#   
# }
# 
# names(batters) <- c("Name", "G", "AVG","HR","EV","SPD","Hit","Game","Raw","Spd")
# 
# 
# positions <- c("1B", "2B", "SS", "3B", "LF", "CF", "RF")
# fielders_blank <- data.frame(matrix(ncol=6, nrow=0))
# 
# for (j in positions) {
#   x <- fielders %>% filter(Pos == j)
#   fld_mean <- mean(x$OAA162)
#   fld_std <- sd(x$OAA162)
#   fld_breaks = c((fld_mean - (2*fld_std)),(fld_mean - fld_std), fld_mean, (fld_mean + fld_std),(fld_mean + (2*fld_std)))
#   
#   Fld <- findInterval(x$OAA162, fld_breaks)
#   Fld <- factor(Fld)
#   levels(Fld) <- c("30","40","50","60","70","80")
#   
#   x <- cbind(x, Fld)
#   
#   fielders_blank <- rbindlist(list(fielders_blank,x), fill = TRUE)
#   
# }
# 
# fielders <- fielders_blank[,c(7,8,12)]
# batters <- merge(batters, fielders, by="Name")
# names(batters) <- c("Name","G","AVG","HR","EV","SPD","Hit","Game","Raw","Spd","Pos","Fld")
# batters <- batters %>% relocate(Pos, .before = G)
# 
# 
# 
# 
# pros_hit_comp <- prospects %>% select(Name, Hit)
# mlb_hit_comp <- batters %>% select(Name, Hit)
# hit_comp <- merge(pros_hit_comp, mlb_hit_comp, by = 'Hit', all=TRUE)
# 
# pros_game_comp <- prospects %>% select(Name, Game)
# mlb_game_comp <- batters %>% select(Name, Game)
# game_comp <- merge(pros_game_comp, mlb_game_comp, by = 'Game', all=TRUE)
# 
# pros_raw_comp <- prospects %>% select(Name, Raw)
# mlb_raw_comp <- batters %>% select(Name, Raw)
# raw_comp <- merge(pros_raw_comp, mlb_raw_comp, by = 'Raw', all=TRUE)
# 
# pros_spd_comp <- prospects %>% select(Name, Spd)
# mlb_spd_comp <- batters %>% select(Name, Spd)
# spd_comp <- merge(pros_spd_comp, mlb_spd_comp, by = 'Spd', all=TRUE)
# 
# pros_fld_comp <- prospects %>% select(Name, Pos, Fld)
# mlb_fld_comp <- batters %>% select(Name, Pos, Fld)
# fld_comp <- merge(pros_fld_comp, mlb_fld_comp, by = c('Pos','Fld'), all=TRUE)








