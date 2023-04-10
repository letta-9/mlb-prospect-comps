library(baseballr)
library(dplyr)
library(plyr)
library(tidyr)
library(readr)


# MLB PITCHERS
##############

# Import and Select Data
mlb_pitchers <- fg_pitcher_leaders(x = 2022, y = 2022, qual = 100)
mlb_pitchers <- mlb_pitchers %>% select(Name, IP, wFB_C, FBall_pct, wCT_C, CT_pct, wSF_C, SF_pct, wSL_C, SL_pct,  wCB_C, CB_pct, wCH_C, CH_pct, BB_pct)


# Remove pitches that are thrown < 8 percent of the time
for (i in c(4,6,8,10,12,14)){
  mlb_pitchers[,i-1][mlb_pitchers[,i] < 8] <- NA
}
mlb_pitchers <- mlb_pitchers %>% select(Name, wFB_C, wCT_C, wSF_C, wSL_C, wCB_C, wCH_C, BB_pct)
mlb_pitchers$BB_pct <- 100 - mlb_pitchers$BB_pct

# Find FB type
# mlb_pitchers[is.na(mlb_pitchers)] <- -99
# mlb_pitchers$best_FB <- pmax(mlb_pitchers$wFB_C, mlb_pitchers$wCT_C, mlb_pitchers$wSF_C)



# Convert stats to grades

pit_std <- sd(unlist(mlb_pitchers[,2]), na.rm = TRUE)
pit_mean <- colMeans(mlb_pitchers[,2], na.rm = TRUE)
pit_breaks = c((pit_mean - (2*pit_std)),(pit_mean - pit_std), pit_mean, (pit_mean + pit_std),(pit_mean + (2*pit_std)))
p <- findInterval(unlist(mlb_pitchers[,2]), pit_breaks)
p <- factor(p)
levels(p) <- c(30, 40, 50, 60, 70, 80)
mlb_pitchers <- cbind(mlb_pitchers, p)

for (i in 3:8){
  pit_std <- sd(unlist(mlb_pitchers[,..i]), na.rm = TRUE)
  pit_mean <- colMeans(mlb_pitchers[,..i], na.rm = TRUE)
  pit_breaks = c((pit_mean - (2*pit_std)),(pit_mean - pit_std), pit_mean, (pit_mean + pit_std),(pit_mean + (2*pit_std)))
  p <- findInterval(unlist(mlb_pitchers[,..i]), pit_breaks)
  p <- factor(p)
  levels(p) <- c(30, 40, 50, 60, 70, 80)
  mlb_pitchers <- cbind(mlb_pitchers, p)
}

# Clean

mlb_pitchers <- mlb_pitchers[,c(1,9:15)]
colnames(mlb_pitchers) <- c('Name','FB', 'CT', 'SF', 'SL', 'CB', 'CH', 'CMD')





# cols <-c(2,3,4,5,6,7,8)
# 
# for (i in cols){
#   tool_std <- sd(unlist(mlb_pitchers[,..i]), na.rm = TRUE)
#   tool_mean <- colMeans(mlb_pitchers[,..i], na.rm = TRUE)
#   tool_breaks = c((tool_mean - (2*tool_std)),(tool_mean - tool_std), tool_mean, (tool_mean + tool_std),(tool_mean + (2*tool_std)))
#   g <- findInterval(unlist(mlb_pitchers[,..i]), tool_breaks)
#   g <- factor(g)
#   levels(g) <- c(30,40,50,60,70,80)
#   mlb_pitchers <- cbind(mlb_pitchers, g)
# }



















# # Convert stats to grades
# 
# pit_na <- mlb_pitchers[is.na(mlb_pitchers[,4]),]
# pit_no_na <- mlb_pitchers[!is.na(mlb_pitchers[,4]),]
# 
# pit_std <- sd(pit_no_na[,4])
# pit_mean <- mean(pit_no_na[,4])
# 
# pit_breaks = c((pit_mean - (2*pit_std)),(pit_mean - pit_std), pit_mean, (pit_mean + pit_std),(pit_mean + (2*pit_std)))
# 
# p <- findInterval(pit_no_na[,4], pit_breaks)
# p <- factor(p)
# 
# levels(p) <- c(30, 40, 50, 60, 70, 80)
# 
# pit_no_na <- cbind(pit_no_na, p)
# 
# mlb_pitchers <- merge(mlb_pitchers, p, by = 'Name')







# # Identify best fast ball
# mlb_pitchers[is.na(mlb_pitchers)] <- -100
# mlb_pitchers$FB <- pmax(mlb_pitchers$wFB_C, mlb_pitchers$wCT_C, mlb_pitchers$wSF_C)
# mlb_pitchers$FB.Type <- NA
# mlb_pitchers$FB.Type[mlb_pitchers$FB == mlb_pitchers$wFB_C] <- 'Straight' 
# mlb_pitchers$FB.Type[mlb_pitchers$FB == mlb_pitchers$wCT_C] <- 'Cut' 
# mlb_pitchers$FB.Type[mlb_pitchers$FB == mlb_pitchers$wSF_C] <- 'Sink' 






# # Imports
# 
# #CSV From https://www.fangraphs.com/leaders.aspx?pos=all&stats=sta&lg=all&qual=100&type=c,13,98,75,99,77,100,79,101,81,102,83,103,85,104,87&season=2022&month=0&season1=2022&ind=0&team=0&rost=0&age=0&filter=&players=0&startdate=2022-01-01&enddate=2022-12-31
# mlb_pitchers <- read.csv('fg_pitcher_leaders.csv') # Check back on baseballr for bug fix
# 
# colnames(mlb_pitchers)[1] <- 'Name'
# mlb_pitchers$Name <- iconv(mlb_pitchers$Name, from = "UTF-8", to = "ASCII//TRANSLIT")
# 
# mlb_pitchers$FB. <- as.numeric(mlb_pitchers)











#mlb_pitchers <- mlb_pitchers %>% filter()

# # Clean
# 
# mlb_pitchers$first_name <- sub(' ','',mlb_pitchers$first_name)
# mlb_pitchers$last_name <- sub(' ','',mlb_pitchers$last_name)
# mlb_pitchers$Name <- paste(mlb_pitchers$first_name, mlb_pitchers$last_name)
# mlb_pitchers$Name <- iconv(mlb_pitchers$Name, from = "UTF-8", to = "ASCII//TRANSLIT")
# 
# mlb_pitchers <- mlb_pitchers %>% filter(total_pitches >= 1500, pitch_per >= 0.05) %>%
#   select(Name, pitch_hand, pitch_type_name, avg_speed, diff_z, diff_x) # Max pitches thrown by reliever 
# 
# 
# 
# names(mlb_pitchers) <- c('Name', 'T', 'Pitch', 'Vel', 'vMov', 'hMov')
# mlb_pitchers <- mlb_pitchers %>%
#   mutate(
#     Pitch = case_when(
#       Pitch == '4-Seamer' ~ 'FB',
#       Pitch == 'Slider' ~ 'SL',
#       Pitch == 'Curveball' ~ 'CB',
#       Pitch == 'Sweeper' ~ 'CB',
#       Pitch == 'Slurve' ~ 'CB',
#       Pitch == 'Changeup' ~ 'CH',
#       Pitch == 'Splitter' ~ 'SPL',
#       Pitch == 'Sinker' ~ 'SNK',
#       Pitch == 'Cutter' ~ 'CUT'
#     )
#   )
# 
# 
# for (i in 4:6){
#   pit_std <- sd(mlb_pitchers[,i])
#   pit_mean <- mean(mlb_pitchers[,i])
# 
#   pit_breaks = c((pit_mean - (2*pit_std)),(pit_mean - pit_std), pit_mean, (pit_mean + pit_std),(pit_mean + (2*pit_std)))
# 
#   p <- findInterval(mlb_pitchers[,i], pit_breaks)
#   p <- factor(p)
# 
#   levels(p) <- c(30, 40, 50, 60, 70, 80)
# 
#   mlb_pitchers <- cbind(mlb_pitchers, p)
# 
# }


# names(mlb_pitchers) <- c('Name', 'T', 'Pitch', 'Vel', 'vMov', 'hMov', 'gVel','gVMov','gHMov')
# mlb_pitchers <- mlb_pitchers %>% select(c(1,2,3,7,8,9))
# mlb_pitchers$gVel <- (as.integer(mlb_pitchers$gVel) + 2) * 10
# mlb_pitchers$gHMov <- (as.integer(mlb_pitchers$gHMov) + 2) * 10
# mlb_pitchers$gVMov <- (as.integer(mlb_pitchers$gVMov) + 2) * 10
# 
# mlb_pitchers$Grade <- ifelse(mlb_pitchers$Pitch == 'FB', (mlb_pitchers$gVel + ((mlb_pitchers$gVMov + mlb_pitchers$gHMov)/2))/2, NA)
# mlb_pitchers$Grade <- ifelse(mlb_pitchers$Pitch == 'CH', (mlb_pitchers$gVMov + mlb_pitchers$gHMov)/2, mlb_pitchers$Grade)
# mlb_pitchers$Grade <- ifelse(mlb_pitchers$Pitch == 'SL', (mlb_pitchers$gVel + mlb_pitchers$gVMov + mlb_pitchers$gHMov)/3, mlb_pitchers$Grade)
# mlb_pitchers$Grade <- ifelse(mlb_pitchers$Pitch == 'CB', (mlb_pitchers$gVMov + mlb_pitchers$gHMov)/2, mlb_pitchers$Grade)
# mlb_pitchers$Grade <- ifelse(mlb_pitchers$Pitch == 'CUT', (mlb_pitchers$gVel + mlb_pitchers$gVMov + mlb_pitchers$gHMov)/3, mlb_pitchers$Grade)
# mlb_pitchers$Grade <- ifelse(mlb_pitchers$Pitch == 'SPL', (mlb_pitchers$gVel + mlb_pitchers$gVMov + mlb_pitchers$gHMov)/3, mlb_pitchers$Grade)
# mlb_pitchers$Grade <- ifelse(mlb_pitchers$Pitch == 'SNK', (mlb_pitchers$gVel + mlb_pitchers$gVMov + mlb_pitchers$gHMov)/3, mlb_pitchers$Grade)
# 
# mlb_pitchers$Grade <- round_any(mlb_pitchers$Grade, 10)
# mlb_pitchers <- mlb_pitchers %>% select(c(1,2,3,7))
# 
# mlb_pitchers <- reshape(mlb_pitchers, timevar = 'Pitch', idvar = c('Name','T'), direction = 'wide')
# names(mlb_pitchers) <- c('Name', 'T', 'CUT', 'FB', 'SNK', 'SL', 'CB','CH','SPL')
# mlb_pitchers[is.na(mlb_pitchers)] <- 0
# mlb_pitchers$Class <- 'Pitcher'
# 
# mlb_pitchers$FB <- pmax(mlb_pitchers$CUT, mlb_pitchers$FB, mlb_pitchers$SNK)
# 
# 
# mlb_pitchers <- merge(mlb_pitchers, mlb_bios, by='Name')
# 
# mlb_pitchers <- mlb_pitchers[c(1,4,6,7,8,2,10,15,16)]
# 
# colnames(mlb_pitchers)[6] <- 'T'
# 
# #####################
# #Add CMD column
# #####################
# 
# CMD_raw <- mlb_stats(stat_type = 'season', player_pool = 'all', stat_group = 'pitching', season = '2022')
# CMD_raw <- CMD_raw %>% filter(number_of_pitches >= 250) %>% select(player_full_name, walks_per9inn)
# 
# names(CMD_raw) <- c('Name','BB')
# 
# CMD_raw$BB <- as.numeric(CMD_raw$BB)
# CMD_raw$BB <- 100 - CMD_raw$BB
# 
# cmd_std <- sd(CMD_raw$BB)
# cmd_mean <- mean(CMD_raw$BB)
# cmd_breaks = c((cmd_mean - (2*cmd_std)),(cmd_mean - cmd_std), cmd_mean, (cmd_mean + cmd_std),(cmd_mean + (2*cmd_std)))
# CMD <- findInterval(CMD_raw$BB, cmd_breaks)
# CMD <- factor(CMD)
# levels(CMD) <- c(30,40,50,60,70,80)
# 
# CMD_raw <- cbind(CMD_raw, CMD)
# CMD_raw <- CMD_raw[,c(1,3)]
# 
# mlb_pitchers <- merge(mlb_pitchers, CMD_raw, by = 'Name')
# mlb_pitchers <- mlb_pitchers %>% relocate(CMD, .before = T)
# mlb_pitchers$CMD <- (as.numeric(mlb_pitchers$CMD) + 2) * 10
# 
# #####################
# #Add Achetype column
# #####################
# 
# mlb_pitchers$Arch <- NA
# 
# mlb_pitchers <- mlb_pitchers %>%
#   mutate(
#     Arch = case_when(
#       FB >= 70 & CMD >= 40  ~ 'VELO',
#       FB < 70 & CMD >= 60  ~ 'CTRL',
#       FB < 70 & (SL >= 70 | CB >= 70 | CH >= 70)  ~ 'BREAK',
#       FB < 70 & CMD < 40 ~ 'B-WLD',
#       FB >= 70 & CMD < 40 ~ 'V-WLD',
#       is.na(Arch) ~ 'BLNC',
#     )
#   )
# 
# 
# 
# write_csv(mlb_pitchers, 'pitchers_clean.csv')