library(baseballr)
library(dplyr)
library(tidyr)
library(readr)
# library(glue)
# library(plyr)
# library(stringi)
# library(data.table)
# library(XML)


################
# MLB BATTERS #
################

# HIT : AVG
# PWR : HR
# RAW : MAX EV
# FLD : OAA, POP TIME
# SPD : SPRINT SPEED
# BAT_CTRL: CONTACT PERCENTAGE
# DISC : O SWING PERCENTAGE


mlb_hit_pwr <- data.frame()
mlb_fld <- data.frame()
mlb_spd <- data.frame()
mlb_raw <- data.frame()
# mlb_arm <- data.frame()


##################################################################
# STATS FOR GRADES -> HIT, PWR, FLD, SPD, RAW AVERAGED OVER 2 YEARS
##################################################################


years <- c('2022','2021')

for (i in years){
  v <- mlb_stats(stat_type = 'season', player_pool = 'all', stat_group = 'hitting', season = i)
  v <- v %>% filter(plate_appearances > 340)
  mlb_hit_pwr <- rbind(mlb_hit_pwr, v)
  
  w <- statcast_leaderboards(leaderboard = "outs_above_average", year = i, min_field = 50)
  w$player_full_name <- paste(w$first_name, w$last_name)
  mlb_fld <- rbind(mlb_fld, w)

}

mlb_raw <- statcast_leaderboards(leaderboard = "exit_velocity_barrels", year = 2022, min_pa = 25)
mlb_raw$Name <- paste(mlb_raw$first_name, mlb_raw$last_name)

mlb_disc_ctrl <- fg_batter_leaders(2021, 2022, league = 'all', qual = 340, ind = 0, exc_p = TRUE)

mlb_cat <- statcast_leaderboards(leaderboard = "pop_time", year = 2022, min_field = "50")
colnames(mlb_cat)[2] <- 'Name'

mlb_spd <- statcast_leaderboards(leaderboard = "sprint_speed", year = 2022)
mlb_spd$Name <- paste(mlb_spd$first_name, mlb_spd$last_name)


###################################
# AVERAGE AND SELECT RELEVANT STATS
###################################


mlb_hit_pwr$avg <- as.numeric(mlb_hit_pwr$avg)
mlb_hit_pwr$at_bats_per_home_run <- as.numeric(mlb_hit_pwr$at_bats_per_home_run)
mlb_hit_pwr <- aggregate(mlb_hit_pwr, list(Name = mlb_hit_pwr$player_full_name), mean)
mlb_hit_pwr$Name <- iconv(mlb_hit_pwr$Name, from = "UTF-8", to = "ASCII//TRANSLIT")

mlb_hit <- mlb_hit_pwr %>% select(Name, avg)

mlb_pwr <- mlb_hit_pwr %>% select(Name, at_bats_per_home_run)
mlb_pwr$at_bats_per_home_run[is.na(mlb_pwr$at_bats_per_home_run)] <- 500

mlb_raw <- mlb_raw %>% select(Name, max_hit_speed)
mlb_raw$Name <- iconv(mlb_raw$Name, from = "UTF-8", to = "ASCII//TRANSLIT")

mlb_disc_ctrl <- mlb_disc_ctrl %>% select(Name, `O-Swing_pct`,Contact_pct)
mlb_disc_ctrl$Name <- iconv(mlb_disc_ctrl$Name, from = "UTF-8", to = "ASCII//TRANSLIT")

mlb_fld <- aggregate(mlb_fld, list(Name = mlb_fld$player_full_name), mean)
mlb_fld <- mlb_fld %>% select(Name, outs_above_average)
mlb_fld$Name <- iconv(mlb_fld$Name, from = "UTF-8", to = "ASCII//TRANSLIT")

mlb_cat <- mlb_cat %>% select(Name,pop_2b_sba, maxeff_arm_2b_3b_sba)
mlb_cat$Name <- iconv(mlb_cat$Name, from = "UTF-8", to = "ASCII//TRANSLIT")

mlb_spd <- mlb_spd %>% select(Name, sprint_speed)
mlb_spd$Name <- iconv(mlb_spd$Name, from = "UTF-8", to = "ASCII//TRANSLIT")


#############
# FINAL CLEAN
#############

# Merge all dataframes into one
mlb_batters <- merge(mlb_hit, mlb_pwr, by="Name", all = TRUE)
mlb_batters <- merge(mlb_batters, mlb_raw, by="Name", all = TRUE)
mlb_batters <- merge(mlb_batters, mlb_disc_ctrl, by = 'Name', all = TRUE)
mlb_batters <- merge(mlb_batters, mlb_fld, by = 'Name', all = TRUE)
mlb_batters <- merge(mlb_batters, mlb_cat, by = 'Name', all = TRUE)
mlb_batters <- merge(mlb_batters, mlb_spd, by = 'Name', all = TRUE)



# Remove NAs from columns
mlb_batters <- mlb_batters %>% tidyr::drop_na(c(avg, max_hit_speed, Contact_pct, sprint_speed))

# Temp make Catchers OAA 0.0
mlb_batters$outs_above_average[!is.na(mlb_batters$pop_2b_sba) & is.na(mlb_batters$outs_above_average)] <- 0

# Make DHs OAA 0.0
mlb_batters$outs_above_average[is.na(mlb_batters$pop_2b_sba) & is.na(mlb_batters$outs_above_average)] <- 0
 
# Grades for stats that larger numbers are better
for (i in c(2,4,6,7,10)){
  tool_std <- sd(mlb_batters[,i])
  tool_mean <- mean(mlb_batters[,i])
  tool_breaks = c((tool_mean - (2*tool_std)),(tool_mean - tool_std), tool_mean, (tool_mean + tool_std),(tool_mean + (2*tool_std)))
  g <- findInterval(mlb_batters[,i], tool_breaks)
  g <- factor(g)
  levels(g) <- c(30,40,50,60,70,80)
  mlb_batters <- cbind(mlb_batters, g)
}

mlb_batters <- mlb_batters %>% 
    dplyr::rename(HIT = 11, RAW = 12, BAT_CTRL = 13, FLD = 14, SPD = 15)

# Grades for Discipline
for (i in c(5)){
  tool_std <- sd(mlb_batters[,i])
  tool_mean <- mean(mlb_batters[,i])
  tool_breaks = c((tool_mean - (2*tool_std)),(tool_mean - tool_std), tool_mean, (tool_mean + tool_std),(tool_mean + (2*tool_std)))
  g <- findInterval(mlb_batters[,i], tool_breaks)
  g <- factor(g)
  levels(g) <- c(80,70,60,50,40,30)
  mlb_batters <- cbind(mlb_batters, g)
  
}

# Grades for Power, Take out outliers

pwr_outliers <- boxplot.stats(mlb_batters[,3])$stats[5]
mlb_batters_in <- mlb_batters %>% filter(at_bats_per_home_run <= pwr_outliers)
mlb_batters_out <- mlb_batters %>% filter(at_bats_per_home_run > pwr_outliers)



# Grades for Power
for (i in c(3)){
  tool_std <- sd(mlb_batters_in[,i])
  tool_mean <- mean(mlb_batters_in[,i])
  tool_breaks = c((tool_mean - (2*tool_std)),(tool_mean - tool_std), tool_mean, (tool_mean + tool_std),(tool_mean + (2*tool_std)))
  g <- findInterval(mlb_batters_in[,i], tool_breaks)
  g <- factor(g)
  levels(g) <- c(70,60,50,40,30)
  mlb_batters_in <- cbind(mlb_batters_in, g)
}

mlb_batters_in <- mlb_batters_in %>% dplyr::rename(DISC = 16, PWR = 17)

mlb_batters_out$PWR <- 20
mlb_batters_out <- mlb_batters_out %>% dplyr::rename(DISC = 16, PWR = 17)

mlb_batters_out$PWR <- as.factor(mlb_batters_out$PWR)


mlb_batters <- rbind(mlb_batters_in, mlb_batters_out)

# Grades for Catcher Fielding

mlb_catchers <- mlb_batters %>% filter(!is.na(pop_2b_sba))

for (i in c(8)){
  tool_std <- sd(mlb_catchers[,i])
  tool_mean <- mean(mlb_catchers[,i])
  tool_breaks = c((tool_mean - (2*tool_std)),(tool_mean - tool_std), tool_mean, (tool_mean + tool_std),(tool_mean + (2*tool_std)))
  g <- findInterval(mlb_catchers[,i], tool_breaks)
  g <- factor(g)
  levels(g) <- c(80,70,60,50,40,30)
  mlb_catchers <- cbind(mlb_catchers, g)
}

mlb_catchers <- mlb_catchers %>% dplyr::rename(cFLD = 18)

mlb_batters_out_cat <- mlb_batters %>% filter(is.na(pop_2b_sba))
mlb_batters_out_cat$cFLD <- NA

mlb_batters <- rbind(mlb_batters_out_cat, mlb_catchers)

# Swap cFLD data in FLD
mlb_batters <- mlb_batters %>% mutate(cFLD = coalesce(cFLD, FLD))


mlb_batters <- mlb_batters[c(1,11,17,12,13,16,18,15)]
mlb_batters <- mlb_batters %>% dplyr::rename(FLD = 7)


for (i in 2:8){
  mlb_batters[,i] <- as.numeric(as.character(mlb_batters[,i]))
}

mlb_batters$Arch <- NA

mlb_batters <- mlb_batters %>%
  mutate(
    Arch = case_when(
      PWR >= 60 & (PWR - HIT) >= 20  ~ 'PWR',
      (HIT - PWR) >= 20  ~ 'CON',
      FLD > HIT & FLD > PWR & FLD > RAW & HIT < 50 & PWR < 50 ~ 'FLD',
      SPD >= 70 & HIT <= 50 & PWR <= 50 ~ 'SPD',
      HIT >= 70 & PWR >= 70 & FLD >= 60 & SPD >= 60 ~ '5-T',
      is.na(Arch) ~ 'BLNC'
    )
  )

mlb_batters <- mlb_batters %>% relocate(Arch, .before = HIT)

for (i in 3:9){
  mlb_batters[,i] <- as.character(mlb_batters[,i])
}

write_csv(mlb_batters, 'app/mlb_batters_clean.csv')
