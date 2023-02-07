library(baseballr)
library(dplyr)

###################################################################################################################################################
# MLB BIOS #
###################################################################################################################################################

mlb_bios <- mlb_sports_players(sport_id = 1, season = 2022) #BASEBALL R

mlb_bios$primary_position_type <- ifelse(mlb_bios$primary_position_type != 'Pitcher', 'Position', mlb_bios$primary_position_type)
mlb_bios <- mlb_bios %>% select(full_name, primary_position_type, primary_position_abbreviation,  bat_side_code, pitch_hand_code, weight, height, current_age, birth_country)
names(mlb_bios) <- c('Name', 'Bio.Class', 'Pos', 'B', 'T', 'W', 'H', 'Age', 'Born')
mlb_bios <- cbind(mlb_bios, mlb_bios$H)
mlb_bios <- mlb_bios %>% separate(H, c('Ft','In'), "' ")
mlb_bios$In <- sub('"','',mlb_bios$In)
mlb_bios$Ft <- as.numeric(mlb_bios$Ft)
mlb_bios$In <- as.numeric(mlb_bios$In)
mlb_bios$H <- (mlb_bios$Ft * 12) + mlb_bios$In
mlb_bios <- mlb_bios %>% select('Name', 'Bio.Class', 'Pos', 'B', 'T', 'V2', 'H', 'W','Age', 'Born')
names(mlb_bios) <- c('Name', 'Bio.Class', 'Pos', 'B', 'T', 'H_F_I', 'H', 'W','Age', 'Born')
mlb_bios <- data.frame(mlb_bios)

#CONVERT TO 20-80 SCALE
for (i in 7:8){
  body_std <- sd(mlb_bios[,i])
  body_mean <- mean(mlb_bios[,i])
  body_breaks = c((body_mean - (2*body_std)),(body_mean - body_std), body_mean, (body_mean + body_std),(body_mean + (2*body_std)))
  t <- findInterval(mlb_bios[,i], body_breaks)
  t <- factor(t)
  levels(t) <- c(30,40,50,60,70,80)
  mlb_bios <- cbind(mlb_bios, t)
}

names(mlb_bios) <- c('Name', 'Bio.Class', 'Pos', 'Bats', 'Throws', 'Height', 'Inches', 'Weight','Age', 'Born', 'H', 'W')
mlb_bios <- mlb_bios %>% select('Name', 'Bio.Class', 'Pos', 'Bats', 'Throws', 'Height', 'H', 'Weight', 'W','Age', 'Born')