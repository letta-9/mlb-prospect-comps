library(baseballr)
library(dplyr)
library(plyr)
library(tidyr)
library(readr)



prospects <- read_csv("prospects_clean.csv") #From fangraphs THE BOARD / mlb.com
mlb_bios <- read_csv("mlb_bios_clean.csv") #From data.R
batters <- read_csv("batters_clean.csv") #From data.R
pitchers <- read.csv('pitchers_clean.csv') #Fome baseball savant export csv

# #######################
# # CLEAN PROSPECT DATA #
# #######################
# 
# prospects$Age <- floor(prospects$Age)
# 
# prospects$W <- plyr::round_any(prospects$W, 5, f=ceiling)
# prospects$Class <- ifelse(prospects$Pos == 'SP' | prospects$Pos == 'MIRP'| prospects$Pos == 'SIRP', 'Pitcher', 'Position')
# 
# tool_list <- c('Hit','Game','Raw','Spd','Fld','FB','SL','CB','CH','CMD')
# for (a in tool_list){
#   prospects <- prospects %>% separate(a, c(paste('c',a),a), " / ")
# }
# 
# 
# prospects$Hit <-as.numeric(prospects$Hit)
# prospects$Hit <- plyr::round_any(prospects$Hit, 10, f = ceiling)
# prospects$Game <-as.numeric(prospects$Game)
# prospects$Game <- plyr::round_any(prospects$Game, 10, f = ceiling)
# prospects$Raw <-as.numeric(prospects$Raw)
# prospects$Raw <- plyr::round_any(prospects$Raw, 10, f = ceiling)
# prospects$Spd <-as.numeric(prospects$Spd)
# prospects$Spd <- plyr::round_any(prospects$Spd, 10, f = ceiling)
# prospects$Fld <-as.numeric(prospects$Fld)
# prospects$Fld <- plyr::round_any(prospects$Fld, 10, f = ceiling)
# prospects$FB <-as.numeric(prospects$FB)
# prospects$FB <- plyr::round_any(prospects$FB, 10, f = ceiling)
# prospects$SL <-as.numeric(prospects$SL)
# prospects$SL <- plyr::round_any(prospects$SL, 10, f = ceiling)
# prospects$CB <-as.numeric(prospects$CB)
# prospects$CB <- plyr::round_any(prospects$CB, 10, f = ceiling)
# prospects$CH <-as.numeric(prospects$CH)
# prospects$CH <- plyr::round_any(prospects$CH, 10, f = ceiling)
# prospects$CMD <-as.numeric(prospects$CMD)
# prospects$CMD <- plyr::round_any(prospects$CMD, 10, f = ceiling)
# 
# prospects <- merge(prospects, pros_cat, by='Name')
# prospects$Class[prospects$Pos.x == 'C'] <- 'Catcher'
# 
# prospects <- prospects %>% select(c(1,3,39,8,9,11,15,17,19,21,45,24,26,28,30,32))
# colnames(prospects)[2] <- 'Pos'
# 
# 
# #############################
# # CLEAN MLB PITCHER DATA #
# #############################
# 
# pit_mov$Name <- paste(pit_mov$first_name, pit_mov$last_name)
# pit_mov$Name <- iconv(pit_mov$Name, from = "UTF-8", to = "ASCII//TRANSLIT")
# pit_mov <- pit_mov %>% select(c(25, 7, 14, 8, 17, 21))
# names(pit_mov) <- c('Name', 'T', 'Pitch', 'Vel', 'vMov', 'hMov')
# pit_mov$Pitch[pit_mov$Pitch == '4-Seamer'] <- 'FB'
# pit_mov$Pitch[pit_mov$Pitch == 'Slider'] <- 'SL'
# pit_mov$Pitch[pit_mov$Pitch == 'Curveball'] <- 'CB'
# pit_mov$Pitch[pit_mov$Pitch == 'Changeup'] <- 'CH'
# pit_mov$Pitch[pit_mov$Pitch == 'Splitter'] <- 'SPL'
# pit_mov$Pitch[pit_mov$Pitch == 'Sinker'] <- 'SNK'
# pit_mov$Pitch[pit_mov$Pitch == 'Cutter'] <- 'CUT'
# 
# 
# 
# 
# for (i in 4:6){
#   pit_std <- sd(pit_mov[,i])
#   pit_mean <- mean(pit_mov[,i])
# 
#   pit_breaks = c((pit_mean - (2*pit_std)),(pit_mean - pit_std), pit_mean, (pit_mean + pit_std),(pit_mean + (2*pit_std)))
# 
#   p <- findInterval(pit_mov[,i], pit_breaks)
#   p <- factor(p)
# 
#   levels(p) <- c(30, 40, 50, 60, 70, 80)
# 
#   pit_mov <- cbind(pit_mov, p)
# 
# }
# 
# names(pit_mov) <- c('Name', 'T', 'Pitch', 'Vel', 'vMov', 'hMov', 'gVel','gVMov','gHMov')
# pit_mov <- pit_mov %>% select(c(1,2,3,7,8,9))
# pit_mov$gVel <- (as.integer(pit_mov$gVel) + 2) * 10
# pit_mov$gHMov <- (as.integer(pit_mov$gHMov) + 2) * 10
# pit_mov$gVMov <- (as.integer(pit_mov$gVMov) + 2) * 10
# 
# pit_mov$Grade <- ifelse(pit_mov$Pitch == 'FB', (pit_mov$gVel + ((pit_mov$gVMov + pit_mov$gHMov)/2))/2, NA)
# pit_mov$Grade <- ifelse(pit_mov$Pitch == 'CH', (pit_mov$gVMov + pit_mov$gHMov)/2, pit_mov$Grade)
# pit_mov$Grade <- ifelse(pit_mov$Pitch == 'SL', (pit_mov$gVel + pit_mov$gVMov + pit_mov$gHMov)/3, pit_mov$Grade)
# pit_mov$Grade <- ifelse(pit_mov$Pitch == 'CB', (pit_mov$gVMov + pit_mov$gHMov)/2, pit_mov$Grade)
# pit_mov$Grade <- ifelse(pit_mov$Pitch == 'CUT', (pit_mov$gVel + pit_mov$gVMov + pit_mov$gHMov)/3, pit_mov$Grade)
# pit_mov$Grade <- ifelse(pit_mov$Pitch == 'SPL', (pit_mov$gVel + pit_mov$gVMov + pit_mov$gHMov)/3, pit_mov$Grade)
# pit_mov$Grade <- ifelse(pit_mov$Pitch == 'SNK', (pit_mov$gVel + pit_mov$gVMov + pit_mov$gHMov)/3, pit_mov$Grade)
# 
# pit_mov$Grade <- round_any(pit_mov$Grade, 10)
# pit_mov <- pit_mov %>% select(c(1,2,3,7))
# 
# pit_mov <- reshape(pit_mov, timevar = 'Pitch', idvar = c('Name','T'), direction = 'wide')
# names(pit_mov) <- c('Name', 'T', 'CUT', 'FB', 'SNK', 'SL', 'CB','CH','SPL')
# pit_mov[is.na(pit_mov)] <- 0


###################################
# CREATE MODAL DISPLAY FOR SERVER #
###################################


modal <- data.frame(matrix(ncol=13, nrow=1))
colnames(modal) <- c('Name', 'Body', 'Hit', 'Game', 'Raw', 'Spd', 'Fld', 'Arm', 'FB', 'SL', 'CB', 'CH','CMD')

selected <- "Corbin Carrol"


selected_data <- prospects %>% filter(Name == selected)

modal$Name <- selected

if (selected_data$Class == 'Position'){
  modal$Hit <- dplyr::select(sample_n(merge(selected_data, batters, by=c('Hit','B')),1), Name.y)
  modal$Game <- dplyr::select(sample_n(merge(selected_data, batters, by=c('Game','B')),1), Name.y)
  modal$Raw <- dplyr::select(sample_n(merge(selected_data, batters, by=c('Raw','B')),1), Name.y)
  modal$Spd <- dplyr::select(sample_n(merge(selected_data, batters, by=('Spd')),1), Name.y)
  modal$Fld <- dplyr::select(sample_n(merge(selected_data, batters, by=c('Pos','Fld')),1), Name.y)
} 

if (selected_data$Class == 'Catcher'){
  modal$Hit <- dplyr::select(sample_n(merge(selected_data, batters, by=c('Hit','B')),1), Name.y)
  modal$Game <- dplyr::select(sample_n(merge(selected_data, batters, by=c('Game','B')),1), Name.y)
  modal$Raw <- dplyr::select(sample_n(merge(selected_data, batters, by=c('Raw','B')),1), Name.y)
  modal$Spd <- dplyr::select(sample_n(merge(selected_data, batters, by=('Spd')),1), Name.y)
  modal$Arm <- dplyr::select(sample_n(merge(selected_data, batters, by=c('Pos','Arm')),1), Name.y)
} 
  
if (selected_data$Class == 'Pitcher'){
  modal$FB <- dplyr::select(sample_n(merge(selected_data, pitchers, by=c('T','FB')),1), Name.y)
  modal$SL <- dplyr::select(sample_n(merge(selected_data, pitchers, by=c('T','SL')),1), Name.y)
  modal$CB <- dplyr::select(sample_n(merge(selected_data, pitchers, by=c('T','CB')),1), Name.y)
  modal$CH <- dplyr::select(sample_n(merge(selected_data, pitchers, by=c('T','CH')),1), Name.y) 
} 
  

modal <- t(modal)
modal <- na.omit(modal)



