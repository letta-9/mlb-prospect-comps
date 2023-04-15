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
mlb_pitchers[is.na(mlb_pitchers)] <- -99
mlb_pitchers$FB <- pmax(mlb_pitchers$wFB_C, mlb_pitchers$wCT_C, mlb_pitchers$wSF_C)
mlb_pitchers$FB.Type <- NA
mlb_pitchers[mlb_pitchers == -99] <- NA

mlb_pitchers$FB.Type[mlb_pitchers$FB == mlb_pitchers$wFB_C] <- '4-Seamer'
mlb_pitchers$FB.Type[mlb_pitchers$FB == mlb_pitchers$wCT_C] <- 'Cutter'
mlb_pitchers$FB.Type[mlb_pitchers$FB == mlb_pitchers$wSF_C] <- 'Sinker'

mlb_pitchers <- mlb_pitchers %>% select(Name, FB.Type, FB, wSL_C, wCB_C, wCH_C, BB_pct)


# Convert stats to grades

pit_std <- sd(unlist(mlb_pitchers[,3]), na.rm = TRUE)
pit_mean <- colMeans(mlb_pitchers[,3], na.rm = TRUE)
pit_breaks = c((pit_mean - (2*pit_std)),(pit_mean - pit_std), pit_mean, (pit_mean + pit_std),(pit_mean + (2*pit_std)))
p <- findInterval(unlist(mlb_pitchers[,3]), pit_breaks)
p <- factor(p)
levels(p) <- c(30, 40, 50, 60, 70, 80)
mlb_pitchers <- cbind(mlb_pitchers, p)

for (i in 4:7){
  pit_std <- sd(unlist(mlb_pitchers[,..i]), na.rm = TRUE)
  pit_mean <- colMeans(mlb_pitchers[,..i], na.rm = TRUE)
  pit_breaks = c((pit_mean - (2*pit_std)),(pit_mean - pit_std), pit_mean, (pit_mean + pit_std),(pit_mean + (2*pit_std)))
  p <- findInterval(unlist(mlb_pitchers[,..i]), pit_breaks)
  p <- factor(p)
  levels(p) <- c(30, 40, 50, 60, 70, 80)
  mlb_pitchers <- cbind(mlb_pitchers, p)
}

# Clean

mlb_pitchers <- mlb_pitchers[,c(1:2,8:12)]
colnames(mlb_pitchers) <- c('Name','FB.Type', 'FB', 'SL', 'CB', 'CH', 'CMD')

mlb_pitchers$Has.FB <- !is.na(mlb_pitchers$FB)
mlb_pitchers$Has.SL <- !is.na(mlb_pitchers$SL)
mlb_pitchers$Has.CB <- !is.na(mlb_pitchers$CB)
mlb_pitchers$Has.CH <- !is.na(mlb_pitchers$CH)

