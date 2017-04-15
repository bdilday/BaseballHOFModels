library(tidyverse)
library(Lahman)
library(tensorflow)
library(randomForest)
library(xgboost)
library(glmnet)
library(caret)
library(caretEnsemble)
library(magrittr)

combine_war_stints <- function(.data) {
  
  .data %>%
    gather(key, value, -player_ID, -year_ID, -stint_ID) %>%
    dplyr::group_by(player_ID, year_ID, key) %>%
    dplyr::mutate(value=as.numeric(value)) %>%
    dplyr::summarise(value=sum(value, na.rm=TRUE)) %>%
    spread(key, value) %>%
    ungroup()

}

combine_stints <- function(batting) {
  batting %>%
    dplyr::mutate(wwoba=BASIC_WOBA*PA) %>%
    gather(key, value, -playerID, -yearID, -stint, -teamID, -lgID) %>%
    dplyr::group_by(playerID, yearID, key) %>%
    dplyr::summarise(value=sum(value, na.rm=TRUE)) %>%
    spread(key, value) %>%
    dplyr::mutate(SlugPct=TB/AB, 
           OBP=(H+BB+HBP)/PA, 
           BA=H/AB, BASIC_WOBA=wwoba/PA, 
           OPS=OBP+SlugPct) %>%
    ungroup()
  
}

pull_br_data <- function() {
  bref_bat_url = 'http://www.baseball-reference.com/data/war_daily_bat.txt'
  bref_pitch_url = 'http://www.baseball-reference.com/data/war_daily_pitch.txt'
  br_bat_war <- read_csv(bref_bat_url) %>% mutate(WAA=as.numeric(WAA), 
                                                  WAR=as.numeric(WAR),
                                                  WAR_off=as.numeric(WAR_off),
                                                  WAR_def=as.numeric(WAR_def))
  br_pitch_war <- read_csv(bref_pitch_url)
  list(BattingWAR=br_bat_war, PitchingWAR=br_pitch_war)
}

get_primary_pos <-function() {
  Lahman::Fielding %>% 
    dplyr::group_by(playerID, POS) %>% 
    dplyr::summarise(ngame=sum(G, na.rm=TRUE)) %>% 
    dplyr::arrange(playerID, -ngame) %>% 
    dplyr::mutate(pos_rank=row_number()) %>% 
    dplyr::filter(pos_rank==1) %>% 
    dplyr::select(playerID, POS)  
}

list_of_pitchers <- function() {
  Lahman::Fielding %>% 
    dplyr::group_by(playerID, POS) %>% 
    dplyr::summarise(ngame=sum(G, na.rm=TRUE)) %>% 
    dplyr::arrange(playerID, -ngame) %>% 
    dplyr::mutate(pos_rank=row_number()) %>% 
    dplyr::filter(pos_rank==1, POS=='P') %>% 
    dplyr::select(playerID)
}

append_age <- function(.data, master) {
  .data %>% merge(master, by="playerID") %>% 
    dplyr::mutate(Age=yearID-birthYear)
}

get_pos_player_hofers <- function(last_game_min='1800-01-01', last_game_max='2020-01-01') {
  hofers <- Lahman::HallOfFame %>%
    dplyr::filter(inducted=='Y', 
           category=='Player', 
           votedBy %in% c("BBWAA", "Special Election")) %>%
    dplyr::select(playerID, inducted)
  bb <- append_age(combine_stints(Lahman::battingStats()), 
                   Lahman::Master %>% 
                     dplyr::filter(finalGame >= last_game_min, finalGame <= last_game_max) 
                   %>% dplyr::select(playerID, bbrefID, birthYear)
  )
  bbh <- anti_join(bb %>% merge(hofers, by="playerID", all.x=TRUE), 
                   list_of_pitchers())
  bbh <- bbh %>% replace_na(list(inducted='N'))
  bbh$inducted <- as.factor(bbh$inducted)
  
  if (! 'br_war' %in% ls()) {
    load('./data/br_war.RData')


  }
    
  bbh <- bbh %>% 
    merge(br_war$BattingWAR %>% 
            dplyr::select(player_ID, WAA, WAR, WAR_off, WAR_def, year_ID, stint_ID) %>% 
            combine_war_stints() %>% 
            dplyr::rename(bbrefID=player_ID, yearID=year_ID)
    ) %>% 
    merge(get_primary_pos(), by="playerID")
  bbh
}

get_fit_data <- function(.data, maxyear=20, list_of_stats=NULL) {
  
  if (is.null(list_of_stats)) {
    list_of_stats <- c("AB", "BB", "CS", "G", "GIDP", 
                       "HBP", "HR", "IBB", "BR", 
                       "PA", "R", "RBI", "SB", "SF", "SH", "SO", "TB", 
                       "wwoba", "X1B", "X2B", "X3B", "Age", "WAA", "WAR", "WAR_off", "WAR_def")
  }
  
  kk = .data %>% 
    dplyr::mutate(BR=OBP*SlugPct*PA) %>% 
    dplyr::arrange(playerID, -BR) %>% 
    dplyr::group_by(playerID) %>% 
    dplyr::mutate(nyear=row_number()) %>% 
    dplyr::ungroup() %>% 
    dplyr::filter(nyear>=1, nyear <= maxyear) 
  kk <- kk[,c("playerID", "yearID", "nyear", list_of_stats, "POS", "inducted")]
  
  kk %<>% 
    gather(key, value, -playerID, -yearID, -nyear, -POS, -inducted) %>% 
    mutate(new_key=paste(key, nyear, sep='_'))
  
  kk %<>% 
    select(-key, -nyear, -yearID) %>% 
    spread(new_key, value, fill=0)
  
  # ineligible
  kk %<>% filter(playerID!='rosepe01', playerID!='jacksjo01')
  
  kk$POS <- as.factor(kk$POS)
  
  kk
  
}


#fit_control <- trainControl(method = "cv", number = 10)
#set.seed(825)
# gbmFit1 <- train(xx, yy, method = "gbm", trControl = fit_control, verbose = FALSE)
# xgbFit1 <- train(xx, yy, method = "xgbLinear", trControl = fit_control, verbose = FALSE)

