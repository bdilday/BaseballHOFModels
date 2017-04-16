library(tidyverse)
library(Lahman)
library(tensorflow)
library(randomForest)
library(xgboost)
library(glmnet)
library(caret)
library(caretEnsemble)
library(magrittr)
library(doParallel)

#' returns a data frame with batting stats, aggregating over stints
get_lahman_batting <- function() {
  Lahman::battingStats() %>% combine_lahman_batting_stints()
}

#' returns a data frame with pitching stats, aggregating over stints
get_lahman_pitching <- function() {
  Lahman::Pitching %>% combine_lahman_pitching_stints()
}


filter_by_retirement_year <- function(.data) {
  Lahman::Master %>%
    dplyr::filter(finalGame >= last_game_min, finalGame <= last_game_max) %>%
    dplyr::select(playerID, bbrefID, birthYear)
}


#' combine baseball reference war data
#' applies to batting or pitching
combine_war_stints <- function(.data) {
  .data %>%
    gather(key, value, -player_ID, -year_ID, -stint_ID) %>%
    dplyr::group_by(player_ID, year_ID, key) %>%
    dplyr::mutate(value=as.numeric(value)) %>%
    dplyr::summarise(value=sum(value, na.rm=TRUE)) %>%
    spread(key, value) %>%
    ungroup()
}

combine_lahman_pitching_stints <- function(batting) {
  batting %>%
    gather(key, value, -playerID, -yearID, -stint, -teamID, -lgID) %>%
    dplyr::group_by(playerID, yearID, key) %>%
    dplyr::summarise(value=sum(value, na.rm=TRUE)) %>%
    spread(key, value) %>%
    dplyr::mutate(RA9=27*R/IPouts) %>%
    ungroup()
}

combine_lahman_batting_stints <- function(batting) {
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

#' pull data from baseball reference
#' in practice, this has been run and results archived in data directory
pull_br_data <- function() {
  bref_bat_url = 'http://www.baseball-reference.com/data/war_daily_bat.txt'
  bref_pitch_url = 'http://www.baseball-reference.com/data/war_daily_pitch.txt'
  br_bat_war <- read_csv(bref_bat_url) %>% mutate(WAA=as.numeric(WAA),
                                                  WAR=as.numeric(WAR),
                                                  WAR_off=as.numeric(WAR_off),
                                                  WAR_def=as.numeric(WAR_def))
  br_pitch_war <- read_csv(bref_pitch_url)
  br_war <- list(BattingWAR=br_bat_war, PitchingWAR=br_pitch_war)
}

#' get primary position
#' defined as most career games
#' fielding table double counts OF positions, so RF, CF, LF all come out as generic OF
#' this misses all players that batted without ever fielding. In principle this would miss DH
#' but doesn't seem to be an issue in practice,
#' so I'm being lazy and just excluding about 200 such records
get_primary_pos <- function() {
  Lahman::Fielding %>%
    dplyr::group_by(playerID, POS) %>%
    dplyr::summarise(ngame=sum(G, na.rm=TRUE)) %>%
    dplyr::arrange(playerID, -ngame) %>%
    dplyr::mutate(pos_rank=row_number()) %>%
    dplyr::filter(pos_rank==1) %>%
    dplyr::select(playerID, POS)
}

append_pos <- function(.data) {
  pp = get_primary_pos()
  .data %>% merge(pp) %>% mutate(POS=as.factor(POS))
}


#' Append age to a data frame based on Lahman::Master playerID
append_age <- function(.data) {
  .data %>%
    merge(Lahman::Master, by="playerID") %>%
    dplyr::mutate(Age=yearID-birthYear)
}

#' append HOF status: inducted = (TRUE, FALSE)
#' TODO: specify as 1 of 4 categories; 1st ballot, BBWAA, Veterns, No
append_hof <- function(.data) {
  hofers <- Lahman::HallOfFame %>%
    dplyr::filter(inducted=='Y',
                  category=='Player') %>%
    dplyr::select(playerID, inducted, votedBy)

  .data %>% merge(hofers, by="playerID", all.x=TRUE) %>%
    replace_na(list(inducted='N')) %>%
    replace_na(list(votedBy='N')) %>%
    mutate(votedBy=ifelse(votedBy %in% c("BBWAA", "Special Election"), "BBWAA", votedBy)) %>%
    mutate(votedBy=ifelse(votedBy %in% c("BBWAA", "N"), votedBy, "VetCom")) %>%
    mutate(inducted=as.factor(inducted), votedBy=as.factor(votedBy))
}

#' append batting war
append_br_war <- function(.data, war) {
  tmp <- war %>%
    dplyr::select(player_ID, WAA, WAR, WAR_off, WAR_def, year_ID, stint_ID)

  tmp <- tmp %>%
    combine_war_stints() %>%
    dplyr::rename(bbrefID=player_ID, yearID=year_ID)

  .data %>% merge(tmp)
}


#' append WS wins
append_all_star <- function(.data) {

  all_stars <- Lahman::AllstarFull %>%
    mutate(AllStar='Y',
           AllstarStart=ifelse(is.na(StartingPos), 'N', 'Y')) %>%
    select(playerID, yearID, AllStar, AllstarStart)

  .data %>% merge(all_stars, by=c("playerID", "yearID")) %>%
    replace_na(list(AllStar='N', AllStarStart='N'))
}


#' append WS wins
append_ws_wins <- function(.data) {

  all_players <- Lahman::Appearances %>%
    group_by(playerID, yearID) %>%
    mutate(rr=row_number()) %>%
    filter(rr==max(rr)) %>%
    select(playerID, yearID, teamID)

  ws_winners <- Lahman::Teams %>%
    filter(WSWin == 'Y') %>%
    select(yearID, teamID, WSWin)

  ws_players <- merge(all_players, ws_winners, by=c("yearID", "teamID")) %>%
    select(playerID, yearID, WSWin)

  ee = merge(.data, ws_players, by=c("yearID", "playerID"), all.x=TRUE) %>%
    replace_na(list(WSWin='N'))
}

#' fit data
#' takes the top MAXYEAR seasons, ordered by ORDER_KEY and
#' returns the value of LIST_OF_STATS. If less than MAXYEAR has been played, filled in with 0
get_fit_data <- function(.data,
                         MAXYEAR=20,
                         LIST_OF_STATS=NULL,
                         ORDER_KEY="WAA",
                         final_game_min='1901-01-01',
                         final_game_max='2006-01-01') {

  kitchen_sink <- c("AB", "BB", "CS", "G", "GIDP",
                    "HBP", "HR", "IBB", "BR",
                    "PA", "R", "RBI", "SB", "SF", "SH", "SO", "TB",
                    "wwoba", "X1B", "X2B", "X3B", "Age", "WAA", "WAR", "WAR_off", "WAR_def")
  war_and_fip <- c("PA", "BB", "SO", "WAA", "WAR", "WAR_off", "WAR_def")

  if (is.null(LIST_OF_STATS)) {
    LIST_OF_STATS <- war_and_fip
  }

  kk = .data %>%
    dplyr::filter(finalGame>=final_game_min, finalGame<=final_game_max) %>%
    mutate(ORDER_KEY=ORDER_KEY) %>%
    dplyr::arrange(playerID, ORDER_KEY) %>%
    dplyr::select(-ORDER_KEY) %>%
    dplyr::group_by(playerID) %>%
    dplyr::mutate(nyear=row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(nyear>=1, nyear <= MAXYEAR)
  kk <- kk[,c("playerID", "yearID", "nyear", LIST_OF_STATS, "POS", "inducted")]

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

