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
    dplyr::mutate(BASIC_WOBA =
                    0.7*BB +
                    0.9*(H-X2B-X3B-HR) +
                    1.25*X2B +
                    1.6*X3B +
                    2.0*HR) %>%
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
    dplyr::select(playerID, POS) %>%
    ungroup()
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

  tmp <- tmp %>%
    group_by(yearID) %>%
    arrange(-WAA) %>%
    mutate(WAA_rank=row_number()) %>%
    ungroup()

  .data %>% merge(tmp)
}


#' append MVPs
append_mvps <- function(.data) {

  mvps <- Lahman::AwardsPlayers %>%
    dplyr::filter(awardID=='Most Valuable Player') %>%
    dplyr::mutate(MVPWin='Y') %>%
    dplyr::select(playerID, yearID, MVPWin)

  mvp_shares <- Lahman::AwardsSharePlayers %>%
    dplyr::filter(awardID=='MVP') %>%
    dplyr::mutate(MVPShare=pointsWon/pointsMax) %>%
    dplyr::select(playerID, yearID, MVPShare)

  kk <- .data %>%
    merge(mvps, by=c("playerID", "yearID"), all.x=TRUE) %>%
    replace_na(list(MVPWin='N')) %>%
    merge(mvp_shares, by=c("playerID", "yearID"), all.x=TRUE) %>%
    replace_na(list(MVPShare=0.0)) %>%
    dplyr::mutate(MVPWin = ifelse(MVPWin=='Y', 1, 0), MPVWin=as.numeric(MVPWin))
}

#' append WS wins
append_all_star <- function(.data) {

  all_stars <- Lahman::AllstarFull %>%
    group_by(playerID, yearID) %>%
    summarise(startingPos=max(startingPos)) %>%
    mutate(AllStar='Y',
           AllStarStart=ifelse(is.na(startingPos), 'N', 'Y')) %>%
    select(playerID, yearID, AllStar, AllStarStart)

  .data %>% merge(all_stars, by=c("playerID", "yearID"), all.x=TRUE) %>%
    replace_na(list(AllStar='N', AllStarStart='N')) %>%
    mutate(AllStar=as.numeric(ifelse(AllStar=='Y', 1, 0)),
           AllStarStart=as.numeric(ifelse(AllStarStart=='Y', 1, 0))
           )
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
    replace_na(list(WSWin='N')) %>%
    mutate(WSWin=as.numeric(ifelse(WSWin=='Y', 1, 0)))
}

#' append WS wins
append_war_rank <- function(war_df) {
  war_df %>%
    group_by(playerID) %>%
    mutate(WAR=as.numeric(WAR)) %>%
    arrange(-WAR) %>%
    dplyr::mutate(war_rank=row_number()) %>%
    ungroup() %>% arrange(playerID, yearID)
}

filter_by_years <- function(war_df, min_year=10) {
  war_df %>%
    group_by(playerID) %>%
    mutate(nyear=max(war_rank)) %>%
    filter(nyear >= min_year) %>%
    ungroup() %>%
    dplyr::select(-nyear)
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
  war_and_fip <- c("PA", "BB", "SO", "WAA", "WAA_rank", "WAR", "WAR_off", "WAR_def")
  grit <- c("AllStar", "AllStarStart", "WSWin", "MVPWin", "MVPShare")

  if (is.null(LIST_OF_STATS)) {
    LIST_OF_STATS <- c(war_and_fip, grit)
  }

  kk <- .data %>%
    dplyr::filter(finalGame>=final_game_min, finalGame<=final_game_max)

  kk$ORDER_KEY <- kk[[ORDER_KEY]]

  kk <- kk %>%
    dplyr::arrange(playerID, -ORDER_KEY) %>%
    dplyr::select(-ORDER_KEY) %>%
    dplyr::group_by(playerID) %>%
    dplyr::mutate(nyear=row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(nyear>=1, nyear <= MAXYEAR)
  kk <- kk[,c("playerID", "yearID", "nyear", LIST_OF_STATS, "POS", "inducted")]

  col_numerics <- names(kk)[sapply(kk, is.numeric)]
  col_factors <- names(kk)[sapply(kk, is.factor)]

  kkg <- kk %>%
    gather(key, value,
           -playerID,
           -yearID,
           -nyear,
           -POS,
           -inducted) %>%
    mutate(new_key=paste(key, nyear, sep='_'))

  kkg %<>%
    select(-key, -nyear, -yearID) %>%
    spread(new_key, value, fill=0)

  #TODO: find a better way to accomplish this
  # specifically, because I'm gathering both numeric and factor columns,
  # everything gets cast as character, and need to convert back
  for (s in col_numerics) {
    kkg %<>% mutate_each(funs(as.numeric), starts_with(s))
  }

  for (s in col_factors) {
    kkg %<>% mutate_each(funs(as.factor), starts_with(s))
  }

  col_factors <- names(kkg)[sapply(kkg, is.factor)]
  for (s in col_factors) {
    print(s)
    print(levels(kkg[[s]]))

    if ("0" %in% levels(kkg[[s]])){
      kkg[kkg[[s]] == "0",][[s]] = "N"
      kkg[[s]] <- droplevels(kkg)[[s]]
    }
  }

  # ineligible
  kkg %>% filter(playerID!='rosepe01', playerID!='jacksjo01')

}

convert_to_factors <- function(dfX,
                               factors_list=c("^AllStar", "^MVPWin", "^WSWin")) {

  cc <- unlist(lapply(factors_list, grep, names(dfX)))
  for (i in seq_along(cc)) {
    c1 <- cc[i]
    dfX[,c1] <- as.factor(unlist(dfX[,c1]))
  }

  dfX[,!(sapply(dfX, nlevels) == 1)]

}

compute_jaws <- function(war_df) {
  jaws_df <- war_df %>%
    group_by(playerID) %>%
    mutate(WAR=as.numeric(WAR)) %>%
    arrange(-WAR) %>%
    dplyr::mutate(war_rank=row_number(),
                  ipeak=as.integer(war_rank<=7),
                  nyear=max(war_rank)) %>%
    group_by(playerID, votedBy, POS) %>%
    dplyr::summarise(last_year=max(yearID),
                     nyear=mean(nyear),
                     cwar=sum(WAR),
                     pwar=sum(ipeak*WAR),
                     jaws=0.5*(cwar+pwar)) %>%
    ungroup()
}


