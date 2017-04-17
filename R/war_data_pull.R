
#library(baseballr)
library(readr)

pull_br_data <- function() {
  bref_bat_url = 'http://www.baseball-reference.com/data/war_daily_bat.txt'
  bref_pitch_url = 'http://www.baseball-reference.com/data/war_daily_pitch.txt'
  br_bat_war <- read_csv(bref_bat_url)
  br_pitch_war <- read_csv(bref_pitch_url)
  list(BattingWAR=br_bat_war, PitchingWAR=br_pitch_war)
}

pull_fg_data <- function () {
  #http://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=y&type=c,4,6,11,12,13,21,-1,40,41,-1,23,37,38,50,61,111,-1,203,199,58,7,8,9,10,14,15,16,17,18,19,20,21,22,50,51,52&season=2016&month=0&season1=1871&ind=1&team=&rost=&age=&filter=&players=
  #http://www.fangraphs.com/leaders.aspx?pos=all&stats=pit&lg=all&qual=y&type=c,4,5,11,7,8,13,-1,36,37,40,43,44,48,51,-1,6,45,62,-1,59,6,14,15,16,17,18,19,20,21,22,23,24&season=2016&month=0&season1=1871&ind=1&team=0&rost=0&age=0&filter=&players=0
  fg_bat_war <- read_csv('extdata/fg_bat_stats.csv')
  fg_pitch_war <- read_csv('extdata/fg_pitch_stats.csv')
  list(BattingWAR=fg_bat_war, PitchingWAR=fg_pitch_war)
}

combine_bat_pitch <- function(.data) {
  db = .data$BattingWAR
  dp = .data$PitchingWAR
  db %>%
    select(player_ID, year_ID, WAR) %>%
    group_by(player_ID, year_ID) %>%
    summarise(WAR=sum(as.numeric(WAR)))
  
}

