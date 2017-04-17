

load('./data/br_war.RData')

bb <- get_lahman_batting()
bb %>% filter(playerID=='aaronha01') %>% count(playerID, yearID, sort=TRUE)

bb <- append_age(bb)
bb %>% filter(playerID=='aaronha01') %>% count(playerID, yearID, sort=TRUE)

bb <- append_br_war(bb, war=br_war$BattingWAR)
bb %>% filter(playerID=='aaronha01') %>% count(playerID, yearID, sort=TRUE)

bb <- append_pos(bb)
bb %>% filter(playerID=='aaronha01') %>% count(playerID, yearID, sort=TRUE)

bb <- append_hof(bb)
bb %>% filter(playerID=='aaronha01') %>% count(playerID, yearID, sort=TRUE)

bb <- append_ws_wins(bb)
bb %>% filter(playerID=='aaronha01') %>% count(playerID, yearID, sort=TRUE)

bb <- append_mvps(bb)
bb %>% filter(playerID=='aaronha01') %>% count(playerID, yearID, sort=TRUE)

bb <- append_all_star(bb)
bb %>% filter(playerID=='aaronha01') %>% count(playerID, yearID, sort=TRUE)

tmp <- bb %>% filter(POS!='P')
fit_df_train <- get_fit_data(tmp, final_game_min='1901-01-01', final_game_max='2006-01-01') %>% filter(PA_1>=30)
fit_df_test <- get_fit_data(tmp, final_game_min='2006-01-01', final_game_max='2016-01-01') %>% filter(PA_1>=30)


do_hof_fit <- function(fit_df) {
  frm <- as.formula(inducted ~ . - playerID)

  tmp <- fit_df_train
  xx = model.matrix(frm, data=tmp)[,-1]
  yy = tmp$inducted

  xx_test <- model.matrix(frm,data=df_fit_test)[,-1]
  xx_test <- df_fit_test$inducted

  cl <- makeCluster(detectCores())
  registerDoParallel(cl)

  set.seed(825)
  cv_train <- trainControl(method = "cv", number = 10)
  repeated_cv_train <- trainControl(method = "repeatedcv", number = 5, repeats=5)

  #gbmFit1 <- train(xx, yy, method = "gbm", trControl = cv_train, verbose = FALSE)
  xgbFit1 <- train(xx, yy, method = "xgbLinear", trControl = cv_train, verbose = FALSE)

  stopCluster(cl)
}



