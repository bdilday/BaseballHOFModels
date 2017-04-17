

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

fit_df <- get_fit_data(bb)


do_hof_fit <- function(fit_df) {
  frm <- as.formula(inducted ~ . - playerID - votedBy)

  xx = model.matrix(frm, data=fit_df)[,-1]
  yy = fit_df$inducted

  cl <- makeCluster(detectCores())
  registerDoParallel(cl)

  set.seed(825)
  cv_train <- trainControl(method = "cv", number = 10)
  repeated_cv_train <- trainControl(method = "repeatedcv", number = 5, repeats=5)

  gbmFit1 <- train(xx, yy, method = "gbm", trControl = cv_train, verbose = FALSE)
  xgbFit1 <- train(xx, yy, method = "xgbLinear", trControl = fit_control, verbose = FALSE)

  stopCluster(cl)
}



