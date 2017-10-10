
library(Rtsne)

get_br_fit_data <- function() {

  br_war <- readRDS('inst/extdata/br_war.rds')
  #load('./data/br_war.RData')

  bb <- get_lahman_batting() %>% as_tibble()
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

  bb <- append_war_rank(bb)
  bb %>% filter(playerID=='aaronha01') %>% count(playerID, yearID, sort=TRUE)

  bb10 <- filter_by_years(bb) %>%
    filter(POS!='P') %>%
    mutate(inducted=ifelse(votedBy == 'BBWAA', 'Y', 'N'))

  bb10$inducted <- factor(bb10$inducted)
  bb10
}


do_tsne <- function(dfX,
                    pca=TRUE,
                    pca_scale=TRUE,
                    theta=0,
                    check_duplicates=FALSE,
                    max_iter=1000,
                    selection_regex='^playerID|^WAR_') {
  tmp  <- dfX %>%
    select(which(grepl(selection_regex, names(dfX))))
  nz <- caret::nearZeroVar(tmp)
  if (length(nz) > 0) {
    tmp <- tmp[,-nz]
  }

  tsne_mod <- Rtsne(tmp, pca=pca,
                    pca_scale=pca_scale,
                    check_duplicates=check_duplicates,
                    theta=theta, max_iter=max_iter)
  dfX$x1 <- tsne_mod$Y[,1]
  dfX$x2 <- tsne_mod$Y[,2]
  dfX
}

make_train_test <- function(bb)  {
  tmp <- bb %>% filter(POS!='P')
  fit_df_train <- get_fit_data(tmp, final_game_min='1901-01-01', final_game_max='2006-01-01') %>% filter(PA_1>=30)
  fit_df_test <- get_fit_data(tmp, final_game_min='2006-01-01', final_game_max='2016-01-01') %>% filter(PA_1>=30)
  list(fit_df_train, fit_df_test)
}

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



