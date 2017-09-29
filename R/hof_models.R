
library(glmnet)
library(lme4)

glmnet_fit <- function(fit_df) {
  xx <- model.matrix(inducted ~ . - playerID, data=fit_df)[,-1]
  yy <- fit_df$inducted
  glmnet_mod <- cv.glmnet(xx, yy , family='binomial')

}

glmer_fit <- function(war_df) {
  bb10 <- filter_by_years(war_df)
  tt <- bb10 %>% mutate(ipeak=as.integer(war_rank<=7),
                        WAR_PEAK=ipeak*WAR) %>%
    group_by(playerID, inducted, POS) %>%
    summarise(cwar=sum(WAR),
              pwar=sum(WAR_PEAK),
              mvps=sum(MVPWin),
              allstars=sum(AllStar),
              allstarstarts=sum(AllStarStart),
              wswins=sum(WSWin)) %>%
    ungroup() %>%
    filter(POS!='P')

  glmer_mod <- glmer(inducted ~ 1 + mvps + (cwar+pwar|POS),
                     data=tt, family='binomial')

}
