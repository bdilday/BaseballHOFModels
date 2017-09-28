
library(glmnet)

glmnet_fit <- function(fit_df) {
  xx <- model.matrix(inducted ~ . - playerID, data=fit_df)[,-1]
  yy <- fit_df$inducted
  glmnet_mod <- cv.glmnet(xx, yy , family='binomial')

}

glmer_fit <- function(war_jaws_df) {
  bb10 <- filter_by_years(bb)
  tt <- bb10 %>% mutate(ipeak=as.integer(war_rank<=7), WAR_PEAK=ipeak*WAR) %>% select(playerID, WAR, WAR_PEAK, inducted, POS)  %>% group_by(playerID, inducted, POS) %>% summarise(cwar=sum(WAR), pwar=sum(WAR_PEAK)) %>% ungroup() %>% filter(POS!='P')

  glmer_mod <- glmer(inducted ~ 1 + (cwar+pwar|POS),
                     data=war_jaws_df, family='binomial')

}
