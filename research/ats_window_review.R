suppressPackageStartupMessages({library(dplyr); library(readr); library(jsonlite)})
root <- "data/ats_experiments/training-window"
arms <- c("window8","window12","half4","half8","half12","hybrid")
raw <- read_csv("data/game_w_features.csv",show_col_types=FALSE) %>%
  filter(season>=2008,!is.na(result),!is.na(home_moneyline),
    (season<=2020 & week<=17)|(season>=2021 & week<=18))
audit <- bind_rows(lapply(c("expanding",arms),function(a) {
  bind_rows(lapply(c(2016,2019,2022:2025),function(y) {
    bind_rows(lapply(c("pre_cal","final"),function(stage) {
      end <- y-ifelse(stage=="pre_cal",3,1)
      s <- raw$season[raw$season<=end]
      age <- end-s
      if(grepl("window",a)) age <- age[age<as.numeric(sub("window","",a))]
      w <- rep(1,length(age))
      if(grepl("half",a)) w <- 2^(-age/as.numeric(sub("half","",a)))
      if(a=="hybrid") w <- pmax(.25,2^(-age/4))
      data.frame(arm=a,season=y,stage=stage,rows=length(w),ess=sum(w)^2/sum(w^2))
    }))
  }))
}))
write_csv(audit,file.path(root,"sample_audit.csv"))
lambda_sd <- function(a) {
  d <- read_csv(file.path(root,a,"glm_diagnostics.csv"),show_col_types=FALSE)
  d %>% group_by(stage) %>% summarise(sd=sd(log(selected_lambda)),.groups="drop")
}
base <- lambda_sd("expanding")
results <- bind_rows(lapply(arms,function(a) {
  p <- file.path(root,paste0("compare-",a))
  d <- read_csv(file.path(p,"by_season.csv"),show_col_types=FALSE)
  o <- read_csv(file.path(p,"overall.csv"),show_col_types=FALSE)
  recent <- mean(d$delta_mae[d$season>=2022]); early <- mean(d$delta_mae[d$season<2022])
  stable <- all(lambda_sd(a)$sd<=pmax(.1,2*base$sd))
  ess <- min(audit$ess[audit$arm==a])
  pass <- recent<= -.01 && sum(d$delta_mae[d$season>=2022]<0)>=3 &&
    early<=.05 && max(d$delta_mae)<=.20 && o$delta_brier<=0 && o$delta_logloss<=0 &&
    mean(d$delta_mae[d$season!=2024])<0 && ess>=750 && stable
  data.frame(arm=a,delta_mae=o$delta_mae,recent_mae=recent,early_mae=early,
    recent_wins=sum(d$delta_mae[d$season>=2022]<0),worst_year=max(d$delta_mae),
    delta_brier=o$delta_brier,delta_logloss=o$delta_logloss,
    min_ess=ess,lambda_stable=stable,decision=ifelse(pass,"advance","stop"))
}))
write_csv(results,file.path(root,"screen_decisions.csv")); print(results)
