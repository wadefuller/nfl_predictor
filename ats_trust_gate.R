# R1.4: past-only gate on frozen outer-fold forecasts.
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(glmnet); library(jsonlite)
  library(ggplot2); library(tidyr)
})

gate_features <- function(d) {
  data.frame(edge = abs(d$nfl_predictor_line - d$spread_line),
    spread = abs(d$spread_line), medium = as.numeric(abs(d$spread_line) >= 3),
    large = as.numeric(abs(d$spread_line) >= 7), total = d$total_line,
    season = d$season, week = d$week,
    rest = abs(d$home_team_rest - d$away_team_rest),
    division = d$div_game, uncertainty = d$sigma)
}

ridge_gate <- function(train, holdout, target) {
  x <- as.matrix(gate_features(train)); z <- as.matrix(gate_features(holdout))
  y <- target
  ok <- is.finite(y); x <- x[ok, , drop = FALSE]; y <- y[ok]
  if (length(y) < 100 || length(unique(y)) < 2) stop("Insufficient gate training labels")
  for (j in seq_len(ncol(x))) {
    med <- median(x[,j], na.rm = TRUE)
    if (!is.finite(med)) med <- 0
    x[!is.finite(x[,j]),j] <- med; z[!is.finite(z[,j]),j] <- med
    center <- mean(x[,j]); scale <- sd(x[,j])
    if (!is.finite(scale) || scale == 0) scale <- 1
    x[,j] <- (x[,j] - center) / scale
    z[,j] <- (z[,j] - center) / scale
  }
  fit <- glmnet(x, y, family = "binomial", alpha = 0, lambda = 0.1,
                standardize = FALSE)
  list(probability = as.numeric(predict(fit, z, type = "response", s = 0.1)),
       coefficients = as.matrix(coef(fit, s = 0.1))[,1])
}

gate_fold <- function(all, year) {
  tr <- all[all$season < year, ]; te <- all[all$season == year, ]
  stopifnot(nrow(tr) > 0, nrow(te) > 0, max(tr$season) < min(te$season),
            !any(tr$game_id %in% te$game_id))
  mae <- ridge_gate(tr, te, as.integer(tr$model_error < tr$market_error))
  cover <- ridge_gate(tr, te, tr$win)
  ceiling <- median(tr$sigma)
  masks <- list(all_games = rep(TRUE,nrow(te)),
    mae_gate = mae$probability > .5,
    cover_gate = is.finite(te$break_even) & cover$probability > te$break_even + .02,
    edge_1 = te$edge >= 1, edge_2_5 = te$edge >= 2.5,
    uncertainty = te$sigma <= ceiling,
    edge_uncertainty = te$edge >= 1 & te$sigma <= ceiling)
  predictions <- bind_rows(lapply(names(masks), function(arm) {
    te %>% mutate(arm = .env$arm, selected = masks[[.env$arm]],
      gate_mae_probability = mae$probability, gate_cover_probability = cover$probability,
      uncertainty_ceiling = ceiling,
      gated_margin = ifelse(selected, nfl_predictor_line, spread_line),
      gated_error = abs(gated_margin - home_result),
      policy_pnl = ifelse(selected, pnl, 0))
  }))
  coefs <- bind_rows(lapply(c("mae","cover"), function(arm) {
    v <- if (arm == "mae") mae$coefficients else cover$coefficients
    data.frame(season = year, gate = arm, feature = names(v), coefficient = unname(v))
  }))
  list(predictions = predictions, coefficients = coefs,
       audit = data.frame(season = year, train_min = min(tr$season),
         train_max = max(tr$season), training_games = nrow(tr),
         holdout_games = nrow(te), uncertainty_ceiling = ceiling))
}

score_gate <- function(d) {
  sel <- d$selected
  bets <- sel & is.finite(d$pnl)
  graded <- bets & !is.na(d$win)
  n <- sum(bets)
  data.frame(games = nrow(d), selected = sum(sel), bets = n,
    graded_bets = sum(graded), pushes = sum(bets & is.na(d$win)),
    selection_rate = mean(sel),
    selected_mae_delta_market = if(any(sel)) mean(d$model_error[sel]-d$market_error[sel]) else NA_real_,
    slate_mae_delta_incumbent = mean(d$gated_error-d$model_error),
    slate_mae_delta_market = mean(d$gated_error-d$market_error),
    pnl = sum(d$policy_pnl,na.rm=TRUE),
    roi = if(n) sum(d$policy_pnl,na.rm=TRUE)/n else NA_real_,
    roi_minus110 = if(n) sum(ifelse(is.na(d$win[bets]),0,
      ifelse(d$win[bets]==1,100/110,-1)))/n else NA_real_,
    accuracy = if(any(graded)) mean(d$win[graded]) else NA_real_)
}

load_gate_rows <- function(run, cache) {
  meta <- fromJSON(file.path(run,"run_metadata.json"))
  if (meta$architecture != "glm_only" || meta$glm_lambda_rule != "min")
    stop("Trust experiment requires GLM-only incumbent lambda.min")
  rows <- lapply(seq_along(meta$holdout_seasons), function(i) {
    year <- meta$holdout_seasons[i]
    f <- file.path(cache, sprintf("season-%d_%s.rds",year,meta$fold_hashes[i]))
    a <- readRDS(f)
    if(a$season != year || a$provenance$training_fingerprint$panel !=
       meta$training_fingerprint$panel) stop("Cache identity mismatch")
    c <- a$calibration_frame
    if(is.null(c) || any(c$calibration_season >= year)) stop("Invalid calibration history")
    a$predictions %>% mutate(sigma = sd(c$residual)*1.05)
  })
  d <- bind_rows(rows)
  if(anyDuplicated(d$game_id)) stop("Duplicate OOF games")
  d %>% mutate(
    edge = abs(nfl_predictor_line-spread_line),
    model_error = abs(nfl_predictor_line-home_result),
    market_error = abs(spread_line-home_result),
    odds = ifelse(nfl_predictor_line > spread_line,home_spread_odds,away_spread_odds),
    payout = ifelse(odds > 0,odds/100,100/abs(odds)),
    break_even = 1/(1+payout), win = cover_correct,
    pnl = ifelse(!is.finite(odds) | odds == 0,NA_real_,
      ifelse(is.na(win),0,ifelse(win==1,payout,-1))))
}

gate_bootstrap <- function(d, reps = 4000L) {
  # Shared blocks preserve the pairing; aggregate before sampling for speed.
  block <- if(length(unique(d$season)) >= 6) as.character(d$season) else
    paste(d$season,d$week,sep="-")
  d$block <- block
  b <- d %>% group_by(block) %>% summarise(
    n=n(), loss=sum(gated_error-model_error),
    profit=sum(policy_pnl,na.rm=TRUE),
    stakes=sum(selected & is.finite(pnl)),
    base_profit=sum(pnl,na.rm=TRUE), base_stakes=sum(is.finite(pnl)), .groups="drop")
  set.seed(989)
  draw <- replicate(reps, {
    ix <- sample.int(nrow(b),nrow(b),replace=TRUE); z <- b[ix,]
    c(mae=sum(z$loss)/sum(z$n),
      roi=if(sum(z$stakes)>0) sum(z$profit)/sum(z$stakes) -
        sum(z$base_profit)/sum(z$base_stakes) else NA_real_)
  })
  bind_rows(lapply(seq_len(nrow(draw)),function(i) {
    v <- draw[i,]
    data.frame(metric=rownames(draw)[i],lo90=quantile(v,.05,na.rm=TRUE),
      hi90=quantile(v,.95,na.rm=TRUE))
  }))
}

main <- function() {
  argv <- commandArgs(TRUE)
  if(length(argv) %% 2) stop("Use --run PATH --out PATH --profile probe|screen|confirm")
  args <- setNames(as.list(argv[seq(2,length(argv),2)]),sub("^--","",argv[seq(1,length(argv),2)]))
  profile <- match.arg(args$profile,c("probe","screen","confirm"))
  years <- switch(profile,probe=2025L,screen=c(2016L,2019L,2022L,2025L),confirm=2016:2025)
  cache <- if(is.null(args$cache)) "data/ats_experiment_cache/folds" else args$cache
  all <- load_gate_rows(args$run,cache)
  if(!all(2012:max(years) %in% all$season)) stop("Missing warm-up/evaluation seasons")
  fits <- lapply(years,function(y)gate_fold(all,y))
  p <- bind_rows(lapply(fits,`[[`,"predictions"))
  if (profile == "confirm") {
    if (is.null(args$screen)) stop("Confirmation requires --screen PATH")
    screened <- read_csv(file.path(args$screen,"decisions.csv"),show_col_types=FALSE)
    eligible <- screened$arm[screened$decision == "advance_to_confirm"]
    p <- p %>% filter(arm %in% c("all_games", eligible))
  }
  summary <- p %>% group_by(arm) %>% group_modify(~score_gate(.x)) %>% ungroup()
  yearly <- p %>% group_by(arm,season) %>% group_modify(~score_gate(.x)) %>% ungroup()
  baseline <- yearly %>% filter(arm=="all_games") %>% select(season,base_roi=roi)
  yearly <- yearly %>% left_join(baseline,by="season") %>% mutate(roi_gain=roi-base_roi)
  base_roi <- summary$roi[summary$arm=="all_games"]
  boot <- bind_rows(lapply(setdiff(unique(p$arm),"all_games"),function(a)
    gate_bootstrap(p[p$arm==a,]) %>% mutate(arm=a)))
  decisions <- bind_rows(lapply(setdiff(summary$arm,"all_games"),function(a) {
    s <- summary[summary$arm==a,]; y <- yearly[yearly$arm==a,]
    error_wins <- sum(y$selected_mae_delta_market<0,na.rm=TRUE)
    roi_wins <- sum(y$roi_gain>0,na.rm=TRUE)
    adequate <- all(y$graded_bets>=25)
    positive <- pmax(y$pnl,0)
    concentration <- if(sum(positive)>0) max(positive)/sum(positive) else 1
    gain <- s$slate_mae_delta_incumbent<0 || s$roi>base_roi
    pass <- adequate && gain && (error_wins>=3 || roi_wins>=3)
    decision <- if(profile=="probe") "diagnostic_only" else if(profile=="screen")
      if(pass) "advance_to_confirm" else "stop_after_screen" else {
        b <- boot[boot$arm==a,]
        error_pass <- error_wins>=6 && b$hi90[b$metric=="mae"]<0
        roi_pass <- roi_wins>=6 && b$lo90[b$metric=="roi"]>0
        if(adequate && s$graded_bets>=500 && concentration<=.5 &&
           (error_pass || roi_pass)) "historical_candidate_provenance_review_required" else "do_not_promote"
      }
    data.frame(arm=a,error_seasons_won=error_wins,roi_seasons_won=roi_wins,
      min_graded_bets=min(y$graded_bets),positive_pnl_concentration=concentration,
      decision=decision)
  }))
  out <- args$out; dir.create(out,recursive=TRUE,showWarnings=FALSE)
  write_csv(summary,file.path(out,"overall.csv"))
  write_csv(yearly,file.path(out,"by_season.csv"))
  slices <- bind_rows(lapply(c("spread", "total", "week", "division"), function(slice) {
    q <- p %>% mutate(regime = switch(slice,
      spread = ifelse(abs(spread_line)<3,"under 3",ifelse(abs(spread_line)<7,"3 to under 7","7+")),
      total = ifelse(total_line<44,"under 44","44+"),
      week = ifelse(week<=9,"weeks 1-9","weeks 10+"),
      division = ifelse(div_game==1,"division","nondivision")))
    q %>% group_by(arm,regime) %>% group_modify(~score_gate(.x)) %>%
      ungroup() %>% mutate(slice=slice)
  }))
  write_csv(slices,file.path(out,"by_regime.csv"))
  write_csv(decisions,file.path(out,"decisions.csv"))
  write_csv(boot,file.path(out,"bootstrap.csv"))
  write_csv(bind_rows(lapply(fits,`[[`,"audit")),file.path(out,"training_audit.csv"))
  write_csv(bind_rows(lapply(fits,`[[`,"coefficients")),file.path(out,"coefficients.csv"))
  write_csv(p %>% select(game_id,season,week,arm,selected,nfl_predictor_line,
    spread_line,home_result,gated_margin,gate_mae_probability,gate_cover_probability,
    uncertainty_ceiling,sigma,edge,odds,win,pnl,policy_pnl),
    file.path(out,"policy_predictions.csv"))
  write_json(list(profile=profile,years=years,source_run=normalizePath(args$run),
    source_metadata=fromJSON(file.path(args$run,"run_metadata.json")),
    policy_source_sha256=digest::digest("ats_trust_gate.R",algo="sha256",file=TRUE),
    screen_decisions_sha256=if(profile=="confirm") digest::digest(
      file.path(args$screen,"decisions.csv"),algo="sha256",file=TRUE) else NULL,
    lambda=.1,alpha=0,bootstrap_replicates=4000,promotion_authority=FALSE),
    file.path(out,"metadata.json"),pretty=TRUE,auto_unbox=TRUE)
  ggsave(file.path(out,"selection_and_roi.png"),
    ggplot(yearly,aes(factor(season),roi,colour=arm,group=arm))+
      geom_hline(yintercept=0,colour="grey60")+geom_line()+geom_point(aes(size=graded_bets))+
      facet_wrap(~arm)+guides(colour="none",size="none")+
      scale_y_continuous(labels=function(x) paste0(round(x*100),"%"))+
      labs(x=NULL,y="ROI per unit staked",title="Trust policies: return and sample size",
           subtitle="Point size indicates graded bet count; pushes refunded")+
      theme_minimal(),width=12,height=7,dpi=140)
  print(summary %>% select(arm,graded_bets,slate_mae_delta_incumbent,roi))
  print(decisions)
}
if(sys.nframe()==0) main()
