# Focused regression checks for ATS GLM lambda-selection research controls.

suppressPackageStartupMessages(
  source(path.expand("~/Projects/R/nfl_predictor/ensemble_lib.R"))
)

set.seed(42)
n_per_season <- 24L
raw <- tibble::tibble(
  season = rep(2015:2022, each = n_per_season),
  week = rep(seq_len(12L), length.out = 8L * n_per_season),
  weekday = rep(c("Sun", "Mon", "Thu"), length.out = 8L * n_per_season),
  div_game = rep(c(0L, 1L), length.out = 8L * n_per_season),
  home_moneyline = -110,
  x_signal = stats::rnorm(8L * n_per_season),
  x_noise = stats::rnorm(8L * n_per_season)
) %>%
  dplyr::mutate(
    home_result = 2.5 * x_signal + stats::rnorm(dplyr::n(), sd = 4),
    result = home_result
  )

train <- prep_for_train(raw)

# Uniform case weights must preserve the incumbent; malformed weights fail.
plain <- train_glm_model(train, seed=989, alpha=.25, lambda_rule="min")
uniform <- train_glm_model(train, seed=989, alpha=.25, lambda_rule="min",
                           observation_weights=rep(1,nrow(train)))
stopifnot(isTRUE(all.equal(plain$lambda,uniform$lambda)),
          isTRUE(all.equal(as.matrix(coef(plain$model,s="lambda.min")),
                           as.matrix(coef(uniform$model,s="lambda.min")))))
stopifnot(inherits(try(train_glm_model(train,observation_weights=c(-1,1)),
                      silent=TRUE),"try-error"))

incumbent <- train_glm_model(train, seed = 989, alpha = 0.25,
                             lambda_rule = "min", raw_data = raw)
stopifnot(
  identical(incumbent$diagnostics$lambda_rule, "min"),
  identical(incumbent$prediction_s, "lambda.min"),
  is.finite(incumbent$lambda)
)

# Bundles written before this experiment do not carry prediction_s. They must
# continue to use lambda.min and produce the same predictions.
legacy <- incumbent
legacy$prediction_s <- NULL
stopifnot(isTRUE(all.equal(
  predict_glm(incumbent, train),
  predict_glm(legacy, train),
  tolerance = 0
)))

one_se <- train_glm_model(train, seed = 989, alpha = 0.25,
                          lambda_rule = "1se", raw_data = raw)
repeated <- train_glm_model(train, seed = 989, alpha = 0.25,
                            lambda_rule = "repeat_median", cv_repeats = 3L,
                            raw_data = raw)
temporal <- train_glm_model(train, seed = 989, alpha = 0.25,
                            lambda_rule = "temporal", temporal_folds = 2L,
                            raw_data = raw)
fixed_prior <- train_glm_model(train, seed = 989, alpha = 0.25,
                               lambda_rule = "fixed_prior", prior_folds = 3L,
                               raw_data = raw)

stopifnot(
  one_se$lambda >= incumbent$lambda,
  length(repeated$diagnostics$selected_lambdas) == 3L,
  length(temporal$diagnostics$temporal_validation_seasons) == 2L,
  length(fixed_prior$diagnostics$prior_holdout_seasons) == 3L,
  all(vapply(list(one_se, repeated, temporal, fixed_prior), function(fit)
    all(is.finite(predict_glm(fit, train))), logical(1)))
)

cat("PASS: GLM lambda-selection rules\n")
