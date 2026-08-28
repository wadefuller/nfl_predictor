# ============================================
# NFL Predictor: DNN POC (wide-and-deep w/ team embeddings)
# ============================================
# Standalone proof-of-concept BEFORE wiring into the ensemble.
# Goal: can a torch-based MLP with team/week/weekday embeddings produce
# spread predictions competitive with the GLM baseline on cal set?
#
# If MAE ≈ 9.5-9.8 and Brier ≈ 0.20 on 2024-2025, the architecture is
# viable as a 4th stacker base learner. If much worse, abandon.
#
# Architecture:
#   team_emb (32 → 8, shared home/away)
#   week_emb (22 → 3)
#   weekday_emb (7 → 2)
#   div_game (1 binary passed through)
#   + 334 numeric features (median-impute + standardize)
#   → concat (8+8+3+2+1+334 = 356)
#   → Dense(256) → ReLU → Dropout(0.2)
#   → Dense(128) → ReLU → Dropout(0.2)
#   → Dense(1)   regression head
#
# Train: 2008-2023, val 2024-2025, 50 epochs max with early stopping.
# Loss: MSE.  Optimizer: Adam(lr=1e-3).
# ============================================

suppressPackageStartupMessages({
  library(torch)
  library(dplyr)
  library(recipes)
})
source("~/Projects/R/nfl_predictor/ensemble_lib.R")

SEED          <- 989
EMB_TEAM_DIM  <- 4    # smaller embedding (was 8)
EMB_WEEK_DIM  <- 2    # smaller (was 3)
EMB_WKDAY_DIM <- 2
HIDDEN1       <- 64   # MUCH smaller (was 256)
HIDDEN2       <- 32   # MUCH smaller (was 128)
DROPOUT       <- 0.40 # heavier (was 0.20)
WEIGHT_DECAY  <- 1e-4 # L2 reg added
LR            <- 5e-4 # slower (was 1e-3)
BATCH_SIZE    <- 128  # larger (was 64) — smoother gradients
MAX_EPOCHS    <- 120
PATIENCE      <- 15
DEVICE        <- if (backends_mps_is_available()) "mps" else "cpu"

cat(sprintf("Device: %s\n", DEVICE))

# ---- Load + split ---------------------------------------------------------

cat("Loading features...\n")
g <- read.csv(file.path(DATA_DIR, "game_w_features.csv"))
g <- g %>% filter(!is.na(home_result), !is.na(home_moneyline),
                  (week <= 17 & season <= 2020) | (week <= 18 & season >= 2021),
                  season >= 2008)
train_raw <- g %>% filter(season <= 2023)
val_raw   <- g %>% filter(season >= 2024)
cat(sprintf("Train: %d rows  Val: %d rows\n", nrow(train_raw), nrow(val_raw)))

# ---- Build vocabularies from training set ---------------------------------

team_vocab    <- sort(unique(c(train_raw$home_team, train_raw$away_team)))
week_vocab    <- sort(unique(c(train_raw$week, val_raw$week)))   # weeks are tiny
weekday_vocab <- sort(unique(c(train_raw$weekday, val_raw$weekday)))
cat(sprintf("Vocab — teams:%d  weeks:%d  weekdays:%d\n",
            length(team_vocab), length(week_vocab), length(weekday_vocab)))

# Convert categorical → 1-indexed integer (torch embeddings expect 1-N).
# Unknown levels → fallback to last index (rare in this dataset; teams are stable).
encode <- function(x, vocab) {
  out <- match(x, vocab)
  out[is.na(out)] <- length(vocab) + 1L      # +1 reserved for "unknown"
  out
}
n_teams    <- length(team_vocab)    + 1L  # +1 for unknown
n_weeks    <- length(week_vocab)    + 1L
n_weekdays <- length(weekday_vocab) + 1L

# ---- Build numeric feature matrix ----------------------------------------
# Drop: leakage cols + the categorical cols we're embedding + non-feature ids.

DROP_FROM_NUMERIC <- c(
  LEAKAGE_VARS_REG,
  "home_team", "away_team", "week", "weekday", "season",
  # the team-dummies are explicit (audited dead in importance) — drop here too
  grep("^home_team_|^away_team_", names(g), value = TRUE)
)

num_train <- train_raw %>%
  select(-any_of(DROP_FROM_NUMERIC)) %>%
  select(where(is.numeric))
num_val   <- val_raw %>%
  select(-any_of(DROP_FROM_NUMERIC)) %>%
  select(where(is.numeric))

# Recipe: median-impute + standardize, fit on train, apply to val.
rec <- recipe(home_result ~ ., data = num_train) %>%
  step_impute_median(all_numeric_predictors()) %>%
  step_zv(all_numeric_predictors()) %>%
  step_normalize(all_numeric_predictors())
prep_rec <- prep(rec, training = num_train)
train_baked <- bake(prep_rec, new_data = num_train)
val_baked   <- bake(prep_rec, new_data = num_val)

X_train <- as.matrix(train_baked %>% select(-home_result))
y_train <- train_baked$home_result
X_val   <- as.matrix(val_baked %>% select(-home_result))
y_val   <- val_baked$home_result
n_features_numeric <- ncol(X_train)
cat(sprintf("Numeric feature dim (after recipe): %d\n", n_features_numeric))

# Categorical tensors
home_team_train    <- encode(train_raw$home_team, team_vocab)
away_team_train    <- encode(train_raw$away_team, team_vocab)
week_train         <- encode(train_raw$week,      week_vocab)
weekday_train      <- encode(train_raw$weekday,   weekday_vocab)
div_train          <- train_raw$div_game

home_team_val      <- encode(val_raw$home_team, team_vocab)
away_team_val      <- encode(val_raw$away_team, team_vocab)
week_val           <- encode(val_raw$week,      week_vocab)
weekday_val        <- encode(val_raw$weekday,   weekday_vocab)
div_val            <- val_raw$div_game

# ---- Internal val split for early stopping --------------------------------
set.seed(SEED)
inner_val_idx <- sample(seq_len(nrow(X_train)), size = floor(0.15 * nrow(X_train)))
inner_trn_idx <- setdiff(seq_len(nrow(X_train)), inner_val_idx)

# ---- Model definition -----------------------------------------------------
model <- nn_module(
  "WideDeep",
  initialize = function(n_features_numeric, n_teams, n_weeks, n_weekdays,
                         emb_team, emb_week, emb_wkday,
                         hidden1, hidden2, dropout) {
    self$team_emb    <- nn_embedding(n_teams,    emb_team)
    self$week_emb    <- nn_embedding(n_weeks,    emb_week)
    self$weekday_emb <- nn_embedding(n_weekdays, emb_wkday)
    in_dim <- n_features_numeric + 2*emb_team + emb_week + emb_wkday + 1L
    self$fc1     <- nn_linear(in_dim, hidden1)
    self$drop1   <- nn_dropout(p = dropout)
    self$fc2     <- nn_linear(hidden1, hidden2)
    self$drop2   <- nn_dropout(p = dropout)
    self$head    <- nn_linear(hidden2, 1)
  },
  forward = function(x_num, home_team, away_team, week, weekday, div_game) {
    h_emb <- self$team_emb(home_team)
    a_emb <- self$team_emb(away_team)
    w_emb <- self$week_emb(week)
    d_emb <- self$weekday_emb(weekday)
    z <- torch_cat(list(x_num, h_emb, a_emb, w_emb, d_emb,
                          div_game$unsqueeze(2)), dim = 2)
    z <- self$fc1(z); z <- nnf_relu(z); z <- self$drop1(z)
    z <- self$fc2(z); z <- nnf_relu(z); z <- self$drop2(z)
    self$head(z)$squeeze(2)
  }
)

# ---- Tensor builders ------------------------------------------------------

to_tensors <- function(idx, X, y, ht, at, wk, wd, dv) {
  list(
    x_num     = torch_tensor(X[idx, , drop = FALSE], dtype = torch_float()),
    home_team = torch_tensor(ht[idx], dtype = torch_long()),
    away_team = torch_tensor(at[idx], dtype = torch_long()),
    week      = torch_tensor(wk[idx], dtype = torch_long()),
    weekday   = torch_tensor(wd[idx], dtype = torch_long()),
    div_game  = torch_tensor(dv[idx], dtype = torch_float()),
    y         = torch_tensor(y[idx],  dtype = torch_float())
  )
}

trn <- to_tensors(inner_trn_idx, X_train, y_train,
                   home_team_train, away_team_train, week_train, weekday_train, div_train)
ivl <- to_tensors(inner_val_idx, X_train, y_train,
                   home_team_train, away_team_train, week_train, weekday_train, div_train)
val <- to_tensors(seq_len(nrow(X_val)), X_val, y_val,
                   home_team_val, away_team_val, week_val, weekday_val, div_val)

device <- torch_device(DEVICE)
# Move tensors to device
move <- function(lst) lapply(lst, function(t) t$to(device = device))
trn <- move(trn); ivl <- move(ivl); val <- move(val)

# ---- Train loop -----------------------------------------------------------
cat("\nBuilding model...\n")
torch_manual_seed(SEED)
net <- model(n_features_numeric, n_teams, n_weeks, n_weekdays,
              EMB_TEAM_DIM, EMB_WEEK_DIM, EMB_WKDAY_DIM,
              HIDDEN1, HIDDEN2, DROPOUT)$to(device = device)

opt   <- optim_adam(net$parameters, lr = LR, weight_decay = WEIGHT_DECAY)
loss_fn <- nnf_mse_loss

n_train <- length(trn$y)
batch_starts <- seq(1, n_train, by = BATCH_SIZE)

best_val_mae <- Inf
patience_left <- PATIENCE
best_state    <- NULL

cat(sprintf("\nTraining %d epochs (early-stop patience=%d, bs=%d)...\n",
            MAX_EPOCHS, PATIENCE, BATCH_SIZE))
t0 <- Sys.time()
for (epoch in seq_len(MAX_EPOCHS)) {
  net$train()
  set.seed(SEED + epoch)
  perm <- sample(n_train)
  ep_loss <- 0
  for (s in batch_starts) {
    e <- min(s + BATCH_SIZE - 1L, n_train)
    idx_t <- torch_tensor(perm[s:e], dtype = torch_long())$to(device = device)
    opt$zero_grad()
    pred <- net(trn$x_num[idx_t, ], trn$home_team[idx_t], trn$away_team[idx_t],
                trn$week[idx_t], trn$weekday[idx_t], trn$div_game[idx_t])
    loss <- loss_fn(pred, trn$y[idx_t])
    loss$backward()
    opt$step()
    ep_loss <- ep_loss + loss$item() * length(s:e)
  }
  ep_loss <- ep_loss / n_train
  # Val
  net$eval()
  with_no_grad({
    pv <- net(ivl$x_num, ivl$home_team, ivl$away_team,
              ivl$week, ivl$weekday, ivl$div_game)
    val_mae <- (pv - ivl$y)$abs()$mean()$item()
  })
  improved <- val_mae < best_val_mae - 1e-4
  if (improved) {
    best_val_mae <- val_mae
    patience_left <- PATIENCE
    best_state <- lapply(net$state_dict(), function(t) t$clone())
  } else {
    patience_left <- patience_left - 1L
  }
  if (epoch %% 5 == 0 || epoch == 1 || patience_left <= 0) {
    cat(sprintf("  epoch %2d  trn_loss=%.3f  inner_val_mae=%.3f  best=%.3f  patience_left=%d\n",
                epoch, ep_loss, val_mae, best_val_mae, patience_left))
  }
  if (patience_left <= 0) break
}
cat(sprintf("Trained in %.1f sec.\n", as.numeric(Sys.time() - t0)))

# Restore best
if (!is.null(best_state)) net$load_state_dict(best_state)

# ---- Score on val 2024-2025 -----------------------------------------------
net$eval()
with_no_grad({
  pred_val <- net(val$x_num, val$home_team, val$away_team,
                  val$week, val$weekday, val$div_game)
  pred_v <- as.numeric(pred_val$to(device = "cpu"))
})

mae_val <- mean(abs(pred_v - y_val))
rmse_val <- sqrt(mean((pred_v - y_val)^2))
net$eval()
with_no_grad({
  p <- net(trn$x_num, trn$home_team, trn$away_team,
           trn$week, trn$weekday, trn$div_game)
  pred_trn <- as.numeric(p$to(device = "cpu"))
})
sigma <- sd(y_train[inner_trn_idx] - pred_trn)
wp <- pnorm(pred_v / sigma)
win <- as.integer(y_val > 0)
brier <- mean((wp - win)^2)
cover_acc <- mean(sign(pred_v) == sign(y_val))

cat(sprintf("\n=== POC results on val 2024-2025 (%d games) ===\n", length(y_val)))
cat(sprintf("  MAE:    %.3f\n", mae_val))
cat(sprintf("  RMSE:   %.3f\n", rmse_val))
cat(sprintf("  Brier:  %.4f  (σ=%.2f)\n", brier, sigma))
cat(sprintf("  Cover:  %.3f\n", cover_acc))

cat("\n--- Reference (existing ensemble on same 544-game cal set) ---\n")
cat("  GLM alone:   MAE 9.59  Brier 0.2041\n")
cat("  Full stack:  MAE 9.59  Brier 0.2016\n")
cat("  Vegas:       MAE ~9.78 Brier ~0.2103\n")
