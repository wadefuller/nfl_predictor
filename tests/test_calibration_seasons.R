source("ensemble_lib.R")
g <- data.frame(season = rep(2023:2026, each = 2),
                home_result = c(1, -1, 1, -1, 1, -1, 1, NA_real_))
stopifnot(identical(complete_calibration_seasons(g), 2024:2025),
          identical(complete_calibration_seasons(g, 1L), 2025L))
cat("PASS: partial current season is excluded from calibration\n")
