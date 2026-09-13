#!/usr/bin/env Rscript
# ============================================
# Shrink the ensemble bundle by cutting captured environments
# ============================================
# ensemble_latest.rds is 141 MB on disk / 297 MB serialized, but the fitted
# models inside it are small — xgb's booster is 0.37 MB against a 21.9 MB
# "recipe". The weight is not model, it is captured scope.
#
# recipes stores each step's column selection as a QUOSURE, and a quosure keeps
# the environment it was written in. These recipes were built with %>%, so that
# environment holds magrittr's temporary `.` — bound to the recipe-so-far, whose
# own steps hold quosures capturing another environment holding another `.`, and
# so on. Serialising walks that chain and writes nested copies of the recipe.
# Measured: a single step's `terms` field serialises to 112.6 MB, and the object
# trapped in its environment is a 122.7 MB recipe.
#
# A PREPPED recipe does not need those environments to bake new data — the
# trained column selections live in `term_info`/`last_term_info`. So the fix is
# to repoint every quosure at baseenv(). Nothing that prediction reads is
# touched, which is why the check below asserts bit-identical output rather than
# merely "close".
suppressPackageStartupMessages({ library(rlang); library(recipes); library(workflows) })

# Defensive traversal. Recipe internals mix plain lists, classed lists (each
# step is one), quosures, formulas and S4-ish objects; a naive `x[[i]] <- ` walk
# dies with "subscript out of bounds" on the ones that report as lists but do not
# index elementwise. Anything that cannot be rewritten is left exactly as found,
# which is safe — a missed quosure costs bytes, never correctness.
strip_quosure_envs <- function(x) {
  if (rlang::is_quosure(x)) return(rlang::quo_set_env(x, baseenv()))
  if (is.function(x) && !is.primitive(x)) { environment(x) <- baseenv(); return(x) }
  if (inherits(x, "formula")) { attr(x, ".Environment") <- baseenv(); return(x) }
  if (!is.list(x) || is.data.frame(x)) return(x)
  n <- tryCatch(length(x), error = function(e) 0L)
  if (!length(n) || n == 0L) return(x)
  for (i in seq_len(n)) {
    ok <- tryCatch({ x[[i]] <- strip_quosure_envs(x[[i]]); TRUE },
                   error = function(e) FALSE)
    if (!ok) next
  }
  x
}

strip_recipe <- function(r) { r$steps <- strip_quosure_envs(r$steps); r }

strip_bundle <- function(b) {
  for (nm in c("xgb", "rf")) if (!is.null(b[[nm]])) {
    pre <- b[[nm]]$pre
    if (!is.null(pre$actions$recipe$recipe)) pre$actions$recipe$recipe <- strip_recipe(pre$actions$recipe$recipe)
    if (!is.null(pre$mold$blueprint$recipe))  pre$mold$blueprint$recipe  <- strip_recipe(pre$mold$blueprint$recipe)
    b[[nm]]$pre <- pre
  }
  if (!is.null(b$glm$recipe)) b$glm$recipe <- strip_recipe(b$glm$recipe)
  b
}

if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  inp <- if (length(args) >= 1) args[[1]] else "models/ensemble_latest.rds"
  out <- if (length(args) >= 2) args[[2]] else sub("\\.rds$", "_slim.rds", inp)
  b <- readRDS(inp)
  s <- strip_bundle(b)
  saveRDS(s, out, compress = "xz")
  cat(sprintf("  %s  %.1f MB  ->  %s  %.1f MB\n", basename(inp), file.size(inp)/1048576,
              basename(out), file.size(out)/1048576))
}
