#' @title stopping_rules
#' @description list, stopping_rules object
#' @export
stopping_rules <- list(
  stopping_rounds = 3,
  stopping_metric = "logloss",
  stopping_tolerance = 0.01,
  score_each_iteration = TRUE,
  ntrees = 10000
)

