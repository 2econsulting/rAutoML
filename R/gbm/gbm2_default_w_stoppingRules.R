# title : gbm2_default_w_stoppingRules
# author : jacob
# desc :


gbm2_default_w_stoppingRules <- h2o.gbm(
  x = x,
  y = y,
  training_frame = train_hex,
  validation_frame = valid_hex,
  seed = 1234,
  stopping_rounds = stopping_rules$stopping_rounds,
  stopping_metric = stopping_rules$stopping_metric,
  stopping_tolerance = stopping_rules$stopping_tolerance,
  score_each_iteration = stopping_rules$score_each_iteration,
  ntrees = stopping_rules$ntrees,
  model_id = "gbm2",
  verbose = TRUE
)

cat(">> gbm2 done! \n")
