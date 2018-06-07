# title : gbm1_default
# author : jacob
# desc :


gbm1_default <- h2o.gbm(
  x = x,
  y = y,
  training_frame = train_hex,
  validation_frame = valid_hex,
  seed = 1234,
  model_id = "gbm1",
  verbose = TRUE
)

cat(">> gbm1 done! \n")
