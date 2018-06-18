# title : H2OGBM_Default
# author : jacob
# desc :
h2o.rm("H2OGBM_Default")

H2OGBM_Default <- h2o.gbm(
  x = x,
  y = y,
  training_frame = train_hex,
  validation_frame = valid_hex,
  seed = 1234,
  model_id = "H2OGBM_Default",
  verbose = TRUE
)

cat(">> H2OGBM_Default done! \n")
