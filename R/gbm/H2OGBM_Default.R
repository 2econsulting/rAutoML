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

autoGBM_Models['H2OGBM_Default'] <- list(h2o.getModel("H2OGBM_Default"))
h2o.auc(h2o.performance(H2OGBM_Default, newdata = test_hex))
saveRDS(autoGBM_Models['H2OGBM_Default'], file.path(model_path, "H2OGBM_Default.rda"))

cat(">> H2OGBM_Default done! \n")
