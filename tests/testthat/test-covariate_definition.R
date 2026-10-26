library(campsismod)

test_that("Continuous covariate definition can be created and added to model", {
  model <- CampsisModel()
  wt <- ContinuousCovariateDefinition(name = "WT", default_value = 70, label = "Weight", unit = "kg")
  model <- model %>%
    add(wt)

  expect_equal(model@covariates %>% length(), 1)
  expect_equal(model %>% find(wt), wt)
})

test_that("Categorical covariate definition can be created and added to model", {
  model <- CampsisModel()
  metab <- CategoricalCovariateDefinition(
    name = "METAB",
    default_value = 0,
    label = "Metaboliser",
    categories = c(Slow = 0, Fast = 1)
  )
  model <- model %>%
    add(metab)

  expect_equal(model@covariates %>% length(), 1)
  expect_equal(model %>% find(metab), metab)
})

test_that("Covariate definition can be deleted from model", {
  model <- CampsisModel()
  wt <- ContinuousCovariateDefinition(name = "WT", default_value = 70)
  model <- model %>%
    add(wt)
  expect_equal(model@covariates %>% length(), 1)

  model <- model %>%
    delete(wt)
  expect_equal(model@covariates %>% length(), 0)
})

test_that("Covariate definition can be replaced in model", {
  model <- CampsisModel()
  wt1 <- ContinuousCovariateDefinition(name = "WT", default_value = 70)
  wt2 <- ContinuousCovariateDefinition(name = "WT", default_value = 80)

  model <- model %>%
    add(wt1)
  model <- model %>%
    replace(wt2)

  expect_equal(model@covariates %>% length(), 1)
  expect_equal((model %>% find(wt2))@default_value, 80)
})

test_that("Model with covariate definitions can be exported to and loaded from JSON", {
  model <- CampsisModel()
  model <- model %>% add(MainRecord(c("CL=THETA_1")))
  wt <- ContinuousCovariateDefinition(name = "WT", default_value = 70, label = "Weight", unit = "kg")
  metab <- CategoricalCovariateDefinition(
    name = "METAB",
    default_value = 0,
    label = "Metaboliser",
    categories = c(Slow = 0, Fast = 1)
  )

  model <- model %>%
    add(wt) %>%
    add(metab)

  json <- export_to_json(model)

  model2 <- load_from_json(CampsisModel(), json)

  expect_equal(model2@covariates %>% length(), 2)
  expect_equal(model2 %>% find(wt), wt)
  expect_equal(model2 %>% find(metab), metab)
})
