library(testthat)

context("Test the conversion to nlmixr2")

source(file.path(getwd(), test_path(), "test-utils.R"))

endpoints_model <- function() {
  return(CampsisModel(json = file.path(getwd(), test_path(), "json_examples", "1cpt_fo_endpoints_v2.json")))
}

endpoint_model <- function(error_model) {
  thetaAdd <- if (error_model == "proportional") NA else "ADD_RUV"
  thetaProp <- if (error_model == "additive") NA else "PROP_RUV"
  endpoint <- Endpoint("CONC", error_model, sigma = "RUV", theta_add = thetaAdd, theta_prop = thetaProp)
  return(endpoints_model() %>% replace(endpoint))
}

# Additive and proportional models leave one residual error THETA unused
export_endpoint_model <- function(error_model) {
  unused <- switch(error_model, additive = "THETA_PROP_RUV", proportional = "THETA_ADD_RUV", NA)
  expect_warning(
    nlmod <- endpoint_model(error_model) %>% export(dest = "nlmixr2"),
    regexp = if (is.na(unused)) NA else sprintf("Parameter\\(s\\) %s are not used", unused)
  )
  return(nlmod)
}

test_that("Export method works (endpoints, un-prefixed compartments)", {
  nlmod <- endpoints_model() %>% export(dest = "nlmixr2")
  expect_s4_class(nlmod, "nlmixr_model")

  omegaVar <- as.character(log(0.25^2 + 1))
  expect_equal(
    nlmod@ini,
    c(
      "THETA_BIO <- 1; label(\"Bioavailability\")",
      "THETA_KA <- 1; label(\"Absorption rate\")",
      "THETA_VC <- 10; label(\"Volume of central compartment\")",
      "THETA_CL <- 3; label(\"Clearance\")",
      "THETA_ADD_RUV <- 0.05; label(\"Additive residual error (SD)\")",
      "THETA_PROP_RUV <- 0.1; label(\"Proportional residual error (SD)\")",
      paste0("ETA_KA ~ ", omegaVar),
      paste0("ETA_VC ~ ", omegaVar),
      paste0("ETA_CL ~ ", omegaVar)
    )
  )

  expect_true(
    all(
      c(
        "d/dt(ABS)=-KA * ABS # Absorption depot",
        "d/dt(CENTRAL)=KA * ABS - (CL / VC) * CENTRAL # Central compartment",
        "CONC=CENTRAL / VC # Plasma concentration",
        "f(ABS)=BIO",
        "lag(ABS)=0.5",
        "CENTRAL(0)=0"
      ) %in%
        nlmod@model
    )
  )
  expect_false(any(grepl("\\bA_|EPS_|CONC_ERR", nlmod@model)))

  nlmixr_non_reg_test(nlmod, "1cpt_fo_endpoints")
  expect_type(parse(text = nlmod %>% to_string()), "expression")
})

test_that("Endpoints are mapped to the nlmixr2 residual error syntax", {
  expected <- c(
    additive = "CONC ~ add(THETA_ADD_RUV)",
    proportional = "CONC ~ prop(THETA_PROP_RUV)",
    combined1 = "CONC ~ add(THETA_ADD_RUV) + prop(THETA_PROP_RUV) + combined1()",
    combined2 = "CONC ~ add(THETA_ADD_RUV) + prop(THETA_PROP_RUV) + combined2()"
  )
  for (errorModel in names(expected)) {
    nlmod <- export_endpoint_model(errorModel)
    expect_equal(nlmod@model[length(nlmod@model)], expected[[errorModel]])
    expect_match(nlmod %>% to_string(), expected[[errorModel]], fixed = TRUE)
  }
})

test_that("Generated code is understood by rxode2/nlmixr2", {
  skip_if_not_installed("rxode2")
  expectedErrType <- c(additive = "add", proportional = "prop", combined1 = "add + prop", combined2 = "add + prop")
  for (errorModel in names(expectedErrType)) {
    nlmod <- export_endpoint_model(errorModel)
    ui <- suppressMessages(rxode2::rxode2(eval(parse(text = nlmod %>% to_string()))))
    expect_equal(as.character(ui$predDf$var), "CONC")
    expect_equal(as.character(ui$predDf$errType), expectedErrType[[errorModel]])
    if (errorModel %in% c("combined1", "combined2")) {
      expect_equal(as.character(ui$predDf$addProp), errorModel)
    }
    expect_equal(ui$theta[["THETA_CL"]], 3)
    expect_equal(ui$state, c("ABS", "CENTRAL"))
  }
})

test_that("OMEGA blocks, fixed parameters and legacy SIGMAs are exported", {
  model <- model_suite$testing$nonmem$advan4_trans4 %>%
    replace(Theta(name = "KA", index = 1, value = 1, fix = TRUE)) %>%
    replace(Omega(name = "CL", index = 2, index2 = 2, value = 0.025, fix = TRUE)) %>%
    replace(Omega(name = "V2", index = 3, index2 = 3, value = 0.025, fix = TRUE)) %>%
    add(Omega(name = "CL_V2", index = 3, index2 = 2, value = 0.01, type = "covar"))

  expect_warning(
    nlmod <- model %>% export(dest = "nlmixr2"),
    regexp = "SIGMA\\(s\\) EPS_PROP are not attached to any endpoint"
  )
  expect_equal(
    nlmod@ini,
    c(
      "THETA_KA <- fix(1)",
      "THETA_CL <- 5",
      "THETA_V2 <- 80",
      "THETA_V3 <- 20",
      "THETA_Q <- 4",
      "ETA_KA ~ 0.025",
      "ETA_CL + ETA_V2 ~ fix(c(0.025, 0.01, 0.025))",
      "ETA_V3 ~ 0.025",
      "ETA_Q ~ 0.025"
    )
  )
  expect_true("d/dt(DEPOT)=-KA*DEPOT" %in% nlmod@model)
  expect_true("OBS_CP=CP*(0 + 1)" %in% nlmod@model)
})

test_that("Unused parameters are not exported", {
  model <- endpoints_model() %>%
    add(Theta(name = "UNUSED", index = 7, value = 1)) %>%
    add(Omega(name = "UNUSED", index = 4, index2 = 4, value = 0.1))
  expect_warning(
    nlmod <- model %>% export(dest = "nlmixr2"),
    regexp = "Parameter\\(s\\) THETA_UNUSED, ETA_UNUSED are not used"
  )
  expect_false(any(grepl("UNUSED", nlmod@ini)))
})

test_that("Endpoint SIGMA with a non-unit variance raises a warning", {
  model <- endpoints_model() %>% replace(Sigma(name = "RUV", index = 1, index2 = 1, value = 0.5))
  expect_warning(model %>% export(dest = "nlmixr2"), regexp = "SIGMA 'EPS_RUV' has a variance of 0.5")
})

test_that("Unnamed compartments keep their prefixed name", {
  model <- model_suite$literature$filgrastim_pkpd_krzyzanski
  nlmod <- suppressWarnings(model %>% export(dest = "nlmixr2"))
  expect_true(any(grepl("d/dt(A_1)", nlmod@model, fixed = TRUE)))
})

test_that("Unsupported destination gives an explicit error", {
  expect_error(
    model_suite$pk$`1cpt_fo` %>% export(dest = "nlmixr"),
    regexp = "Only rxode2 \\(previously RxODE\\), mrgsolve and nlmixr2 are currently supported"
  )
})
