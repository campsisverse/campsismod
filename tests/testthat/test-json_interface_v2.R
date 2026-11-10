library(testthat)

context("Test the JSON interface (version 2.0) and backward compatibility")

TEST_FOLDER <- file.path(getwd(), test_path())

json_example <- function(file) {
  return(file.path(TEST_FOLDER, "json_examples", file))
}

endpoints_model <- function() {
  return(CampsisModel(json = json_example("1cpt_fo_endpoints_v2.json")))
}

roundtrip_v2 <- function(model) {
  tmp <- tempfile(fileext = ".json")
  model %>% write(file = tmp, version = "2.0")
  return(read.campsis(tmp))
}

test_that("Legacy JSON files (no version or version 1.0) are loaded by the legacy parser", {
  expected <- model_suite$pk$`1cpt_fo`
  expect_equal(CampsisModel(json = json_example("1cpt_fo_model.json")), expected)

  json <- jsonlite::read_json(json_example("1cpt_fo_model.json"))
  json$version <- "1.0"
  expect_equal(CampsisModel(json = jsonlite::toJSON(json, auto_unbox = TRUE, digits = NA)), expected)

  # Default export remains version 1.0
  expect_null(export_to_json(expected)@data$version)
  expect_equal(export_to_json(expected)@data$code[1], "[MAIN]")
})

test_that("Legacy Campsis folders are still loaded", {
  model <- model_suite$pk$`1cpt_fo`
  folder <- file.path(tempdir(), "legacy_1cpt_fo")
  model %>% write(file = folder)
  expect_true(file.exists(file.path(folder, "model.campsis")))
  expect_equal(read.campsis(folder), model)
})

test_that("JSON schema is selected based on the version field", {
  expect_equal(basename(get_json_schema("1.0")), "campsismod_v1.schema.json")
  expect_equal(basename(get_json_schema("2.0")), "campsismod_v2.schema.json")
  expect_true(file.exists(get_json_schema("2.0")))
  expect_equal(get_json_version(list()), "1.0")
  expect_equal(get_json_version(list(version = "2.0")), "2.0")
  expect_error(CampsisModel(json = "{\"version\":\"3.0\"}"), regexp = "Unsupported Campsis JSON version '3.0'")

  # Empty models
  expect_equal(CampsisModel(json = "{\"version\":\"2.0\"}"), CampsisModel())

  # Legacy content tagged as version 2.0 is rejected by the v2 schema
  expect_error(
    CampsisModel(json = "{\"version\":\"2.0\", \"code\":[], \"parameters\":[]}"),
    regexp = "additionalProperties"
  )
})

test_that("Version 2.0 schema rejects invalid content", {
  base <- jsonlite::read_json(json_example("1cpt_fo_endpoints_v2.json"))
  to_json <- function(x) as.character(jsonlite::toJSON(x, auto_unbox = TRUE, digits = NA))

  # 'endpoints' and 'error' are mutually exclusive
  json <- base
  json$error <- list("CONC_ERR=CONC")
  expect_error(CampsisModel(json = to_json(json)), regexp = "#/not", fixed = TRUE)

  # Legacy 'A_' prefix not allowed on compartment names
  json <- base
  json$ode[[1]]$compartment <- "A_ABS"
  expect_error(CampsisModel(json = to_json(json)), regexp = "compartment_name/not", fixed = TRUE)

  # Additive endpoint can't have a proportional theta
  json <- base
  json$endpoints[[1]]$error_model <- "additive"
  expect_error(CampsisModel(json = to_json(json)), regexp = "allOf/0/then", fixed = TRUE)

  # Same checks happen in R when schema validation is disabled
  json <- base
  json$ode[[1]]$compartment <- "A_ABS"
  expect_error(CampsisModel(json = json), regexp = "legacy 'A_' prefix")
  json <- base
  json$error <- list("CONC_ERR=CONC")
  expect_error(CampsisModel(json = json), regexp = "mutually exclusive")
  json <- base
  json$f <- list(DEPOT = "BIO")
  expect_error(CampsisModel(json = json), regexp = "Compartment undefined: 'DEPOT' in 'f'")
})

test_that("Version 2.0 JSON with raw error block is loaded into the same S4 model", {
  model <- CampsisModel(json = json_example("1cpt_fo_model_v2.json"))
  expect_s4_class(model, "campsis_model")
  expect_equal(model, model_suite$pk$`1cpt_fo`)
  expect_equal(read.campsis(json_example("1cpt_fo_model_v2.json")), model)
})

test_that("Version 2.0 JSON with endpoints is translated into a Campsis model", {
  model <- endpoints_model()
  expect_s4_class(model, "campsis_model")

  # Compartments get the 'A_' prefix in the S4 model
  expect_equal(model@compartments@list %>% purrr::map_chr(~ .x %>% to_string()), c("A_ABS", "A_CENTRAL"))
  expect_equal(
    model %>% find(Ode("A_CENTRAL")),
    Ode("A_CENTRAL", "KA * A_ABS - (CL / VC) * A_CENTRAL", comment = "Central compartment")
  )
  expect_equal(model %>% find(Equation("CONC")), Equation("CONC", "A_CENTRAL / VC", comment = "Plasma concentration"))

  # Comments, blank lines and IF-statements
  main <- model %>% find(MainRecord())
  expect_equal(main@statements@list[[1]], Comment("--- Typical parameters ---"))
  expect_equal(main@statements@list[[6]], LineBreak())
  expect_equal(model %>% find(Equation("TVCL")), Equation("TVCL", "THETA_CL", comment = "Typical clearance"))
  ifStatement <- main@statements@list %>% purrr::detect(~ is(.x, "if_statement"))
  expect_equal(
    ifStatement,
    IfStatement("TIME > 12", Equation("KA", "TVKA * 1.5 * exp(ETA_KA)"), comment = "Time-dependent absorption switch")
  )

  # Compartment properties
  expect_equal(model %>% find(Bioavailability(1)) %>% slot("rhs"), "BIO")
  lag <- model %>% find(LagTime(1))
  expect_equal(c(lag@rhs, lag@comment), c("0.5", "Absorption lag"))
  expect_equal(model %>% find(InitialCondition(2)) %>% slot("rhs"), "0")

  # Endpoint
  endpoint <- model %>% find(Equation("CONC_ERR"))
  expect_s4_class(endpoint, "endpoint")
  expect_equal(
    endpoint,
    Endpoint(
      "CONC",
      "combined2",
      sigma = "RUV",
      theta_add = "ADD_RUV",
      theta_prop = "PROP_RUV",
      comment = "Observed concentration"
    )
  )
})

test_that("Version 2.0 models round-trip through JSON", {
  model <- endpoints_model()

  # In memory
  json <- model %>% export_to_json(version = "2.0")
  expect_equal(json@data$version, "2.0")
  expect_equal(names(json@data$f), "ABS")
  expect_equal(json@data$ode[[2]]$compartment, "CENTRAL")
  expect_equal(json@data$ode[[2]]$rate, "KA * ABS - (CL / VC) * CENTRAL")
  expect_null(json@data$error)
  expect_equal(json@data$endpoints[[1]]$error_model, "combined2")
  expect_equal(CampsisModel(json = json@data), model)

  # Through a file, validated against the v2 schema
  expect_equal(roundtrip_v2(model), model)

  # Same JSON content as the original file
  original <- jsonlite::read_json(json_example("1cpt_fo_endpoints_v2.json"))
  exported <- jsonlite::parse_json(jsonlite::toJSON(json@data, auto_unbox = TRUE, digits = NA))
  expect_equal(
    exported[c("version", "main", "ode", "f", "lag", "init", "endpoints")],
    original[c("version", "main", "ode", "f", "lag", "init", "endpoints")]
  )

  # Legacy models
  for (model in list(
    model_suite$pk$`1cpt_fo_transit`,
    model_suite$pk$`2cpt_fo_lag_mixed`,
    model_suite$testing$other$`2cpt_zo_allo_metab_effect_on_cl`,
    model_suite$testing$nonmem$advan4_trans4,
    model_suite$tmdd$`2cpt_fo_tmdd_full`
  )) {
    expect_equal(roundtrip_v2(model), model)
  }

  # Variance-covariance matrix and covariates
  model <- model_suite$pk$`1cpt_fo` %>%
    add(Omega(name = "CL_VC", index = 3, index2 = 2, value = 0.5, type = "cor")) %>%
    add_rse(Theta(name = "CL"), value = 10) %>%
    add_rse(Omega(name = "CL"), value = 50) %>%
    add(ContinuousCovariateDefinition(name = "WT", default_value = 70, unit = "kg")) %>%
    campsismod::sort()
  expect_equal(roundtrip_v2(model), model)
})

test_that("Legacy error block in version 2.0 uses compartment names without prefix", {
  json <- model_suite$pk$`1cpt_fo` %>% export_to_json(version = "2.0")
  expect_equal(unlist(json@data$error)[1], "CONC=CENTRAL/VC")
  expect_null(json@data$endpoints)

  # A single error line is still exported as an array
  model <- model_suite$pk$`1cpt_fo` %>% replace(ErrorRecord("CONC_ERR=A_CENTRAL/VC"))
  tmp <- tempfile(fileext = ".json")
  model %>% write(file = tmp, version = "2.0")
  expect_true(any(grepl("\"error\": \\[", readLines(tmp))))
  expect_equal(read.campsis(tmp), model)
})

test_that("Endpoints generate the expected error code", {
  rhs <- function(...) Endpoint("CONC", ..., sigma = "RUV")@rhs
  expect_equal(rhs("additive", theta_add = "A"), "CONC + THETA_A*EPS_RUV")
  expect_equal(rhs("proportional", theta_prop = "P"), "CONC*(1 + THETA_P*EPS_RUV)")
  expect_equal(rhs("combined1", theta_add = "A", theta_prop = "P"), "CONC + (THETA_A + THETA_P*CONC)*EPS_RUV")
  expect_equal(
    rhs("combined2", theta_add = "A", theta_prop = "P"),
    "CONC + sqrt(THETA_A*THETA_A + (THETA_P*CONC)*(THETA_P*CONC))*EPS_RUV"
  )
  expect_equal(Endpoint("CONC", "additive", sigma = "RUV", theta_add = "A", output = "Y")@lhs, "Y")
  expect_error(Endpoint("CONC", "exponential", sigma = "RUV"), regexp = "error_model must be one of")
  expect_error(Endpoint("CONC", "additive", sigma = "RUV"), regexp = "theta_add must be provided")
  expect_error(
    Endpoint("CONC", "additive", sigma = "RUV", theta_add = "A", theta_prop = "P"),
    regexp = "theta_prop must be NA"
  )
})

test_that("Endpoints are compiled into an [ERROR] block before rxode2/mrgsolve export", {
  model <- endpoints_model()
  expectedEquation <- "CONC_ERR=CONC + sqrt(THETA_ADD_RUV*THETA_ADD_RUV + (THETA_PROP_RUV*CONC)*(THETA_PROP_RUV*CONC))*EPS_RUV"

  compiled <- compile_endpoints(model)
  errorStatement <- (compiled %>% find(ErrorRecord()))@statements@list[[1]]
  expect_equal(class(errorStatement)[[1]], "equation")
  expect_equal(errorStatement %>% to_string(), paste0(expectedEquation, " # Observed concentration"))

  rxmod <- model %>% export(dest = "rxode2")
  expect_true(paste0(expectedEquation, " # Observed concentration") %in% rxmod@code)
  expect_equal(rxmod, compiled %>% export(dest = "rxode2"))

  mrgmod <- model %>% export(dest = "mrgsolve", outvars = c("CONC_ERR", "KA"))
  expect_equal(mrgmod@table[2], paste0("capture ", expectedEquation, "; // Observed concentration"))
  expect_equal(mrgmod@capture, "[CAPTURE] KA")

  # Legacy formats contain the translated code
  expect_true(paste0(expectedEquation, " # Observed concentration") %in% export_to_json(model)@data$code)
})

test_that("Endpoint parameter references are checked when loading", {
  json <- jsonlite::read_json(json_example("1cpt_fo_endpoints_v2.json"))

  missingTheta <- json
  missingTheta$endpoints[[1]]$theta_add <- "UNKNOWN"
  expect_error(CampsisModel(json = missingTheta), regexp = "THETA 'UNKNOWN' not found")

  missingSigma <- json
  missingSigma$endpoints[[1]]$sigma <- "UNKNOWN"
  expect_error(CampsisModel(json = missingSigma), regexp = "SIGMA 'UNKNOWN' not found")

  nonUnitSigma <- json
  nonUnitSigma$parameters[[10]]$value <- 0.1
  expect_warning(CampsisModel(json = nonUnitSigma), regexp = "should have a unit variance")

  sdSigma <- json
  sdSigma$parameters[[10]]$var_type <- "sd"
  expect_warning(CampsisModel(json = sdSigma), regexp = NA)
})

test_that("Export to version 2.0 refuses models that can't be represented", {
  # Unnamed compartments
  model <- model_suite$literature$filgrastim_pkpd_krzyzanski
  expect_error(model %>% export_to_json(version = "2.0"), regexp = "have no name")

  # Variable name colliding with a compartment once the prefix is dropped
  model <- model_suite$pk$`1cpt_fo` %>% add(Equation("CENTRAL", "A_CENTRAL"))
  expect_error(model %>% export_to_json(version = "2.0"), regexp = "CENTRAL collide")

  # Unknown statements
  model <- model_suite$pk$`1cpt_fo` %>% add(UnknownStatement("hello"))
  expect_error(model %>% export_to_json(version = "2.0"), regexp = "Unknown statement 'hello'")

  expect_error(
    model_suite$pk$`1cpt_fo` %>% export_to_json(version = "3.0"),
    regexp = "Unsupported Campsis JSON version"
  )
})

test_that("Endpoints mixed with other error statements are exported as raw code", {
  model <- endpoints_model() %>%
    add(Equation("CONC_LOG", "log(CONC)"), pos = Position(ErrorRecord()))
  expect_warning(json <- model %>% export_to_json(version = "2.0"), regexp = "exported as raw code")
  expect_null(json@data$endpoints)
  expect_equal(length(json@data$error), 2)
})

test_that("add_suffix and replace_all keep endpoints consistent", {
  model <- endpoints_model() %>% add_suffix("DRUG")
  endpoint <- model %>% find(Equation("CONC_ERR_DRUG"))
  expect_s4_class(endpoint, "endpoint")
  expect_equal(
    c(endpoint@variable, endpoint@theta_add, endpoint@theta_prop, endpoint@sigma),
    c("CONC_DRUG", "ADD_RUV_DRUG", "PROP_RUV_DRUG", "RUV_DRUG")
  )
  expect_equal(roundtrip_v2(model), model)
})
