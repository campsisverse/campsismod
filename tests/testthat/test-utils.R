OVERWRITE_NON_REG_FILES <- FALSE

to_file <- function(code, path) {
  fileConn <- file(path)
  writeLines(code, fileConn)
  close(fileConn)
}

campsis_non_reg_path <- function(regFilename) {
  return(file.path(getwd(), test_path(), "non_regression", "campsis", regFilename))
}

mrgsolve_non_reg_path <- function(regFilename) {
  return(file.path(getwd(), test_path(), "non_regression", "mrgsolve", paste0(regFilename, ".txt")))
}

rxode_non_reg_path <- function(regFilename) {
  return(file.path(getwd(), test_path(), "non_regression", "rxode", paste0(regFilename, ".txt")))
}

nlmixr_non_reg_path <- function(regFilename) {
  return(file.path(getwd(), test_path(), "non_regression", "nlmixr", paste0(regFilename, ".txt")))
}

nlmixr_non_reg_test <- function(nlmod, regFilename) {
  nlmodCode <- nlmod %>% to_string()
  if (OVERWRITE_NON_REG_FILES) {
    to_file(nlmodCode, nlmixr_non_reg_path(regFilename))
  }
  expectedNlmodCode <- readLines(con = nlmixr_non_reg_path(regFilename)) %>% paste0(collapse = "\n")
  expect_equal(nlmodCode, expectedNlmodCode)
}

campsis_non_reg_test <- function(model, regFilename) {
  if (OVERWRITE_NON_REG_FILES) {
    model %>% write(file = campsis_non_reg_path(regFilename))
  }
  expectedModel <- read.campsis(file = campsis_non_reg_path(regFilename))
  expect_equal(model, expectedModel)
}

mrgsolve_non_reg_test <- function(mrgmod, regFilename) {
  mrgmodCode <- mrgmod %>% to_string()
  if (OVERWRITE_NON_REG_FILES) {
    to_file(mrgmodCode, mrgsolve_non_reg_path(regFilename))
  }
  expectedMrgmodCode <- readLines(con = mrgsolve_non_reg_path(regFilename)) %>% paste0(collapse = "\n")
  expect_equal(mrgmodCode, expectedMrgmodCode)
}

rxode_non_reg_test <- function(rxmod, regFilename) {
  rxmodCode <- rxmod@code %>% paste0(collapse = "\n")
  if (OVERWRITE_NON_REG_FILES) {
    to_file(rxmodCode, rxode_non_reg_path(regFilename))
  }
  expectedRxmodCode <- readLines(con = rxode_non_reg_path(regFilename)) %>% paste0(collapse = "\n")
  expect_equal(rxmodCode, expectedRxmodCode)
}

read_campsis_model_no_params <- function(file) {
  # Only first warning is actually checked
  model <- expect_warning(read.campsis(file), regexp = "No file '(theta|omega|sigma)\\.csv' could be found")
  return(model)
}

on_cran <- function() {
  # Copied from testthat:::on_cran()
  return(!interactive() && !env_var_is_true("NOT_CRAN"))
}

env_var_is_true <- function(x) {
  return(isTRUE(as.logical(Sys.getenv(x, "false"))))
}

skip_performance_tests <- function() {
  # On CRAN, default value is TRUE
  # FALSE otherwise
  return(get_campsismod_option(name = "SKIP_PERFORMANCE_TESTS", default = on_cran()))
}
