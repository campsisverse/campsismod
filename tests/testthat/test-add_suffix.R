library(testthat)

context("Test generic method add_suffix")

TEST_FOLDER <- file.path(getwd(), test_path())

test_that("Add suffix to parameters is working as expected", {
  parameters <- Parameters() %>%
    add(Theta(name = "CL", index = 1, fix = FALSE, value = 4)) %>%
    add(Theta(name = "V", index = 2, fix = FALSE, value = 40)) %>%
    add(Theta(name = "KA", index = 3, fix = FALSE, value = 0.5)) %>%
    add(Omega(name = "CL", index = 1, index2 = 1, fix = FALSE, value = 0.1)) %>%
    add(Omega(name = "V", index = 2, index2 = 2, fix = FALSE, value = 0.1)) %>%
    add(Omega(name = NA, index = 2, index2 = 1, fix = FALSE, value = 0.5, type = "cor"))
  parameters@varcov <- matrix(dimnames = list("THETA_CL", "THETA_CL"), data = 0.1)

  updatedParameters <- parameters %>%
    add_suffix("A")

  expectedParameters <- Parameters() %>%
    add(Theta(name = "CL_A", index = 1, fix = FALSE, value = 4)) %>%
    add(Theta(name = "V_A", index = 2, fix = FALSE, value = 40)) %>%
    add(Theta(name = "KA_A", index = 3, fix = FALSE, value = 0.5)) %>%
    add(Omega(name = "CL_A", index = 1, index2 = 1, fix = FALSE, value = 0.1)) %>%
    add(Omega(name = "V_A", index = 2, index2 = 2, fix = FALSE, value = 0.1)) %>%
    add(Omega(name = NA, index = 2, index2 = 1, fix = FALSE, value = 0.5, type = "cor"))
  expectedParameters@varcov <- matrix(dimnames = list("THETA_CL_A", "THETA_CL_A"), data = 0.1)

  expect_equal(updatedParameters, expectedParameters)

  # Separator can't be a dot for instance
  expect_error(parameters %>% add_suffix("A", separator = "."), regexp = "is not a valid separator")

  # Suffix can't contain a dot neither
  expect_error(parameters %>% add_suffix(".A"), regexp = "is not a valid suffix")
})

test_that("Add suffix to a code record is working as expected", {
  main <- MainRecord() %>%
    add(Equation("KA", "THETA_KA * exp(ETA_KA)")) %>%
    add(Equation("KA2", "2 * KA")) %>%
    add(Equation("TMP", "0")) %>%
    add(IfStatement("KA2 > 10", Equation("TMP", "1")))

  updatedMain <- main %>%
    add_suffix("A")

  expectedMain <- MainRecord() %>%
    add(Equation("KA_A", "THETA_KA * exp(ETA_KA)")) %>%
    add(Equation("KA2_A", "2 * KA_A")) %>%
    add(Equation("TMP_A", "0")) %>%
    add(IfStatement("KA2_A > 10", Equation("TMP_A", "1")))

  expect_equal(updatedMain, expectedMain)
})

test_that("Add suffix to compartments is working as expected", {
  compartments <- Compartments() %>%
    add(Compartment(index = 1, name = "ABS")) %>%
    add(Compartment(index = 2, name = "A_CENTRAL")) %>%
    add(Compartment(index = 3))

  updatedCompartments <- compartments %>%
    add_suffix("A")

  expectedCompartments <- Compartments() %>%
    add(Compartment(index = 1, name = "ABS_A")) %>%
    add(Compartment(index = 2, name = "A_CENTRAL_A")) %>%
    add(Compartment(index = 3, name = "A_3_A"))

  expect_equal(updatedCompartments, expectedCompartments)
})

test_that("Add suffix to a Campsis model is working as expected", {
  modelA <- model_suite$testing$pk$`1cpt_zo_abs_lag` %>%
    add_suffix("1", separator = "") %>%
    add(LineBreak())

  modelB <- model_suite$testing$pk$`1cpt_zo_abs_lag` %>%
    add_suffix("2", separator = "")

  modelAB <- modelA %>%
    add(modelB)

  expectedModelPath <- file.path(TEST_FOLDER, "custom", "model_suffix_ab")
  # modelAB %>% write(expectedModelPath)

  expectedModel <- read.campsis(expectedModelPath)
  expect_equal(modelAB, expectedModel)
  # library(campsis)
  # ds <- Dataset(50) %>%
  #   add(Infusion(time=0, amount=1000, compartment=1)) %>%
  #   add(Infusion(time=0, amount=2000, compartment=3)) %>%
  #   add(Observations(seq(0,24)))
  # results <- simulate(model=modelAB, dataset=ds, dest="mrgsolve", seed=1)
  # shadedPlot(results, "CONC1")
  # shadedPlot(results, "CONC2")
})
