library(testthat)

context("Test the different patterns that can be created to replace occurrences")

test_that("Pattern object works as expected", {
  str <- "ETA_KA + THETA_KA + ETA_KA + ETA_KA3 + ETA_KA"
  pattern <- Pattern("ETA_KA")
  expect_equal(pattern %>% as.character(), "ETA_KA")
  expect_equal(str %>% replace_all(pattern, "ETA_KA2"), "ETA_KA2 + THETA_KA2 + ETA_KA2 + ETA_KA23 + ETA_KA2")

  str <- "HELLO"
  expect_equal(str %>% replace_all(pattern, ""), "HELLO")
})

test_that("Variable pattern object works as expected", {
  str <- "ETA_KA + THETA_KA + ETA_KA + ETA_KA3 + ETA_KA"
  pattern <- VariablePattern("ETA_KA")
  expect_equal(pattern %>% as.character(), "ETA_KA")
  expect_equal(str %>% replace_all(pattern, "ETA_KA2"), "ETA_KA2 + THETA_KA + ETA_KA2 + ETA_KA3 + ETA_KA2")

  str <- "HELLO"
  expect_equal(str %>% replace_all(pattern, ""), "HELLO")

  str <- "ABS*ABS"
  expect_equal(str %>% replace_all(VariablePattern("ABS"), "A_ABS"), "A_ABS*A_ABS")
})

test_that("Replace occurrences in model works as expected", {
  model <- model_suite$testing$nonmem$advan1_trans1 %>%
    add(LineBreak()) %>%
    add(Comment("Check replacement also works in IF-statement")) %>%
    add(IfStatement("K==1", Equation("XX", "K*10"))) %>%
    add(UnknownStatement("THIS IS AN UNKNOWN STATEMENT THAT CONTAINS THE VARIABLE K"))
  model <- model %>% replace_all("K", "K2")

  expect_equal(model %>% find(Equation("K2")), Equation("K2", "THETA_K*exp(ETA_K)"))
  expect_equal(model %>% find(Ode("A_CENTRAL")), Ode("A_CENTRAL", "-K2*A_CENTRAL"))
  expect_equal(model %>% find(Ode("A_OUTPUT")), Ode("A_OUTPUT", "K2*A_CENTRAL"))
  expect_equal(model %>% find(IfStatement("K2==1", Equation("XX"))), IfStatement("K2==1", Equation("XX", "K2*10")))

  main <- model %>% find(MainRecord())
  lastStatement <- main@statements@list[[length(main@statements@list)]]
  expect_equal(lastStatement, UnknownStatement("THIS IS AN UNKNOWN STATEMENT THAT CONTAINS THE VARIABLE K2"))
})

test_that("Function replace_all also replaces occurrences in compartment properties", {
  model <- model_suite$pk$'1cpt_fo'

  model <- model %>%
    replace_all("BIO", "BIO2")

  # Make sure BIO2 equation is there
  expect_equal(model %>% campsismod::find(Equation("BIO2")), Equation("BIO2", "TVBIO"))

  # Make sure compartment property was updated well
  expect_equal(model %>% find(Bioavailability(compartment = 1)), Bioavailability(compartment = 1, rhs = "BIO2"))
})
