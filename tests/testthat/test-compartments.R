library(testthat)

context("Test all methods that may be applied on compartments")

test_that("Method 'add' can't be used to add a compartment (add an ODE instead)", {
  model <- model_suite$testing$nonmem$advan4_trans4
  expect_error(
    model %>% add(Compartment(index = model@compartments %>% length() + 1, name = "OUTPUT2")),
    regexp = "No default function is provided"
  )
})

test_that("Method 'contains' works with compartments", {
  model <- model_suite$testing$nonmem$advan4_trans4
  expect_true(model %>% contains(Compartment(1)))
  expect_false(model %>% contains(Compartment(5)))
})

test_that("Method 'delete' can't be used to delete a compartment (at least for now)", {
  model <- model_suite$testing$nonmem$advan4_trans4
  expect_error(model %>% delete(Compartment(index = 1)), regexp = "No default function is provided")
})

test_that("Method 'find' may return a compartment", {
  # Example 1
  model <- model_suite$testing$nonmem$advan4_trans4
  compartment <- model %>% find(Compartment(index = 2))

  expect_equal(compartment@index, 2)
  expect_equal(compartment@name, "A_CENTRAL")

  # Example 2
  model <- model_suite$testing$nonmem$advan1_trans1
  compartments <- model@compartments
  compartment1 <- compartments %>% find(Compartment(index = 1))
  compartment2 <- compartments %>% find(Compartment(index = 2))

  expect_equal(compartments %>% length(), 2)
  expect_equal(compartment1@name, "A_CENTRAL")
  expect_equal(compartment2@name, "A_OUTPUT")

  expect_equal(capture_output(show(compartment1)), "A_CENTRAL (CMT=1)")
  expect_equal(capture_output(show(compartment2)), "A_OUTPUT (CMT=2)")
})

test_that("Method 'get_compartment_index' works as expected", {
  # Legacy model: name with or without the 'A_' prefix
  model <- model_suite$testing$nonmem$advan4_trans4
  expect_equal(model %>% get_compartment_index("DEPOT"), 1)
  expect_equal(model %>% get_compartment_index("A_DEPOT"), 1)
  expect_equal(model %>% get_compartment_index("CENTRAL"), 2)
  expect_equal(model %>% get_compartment_index("A_CENTRAL"), 2)
  expect_error(model %>% get_compartment_index("XX"), regexp = "Compartment XX not found")

  # Unnamed compartments
  model <- model_suite$literature$filgrastim_pkpd_krzyzanski
  expect_equal(model %>% get_compartment_index("A_2"), 2)
  expect_equal(model %>% get_compartment_index("2"), 2)

  # Model without prefix
  model <- CampsisModel() %>%
    add(Ode("ABS", "-KA*ABS")) %>%
    add(Ode("CENTRAL", "KA*ABS - K*CENTRAL"))
  expect_equal(model %>% get_compartment_index("ABS"), 1)
  expect_equal(model %>% get_compartment_index("CENTRAL"), 2)
  expect_error(model %>% get_compartment_index("A_CENTRAL"), regexp = "Compartment A_CENTRAL not found")

  # Exact name has priority over the legacy fallback
  model <- CampsisModel() %>%
    add(Ode("A_CENTRAL", "-K*A_CENTRAL")) %>%
    add(Ode("CENTRAL", "-K*CENTRAL"))
  expect_equal(model %>% get_compartment_index("CENTRAL"), 2)
  expect_equal(model %>% get_compartment_index("A_CENTRAL"), 1)
})

test_that("Method 'replace' can be used to adapt the name of a compartment", {
  model <- model_suite$testing$nonmem$advan4_trans4 %>% add(Bioavailability(1, "0.75"))

  # Replace compartment and ODE
  model <- model %>%
    replace(Compartment(1, "A_ABS")) %>%
    delete(Ode("A_DEPOT")) %>%
    add(Ode("A_ABS", "-KA*ABS"), Position(Ode("A_CENTRAL"), after = FALSE))

  # Replace occurrence in CENTRAL compartment
  central <- model %>% find(Ode("A_CENTRAL"))
  central@rhs <- gsub(pattern = "A_DEPOT", replacement = "A_ABS", x = central@rhs)
  model <- model %>% replace(central)

  # Check compartment has been correctly replaced
  compartment <- model %>% find(Compartment(1))
  expect_equal(compartment@name, "A_ABS")

  # Check 2 first ODE's
  abs <- model %>% find(Ode("A_ABS"))
  central <- model %>% find(Ode("A_CENTRAL"))
  expect_equal(abs@rhs, "-KA*ABS")
  expect_equal(central@rhs, "KA*A_ABS + Q*A_PERIPHERAL/V3 + (-CL/V2 - Q/V2)*A_CENTRAL")

  # Check bioavailability equation will be adapted too
  expect_equal(model %>% find(Bioavailability(1)) %>% to_string(model = model, dest = "campsis"), "A_ABS=0.75")
})

test_that("Method 'to_string' works as expected on compartment", {
  cmt <- Compartment(index = 1)
  expect_equal(cmt %>% to_string(), "A_1")

  cmt <- Compartment(index = 1, name = "A_CENTRAL")
  expect_equal(cmt %>% to_string(), "A_CENTRAL")

  cmt <- Compartment(index = 1, name = "CENTRAL")
  expect_equal(cmt %>% to_string(), "CENTRAL")
})

test_that("Compartments are detected from ODEs, with or without the 'A_' prefix", {
  model <- CampsisModel() %>%
    add(Ode("A_1", "-K*A_1")) %>%
    add(Ode("CENTRAL", "-K*CENTRAL")) %>%
    add(Ode("A_PERIPH", "-K*A_PERIPH"))
  expect_equal(model@compartments@list %>% purrr::map_chr(~ .x@name), c(NA, "CENTRAL", "A_PERIPH"))
  expect_equal(model@compartments@list %>% purrr::map_chr(~ .x %>% to_string()), c("A_1", "CENTRAL", "A_PERIPH"))
})
