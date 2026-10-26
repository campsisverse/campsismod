library(testthat)

context("Test the replication of the Campsis model")
source(file.path(getwd(), test_path(), "test-utils.R"))

# options(campsismod.options=list(SKIP_PERFORMANCE_TESTS=TRUE))

test_that("Method 'replicate' allows to replicate a model based on its variance-covariance matrix", {
  set.seed(123)

  model <- model_suite$pk$`1cpt_fo` %>%
    set_min_max(Theta(name = "KA"), min = 0.9, max = 1.1) %>%
    set_min_max(Theta(name = "CL"), min = 2.8, max = 3.2) %>%
    add_rse(Theta(name = "KA"), value = 10) %>%
    add_rse(Theta(name = "CL"), value = 10)

  repModel <- model %>% replicate(1000)

  # Model can be printed out in the console!
  print(repModel)

  model1 <- repModel %>% export(dest = CampsisModel(), index = 1)
  expect_equal(model1 %>% find(Theta(name = "KA")) %>% .@value, 1.09958, tolerance = 1e-4)
  expect_equal(model1 %>% find(Theta(name = "CL")) %>% .@value, 2.831857, tolerance = 1e-4)

  # The following test was moved from Campsis v1.6.0 to Campsismod v1.2.0

  set.seed(1)
  model <- model_suite$testing$other$my_model1
  repModel <- model %>% replicate(100)

  thetas <- repModel@replicated_parameters$THETA_CL
  var <- model@parameters@varcov["THETA_CL", "THETA_CL"]
  expect_equal(sd(thetas), sqrt(var), tolerance = 1e-2)

  omegas <- repModel@replicated_parameters$OMEGA_CL
  var <- model@parameters@varcov["OMEGA_CL", "OMEGA_CL"]
  expect_equal(sd(omegas), sqrt(var), tolerance = 1e-3)

  # Setting an impossible range will lead to an error
  set.seed(123)

  model <- model_suite$pk$`1cpt_fo` %>%
    set_min_max(Theta(name = "KA"), min = 1.3, max = 2) %>%
    add_rse(Theta(name = "KA"), value = 10)

  expect_error(repModel <- model %>% replicate(1000), msg = "Too many iterations")

  # Or may truncate the distribution as well if it is forced (here we increase the number of iterations)
  settings <- AutoReplicationSettings()
  settings@max_iterations <- 1000L
  repModel <- model %>% replicate(1000, settings = settings)
  # hist(repModel@replicated_parameters$THETA_KA)
  expect_equal(sum(repModel@replicated_parameters$THETA_KA >= 1.3), 1000)

  # Model can be printed out in the console!
  print(repModel)
})

getInverseWishartVar <- function(phiii, phijj, phiij, nu, p) {
  # See https://en.wikipedia.org/wiki/Inverse-Wishart_distribution
  num <- (nu - p + 1) * phiij^2 + (nu - p - 1) * phiii * phijj
  den <- (nu - p) * (nu - p - 1)^2 * (nu - p - 3)
  return(num / den)
}

getInverseWishartVar2 <- function(phi, nu, p) {
  return((2 * phi^2) / (nu - p - 1)^2 / (nu - p - 3))
}

getInverseWishartMean <- function(phi, nu, p) {
  # See https://en.wikipedia.org/wiki/Inverse-Wishart_distribution
  return(phi / (nu - p - 1))
}

getInverseChiSquaredVar <- function(tau2, nu) {
  # See https://en.wikipedia.org/wiki/Scaled_inverse_chi-squared_distribution
  return(2 * nu^2 * tau2^2 / (nu - 2)^2 / (nu - 4))
}

getInverseChiSquaredMean <- function(tau2, nu) {
  # See https://en.wikipedia.org/wiki/Scaled_inverse_chi-squared_distribution
  return(nu * tau2 / (nu - 2))
}

test_that("Sampling the OMEGAs and SIGMAs based on the scaled inverse chi-square or wishart distributions works as expected", {
  set.seed(123)

  model <- model_suite$testing$other$`2cpt_zo_allo_metab_effect_on_cl` %>%
    add(Omega(name = "VC_CL", index = 2, index2 = 3, value = 0.8, type = "cor")) %>%
    replace(Sigma(name = "RUV_FIX", value = 1, type = "var", fix = FALSE)) # Unfix the RUV_FIX just for the test

  # Previously, issue when printing the model, see #88
  expect_no_error(show(model))

  settings <- AutoReplicationSettings(wishart = TRUE, odf = 30, sdf = 1000, quiet = FALSE)
  repModel <- model %>%
    replicate(30000, settings = settings)

  # Standardise model first
  model <- model %>%
    standardise()
  nu <- settings@odf
  nuSigma <- settings@sdf
  p <- 2

  # See https://en.wikipedia.org/wiki/Scaled_inverse_chi-squared_distribution

  # Check the generated values for OMEGA_DUR
  omegaDur <- model %>% find(Omega(name = "DUR")) %>% .@value
  x <- repModel@replicated_parameters$OMEGA_DUR
  expect_equal(mean(x), getInverseChiSquaredMean(tau2 = omegaDur, nu = nu), tolerance = 0.0001)
  expect_equal(var(x), getInverseChiSquaredVar(tau2 = omegaDur, nu = nu), tolerance = 0.00001)

  # Check the generated values for OMEGA_VC
  omegaVc <- model %>% find(Omega(name = "VC")) %>% .@value
  x <- repModel@replicated_parameters$OMEGA_VC
  expect_equal(mean(x), getInverseWishartMean(phi = omegaVc * nu, nu = nu, p = p), tolerance = 0.0001)
  expect_equal(var(x), getInverseWishartVar2(phi = omegaVc * nu, nu = nu, p = p), tolerance = 0.00001)

  # Check the generated values for OMEGA_CL
  omegaCl <- model %>% find(Omega(name = "CL")) %>% .@value
  x <- repModel@replicated_parameters$OMEGA_CL
  expect_equal(mean(x), getInverseWishartMean(phi = omegaCl * nu, nu = nu, p = p), tolerance = 0.0001)
  expect_equal(var(x), getInverseWishartVar2(phi = omegaCl * nu, nu = nu, p = p), tolerance = 0.000001)

  # Check the generated values for OMEGA_VC (standardise parameter first!)
  omegaVcCl <- model %>% find(Omega(name = "VC_CL")) %>% standardise(parameters = model@parameters) %>% .@value
  x <- repModel@replicated_parameters$OMEGA_VC_CL
  expect_equal(mean(x), getInverseWishartMean(phi = omegaVcCl * nu, nu = nu, p = p), tolerance = 0.0001)
  expect_equal(
    var(x),
    getInverseWishartVar(phiii = omegaVc * nu, phijj = omegaCl * nu, phiij = omegaVcCl * nu, nu = nu, p = p),
    tolerance = 0.00001
  )

  # Check the generated values for RUV_FIX
  sigmaFix <- model %>% find(Sigma(name = "RUV_FIX")) %>% .@value
  x <- repModel@replicated_parameters$SIGMA_RUV_FIX
  expect_equal(mean(x), getInverseChiSquaredMean(tau2 = sigmaFix, nu = nuSigma), tolerance = 0.0005)
  expect_equal(var(x), getInverseChiSquaredVar(tau2 = sigmaFix, nu = nuSigma), tolerance = 0.00001)

  model1 <- repModel %>% export(dest = CampsisModel(), index = 1)
  expect_equal(model1 %>% find(Omega(name = "DUR")) %>% .@value, 0.01086832, tolerance = 1e-4)
  expect_equal(model1 %>% find(Omega(name = "VC")) %>% .@value, 0.07978391, tolerance = 1e-4)
  expect_equal(model1 %>% find(Omega(name = "CL")) %>% .@value, 0.05921943, tolerance = 1e-4)
  expect_equal(model1 %>% find(Omega(name = "VC_CL")) %>% .@value, 0.06040083, tolerance = 1e-4)

  model2 <- repModel %>% export(dest = CampsisModel(), index = 2)
  expect_equal(model2 %>% find(Omega(name = "DUR")) %>% .@value, 0.01469925, tolerance = 1e-4)
  expect_equal(model2 %>% find(Omega(name = "VC")) %>% .@value, 0.06764799, tolerance = 1e-4)
  expect_equal(model2 %>% find(Omega(name = "CL")) %>% .@value, 0.04674834, tolerance = 1e-4)
  expect_equal(model2 %>% find(Omega(name = "VC_CL")) %>% .@value, 0.04431662, tolerance = 1e-4)

  # Method replicate should also accept only 1 replicate (see issue #93)
  settings <- AutoReplicationSettings(wishart = TRUE, odf = 30, sdf = 1000, quiet = FALSE)

  model <- model_suite$testing$other$`2cpt_zo_allo_metab_effect_on_cl` %>%
    add(Omega(name = "VC_CL", index = 2, index2 = 3, value = 0.8, type = "cor")) %>%
    replace(Sigma(name = "RUV_FIX", value = 1, type = "var", fix = FALSE)) %>% # Unfix the RUV_FIX just for the test
    sort()

  repModel <- model %>%
    replicate(1, settings = settings)

  parameterNames <- model@parameters %>%
    get_names()

  expect_equal(c("REPLICATE", parameterNames), colnames(repModel@replicated_parameters))
})

test_that("Method 'set_min_max' can be used on THETAs, OMEGAs and SIGMAs to set limits", {
  set.seed(123)

  model1 <- model_suite$pk$`1cpt_fo` %>%
    set_min_max("theta", min = 0, max = Inf) %>%
    add_rse(Theta(name = "KA"), value = 100) # Very large RSE on KA

  repModel1 <- model1 %>% replicate(1000, settings = AutoReplicationSettings(quiet = FALSE))

  # No THETA_KA should be negative
  expect_true(all(repModel1@replicated_parameters$THETA_KA >= 0))

  # Same model but no limit
  model2 <- model_suite$pk$`1cpt_fo` %>%
    add_rse(Theta(name = "KA"), value = 100) # Very large RSE on KA

  repModel2 <- model2 %>% replicate(1000, settings = AutoReplicationSettings(quiet = FALSE))

  # 841 values of THETA_KA are positive
  # 159 values of THETA_KA are negative
  expect_equal(sum(repModel2@replicated_parameters$THETA_KA >= 0), 841)
  expect_equal(sum(repModel2@replicated_parameters$THETA_KA < 0), 159)
})

test_that("Replicate a model that has IOV works as expected (+ check performances) ", {
  # The following test was moved from Campsis v1.6.0 to Campsismod v1.2.0
  set.seed(123)
  model <- model_suite$testing$nonmem$advan2_trans2

  iovOmegas <- list() %>%
    append(Omega(name = "IOV_CL1", value = 0.025, type = "var", same = FALSE)) %>%
    append(Omega(name = "IOV_CL2", value = 0.025, type = "var", same = TRUE)) %>%
    append(Omega(name = "IOV_CL3", value = 0.025, type = "var", same = TRUE)) %>%
    append(Omega(name = "IOV_CL4", value = 0.025, type = "var", same = TRUE)) %>%
    append(Omega(name = "IOV_KA1", value = 0.030, type = "var", same = FALSE)) %>%
    append(Omega(name = "IOV_KA2", value = 0.030, type = "var", same = TRUE)) %>%
    append(Omega(name = "IOV_KA3", value = 0.030, type = "var", same = TRUE)) %>%
    append(Omega(name = "IOV_KA4", value = 0.030, type = "var", same = TRUE))

  pk <- model %>%
    add(iovOmegas) %>%
    add_rse(Omega(name = "IOV_CL1"), value = 10) %>%
    add_rse(Omega(name = "IOV_KA1"), value = 20)

  replicates <- 1000
  repModel <- pk %>% replicate(replicates)

  # Check performances on exporting the model
  start <- Sys.time()
  models <- seq_len(replicates) %>%
    purrr::map(~ repModel %>% export(dest = CampsisModel(), index = .x))
  end <- Sys.time()
  duration <- as.numeric(end - start)
  if (!skip_performance_tests()) {
    expect_true(duration < 15) # (about 3 seconds on my machine)
  }

  # Check performances on exporting the OMEGA matrices
  start <- Sys.time()
  matrices <- models %>%
    purrr::map(~ rxode_matrix(.x))
  end <- Sys.time()
  duration <- as.numeric(end - start)
  if (!skip_performance_tests()) {
    expect_true(duration < 5) # (about 1 seconds on my machine)
  }

  pk1 <- models[[1]]
  pk2 <- models[[2]]

  set <- iovOmegas %>% purrr::map_dbl(~ pk %>% find(Omega(.x@name)) %>% .@value)
  set1 <- iovOmegas %>% purrr::map_dbl(~ pk1 %>% find(Omega(.x@name)) %>% .@value)
  set2 <- iovOmegas %>% purrr::map_dbl(~ pk2 %>% find(Omega(.x@name)) %>% .@value)

  expect_equal(set, c(0.025, 0.025, 0.025, 0.025, 0.03, 0.03, 0.03, 0.03))
  expect_equal(round(set1, digits = 4), c(0.0275, 0.0275, 0.0275, 0.0275, 0.0266, 0.0266, 0.0266, 0.0266)) # Depends on seed
  expect_equal(round(set2, digits = 4), c(0.0276, 0.0276, 0.0276, 0.0276, 0.0286, 0.0286, 0.0286, 0.0286)) # Depends on seed

  # Check variance distribution on OMEGA IOV_CL_1
  iovCl1Omega <- models %>%
    purrr::map_dbl(~ .x %>% find(Omega("IOV_CL1")) %>% .@value)
  expect_equal(sd(iovCl1Omega), 0.0025, tolerance = 0.0005) # 10% RSE
  iovKa1Omega <- models %>%
    purrr::map_dbl(~ .x %>% find(Omega("IOV_KA1")) %>% .@value)
  expect_equal(sd(iovKa1Omega), 0.0060, tolerance = 0.0005) # 20% RSE
})

test_that("Method 'replicate' also allows to manually replicate a model based on a table (1 replicate/row)", {
  model <- model_suite$testing$nonmem$advan2_trans2
  data <- data.frame(REPLICATE = seq_len(5), SIGMA_PROP = c(0.1, 0.2, 0.3, 0.4, 0.5))

  expect_error(model %>% replicate(n = 6, settings = ManualReplicationSettings(data)))

  repModel <- model %>% replicate(n = 3, settings = ManualReplicationSettings(data))
  expect_equal(repModel@replicated_parameters$SIGMA_PROP, c(0.1, 0.2, 0.3))

  sigma1 <- repModel %>%
    export(dest = CampsisModel(), index = 1) %>%
    find(Sigma("PROP"))
  expect_equal(sigma1@value, 0.1)

  sigma2 <- repModel %>%
    export(dest = CampsisModel(), index = 2) %>%
    find(Sigma("PROP"))
  expect_equal(sigma2@value, 0.2)
  # Etc.
})

test_that("OMEGA's are correctly converted to block of OMEGA's (unfixed omegas, 1 correlation)", {
  parameters <- Parameters() %>%
    add(Theta(name = "CL", value = 5)) %>%
    add(Theta(name = "VC", value = 80)) %>%
    add(Theta(name = "DUR", value = 1)) %>%
    add(Theta(name = "PROP_RUV", value = 0.15)) %>%
    add(Omega(name = "CL", value = 30, type = "cv%")) %>%
    add(Omega(name = "VC", value = 30, type = "cv%")) %>%
    add(Omega(name = "DUR", value = 30, type = "cv%")) %>%
    add(Omega(index = 1, index2 = 2, value = 0.5, type = "cor"))

  # Create first block manually
  expectedBlock1 <- OmegaBlock() %>%
    add(parameters %>% find(Omega(name = "CL"))) %>%
    add(parameters %>% find(Omega(name = "VC"))) %>%
    add(parameters %>% find(Omega(index = 1, index2 = 2)))
  expectedBlock1@start_index <- 0L
  expectedBlock1@block_index <- 1L

  expect_equal(expectedBlock1 %>% get_omega_indexes(), c(1, 2))

  # Create all blocks automatically
  blocks <- OmegaBlocks() %>% add(parameters)

  expect_equal(blocks %>% length(), 2)

  block1 <- blocks %>% get_by_index(1)
  block2 <- blocks %>% get_by_index(2)
  expect_equal(block1, expectedBlock1)

  expect_true(block1 %>% length() == 2)
  expect_true(block2 %>% length() == 1)
  expect_true(block1 %>% has_off_diagonal_omegas())
  expect_true(!block2 %>% has_off_diagonal_omegas())

  expect_true(block1@start_index == 0)
  expect_true(block2@start_index == 2)

  expect_equal(capture_output(show(block1)), "BLOCK(2) - OMEGA_CL / OMEGA_VC")
  expect_equal(capture_output(show(block2)), "BLOCK(1) - OMEGA_DUR")

  # Generic functions not called correctly
  msg <- "No default function is provided"
  expect_error(get_omega_indexes(""), regexp = msg)
  expect_error(has_off_diagonal_omegas(""), regexp = msg)
  expect_error(shift_omega_indexes(""), regexp = msg)
})

test_that("Method 'show' called on replication settings works as expected", {
  manualSettings <- ManualReplicationSettings(data.frame(REPLICATE = 1:5, THETA_KA = 1:5))
  expect_equal(capture_output(show(manualSettings)), "Replication settings: manual")

  defaultSettings <- AutoReplicationSettings()
  expect_equal(capture_output(show(defaultSettings)), "Replication settings: default (auto)")

  adjustedSettings <- AutoReplicationSettings(wishart = TRUE, odf = 30, sdf = 1000)
  expect_equal(capture_output(show(adjustedSettings)), "Replication settings: wishart=TRUE, odf=30, sdf=1000")
})

test_that("Campsismod gives identical results to the simpar package", {
  set.seed(123)

  # Generate parameters with Campsismod
  model <- CampsisModel() %>%
    add(Theta(value = 5)) %>%
    add(Omega(value = 0.2, type = "sd")) %>%
    add(Omega(value = 0.4, type = "sd")) %>%
    add(Omega(index = 2, index2 = 1, value = 0.1^2, type = "covar")) %>%
    add(Sigma(value = 0.1, type = "sd")) %>%
    add_rse(Theta(index = 1), value = 4)

  res <- model %>%
    replicate(n = 10000, settings = AutoReplicationSettings(wishart = TRUE, odf = 30, sdf = 1000)) %>%
    .@replicated_parameters

  expect_equal(mean(res$THETA_1), 5, tolerance = 0.001)
  expect_equal(sd(res$THETA_1), 0.2, tolerance = 0.001)

  expect_equal(mean(res$OMEGA_1_1), 0.044, tolerance = 0.001)
  expect_equal(sd(res$OMEGA_1_1), 0.012, tolerance = 0.001)

  expect_equal(mean(res$OMEGA_2_1), 0.011, tolerance = 0.001)
  expect_equal(sd(res$OMEGA_2_1), 0.017, tolerance = 0.001)

  expect_equal(mean(res$OMEGA_2_2), 0.177, tolerance = 0.001)
  expect_equal(sd(res$OMEGA_2_2), 0.049, tolerance = 0.001)

  expect_equal(mean(res$SIGMA_1_1), 0.010, tolerance = 0.001)
  expect_equal(sd(res$SIGMA_1_1), 0.00045, tolerance = 0.00001)

  # Generate parameters with simpar
  # theta <- c(5)
  # covar <- diag(0.2^2, 1)
  # omega <- matrix(c(0.2^2,0.1^2,0.1^2,0.4^2), nrow=2)
  # sigma <- matrix(0.1^2)
  #
  # res <- simpar::simpar(10000, theta, covar, omega, sigma, odf=30, sdf=1000) %>%
  #   tibble::as_tibble()
  #
  # mean(res$TH.1) # 5.000818
  # sd(res$TH.1) # 0.1994202
  #
  # mean(res$OM1.1) # 0.04449487
  # sd(res$OM1.1) # 0.0128251
  #
  # mean(res$OM2.1) # 0.01109515
  # sd(res$OM2.1) # 0.0176825
  #
  # mean(res$OM2.2) # 0.1771933
  # sd(res$OM2.2) # 0.0497492
  #
  # mean(res$SG1.1) # 0.01000736
  # sd(res$SG1.1) # 0.0004495882
})

test_that("Checking for positive definiteness works as expected", {
  no_of_replicates <- 250L

  set.seed(123)

  model <- model_suite$testing$other$`2cpt_zo_allo_metab_effect_on_cl` %>%
    add(Omega(name = "VC_CL", index = 2, index2 = 3, value = 0.8, type = "cor")) %>%
    replace(Sigma(name = "RUV_FIX", value = 1, type = "var", fix = FALSE)) # Unfix the RUV_FIX just for the test

  settings_a <- AutoReplicationSettings(wishart = FALSE, quiet = FALSE, check_pos_def = TRUE)
  rep_model_a <- model %>%
    replicate(no_of_replicates, settings = settings_a)

  set.seed(123)

  model <- model_suite$testing$other$`2cpt_zo_allo_metab_effect_on_cl` %>%
    add(Omega(name = "VC_CL", index = 2, index2 = 3, value = 0.8, type = "cor")) %>%
    replace(Sigma(name = "RUV_FIX", value = 1, type = "var", fix = FALSE)) # Unfix the RUV_FIX just for the test

  settings_b <- AutoReplicationSettings(wishart = FALSE, quiet = FALSE, check_pos_def = FALSE)
  rep_model_b <- model %>%
    replicate(no_of_replicates, settings = settings_b)

  # Compare manually
  # rep_model_a@replicated_parameters
  # rep_model_b@replicated_parameters

  # Check that at least one Omega matrix in rep_model_b is NOT positive definite
  # (This confirms the distribution is capable of producing invalid matrices)
  is_pd_b <- sapply(seq_len(no_of_replicates), function(i) {
    m <- rep_model_b %>% export(dest = CampsisModel(), index = i)
    is_matrix_positive_definite(rxode_matrix(m, type = "omega"))
  })
  expect_true(any(!is_pd_b), label = "At least one replicate in B should be non-PD")

  # Check that ALL Omega matrices in rep_model_a are positive definite
  # (This confirms that check_pos_def = TRUE successfully filtered them)
  is_pd_a <- sapply(seq_len(no_of_replicates), function(i) {
    m <- rep_model_a %>% export(dest = CampsisModel(), index = i)
    is_matrix_positive_definite(rxode_matrix(m, type = "omega"))
  })
  expect_true(all(is_pd_a), label = "All replicates in A should be PD")
})
