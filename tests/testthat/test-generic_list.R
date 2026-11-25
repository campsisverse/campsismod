library(testthat)

context("Test the generic list")

source(file.path(getwd(), test_path(), "test-utils.R"))

test_that("get_by_index method works well", {
  model <- model_suite$testing$nonmem$advan4_trans4

  depot <- model@compartments %>% get_by_index(1)
  expect_equal(depot@name, "A_DEPOT")

  expect_error(model@compartments %>% get_by_index(5), regexp = "Can't find element at index 5")
})

test_that("Generic methods should throw an error when the call is incorrect", {
  msg <- "No default function is provided"
  expect_error(get_prefix(""), regexp = msg)
  expect_error(get_record_name(""), regexp = msg)
  expect_error(replace(""), regexp = msg)
  expect_error(index_of(""), regexp = msg)
  expect_error(get_by_name(""), regexp = msg)
  expect_error(contains(""), regexp = msg)
  expect_error(find(""), regexp = msg)
  expect_error(get_names(""), regexp = msg)
  expect_error(get_by_index(""), regexp = msg)
  expect_error(sort(""), regexp = msg)
  expect_error(default(""), regexp = msg)
  expect_error(get_name(""), regexp = "No 'get_name' method defined for class 'character'")
})
