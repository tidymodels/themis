library(testthat)
library(recipes)
library(dplyr)

context("adasyn")

test_that("tunable", {
  rec <- recipe(~ ., data = iris) %>%
    step_adasyn(all_predictors(), under_ratio = 1)
  rec_param <- tunable.step_adasyn(rec$steps[[1]])
  expect_equal(rec_param$name, c("over_ratio", "neighbors"))
  expect_true(all(rec_param$source == "recipe"))
  expect_true(is.list(rec_param$call_info))
  expect_equal(nrow(rec_param), 2)
  expect_equal(
    names(rec_param),
    c("name", "call_info", "source", "component", "component_id")
  )
})

test_basic_usage(step_adasyn)
test_printing(step_adasyn)
test_bad_data(step_adasyn)
test_no_skipping(step_adasyn)
test_character_error(step_adasyn)
test_na_response(step_adasyn)
test_seed(step_adasyn)
test_tidy(step_adasyn)
test_over_ratio(step_adasyn)
test_multiclass(step_adasyn)
test_multi_majority(step_adasyn)
test_factor_level_memory(step_adasyn)
test_result_ordering(step_adasyn)
test_id_variables_are_ignores(step_adasyn)
