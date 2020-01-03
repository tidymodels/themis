library(testthat)
library(recipes)
library(dplyr)

set.seed(1234)

context("bsmote")

test_that("all minority classes are upsampled", {
  iris3 <- iris[-c(51:75, 101:110), ]

  out <- recipe(~ ., data = iris3) %>%
    step_bsmote(Species) %>%
    prep() %>%
    juice()

  expect_equal(as.numeric(table(out$Species)), c(50, 50, 50))
})

test_that("errors if there isn't enough danger data", {
  iris4 <- iris[-c(1:10), ]

  expect_error(
    recipe(~ ., data = iris4) %>%
      step_bsmote(Species) %>%
      prep(),
    "Not enough danger observations"
  )
})

test_that("tunable", {
  rec <-
    recipe(~ ., data = iris) %>%
    step_bsmote(all_predictors(), under_ratio = 1)
  rec_param <- tunable.step_bsmote(rec$steps[[1]])
  expect_equal(rec_param$name, c("over_ratio", "neighbors", "all_neighbors"))
  expect_true(all(rec_param$source == "recipe"))
  expect_true(is.list(rec_param$call_info))
  expect_equal(nrow(rec_param), 3)
  expect_equal(
    names(rec_param),
    c("name", "call_info", "source", "component", "component_id")
  )
})

test_basic_usage(step_bsmote)
test_basic_usage(step_bsmote, all_neighbors = TRUE)
test_printing(step_bsmote)
test_bad_data(step_bsmote)
test_no_skipping(step_bsmote)
test_character_error(step_bsmote)
test_na_response(step_bsmote)
test_seed(step_bsmote)
test_tidy(step_bsmote)
test_over_ratio(step_bsmote)
test_over_ratio(step_bsmote, all_neighbors = TRUE)
test_multiclass(step_bsmote)
test_multi_majority(step_bsmote)
test_multi_majority(step_bsmote, all_neighbors = TRUE)
