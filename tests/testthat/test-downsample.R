library(testthat)
library(recipes)
library(dplyr)

context("Down-sampling")

test_that("ratio deprecation", {
  expect_message(
    new_rec <- recipe(~., data = circle_example) %>%
      step_downsample(class, ratio = 2),
    "argument is now deprecated"
  )
  expect_equal(new_rec$steps[[1]]$under_ratio, 2)
})

test_that("tunable", {
  rec <- recipe(~., data = iris) %>%
    step_downsample(all_predictors(), under_ratio = 1)
  rec_param <- tunable.step_downsample(rec$steps[[1]])
  expect_equal(rec_param$name, c("under_ratio"))
  expect_true(all(rec_param$source == "recipe"))
  expect_true(is.list(rec_param$call_info))
  expect_equal(nrow(rec_param), 1)
  expect_equal(
    names(rec_param),
    c("name", "call_info", "source", "component", "component_id")
  )
})

test_basic_usage(step_downsample)
test_printing(step_downsample)
test_bad_data(step_downsample)
test_no_skipping(step_downsample)
test_seed(step_downsample)
test_tidy(step_downsample)
test_under_ratio(step_downsample)
test_multiclass(step_downsample)
test_multi_minority(step_downsample)
test_factor_level_memory(step_downsample)
