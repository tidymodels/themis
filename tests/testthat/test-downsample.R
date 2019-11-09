library(testthat)
library(recipes)
library(dplyr)

context("Down-sampling")

iris2 <- iris[-c(1:45), ]
iris2$Species[seq(6, 96, by = 5)] <- NA
iris2$Species2 <- sample(iris2$Species)
iris2$Species3 <- as.character(sample(iris2$Species))

rec <- recipe(~ ., data = iris2)

test_that("ratio deprecation", {

  expect_message(
    new_rec <-
      rec %>%
      step_downsample(tidyselect::matches("Species$"), ratio = 2),
    "argument is now deprecated"
  )
  expect_equal(new_rec$steps[[1]]$under_ratio, 2)
})

test_that("tunable", {
  rec <-
    recipe(~ ., data = iris) %>%
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
