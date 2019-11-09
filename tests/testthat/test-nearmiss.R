library(testthat)
library(recipes)
library(dplyr)

set.seed(1234)

context("nearmiss")

test_that("minority classes are ignored if there is more than 1", {
  rec1_p2 <- recipe(~., data = iris[-c(1:25, 51:75), ]) %>%
    step_nearmiss(Species, id = "") %>%
    prep() %>%
    juice()

  expect_true(all(max(table(rec1_p2$Species)) == 25))
})

test_that("tunable", {
  rec <-
    recipe(~ ., data = iris) %>%
    step_nearmiss(all_predictors(), under_ratio = 1)
  rec_param <- tunable.step_nearmiss(rec$steps[[1]])
  expect_equal(rec_param$name, c("under_ratio", "neighbors"))
  expect_true(all(rec_param$source == "recipe"))
  expect_true(is.list(rec_param$call_info))
  expect_equal(nrow(rec_param), 2)
  expect_equal(
    names(rec_param),
    c("name", "call_info", "source", "component", "component_id")
  )
})

test_basic_usage(step_nearmiss)
test_printing(step_nearmiss)
test_bad_data(step_nearmiss)
test_no_skipping(step_nearmiss)
test_character_error(step_nearmiss)
test_na_response(step_nearmiss)
test_seed(step_nearmiss)
test_tidy(step_nearmiss)
test_under_ratio(step_nearmiss)
