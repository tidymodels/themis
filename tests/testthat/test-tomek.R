library(testthat)
library(recipes)
library(dplyr)

context("tomek")

iris2 <- iris
iris2$Species <- factor(iris2$Species == "setosa",
                        levels = c(TRUE, FALSE),
                        labels = c("setosa", "not setosa"))

rec <- recipe(~ ., data = iris2)

test_that("factors with more than 2 levels", {
  df_char <- data.frame(x = factor(1:3),
                        y = c(1:3),
                        stringsAsFactors = FALSE)

  expect_error(
    recipe(~ ., data = df_char) %>%
      step_tomek(x) %>%
      prep(),
    "only have 2 levels."
  )
})

test_that("tunable", {
  rec <-
    recipe(~ ., data = iris) %>%
    step_tomek(all_predictors(), under_ratio = 1)
  rec_param <- tunable.step_tomek(rec$steps[[1]])
  expect_equal(rec_param$name, c("neighbors"))
  expect_true(all(rec_param$source == "recipe"))
  expect_true(is.list(rec_param$call_info))
  expect_equal(nrow(rec_param), 1)
  expect_equal(
    names(rec_param),
    c("name", "call_info", "source", "component", "component_id")
  )
})

test_basic_usage(step_tomek)
test_printing(step_tomek)
test_bad_data(step_tomek)
test_no_skipping(step_tomek)
test_character_error(step_tomek)
test_na_response(step_tomek)
test_tidy(step_tomek)
