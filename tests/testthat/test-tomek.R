library(testthat)
library(recipes)
library(dplyr)

context("tomek")

iris2 <- iris
iris2$Species <- factor(iris2$Species == "setosa",
                        levels = c(TRUE, FALSE),
                        labels = c("setosa", "not setosa"))

rec <- recipe(~ ., data = iris2)

test_that("basic usage", {
  rec1 <- rec %>%
    step_tomek(Species, id = "")

  untrained <- tibble(
    terms = "Species",
    id = ""
  )

  expect_equivalent(untrained, tidy(rec1, number = 1))

  rec1_p <- prep(rec1, training = iris2, retain = TRUE)

  trained <- tibble(
    terms = "Species",
    id = ""
  )

  expect_equal(trained, tidy(rec1_p, number = 1))


  tr_xtab <- table(juice(rec1_p)$Species, useNA = "no")
  te_xtab <- table(bake(rec1_p, new_data = iris2)$Species, useNA = "no")
  og_xtab <- table(iris2$Species, useNA = "no")

  expect_equal(
    sum(tr_xtab),
    sum(og_xtab)
  )

  expect_equal(sort(te_xtab), sort(og_xtab))

  expect_warning(prep(rec1, training = iris2), NA)
})

test_that("no skipping", {
  rec3 <- rec %>%
    step_tomek(tidyselect::matches("Species$"), skip = FALSE)

  rec3_p <- prep(rec3, training = iris2, retain = TRUE)

  tr_xtab <- table(juice(rec3_p)$Species, useNA = "always")
  te_xtab <- table(bake(rec3_p, new_data = iris2)$Species, useNA = "always")

  expect_equal(te_xtab, tr_xtab)
})

test_that("bad data", {
  expect_error(
    rec %>%
      step_tomek(Sepal.Width) %>%
      prep(retain = TRUE)
  )
  expect_error(
    rec %>%
      step_tomek(Species3) %>%
      prep(strings_as_factors = FALSE, retain = TRUE)
  )
  expect_error(
    rec %>%
      step_tomek(Sepal.Length, Sepal.Width) %>%
      prep(strings_as_factors = FALSE, retain = TRUE)
  )
})

test_that("printing", {
  rec4 <- rec %>%
    step_tomek(Species)

  expect_output(print(rec))
  expect_output(print(rec4))
  expect_output(prep(rec4, training = iris2, retain = TRUE, verbose = TRUE))
})

test_that("step_tomek errors if character are present", {
  df_char <- data.frame(x = factor(1:2),
                        y = c("A", "A"),
                        stringsAsFactors = FALSE)

  expect_error(
    recipe(~ ., data = df_char) %>%
      step_tomek(x) %>%
      prep(),
    "should be numeric"
  )
})

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

