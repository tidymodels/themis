library(testthat)
library(recipes)
library(dplyr)

context("adasyn")

credit_data2 <- select(credit_data[1:100, ], Status, Time, Price)

rec <- recipe(~ ., data = credit_data2)

test_that("basic usage", {
  rec1 <- rec %>%
    step_adasyn(Status, id = "")

  untrained <- tibble(
    terms = "Status",
    id = ""
  )

  expect_equivalent(untrained, tidy(rec1, number = 1))

  rec1_p <- prep(rec1, training = credit_data2, retain = TRUE)

  trained <- tibble(
    terms = "Status",
    id = ""
  )

  expect_equal(trained, tidy(rec1_p, number = 1))


  tr_xtab <- table(juice(rec1_p)$Status, useNA = "no")
  te_xtab <- table(bake(rec1_p, new_data = credit_data2)$Status, useNA = "no")
  og_xtab <- table(credit_data2$Status, useNA = "no")

  expect_gt(
    tr_xtab[["bad"]],
    og_xtab[["bad"]]
  )

  expect_equal(
    tr_xtab[["good"]],
    og_xtab[["good"]]
  )

  expect_equal(sort(te_xtab), sort(og_xtab))

  expect_warning(prep(rec1, training = credit_data2), NA)
})

test_that("no skipping", {
  rec3 <- rec %>%
    step_adasyn(tidyselect::matches("Status$"), skip = FALSE)

  rec3_p <- prep(rec3, training = credit_data2, retain = TRUE)

  tr_xtab <- table(juice(rec3_p)$Status, useNA = "always")
  te_xtab <- table(bake(rec3_p, new_data = credit_data2)$Status,
                   useNA = "always")

  expect_equal(te_xtab, tr_xtab)
})

test_that("bad data", {
  expect_error(
    rec %>%
      step_adasyn(Sepal.Width) %>%
      prep(retain = TRUE)
  )
  expect_error(
    rec %>%
      step_adasyn(Status3) %>%
      prep(strings_as_factors = FALSE, retain = TRUE)
  )
  expect_error(
    rec %>%
      step_adasyn(Sepal.Length, Sepal.Width) %>%
      prep(strings_as_factors = FALSE, retain = TRUE)
  )
})

test_that("printing", {
  rec4 <- rec %>%
    step_adasyn(Status)

  expect_output(print(rec))
  expect_output(print(rec4))
  expect_output(prep(rec4,
                     training = credit_data2,
                     retain = TRUE,
                     verbose = TRUE))
})

test_that("step_adasyn errors if character are present", {
  df_char <- data.frame(x = factor(1:2),
                        y = c("A", "A"),
                        stringsAsFactors = FALSE)

  expect_error(
    recipe(~ ., data = df_char) %>%
      step_adasyn(x) %>%
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
      step_adasyn(x) %>%
      prep(),
    "only have 2 levels."
  )
})

test_that("tunable", {
  rec <-
    recipe(~ ., data = iris) %>%
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

