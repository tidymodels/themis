library(testthat)
library(recipes)
library(dplyr)
set.seed(1234)

context("ROSE")

iris2 <- iris
iris2$Species <- factor(iris2$Species == "setosa",
                        levels = c(TRUE, FALSE),
                        labels = c("setosa", "not setosa"))

rec <- recipe(~ ., data = iris2)

test_that("basic usage", {
  rec1 <- rec %>%
    step_rose(Species, id = "")

  rec1_p <- prep(rec1, training = iris2, retain = TRUE)

  tr_xtab <- table(juice(rec1_p)$Species, useNA = "no")
  te_xtab <- table(bake(rec1_p, new_data = iris2)$Species, useNA = "no")
  og_xtab <- table(iris2$Species, useNA = "no")

  expect_equal(
    sum(tr_xtab),
    max(og_xtab) * 2
  )

  expect_equal(sort(te_xtab), sort(og_xtab))

  expect_warning(prep(rec1, training = iris2), NA)
})

test_that("minority_prop value", {
  rec21 <- rec %>%
    step_rose(tidyselect::matches("Species$"), minority_prop = 0.1)

  rec22 <- rec %>%
    step_rose(tidyselect::matches("Species$"), minority_prop = 0.2)

  rec21_p <- prep(rec21, training = iris2, retain = TRUE)
  rec22_p <- prep(rec22, training = iris2, retain = TRUE)

  tr_xtab1 <- table(juice(rec21_p)$Species, useNA = "no")
  tr_xtab2 <- table(juice(rec22_p)$Species, useNA = "no")

  expect_equal(sum(tr_xtab1), sum(tr_xtab2))

  expect_lt(tr_xtab1[["setosa"]], tr_xtab2[["setosa"]])
})

test_that("factors with more than 2 levels", {
  df_char <- data.frame(x = factor(1:3),
                        y = c(1:3),
                        stringsAsFactors = FALSE)

  expect_error(
    recipe(~ ., data = df_char) %>%
      step_rose(x) %>%
      prep(),
    "only have 2 levels."
  )
})

test_that("tunable", {
  rec <-
    recipe(~ ., data = iris) %>%
    step_rose(all_predictors(), under_ratio = 1)
  rec_param <- tunable.step_rose(rec$steps[[1]])
  expect_equal(rec_param$name, c("over_ratio"))
  expect_true(all(rec_param$source == "recipe"))
  expect_true(is.list(rec_param$call_info))
  expect_equal(nrow(rec_param), 1)
  expect_equal(
    names(rec_param),
    c("name", "call_info", "source", "component", "component_id")
  )
})

test_printing(step_rose)
test_bad_data(step_rose)
test_no_skipping(step_rose)
test_character_error(step_rose)
test_na_response(step_rose)
test_seed(step_rose)
test_tidy(step_rose)
