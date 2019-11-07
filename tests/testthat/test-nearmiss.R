library(testthat)
library(recipes)
library(dplyr)

set.seed(1234)

context("nearmiss")

iris2 <- iris[-c(51:75), ]

rec <- recipe(~ ., data = iris2)

test_that("basic usage", {
  rec1 <- rec %>%
    step_nearmiss(Species, id = "")

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

  expect_length(unique(tr_xtab[["setosa"]]), 1)

  expect_equal(te_xtab, og_xtab)

  expect_warning(prep(rec1, training = iris2), NA)
})

test_that("minority classes are ignored if there is more than 1", {

  rec1_p2 <- recipe(~., data = iris[-c(1:25, 51:75), ]) %>%
    step_nearmiss(Species, id = "") %>%
    prep() %>%
    juice()

  expect_true(all(max(table(rec1_p2$Species)) == 25))
})

test_that("under_ratio value", {
  rec2 <- rec %>%
    step_nearmiss(tidyselect::matches("Species$"), under_ratio = 1.5)

  rec2_p <- prep(rec2, training = iris2, retain = TRUE)

  tr_xtab <- table(juice(rec2_p)$Species, useNA = "no")
  te_xtab <- table(bake(rec2_p, new_data = iris2)$Species, useNA = "no")
  og_xtab <- table(iris2$Species, useNA = "no")

  expect_equal(floor(min(tr_xtab) * 1.5), max(tr_xtab))

  expect_equal(te_xtab, og_xtab)
})

test_that("no skipping", {
  rec3 <- rec %>%
    step_nearmiss(tidyselect::matches("Species$"), skip = FALSE)

  rec3_p <- prep(rec3, training = iris2, retain = TRUE)

  tr_xtab <- table(juice(rec3_p)$Species, useNA = "always")
  te_xtab <- table(bake(rec3_p, new_data = iris2)$Species, useNA = "always")

  expect_equal(te_xtab, tr_xtab)
})

test_that("bad data", {
  expect_error(
    rec %>%
      step_nearmiss(Sepal.Width) %>%
      prep(retain = TRUE)
  )
  expect_error(
    rec %>%
      step_nearmiss(Species3) %>%
      prep(strings_as_factors = FALSE, retain = TRUE)
  )
  expect_error(
    rec %>%
      step_nearmiss(Sepal.Length, Sepal.Width) %>%
      prep(strings_as_factors = FALSE, retain = TRUE)
  )
})

test_that("`seed` produces identical sampling", {

  nearmiss_with_seed <- function(rec, seed = sample.int(10^5, 1)) {
    rec %>%
      step_nearmiss(Species, seed = seed) %>%
      prep(training = iris2, retain = TRUE) %>%
      juice() %>%
      pull(Petal.Width)
  }

  petal_width_1 <- nearmiss_with_seed(rec, seed = 1234)
  petal_width_2 <- nearmiss_with_seed(rec, seed = 1234)
  petal_width_3 <- nearmiss_with_seed(rec, seed = 12345)

  expect_equal(petal_width_1, petal_width_2)
  expect_false(identical(petal_width_1, petal_width_3))
})


test_that("step_nearmiss errors if character are present", {
  df_char <- data.frame(x = factor(1:2),
                        y = c("A", "A"),
                        stringsAsFactors = FALSE)

  expect_error(
    recipe(~ ., data = df_char) %>%
      step_nearmiss(x) %>%
      prep(),
    "should be numeric"
  )
})

test_that("checks are done to ensure step_nearmiss errors if NA are present", {
  df_char <- data.frame(x = factor(1:2),
                        y = c(NA, 1))

  expect_error(
    recipe(~ ., data = df_char) %>%
      step_nearmiss(x) %>%
      prep(),
    "not allowed"
  )
})

test_printing(step_nearmiss)
