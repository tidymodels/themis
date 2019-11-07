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

test_that("no skipping", {
  rec3 <- rec %>%
    step_rose(tidyselect::matches("Species$"), skip = FALSE)

  rec3_p <- prep(rec3, training = iris2, retain = TRUE)

  tr_xtab <- table(juice(rec3_p)$Species, useNA = "always")
  te_xtab <- table(bake(rec3_p, new_data = iris2)$Species, useNA = "always")

  expect_equal(te_xtab, tr_xtab)
})

test_that("bad data", {
  expect_error(
    rec %>%
      step_rose(Sepal.Width) %>%
      prep(retain = TRUE)
  )
  expect_error(
    rec %>%
      step_rose(Species3) %>%
      prep(strings_as_factors = FALSE, retain = TRUE)
  )
  expect_error(
    rec %>%
      step_rose(Sepal.Length, Sepal.Width) %>%
      prep(strings_as_factors = FALSE, retain = TRUE)
  )
})

test_that("`seed` produces identical sampling", {

  rose_with_seed <- function(rec, seed = sample.int(10^5, 1)) {
    rec %>%
      step_rose(Species, seed = seed) %>%
      prep(training = iris2, retain = TRUE) %>%
      juice() %>%
      pull(Petal.Width)
  }

  petal_width_1 <- rose_with_seed(rec, seed = 1234)
  petal_width_2 <- rose_with_seed(rec, seed = 1234)
  petal_width_3 <- rose_with_seed(rec, seed = 12345)

  expect_equal(petal_width_1, petal_width_2)
  expect_false(identical(petal_width_1, petal_width_3))
})


test_that("step_rose errors if character are present", {
  df_char <- data.frame(x = factor(1:2),
                        y = c("A", "A"),
                        stringsAsFactors = FALSE)

  expect_error(
    recipe(~ ., data = df_char) %>%
      step_rose(x) %>%
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
      step_rose(x) %>%
      prep(),
    "only have 2 levels."
  )
})

test_printing(step_rose)
