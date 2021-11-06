library(testthat)
library(recipes)
library(dplyr)
set.seed(1234)

context("ROSE")

test_that("minority_prop value", {
  rec <- recipe(~., data = circle_example)
  rec21 <- rec %>%
    step_rose(class, minority_prop = 0.1)

  rec22 <- rec %>%
    step_rose(class, minority_prop = 0.2)

  rec21_p <- prep(rec21)
  rec22_p <- prep(rec22)

  tr_xtab1 <- table(bake(rec21_p, new_data = NULL)$class, useNA = "no")
  tr_xtab2 <- table(bake(rec22_p, new_data = NULL)$class, useNA = "no")

  expect_equal(sum(tr_xtab1), sum(tr_xtab2))

  expect_lt(tr_xtab1[["Circle"]], tr_xtab2[["Circle"]])
})

test_that("tunable", {
  rec <-
    recipe(~., data = iris) %>%
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

test_that("row matching works correctly #36", {
  expect_error(
  recipe(class ~ ., data = circle_example) %>%
    step_rose(class, over_ratio = 1.2) %>%
    prep(),
  NA
  )

  expect_error(
    recipe(class ~ ., data = circle_example) %>%
      step_rose(class, over_ratio = 0.8) %>%
      prep(),
    NA
  )

  expect_error(
    recipe(class ~ ., data = circle_example) %>%
      step_rose(class, over_ratio = 1.7) %>%
      prep(),
    NA
  )
})

test_that("basic usage", {
  rec1 <- recipe(~., data = circle_example) %>%
    step_rose(class)

  rec1_p <- prep(rec1)

  te_xtab <- table(bake(rec1_p, new_data = circle_example)$class, useNA = "no")
  og_xtab <- table(circle_example$class, useNA = "no")

  expect_equal(sort(te_xtab), sort(og_xtab))

  expect_warning(prep(rec1), NA)
})

test_that("printing", {
  rec <- recipe(~., data = circle_example) %>%
    step_rose(class)
  expect_output(print(rec))
  expect_output(prep(rec, verbose = TRUE))
})

test_that("bad data", {
  iris2 <- iris[-c(1:45), ]
  iris2$Species2 <- sample(iris2$Species)
  iris2$Species3 <- as.character(sample(iris2$Species))

  rec <- recipe(~., data = iris2)
  # numeric check
  expect_error(
    rec %>%
      step_rose(Sepal.Width) %>%
      prep()
  )
  # Multiple variable check
  expect_error(
    rec %>%
      step_rose(Species, Species2) %>%
      prep()
  )
  # character check
  expect_error(
    rec %>%
      step_rose(Species3) %>%
      prep()
  )
})

test_that("NA in response", {
  iris2 <- iris[-c(1:45), ]
  iris2$Species[seq(6, 96, by = 5)] <- NA
  # NA check
  expect_error(
    recipe(~., data = iris2) %>%
      step_rose(Species) %>%
      prep()
  )
})

test_that("`seed` produces identical sampling", {
  step_with_seed <- function(seed = sample.int(10^5, 1)) {
    recipe(~., data = circle_example) %>%
      step_rose(class, seed = seed) %>%
      prep() %>%
      bake(new_data = NULL) %>%
      pull(x)
  }

  run_1 <- step_with_seed(seed = 1234)
  run_2 <- step_with_seed(seed = 1234)
  run_3 <- step_with_seed(seed = 12345)

  expect_equal(run_1, run_2)
  expect_false(identical(run_1, run_3))
})

test_that("test tidy()", {
  rec <- recipe(~., data = circle_example) %>%
    step_rose(class, id = "")

  rec_p <- prep(rec)

  untrained <- tibble(
    terms = "class",
    id = ""
  )

  trained <- tibble(
    terms = "class",
    id = ""
  )

  expect_equivalent(untrained, tidy(rec, number = 1))
  expect_equal(trained, tidy(rec_p, number = 1))
})

test_that("only except 2 classes", {
  df_char <- data.frame(
    x = factor(1:3),
    stringsAsFactors = FALSE
  )

  expect_error(
    recipe(~., data = df_char) %>%
      step_rose(x) %>%
      prep(),
    "only have 2 levels."
  )
})

test_that("factor levels are not affected by alphabet ordering or class sizes", {
  circle_example_alt_levels <- list()
  for (i in 1:4) circle_example_alt_levels[[i]] <- circle_example

  # Checking for forgetting levels by majority/minor switching
  for (i in c(2, 4)) {
    levels(circle_example_alt_levels[[i]]$class) <-
      rev(levels(circle_example_alt_levels[[i]]$class))
  }

  # Checking for forgetting levels by alphabetical switching
  for (i in c(3, 4)) {
    circle_example_alt_levels[[i]]$class <-
      factor(x = circle_example_alt_levels[[i]]$class,
             levels = rev(levels(circle_example_alt_levels[[i]]$class)))
  }

  for (i in 1:4) {
    rec_p <- recipe(~., data = circle_example_alt_levels[[i]]) %>%
      step_rose(class) %>%
      prep()

    expect_equal(
      levels(circle_example_alt_levels[[i]]$class), # Original levels
      rec_p$levels$class$values # New levels
    )
    expect_equal(
      levels(circle_example_alt_levels[[i]]$class), # Original levels
      levels(bake(rec_p, new_data = NULL)$class) # New levels
    )
  }
})

test_that("non-predictor variables are ignored", {
  circle_example2 <- circle_example %>%
    mutate(id = as.character(row_number())) %>%
    as_tibble()

  res <- recipe(class ~ ., data = circle_example2) %>%
    update_role(id, new_role = "id") %>%
    step_rose(class) %>%
    prep() %>%
    bake(new_data = NULL)

  expect_equal(
    c(circle_example2$id, rep(NA, nrow(res) - nrow(circle_example2))),
    as.character(res$id)
  )
})
