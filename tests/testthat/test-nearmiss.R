library(testthat)
library(recipes)
library(dplyr)

set.seed(1234)

context("nearmiss")

test_that("tunable", {
  rec <-
    recipe(~., data = iris) %>%
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

test_that("basic usage", {
  rec1 <- recipe(~., data = circle_example) %>%
    step_nearmiss(class)

  rec1_p <- prep(rec1)

  te_xtab <- table(bake(rec1_p, new_data = circle_example)$class, useNA = "no")
  og_xtab <- table(circle_example$class, useNA = "no")

  expect_equal(sort(te_xtab), sort(og_xtab))

  expect_warning(prep(rec1), NA)
})

test_that("printing", {
  rec <- recipe(~., data = circle_example) %>%
    step_nearmiss(class)
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
      step_nearmiss(Sepal.Width) %>%
      prep()
  )
  # Multiple variable check
  expect_error(
    rec %>%
      step_nearmiss(Species, Species2) %>%
      prep(strings_as_factors = FALSE)
  )
  # character check
  expect_error(
    rec %>%
      step_nearmiss(Species3) %>%
      prep(strings_as_factors = FALSE)
  )
})

test_that("errors if character are present", {
  df_char <- data.frame(
    x = factor(1:2),
    y = c("A", "A"),
    stringsAsFactors = FALSE
  )

  expect_error(
    recipe(~., data = df_char) %>%
      step_nearmiss(x) %>%
      prep(),
    "should be numeric"
  )
})

test_that("NA in response", {
  iris2 <- iris[-c(1:45), ]
  iris2$Species[seq(6, 96, by = 5)] <- NA
  # NA check
  expect_error(
    recipe(~., data = iris2) %>%
      step_nearmiss(Species) %>%
      prep(strings_as_factors = FALSE)
  )
})

test_that("test tidy()", {
  rec <- recipe(~., data = circle_example) %>%
    step_nearmiss(class, id = "")

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

test_that("ratio value works when undersampling", {
  res1 <- recipe(~., data = circle_example) %>%
    step_nearmiss(class) %>%
    prep() %>%
    bake(new_data = NULL)

  res1.5 <- recipe(~., data = circle_example) %>%
    step_nearmiss(class, under_ratio = 1.5) %>%
    prep() %>%
    bake(new_data = NULL)

  expect_true(all(table(res1$class) == min(table(circle_example$class))))
  expect_equal(
    sort(as.numeric(table(res1.5$class))),
    min(table(circle_example$class)) * c(1, 1.5)
  )
})

test_that("allows multi-class", {
  data <- rename(iris[-c(1:25, 51:75), ], class = Species)
  expect_error(
    recipe(~., data = data) %>%
      step_nearmiss(class) %>%
      prep(),
    NA
  )
})

test_that("minority classes are ignored if there is more than 1", {
  rec1_p2 <- recipe(~., data = iris[-c(1:25, 51:75), ]) %>%
    step_nearmiss(Species) %>%
    prep() %>%
    bake(new_data = NULL)

  expect_true(all(max(table(rec1_p2$Species)) == 25))
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
      step_nearmiss(class) %>%
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

test_that("id variables are ignored", {
  rec_id <- recipe(class ~ ., data = circle_example) %>%
    update_role(x, new_role = "ID") %>%
    step_nearmiss(class, under_ratio = 1) %>%
    prep()

  expect_equal(ncol(bake(rec_id, new_data = NULL)), 3)
})

