cat_example <- data.frame(
  x = factor(sample(letters[1:4], 400, replace = TRUE)),
  y = factor(sample(LETTERS[1:3], 400, replace = TRUE)),
  class = factor(c(rep("rare", 80), rep("common", 320)))
)

test_that("errors if there isn't enough data", {
  df <- data.frame(
    x = factor(c("a", "b", "a", rep("c", 20))),
    class = factor(c(rep("min", 3), rep("maj", 20)))
  )

  expect_snapshot(
    error = TRUE,
    recipe(class ~ x, data = df) |>
      step_smoten(class, neighbors = 5) |>
      prep()
  )
})

test_that("basic usage", {
  rec1 <- recipe(class ~ x + y, data = cat_example) |>
    step_smoten(class)

  rec1_p <- prep(rec1)

  te_xtab <- table(bake(rec1_p, new_data = cat_example)$class, useNA = "no")
  og_xtab <- table(cat_example$class, useNA = "no")

  expect_equal(sort(te_xtab), sort(og_xtab))
  expect_no_warning(prep(rec1))
})

test_that("works with a single predictor", {
  expect_no_error(
    recipe(class ~ x, data = cat_example) |>
      step_smoten(class) |>
      prep() |>
      bake(NULL)
  )
})

test_that("errors on numeric predictors", {
  df <- data.frame(
    x = as.numeric(1:100),
    class = factor(c(rep("min", 20), rep("maj", 80)))
  )
  expect_snapshot(
    error = TRUE,
    recipe(class ~ x, data = df) |>
      step_smoten(class) |>
      prep()
  )
})

test_that("bad data", {
  df <- cat_example
  df$id <- factor(seq_len(nrow(df)))
  # Multiple variable check
  expect_snapshot(
    error = TRUE,
    recipe(~., data = df) |>
      step_smoten(class, id) |>
      prep()
  )
})

test_that("allows for character variables", {
  df_char <- data.frame(
    x = factor(c(rep("a", 10), rep("b", 20))),
    y = c(rep("A", 10), rep("B", 20)),
    class = factor(c(rep("min", 10), rep("maj", 20))),
    stringsAsFactors = FALSE
  )

  expect_no_error(
    recipe(class ~ x + y, data = df_char) |>
      step_smoten(class) |>
      prep()
  )
})

test_that("NA in response", {
  df <- cat_example
  df$x[1] <- NA

  expect_snapshot(
    error = TRUE,
    recipe(class ~ x + y, data = df) |>
      step_smoten(class) |>
      prep()
  )
})

test_that("`seed` produces identical sampling", {
  step_with_seed <- function(seed = sample.int(10^5, 1)) {
    recipe(class ~ x + y, data = cat_example) |>
      step_smoten(class, seed = seed) |>
      prep() |>
      bake(new_data = NULL) |>
      pull(x)
  }

  run_1 <- step_with_seed(seed = 1234)
  run_2 <- step_with_seed(seed = 1234)
  run_3 <- step_with_seed(seed = 12345)

  expect_equal(run_1, run_2)
  expect_false(identical(run_1, run_3))
})

test_that("test tidy()", {
  rec <- recipe(class ~ x + y, data = cat_example) |>
    step_smoten(class, id = "")

  rec_p <- prep(rec)

  untrained <- tibble(terms = "class", id = "")
  trained <- tibble(terms = "class", id = "")

  expect_equal(untrained, tidy(rec, number = 1))
  expect_equal(trained, tidy(rec_p, number = 1))
})

test_that("ratio value works when oversampling", {
  res1 <- recipe(class ~ x + y, data = cat_example) |>
    step_smoten(class) |>
    prep() |>
    bake(new_data = NULL)

  res1.5 <- recipe(class ~ x + y, data = cat_example) |>
    step_smoten(class, over_ratio = 0.5) |>
    prep() |>
    bake(new_data = NULL)

  expect_equal(
    as.vector(table(res1$class)),
    rep(max(table(cat_example$class)), length(table(res1$class)))
  )
  expect_equal(
    sort(as.numeric(table(res1.5$class))),
    max(table(cat_example$class)) * c(0.5, 1)
  )
})

test_that("allows multi-class", {
  df <- data.frame(
    x = factor(sample(letters[1:3], 300, replace = TRUE)),
    class = factor(c(rep("a", 30), rep("b", 90), rep("c", 180)))
  )
  expect_no_error(
    recipe(class ~ x, data = df) |>
      step_smoten(class) |>
      prep() |>
      bake(new_data = NULL)
  )
})

test_that("factor levels are not affected by alphabet ordering or class sizes", {
  cat_example_alt_levels <- list()
  for (i in 1:4) {
    cat_example_alt_levels[[i]] <- cat_example
  }

  for (i in c(2, 4)) {
    levels(cat_example_alt_levels[[i]]$class) <-
      rev(levels(cat_example_alt_levels[[i]]$class))
  }

  for (i in c(3, 4)) {
    cat_example_alt_levels[[i]]$class <-
      factor(
        x = cat_example_alt_levels[[i]]$class,
        levels = rev(levels(cat_example_alt_levels[[i]]$class))
      )
  }

  for (i in 1:4) {
    rec_p <- recipe(class ~ x + y, data = cat_example_alt_levels[[i]]) |>
      step_smoten(class) |>
      prep()

    expect_equal(
      levels(cat_example_alt_levels[[i]]$class),
      rec_p$levels$class$values
    )
    expect_equal(
      levels(cat_example_alt_levels[[i]]$class),
      levels(bake(rec_p, new_data = NULL)$class)
    )
  }
})

test_that("ordering of newly generated points are right", {
  res <- recipe(class ~ x + y, data = cat_example) |>
    step_smoten(class) |>
    prep() |>
    bake(new_data = NULL)

  expect_equal(
    res[seq_len(nrow(cat_example)), ],
    as_tibble(cat_example[, c("x", "y", "class")])
  )
})

test_that("non-predictor variables are ignored", {
  df <- cat_example
  df$id <- as.character(seq_len(nrow(df)))

  res <- recipe(class ~ ., data = df) |>
    update_role(id, new_role = "id") |>
    step_smoten(class) |>
    prep() |>
    bake(new_data = NULL)

  expect_equal(
    c(df$id, rep(NA, nrow(res) - nrow(df))),
    as.character(res$id)
  )
})

test_that("tunable", {
  rec <- recipe(class ~ x + y, data = cat_example) |>
    step_smoten(class)
  rec_param <- tunable.step_smoten(rec$steps[[1]])
  expect_equal(rec_param$name, c("over_ratio", "neighbors"))
  expect_true(all(rec_param$source == "recipe"))
  expect_true(is.list(rec_param$call_info))
  expect_equal(nrow(rec_param), 2)
  expect_equal(
    names(rec_param),
    c("name", "call_info", "source", "component", "component_id")
  )
})

test_that("indicator_column adds logical column marking synthetic rows", {
  rec <- recipe(class ~ x + y, data = cat_example) |>
    step_smoten(class, indicator_column = ".new_row") |>
    prep()

  res <- bake(rec, new_data = NULL)

  expect_true(".new_row" %in% names(res))
  expect_type(res$.new_row, "logical")
  expect_equal(sum(!res$.new_row), nrow(cat_example))
  expect_gt(sum(res$.new_row), 0L)
})

test_that("indicator_column bad args", {
  expect_snapshot(
    error = TRUE,
    recipe(class ~ x + y, data = cat_example) |>
      step_smoten(class, indicator_column = 1)
  )
  expect_snapshot(
    error = TRUE,
    recipe(class ~ x + y, data = cat_example) |>
      step_smoten(class, indicator_column = "") |>
      prep()
  )
  expect_snapshot(
    error = TRUE,
    recipe(class ~ x + y, data = cat_example) |>
      step_smoten(class, indicator_column = "x") |>
      prep()
  )
})

test_that("bad args", {
  expect_snapshot(
    error = TRUE,
    recipe(class ~ x + y, data = cat_example) |>
      step_smoten(over_ratio = "yes") |>
      prep()
  )
  expect_snapshot(
    error = TRUE,
    recipe(class ~ x + y, data = cat_example) |>
      step_smoten(neighbors = TRUE) |>
      prep()
  )
  expect_snapshot(
    error = TRUE,
    recipe(class ~ x + y, data = cat_example) |>
      step_smoten(seed = TRUE)
  )
})

test_that("tunable is setup to works with extract_parameter_set_dials", {
  skip_if_not_installed("dials")
  rec <- recipe(class ~ x + y, data = cat_example) |>
    step_smoten(
      class,
      over_ratio = hardhat::tune(),
      neighbors = hardhat::tune()
    )

  params <- extract_parameter_set_dials(rec)

  expect_s3_class(params, "parameters")
  expect_identical(nrow(params), 2L)
})

test_that("unused outcome levels are skipped with a warning (#238)", {
  cat_example$class <- factor(
    cat_example$class,
    levels = c(levels(cat_example$class), "unused")
  )

  expect_snapshot(
    res <- recipe(class ~ x + y, data = cat_example) |>
      step_smoten(class) |>
      prep() |>
      bake(new_data = NULL)
  )

  expect_gt(nrow(res), 0)
})

test_that("smoten() works with a character `var` (#261)", {
  df <- cat_example
  df$class <- as.character(df$class)

  res <- smoten(df, "class")

  expect_s3_class(res$class, "factor")
  expect_identical(sort(levels(res$class)), c("common", "rare"))
  expect_identical(sum(is.na(res$class)), 0L)
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  rec <- recipe(class ~ x + y, data = cat_example) |>
    step_smoten(class, skip = FALSE) |>
    add_role(class, new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE)

  trained <- prep(rec, training = cat_example, verbose = FALSE)

  expect_snapshot(
    error = TRUE,
    bake(trained, new_data = cat_example[, -3])
  )
})

test_that("empty printing", {
  rec <- recipe(class ~ ., cat_example)
  rec <- step_smoten(rec)

  expect_snapshot(rec)

  rec <- prep(rec, cat_example)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(class ~ ., cat_example)
  rec2 <- step_smoten(rec1)

  rec1 <- prep(rec1, cat_example)
  rec2 <- prep(rec2, cat_example)

  baked1 <- bake(rec1, cat_example)
  baked2 <- bake(rec2, cat_example)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(class ~ ., cat_example)
  rec <- step_smoten(rec)

  expect <- tibble(terms = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, cat_example)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  rec <- recipe(class ~ x + y, data = cat_example) |>
    step_smoten(class)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("0 and 1 rows data work in bake method", {
  rec <- recipe(class ~ x + y, data = cat_example) |>
    step_smoten(class, skip = FALSE) |>
    prep()

  expect_identical(nrow(bake(rec, new_data = slice(cat_example, 0))), 0L)
  expect_identical(nrow(bake(rec, new_data = slice(cat_example, 1))), 1L)
})
