test_that("basic usage", {
  rec1 <- recipe(class ~ x + y, data = circle_example) |>
    step_oss(class)

  rec1_p <- prep(rec1)

  te_xtab <- table(bake(rec1_p, new_data = circle_example)$class, useNA = "no")
  og_xtab <- table(circle_example$class, useNA = "no")

  expect_all_true(as.vector(te_xtab <= og_xtab))

  expect_no_warning(prep(rec1))
})

test_that("only majority classes are removed", {
  res <- oss(
    circle_example[, c("x", "y", "class")],
    var = "class"
  )

  counts <- table(res$class)
  orig <- table(circle_example$class)

  minority <- names(orig)[which.min(orig)]

  expect_identical(counts[[minority]], orig[[minority]])
  expect_lt(counts[["Rest"]], orig[["Rest"]])
})

test_that("removes the union of CNN and majority Tomek links", {
  df <- circle_example[, c("x", "y", "class")]

  removed <- withr::with_seed(1, themis:::oss_impl(df, var = "class"))

  cnn_removed <- withr::with_seed(1, themis:::cnn_impl(df, var = "class"))
  expect_all_true(cnn_removed %in% removed)

  minority <- names(which.min(table(df$class)))
  expect_all_true(as.character(df$class[removed]) != minority)
})

test_that("seed makes step reproducible", {
  baked <- function() {
    recipe(class ~ x + y, data = circle_example) |>
      step_oss(class, seed = 1) |>
      prep() |>
      bake(new_data = NULL)
  }

  expect_identical(baked(), baked())
})

test_that("works with a single predictor", {
  skip_if_not_installed("modeldata")

  data("hpc_data", package = "modeldata")

  expect_no_error(
    recipe(class ~ compounds, data = hpc_data) |>
      step_oss(all_outcomes()) |>
      prep() |>
      bake(NULL)
  )
})

test_that("bad data", {
  rec <- recipe(~., data = circle_example)
  # numeric check
  expect_snapshot(
    error = TRUE,
    rec |>
      step_oss(x) |>
      prep()
  )
  # Multiple variable check
  expect_snapshot(
    error = TRUE,
    rec |>
      step_oss(class, id) |>
      prep()
  )
})

test_that("errors if character are present", {
  df_char <- data.frame(
    x = factor(1:2),
    y = c("A", "A"),
    stringsAsFactors = FALSE
  )

  expect_snapshot(
    error = TRUE,
    recipe(~., data = df_char) |>
      step_oss(x) |>
      prep()
  )
})

test_that("NA in response", {
  skip_if_not_installed("modeldata")

  data("credit_data", package = "modeldata")
  credit_data0 <- credit_data
  credit_data0[1, 1] <- NA

  expect_snapshot(
    error = TRUE,
    recipe(Status ~ Age, data = credit_data0) |>
      step_oss(Status) |>
      prep()
  )
})

test_that("test tidy()", {
  rec <- recipe(class ~ x + y, data = circle_example) |>
    step_oss(class, id = "")

  rec_p <- prep(rec)

  untrained <- tibble(
    terms = "class",
    id = ""
  )

  trained <- tibble(
    terms = "class",
    id = ""
  )

  expect_equal(untrained, tidy(rec, number = 1))
  expect_equal(trained, tidy(rec_p, number = 1))
})

test_that("factor levels are not affected by alphabet ordering or class sizes", {
  circle_example_alt_levels <- list()
  for (i in 1:4) {
    circle_example_alt_levels[[i]] <- circle_example
  }

  # Checking for forgetting levels by majority/minor switching
  for (i in c(2, 4)) {
    levels(circle_example_alt_levels[[i]]$class) <-
      rev(levels(circle_example_alt_levels[[i]]$class))
  }

  # Checking for forgetting levels by alphabetical switching
  for (i in c(3, 4)) {
    circle_example_alt_levels[[i]]$class <-
      factor(
        x = circle_example_alt_levels[[i]]$class,
        levels = rev(levels(circle_example_alt_levels[[i]]$class))
      )
  }

  for (i in 1:4) {
    rec_p <- recipe(class ~ x + y, data = circle_example_alt_levels[[i]]) |>
      step_oss(class) |>
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

test_that("distance_with allows non-numeric columns to be present", {
  df_mixed <- data.frame(
    x = c(rnorm(50, 0, 1), rnorm(20, 0.5, 1)),
    y = c(rnorm(50, 0, 1), rnorm(20, 0.5, 1)),
    name = c(rep("alice", 50), rep("bob", 20)),
    class = factor(c(rep("majority", 50), rep("minority", 20)))
  )

  expect_no_error(
    recipe(class ~ ., data = df_mixed) |>
      step_oss(class, distance_with = c(x, y)) |>
      prep() |>
      bake(new_data = NULL)
  )
})

test_that("distance_with errors on non-numeric column", {
  df_mixed <- data.frame(
    x = c(1:5, 1:2),
    name = c(rep("a", 5), rep("b", 2)),
    class = factor(c(rep("majority", 5), rep("minority", 2)))
  )

  expect_snapshot(
    error = TRUE,
    recipe(class ~ ., data = df_mixed) |>
      step_oss(class, distance_with = c(x, name)) |>
      prep()
  )
})

test_that("id variables are ignored", {
  rec_id <- recipe(class ~ ., data = circle_example) |>
    update_role(id, new_role = "id") |>
    step_oss(class) |>
    prep()

  expect_equal(ncol(bake(rec_id, new_data = NULL)), 4)
})

test_that("id variables don't turn predictors to factors", {
  rec_id <- recipe(class ~ ., data = circle_example) |>
    update_role(id, new_role = "id") |>
    step_oss(class) |>
    prep() |>
    bake(new_data = NULL)

  expect_equal(is.double(rec_id$x), TRUE)
  expect_equal(is.double(rec_id$y), TRUE)
})

test_that("distance argument accepted by step_oss()", {
  expect_no_error(
    recipe(class ~ x + y, data = circle_example) |>
      step_oss(class, distance = "euclidean") |>
      prep() |>
      bake(new_data = NULL)
  )
  expect_no_error(
    recipe(class ~ x + y, data = circle_example) |>
      step_oss(class, distance = "cosine") |>
      prep() |>
      bake(new_data = NULL)
  )
  expect_no_error(
    recipe(class ~ x + y, data = circle_example) |>
      step_oss(class, distance = "mahalanobis") |>
      prep() |>
      bake(new_data = NULL)
  )
  expect_no_error(
    recipe(class ~ x + y, data = circle_example) |>
      step_oss(class, distance = "manhattan") |>
      prep() |>
      bake(new_data = NULL)
  )
  expect_no_error(
    recipe(class ~ x + y, data = circle_example) |>
      step_oss(class, distance = "chebyshev") |>
      prep() |>
      bake(new_data = NULL)
  )
})

test_that("bad distance arg for step_oss()", {
  expect_snapshot(
    error = TRUE,
    bake(
      prep(step_oss(
        recipe(class ~ x + y, data = circle_example),
        class,
        distance = "L2"
      )),
      new_data = NULL
    )
  )
})

test_that("bad args", {
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) |>
      step_oss(seed = TRUE)
  )
})

test_that("unused outcome levels are skipped with a warning (#238)", {
  circle_example$class <- factor(
    circle_example$class,
    levels = c(levels(circle_example$class), "unused")
  )

  expect_snapshot(
    res <- recipe(class ~ x + y, data = circle_example) |>
      step_oss(class) |>
      prep() |>
      bake(new_data = NULL)
  )

  expect_gt(nrow(res), 0)
})

test_that("oss() works with a character `var` (#261)", {
  df <- circle_example[c("x", "y", "class")]
  df$class <- as.character(df$class)

  res <- oss(df, "class")

  expect_type(res$class, "character")
  expect_identical(sort(unique(res$class)), c("Circle", "Rest"))
  expect_identical(sum(is.na(res$class)), 0L)
})

test_that("cnn scan handles a single remaining majority candidate (#245)", {
  # Two majority points seed the store with one, leaving a length-1 candidate
  # vector that must not be treated as a `sample()` count.
  df <- data.frame(
    x = c(rnorm(10, 0), 5, 5.1),
    y = c(rnorm(10, 0), 5, 4.9),
    class = c(rep("a", 10), "b", "b")
  )

  set.seed(42)
  res1 <- oss(df, "class")
  set.seed(42)
  res2 <- oss(df, "class")

  expect_identical(res1, res2)
  expect_identical(sort(unique(res1$class)), c("a", "b"))
  expect_all_true(do.call(paste, res1) %in% do.call(paste, df))
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  rec <- recipe(class ~ x + y, data = circle_example) |>
    step_oss(class, skip = FALSE) |>
    add_role(class, new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE)

  trained <- prep(rec, training = circle_example, verbose = FALSE)

  expect_snapshot(
    error = TRUE,
    bake(trained, new_data = circle_example[, -3])
  )
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_oss(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_oss(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_oss(rec)

  expect <- tibble(terms = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  rec <- recipe(class ~ x + y, data = circle_example) |>
    step_oss(class)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("0 and 1 rows data work in bake method", {
  rec <- recipe(class ~ x + y, data = circle_example) |>
    step_oss(class, skip = FALSE) |>
    prep()

  expect_identical(nrow(bake(rec, new_data = slice(circle_example, 0))), 0L)
  expect_identical(nrow(bake(rec, new_data = slice(circle_example, 1))), 1L)
})
