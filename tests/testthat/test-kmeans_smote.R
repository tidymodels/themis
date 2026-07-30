test_that("distance argument accepted by step_kmeans_smote()", {
  bake_with <- function(distance) {
    recipe(class ~ x + y, data = circle_example) |>
      step_kmeans_smote(class, distance = distance) |>
      prep() |>
      bake(new_data = NULL)
  }

  expect_no_error(bake_with("euclidean"))
  expect_no_error(bake_with("cosine"))
  expect_no_error(bake_with("mahalanobis"))
  expect_no_error(bake_with("manhattan"))
  expect_no_error(bake_with("chebyshev"))
})

test_that("sqrt-embedded distance metrics accepted by step_kmeans_smote()", {
  set.seed(1)
  raw <- matrix(runif(60 * 3), ncol = 3)
  props <- raw / rowSums(raw)
  compositional <- data.frame(
    x = props[, 1],
    y = props[, 2],
    z = props[, 3],
    class = factor(rep(c("a", "b"), times = c(50, 10)))
  )

  bake_with <- function(distance) {
    recipe(class ~ ., data = compositional) |>
      step_kmeans_smote(
        class,
        num_clusters = 2,
        cluster_balance_threshold = 0,
        distance = distance
      ) |>
      prep() |>
      bake(new_data = NULL)
  }

  expect_no_error(bake_with("squared_chord"))
  expect_no_error(bake_with("matusita"))
  expect_no_error(bake_with("hellinger"))
  expect_no_error(bake_with("bhattacharyya"))
})

test_that("philentropy distance metrics accepted by step_kmeans_smote()", {
  skip_if_not_installed("philentropy")
  set.seed(1)
  raw <- matrix(runif(60 * 3), ncol = 3)
  props <- raw / rowSums(raw)
  compositional <- data.frame(
    x = props[, 1],
    y = props[, 2],
    z = props[, 3],
    class = factor(rep(c("a", "b"), times = c(50, 10)))
  )

  bake_with <- function(distance) {
    recipe(class ~ ., data = compositional) |>
      step_kmeans_smote(
        class,
        num_clusters = 2,
        cluster_balance_threshold = 0,
        distance = distance
      ) |>
      prep() |>
      bake(new_data = NULL)
  }

  expect_no_error(bake_with("canberra"))
  expect_no_error(bake_with("jensen-shannon"))
  expect_no_error(bake_with("kumar-johnson"))
})

test_that("bad distance arg for step_kmeans_smote()", {
  expect_snapshot(
    error = TRUE,
    recipe(class ~ x + y, data = circle_example) |>
      step_kmeans_smote(class, distance = "L2") |>
      prep() |>
      bake(new_data = NULL)
  )
})

test_that("basic usage", {
  rec1 <- recipe(class ~ x + y, data = circle_example) |>
    step_kmeans_smote(class)

  rec1_p <- prep(rec1)

  te_xtab <- table(bake(rec1_p, new_data = circle_example)$class, useNA = "no")
  og_xtab <- table(circle_example$class, useNA = "no")

  expect_equal(sort(te_xtab), sort(og_xtab))

  expect_no_warning(prep(rec1))
})

test_that("works with a single predictor", {
  skip_if_not_installed("modeldata")

  data("hpc_data", package = "modeldata")

  expect_no_error(
    recipe(class ~ compounds, data = hpc_data) |>
      step_kmeans_smote(all_outcomes()) |>
      prep() |>
      bake(NULL)
  )
})

test_that("num_clusters is respected", {
  rec <- recipe(class ~ x + y, data = circle_example) |>
    step_kmeans_smote(class, num_clusters = 10) |>
    prep()

  expect_identical(rec$steps[[1]]$num_clusters, 10)
  expect_no_error(bake(rec, new_data = NULL))

  expect_snapshot(
    error = TRUE,
    recipe(class ~ x + y, data = circle_example) |>
      step_kmeans_smote(class, num_clusters = 1000) |>
      prep() |>
      bake(new_data = NULL)
  )
})

test_that("cluster_balance_threshold filters clusters", {
  # The minority class is scattered thinly through the majority class, so no
  # cluster is minority-dominated.
  set.seed(3)
  df <- data.frame(
    x = runif(60),
    y = runif(60),
    class = factor(rep(c("a", "b"), times = c(10, 50)))
  )

  expect_snapshot(
    error = TRUE,
    recipe(class ~ x + y, data = df) |>
      step_kmeans_smote(class, num_clusters = 3) |>
      prep() |>
      bake(new_data = NULL)
  )

  res <- recipe(class ~ x + y, data = df) |>
    step_kmeans_smote(class, num_clusters = 3, cluster_balance_threshold = 0) |>
    prep() |>
    bake(new_data = NULL)

  expect_equal(as.vector(table(res$class)), c(50, 50))
})

test_that("the requested number of points is generated exactly", {
  for (over_ratio in c(0.4, 0.63, 0.77, 1)) {
    res <- recipe(class ~ x + y, data = circle_example) |>
      step_kmeans_smote(class, over_ratio = over_ratio) |>
      prep() |>
      bake(new_data = NULL)

    majority <- max(table(circle_example$class))
    expect_equal(
      sort(as.vector(table(res$class))),
      sort(c(round(majority * over_ratio), majority))
    )
  }
})

test_that("clusters of identical points are handled", {
  # All within-cluster distances are 0, so the sparsity weighting is undefined
  # and falls back to cluster size.
  df <- data.frame(
    x = rep(c(1, 2), each = 10),
    y = rep(c(1, 2), each = 10),
    class = factor(c(rep("a", 4), rep("b", 6), rep("a", 2), rep("b", 8)))
  )

  res <- recipe(class ~ x + y, data = df) |>
    step_kmeans_smote(
      class,
      num_clusters = 2,
      cluster_balance_threshold = 0
    ) |>
    prep() |>
    bake(new_data = NULL)

  expect_equal(as.vector(table(res$class)), c(14, 14))
  expect_identical(sum(is.na(res$x)), 0L)
})

test_that("clusters with too few minority points are skipped", {
  set.seed(4)
  df <- data.frame(
    x = c(rnorm(30, 0), rnorm(6, 10), rnorm(2, 20)),
    y = c(rnorm(30, 0), rnorm(6, 10), rnorm(2, 20)),
    class = factor(c(rep("b", 30), rep("a", 8)))
  )

  # The cluster around 20 holds only 2 "a" observations, fewer than
  # `neighbors + 1`, so it cannot be interpolated within.
  res <- recipe(class ~ x + y, data = df) |>
    step_kmeans_smote(class, num_clusters = 3) |>
    prep() |>
    bake(new_data = NULL)

  expect_equal(as.vector(table(res$class)), c(30, 30))
})

test_that("density_exponent shifts points between clusters", {
  bake_with <- function(density_exponent) {
    recipe(class ~ x + y, data = circle_example) |>
      step_kmeans_smote(
        class,
        density_exponent = density_exponent,
        seed = 1234
      ) |>
      prep() |>
      bake(new_data = NULL) |>
      pull(x)
  }

  expect_false(identical(bake_with(1), bake_with(8)))
})

test_that("bad data", {
  rec <- recipe(~., data = circle_example)
  # numeric check
  expect_snapshot(
    error = TRUE,
    rec |>
      step_kmeans_smote(x) |>
      prep()
  )
  # Multiple variable check
  expect_snapshot(
    error = TRUE,
    rec |>
      step_kmeans_smote(class, id) |>
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
      step_kmeans_smote(x) |>
      prep()
  )
})

test_that("NA in response", {
  skip_if_not_installed("modeldata")

  data("credit_data", package = "modeldata")

  expect_snapshot(
    error = TRUE,
    recipe(Job ~ Age, data = credit_data) |>
      step_kmeans_smote(Job) |>
      prep()
  )
})

test_that("`seed` produces identical sampling", {
  step_with_seed <- function(seed = sample.int(10^5, 1)) {
    recipe(class ~ x + y, data = circle_example) |>
      step_kmeans_smote(class, seed = seed) |>
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
  rec <- recipe(class ~ x + y, data = circle_example) |>
    step_kmeans_smote(class, id = "")

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

test_that("allows multi-class", {
  skip_if_not_installed("modeldata")

  data("hpc_data", package = "modeldata")
  hpc_data0 <- hpc_data[, c("class", "compounds", "input_fields", "iterations")]

  res <- recipe(class ~ ., data = hpc_data0) |>
    step_kmeans_smote(class, over_ratio = 0.45) |>
    prep() |>
    bake(new_data = NULL)

  expect_equal(min(table(res$class)), round(2211 * 0.45))
})

test_that("majority classes are ignored if there is more than 1", {
  skip_if_not_installed("modeldata")

  data("penguins", package = "modeldata")
  rec1_p2 <- recipe(
    species ~ bill_length_mm + bill_depth_mm,
    data = penguins[-(1:28), ]
  ) |>
    step_impute_mean(all_predictors()) |>
    step_kmeans_smote(species, cluster_balance_threshold = 0.5) |>
    prep() |>
    bake(new_data = NULL)

  expect_identical(max(table(rec1_p2$species)), 124L)
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
      step_kmeans_smote(class) |>
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

test_that("ordering of newly generated points are right", {
  res <- recipe(class ~ x + y, data = circle_example) |>
    step_kmeans_smote(class) |>
    prep() |>
    bake(new_data = NULL)

  expect_equal(
    res[seq_len(nrow(circle_example)), ],
    as_tibble(circle_example[, c("x", "y", "class")])
  )
})

test_that("non-predictor variables are ignored", {
  res <- recipe(class ~ ., data = circle_example) |>
    update_role(id, new_role = "id") |>
    step_kmeans_smote(class) |>
    prep() |>
    bake(new_data = NULL)

  expect_equal(
    c(circle_example$id, rep(NA, nrow(res) - nrow(circle_example))),
    as.character(res$id)
  )
})

test_that("id variables don't turn predictors to factors", {
  rec_id <- recipe(class ~ ., data = circle_example) |>
    update_role(id, new_role = "id") |>
    step_kmeans_smote(class) |>
    prep() |>
    bake(new_data = NULL)

  expect_equal(is.double(rec_id$x), TRUE)
  expect_equal(is.double(rec_id$y), TRUE)
})

test_that("tunable", {
  rec <- recipe(~., data = mtcars) |>
    step_kmeans_smote(all_predictors())
  rec_param <- tunable.step_kmeans_smote(rec$steps[[1]])
  expect_equal(rec_param$name, c("over_ratio", "neighbors", "num_clusters"))
  expect_true(all(rec_param$source == "recipe"))
  expect_true(is.list(rec_param$call_info))
  expect_equal(nrow(rec_param), 3)
  expect_equal(
    names(rec_param),
    c("name", "call_info", "source", "component", "component_id")
  )
})

test_that("tunable is setup to works with extract_parameter_set_dials", {
  skip_if_not_installed("dials")
  rec <- recipe(~., data = mtcars) |>
    step_kmeans_smote(
      all_predictors(),
      over_ratio = hardhat::tune(),
      neighbors = hardhat::tune(),
      num_clusters = hardhat::tune()
    )

  params <- extract_parameter_set_dials(rec)

  expect_s3_class(params, "parameters")
  expect_identical(nrow(params), 3L)
})

test_that("indicator_column adds logical column marking synthetic rows", {
  rec <- recipe(class ~ x + y, data = circle_example) |>
    step_kmeans_smote(class, indicator_column = ".new_row") |>
    prep()

  res <- bake(rec, new_data = NULL)

  expect_true(".new_row" %in% names(res))
  expect_type(res$.new_row, "logical")
  expect_equal(sum(!res$.new_row), nrow(circle_example))
  expect_gt(sum(res$.new_row), 0L)
})

test_that("indicator_column bad args", {
  expect_snapshot(
    error = TRUE,
    recipe(class ~ x + y, data = circle_example) |>
      step_kmeans_smote(class, indicator_column = 1)
  )
  expect_snapshot(
    error = TRUE,
    recipe(class ~ x + y, data = circle_example) |>
      step_kmeans_smote(class, indicator_column = "") |>
      prep()
  )
  expect_snapshot(
    error = TRUE,
    recipe(class ~ x + y, data = circle_example) |>
      step_kmeans_smote(class, indicator_column = "x") |>
      prep()
  )
})

test_that("bad args", {
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) |>
      step_kmeans_smote(over_ratio = "yes") |>
      prep()
  )
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) |>
      step_kmeans_smote(neighbors = TRUE) |>
      prep()
  )
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) |>
      step_kmeans_smote(num_clusters = 1) |>
      prep()
  )
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) |>
      step_kmeans_smote(cluster_balance_threshold = "yes") |>
      prep()
  )
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) |>
      step_kmeans_smote(density_exponent = -1) |>
      prep()
  )
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) |>
      step_kmeans_smote(seed = TRUE)
  )
})

test_that("unused outcome levels are skipped with a warning (#238)", {
  circle_example$class <- factor(
    circle_example$class,
    levels = c(levels(circle_example$class), "unused")
  )

  expect_snapshot(
    res <- recipe(class ~ x + y, data = circle_example) |>
      step_kmeans_smote(class) |>
      prep() |>
      bake(new_data = NULL)
  )

  expect_gt(nrow(res), 0)
})

test_that("step_kmeans_smote() errors with case weights (#243)", {
  df <- circle_example[c("x", "y", "class")]
  df$wts <- hardhat::frequency_weights(rep(1L, nrow(df)))

  expect_snapshot(
    error = TRUE,
    recipe(class ~ ., data = df) |>
      step_kmeans_smote(class, skip = FALSE) |>
      prep() |>
      bake(new_data = NULL)
  )
})

test_that("kmeans_smote() with a constant vector matches the scalar (#323)", {
  set.seed(3)
  df <- data.frame(
    x = c(rnorm(10, 0), rnorm(20, 10), rnorm(40, 20)),
    y = c(rnorm(10, 0), rnorm(20, 10), rnorm(40, 20)),
    class = factor(c(rep("a", 10), rep("b", 20), rep("c", 40)))
  )

  set.seed(2)
  res_vec <- kmeans_smote(
    df,
    "class",
    over_ratio = c(a = 0.5, b = 0.5, c = 0.5)
  )
  set.seed(2)
  res_scalar <- kmeans_smote(df, "class", over_ratio = 0.5)

  expect_equal(res_vec, res_scalar)
})

test_that("kmeans_smote() targets a single class with a named vector (#323)", {
  set.seed(3)
  df <- data.frame(
    x = c(rnorm(10, 0), rnorm(20, 10), rnorm(40, 20)),
    y = c(rnorm(10, 0), rnorm(20, 10), rnorm(40, 20)),
    class = factor(c(rep("a", 10), rep("b", 20), rep("c", 40)))
  )

  res <- kmeans_smote(df, "class", over_ratio = c(a = 1))

  expect_equal(as.numeric(table(res$class)), c(40, 20, 40))
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  rec <- recipe(class ~ x + y, data = circle_example) |>
    step_kmeans_smote(class, skip = FALSE) |>
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
  rec <- step_kmeans_smote(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_kmeans_smote(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_kmeans_smote(rec)

  expect <- tibble(terms = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  rec <- recipe(class ~ x + y, data = circle_example) |>
    step_kmeans_smote(class)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("0 and 1 rows data work in bake method", {
  rec <- recipe(class ~ x + y, data = circle_example) |>
    step_kmeans_smote(class, skip = FALSE) |>
    prep()

  expect_identical(nrow(bake(rec, new_data = slice(circle_example, 0))), 0L)
  expect_identical(nrow(bake(rec, new_data = slice(circle_example, 1))), 1L)
})
