tune_tbl <- function(step_fn) {
  rec <- recipe(~., data = mtcars) |>
    step_fn(all_predictors())
  tunable(rec$steps[[1]])
}

test_that("tunable.step_adasyn", {
  res <- tune_tbl(step_adasyn)
  expect_named(
    res,
    c("name", "call_info", "source", "component", "component_id")
  )
  expect_equal(res$name, c("over_ratio", "neighbors"))
  expect_equal(res$call_info[[1]], list(pkg = "dials", fun = "over_ratio"))
  expect_equal(
    res$call_info[[2]],
    list(pkg = "dials", fun = "neighbors", range = c(1, 10))
  )
  expect_all_equal(res$source, "recipe")
  expect_all_equal(res$component, "step_adasyn")
})

test_that("tunable.step_bsmote", {
  res <- tune_tbl(step_bsmote)
  expect_equal(res$name, c("over_ratio", "neighbors", "all_neighbors"))
  expect_equal(res$call_info[[1]], list(pkg = "dials", fun = "over_ratio"))
  expect_equal(
    res$call_info[[2]],
    list(pkg = "dials", fun = "neighbors", range = c(1, 10))
  )
  expect_equal(res$call_info[[3]], list(pkg = "dials", fun = "all_neighbors"))
  expect_all_equal(res$component, "step_bsmote")
})

test_that("tunable.step_cluster_centroids", {
  res <- tune_tbl(step_cluster_centroids)
  expect_equal(res$name, "under_ratio")
  expect_equal(res$call_info[[1]], list(pkg = "dials", fun = "under_ratio"))
  expect_all_equal(res$component, "step_cluster_centroids")
})

test_that("tunable.step_downsample", {
  res <- tune_tbl(step_downsample)
  expect_equal(res$name, "under_ratio")
  expect_equal(res$call_info[[1]], list(pkg = "dials", fun = "under_ratio"))
  expect_all_equal(res$component, "step_downsample")
})

test_that("tunable.step_enn", {
  res <- tune_tbl(step_enn)
  expect_equal(res$name, c("neighbors", "all_k"))
  expect_equal(
    res$call_info[[1]],
    list(pkg = "dials", fun = "neighbors", range = c(1, 10))
  )
  expect_equal(res$call_info[[2]], list(pkg = "dials", fun = "prune"))
  expect_all_equal(res$component, "step_enn")
})

test_that("tunable.step_instance_hardness", {
  res <- tune_tbl(step_instance_hardness)
  expect_equal(res$name, c("under_ratio", "neighbors"))
  expect_equal(res$call_info[[1]], list(pkg = "dials", fun = "under_ratio"))
  expect_equal(
    res$call_info[[2]],
    list(pkg = "dials", fun = "neighbors", range = c(1, 10))
  )
  expect_all_equal(res$component, "step_instance_hardness")
})

test_that("tunable.step_kmeans_smote", {
  res <- tune_tbl(step_kmeans_smote)
  expect_equal(res$name, c("over_ratio", "neighbors", "num_clusters"))
  expect_equal(res$call_info[[1]], list(pkg = "dials", fun = "over_ratio"))
  expect_equal(
    res$call_info[[2]],
    list(pkg = "dials", fun = "neighbors", range = c(1, 10))
  )
  expect_equal(
    res$call_info[[3]],
    list(pkg = "dials", fun = "num_clusters", range = c(2, 10))
  )
  expect_all_equal(res$component, "step_kmeans_smote")
})

test_that("tunable.step_ncl", {
  res <- tune_tbl(step_ncl)
  expect_equal(res$name, c("neighbors", "threshold_clean"))
  expect_equal(
    res$call_info[[1]],
    list(pkg = "dials", fun = "neighbors", range = c(1, 10))
  )
  expect_equal(res$call_info[[2]], list(pkg = "dials", fun = "threshold"))
  expect_all_equal(res$component, "step_ncl")
})

test_that("tunable.step_nearmiss", {
  res <- tune_tbl(step_nearmiss)
  expect_equal(res$name, c("under_ratio", "neighbors"))
  expect_equal(res$call_info[[1]], list(pkg = "dials", fun = "under_ratio"))
  expect_equal(
    res$call_info[[2]],
    list(pkg = "dials", fun = "neighbors", range = c(1, 10))
  )
  expect_all_equal(res$component, "step_nearmiss")
})

test_that("tunable.step_rose", {
  res <- tune_tbl(step_rose)
  expect_equal(res$name, "over_ratio")
  expect_equal(res$call_info[[1]], list(pkg = "dials", fun = "over_ratio"))
  expect_all_equal(res$component, "step_rose")
})

test_that("tunable.step_smogn", {
  res <- tune_tbl(step_smogn)
  expect_equal(res$name, c("neighbors", "threshold"))
  expect_equal(
    res$call_info[[1]],
    list(pkg = "dials", fun = "neighbors", range = c(1, 10))
  )
  expect_equal(res$call_info[[2]], list(pkg = "dials", fun = "threshold"))
  expect_all_equal(res$component, "step_smogn")
})

test_that("tunable.step_smote", {
  res <- tune_tbl(step_smote)
  expect_equal(res$name, c("over_ratio", "neighbors"))
  expect_equal(res$call_info[[1]], list(pkg = "dials", fun = "over_ratio"))
  expect_equal(
    res$call_info[[2]],
    list(pkg = "dials", fun = "neighbors", range = c(1, 10))
  )
  expect_all_equal(res$component, "step_smote")
})

test_that("tunable.step_smoten", {
  res <- tune_tbl(step_smoten)
  expect_equal(res$name, c("over_ratio", "neighbors"))
  expect_equal(res$call_info[[1]], list(pkg = "dials", fun = "over_ratio"))
  expect_equal(
    res$call_info[[2]],
    list(pkg = "dials", fun = "neighbors", range = c(1, 10))
  )
  expect_all_equal(res$component, "step_smoten")
})

test_that("tunable.step_smotenc", {
  res <- tune_tbl(step_smotenc)
  expect_equal(res$name, c("over_ratio", "neighbors"))
  expect_equal(res$call_info[[1]], list(pkg = "dials", fun = "over_ratio"))
  expect_equal(
    res$call_info[[2]],
    list(pkg = "dials", fun = "neighbors", range = c(1, 10))
  )
  expect_all_equal(res$component, "step_smotenc")
})

test_that("tunable.step_svmsmote", {
  res <- tune_tbl(step_svmsmote)
  expect_equal(res$name, c("over_ratio", "neighbors", "m_neighbors"))
  expect_equal(res$call_info[[1]], list(pkg = "dials", fun = "over_ratio"))
  expect_equal(
    res$call_info[[2]],
    list(pkg = "dials", fun = "neighbors", range = c(1, 10))
  )
  expect_equal(
    res$call_info[[3]],
    list(pkg = "dials", fun = "neighbors", range = c(1, 20))
  )
  expect_all_equal(res$component, "step_svmsmote")
})

test_that("tunable.step_upsample", {
  res <- tune_tbl(step_upsample)
  expect_equal(res$name, "over_ratio")
  expect_equal(res$call_info[[1]], list(pkg = "dials", fun = "over_ratio"))
  expect_all_equal(res$component, "step_upsample")
})

test_that("tunable works with extract_parameter_set_dials", {
  skip_if_not_installed("dials")
  rec <- recipe(~., data = mtcars) |>
    step_smote(
      all_predictors(),
      over_ratio = hardhat::tune(),
      neighbors = hardhat::tune()
    )
  params <- extract_parameter_set_dials(rec)
  expect_s3_class(params, "parameters")
  expect_identical(nrow(params), 2L)
})

over_ratio_steps <- c(
  "step_adasyn",
  "step_bsmote",
  "step_kmeans_smote",
  "step_smote",
  "step_smoten",
  "step_smotenc",
  "step_svmsmote",
  "step_upsample"
)

under_ratio_steps <- c(
  "step_cluster_centroids",
  "step_downsample",
  "step_instance_hardness",
  "step_nearmiss"
)

tune_names <- function(step_name, ...) {
  step_fn <- getFromNamespace(step_name, "themis")
  rec <- recipe(~., data = mtcars) |>
    step_fn(all_predictors(), ...)
  tunable(rec$steps[[1]])$name
}

test_that("a named ratio vector opts every step out of tuning (#323)", {
  for (step_name in over_ratio_steps) {
    scalar <- tune_names(step_name)
    vector <- tune_names(step_name, over_ratio = c(a = 1, b = 0.5))

    expect_true("over_ratio" %in% scalar)
    expect_equal(vector, setdiff(scalar, "over_ratio"))
  }

  for (step_name in under_ratio_steps) {
    scalar <- tune_names(step_name)
    vector <- tune_names(step_name, under_ratio = c(a = 1, b = 2))

    expect_true("under_ratio" %in% scalar)
    expect_equal(vector, setdiff(scalar, "under_ratio"))
  }
})

test_that("a `tune()` ratio is still tunable (#323)", {
  # `names(quote(tune(id = "x")))` is not NULL, so the opt-out guard has to
  # look at more than the names to avoid dropping a tuned parameter.
  for (step_name in over_ratio_steps) {
    expect_equal(
      tune_names(step_name, over_ratio = hardhat::tune()),
      tune_names(step_name)
    )
    expect_equal(
      tune_names(step_name, over_ratio = hardhat::tune(id = "ratio")),
      tune_names(step_name)
    )
  }

  for (step_name in under_ratio_steps) {
    expect_equal(
      tune_names(step_name, under_ratio = hardhat::tune()),
      tune_names(step_name)
    )
  }
})

test_that("step_rose() keeps a scalar `over_ratio` tunable (#323)", {
  expect_equal(tune_names("step_rose"), "over_ratio")
  expect_equal(
    tune_names("step_rose", over_ratio = hardhat::tune()),
    "over_ratio"
  )
})
