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
  expect_equal(res$call_info[[2]], list(pkg = "dials", fun = "neighbors"))
  expect_equal(res$call_info[[3]], list(pkg = "dials", fun = "all_neighbors"))
  expect_all_equal(res$component, "step_bsmote")
})

test_that("tunable.step_downsample", {
  res <- tune_tbl(step_downsample)
  expect_equal(res$name, "under_ratio")
  expect_equal(res$call_info[[1]], list(pkg = "dials", fun = "under_ratio"))
  expect_all_equal(res$component, "step_downsample")
})

test_that("tunable.step_enn", {
  res <- tune_tbl(step_enn)
  expect_equal(res$name, "neighbors")
  expect_equal(
    res$call_info[[1]],
    list(pkg = "dials", fun = "neighbors", range = c(1, 10))
  )
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

test_that("tunable.step_ncl", {
  res <- tune_tbl(step_ncl)
  expect_equal(res$name, "neighbors")
  expect_equal(
    res$call_info[[1]],
    list(pkg = "dials", fun = "neighbors", range = c(1, 10))
  )
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
  expect_equal(res$name, "neighbors")
  expect_equal(
    res$call_info[[1]],
    list(pkg = "dials", fun = "neighbors", range = c(1, 10))
  )
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
  expect_equal(res$name, c("over_ratio", "neighbors"))
  expect_equal(res$call_info[[1]], list(pkg = "dials", fun = "over_ratio"))
  expect_equal(res$call_info[[2]], list(pkg = "dials", fun = "neighbors"))
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
