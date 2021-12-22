data(two_class_dat, package = "modeldata")

# ------------------------------------------------------------------------------

r1 <- recipe(Class ~ ., data = two_class_dat)
r2 <- r1 %>% step_adasyn(Class)
r3 <- r1 %>% step_bsmote(Class)
r4 <- r1 %>% step_downsample(Class)
r5 <- r1 %>% step_nearmiss(Class)
r6 <- r1 %>% step_rose(Class)
r7 <- r1 %>% step_smote(Class)
r8 <- r1 %>% step_tomek(Class)
r9 <- r1 %>% step_upsample(Class)

# ------------------------------------------------------------------------------

test_that("required packages", {
  expect_equal(required_pkgs(r2), c("recipes", "themis"))
  expect_equal(required_pkgs(r3), c("recipes", "themis"))
  expect_equal(required_pkgs(r4), c("recipes", "themis"))
  expect_equal(required_pkgs(r5), c("recipes", "themis"))
  expect_equal(required_pkgs(r6), c("recipes", "themis", "ROSE"))
  expect_equal(required_pkgs(r7), c("recipes", "themis"))
  expect_equal(required_pkgs(r8), c("recipes", "themis", "unbalanced"))
  expect_equal(required_pkgs(r9), c("recipes", "themis"))
})

test_that("tunable arguments", {
  expect_equal(tunable(r2)$name, c("over_ratio", "neighbors"))
  expect_equal(
    tunable(r3)$name,
    c("over_ratio", "neighbors", "all_neighbors")
  )
  expect_equal(tunable(r4)$name, "under_ratio")
  expect_equal(tunable(r5)$name, c("under_ratio", "neighbors"))
  expect_equal(tunable(r6)$name, "over_ratio")
  expect_equal(tunable(r7)$name, c("over_ratio", "neighbors"))
  expect_true(nrow(tunable(r8)) == 0)
  expect_equal(tunable(r9)$name, "over_ratio")
})
