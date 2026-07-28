# Common pitfalls

Resampling is easy to misuse in ways that quietly inflate your
performance estimates. This article covers the two mistakes that matter
most, mirroring imbalanced-learn’s [Common pitfalls and recommended
practices](https://imbalanced-learn.org/stable/common_pitfalls.html)
page. The short version: **resampling must only ever touch the training
data**, and doing it before you split for validation leaks information
across the split.

``` r

library(recipes)
library(themis)
```

## Resample the training set only

Every row-changing themis step defaults to `skip = TRUE`. This means the
step runs during
[`prep()`](https://recipes.tidymodels.org/reference/prep.html) and
during `bake(new_data = NULL)` (the training data), but is *skipped*
when new data is baked, such as when a workflow bakes the assessment set
at [`predict()`](https://rdrr.io/r/stats/predict.html) time. The data
you evaluate on keeps its natural class balance.

We can see this directly. Here is a small imbalanced training set and a
held-out set:

``` r

set.seed(1)
train <- data.frame(
  class = factor(rep(c("minority", "majority"), c(20, 180))),
  x = rnorm(200),
  y = rnorm(200)
)
test <- data.frame(
  class = factor(rep(c("minority", "majority"), c(5, 45))),
  x = rnorm(50),
  y = rnorm(50)
)

rec <- recipe(class ~ x + y, data = train) |>
  step_smote(class) |>
  prep()
```

Baking the training data applies SMOTE, balancing the classes:

``` r

bake(rec, new_data = NULL) |>
  count(class)
#> # A tibble: 2 × 2
#>   class        n
#>   <fct>    <int>
#> 1 majority   180
#> 2 minority   180
```

Baking new data skips the step, so the held-out set keeps its natural,
imbalanced distribution:

``` r

bake(rec, new_data = test) |>
  count(class)
#> # A tibble: 2 × 2
#>   class        n
#>   <fct>    <int>
#> 1 majority    45
#> 2 minority     5
```

This is exactly what you want: the model trains on balanced data, but is
evaluated on the real class balance it will see in production. Leaving
`skip = TRUE` (the default) is almost always correct.

## Do not resample before splitting or cross-validation

The most common leakage mistake is applying a sampling step to the
*whole* data set before splitting it, for example running SMOTE and then
calling `rsample::initial_split()` or `rsample::vfold_cv()`. Synthetic
minority points are built by interpolating between real points, so a
synthetic row can land in the analysis fold while the real rows it was
derived from land in the assessment fold. The model then effectively
sees the assessment data during training, and the resulting metrics are
optimistic and unrealistic.

``` r

# WRONG: resampling leaks across the cross-validation split
resampled <- recipe(class ~ ., data = train) |>
  step_smote(class) |>
  prep() |>
  bake(new_data = NULL)

folds <- rsample::vfold_cv(resampled) # synthetic rows now span folds
```

The correct pattern is to put the step in a recipe inside a workflow,
and let `tune::fit_resample()` or `tune::tune_grid()` apply it *inside
each analysis fold only*. Because the step has `skip = TRUE`, it never
touches the assessment fold.

``` r

# CORRECT: resample the raw data, then apply the recipe inside each fold
folds <- rsample::vfold_cv(train)

rec <- recipe(class ~ ., data = train) |>
  step_smote(class)

wf <- workflows::workflow() |>
  workflows::add_recipe(rec) |>
  workflows::add_model(parsnip::logistic_reg())

tune::fit_resamples(wf, folds)
```

Here SMOTE is fit on each analysis fold and skipped on each assessment
fold, so metrics are computed on the natural class balance. This is the
honest estimate, and it is usually noticeably lower than the leaky
version, which is the point.

## The imbalanced-learn parallel

If you are coming from Python, this is the same guarantee that
imbalanced-learn’s `Pipeline` provides. There, a sampler’s
`fit_resample` runs during `fit` (on the training fold) and is bypassed
at `predict`/`score`. In tidymodels, recipes’ `skip = TRUE` plus a
workflow gives you the identical “resample train, bypass test” behavior,
without a special pipeline object.
