# Neighborhood Cleaning Rule

`step_ncl()` creates a *specification* of a recipe step that removes
majority class observations that are noisy or that pollute the
neighborhood of minority class observations.

## Usage

``` r
step_ncl(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  column = NULL,
  neighbors = 3,
  distance = "euclidean",
  threshold_clean = 0.5,
  skip = TRUE,
  seed = sample.int(10^5, 1),
  distance_with = recipes::all_predictors(),
  id = rand_id("ncl")
)
```

## Arguments

- recipe:

  A recipe object. The step will be added to the sequence of operations
  for this recipe.

- ...:

  One or more selector functions to choose which variable is used to
  sample the data. See
  [recipes::selections](https://recipes.tidymodels.org/reference/selections.html)
  for more details. The selection should result in *single factor
  variable*. For the `tidy` method, these are not currently used.

- role:

  Not used by this step since no new variables are created.

- trained:

  A logical to indicate if the quantities for preprocessing have been
  estimated.

- column:

  A character string of the variable name that will be populated
  (eventually) by the `...` selectors.

- neighbors:

  An integer. Number of nearest neighbor that are used to decide whether
  an observation is removed.

- distance:

  A character string specifying the distance metric used for nearest
  neighbor calculations. One of `"euclidean"` (default), `"cosine"`,
  `"mahalanobis"`, `"manhattan"`, or `"chebyshev"`. `"euclidean"`,
  `"cosine"`, and `"mahalanobis"` use approximate nearest neighbors via
  the RANN package and scale well to large datasets. `"manhattan"` and
  `"chebyshev"` compute an exact O(n^2) distance matrix and may be slow
  for large datasets.

- threshold_clean:

  A numeric. Majority classes are only cleaned around minority class
  observations when their size is greater than `threshold_clean` times
  the size of the minority class. Defaults to `0.5`.

- skip:

  A logical. Should the step be skipped when the recipe is baked by
  [`bake()`](https://recipes.tidymodels.org/reference/bake.html)? While
  all operations are baked when
  [`prep()`](https://recipes.tidymodels.org/reference/prep.html) is run,
  some operations may not be able to be conducted on new data (e.g.
  processing the outcome variable(s)). Care should be taken when using
  `skip = TRUE` as it may affect the computations for subsequent
  operations.

- seed:

  An integer that will be used as the seed when applied.

- distance_with:

  A call to a selector function to choose which variables are used for
  distance calculations. Defaults to
  [`recipes::all_predictors()`](https://recipes.tidymodels.org/reference/has_role.html).
  The variable selected by `...` is always excluded from the distance
  calculations.

- id:

  A character string that is unique to this step to identify it.

## Value

An updated version of `recipe` with the new step added to the sequence
of existing steps (if any). For the `tidy` method, a tibble with columns
`terms` which is the variable used to sample.

## Details

The Neighborhood Cleaning Rule (NCL) is a cleaning method that combines
two passes over the data. First, it applies the Edited Nearest Neighbors
rule, removing majority class observations whose class differs from the
majority of their `neighbors` nearest neighbors. Second, for each
minority class observation that is itself misclassified by its
neighbors, the majority class observations among those neighbors are
removed. Compared to Edited Nearest Neighbors, this focuses the cleaning
on the neighborhoods of minority class observations.

The smallest class is treated as the minority class. Only majority
classes larger than `threshold_clean` times the size of the minority
class are cleaned in the second pass.

All variables selected by `distance_with` must be numeric with no
missing data.

All columns in the data are sampled and returned by
[`recipes::juice()`](https://recipes.tidymodels.org/reference/juice.html)
and
[`recipes::bake()`](https://recipes.tidymodels.org/reference/bake.html).

When used in modeling, users should strongly consider using the option
`skip = TRUE` so that the extra sampling is *not* conducted outside of
the training set.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/reference/tidy.recipe.html)
this step, a tibble is returned with columns `terms` and `id`:

- terms:

  character, the selectors or variables selected

- id:

  character, id of this step

## Tuning Parameters

This step has 1 tuning parameters:

- `neighbors`: \# Nearest Neighbors (type: integer, default: 3)

## Case weights

The underlying operation does not allow for case weights. Supplying data
with a case weights column to this step results in an error.

## References

Laurikkala, J. (2001). Improving identification of difficult small
classes by balancing class distribution. In Conference on Artificial
Intelligence in Medicine in Europe (pp. 63-66). Springer.

## See also

[`ncl()`](https://themis.tidymodels.org/dev/reference/ncl.md) for direct
implementation

Other Steps for under-sampling:
[`step_cnn()`](https://themis.tidymodels.org/dev/reference/step_cnn.md),
[`step_downsample()`](https://themis.tidymodels.org/dev/reference/step_downsample.md),
[`step_enn()`](https://themis.tidymodels.org/dev/reference/step_enn.md),
[`step_instance_hardness()`](https://themis.tidymodels.org/dev/reference/step_instance_hardness.md),
[`step_nearmiss()`](https://themis.tidymodels.org/dev/reference/step_nearmiss.md),
[`step_oss()`](https://themis.tidymodels.org/dev/reference/step_oss.md),
[`step_tomek()`](https://themis.tidymodels.org/dev/reference/step_tomek.md)

## Examples

``` r
library(recipes)
library(modeldata)
data(hpc_data)

hpc_data0 <- hpc_data |>
  select(-protocol, -day)

orig <- count(hpc_data0, class, name = "orig")
orig
#> # A tibble: 4 × 2
#>   class  orig
#>   <fct> <int>
#> 1 VF     2211
#> 2 F      1347
#> 3 M       514
#> 4 L       259

up_rec <- recipe(class ~ ., data = hpc_data0) |>
  step_ncl(class) |>
  prep()

training <- up_rec |>
  bake(new_data = NULL) |>
  count(class, name = "training")
training
#> # A tibble: 4 × 2
#>   class training
#>   <fct>    <int>
#> 1 VF        1711
#> 2 F          709
#> 3 M          225
#> 4 L          259

# Since `skip` defaults to TRUE, baking the step has no effect
baked <- up_rec |>
  bake(new_data = hpc_data0) |>
  count(class, name = "baked")
baked
#> # A tibble: 4 × 2
#>   class baked
#>   <fct> <int>
#> 1 VF     2211
#> 2 F      1347
#> 3 M       514
#> 4 L       259

orig |>
  left_join(training, by = "class") |>
  left_join(baked, by = "class")
#> # A tibble: 4 × 4
#>   class  orig training baked
#>   <fct> <int>    <int> <int>
#> 1 VF     2211     1711  2211
#> 2 F      1347      709  1347
#> 3 M       514      225   514
#> 4 L       259      259   259

library(ggplot2)

ggplot(circle_example, aes(x, y, color = class)) +
  geom_point() +
  labs(title = "Without NCL") +
  xlim(c(1, 15)) +
  ylim(c(1, 15))


recipe(class ~ x + y, data = circle_example) |>
  step_ncl(class) |>
  prep() |>
  bake(new_data = NULL) |>
  ggplot(aes(x, y, color = class)) +
  geom_point() +
  labs(title = "With NCL") +
  xlim(c(1, 15)) +
  ylim(c(1, 15))
```
