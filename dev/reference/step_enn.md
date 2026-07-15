# Edited Nearest Neighbors

`step_enn()` creates a *specification* of a recipe step that removes
observations whose class differs from the majority of their nearest
neighbors.

## Usage

``` r
step_enn(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  column = NULL,
  neighbors = 3,
  distance = "euclidean",
  times = 1,
  skip = TRUE,
  seed = sample.int(10^5, 1),
  distance_with = recipes::all_predictors(),
  id = rand_id("enn")
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

- times:

  A positive integer for the maximum number of times ENN is applied.
  Defaults to `1` for a single pass. Values greater than `1` repeat the
  cleaning, stopping early once a pass removes no observations. Use
  `Inf` to repeat until convergence (Repeated Edited Nearest Neighbors).

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

Edited Nearest Neighbors (ENN) is a cleaning method. For each
observation it finds the `neighbors` nearest neighbors and, if the class
of the observation does not match the majority class among those
neighbors, the observation is removed. This tends to remove noisy and
borderline observations, which can lead to smoother decision boundaries.

Setting `times` greater than 1 applies ENN repeatedly, removing more
noisy and borderline observations on each pass and stopping early once a
pass removes nothing. This corresponds to Repeated Edited Nearest
Neighbors (RENN).

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

The underlying operation does not allow for case weights.

## References

Wilson, D. L. (1972). Asymptotic properties of nearest neighbor rules
using edited data. IEEE Transactions on Systems, Man, and Cybernetics,
(3), 408-421.

Tomek, I. (1976). An experiment with the edited nearest-neighbor rule.
IEEE Transactions on Systems, Man, and Cybernetics, (6), 448-452.

## See also

[`enn()`](https://themis.tidymodels.org/dev/reference/enn.md) for direct
implementation

Other Steps for under-sampling:
[`step_downsample()`](https://themis.tidymodels.org/dev/reference/step_downsample.md),
[`step_instance_hardness()`](https://themis.tidymodels.org/dev/reference/step_instance_hardness.md),
[`step_nearmiss()`](https://themis.tidymodels.org/dev/reference/step_nearmiss.md),
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
  step_enn(class) |>
  prep()

training <- up_rec |>
  bake(new_data = NULL) |>
  count(class, name = "training")
training
#> # A tibble: 4 × 2
#>   class training
#>   <fct>    <int>
#> 1 VF        1737
#> 2 F          732
#> 3 M          262
#> 4 L          173

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
#> 1 VF     2211     1737  2211
#> 2 F      1347      732  1347
#> 3 M       514      262   514
#> 4 L       259      173   259

library(ggplot2)

ggplot(circle_example, aes(x, y, color = class)) +
  geom_point() +
  labs(title = "Without ENN") +
  xlim(c(1, 15)) +
  ylim(c(1, 15))


recipe(class ~ x + y, data = circle_example) |>
  step_enn(class) |>
  prep() |>
  bake(new_data = NULL) |>
  ggplot(aes(x, y, color = class)) +
  geom_point() +
  labs(title = "With ENN") +
  xlim(c(1, 15)) +
  ylim(c(1, 15))
```
