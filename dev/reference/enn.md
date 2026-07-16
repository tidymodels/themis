# Edited Nearest Neighbors

Removes observations whose class differs from the majority of their
nearest neighbors.

## Usage

``` r
enn(df, var, neighbors = 3, distance = "euclidean", times = 1, all_k = FALSE)
```

## Arguments

- df:

  data.frame or tibble. Must have 1 factor variable and remaining
  numeric variables.

- var:

  Character, name of variable containing factor variable.

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

- all_k:

  A logical. When `TRUE`, ENN is applied with an increasing number of
  neighbors, from `1` up to `neighbors`, cleaning the data at each step
  (All k-Nearest Neighbors). Takes precedence over `times`. Defaults to
  `FALSE`.

## Value

A data.frame or tibble, depending on type of `df`.

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

Setting `all_k = TRUE` applies ENN with increasing numbers of neighbors,
from `1` up to `neighbors`, cleaning the data at each step. This
corresponds to All k-Nearest Neighbors (AllKNN) and takes precedence
over `times`.

All columns used in this function must be numeric with no missing data.

Use `times = Inf` to repeat ENN until convergence.

## References

Wilson, D. L. (1972). Asymptotic properties of nearest neighbor rules
using edited data. IEEE Transactions on Systems, Man, and Cybernetics,
(3), 408-421.

Tomek, I. (1976). An experiment with the edited nearest-neighbor rule.
IEEE Transactions on Systems, Man, and Cybernetics, (6), 448-452.

## See also

[`step_enn()`](https://themis.tidymodels.org/dev/reference/step_enn.md)
for step function of this method

Other Direct Implementations:
[`adasyn()`](https://themis.tidymodels.org/dev/reference/adasyn.md),
[`bsmote()`](https://themis.tidymodels.org/dev/reference/bsmote.md),
[`instance_hardness()`](https://themis.tidymodels.org/dev/reference/instance_hardness.md),
[`ncl()`](https://themis.tidymodels.org/dev/reference/ncl.md),
[`nearmiss()`](https://themis.tidymodels.org/dev/reference/nearmiss.md),
[`rose()`](https://themis.tidymodels.org/dev/reference/rose.md),
[`smote()`](https://themis.tidymodels.org/dev/reference/smote.md),
[`smotenc()`](https://themis.tidymodels.org/dev/reference/smotenc.md),
[`tomek()`](https://themis.tidymodels.org/dev/reference/tomek.md)

## Examples

``` r
circle_numeric <- circle_example[, c("x", "y", "class")]

res <- enn(circle_numeric, var = "class")

res <- enn(circle_numeric, var = "class", neighbors = 5)

res <- enn(circle_numeric, var = "class", distance = "manhattan")

# Repeated Edited Nearest Neighbors (RENN)
res <- enn(circle_numeric, var = "class", times = Inf)

# All k-Nearest Neighbors (AllKNN)
res <- enn(circle_numeric, var = "class", all_k = TRUE)
```
