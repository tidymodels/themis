# Edited Nearest Neighbors

Removes observations whose class differs from the majority of their
nearest neighbors.

## Usage

``` r
enn(
  df,
  var,
  neighbors = 3,
  distance = "euclidean",
  times = 1,
  all_k = FALSE,
  kind_sel = "mode"
)
```

## Arguments

- df:

  data.frame or tibble. Must have 1 factor variable and remaining
  numeric variables.

- var:

  Character, name of variable containing factor variable.

- neighbors:

  An integer. Number of nearest neighbor that are used to decide whether
  an observation is removed. Defaults to `3`, unlike the over-sampling
  steps which default to `5`.

- distance:

  A character string specifying the distance metric used for nearest
  neighbor calculations, defaulting to `"euclidean"`. The available
  metrics fall into three groups.

  `"euclidean"`, `"cosine"`, and `"mahalanobis"` use approximate nearest
  neighbors via the RANN package and scale well to large datasets.

  `"squared_chord"`, `"matusita"`, `"hellinger"`, and `"bhattacharyya"`
  are probability-divergence measures that treat each row as a
  distribution over the predictors, so they require non-negative values.
  `"hellinger"` and `"bhattacharyya"` further require each row to sum
  to 1. All four also use the RANN package and scale well to large
  datasets.

  `"manhattan"`, `"chebyshev"`, `"canberra"`, `"soergel"`,
  `"lorentzian"`, `"jeffreys"`, `"topsoe"`, `"jensen-shannon"`,
  `"jensen_difference"`, `"taneja"`, and `"kumar-johnson"` compute an
  exact all-pairs distance matrix. This takes time and memory
  proportional to the square of the number of observations in a class,
  so these are best suited to smaller datasets. Everything from
  `"canberra"` onwards is a probability divergence requiring
  non-negative values, is provided by the philentropy package (which
  must be installed separately), and in the case of `"jeffreys"`,
  `"taneja"`, and `"kumar-johnson"` requires strictly positive values,
  since those divide by individual predictor values.

  The probability divergences are meaningful for compositional
  predictors such as proportions or counts normalized per observation,
  and are generally not appropriate for standardized predictors.

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

- kind_sel:

  A character string. The rule used to decide whether an observation is
  removed. `"mode"` (the default) removes an observation when the
  majority of its neighbors disagree with its class. `"all"` is stricter
  and removes an observation unless all of its neighbors share its
  class.

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

Setting `kind_sel = "all"` uses a stricter cleaning rule: instead of
removing an observation when the majority of its neighbors disagree, it
is removed unless every one of its neighbors shares its class. This
removes more observations than the default `kind_sel = "mode"`.

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
[`cnn()`](https://themis.tidymodels.org/dev/reference/cnn.md),
[`instance_hardness()`](https://themis.tidymodels.org/dev/reference/instance_hardness.md),
[`ncl()`](https://themis.tidymodels.org/dev/reference/ncl.md),
[`nearmiss()`](https://themis.tidymodels.org/dev/reference/nearmiss.md),
[`oss()`](https://themis.tidymodels.org/dev/reference/oss.md),
[`rose()`](https://themis.tidymodels.org/dev/reference/rose.md),
[`smogn()`](https://themis.tidymodels.org/dev/reference/smogn.md),
[`smote()`](https://themis.tidymodels.org/dev/reference/smote.md),
[`smoten()`](https://themis.tidymodels.org/dev/reference/smoten.md),
[`smotenc()`](https://themis.tidymodels.org/dev/reference/smotenc.md),
[`svmsmote()`](https://themis.tidymodels.org/dev/reference/svmsmote.md),
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

# Stricter cleaning: remove unless all neighbors agree
res <- enn(circle_numeric, var = "class", kind_sel = "all")
```
