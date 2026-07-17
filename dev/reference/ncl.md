# Neighborhood Cleaning Rule

Under-samples the majority classes by cleaning noisy observations and
observations that pollute the neighborhood of minority class
observations.

## Usage

``` r
ncl(df, var, neighbors = 3, distance = "euclidean", threshold_clean = 0.5)
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

- threshold_clean:

  A numeric. Majority classes are only cleaned around minority class
  observations when their size is greater than `threshold_clean` times
  the size of the minority class. Defaults to `0.5`.

## Value

A data.frame or tibble, depending on type of `df`.

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

All columns used in this function must be numeric with no missing data.

## References

Laurikkala, J. (2001). Improving identification of difficult small
classes by balancing class distribution. In Conference on Artificial
Intelligence in Medicine in Europe (pp. 63-66). Springer.

## See also

[`step_ncl()`](https://themis.tidymodels.org/dev/reference/step_ncl.md)
for step function of this method

Other Direct Implementations:
[`adasyn()`](https://themis.tidymodels.org/dev/reference/adasyn.md),
[`bsmote()`](https://themis.tidymodels.org/dev/reference/bsmote.md),
[`cnn()`](https://themis.tidymodels.org/dev/reference/cnn.md),
[`enn()`](https://themis.tidymodels.org/dev/reference/enn.md),
[`instance_hardness()`](https://themis.tidymodels.org/dev/reference/instance_hardness.md),
[`nearmiss()`](https://themis.tidymodels.org/dev/reference/nearmiss.md),
[`oss()`](https://themis.tidymodels.org/dev/reference/oss.md),
[`rose()`](https://themis.tidymodels.org/dev/reference/rose.md),
[`smogn()`](https://themis.tidymodels.org/dev/reference/smogn.md),
[`smote()`](https://themis.tidymodels.org/dev/reference/smote.md),
[`smoten()`](https://themis.tidymodels.org/dev/reference/smoten.md),
[`smotenc()`](https://themis.tidymodels.org/dev/reference/smotenc.md),
[`tomek()`](https://themis.tidymodels.org/dev/reference/tomek.md)

## Examples

``` r
circle_numeric <- circle_example[, c("x", "y", "class")]

res <- ncl(circle_numeric, var = "class")

res <- ncl(circle_numeric, var = "class", neighbors = 5)

res <- ncl(circle_numeric, var = "class", distance = "manhattan")
```
