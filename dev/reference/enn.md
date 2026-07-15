# Edited Nearest Neighbors

Removes observations whose class differs from the majority of their
nearest neighbors.

## Usage

``` r
enn(df, var, neighbors = 3, distance = "euclidean")
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

## Value

A data.frame or tibble, depending on type of `df`.

## Details

All columns used in this function must be numeric with no missing data.

## References

Wilson, D. L. (1972). Asymptotic properties of nearest neighbor rules
using edited data. IEEE Transactions on Systems, Man, and Cybernetics,
(3), 408-421.

## See also

[`step_enn()`](https://themis.tidymodels.org/dev/reference/step_enn.md)
for step function of this method

Other Direct Implementations:
[`adasyn()`](https://themis.tidymodels.org/dev/reference/adasyn.md),
[`bsmote()`](https://themis.tidymodels.org/dev/reference/bsmote.md),
[`instance_hardness()`](https://themis.tidymodels.org/dev/reference/instance_hardness.md),
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
```
