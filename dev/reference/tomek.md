# Remove Tomek's links

Removed observations that are part of tomek links.

## Usage

``` r
tomek(df, var, distance = "euclidean")
```

## Arguments

- df:

  data.frame or tibble. Must have 1 factor variable and remaining
  numeric variables.

- var:

  Character, name of variable containing factor variable.

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

Tomek. Two modifications of cnn. IEEE Trans. Syst. Man Cybern.,
6:769-772, 1976.

## See also

[`step_tomek()`](https://themis.tidymodels.org/dev/reference/step_tomek.md)
for step function of this method

Other Direct Implementations:
[`adasyn()`](https://themis.tidymodels.org/dev/reference/adasyn.md),
[`bsmote()`](https://themis.tidymodels.org/dev/reference/bsmote.md),
[`enn()`](https://themis.tidymodels.org/dev/reference/enn.md),
[`instance_hardness()`](https://themis.tidymodels.org/dev/reference/instance_hardness.md),
[`nearmiss()`](https://themis.tidymodels.org/dev/reference/nearmiss.md),
[`rose()`](https://themis.tidymodels.org/dev/reference/rose.md),
[`smote()`](https://themis.tidymodels.org/dev/reference/smote.md),
[`smotenc()`](https://themis.tidymodels.org/dev/reference/smotenc.md)

## Examples

``` r
circle_numeric <- circle_example[, c("x", "y", "class")]

res <- tomek(circle_numeric, var = "class")

res <- tomek(circle_numeric, var = "class", distance = "manhattan")
```
