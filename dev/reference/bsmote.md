# borderline-SMOTE Algorithm

BSMOTE generates new examples of the minority class using nearest
neighbors of these cases in the border region between classes.

## Usage

``` r
bsmote(
  df,
  var,
  k = 5,
  over_ratio = 1,
  all_neighbors = FALSE,
  distance = "euclidean"
)
```

## Arguments

- df:

  data.frame or tibble. Must have 1 factor variable and remaining
  numeric variables.

- var:

  Character, name of variable containing factor variable.

- k:

  An integer. Number of nearest neighbor that are used to generate the
  new examples of the minority class.

- over_ratio:

  A numeric value for the ratio of the minority-to-majority frequencies.
  The default value (1) means that all other levels are sampled up to
  have the same frequency as the most occurring level. A value of 0.5
  would mean that the minority levels will have (at most)
  (approximately) half as many rows as the majority level. See
  `vignette("ratio", package = "themis")` for more details.

- all_neighbors:

  Type of two borderline-SMOTE method. Defaults to FALSE. See details.

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

BSMOTE (borderline-SMOTE) works the same way as SMOTE, except that
instead of generating points around every point of the minority class
each point is first classified into the boxes "danger" and "not". For
each point the nearest neighbors are calculated. If all the neighbors
come from a different class it is labeled noise and put into the "not"
box. If more than half of the neighbors come from a different class it
is labeled "danger". Points are generated around points labeled
"danger".

If `all_neighbors = FALSE` then points are generated between nearest
neighbors in its own class. If `all_neighbors = TRUE` then points are
generated between any nearest neighbors. See examples for visualization.

SMOTE generates new examples of the minority class using nearest
neighbors of these cases. For each existing minority class example, new
examples are created by interpolating between the example and its
nearest neighbors. The number of nearest neighbors used is controlled by
the number of neighbors argument (`k` in
[`smote()`](https://themis.tidymodels.org/dev/reference/smote.md),
`neighbors` in
[`step_smote()`](https://themis.tidymodels.org/dev/reference/step_smote.md)),
and the number of new examples generated is controlled by `over_ratio`.

All columns used in this function must be numeric with no missing data.

## References

Hui Han, Wen-Yuan Wang, and Bing-Huan Mao. Borderline-smote: a new
over-sampling method in imbalanced data sets learning. In International
Conference on Intelligent Computing, pages 878–887. Springer, 2005.

## See also

[`step_bsmote()`](https://themis.tidymodels.org/dev/reference/step_bsmote.md)
for step function of this method

Other Direct Implementations:
[`adasyn()`](https://themis.tidymodels.org/dev/reference/adasyn.md),
[`cnn()`](https://themis.tidymodels.org/dev/reference/cnn.md),
[`enn()`](https://themis.tidymodels.org/dev/reference/enn.md),
[`instance_hardness()`](https://themis.tidymodels.org/dev/reference/instance_hardness.md),
[`ncl()`](https://themis.tidymodels.org/dev/reference/ncl.md),
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

res <- bsmote(circle_numeric, var = "class")

res <- bsmote(circle_numeric, var = "class", k = 10)

res <- bsmote(circle_numeric, var = "class", over_ratio = 0.8)

res <- bsmote(circle_numeric, var = "class", all_neighbors = TRUE)

res <- bsmote(circle_numeric, var = "class", distance = "manhattan")
```
