# One-Sided Selection

Under-samples the majority classes by combining Condensed Nearest
Neighbors and Tomek's links, first reducing redundant majority class
observations and then removing majority class observations that form
Tomek links with minority class observations.

## Usage

``` r
oss(df, var, distance = "euclidean")
```

## Arguments

- df:

  data.frame or tibble. Must have 1 factor variable and remaining
  numeric variables.

- var:

  Character, name of variable containing factor variable.

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

## Value

A data.frame or tibble, depending on type of `df`.

## Details

One-Sided Selection (OSS) is an under-sampling method that combines two
cleaning techniques. It first applies Condensed Nearest Neighbors (CNN)
to reduce the majority classes to a consistent subset that correctly
classifies the data using a 1-nearest-neighbor rule, discarding
redundant interior observations. It then applies Tomek's links to the
remaining observations, removing the majority class observations that
form Tomek links with minority class observations, cleaning the decision
boundary.

The smallest class is treated as the minority class and is always kept.
Because the CNN step relies on a random seed observation and a random
scan order, results depend on the random seed.

With more than two classes, the Tomek's links step removes both members
of a majority-majority link, not only links between a majority and the
minority class. The binary case, the primary intended use, is
unaffected.

All columns used in this function must be numeric with no missing data.

## References

Kubat, M., & Matwin, S. (1997). Addressing the curse of imbalanced
training sets: one-sided selection. In ICML (Vol. 97, pp. 179-186).

## See also

[`step_oss()`](https://themis.tidymodels.org/reference/step_oss.md) for
step function of this method

Other Direct Implementations:
[`adasyn()`](https://themis.tidymodels.org/reference/adasyn.md),
[`bsmote()`](https://themis.tidymodels.org/reference/bsmote.md),
[`cluster_centroids()`](https://themis.tidymodels.org/reference/cluster_centroids.md),
[`cnn()`](https://themis.tidymodels.org/reference/cnn.md),
[`enn()`](https://themis.tidymodels.org/reference/enn.md),
[`instance_hardness()`](https://themis.tidymodels.org/reference/instance_hardness.md),
[`kmeans_smote()`](https://themis.tidymodels.org/reference/kmeans_smote.md),
[`ncl()`](https://themis.tidymodels.org/reference/ncl.md),
[`nearmiss()`](https://themis.tidymodels.org/reference/nearmiss.md),
[`rose()`](https://themis.tidymodels.org/reference/rose.md),
[`smogn()`](https://themis.tidymodels.org/reference/smogn.md),
[`smote()`](https://themis.tidymodels.org/reference/smote.md),
[`smoten()`](https://themis.tidymodels.org/reference/smoten.md),
[`smotenc()`](https://themis.tidymodels.org/reference/smotenc.md),
[`svmsmote()`](https://themis.tidymodels.org/reference/svmsmote.md),
[`tomek()`](https://themis.tidymodels.org/reference/tomek.md)

## Examples

``` r
circle_numeric <- circle_example[, c("x", "y", "class")]

res <- oss(circle_numeric, var = "class")

res <- oss(circle_numeric, var = "class", distance = "manhattan")
```
