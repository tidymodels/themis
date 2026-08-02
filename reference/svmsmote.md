# SVM-SMOTE Algorithm

SVM-SMOTE generates new examples of the minority class near the decision
boundary, using the support vectors of a fitted SVM to decide where to
place synthetic examples.

## Usage

``` r
svmsmote(
  df,
  var,
  k = 5,
  over_ratio = 1,
  distance = "euclidean",
  m_neighbors = NULL,
  out_step = 0.5
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
  (approximately) half as many rows as the majority level.

  A named numeric vector can be used instead to give different levels
  different targets, for example `c(a = 1, b = 0.5)`. The names must be
  levels of the outcome and the values are ratios of the majority level,
  exactly as in the single-number case. Levels that are not named are
  left untouched, as are rows with a missing outcome. Because a vector
  of targets is not a single value, supplying one means this argument
  can no longer be tuned. See `vignette("ratio", package = "themis")`
  for more details.

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

- m_neighbors:

  An integer or `NULL`. Number of nearest neighbors, among all classes,
  that are used to label each minority support vector as noise, danger,
  or safe. Defaults to `NULL`, which means `2 * k`.

- out_step:

  A number. Step size used when extrapolating new examples away from
  safe support vectors. Defaults to 0.5.

## Value

A data.frame or tibble, depending on type of `df`.

## Details

SVM-SMOTE (Support Vector Machine SMOTE) works the same way as SMOTE,
except that instead of generating points around every point of the
minority class, it focuses generation near the decision boundary. A
support vector machine is fitted to the data and the support vectors
that belong to the minority class are used as the base points for
generating new examples.

For each minority support vector its nearest neighbors among all classes
are calculated. If all of the neighbors come from a different class the
support vector is labeled noise and is discarded. If more than half of
the neighbors come from a different class the support vector is labeled
"danger" and new points are interpolated between it and its
minority-class neighbors. The remaining support vectors are considered
to be in a safe region and new points are extrapolated away from their
minority-class neighbors.

The number of neighbors used for this labeling is controlled by
`m_neighbors`, and how far the extrapolated points are placed is
controlled by `out_step`.

The support vector machine is always fitted with
[`kernlab::ksvm()`](https://rdrr.io/pkg/kernlab/man/ksvm.html) using a
radial basis function kernel (`kernel = "rbfdot"`) and a cost of
`C = 1`, matching the reference implementations of the method. These are
not exposed as arguments because the fitted model is only used to
identify which minority observations are support vectors, and not for
prediction, so the sampling results are relatively insensitive to them.

SMOTE generates new examples of the minority class using nearest
neighbors of these cases. For each existing minority class example, new
examples are created by interpolating between the example and its
nearest neighbors. The number of nearest neighbors used is controlled by
the number of neighbors argument (`k` in
[`smote()`](https://themis.tidymodels.org/reference/smote.md),
`neighbors` in
[`step_smote()`](https://themis.tidymodels.org/reference/step_smote.md)),
and the number of new examples generated is controlled by `over_ratio`.

All columns used in this function must be numeric with no missing data.

## References

Nguyen, H. M., Cooper, E. W., and Kamei, K. (2011). Borderline
over-sampling for imbalanced data classification. International Journal
of Knowledge Engineering and Soft Data Paradigms, 3(1), 4-21.

## See also

[`step_svmsmote()`](https://themis.tidymodels.org/reference/step_svmsmote.md)
for step function of this method

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
[`oss()`](https://themis.tidymodels.org/reference/oss.md),
[`rose()`](https://themis.tidymodels.org/reference/rose.md),
[`smogn()`](https://themis.tidymodels.org/reference/smogn.md),
[`smote()`](https://themis.tidymodels.org/reference/smote.md),
[`smoten()`](https://themis.tidymodels.org/reference/smoten.md),
[`smotenc()`](https://themis.tidymodels.org/reference/smotenc.md),
[`tomek()`](https://themis.tidymodels.org/reference/tomek.md)

## Examples

``` r
circle_numeric <- circle_example[, c("x", "y", "class")]

res <- svmsmote(circle_numeric, var = "class")

res <- svmsmote(circle_numeric, var = "class", k = 10)

res <- svmsmote(circle_numeric, var = "class", over_ratio = 0.8)

res <- svmsmote(circle_numeric, var = "class", distance = "manhattan")

res <- svmsmote(circle_numeric, var = "class", m_neighbors = 20)
```
