# SMOGN Algorithm

SMOGN generates new examples for imbalanced regression problems. It
identifies rare regions of the continuous outcome using a relevance
function, over-samples those regions with a combination of SMOTE-style
interpolation and the introduction of Gaussian noise, and under-samples
the common regions.

## Usage

``` r
smogn(
  df,
  var,
  threshold = 0.5,
  relevance = NULL,
  neighbors = 5,
  perturbation = 0.02,
  distance = "euclidean"
)
```

## Arguments

- df:

  data.frame or tibble. Must have 1 numeric outcome variable and
  remaining numeric variables.

- var:

  Character, name of the numeric outcome variable.

- threshold:

  A number between 0 and 1. Outcome values with a relevance at or above
  this value are treated as rare and over-sampled. Defaults to `0.5`.

- relevance:

  A matrix of relevance control points, or `NULL` (default). When
  `NULL`, relevance is derived automatically from the boxplot extremes
  of the outcome. When supplied, the first column gives outcome values
  and the second column their relevance in `[0, 1]`.

- neighbors:

  An integer. Number of nearest neighbor that are used to generate the
  new examples of the rare values.

- perturbation:

  A number. The magnitude of the Gaussian noise added when generating
  synthetic examples in unsafe regions. Defaults to `0.02`.

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

SMOGN is a pre-processing approach for imbalanced regression. A
relevance function assigns each outcome value a relevance score, and
values with a relevance at or above `threshold` are treated as rare. The
data is split into contiguous bins of rare and common outcome values.
Common bins are under-sampled and rare bins are over-sampled toward a
balanced size. New rare examples are generated either by interpolating
between an example and a nearby neighbor (when they are close enough to
be considered safe) or by perturbing the example with Gaussian noise
(when they are not), where the amount of noise is controlled by
`perturbation`.

By default relevance is derived automatically from the boxplot extremes
of the outcome, giving the median a relevance of 0 and the extreme
values a relevance of 1. A matrix of relevance control points can
instead be supplied through `relevance`, with the first column giving
outcome values and the second column their relevance.

All columns used in this function must be numeric with no missing data.

## References

Branco, P., Torgo, L., and Ribeiro, R. P. (2017). SMOGN: a
pre-processing approach for imbalanced regression. Proceedings of
Machine Learning Research, 74:36-50.

## See also

[`step_smogn()`](https://themis.tidymodels.org/reference/step_smogn.md)
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
[`smote()`](https://themis.tidymodels.org/reference/smote.md),
[`smoten()`](https://themis.tidymodels.org/reference/smoten.md),
[`smotenc()`](https://themis.tidymodels.org/reference/smotenc.md),
[`svmsmote()`](https://themis.tidymodels.org/reference/svmsmote.md),
[`tomek()`](https://themis.tidymodels.org/reference/tomek.md)

## Examples

``` r
circle_numeric <- circle_example[, c("x", "y")]

res <- smogn(circle_numeric, var = "x")

res <- smogn(circle_numeric, var = "x", neighbors = 10)
```
