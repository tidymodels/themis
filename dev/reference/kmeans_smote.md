# KMeans-SMOTE Algorithm

KMeans-SMOTE clusters the predictor space, keeps only the clusters that
are dominated by the class being over-sampled, and generates new
examples with SMOTE inside those clusters, giving sparser clusters more
of the new points.

## Usage

``` r
kmeans_smote(
  df,
  var,
  k = 2,
  over_ratio = 1,
  num_clusters = NULL,
  cluster_balance_threshold = 1,
  density_exponent = NULL,
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
  (approximately) half as many rows as the majority level.

  A named numeric vector can be used instead to give different levels
  different targets, for example `c(a = 1, b = 0.5)`. The names must be
  levels of the outcome and the values are ratios of the majority level,
  exactly as in the single-number case. Levels that are not named are
  left untouched, as are rows with a missing outcome. Because a vector
  of targets is not a single value, supplying one means this argument
  can no longer be tuned. See `vignette("ratio", package = "themis")`
  for more details.

- num_clusters:

  An integer, the number of clusters to split the predictor space into,
  or `NULL` (the default) to use `max(2, floor(sqrt(n / 2)))` where `n`
  is the number of observations.

- cluster_balance_threshold:

  A number. A cluster is used for over-sampling a class only if it
  contains at least `cluster_balance_threshold` times as many
  observations of that class as of all other classes combined. Defaults
  to `1`, meaning that the class must be at least as common as the rest
  of the data within the cluster. Smaller values keep more clusters.

- density_exponent:

  A number, the exponent applied to the average pairwise distance when
  measuring how sparse a cluster is, or `NULL` (the default) to use the
  number of predictors.

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

KMeans-SMOTE combines k-means clustering with SMOTE to avoid generating
synthetic points in regions where the minority class is not actually
present. It works in three stages:

1.  The predictor space of the whole data set is clustered with
    [`stats::kmeans()`](https://rdrr.io/r/stats/kmeans.html) into
    `num_clusters` clusters.

2.  Each cluster is either kept or filtered out. A cluster is kept for a
    given minority class if the ratio of that class's observations to
    all other observations in the cluster is at least
    `cluster_balance_threshold`, and if the cluster contains more than
    `neighbors` observations of that class so that interpolation is
    possible.

3.  The synthetic points are distributed over the kept clusters
    according to how sparse each cluster is. The sparsity of a cluster
    is `mean_pairwise_distance ^ density_exponent / n`, where `n` is the
    number of minority observations in the cluster. Sparser clusters
    receive more points. Within each cluster the points are then
    generated by ordinary SMOTE interpolation, using only that cluster's
    observations as neighbors.

Filtering on cluster balance keeps synthetic points away from
majority-dominated regions, and the density weighting counteracts
within-class imbalance rather than only between-class imbalance.

The clustering is done once on all observations, so in a multi-class
problem every minority class shares the same clusters; only the
filtering and weighting are done per class. Note also that k-means
always uses Euclidean distance. The `distance` argument affects the
nearest-neighbor search used for interpolation and the sparsity
calculation, not the clustering itself.

All columns used in this function must be numeric with no missing data.

## References

Douzas, G., Bacao, F., and Last, F. (2018). Improving imbalanced
learning through a heuristic oversampling method based on k-means and
SMOTE. Information Sciences, 465:1-20.

## See also

[`step_kmeans_smote()`](https://themis.tidymodels.org/dev/reference/step_kmeans_smote.md)
for step function of this method

Other Direct Implementations:
[`adasyn()`](https://themis.tidymodels.org/dev/reference/adasyn.md),
[`bsmote()`](https://themis.tidymodels.org/dev/reference/bsmote.md),
[`cluster_centroids()`](https://themis.tidymodels.org/dev/reference/cluster_centroids.md),
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
[`svmsmote()`](https://themis.tidymodels.org/dev/reference/svmsmote.md),
[`tomek()`](https://themis.tidymodels.org/dev/reference/tomek.md)

## Examples

``` r
circle_numeric <- circle_example[, c("x", "y", "class")]

res <- kmeans_smote(circle_numeric, var = "class")

res <- kmeans_smote(circle_numeric, var = "class", num_clusters = 10)

res <- kmeans_smote(circle_numeric, var = "class", over_ratio = 0.8)
```
