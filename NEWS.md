# themis (development version)

## New steps

* `step_cluster_centroids()` (and its direct-implementation counterpart `cluster_centroids()`) under-samples the majority classes by running k-means within each class and replacing the class with one representative per cluster, either the centroid itself (`voting = "soft"`) or the observation closest to it (`voting = "hard"`) (#318).

* `step_cnn()` (and its direct-implementation counterpart `cnn()`) under-samples the majority classes using Condensed Nearest Neighbors, keeping only a consistent subset of observations that correctly classifies the data using a 1-nearest-neighbor rule (#113).

* `step_enn()` (and its direct-implementation counterpart `enn()`) cleans the data using the Edited Nearest Neighbors rule, removing observations whose class differs from the majority of their nearest neighbors. A `times` argument applies the cleaning repeatedly, stopping early on convergence, which corresponds to Repeated Edited Nearest Neighbors (RENN), and an `all_k` argument applies it with an increasing number of neighbors, from 1 up to `neighbors`, which corresponds to All k-Nearest Neighbors (AllKNN) (#115, #173, #174).

* `step_instance_hardness()` (and its direct-implementation counterpart `instance_hardness()`) under-samples the majority classes by removing the observations that are hardest to classify, estimated using the k-Disagreeing Neighbors measure (#172).

* `step_kmeans_smote()` (and its direct-implementation counterpart `kmeans_smote()`) over-samples the minority classes with KMeans-SMOTE, which clusters the predictor space, generates new examples only inside the clusters where the minority class is dominant, and gives sparser clusters more of the new examples (#317).

* `step_ncl()` (and its direct-implementation counterpart `ncl()`) cleans the data using the Neighborhood Cleaning Rule, removing majority class observations that are noisy or that pollute the neighborhood of minority class observations (#116).

* `step_oss()` (and its direct-implementation counterpart `oss()`) under-samples the majority classes using One-Sided Selection, combining Condensed Nearest Neighbors to reduce redundant majority class observations with Tomek's links to remove majority class observations on the decision boundary (#114).

* `step_smogn()` (and its direct-implementation counterpart `smogn()`) over-samples rare regions of a numeric outcome for imbalanced regression using a combination of SMOTE-style interpolation and Gaussian noise, while under-sampling common regions (#49).

* `step_smoten()` (and its direct-implementation counterpart `smoten()`) over-samples the minority classes for data sets where all predictors are categorical, using the Value Difference Metric to find nearest neighbors and majority voting to generate new examples (#54).

* `step_svmsmote()` (and its direct-implementation counterpart `svmsmote()`) over-samples the minority classes near the decision boundary by fitting a support vector machine and generating new examples around the minority class support vectors, interpolating toward minority neighbors where majority neighbors dominate the boundary and extrapolating outward where the support vector sits in a dense minority region (#170).

## Improvements

* New "Common pitfalls" article on resampling only the training set (`skip = TRUE`) and avoiding cross-validation leakage (#320).

* New "Methods overview" article organizing the sampling steps into a taxonomy and documenting the SMOTE + ENN / SMOTE + Tomek composition (#319, #321).

* New article explaining how `over_ratio` and `under_ratio` work (#141).

* `over_ratio` and `under_ratio` now accept a named numeric vector in addition to a single number, giving each outcome level its own sampling target. The names are levels of the outcome and the values are ratios of the reference class, exactly as in the single-number case, so `over_ratio = c(a = 1, b = 0.5)` brings `"a"` up to the size of the majority level and `"b"` up to half of it. Levels that are not named are left untouched, as are rows with a missing outcome, and supplying a vector means the argument can no longer be tuned. `step_rose()` is the exception and still requires a single number, since its `over_ratio` scales the size of the total generated sample rather than setting a target per class (#323).

* All upsampling steps gain an `indicator_column` argument. When set, a logical column is added to the baked data marking rows added by the step (`TRUE`) vs rows from the original data (`FALSE`). For `step_rose()`, all rows are `TRUE` since ROSE generates a fully synthetic dataset (#58).

* The `distance` argument of every step that performs nearest neighbor calculations gains four probability-divergence metrics: `"squared_chord"`, `"matusita"`, `"hellinger"`, and `"bhattacharyya"`. These treat each row as a distribution over the predictors and so require non-negative values, with `"hellinger"` and `"bhattacharyya"` further requiring each row to sum to 1. All four run on the fast approximate nearest neighbor path and scale to large data (#234).

* The `distance` argument of every step that performs nearest neighbor calculations gains nine further probability-divergence metrics, provided by the philentropy package: `"canberra"`, `"soergel"`, `"lorentzian"`, `"jeffreys"`, `"topsoe"`, `"jensen-shannon"`, `"jensen_difference"`, `"taneja"`, and `"kumar-johnson"`. philentropy is an optional dependency, and since these metrics compute an exact all-pairs distance matrix they are best suited to smaller datasets (#234).

* `distance = "mahalanobis"` now fails with an informative error when the predictors have a singular covariance matrix, instead of a low-level message from `chol()` or silently returning distances computed from a numerically unusable inverse. This covers collinear and constant predictors as well as duplicated rows (#246).

* `adasyn()`, `bsmote()`, `nearmiss()`, `smote()`, and `smotenc()` now reject a negative `over_ratio` or `under_ratio`, matching the validation their recipe steps already performed (#323).

* `rose()` is a new thin wrapper around `ROSE::ROSE()`, making it consistent with the other algorithms in the package that expose a direct implementation alongside their recipe step (#195).

* `step_adasyn()`, `step_bsmote()`, `step_nearmiss()`, `step_smote()`, and `step_smotenc()` now document the minimum number of observations needed to perform the algorithm (#104).

* `step_adasyn()`, `step_bsmote()`, `step_nearmiss()`, `step_smote()`, and `step_tomek()` (and their direct-implementation counterparts `adasyn()`, `bsmote()`, `nearmiss()`, `smote()`, and `tomek()`) gain a `distance` argument to control which distance metric is used for nearest neighbor calculations. Supported metrics are `"euclidean"` (default), `"cosine"`, `"mahalanobis"`, `"manhattan"`, and `"chebyshev"` (#171).

* `step_adasyn()` (and its direct-implementation counterpart `adasyn()`) is now faster on large data, computing the full-data nearest neighbors only for the minority observations it needs. Results are unchanged (#257).

* `step_bsmote()` now sets the tuning range of its `neighbors` parameter to `c(1, 10)`, matching the other steps that tune `neighbors` (#254).

* `step_downsample()` gains a `replacement` argument. When set to `TRUE` the under-sample is drawn with replacement, giving a bootstrapped under-sample where the same row can be selected more than once. The default `FALSE` keeps the current behavior (#325).

* `step_nearmiss()` (and its direct-implementation counterpart `nearmiss()`) gains a `version` argument to select between the NearMiss-1, NearMiss-2, and NearMiss-3 variants of Mani & Zhang (2003), together with `n_neighbors_ver3` to control the size of the NearMiss-3 candidate pool. The default `version = 1` preserves the previous behavior (#279).

* `step_nearmiss()` and `step_tomek()` gain a `distance_with` argument to control which variables are used for distance calculations. This allows the steps to be used when non-numeric predictor variables are present in the data (#166).

* `step_rose()` now validates predictor types during `prep()`, giving a clear error for unsupported types consistent with the other sampling steps instead of relying on `ROSE::ROSE()` to fail downstream (#265).

* `step_rose()` now documents `minority_prop` better, clarifying that it controls the proportion of synthetic observations from the minority class, and how it differs from `over_ratio` (#144).

* `step_rose()` now validates that `minority_prop` is at most 1, since it is a proportion (#269).

* `step_smotenc()` now validates that all predictors are numeric or nominal, erroring on unsupported column types such as dates instead of failing later (#254).

## Bug fixes

* All sampling steps now handle an unused (zero-count) factor level in the outcome gracefully, dropping it with a warning before computing sampling targets instead of deleting all rows or erroring (#238).

* All `step_*()` functions now correctly handle 0 and 1 row inputs in `bake()` (#160).

* Over-sampling steps (`step_adasyn()`, `step_bsmote()`, `step_rose()`, `step_smote()`, and `step_smotenc()`) now error when supplied a case weights column instead of silently filling synthetic rows' weights with `NA`. These steps have never supported case weights (#243).

* Nearest-neighbor computations in `step_adasyn()`, `step_smote()`, `step_smotenc()`, and `step_tomek()` (and their direct-implementation counterparts) now exclude each observation from its own neighbor list by row index rather than assuming it is always the first neighbor returned. Exact-duplicate coordinates could previously leave a point as its own neighbor or make the farthest candidate unreachable (#247).

* `adasyn()`, `bsmote()`, `nearmiss()`, `smote()`, and `tomek()` now correctly attribute errors from non-numeric columns to the user-facing function (#181).

* `adasyn()`, `bsmote()`, `smote()`, and `smotenc()` now return a proper factor outcome when called with a character `var`, instead of an all-`NA`, zero-level factor (#261).

* `bsmote()` now correctly passes the `all_neighbors` argument to the underlying implementation (#176).

* `smotenc()` now only suppresses the specific benign warning from `gower::gower_topn()` about variables with zero range, rather than all warnings (#182).

* `step_adasyn()` (and its direct-implementation counterpart `adasyn()`) now weights minority observations by their exact majority-neighbor count. An off-by-one subtraction previously undercounted majority neighbors, zeroing out the weight of border points with a single majority neighbor and biasing sampling away from the class boundary (#239).

* `step_adasyn()` (and its direct-implementation counterpart `adasyn()`) no longer errors with a cryptic message when a minority class is well separated from the majority classes; it now falls back to uniform sampling and checks the minority class size before sampling (#240).

* `step_adasyn()`, `step_bsmote()`, `step_smote()`, and `step_smotenc()` (and their direct-implementation counterparts) now round the fractional oversampling target instead of truncating it, so a fractional `over_ratio` lands on the nearest integer count (#248).

* `step_bsmote()` (and its direct-implementation counterpart `bsmote()`) now selects the correct "danger" observations on the class border. The danger criterion had inverted the roles of minority and majority neighbors, causing it to oversample safe interior points instead of borderline ones (#235).

* `step_bsmote()` (and its direct-implementation counterpart `bsmote()`) with `all_neighbors = TRUE` now seeds synthetic points only from minority-class danger observations and takes a reduced step toward majority-class neighbors, matching borderline-SMOTE2. Previously it could seed from border-adjacent majority rows, generating minority-labeled points around majority centers (#242).

* `step_bsmote()` now works correctly when there is only a single predictor (#151).

* `step_downsample()` and `step_upsample()` now correctly handle `NA` values in the outcome variable instead of erroring (#177).

* `step_nearmiss()` (and its direct-implementation counterpart `nearmiss()`) now keeps the majority observations that are genuinely closest to the minority class, rather than selecting rows by their position in the data (#236).

* `step_nearmiss()` (and its direct-implementation counterpart `nearmiss()`) now returns true cosine-distance magnitudes with `distance = "cosine"`. Previously the cosine branch L2-normalized and took Euclidean distances, returning `sqrt(2 - 2 * cos_sim)` instead of `1 - cos_sim`. Neighbor ordering was unaffected, but the neighbor-distance averages used the wrong magnitudes (#244).

* `step_smotenc()` (and its direct-implementation counterpart `smotenc()`) now sets each synthetic sample's nominal features to the majority vote across the seed's k nearest neighbors, matching the SMOTENC algorithm, rather than voting over the randomly chosen interpolation partners (#241).

* `step_tomek()` (and its direct-implementation counterpart `tomek()`) now removes only the majority-class member of each Tomek link, retaining the minority-class member, matching the documented behavior. Previously it removed both members of the pair (#262).

* `step_upsample()` now names itself, rather than `step_downsample()`, in the deprecation message shown when the defunct `ratio` argument is supplied (#252).

* `step_upsample()` now leaves classes that already meet or exceed the target size untouched instead of resampling them with replacement, and produces the same rows whether or not `indicator_column` is set (#263).

# themis 1.0.3

## Improvements

* Calling `?tidy.step_*()` now sends you to the documentation for `step_*()` where the outcome is documented. (#142)

* Documentation now correctly specifies majority-to-minority and minority-to-majority. (#143, #110)

* Documentation for tidy methods for all steps has been improved to describe the return value more accurately. (#148)

* All messages, warnings and errors has been translated to use {cli} package (#153, #155).

# themis 1.0.2

## Improvements

* Many internal changes to improve consistency and slight speed increases.

# themis 1.0.1

## Improvements

* Fixed bug where some upsampling functions would error if no upsampling was needed. (#119)

* Steps with tunable arguments now have those arguments listed in the documentation.

# themis 1.0.0

* Added case weights support for `step_upsample()` and `step_downsample()`

# themis 0.2.2

* `tomek()` has been added, rewritten to apply to multiple classes, removing the need for the unbalanced package, which has been removed as a dependency. 

# themis 0.2.1

* A bug was fixed in `step_downsample()` and `step_upsample()` that made the steps unable to be tuned. (#90)

# themis 0.2.0

## New steps

* `step_smotenc()` have been added to implement SMOTENC which can handle categorical as well as numerical values. Thanks to @RobertGregg (#82)

## Improvements and Other Changes

* export `nearmiss()` functions to users.
* Update examples to no longer use `iris` or `okc` data sets.
* All recipe steps now officially support empty selections to be more aligned with dplyr and other packages that use tidyselect (#55)

## Bug fixes

* `step_rose()` now correctly allows you to use characters variables. (#26)
* `step_tomek()` now ignore non-predictor variables when appropriate. (#51)
* Fix bug where wrong ordering of columns caused error in `smote()`. (#76)

# themis 0.1.4

* export `smote()`, `adasyn()`, and `bsmote()` functions to users.

# themis 0.1.3

* Steps that use nearest neighbors gives cleaner errors.

# themis 0.1.2

* tuneable steps now properly work with tune package.
* Steps now Retain original factor level ordering. (#22)
* Oversampling steps now ignore non-predictor variables when appropriate. (#20)

# themis 0.1.1

* `step_smote()` now work regardless of order of classes. Thanks to @sebastien-foulle for point it out #14.

# themis 0.1.0

* Added a `NEWS.md` file to track changes to the package.
