# themis (development version)

* Over-sampling steps (`step_smote()`, `step_adasyn()`, `step_bsmote()`, `step_svmsmote()`, `step_smoten()`, `step_smotenc()`, `step_rose()`, and `step_smogn()`) now error when supplied a case weights column instead of silently filling synthetic rows' weights with `NA`. These steps have never supported case weights (#243).

* All sampling steps now handle an unused (zero-count) factor level in the outcome gracefully, dropping it with a warning before computing sampling targets instead of deleting all rows or erroring (#238).

* `step_smote()`, `step_adasyn()`, `step_bsmote()`, `step_svmsmote()`, `step_smoten()`, and `step_smotenc()` (and their direct-implementation counterparts) now round the fractional oversampling target instead of truncating it, so a fractional `over_ratio` lands on the nearest integer count (#248).

* Nearest-neighbor computations in `step_adasyn()`, `step_enn()`, `step_ncl()`, `step_smote()`, `step_smotenc()`, and `step_tomek()` (and their direct-implementation counterparts) now exclude each observation from its own neighbor list by row index rather than assuming it is always the first neighbor returned. Exact-duplicate coordinates could previously leave a point as its own neighbor or make the farthest candidate unreachable (#247).

* `step_adasyn()` (and its direct-implementation counterpart `adasyn()`) now weights minority observations by their exact majority-neighbor count. An off-by-one subtraction previously undercounted majority neighbors, zeroing out the weight of border points with a single majority neighbor and biasing sampling away from the class boundary (#239).

* `step_adasyn()` (and its direct-implementation counterpart `adasyn()`) no longer errors with a cryptic message when a minority class is well separated from the majority classes; it now falls back to uniform sampling and checks the minority class size before sampling (#240).

* `step_bsmote()` (and its direct-implementation counterpart `bsmote()`) now selects the correct "danger" observations on the class border. The danger criterion had inverted the roles of minority and majority neighbors, causing it to oversample safe interior points instead of borderline ones (#235).

* `step_bsmote()` (and its direct-implementation counterpart `bsmote()`) with `all_neighbors = TRUE` now seeds synthetic points only from minority-class danger observations and takes a reduced step toward majority-class neighbors, matching borderline-SMOTE2. Previously it could seed from border-adjacent majority rows, generating minority-labeled points around majority centers (#242).

* `step_cnn()` (and its direct-implementation counterpart `cnn()`) was added. It under-samples the majority classes using Condensed Nearest Neighbors, keeping only a consistent subset of observations that correctly classifies the data using a 1-nearest-neighbor rule (#113).

* `step_cnn()`, `step_oss()`, and `step_smogn()` (and their direct-implementation counterparts `cnn()`, `oss()`, and `smogn()`) now sample correctly when only one candidate remains. A length-1 vector passed to `sample()` was interpreted as a count and sampled from `1:n`, which could select the wrong observations (#245).

* `step_enn()` (and its direct-implementation counterpart `enn()`) was added. It cleans the data using the Edited Nearest Neighbors rule, removing observations whose class differs from the majority of their nearest neighbors (#115).

* `step_enn()` (and its direct-implementation counterpart `enn()`) gain a `times` argument to apply the cleaning repeatedly, stopping early on convergence, which corresponds to Repeated Edited Nearest Neighbors (RENN) (#173).

* `step_enn()` (and its direct-implementation counterpart `enn()`) gain an `all_k` argument to apply the cleaning with an increasing number of neighbors, from 1 up to `neighbors`, which corresponds to All k-Nearest Neighbors (AllKNN) (#174).

* `step_instance_hardness()` (and its direct-implementation counterpart `instance_hardness()`) was added. It under-samples the majority classes by removing the observations that are hardest to classify, estimated using the k-Disagreeing Neighbors measure (#172).

* `step_ncl()` (and its direct-implementation counterpart `ncl()`) was added. It cleans the data using the Neighborhood Cleaning Rule, removing majority class observations that are noisy or that pollute the neighborhood of minority class observations (#116).

* `step_ncl()` (and its direct-implementation counterpart `ncl()`) now treats a majority class whose size is exactly `threshold_clean` times the minority size as eligible for cleaning, using the `>=` comparison from Laurikkala (2001) instead of a strict `>` (#250).

* `step_nearmiss()` (and its direct-implementation counterpart `nearmiss()`) now keeps the majority observations that are genuinely closest to the minority class, rather than selecting rows by their position in the data (#236).

* `step_nearmiss()` and `step_smogn()` (and their direct-implementation counterparts `nearmiss()` and `smogn()`) now return true cosine-distance magnitudes with `distance = "cosine"`. Previously the cosine branch L2-normalized and took Euclidean distances, returning `sqrt(2 - 2 * cos_sim)` instead of `1 - cos_sim`. Neighbor ordering was unaffected, but NearMiss neighbor-distance averages and SMOGN's interpolate-vs-noise threshold used the wrong magnitudes (#244).

* `step_oss()` (and its direct-implementation counterpart `oss()`) was added. It under-samples the majority classes using One-Sided Selection, combining Condensed Nearest Neighbors to reduce redundant majority class observations with Tomek's links to remove majority class observations on the decision boundary (#114).

* `step_oss()` (and its direct-implementation counterpart `oss()`) now condenses the majority classes with a single pass, matching Kubat & Matwin (1997), instead of iterating Condensed Nearest Neighbors to convergence, which removed more observations than the reference (#250).

* `step_smogn()` (and its direct-implementation counterpart `smogn()`) was added. It over-samples rare regions of a numeric outcome for imbalanced regression using a combination of SMOTE-style interpolation and Gaussian noise, while under-sampling common regions (#49).

* `step_smogn()` (and its direct-implementation counterpart `smogn()`) now emits a clear early error when automatic relevance is requested for a degenerate outcome (zero interquartile range or heavily tied values), pointing users to the `relevance` argument instead of aborting deep inside the boxplot-based computation (#264).

* `step_smogn()` (and its direct-implementation counterpart `smogn()`) now scales the Gaussian perturbation of unsafe cases by each feature's standard deviation and caps the perturbation amount at the safe distance, using `sd * min(perturbation, maxD)` as in the reference, instead of scaling by a distance-capped standard deviation (#250).

* `step_smoten()` (and its direct-implementation counterpart `smoten()`) was added. It over-samples the minority classes for data sets where all predictors are categorical, using the Value Difference Metric to find nearest neighbors and majority voting to generate new examples (#54).

* `step_smotenc()` (and its direct-implementation counterpart `smotenc()`) now sets each synthetic sample's nominal features to the majority vote across the seed's k nearest neighbors, matching the SMOTENC algorithm, rather than voting over the randomly chosen interpolation partners (#241).

* `step_tomek()` (and its direct-implementation counterpart `tomek()`) now removes only the majority-class member of each Tomek link, retaining the minority-class member, matching the documented behavior. Previously it removed both members of the pair (#262).

* `step_svmsmote()` (and its direct-implementation counterpart `svmsmote()`) was added. It over-samples the minority classes near the decision boundary by fitting a support vector machine and generating new examples around the minority class support vectors, interpolating in dense regions and extrapolating in sparse ones (#170).

* `step_upsample()` now names itself, rather than `step_downsample()`, in the deprecation message shown when the defunct `ratio` argument is supplied (#252).

* `step_adasyn()`, `step_bsmote()`, `step_nearmiss()`, `step_smote()`, and `step_tomek()` (and their direct-implementation counterparts `adasyn()`, `bsmote()`, `nearmiss()`, `smote()`, and `tomek()`) gain a `distance` argument to control which distance metric is used for nearest neighbor calculations. Supported metrics are `"euclidean"` (default), `"cosine"`, `"mahalanobis"`, `"manhattan"`, and `"chebyshev"` (#171).

* Added a new article explaining how `over_ratio` and `under_ratio` work (#141).

* All upsampling steps gain an `indicator_column` argument. When set, a logical column is added to the baked data marking rows added by the step (`TRUE`) vs rows from the original data (`FALSE`). For `step_rose()`, all rows are `TRUE` since ROSE generates a fully synthetic dataset (#58).

* `step_rose()` now validates predictor types during `prep()`, giving a clear error for unsupported types consistent with the other sampling steps instead of relying on `ROSE::ROSE()` to fail downstream (#265).

* `step_rose()` and `rose()` now have improved documentation for `minority_prop`, clarifying that it controls the proportion of synthetic observations from the minority class, and how it differs from `over_ratio` (#144).

* `step_rose()` (and its direct-implementation counterpart `rose()`) now validate that `minority_prop` is at most 1, since it is a proportion (#269).

* Added standalone `rose()` function as a thin wrapper around `ROSE::ROSE()`, making it consistent with the other algorithms in the package that expose a direct implementation alongside their recipe step (#195).

* `step_nearmiss()` and `step_tomek()` gain a `distance_with` argument to control which variables are used for distance calculations. This allows the steps to be used when non-numeric predictor variables are present in the data (#166).

* `step_adasyn()`, `step_bsmote()`, `step_nearmiss()`, `step_smote()`, and `step_smotenc()` now document the minimum number of observations needed to perform the algorithm (#104).

* All `step_*()` functions now correctly handle 0 and 1 row inputs in `bake()` (#160).

* `adasyn()`, `bsmote()`, `smote()`, `smoten()`, `smotenc()`, and `svmsmote()` now return a proper factor outcome when called with a character `var`, instead of an all-`NA`, zero-level factor (#261).

* `adasyn()`, `bsmote()`, `nearmiss()`, `smote()`, and `tomek()` now correctly attribute errors from non-numeric columns to the user-facing function (#181).

* `smotenc()` now only suppresses the specific benign warning from `gower::gower_topn()` about variables with zero range, rather than all warnings (#182).

* `bsmote()` now correctly passes the `all_neighbors` argument to the underlying implementation (#176).

* `step_bsmote()` now works correctly when there is only a single predictor (#151).

* `step_downsample()` and `step_upsample()` now correctly handle `NA` values in the outcome variable instead of erroring (#177).

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
