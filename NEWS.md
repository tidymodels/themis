# themis (development version)

* `step_rose()` and `rose()` now have improved documentation for `minority_prop`, clarifying that it controls the proportion of synthetic observations from the minority class, and how it differs from `over_ratio` (#144).

* Added standalone `rose()` function as a thin wrapper around `ROSE::ROSE()`, making it consistent with the other algorithms in the package that expose a direct implementation alongside their recipe step (#195).

* `step_nearmiss()` and `step_tomek()` gain a `distance_with` argument to control which variables are used for distance calculations. This allows the steps to be used when non-numeric predictor variables are present in the data (#166).

* `step_adasyn()`, `step_bsmote()`, `step_nearmiss()`, `step_smote()`, and `step_smotenc()` now document the minimum number of observations needed to perform the algorithm (#104).

* All `step_*()` functions now correctly handle 0 and 1 row inputs in `bake()` (#160).

* `adasyn()`, `bsmote()`, `nearmiss()`, `smote()`, and `tomek()` now correctly attribute errors from non-numeric columns to the user-facing function (#181).

* `smotenc()` now only suppresses the specific benign warning from `gower::gower_topn()` about variables with zero range, rather than all warnings (#182).

* `bsmote()` now correctly passes the `all_neighbors` argument to the underlying implementation (#176).

* `step_bsmote()` now works correctly when there is only a single predictor (#151).

* `step_downsample()` and `step_upsample()` now correctly handle `NA` values in the outcome variable instead of erroring (#177).

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
