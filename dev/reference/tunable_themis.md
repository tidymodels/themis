# tunable methods for themis

These functions define what parameters *can* be tuned for specific
steps. They also define the recommended objects from the `dials` package
that can be used to generate new parameter values and other
characteristics.

## Usage

``` r
# S3 method for class 'step_adasyn'
tunable(x, ...)

# S3 method for class 'step_bsmote'
tunable(x, ...)

# S3 method for class 'step_downsample'
tunable(x, ...)

# S3 method for class 'step_nearmiss'
tunable(x, ...)

# S3 method for class 'step_rose'
tunable(x, ...)

# S3 method for class 'step_smote'
tunable(x, ...)

# S3 method for class 'step_smotenc'
tunable(x, ...)

# S3 method for class 'step_upsample'
tunable(x, ...)
```

## Arguments

- x:

  A recipe step object

- ...:

  Not used.

## Value

A tibble object.
