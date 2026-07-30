#' Apply KMeans-SMOTE Algorithm
#'
#' `step_kmeans_smote()` creates a *specification* of a recipe step that
#' generates new examples of the minority class, restricting them to the regions
#' of the predictor space where that class is dominant.
#'
#' @inheritParams recipes::step_center
#' @inheritParams step_smote
#' @param ... One or more selector functions to choose which
#'  variable is used to sample the data. See [recipes::selections]
#'  for more details. The selection should result in _single
#'  factor variable_. For the `tidy` method, these are not
#'  currently used.
#' @param neighbors An integer. Number of nearest neighbor that are used
#'  to generate the new examples of the minority class. Only observations
#'  within the same cluster are considered as neighbors.
#' @param num_clusters An integer, the number of clusters to split the predictor
#'  space into, or `NULL` (the default) to use `max(2, floor(sqrt(n / 2)))`
#'  where `n` is the number of observations.
#' @param cluster_balance_threshold A number. A cluster is used for
#'  over-sampling a class only if it contains at least
#'  `cluster_balance_threshold` times as many observations of that class as of
#'  all other classes combined. Defaults to `1`, meaning that the class must be
#'  at least as common as the rest of the data within the cluster. Smaller
#'  values keep more clusters.
#' @param density_exponent A number, the exponent applied to the average
#'  pairwise distance when measuring how sparse a cluster is, or `NULL` (the
#'  default) to use the number of predictors.
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` which is
#'  the variable used to sample.
#'
#' @template details-kmeans_smote
#'
#' @details
#' All columns in the data are sampled and returned by [recipes::juice()]
#'  and [recipes::bake()].
#'
#' All columns used in this step must be numeric with no missing data.
#'
#' When used in modeling, users should strongly consider using the
#'  option `skip = TRUE` so that the extra sampling is _not_
#'  conducted outside of the training set.
#'
#' # Minimum observations
#'
#' Each minority class must have more than `neighbors` observations in at least
#' one kept cluster. If no cluster qualifies, an error is thrown suggesting
#' which arguments to loosen.
#'
#' # Tidying
#'
#' When you [`tidy()`][recipes::tidy.recipe()] this step, a tibble is returned with
#'  columns `terms` and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{id}{character, id of this step}
#' }
#'
#' ```{r, echo = FALSE, results="asis"}
#' step <- "step_kmeans_smote"
#' result <- knitr::knit_child("man/rmd/tunable-args.Rmd")
#' cat(result)
#' ```
#'
#' @template case-weights-not-supported
#'
#' @references Douzas, G., Bacao, F., and Last, F. (2018). Improving imbalanced
#'  learning through a heuristic oversampling method based on k-means and SMOTE.
#'  Information Sciences, 465:1-20.
#'
#' @seealso [kmeans_smote()] for direct implementation
#'
#'  [step_smote()] for the same interpolation without the clustering step
#' @family Steps for over-sampling
#'
#' @export
#' @examplesIf rlang::is_installed("modeldata")
#' library(recipes)
#' library(modeldata)
#' data(hpc_data)
#'
#' hpc_data0 <- hpc_data |>
#'   select(-protocol, -day)
#'
#' orig <- count(hpc_data0, class, name = "orig")
#' orig
#'
#' up_rec <- recipe(class ~ ., data = hpc_data0) |>
#'   # Bring the minority levels up to about 1000 each
#'   # 1000/2211 is approx 0.4523
#'   step_kmeans_smote(class, over_ratio = 0.4523) |>
#'   prep()
#'
#' training <- up_rec |>
#'   bake(new_data = NULL) |>
#'   count(class, name = "training")
#' training
#'
#' # Since `skip` defaults to TRUE, baking the step has no effect
#' baked <- up_rec |>
#'   bake(new_data = hpc_data0) |>
#'   count(class, name = "baked")
#' baked
#'
#' library(ggplot2)
#'
#' ggplot(circle_example, aes(x, y, color = class)) +
#'   geom_point() +
#'   labs(title = "Without KMeans-SMOTE")
#'
#' recipe(class ~ x + y, data = circle_example) |>
#'   step_kmeans_smote(class) |>
#'   prep() |>
#'   bake(new_data = NULL) |>
#'   ggplot(aes(x, y, color = class)) +
#'   geom_point() +
#'   labs(title = "With KMeans-SMOTE")
step_kmeans_smote <-
  function(
    recipe,
    ...,
    role = NA,
    trained = FALSE,
    column = NULL,
    over_ratio = 1,
    neighbors = 2,
    num_clusters = NULL,
    cluster_balance_threshold = 1,
    density_exponent = NULL,
    distance = "euclidean",
    indicator_column = NULL,
    skip = TRUE,
    seed = sample.int(10^5, 1),
    id = rand_id("kmeans_smote")
  ) {
    check_number_whole(seed)
    check_string(indicator_column, allow_null = TRUE, allow_empty = FALSE)
    check_distance_arg(distance)

    add_step(
      recipe,
      step_kmeans_smote_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        column = column,
        over_ratio = over_ratio,
        neighbors = neighbors,
        num_clusters = num_clusters,
        cluster_balance_threshold = cluster_balance_threshold,
        density_exponent = density_exponent,
        distance = distance,
        predictors = NULL,
        indicator_column = indicator_column,
        skip = skip,
        seed = seed,
        id = id
      )
    )
  }

step_kmeans_smote_new <-
  function(
    terms,
    role,
    trained,
    column,
    over_ratio,
    neighbors,
    num_clusters,
    cluster_balance_threshold,
    density_exponent,
    distance,
    predictors,
    indicator_column,
    skip,
    seed,
    id
  ) {
    step(
      subclass = "kmeans_smote",
      terms = terms,
      role = role,
      trained = trained,
      column = column,
      over_ratio = over_ratio,
      neighbors = neighbors,
      num_clusters = num_clusters,
      cluster_balance_threshold = cluster_balance_threshold,
      density_exponent = density_exponent,
      distance = distance,
      predictors = predictors,
      indicator_column = indicator_column,
      skip = skip,
      seed = seed,
      id = id
    )
  }

#' @export
prep.step_kmeans_smote <- function(x, training, info = NULL, ...) {
  col_name <- recipes_eval_select(x$terms, training, info)

  check_ratio(x$over_ratio, arg = "over_ratio")
  check_number_whole(x$neighbors, arg = "neighbors", min = 1)
  check_number_whole(
    x$num_clusters,
    arg = "num_clusters",
    min = 2,
    allow_null = TRUE
  )
  check_number_decimal(
    x$cluster_balance_threshold,
    arg = "cluster_balance_threshold",
    min = 0,
    allow_infinite = FALSE
  )
  check_number_decimal(
    x$density_exponent,
    arg = "density_exponent",
    min = 0,
    allow_null = TRUE
  )

  check_1_selected(col_name)
  check_column_factor(training, col_name)
  warn_unused_levels(training, col_name)
  check_ratio_column(x$over_ratio, training, col_name, arg = "over_ratio")

  recipes::check_name(
    tibble(x = logical(0)),
    training,
    x,
    newname = x$indicator_column
  )

  predictors <- setdiff(recipes::recipes_names_predictors(info), col_name)

  check_type(training[, predictors], types = c("double", "integer"))
  check_na(select(training, all_of(c(col_name, predictors))))

  step_kmeans_smote_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    column = col_name,
    over_ratio = x$over_ratio,
    neighbors = x$neighbors,
    num_clusters = x$num_clusters,
    cluster_balance_threshold = x$cluster_balance_threshold,
    density_exponent = x$density_exponent,
    distance = x$distance,
    predictors = predictors,
    indicator_column = x$indicator_column,
    skip = x$skip,
    seed = x$seed,
    id = x$id
  )
}

#' @export
bake.step_kmeans_smote <- function(object, new_data, ...) {
  col_names <- unique(c(object$predictors, object$column))
  check_new_data(col_names, object, new_data)

  if (length(object$column) == 0L) {
    # Empty selection
    return(new_data)
  }

  if (nrow(new_data) <= 1) {
    return(new_data)
  }

  n_orig <- nrow(new_data)
  new_data <- as.data.frame(new_data)

  predictor_data <- new_data[, col_names]

  # kmeans_smote with seed for reproducibility
  with_seed(
    seed = object$seed,
    code = {
      synthetic_data <- kmeans_smote_impl(
        predictor_data,
        object$column,
        k = object$neighbors,
        over_ratio = object$over_ratio,
        num_clusters = object$num_clusters,
        cluster_balance_threshold = object$cluster_balance_threshold,
        density_exponent = object$density_exponent,
        distance = object$distance
      )
      synthetic_data <- as_tibble(synthetic_data)
    }
  )
  new_data <- na_splice(new_data, synthetic_data, object)

  new_data <- add_indicator_column(new_data, n_orig, object$indicator_column)

  new_data
}

#' @export
print.step_kmeans_smote <-
  function(x, width = max(20, options()$width - 26), ...) {
    title <- "KMeans-SMOTE based on "
    print_step(x$column, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname step_kmeans_smote
#' @usage NULL
#' @export
tidy.step_kmeans_smote <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = unname(x$column))
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = unname(term_names))
  }
  res$id <- x$id
  res
}

#' @export
#' @rdname tunable_themis
tunable.step_kmeans_smote <- function(x, ...) {
  tibble::tibble(
    name = c("over_ratio", "neighbors", "num_clusters"),
    call_info = list(
      list(pkg = "dials", fun = "over_ratio"),
      list(pkg = "dials", fun = "neighbors", range = c(1, 10)),
      list(pkg = "dials", fun = "num_clusters", range = c(2, 10))
    ),
    source = "recipe",
    component = "step_kmeans_smote",
    component_id = x$id
  ) |>
    drop_per_class_ratio(x$over_ratio)
}

#' @rdname required_pkgs.step
#' @export
required_pkgs.step_kmeans_smote <- function(x, ...) {
  c("themis")
}
