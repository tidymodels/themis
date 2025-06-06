% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/smote.R
\name{step_smote}
\alias{step_smote}
\alias{tidy.step_smote}
\title{Apply SMOTE Algorithm}
\usage{
step_smote(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  column = NULL,
  over_ratio = 1,
  neighbors = 5,
  skip = TRUE,
  seed = sample.int(10^5, 1),
  id = rand_id("smote")
)
}
\arguments{
\item{recipe}{A recipe object. The step will be added to the sequence of
operations for this recipe.}

\item{...}{One or more selector functions to choose which
variable is used to sample the data. See \link[recipes:selections]{recipes::selections}
for more details. The selection should result in \emph{single
factor variable}. For the \code{tidy} method, these are not
currently used.}

\item{role}{Not used by this step since no new variables are
created.}

\item{trained}{A logical to indicate if the quantities for preprocessing have
been estimated.}

\item{column}{A character string of the variable name that will
be populated (eventually) by the \code{...} selectors.}

\item{over_ratio}{A numeric value for the ratio of the
minority-to-majority frequencies. The default value (1) means
that all other levels are sampled up to have the same
frequency as the most occurring level. A value of 0.5 would mean
that the minority levels will have (at most) (approximately)
half as many rows than the majority level.}

\item{neighbors}{An integer. Number of nearest neighbor that are used
to generate the new examples of the minority class.}

\item{skip}{A logical. Should the step be skipped when the recipe is baked by
\code{\link[recipes:bake]{bake()}}? While all operations are baked when \code{\link[recipes:prep]{prep()}} is run, some
operations may not be able to be conducted on new data (e.g. processing the
outcome variable(s)). Care should be taken when using \code{skip = TRUE} as it
may affect the computations for subsequent operations.}

\item{seed}{An integer that will be used as the seed when
smote-ing.}

\item{id}{A character string that is unique to this step to identify it.}
}
\value{
An updated version of \code{recipe} with the new step
added to the sequence of existing steps (if any). For the
\code{tidy} method, a tibble with columns \code{terms} which is
the variable used to sample.
}
\description{
\code{step_smote()} creates a \emph{specification} of a recipe step that generate new
examples of the  minority class using nearest neighbors of these cases.
}
\details{
The parameter \code{neighbors} controls the way the new examples are created.
For each currently existing minority class example X new examples will be
created (this is controlled by the parameter \code{over_ratio} as mentioned
above). These examples will be generated by using the information from the
\code{neighbors} nearest neighbor of each example of the minority class.
The parameter \code{neighbors} controls how many of these neighbor are used.

All columns in the data are sampled and returned by \code{\link[recipes:juice]{recipes::juice()}}
and \code{\link[recipes:bake]{recipes::bake()}}.

All columns used in this step must be numeric with no missing data.

When used in modeling, users should strongly consider using the
option \code{skip = TRUE} so that the extra sampling is \emph{not}
conducted outside of the training set.
}
\section{Tidying}{
When you \code{\link[recipes:tidy.recipe]{tidy()}} this step, a tibble is retruned with
columns \code{terms} and \code{id}:

\describe{
\item{terms}{character, the selectors or variables selected}
\item{id}{character, id of this step}
}
}

\section{Tuning Parameters}{
This step has 2 tuning parameters:
\itemize{
\item \code{over_ratio}: Over-Sampling Ratio (type: double, default: 1)
\item \code{neighbors}: # Nearest Neighbors (type: integer, default: 5)
}
}

\section{Case weights}{


The underlying operation does not allow for case weights.
}

\examples{
\dontshow{if (rlang::is_installed("modeldata")) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
library(recipes)
library(modeldata)
data(hpc_data)

hpc_data0 <- hpc_data |>
  select(-protocol, -day)

orig <- count(hpc_data0, class, name = "orig")
orig

up_rec <- recipe(class ~ ., data = hpc_data0) |>
  # Bring the minority levels up to about 1000 each
  # 1000/2211 is approx 0.4523
  step_smote(class, over_ratio = 0.4523) |>
  prep()

training <- up_rec |>
  bake(new_data = NULL) |>
  count(class, name = "training")
training

# Since `skip` defaults to TRUE, baking the step has no effect
baked <- up_rec |>
  bake(new_data = hpc_data0) |>
  count(class, name = "baked")
baked

# Note that if the original data contained more rows than the
# target n (= ratio * majority_n), the data are left alone:
orig |>
  left_join(training, by = "class") |>
  left_join(baked, by = "class")

library(ggplot2)

ggplot(circle_example, aes(x, y, color = class)) +
  geom_point() +
  labs(title = "Without SMOTE")

recipe(class ~ x + y, data = circle_example) |>
  step_smote(class) |>
  prep() |>
  bake(new_data = NULL) |>
  ggplot(aes(x, y, color = class)) +
  geom_point() +
  labs(title = "With SMOTE")
\dontshow{\}) # examplesIf}
}
\references{
Chawla, N. V., Bowyer, K. W., Hall, L. O., and Kegelmeyer,
W. P. (2002). Smote: Synthetic minority over-sampling technique.
Journal of Artificial Intelligence Research, 16:321-357.
}
\seealso{
\code{\link[=smote]{smote()}} for direct implementation

Other Steps for over-sampling: 
\code{\link{step_adasyn}()},
\code{\link{step_bsmote}()},
\code{\link{step_rose}()},
\code{\link{step_smotenc}()},
\code{\link{step_upsample}()}
}
\concept{Steps for over-sampling}
