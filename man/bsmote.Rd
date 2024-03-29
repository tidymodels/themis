% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bsmote_impl.R
\name{bsmote}
\alias{bsmote}
\title{borderline-SMOTE Algorithm}
\usage{
bsmote(df, var, k = 5, over_ratio = 1, all_neighbors = FALSE)
}
\arguments{
\item{df}{data.frame or tibble. Must have 1 factor variable and remaining
numeric variables.}

\item{var}{Character, name of variable containing factor variable.}

\item{k}{An integer. Number of nearest neighbor that are used
to generate the new examples of the minority class.}

\item{over_ratio}{A numeric value for the ratio of the
minority-to-majority frequencies. The default value (1) means
that all other levels are sampled up to have the same
frequency as the most occurring level. A value of 0.5 would mean
that the minority levels will have (at most) (approximately)
half as many rows than the majority level.}

\item{all_neighbors}{Type of two borderline-SMOTE method. Defaults to FALSE.
See details.}
}
\value{
A data.frame or tibble, depending on type of \code{df}.
}
\description{
BSMOTE generates generate new examples of the minority class using nearest
neighbors of these cases in the border region between classes.
}
\details{
This methods works the same way as \code{\link[=smote]{smote()}}, expect that instead of
generating points around every point of of the minority class each point is
first being classified into the boxes "danger" and "not". For each point the
k nearest neighbors is calculated. If all the neighbors comes from a
different class it is labeled noise and put in to the "not" box. If more then
half of the neighbors comes from a different class it is labeled "danger.

If \code{all_neighbors = FALSE} then points will be generated between nearest
neighbors in its own class. If \code{all_neighbors = TRUE} then points will be
generated between any nearest neighbors. See examples for visualization.

The parameter \code{neighbors} controls the way the new examples are created.
For each currently existing minority class example X new examples will be
created (this is controlled by the parameter \code{over_ratio} as mentioned
above). These examples will be generated by using the information from the
\code{neighbors} nearest neighbor of each example of the minority class.
The parameter \code{neighbors} controls how many of these neighbor are used.

All columns used in this step must be numeric with no missing data.
}
\examples{
circle_numeric <- circle_example[, c("x", "y", "class")]

res <- bsmote(circle_numeric, var = "class")

res <- bsmote(circle_numeric, var = "class", k = 10)

res <- bsmote(circle_numeric, var = "class", over_ratio = 0.8)

res <- bsmote(circle_numeric, var = "class", all_neighbors = TRUE)
}
\references{
Hui Han, Wen-Yuan Wang, and Bing-Huan Mao. Borderline-smote:
a new over-sampling method in imbalanced data sets learning. In
International Conference on Intelligent Computing, pages 878–887. Springer,
2005.
}
\seealso{
\code{\link[=step_bsmote]{step_bsmote()}} for step function of this method

Other Direct Implementations: 
\code{\link{adasyn}()},
\code{\link{nearmiss}()},
\code{\link{smote}()},
\code{\link{smotenc}()},
\code{\link{tomek}()}
}
\concept{Direct Implementations}
