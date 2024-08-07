% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/correctly_predicted.R
\name{correctly_predicted}
\alias{correctly_predicted}
\alias{correctly_predicted.default}
\alias{correctly_predicted.elo}
\alias{correctly_predicted.fastelo}
\alias{correctly_predicted.list}
\alias{correctly_predicted.matrix}
\title{correctly predicted outcomes}
\usage{
correctly_predicted(xdata, ...)

\method{correctly_predicted}{default}(xdata, ...)

\method{correctly_predicted}{elo}(xdata, exclude_draws = TRUE, daterange = NULL, ...)

\method{correctly_predicted}{fastelo}(xdata, ...)

\method{correctly_predicted}{list}(xdata, ...)

\method{correctly_predicted}{matrix}(xdata, ...)
}
\arguments{
\item{xdata}{result from \code{\link{elo.seq}}, \code{\link{fastelo}},
a square interaction matrix or a list with two items where the first is a
character vector with ID names (which represents the rank order to be
checked) and the second is an interaction matrix (which needs to be square
and which has column and row names)}

\item{...}{additional arguments depending on the class of object you supplied}

\item{exclude_draws}{logical, should draws be excluded from the calculation,
by default \code{TRUE}. If they are included, such interactions will be
scored as incorrectly predicted.}

\item{daterange}{character or Date of length two, which allows to restrict
the time range to be considered for \code{elo} objects}
}
\value{
a list with two items where the first item is the proportion of
correctly predicted outcomes and the second item is the total number of
interactions for which the winning probability is not 0.5 (in the case of
elo or fastelo) or the total number of interactions (in case of matrix or
list)
}
\description{
correctly predicted outcomes
}
\details{
If you provide results from \code{\link{elo.seq}} or
\code{\link{fastelo}}, this function first extracts the number of
interactions for which a winning expectation can be expressed, i.e. for all
interactions for which the winning probability for either individual is
different from 0.5. If the winning probability for both IDs is 0.5 then
either outcome is equally likely and hence it cannot be verified whether
the winning probability 'worked correctly'.

If you provide an interaction matrix, the order of columns in which it is
supplied is taken as the order to be checked, i.e. this just calculates the
proportion of interactions that are in upper triangle of the matrix.

If you provide a list with a rank order and an interaction matrix, the matrix
will be 'reshuffled' according to the rank order and then all entries above
the diagonal will be divided by the total number of interactions.

Note that there is one potential issue for the list-based method (rank order
and interaction matrix supplied), which is that it can't accomodate tied
ranks.
}
\section{Methods (by class)}{
\itemize{
\item \code{correctly_predicted(default)}: default method for logical vector

\item \code{correctly_predicted(elo)}: for usage with results of
\code{\link{elo.seq}}

\item \code{correctly_predicted(fastelo)}: for usage with results of
\code{\link{fastelo}}

\item \code{correctly_predicted(list)}: for usage with a list of order and
interaction matrix

\item \code{correctly_predicted(matrix)}: for usage with an interaction matrix

}}
\examples{
data(adv)
res <- elo.seq(winner = adv$winner, loser = adv$loser, Date = adv$Date)
correctly_predicted(res)
correctly_predicted(res, daterange = c("2010-01-10", "2010-01-20"))
# only one interaction considered because for the first no expection was
# expressed (same starting values for both contestants)
correctly_predicted(res, daterange = c("2010-01-01", "2010-01-02"))

data("devries98")
correctly_predicted(list(colnames(devries98), devries98))
# is the same as
correctly_predicted(devries98)
# reversed order
correctly_predicted(list(rev(colnames(devries98)), devries98))

mat <- matrix(ncol = 10, nrow = 10, 0)
colnames(mat) <- rownames(mat) <- letters[1:10]
mat[upper.tri(mat)] <- 101
mat[lower.tri(mat)] <- 100
# correct order
order1 <- colnames(mat)
correctly_predicted(list(order1, mat))
# not very good

# the worst possible order for that matrix:
order2 <- rev(order1)
correctly_predicted(list(order2, mat))
# not much worse than order 1...

mat <- matrix(ncol = 10, nrow = 10, 0)
colnames(mat) <- rownames(mat) <- letters[1:10]
mat[upper.tri(mat)] <- 1
mat[1, 2] <- 100
# correct ranking
order1 <- letters[1:10]
correctly_predicted(xdata = list(order1, mat))
# almost correct order
order2 <- c("b", "a", letters[3:10])
correctly_predicted(xdata = list(order2, mat))

}
\author{
Christof Neumann
}
