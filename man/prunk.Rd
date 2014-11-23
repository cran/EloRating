\name{prunk}
\alias{prunk}
\title{
unknown relationships
}
\description{
unknown relationships
}
\usage{
prunk(eloobject, daterange=NULL)
}
\arguments{
  \item{eloobject}{
  output from \code{\link{elo.seq}} or a matrix, e.g. from \code{\link{creatematrix}}
}
  \item{daterange}{
  date range to be considered (character or Date of length 2), by default considers the entire date range of the sequence. In case the function works on a matrix this is ignored.
}
}
\value{
numeric, proportion of unknown relationships (and total N) when considering all possible dyads, and the same after accounting for co-residency. For matrices, considering co-residency is ignored (not possible...)
}
\author{
Christof Neumann
}
\examples{
data(adv); data(advpres)
x <- elo.seq(winner=adv$winner, loser=adv$loser, Date=adv$Date, presence=advpres)
prunk(x, c("2010-01-01", "2010-01-15"))
mat <- creatematrix(x, c("2010-01-01", "2010-01-15"))
prunk(mat)
}
