% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DCindex.R
\name{DCindex}
\alias{DCindex}
\title{Directional Consistency Index}
\usage{
DCindex(interactionmatrix)
}
\arguments{
\item{interactionmatrix}{square interaction matrix with winner in rows and losers in columns, for example the output from \code{\link{creatematrix}}}
}
\value{
numeric value, the DCI
}
\description{
calculate Directional Consistency Index
}
\examples{
data(adv)
SEQ <- elo.seq(winner = adv$winner, loser = adv$loser, Date = adv$Date)
mat <- creatematrix(SEQ)
DCindex(mat)

# or directly from a matrix
data(bonobos)
DCindex(bonobos)
}
\references{
\insertRef{vanhooff1987}{EloRating}
}
\author{
Christof Neumann
}
