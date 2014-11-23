\name{randomelo}
\alias{randomelo}
\title{
calculate Elo ratings from an interaction matrix
}
\description{
calculate Elo ratings from an interaction matrix based on randomly generated sequences
}
\usage{
randomelo(interactionmatrix, runs=2000)
}
\arguments{
  \item{interactionmatrix}{
square interaction matrix with winner in rows and losers in columns, for example the output from \code{\link{creatematrix}}
}
  \item{runs}{
number of randomly generated sequences based on the interactions in the matrix
}
}
\value{
list of length 2. The first element contains a matrix with the final ratings of each individual from each random sequence. IDs are in the columns, each run is represented as one row. The second element of the list contains the original interaction matrix.
}
\author{
Christof Neumann
}
\examples{
data(adv)
SEQ <- elo.seq(winner=adv$winner, loser=adv$loser, Date=adv$Date)
mat <- creatematrix(SEQ)
res <- randomelo(mat, 10)
data.frame(ID=colnames(res[[1]]), avg=round(colMeans(res[[1]]),1))
}
