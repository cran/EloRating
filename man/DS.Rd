\name{DS}
\alias{DS}
\title{
calculate David's scores
}
\description{
calculate David's scores
}
\usage{
DS(interactionmatrix, prop=c("Pij", "Dij"))
}
\arguments{
  \item{interactionmatrix}{
square interaction matrix with winner in rows and losers in columns, for example the output from \code{\link{creatematrix}}
}
  \item{prop}{
the type of dyadic win proportion to be use. By default corrected for number of interactions in a dyad, otherwise the raw proportion
}
}
\value{
a data.frame with columns ID, DS (David's scores) and normDS (normalized David's scores)
}
\author{
Christof Neumann
}
\references{
David, H. A. 1987. Ranking from unbalanced paired-comparison data.  Biometrika, 74, 432-436.

Gammell, M. P., de Vries, H., Jennings, D. J., Carlin, C. M. and Hayden, T. J. 2003. David's score: a more appropriate dominance ranking method than Clutton-Brock et al.'s index. Animal Behaviour, 66, 601-605.

de Vries, H., Stevens, J. M. G. and Vervaecke, H. 2006. Measuring and testing the steepness of dominance hierarchies. Animal Behaviour, 71, 585-592.
}
\examples{
data(adv)
SEQ <- elo.seq(winner=adv$winner, loser=adv$loser, Date=adv$Date)
mat <- creatematrix(SEQ)
DS(mat)
}
