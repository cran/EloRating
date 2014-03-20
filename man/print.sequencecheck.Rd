\name{print}
\alias{print.sequencecheck}
\alias{print.seqchecknopres}
\title{
prints its argument
}
\description{
prints its argument
}
\usage{
\method{print}{sequencecheck}(x, \dots)
\method{print}{seqchecknopres}(x, \dots)
}
\arguments{
  \item{x}{
the result from \link{seqcheck}
}
  \item{\dots}{
further arguments passed to or from other methods
}
}
\author{
Christof Neumann
}
\examples{
data(adv); data(advpres)
SEQC <- seqcheck(winner=adv$winner, loser=adv$loser, Date=adv$Date,
                 presence=advpres)
print(SEQC)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
