\name{print.elo}
\alias{print.elo}
\title{
prints its argument
}
\description{
prints its argument
}
\usage{
\method{print}{elo}(x, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
result from \code{\link{elo.seq}}
}
  \item{\dots}{
further arguments passed to or from other methods
}
}
\author{
Christof Neumann
}
\examples{
data(adv)
SEQ <- elo.seq(winner=adv$winner, loser=adv$loser, Date=adv$Date)
print(SEQ)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
