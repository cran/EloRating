\name{scale.elo}
\alias{scale.elo}
\title{
standardize Elo ratings
}
\description{
standardize Elo ratings between 0 and 1
}
\usage{
\method{scale}{elo}(x, center = TRUE, scale = TRUE)
}
\arguments{
  \item{x}{
a vector of Elo ratings.
}
  \item{center}{
ignored.
}
  \item{scale}{
ignored.
}
}
\value{
returns a vector of Elo ratings, which are scaled between 0 and 1, with the highest rating that is supplied becoming 1, the lowest becoming 0, and all others being proportionally scaled in between
}
\author{
Christof Neumann
}
\examples{
data(adv)
SEQ <- elo.seq(winner=adv$winner, loser=adv$loser, Date=adv$Date)
extract.elo(SEQ, "2010-01-30")
extract.elo(SEQ, "2010-01-30", standardize=TRUE)
# same as
scale.elo(extract.elo(SEQ, "2010-01-30"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
