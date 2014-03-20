\name{randomsequence}
\alias{randomsequence}
\title{
create a random dominance sequence
}
\description{
create a random dominance sequence for testing or simulations
}
\usage{
randomsequence(nID=10, avgIA=20, startdate=as.Date("2000-01-01"),
               alphabet=T, reversals=0.1, ties=NULL, presence=NULL)
}
\arguments{
  \item{nID}{
number of IDs, must be less than 2601.
}
  \item{avgIA}{
average number of interactions and individual is involved in.
}
  \item{startdate}{
a start date
}
  \item{alphabet}{
should the individual within an interaction that comes first in alphabetical order be the winner? By default TRUE, which gives some orderliness in the hierarchy.
}
  \item{reversals}{
this proportion of interactions ends in reversed outcomes, i.e. the individual that comes first in the alphabet looses. Default = 0.1.
}
  \item{ties}{
this proportion of interactions ends undecided.
}
  \item{presence}{
vector of length 2. The first value indicates what proportion of individuals are absent for some time. The second value indicates the propotion of time (days) these individuals are absent.
}
}
\value{
an object of class \code{randomsequence}, which is a list with the following items:
  \item{seqdat}{an interaction sequence}
  \item{pres}{a presence matrix, actually a \code{data.frame}}
}
\author{
Christof Neumann
}
\examples{
IA <- randomsequence()
SEQ <- elo.seq(winner=IA$seqdat$winner, loser=IA$seqdat$loser,
               Date=IA$seqdat$Date)
stab.elo(SEQ)

IA <- randomsequence(presence=c(0.5,0.5))
SEQ <- elo.seq(winner=IA$seqdat$winner, loser=IA$seqdat$loser,
               Date=IA$seqdat$Date, presence=IA$pres)
stab.elo(SEQ)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
