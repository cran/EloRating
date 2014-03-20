\name{traj.elo}
\alias{traj.elo}
\title{
calculate dominance trajectory
}
\description{
calculate individual Elo rating trajectory over time
}
\usage{
traj.elo(eloobject, ID, from=min(eloobject$stability$date),
         to=max(eloobject$stability$date))
}
\arguments{
  \item{eloobject}{
result from \link{elo.seq}
}
  \item{ID}{
the ID(s) of the individual(s)
}
  \item{from}{
from which date onwards should the trajectory be calculated. By default the first date in the sequence is used
}
  \item{to}{
until which date should the trajectory be calculated. By default the last date in the sequence is used
}
}
\value{
A data.frame with as many lines as specified in \code{ID}, columns for ID, date range, the actual slope (trajectory),
and the number of observed interactions within the date range
}
\author{
Christof Neumann
}
\examples{
data(adv)
SEQ <- elo.seq(winner=adv$winner, loser=adv$loser, Date=adv$Date)
traj.elo(SEQ, "a")

traj.elo(SEQ, "a", from="2010-01-20", to="2010-01-30")

# no slope available if ID was not observed interacting 
# inside the date range
traj.elo(SEQ, "a", from="2010-01-17", to="2010-01-18")

# no slope available if ID was only observed interacting 
# once within the date range
traj.elo(SEQ, "a", from="2010-01-17", to="2010-01-19")

# for several individuals
traj.elo(SEQ, c("a", "b", "c"))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
