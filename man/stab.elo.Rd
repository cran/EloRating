\name{stab.elo}
\alias{stab.elo}
\title{
calculate stability index \emph{S} 
}
\description{
Calculates the \emph{S} index as metric for the overall stability of a hierarchy during a specified time period
}
\usage{
stab.elo(eloobject, from=min(eloobject$stability$date),
         to=max(eloobject$stability$date), weight=TRUE)
}
\arguments{
  \item{eloobject}{
    an object of class \code{"elo"}, usually the result of a call to \link{elo.seq}
}
  \item{from}{
    from which date onwards should \emph{S} be calculated. By default the first date in the sequence is used
}
  \item{to}{
    until which date should \emph{S} be calculated. By default the last date in the sequence is used
}
  \item{weight}{
    should single rank changes be weighted by the Elo rating of the highest-rated individual involved in a rank change? Default is \code{TRUE}
}
}
\details{
\emph{S} ranges between 0 and 1, where 0 indicates an unstable hierarchy, in which the ordering reverses every other day, and 1, in which the ordering is stable and no rank changes occur.\cr
In contrast to the originally proposed \emph{S}, this version is now standardized between 0 and 1, and additionally, the interpretation is reversed, i.e. 1 refers to stable situations, whereas values closer to 0 indicate more instable hierarchies
}
\value{
returns the \emph{S} index
}
\references{
Neumann, C., Duboscq, J., Dubuc, C., Ginting, A., Irwan, A. M., Agil, M., Widdig, A. & Engelhardt, A. 2011. Assessing dominance hierarchies: validation and advantages of progressive evaluation with Elo-rating. Animal Behaviour, 82, 911-921.

McDonald, D. B. & Shizuka, D. 2013. Comparative transitive and temporal orderliness in dominance networks. Behavioral Ecology, 24, 511-520.
}
\author{
Christof Neumann
}
\examples{
data(adv)
SEQ <- elo.seq(winner=adv$winner, loser=adv$loser, Date=adv$Date)
stab.elo(SEQ)
stab.elo(SEQ, weight=FALSE)
stab.elo(SEQ, from="2010-01-20", to="2010-01-30")
stab.elo(SEQ, from="2010-01-20", to="2010-01-30", weight=FALSE)
}
