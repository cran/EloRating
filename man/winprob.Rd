\name{winprob}
\alias{winprob}
\title{
calculate expected probability of winning
}
\description{
calculate expected probability of winning given known strength of both opponents
}
\usage{
winprob(elo1, elo2)
}
\arguments{
  \item{elo1}{
    Elo rating from individual for which the winning probability should be calculated
}
  \item{elo2}{
    Elo rating of the opponent
}
}
\value{
numeric, expected chance of first individual to win an interacation with the second individual
}
\references{
Elo, A. E. 1978. The Rating of Chess Players, Past and Present. New York: Arco.
}
\author{
Christof Neumann
}
\examples{
winprob(1200,1000)
winprob(1000,1200)
winprob(1000,1000)
}
