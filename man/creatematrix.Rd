\name{creatematrix}
\alias{creatematrix}
\title{
create a dominance matrix
}
\description{
create a dominance matrix from the underlying sequence
}
\usage{
creatematrix(eloobject, from="start", to="end",
             drawmethod=c("omit"), onlyinteracting=FALSE)
}
\arguments{
  \item{eloobject}{
    output from \link{elo.seq}.
}
  \item{from}{
    date from which on the matrix should start (default is first in sequence)
}
  \item{to}{
    date up to which the matrix should runs (default is last in sequence)
}
  \item{drawmethod}{
    "omit" = undecided interactions (draws/ties) are ignored (default).\cr
    "0.5" = each undecided is counted half a win for each dyad member\cr
    "1" = each undecided interaction is counted twice, i.e. as win for both individuals
}
  \item{onlyinteracting}{
    logical. Indicating whether all individuals that were present (default) are shown in the matrix, or 
    only those that were involved in an interaction in the specified date period. If no presence data 
    was supplied to \link{elo.seq}, it is assumed that all individuals were present at all times.
}
}
\value{
square matrix with dominance interactions (winner in rows, loser in columns)
}
\author{
Christof Neumann
}
\examples{
data(adv)
SEQ <- elo.seq(winner=adv$winner, loser=adv$loser, Date=adv$Date)
# create dyadic matrix over the entire period of data collection
creatematrix(SEQ)
# limit to a subset of interactions
creatematrix(SEQ, from="2010-01-25", to="2010-02-01")
# limit to a subset of interactions and show only those IDs that were 
# involved in at least one interaction
creatematrix(SEQ, from="2010-01-25", to="2010-02-01",
             onlyinteracting=TRUE)

## dealing with undecided interactions
data(adv2)
SEQ <- elo.seq(winner=adv2$winner, loser=adv2$loser, Date=adv2$Date, 
               draw=adv2$tie)
# omit ties/draws
creatematrix(SEQ)
# omit ties/draws
creatematrix(SEQ, drawmethod="0.5")
# omit ties/draws
creatematrix(SEQ, drawmethod="1")
}
