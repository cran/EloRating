\name{extract.elo}
\alias{extract.elo}
\title{
extract Elo ratings from elo object
}
\description{
extract Elo ratings from elo object
}
\usage{
extract.elo(eloobject, extractdate, standardize = FALSE, IDs = NULL,
            NA.interpolate = FALSE, daterange = 1)
}
\arguments{
  \item{eloobject}{
result from \link{elo.seq}
}
  \item{extractdate}{
date on which Elo ratings should be obtained, defaults to the last day in the data set
}
  \item{standardize}{
should the returned ratings be scaled between 0 and 1. Default is \code{FALSE}. See \link{scale.elo}
}
  \item{IDs}{
specify IDs for which ratings are returned. By default, returns all that were present on the date or at least on one day of the date range
}
  \item{NA.interpolate}{
if \code{FALSE} (default), the last known rating is returned, which might not be from the specified date itself (but older). If \code{TRUE}, ratings on days without observations are linearly interpolated between days with known ratings (i.e. dates with observed interactions)
}
  \item{daterange}{
if averaged ratings are desired, supply here the number of days from \cr \code{extractdate - 1}. By default (\code{daterange = 1}), the ratings of the single \code{extractdate} are returned. \code{daterange = 2} produces average ratings from \code{extractdate} and the day after, and so on...
}
}
\value{
named (IDs) vector of (average) Elo ratings
}
\author{
Christof Neumann
}
\examples{
data(adv)
SEQ <- elo.seq(winner=adv$winner, loser=adv$loser, Date=adv$Date)
extract.elo(SEQ, "2010-01-30")
extract.elo(SEQ, "2010-01-30", standardize=TRUE)

# same ratings (regardless of NA.interpolate),
# since "g" was observed on both days
extract.elo(SEQ, "2010-01-29", IDs="g")
extract.elo(SEQ, "2010-01-29", IDs="g", NA.interpolate=TRUE)

extract.elo(SEQ, "2010-01-31", IDs="g")
extract.elo(SEQ, "2010-01-31", IDs="g", NA.interpolate=TRUE)

# different ratings (depending on NA.interpolate),
# since "g" was not observed that day
extract.elo(SEQ, "2010-01-30", IDs="g")
extract.elo(SEQ, "2010-01-30", IDs="g", NA.interpolate=TRUE)

extract.elo(SEQ, "2010-01-10", daterange=5)
extract.elo(SEQ, "2010-01-10", daterange=5, NA.interpolate=TRUE)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
