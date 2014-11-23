\name{elo.seq}
\alias{elo.seq}
\title{
Calculate Elo ratings
}
\description{
Calculate Elo ratings from a sequence of dominance interactions
}
\arguments{
  \item{winner}{
Either a factor or character vector with winners IDs of dominance interactions
}
  \item{loser}{
Either a factor or character vector with loser IDs of dominance interactions
}
  \item{Date}{
character vector of form "YYYY-MM-DD" with the date of the respective interaction
}
  \item{draw}{
logical. Did the interaction end undecided (i.e. drawn or tied)? By default all \code{FALSE}, i.e. no undecided interactions occurred. Note that in this case, \code{winner}/\code{loser} values can be interchanged
}
  \item{presence}{
option to supply data about presence and absence of individuals for part of the time the data collection covered. see details
}
  \item{startvalue}{
the value of Elo ratings of the two individuals that are involved in the first interaction of the overall sequence prior to this interaction. By default set to 1000. See also \code{init}.
}
  \item{k}{
factor \emph{k} that determines the maximum change in ratings. By default \code{k=100}
}
  \item{init}{
what Elo rating does an individual have prior to its first interaction. Three options are available:
 \code{average}: individuals always start with the value specified in \code{startvalue}. Given stable composition of the group, this also reflects the average Elo rating on each day in that group, \cr
 \code{bottom}: subjects entering at the current lowest Elo value if the lowest value getting lower its getting lower for all subjects which had this lowest values before, it is reflecting that in some species new subjects entering a group at the bottom level "bottom entry"\cr
 \code{bottom_low}: same as \code{bottom} but additionally the start values getting after the first interaction lower for all non-interacting subjects and, reflecting that we have at start no knowledge about the subjects this option offers for "bottom entry" species the possibility to consider that in a way that those subjects which are not interacting getting lower from start on,
}
  \item{iterate}{
not yet implemented
}
  \item{runcheck}{
should several checks regarding data integrety be performed, by default \code{TRUE}. See \link{seqcheck}
}
  \item{progressbar}{
should progress bars be displayed, by default \code{progressbar=TRUE}
}
}
\details{
the presence 'matrix' is actually an object of class \code{data.frame} containing information about wether an individual was present on a given day or not. The first column represents the dates, running at least from the date of the earliest interaction until at least the date of the last interaction with one line per day (regardless of whether there were actually interactions observed on each day). Further, each individual is represented as a column in which "1" indicates an individual was present on the row-date and a "0" indicates the individuals absence on this date. \code{NA}s are not allowed. See \link{advpres} for an example.
}
\value{
An object of class \code{elo}, which is list with 10 items that serves as basis to extract relevant info.
\item{mat}{a date by ID-\code{matrix} with raw Elo ratings}
\item{lmat}{a date by ID-\code{matrix} with raw Elo ratings}
\item{cmat}{a date by ID-\code{matrix} with raw Elo ratings}
\item{pmat}{a date by ID-\code{matrix} with with presence data}
\item{nmat}{a date by ID-\code{matrix} containing the number of interactions a given ID was involved in on a given day}
\item{logtable}{details on each single interaction}
\item{stability}{a \code{data.frame} containing information about stability (see \link{stab.elo}) }
\item{truedates}{vector of class \code{Date} covering the ranges of dates in the dataset}
\item{misc}{various}
\item{allids}{a (sorted) character vector with all IDs that occur in the dataset}
}
\references{
Albers, P. C. H. & de Vries, H. 2001. Elo-rating as a tool in the sequential estimation of dominance strengths. Animal Behaviour, 61, 489-495.

Neumann, C., Duboscq, J., Dubuc, C., Ginting, A., Irwan, A. M., Agil, M., Widdig, A. & Engelhardt, A. 2011. Assessing dominance hierarchies: validation and advantages of progressive evaluation with Elo-rating. Animal Behaviour, 82, 911-921.
}
\author{
Christof Neumann & Lars Kulik
}
\examples{
data(adv)
SEQ <- elo.seq(winner=adv$winner, loser=adv$loser, Date=adv$Date)
summary(SEQ)
}
