\name{seqcheck}
\alias{seqcheck}
\title{
runs raw data diagnostics for Elo rating
}
\description{
runs some diagnostics on the data supplied to \link{elo.seq}, to check whether \link{elo.seq} will run without errors
}
\usage{
seqcheck(winner, loser, Date, draw=NULL, presence=NULL)
}
\arguments{
  \item{winner}{
factor or character vector of winner IDs
}
  \item{loser}{
factor or character vector of loser IDs
}
  \item{Date}{
character vector of form "YYYY-MM-DD" with the date of the respective interaction
}
  \item{draw}{
logical (of \code{length(winner)}). Did did the interaction end undecided (i.e. drawed or tied)? By default all FALSE, i.e. no undecided interactions occured
}
  \item{presence}{
data.frame with presence data, see \link{elo.seq}
}
}
\details{
calender dates (for the sequence as well as in the first column of \code{presence}, if supplied) need to be in "YYYY-MM-DD" format!

\code{seqcheck} will return two types of messages: warnings and errors. Errors will result in the data NOT working when supplied to \code{elo.seq}, and need to be fixed. Warning message do not necessarily lead to failure of executing \code{elo.seq}. Note that by default \code{seqcheck} is part of \code{elo.seq}. If any error or warning is produced by \code{seqcheck}, these data will not work in \code{elo.seq}. Some warning (but not error) messages can be ignored (see below) and if the \code{runcheck} argument in \code{elo.seq} is set to \code{FALSE} Elo ratings will be calculated properly in such cases.

The actual checks (and corresponding messages) that are performed are described in more detail here:

Most likely (i.e. in our experience), problems are caused by mismatches between the interaction data and the corresponding presence data.

Errors:\cr
\code{presence starts AFTER data}: indicates that during interactions at the beginning of the sequence, no corresponding information was found in the presence data. Solution: augment presence data, or remove interactions until the date on which presence data starts\cr
\code{presence stops BEFORE data}: refers to the corresponding problem towards the end of interaction and presence data\cr
\code{during the following interactions, IDs were absent...}: indicates that according to the presence data, IDs were absent (i.e. "0"), but interactions with them occured on the very date(s) according to the interaction data\cr
\code{the following IDs occur in the data sequence but NOT...}: there is/are no columns corresponding to the listed IDs in the presence data\cr
\code{there appear to be gaps in your presence (days missing?)...}: check whether your presence data includes a line for \emph{each date} starting from the date of the first interaction through to the date of the last interaction

Warnings:\cr
\code{presence continues beyond data}: indicates that presence and interaction data do not end on the same date.\cr
\code{presence starts earlier than data}: indicates that presence and interaction data do not start on the same date.\cr
\code{the following IDs occur in the presence data but NOT...}: there are more ID columns in the presence data than IDs occuring in the interaction data

Other warnings/errors can result from inconsistencies in either the presence or sequence data, or be of a more general nature:

Errors:\cr
\code{no 'Date' column found}: in the presence data, no column exists with the name/header "Date". Please rename (or add) the necessary column named "Date" to your presence data.\cr
\code{at least one presence entry is not 1 or 0}: presence data must come in binary form, i.e. an ID was either present ("1") or absent ("0") on a given date. No \code{NA}s or other values are allowed.\cr
\code{your data vectors do not match in length}: at least one of the three mandatory arguments (winner, loser, Date) differs from one other in length. Consider handling your data in a data.frame, which avoids this error.

Warnings:\cr
\code{IDs occur in the data with inconsistent capitalization}: because \code{R} is case-sensitive, "A" and "a" are considered different individuals. If such labelling of IDs is on purpose, ignore the warning and set \code{runcheck=FALSE} when calling \code{elo.seq()}\cr
\code{There is (are) X case(s) in which loser ID equals winner ID}: winner and loser represent the same ID\cr
\code{the following individuals were observed only on one day}: while not per se a problem for the calculation of Elo ratings, individuals that were observed only on one day (irrespective of the number of interactions on that day) cannot be plotted. \code{\link{eloplot}} will give a warning in such cases, too.
}
\value{
returns info about possible errors, or states that data are fine for running with \link{elo.seq}
}
\author{
Christof Neumann
}
\examples{
data(adv)
seqcheck(winner=adv$winner, loser=adv$loser, Date=adv$Date)
data(advpres)
seqcheck(winner=adv$winner, loser=adv$loser, Date=adv$Date,
         presence=advpres)

# create faulty presence data
faultypres <- advpres[-1, ]
faultypres[5,2:8] <- 0
# seqcheck(winner=adv$winner, loser=adv$loser, Date=adv$Date,
#          presence=faultypres)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
