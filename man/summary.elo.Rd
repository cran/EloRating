\name{summary.elo}
\alias{summary.elo}
\title{
summarize elo object
}
\description{
overview of elo object
}
\usage{
\method{summary}{elo}(object, \dots)
}
\arguments{
  \item{object}{
    an object of class \code{"elo"}, usually the result of a call to \code{\link{elo.seq}}
}  \item{\dots}{
    further arguments passed to or from other methods.
}
}
\author{
Christof Neumann
}
\examples{
IA <- randomsequence()
SEQ <- elo.seq(winner=IA$seqdat$winner, loser=IA$seqdat$loser,
               Date=IA$seqdat$Date, draw=IA$seqdat$Draw,
               presence=IA$pres)
summary(SEQ)
}
