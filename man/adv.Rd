\name{adv}
\alias{adv}
\alias{adv2}
\alias{advpres}
\docType{data}
\title{
Dominance sequence from Albers and de Vries (2001)
}
\description{
Dominance sequence from Albers and de Vries (2001)
}
\usage{data(adv)}
\format{
  Fictional example of an interaction sequence, with 33 interactions between 7 individuals.
  \describe{
    \item{\code{Date}}{Date of the interaction}
    \item{\code{Time}}{Time of the interaction (not mandatory for calculations)}
    \item{\code{winner}}{Winner of the interaction}
    \item{\code{loser}}{Loser of the interaction}
    \item{\code{tie}}{indicates whether the interaction ended undecided, i.e. in a draw/tie}
  }
}
\details{
\code{adv} is the data set of fictional interactions that was used by Albers and de Vries (2001). \code{adv2} adds to this fictional data to indicate whether an interaction ended undecided (draw). \code{advpres} is a fictional presence data set, indicating which individuals were present and absent on which date.
}
\source{
Albers, P. C. H. & de Vries, H. 2001. Elo-rating as a tool in the sequential estimation of dominance strengths. Animal Behaviour, 61, 489-495.
}
\references{
Albers, P. C. H. & de Vries, H. 2001. Elo-rating as a tool in the sequential estimation of dominance strengths. Animal Behaviour, 61, 489-495.
}
\examples{
data(adv)
}
\keyword{datasets}
