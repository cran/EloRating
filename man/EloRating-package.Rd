\name{EloRating-package}
\alias{EloRating-package}
\alias{EloRating}
\docType{package}
\title{
Elo Rating
}
\description{
Calculate Elo ratings as means to describe animal dominance hierarchies
}
\details{
\tabular{ll}{
Package: \tab EloRating\cr
Type: \tab Package\cr
Version: \tab 0.43\cr
Date: \tab 2014-11-23\cr
License: \tab GPL (>= 2)\cr
}
}
\author{
Christof Neumann & Lars Kulik
Maintainer: Christof Neumann <christofneumann1@gmail.com>
}
\references{
Elo, A. E. 1978. The Rating of Chess Players, Past and Present. New York: Arco.

Albers, P. C. H. & de Vries, H. 2001. Elo-rating as a tool in the sequential estimation of dominance strengths. Animal Behaviour, 61, 489-495.

Neumann, C., Duboscq, J., Dubuc, C., Ginting, A., Irwan, A. M., Agil, M., Widdig, A. & Engelhardt, A. 2011. Assessing dominance hierarchies: validation and advantages of progressive evaluation with Elo-rating. Animal Behaviour, 82, 911-921.
}
\keyword{ package }
\examples{
data(adv)
SEQ <- elo.seq(winner=adv$winner, loser=adv$loser, Date=adv$Date)
summary(SEQ)
}
