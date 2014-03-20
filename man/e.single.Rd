\name{e.single}
\alias{e.single}
\title{
calculate Elo ratings for a single interaction
}
\description{
calculate Elo ratings for a single interaction
}
\usage{
e.single(ELO1old, ELO2old, outcome, k = 100)
}
\arguments{
  \item{ELO1old}{
Elo rating of the first individual
}
  \item{ELO2old}{
Elo rating of the second individual
}
  \item{outcome}{
        "1" = first individual wins and second looses\cr
        "2" = second individual wins and first looses\cr
        "0" = interaction ends in a draw/tie (no winner and no looser)
}
  \item{k}{
\emph{k} factor
}
}
\value{
length of vector 2 with updated ratings after the interaction of first and second individual
}
\references{
Albers, P. C. H. & de Vries, H. 2001. Elo-rating as a tool in the sequential estimation of dominance strengths. Animal Behaviour, 61, 489-495.

Neumann, C., Duboscq, J., Dubuc, C., Ginting, A., Irwan, A. M., Agil, M., Widdig, A. & Engelhardt, A. 2011. Assessing dominance hierarchies: validation and advantages of progressive evaluation with Elo-rating. Animal Behaviour, 82, 911-921.
}
\author{
Christof Neumann
}
\examples{
e.single(ELO1old=1200, ELO2old=1000, outcome=1, k=100)
# same as before
e.single(ELO1old=1000, ELO2old=1200, outcome=2, k=100)
# an undecided interaction
e.single(ELO1old=1200, ELO2old=1000, outcome=0, k=100)
# if rating differences are too big, no change occurs 
# if higher-rated individual wins
e.single(ELO1old=2000, ELO2old=1000, outcome=1, k=100)
# same as before but lower-rated individual wins and 
# therefore wins maximum number of points possible (i.e. k)
e.single(ELO1old=2000, ELO2old=1000, outcome=2, k=100)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
