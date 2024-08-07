% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eloplot.r
\name{eloplot}
\alias{eloplot}
\title{Elo rating plots}
\usage{
eloplot(
  eloobject,
  ids = "all",
  interpolate = "yes",
  from = "start",
  to = "end",
  color = TRUE
)
}
\arguments{
\item{eloobject}{elo object, output of \code{\link{elo.seq}} function}

\item{ids}{character, \code{"all"} will plot trajectories for all individuals within the dataset. \code{"first.20"} will plot the 20 first individuals. \code{"random.20"} will plot 20 randomly chosen individuals from the dataset. Alternatively, provide a list of individual IDs.}

\item{interpolate}{character, by default (\code{"yes"}) plot interpolated Elo values or plot Elo values without interpolation (\code{"no"})}

\item{from}{character, either \code{"start"}, i.e. the plotted date range will start at the first date of the dataset, or provide a custom date ("YYYY-MM-DD")}

\item{to}{character, either \code{"end"}, i.e. the plotted date range will end at the last date of the dataset, or provide a custom date ("YYYY-MM-DD")}

\item{color}{logical, the plot is either colored (\code{TRUE}) or in black and white with symbols}
}
\value{
a plot
}
\description{
plot Elo ratings for all or selected individuals over a specified time period
}
\details{
For a visual inspection of an Elo object it is useful to plot the calculated trajectories. We recommend not to plot trajectories for more than 20 individuals at once.

Note also, if plots for IDs are requested that had observations on only one day, these IDs are excluded from plotting and a corresponding warning message is produced.
}
\examples{
data(adv)
SEQ <- elo.seq(winner=adv$winner, loser=adv$loser, Date=adv$Date)
eloplot(SEQ, ids="all", interpolate="yes", from="start", to="end",
        color=TRUE)
}
\author{
Lars Kulik and Christof Neumann
}
