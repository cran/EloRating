\name{eloplot}
\alias{eloplot}
\title{
Elo rating plotting
}
\description{
Plotting Elo ratings for all or selected individuals over a specified time period
}
\usage{
eloplot(eloobject, ids="all", interpolate="yes", from="start",
        to="end", color=TRUE)
}
\arguments{
  \item{eloobject}{
elo object output of \link{elo.seq} function
}
  \item{ids}{
\code{"all"} will plot trajectories for all individuals within the dataset. \code{"first.20"} will plot the 20 first individuals. \code{"random.20"} will plot 20 randomly chosen individuals from the dataset. Alternatively, provide a list of individual IDs.
}
  \item{interpolate}{
by default (\code{"yes"}) plot interpolated Elo values or plot Elo values without interpolation (\code{"no"})
}
  \item{from}{
\code{"start"}: the plotted date range will start at the first date of the dataset or provide a custom date ("YYYY-MM-DD")
}
  \item{to}{
\code{"end"}: the plotted date range will end at the last date of the dataset or provide a custom date ("YYYY-MM-DD")
}
  \item{color}{
the plot is either colored (\code{TRUE}) or in black and white with symbols
}
}
\details{
For a visual inspection of an Elo object it is useful to plot the calculated trajectories. We recommend not to plot trajectories for more than 20 individuals at once.
}
\author{
Lars Kulik & Christof Neumann
}
\examples{
data(adv)
SEQ <- elo.seq(winner=adv$winner, loser=adv$loser, Date=adv$Date)
eloplot(SEQ, ids="all", interpolate="yes", from="start", to="end",
        color=TRUE)
}

