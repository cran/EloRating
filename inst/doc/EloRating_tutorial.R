## ----setup, include=FALSE---------------------------------------------------------------------------------------------
# csl: animal-behaviour.csl
knitr::opts_chunk$set(echo = TRUE)
options(width = 120)
def.chunk.hook  <- knitr::knit_hooks$get("chunk")
knitr::knit_hooks$set(chunk = function(x, options) {
  x <- def.chunk.hook(x, options)
  ifelse(options$size != "normalsize", paste0("\\", options$size,"\n\n", x, "\n\n \\normalsize"), x)
})
knitr::opts_chunk$set(size = 'footnotesize', fig.align = 'center')
# cache location
knitr::opts_chunk$set(cache.path = "zz_cacheloc/")

## ---- echo=FALSE------------------------------------------------------------------------------------------------------
runbaboonplots <- TRUE

## ---- eval=FALSE------------------------------------------------------------------------------------------------------
#  install.packages("EloRating")

## ---- eval=FALSE------------------------------------------------------------------------------------------------------
#  library(devtools)
#  install_github("gobbios/EloRating")
#  install_github("gobbios/EloRating", build_vignettes = TRUE) # with pdf tutorial

## ---- echo=FALSE------------------------------------------------------------------------------------------------------
xdata <- read.table(system.file("ex-sequence.txt", package = "EloRating"), header = TRUE)
xtab1 <- head(xdata[, c("Date", "winner", "loser")])
knitr::kable(xtab1)

## ---- echo=FALSE------------------------------------------------------------------------------------------------------
xtab1 <- head(xdata[, c("Date", "winner", "loser", "Draw", "intensity")])
knitr::kable(xtab1)

## ---- warning=FALSE, message=FALSE------------------------------------------------------------------------------------
library(EloRating)

## ---------------------------------------------------------------------------------------------------------------------
xdata <- read.table(system.file("ex-sequence.txt", package = "EloRating"), header = TRUE)

## ---- eval=FALSE------------------------------------------------------------------------------------------------------
#  # on Windows
#  xdata <- read.table("c:\\temp\\ex-sequence.txt", header = TRUE, sep = "\t")
#  # on Mac
#  xdata <- read.table("~/Documents/ex-sequence.txt", header = TRUE, sep = "\t")

## ---------------------------------------------------------------------------------------------------------------------
seqcheck(winner = xdata$winner, loser = xdata$loser, Date = xdata$Date)

## ---------------------------------------------------------------------------------------------------------------------
res <- elo.seq(winner = xdata$winner, loser = xdata$loser, Date = xdata$Date, runcheck = TRUE)
summary(res)

## ---------------------------------------------------------------------------------------------------------------------
extract_elo(res, extractdate = "2000-05-28")
extract_elo(res, extractdate = "2000-05-28", IDs = c("s", "a", "c", "k"))

## ---------------------------------------------------------------------------------------------------------------------
extract_elo(res)
# the same as because 2000-09-06 is the last date in the sequence: 
extract_elo(res, extractdate = "2000-09-06")

## ---- echo=FALSE, eval=FALSE------------------------------------------------------------------------------------------
#  # data generation for parasites example
#  xdata <- read.table(system.file("ex-sequence.txt", package = "EloRating"), header = TRUE)
#  xpres <- read.table(system.file("ex-presence.txt", package = "EloRating"), header = TRUE)
#  xpres$Date <- as.Date(as.character(xpres$Date))
#  res <- elo.seq(winner = xdata$winner, loser = xdata$loser, Date = xdata$Date,
#                 presence = xpres, draw = xdata$Draw)
#  set.seed(1234)
#  tdata <- data.frame(id = sample(colnames(xpres)[2:11], size = 52, replace = TRUE),
#                      Date = sort(sample(xpres$Date, size = 52, replace = TRUE)))
#  sum(duplicated(tdata))
#  tdata$elo <- extract_elo(res, tdata$Date, IDs = tdata$id)
#  tdata <- na.omit(tdata)
#  
#  b0 <- 0 # intercept
#  b1 <- 0.0015 # slope
#  mu <- exp(b0 + b1 * tdata$elo)
#  tdata$parasites <- rpois(n = nrow(tdata), lambda = mu)
#  write.table(tdata[, c("id", "Date", "parasites")], file = "inst/ex-parasites.txt",
#              sep = "\t", quote = FALSE, row.names = FALSE)

## ---------------------------------------------------------------------------------------------------------------------
parasites <- read.table(system.file("ex-parasites.txt", package = "EloRating"), header = TRUE)
parasites$Date <- as.Date(as.character(parasites$Date))
head(parasites)

## ---------------------------------------------------------------------------------------------------------------------
parasites$elo <- extract_elo(res, extractdate = parasites$Date, IDs = parasites$id)
head(parasites)

## ---- echo=FALSE, fig.width=6, fig.height=3.8, out.width = "50%", fig.align='center', fig.cap="\\small Parasite count as a function of day-specific Elo ratings. Each individual has its own colour. Code to produce the figure is in the \\nameref{sec:appendix}. \\label{fig:parasites}"----
# for simplicity's sake, use a glm, not glmm: it's just for illustration
mod <- glm(parasites ~ elo, data = parasites, family = "poisson")
pdata <- data.frame(elo = seq(min(parasites$elo), max(parasites$elo), length.out = 51))
pdata$par <- predict(mod, newdata = pdata, type = "r")
if (exists("hcl.colors")) {
  xcols <- hcl.colors(n = 9, palette = "zissou1", alpha = 0.5)[parasites$id]
} else {
  xcols <- rainbow(n = 9, alpha = 0.5)[parasites$id]
}
plot(parasites$elo, parasites$parasites, pch = 16, col = xcols, 
     xlab = "Elo rating", ylab = "parasite count", las = 1, cex = 1.3)
points(pdata$elo, pdata$par, type = "l")

## ---- fig.width=7, fig.height=4.3, out.width="50%", fig.align='center', fig.cap="\\small Elo-ratings of 10 individuals over the entire study period. \\label{fig:one}"----
eloplot(res)

## ---- fig.width=7, fig.height=4.3, out.width="50%", fig.align='center', fig.cap="\\small Elo-ratings of 5 individuals over a month. \\label{fig:two}"----
eloplot(eloobject = res, ids = c("s", "a", "w", "k", "c"), from = "2000-06-05", to = "2000-07-04")

## ---------------------------------------------------------------------------------------------------------------------
xpres <- read.table(system.file("ex-presence.txt", package = "EloRating"), header = TRUE)
xpres$Date <- as.Date(as.character(xpres$Date))
head(xpres)

## ---------------------------------------------------------------------------------------------------------------------
seqcheck(winner = xdata$winner, loser = xdata$loser, Date = xdata$Date, presence = xpres, draw = xdata$Draw)
res2 <- elo.seq(winner = xdata$winner, loser = xdata$loser, Date = xdata$Date, presence = xpres, draw = xdata$Draw)

## ---------------------------------------------------------------------------------------------------------------------
extract_elo(res2, extractdate = "2000-05-28")
# note that "s" is absent and omitted
extract_elo(res2, extractdate = "2000-05-28", IDs = c("s", "a", "c", "k"))
# note that "s" is absent and returned as NA

## ---- fig.width=7, fig.height=4.3, out.width="50%", fig.align='center', fig.cap="\\small Elo-ratings of 10 individuals over the entire study period. Note that several individuals were absent during parts of the date range and therefore appear with gaps in the plot (e.g. \\textit{c} and \\textit{f}). Compare to figure \\ref{fig:one}. \\label{fig:three}"----
eloplot(res2)

## ---- fig.width=7, fig.height=4.3, out.width="50%", fig.align='center', fig.cap="\\small Elo-ratings of 5 individuals over a month. Note that individual \\textit{c} is not displayed in the plot, since it has not been present during the date range supplied to \\texttt{eloplot()}. Compare to figure \\ref{fig:two}. \\label{fig:four}"----
eloplot(res2, ids = c("s", "a", "w", "k", "c"), from = "2000-06-05", to = "2000-07-04")

## ---------------------------------------------------------------------------------------------------------------------
myranks <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
# vector needs to have names!
names(myranks) <- c("a", "c", "d", "f", "g", "k", "n", "s", "w", "z")
myranks

## ---- echo=FALSE, fig.width=6, fig.height=3.6, out.width = "50%", fig.align='center', fig.cap="\\small Different shape parameters supplied to the \\texttt{createstartvalues()} function. Code to produce the figure is in the \\nameref{sec:appendix}. \\label{fig:shapes}"----
plot(0, 0, "n", xlim = c(1,10), ylim = c(500, 1500), xlab = "prior ordinal rank", ylab = "custom startvalue", cex.axis = 0.8, cex.lab = 0.8, las = 1)
shapes <- c(0, 0.1, 0.3, 0.5, 1)
xcols <- c("black", "grey", "red", "yellow", "blue")
for(i in 1:length(shapes)) {
  points(myranks, createstartvalues(ranks = myranks, shape = shapes[i])$res, type = "l", col = xcols[i], lwd = 2)
}
legend("topright", lty = 1, col = xcols, legend = shapes, lwd = 2, title = "shape", bty = "n",  cex = 0.7)

## ---------------------------------------------------------------------------------------------------------------------
startvals <- createstartvalues(ranks = myranks, shape = 0.3)
res3 <- elo.seq(winner = xdata$winner, loser = xdata$loser, Date = xdata$Date, presence = xpres, startvalue = startvals$res)

## ---- fig.width=7, fig.height=4.3, out.width="50%", fig.align='center', fig.cap="\\small Elo-ratings of 10 individuals over the entire study period with prior knowledge in the form of known ranks incorporated. Compare to figure \\ref{fig:three}. \\label{fig:priorknowledge1}"----
eloplot(res3)

## ---------------------------------------------------------------------------------------------------------------------
data(adv2)
# no prior knowledge
res1 <- elo.seq(winner = adv2$winner, loser = adv2$loser, Date = adv2$Date)
extract_elo(res1)

# use the above calculated ratings as known 'ranks'
myranks <- 1:length(extract_elo(res1))
names(myranks) <- names(extract_elo(res1))
mystart <- createstartvalues(myranks, startvalue = 1000, k = 100)
res2 <- elo.seq(winner = adv2$winner, loser = adv2$loser, Date = adv2$Date, startvalue = mystart$res)
extract_elo(res2)

## ---- echo=FALSE------------------------------------------------------------------------------------------------------
e1 <- extract_elo(res1); e1 <- e1[sort(names(e1))]
e2 <- extract_elo(res2); e2 <- e2[sort(names(e2))]

## ---- echo=FALSE, fig.width=9, fig.height=3.3, out.width="90%", fig.cap="\\small Elo-ratings from a group of seven individuals. On the left without prior knowledge and in the center with prior knowledge (here: the ordinal ranking at the end of the sequence without prior knowledge). The right panel shows the correlation between ratings on each date between the two runs. Code to produce the figure is in the \\nameref{sec:appendix}. \\label{fig:prior}"----
par(mfrow = c(1, 3))

dates <- res1$truedates
mycols <- c("red", "blue", "gold", "black", "grey", "green", "darkred")


ratings1 <- res1$cmat
ratings1 <- ratings1[, sort(colnames(ratings1))]
plot(0, 0, type = "n", xlim = range(dates), ylim = c(600, 1400), axes = FALSE, xlab = "Date", 
     ylab = "Elo-ratings", cex.lab = 0.7)
for(i in 1:ncol(ratings1)) points(dates, ratings1[, i], type = "l", col = mycols[i])
axis.Date(1, x = dates, cex.axis = 0.7)
axis(2, las = 1, cex.axis = 0.7)
box()

ratings2 <- res2$cmat
ratings2 <- ratings2[, sort(colnames(ratings2))]
plot(0, 0, type = "n", xlim = range(dates), ylim = c(600, 1400), axes = FALSE, xlab = "Date", 
     ylab = "Elo-ratings", cex.lab = 0.7)
for(i in 1:ncol(ratings2)) points(dates, ratings2[, i], type = "l", col = mycols[i])
axis.Date(1, x = dates, cex.axis = 0.7)
axis(2, las = 1, cex.axis = 0.7)
box()

plot(0, 0, type = "n", xlim = range(dates), ylim = c(0, 1), axes = FALSE, xlab = "Date", 
     ylab = "correlation coefficient", cex.lab = 0.7)
for(i in 1:nrow(ratings1)) points(dates[i], cor(ratings1[i, ], ratings2[i, ]), pch = 16)
axis.Date(1, x = dates, cex.axis = 0.7)
axis(2, las = 1, cex.axis = 0.7)
box()

## ---------------------------------------------------------------------------------------------------------------------
# with four rank classes
myrankclasses <- list(alpha = "a", high = c("b", "c"), mid = c("d", "e"), low = c("f", "g"))
createstartvalues(rankclasses = myrankclasses)$res

# with two rank classes
myrankclasses2 <- list(class1 = NULL, high = c("a", "b", "c"), class3 = NULL,
                       low = c("d", "e", "f", "g"))
createstartvalues(rankclasses = myrankclasses2)$res

# with two rank classes
myrankclasses3 <- list(high = c("a", "b", "c"), mid = NULL,
                       low = c("d", "e", "f", "g"), superlow = NULL)
createstartvalues(rankclasses = myrankclasses3)$res

## ---- eval=FALSE------------------------------------------------------------------------------------------------------
#  mypriors <- c(2000, 0); names(mypriors) <- c("a", "g")
#  elo.seq(winner = adv2$winner, loser = adv2$loser, Date = adv2$Date,
#                      startvalue = mypriors)

## ---------------------------------------------------------------------------------------------------------------------
table(adv2$intensity) # remind ourselves how the intensity categories are named

myk <- list(displace = 50, fight = 200)
res3 <- elo.seq(winner = adv2$winner, loser = adv2$loser, Date = adv2$Date, intensity = adv2$intensity, k = myk)
extract_elo(res3)

## ---- eval=TRUE, echo=TRUE, fig.width=7, fig.height=4, out.width="4in", fig.align='center', fig.cap="\\small Final ratings of 7 individuals when calculated without any prior knowledge (red), prior knowledge of `ranks' (blue) and accounting for different interaction intensities (gold). \\label{fig:custom}"----
plot(0, 0, "n", xlim = c(1, 7), ylim = c(600, 1400), xlab = "individual", ylab = "Elo-rating", 
     axes = FALSE)
axis(1, at = 1:7, labels = res1$allids, lwd = NA); axis(2, las = 1)

x <- extract_elo(res1); x <- x[sort(names(x))]
points(1:7, x, pch = 16, col = "red")

x <- extract_elo(res2); x <- x[sort(names(x))]
points(1:7, x, pch = 16, col = "blue")

x <- extract_elo(res3); x <- x[sort(names(x))]
points(1:7, x, pch = 16, col = "gold")
box()
legend(4, 1400, legend = c("no prior knowledge", "prior ranks", "custom k"), lwd = 1, 
       col = c("red", "blue", "gold"), ncol = 3, xpd = TRUE, xjust = 0.5, yjust = 0, 
       cex = 0.8, bg = "white")

## ---- echo=FALSE, fig.width=9, fig.height=3.3, fig.align='center', fig.cap="\\small Elo-ratings from a group of ten individuals. On the left without prior knowledge and in the center with prior knowledge (here: the ordinal ranking at the end of the sequence without prior knowledge). The right panel shows the ratings as they develop if we use some obviously wrong prior ranking. Code to produce the figure is in the \\nameref{sec:appendix}. \\label{fig:prior2}"----
xdata <- read.table(system.file("ex-sequence.txt", package = 'EloRating'), header = TRUE)
# no prior knowledge
s1 <- elo.seq(winner = xdata$winner, loser = xdata$loser, Date = xdata$Date)

# use the above calculated ratings as known 'ranks'
myranks <- 1:length(extract_elo(s1))
names(myranks) <- names(extract_elo(s1))
mystart <- createstartvalues(myranks, startvalue = 1000, k = 100)
s2 <- elo.seq(winner = xdata$winner, loser = xdata$loser, Date = xdata$Date,
               startvalue = mystart$res)

# reverse
myranks[1:10] <- 10:1
mystart <- createstartvalues(myranks, startvalue = 1000, k = 100)
s3 <- elo.seq(winner = xdata$winner, loser = xdata$loser, Date = xdata$Date,
               startvalue = mystart$res)

par(mfrow = c(1, 3))

dates <- s1$truedates
mycols <- c("red", "blue", "gold", "black", "grey", "green", "darkred", "darkblue", "pink", "cyan")

# do the plots
ratings1 <- s1$cmat
ratings1 <- ratings1[, sort(colnames(ratings1))]
plot(0, 0, type = "n", xlim = range(dates), ylim = c(400, 1600), axes = FALSE, xlab = "Date",
     ylab = "Elo-ratings", cex.lab = 0.7)
for(i in 1:ncol(ratings1)) points(dates, ratings1[, i], type = "l", col = mycols[i])
axis.Date(1, x = dates, cex.axis = 0.7)
axis(2, las = 1, cex.axis = 0.7)
box()

ratings2 <- s2$cmat
ratings2 <- ratings2[, sort(colnames(ratings2))]
plot(0, 0, type = "n", xlim = range(dates), ylim = c(400, 1600), axes = FALSE, xlab = "Date",
     ylab = "Elo-ratings", cex.lab = 0.7)
for(i in 1:ncol(ratings2)) points(dates, ratings2[, i], type = "l", col = mycols[i])
axis.Date(1, x = dates, cex.axis = 0.7)
axis(2, las = 1, cex.axis = 0.7)
box()

ratings3 <- s3$cmat
ratings3 <- ratings3[, sort(colnames(ratings3))]
plot(0, 0, type = "n", xlim = range(dates), ylim = c(400, 1600), axes = FALSE, xlab = "Date",
     ylab = "Elo-ratings", cex.lab = 0.7)
for(i in 1:ncol(ratings3)) points(dates, ratings3[, i], type = "l", col = mycols[i])
axis.Date(1, x = dates, cex.axis = 0.7)
axis(2, las = 1, cex.axis = 0.7)
box()

## ---------------------------------------------------------------------------------------------------------------------
eres <- elo.seq(xdata$winner, xdata$loser, xdata$Date)
ores <- optimizek(eres, krange = c(10, 500), resolution = 491)
ores$best

## ---- fig.width=7, fig.height=4.4, out.width="50%", fig.cap = "\\small Optimal \\textit{k} for an interaction sequence with 250 interactions among 10 individuals. The \\textit{k} that maximizes the likelihood is 93. \\label{fig:maxlik1}"----
plot(ores$complete$k, ores$complete$loglik, type = "l", las = 1, xlab = bquote(italic(k)), ylab = "log likelihood")
abline(v = ores$best$k, col = "red")

## ---- fig.width=7, fig.height=4.4, out.width="50%", fig.cap = "\\small Optimization procedure that did not result in a local maximum. The maximum likelihood estimate for \\textit{k} here is 200, which lies at the boundary of the tested values. \\label{fig:maxlik2}"----
ores1 <- optimizek(eres, krange = c(200, 400), resolution = 501)
ores1$best
plot(ores1$complete$k, ores1$complete$loglik, type = "l", las = 1, xlab = bquote(italic(k)), ylab = "log likelihood")
abline(v = ores1$best$k, col = "red")

## ----differentshapes, cache = TRUE, echo=FALSE, fig.width=8, fig.height=5, out.width="60%", fig.cap = "\\small Likelihood functions for six example data sets. The panel titles reflect the names of the data sets as stored in the package. \\label{fig:maxlik3}"----
plotfoo <- function(xd, ex = FALSE) {
  if("winner" %in% colnames(xd)) {
    xd$Winner <- xd$winner
    xd$Loser <- xd$loser
  }
  xd$Winner <- as.character(xd$Winner)
  xd$Loser <- as.character(xd$Loser)
  m <- unique(c(xd$Winner, xd$Loser))
  eres1 <- fastelo(xd$Winner, xd$Loser, m, rep(100, nrow(xd)), rep(1000, length(m)))
  ores1 <- optimizek(eres1, krange = c(0, 400), resolution = 101)
  yrange <- range(ores1$complete$loglik)
  
  if(ex) {
    eres2 <- fastelo(xd$Winner, xd$Loser, m, rep(100, nrow(xd)), rep(1000, length(m)), NORMPROB = FALSE)
    ores2 <- optimizek(eres2, krange = c(0, 400), resolution = 101)
    yrange <- range(c(ores1$complete$loglik, ores2$complete$loglik))
  }
  
  plot(0, 0, type = "n", xlim = c(10, 400), ylim = yrange, ylab = "log likelihood", xlab = bquote(italic(k)), las = 1, cex.axis = 0.8)
  
  points(ores1$complete$k, ores1$complete$loglik, type = "l", col = "red")
  abline(v = ores1$best$k, col = "red")
  mtext(text = ores1$best$k, at = ores1$best$k, line = 0, font = 2, col = "red", cex = 0.8)

  if(ex) {
    points(ores2$complete$k, ores2$complete$loglik, type = "l", col = "blue")
  abline(v = ores2$best$k, col = "blue")
  legend("bottomright", lty = c(1, 1), col = c("red", "blue"), legend = c("normal", "exponential"), cex = 0.6, title = "expectation function")
  rm(eres2, ores2)
  }
  rm(m, eres1, ores1, yrange, ex)
  
  x <- as.character(match.call())[2]
  title(main = x)
}

par(mfrow = c(2, 2), family = "serif")
if(runbaboonplots) plotfoo(baboons2)
if(runbaboonplots) plotfoo(baboons3)
plotfoo(adv)
plotfoo(xdata)

## ---------------------------------------------------------------------------------------------------------------------
eres <- elo.seq(xdata$winner, xdata$loser, xdata$Date, intensity = xdata$intensity)
# two list items: 'fight' and 'threat', because these are the two interaction types specified in xdata$intensity
mykranges <- list(fight = c(10, 500), threat = c(10, 500))
ores2 <- optimizek(eres, krange = mykranges, resolution = 91)
ores2$best

## ----ksimu, cache=TRUE, fig.height=6, fig.width=8, out.width="65%", fig.cap="\\small Optimal k values for an interaction sequence with two different interaction types (fights and threats). Large likelihoods are red, small likelihoods are blue. \\label{fig:2k}"----
heatmapplot(loglik ~ threat + fight, data = ores2$complete)

## ---- echo = FALSE, fig.width=7, fig.height=4.8, out.width="50%", fig.cap = "\\small Incorporating versus ignoring interaction type leads to different shapes in likelihood functions. The arrows below the horizontal axis indicate the optimal \\textit{k} for each curve. See text for more explanations. \\label{fig:likelihoodcompare}"----
plot(0, 0, type = "n", xlim = c(0, 500), ylim = c(-220, -100), las = 1, xlab = bquote(italic(k)), 
     ylab = "log likelihood", yaxs = "i")
points(ores$complete$k, ores$complete$loglik, type = "l", col = "blue", lwd = 2)
arrows(x0 = ores$best$k, y0 = -230, x1 = ores$best$k, y1 = -220, col = "blue", lwd = 2, xpd = TRUE, length = 0.1)

pdata <- ores2$complete[ores2$complete$fight == ores2$best$fight, ]
points(pdata$threat, pdata$loglik, type = "l", col = "red")
arrows(x0 = ores2$best$threat, y0 = -230, x1 = ores2$best$threat, y1 = -220, col = "red", lwd = 2, xpd = TRUE, length = 0.1)

pdata <- ores2$complete[ores2$complete$fight == 10, ]
points(pdata$threat, pdata$loglik, type = "l", col = "red", lty = 3)
x <- pdata$threat[which.max(pdata$loglik)]
arrows(x0 = x, y0 = -230, x1 = x, y1 = -220, col = "red", lwd = 2, xpd = TRUE, length = 0.1, lty = 3)

pdata <- ores2$complete[ores2$complete$threat == ores2$best$threat, ]
points(pdata$fight, pdata$loglik, type = "l", col = "grey")
arrows(x0 = ores2$best$fight, y0 = -230, x1 = ores2$best$fight, y1 = -220, col = "grey", lwd = 2, xpd = TRUE, length = 0.1)

pdata <- ores2$complete[ores2$complete$threat == 500, ]
points(pdata$fight, pdata$loglik, type = "l", col = "grey", lty = 3)
x <- pdata$fight[which.max(pdata$loglik)]
arrows(x0 = x, y0 = -230, x1 = x, y1 = -220, col = "grey", lwd = 2, xpd = TRUE, length = 0.1, lty = 3)

legend("bottom", legend = c("one interaction type", "threat (fight fixed)", "fight (threat fixed)"), 
       col = c("blue", "red", "grey"), lty = 1, cex = 0.7)

## ---------------------------------------------------------------------------------------------------------------------
orires <- elo.seq(winner = adv$winner, 
                  loser = adv$loser, 
                  Date = adv$Date, 
                  runcheck = FALSE)
xres <- optistart(eloobject = orires, 
                  runs = 5000)

# using 'good' ('optimal') start values
xres1 <- elo.seq(winner = adv$winner, 
                 loser = adv$loser, 
                 Date = adv$Date, 
                 runcheck = FALSE, 
                 startvalue = xres$resmat[c(which.max(xres$logliks)), ])
# using 'bad' start values
xres2 <- elo.seq(winner = adv$winner, 
                 loser = adv$loser, 
                 Date = adv$Date, 
                 runcheck = FALSE, 
                 startvalue = xres$resmat[c(which.min(xres$logliks)), ])

## ---- eval=FALSE------------------------------------------------------------------------------------------------------
#  eloplot(xres1)
#  eloplot(xres2)

## ---- fig.width=7, fig.height=4, out.width="70%", fig.cap = "Ratings with starting values that were created using the \\texttt{optistart()} function."----
eloplot(xres1)

## ----optistart, eval = TRUE, cache = TRUE, fig.width=7, fig.height=3.8, out.width = "70%", fig.cap = "\\small Comparison of performance between different approaches to assign custom start values. Note that the third boxplot is omitted for performance reasons. If you want to see it, you need to uncomment the relevant lines in the code block. \\label{fig:optistart}"----
library(aniDom)
set.seed(123)
resmat <- matrix(ncol = 4, nrow = 50)

for (i in 1:nrow(resmat)) {
  # create interactions from known ranks
  xd <- generate_interactions(N.inds = 10, N.obs = 200, b = -2, a = 1, id.biased = TRUE)
  allids <- letters[1:10]
  w <- allids[xd$interactions$Winner]
  l <- allids[xd$interactions$Loser]
  D <- seq.Date(as.Date("2000-01-01"), by = "day", length.out = length(w))
  
  # informed by known ranks
  myranks <- 1:10
  names(myranks) <- allids
  kvals <- rep(100, length(w))
  svals <- createstartvalues(ranks = myranks, shape = 0.5)$res
  ores1 <- fastelo(w, l, allids, kvals, svals)
  ores1 <- ores1[[1]][allids]
  
  # informed by optimized start values
  templist <- list(allids = allids, 
                   misc = c(normprob = "1"), 
                   logtable = data.frame(winner = w, loser = l),
                   kvals = rep(100, length(w)), 
                   startvalues = rep(1000, length(allids)))
  svals <- optistart(templist, runs = 200)$best
  ores2 <- fastelo(w, l, allids, kvals, svals)
  ores2 <- ores2[[1]][allids]
  
  # with more runs
  # svals <- optistart(templist, runs = 2000)$best
  # ores3 <- fastelo(w, l, allids, kvals, svals)
  # ores3 <- ores3[[1]][allids]
  
  # uninformed
  svals <- rep(1000, length(allids))
  ores4 <- fastelo(w, l, allids, kvals, svals)
  ores4 <- ores4[[1]]
  
  # store results
  resmat[i, 1] <- cor(ores1, myranks, method = "s")
  resmat[i, 2] <- cor(ores2, myranks, method = "s")
  # resmat[i, 3] <- cor(ores3, myranks, method = "s")
  resmat[i, 4] <- cor(ores4, myranks, method = "s")
  
  # clean up
  rm(xd, allids, w, l, D, myranks, kvals, svals, ores1, ores2, ores4, templist)
}

# 'inverse', so that correlations are positive
resmat <- resmat * (-1)
boxplot(resmat, axes = FALSE, boxwex = 0.4, lty = 1)
axis(1, at = 1:4, tcl = 0, cex.axis = 0.7, padj = 0.5,
     labels = c("informed by\nknown ranks", 
                "informed by\noptimized start values\n(200 runs)", 
                "informed by\noptimized start values\n(2000 runs)", 
                "uninformed"))
axis(2, las = 1)
title(ylab = "Spearman correlation with true ranks")
box()

## ---------------------------------------------------------------------------------------------------------------------
res2 <- elo.seq(winner = xdata$winner, loser = xdata$loser, Date = xdata$Date, presence = xpres, 
               draw = xdata$Draw)
stab_elo(res2, from = "2000-05-05", to = "2000-06-05")

## ---------------------------------------------------------------------------------------------------------------------
traj_elo(res2, ID = c("s", "f", "n", "z"), from = "2000-05-05", to = "2000-06-05")
traj_elo(res2, ID = c("s", "f", "n", "z"), from = "2000-06-05", to = "2000-07-05")

## ---------------------------------------------------------------------------------------------------------------------
individuals(res2, from = "2000-05-05", to = "2000-05-05", outp = "N")
individuals(res2, from = "2000-05-05", to = "2000-06-05", outp = "N")
individuals(res2, from = "2000-05-05", to = "2000-06-05", outp = "CV")
individuals(res2, from = "2000-05-05", to = "2000-06-05", outp = "IDs")

## ---------------------------------------------------------------------------------------------------------------------
winprob(1000, 1200)
winprob(1000, 1200, normprob = FALSE)
winprob(1200, 1000)
winprob(1200, 1000, normprob = FALSE)
winprob(1200, 1200)
winprob(1200, 1200, normprob = FALSE)

## ---- eval=TRUE, echo=FALSE, fig.width=7, fig.height=6, out.width="4in", fig.align='center', fig.cap="\\small Three ways of calculating winning probabilities. Up to a rating difference of 200 points, the two curves proposed by Elo 1978 (red and gold) are virtually indistinguishable. The step-wise curves are taken from Elo 1978 and Albers and de Vries 2001, which provide tables in intervals, for example, the winning probability is 0.5 if the rating difference is between 0 and 3. The curve for the algorithm used by Feldblum and colleagues and Farine and colleagues, in contrast, is much steeper (grey line). Code to produce the figure is in the \\nameref{sec:appendix}. \\label{fig:differentprobs}"----
par(family = "serif")
elotable <- list(0:3, 4:10, 11:17, 18:24, 25:31, 32:38, 39:45, 46:52, 53:59, 60:66, 67:74, 75:81, 82:88, 89:96, 97:103, 104:111, 112:119, 120:127, 128:135, 136:143, 144:151, 152:159, 160:168, 169:177, 178:186, 187:195, 196:205, 206:214, 215:224, 225:235, 236:246, 247:257, 258:269, 270:281, 282:294, 295:308, 309:323, 324:338, 339:354, 355:372, 373:391, 392:412, 413:436, 437:463, 464:494, 495: 530, 531:576, 577:636, 637:726, 727:920, 921:1000)
alberstable <- list(0:3, 4:10, 11:17, 18:25, 26:32, 33:39, 40:46, 47:53, 54:61, 62:68, 69:76, 77:83, 84:91, 92:98, 99:106, 107:113, 114:121, 122:129, 130:137, 138:145, 146:153, 154:162, 163:170, 171:179, 180:188, 189:197, 198:206, 207:215, 216:225, 226:235, 236:245, 246:256, 257:267, 268:278, 279:290, 291:302, 303:315, 316:328, 329:344, 345:357, 358:374, 375:391, 392:411, 412:432, 433:456, 457:484, 485:517, 518:559, 560:619, 620:735, 736:1000)

elotable <- data.frame(rtgdiff=unlist(elotable), P = rep(seq(0.5, 1, by=0.01), unlist(lapply(elotable, length))))
alberstable <- data.frame(rtgdiff=unlist(alberstable), P = rep(seq(0.5, 1, by=0.01), unlist(lapply(alberstable, length))))

w <- rep(0, 1001) # winner rating: constant
l <- w - 0:1000 # loser rating: varying

elonorm <- numeric(length(w))
eloexpo <- numeric(length(w))
eloopti <- numeric(length(w))

i=100
for(i in 1:length(w)) {
  elonorm[i] <- winprob(w[i], l[i], normprob = TRUE)
  eloexpo[i] <- winprob(w[i], l[i], normprob = FALSE)
  # EloOptimized package (same as aniDom with default parameters)
  # eloopti[i] <- 1/(1 + exp(-0.01 * (w[i] - l[i])))
  eloopti[i] <- winprob(w[i], l[i], normprob = FALSE, fac = 0.01)
}

plot(0, 0, "n", xlim = c(0, 1000), ylim = c(0.5, 1), xlab = "rating difference", ylab = "winning probability", las = 1, yaxs = "i")
points(abs(l), elonorm, "l", col = "red") 
points(abs(l), eloexpo, "l", col = "gold")
points(abs(l), eloopti, "l", col = "grey")

points(alberstable$rtgdiff, alberstable$P, type="l", col="red")
points(elotable$rtgdiff, elotable$P, type="l", col="gold")
legend("bottomright", legend = c("normal", "exponential", "exponential (alternative)"), col = c("red", "gold", "grey"), lwd = 2, cex = 0.9)

## ----differentprobssimu, cache=TRUE, echo=FALSE, fig.width=7, fig.height=4, out.width="70%", fig.cap = "Ratings from individuals that were based on either normally distributed winning probabilities or exponentially distributed winning probabilities. Code for the simulation and figure is in the \\nameref{sec:appendix}. \\label{fig:differentprobssimu}"----
set.seed(123)
n <- 100
xres <- data.frame(nid = sample(8:26, n, TRUE), 
                   r1 = NA, r2 = NA, 
                   k = sample(50:300, n, TRUE))

for (i in 1:length(xres$nid)) {
  xd <- randomsequence(nID = xres$nid[i], avgIA = sample(3:100, 1), 
                       reversals = runif(1, 0, 0.4))$seqdat
  allids <- letters[1:xres$nid[i]]
  w <- allids[xd$winner]
  l <- allids[xd$loser]
  kvals <- rep(res$k[i], length(w))
  svals <- rep(1000, length(allids))
  xres1 <- fastelo(w, l, allids, kvals, svals, NORMPROB = TRUE)
  xres2 <- fastelo(w, l, allids, kvals, svals, NORMPROB = FALSE)
  xind <- sample(1:xres$nid[i], 1)
  xres$r1[i] <- xres1[[1]][xind]
  xres$r2[i] <- xres2[[1]][xind]
}
  
plot(xres$r1, xres$r2, xlab = "normal", ylab = "exponential", las = 1)
abline(0, 1)

## ---------------------------------------------------------------------------------------------------------------------
creatematrix(res2)
sum(creatematrix(res2))
creatematrix(res2, drawmethod = "0.5")
sum(creatematrix(res2, drawmethod = "0.5"))
# "c" and "n" are omitted
creatematrix(res2, daterange = c("2000-06-10", "2000-06-16"))
creatematrix(res2, daterange = c("2000-06-10", "2000-06-16"), onlyinteracting = TRUE)

## ---------------------------------------------------------------------------------------------------------------------
creatematrix(winners = xdata$winner, losers = xdata$loser)

## ---- eval=FALSE------------------------------------------------------------------------------------------------------
#  creatematrix(xdata$winner, xdata$loser)

## ---------------------------------------------------------------------------------------------------------------------
rdata <- randomsequence()
xres <- elo.seq(winner = rdata$seqdat$winner, loser = rdata$seqdat$loser, Date = rdata$seqdat$Date, 
                presence = rdata$pres)
summary(xres)

## ---------------------------------------------------------------------------------------------------------------------
data(bonobos)
xdata <- randomelo(bonobos, runs = 5)
res <- data.frame(ID = colnames(xdata[[1]]), avg = round(colMeans(xdata[[1]]), 1))

## ---- fig.width=6, fig.height=4.3, out.width="4in", fig.align='center', fig.cap="David's scores and average randomized Elo-ratings from seven bonobos (data from de Vries et al 2006).\\label{fig:five}"----
ds <- DS(bonobos)
ds <- ds[order(ds$ID), ]
plot(ds$normDS, res$avg, xlab = "David's score", ylab = "randomized average Elo-rating", las = 1,
     xlim=c(0, 6))

## ---------------------------------------------------------------------------------------------------------------------
data(adv); data(advpres)
x <- elo.seq(winner = adv$winner, loser = adv$loser, Date = adv$Date, presence = advpres)
prunk(x, daterange = c("2010-01-01", "2010-01-15"))
mat <- creatematrix(x, daterange = c("2010-01-01", "2010-01-15"))
prunk(mat)

## ---------------------------------------------------------------------------------------------------------------------
data(bonobos)
DS(bonobos, prop = "Dij")

## ---------------------------------------------------------------------------------------------------------------------
steepness(bonobos, nrand = 1000)

## ---- fig.width=6, fig.height=4.3, out.width="4in", fig.align='center', fig.cap="Relationship between sparseness and steepness (data from 100 random matrices). \\label{fig:steepness}"----
plot(0, 0, "n", xlab = "sparseness", ylab = "steepness", las = 1, xlim = c(0, 1), ylim = c(0, 1))
for(i in 1:100) {
  x <- randomsequence(nID = 15, avgIA = 40)
  xmat <- creatematrix(winners = x$seqdat$winner, losers = x$seqdat$loser)
  # remove a random number of cells (replace by 0)
  xmat[sample(1:225, sample(0:200, 1))] <- 0
  # calculate and plot sparseness and steepness
  points(prunk(xmat)[1], steepness(xmat)[1])
}

## ---------------------------------------------------------------------------------------------------------------------
DCindex(devries98)
DCindex(bonobos)

## ---------------------------------------------------------------------------------------------------------------------
mat <- creatematrix(winners = adv$winner, losers = adv$loser)
h.index(mat, loops = 1000)

## ---------------------------------------------------------------------------------------------------------------------
data(bonobos)
h.index(bonobos, loops = 1000)

## ---------------------------------------------------------------------------------------------------------------------
data(devries98)
set.seed(123)
transitivity(devries98, runs = 1000)

## ---------------------------------------------------------------------------------------------------------------------
data(adv)
mat <- creatematrix(winners = adv$winner, losers = adv$loser)
set.seed(123)
transitivity(mat, runs = 1000)

## ---------------------------------------------------------------------------------------------------------------------
data(devries98)
set.seed(123)
h.index(devries98)
ISI(devries98)

## ---------------------------------------------------------------------------------------------------------------------
data(adv)
mat <- creatematrix(winners = adv$winner, losers = adv$loser)
h.index(mat)
set.seed(123)
res <- ISI(mat)

## ---------------------------------------------------------------------------------------------------------------------
ISIranks(res, sortbyID = TRUE)

## ---------------------------------------------------------------------------------------------------------------------
data(adv); data(advpres)
SEQ <- elo.seq(winner = adv$winner, loser = adv$loser, Date = adv$Date, presence = advpres)
ratings <- SEQ$cmat
head(ratings)

## ---------------------------------------------------------------------------------------------------------------------
dates <- SEQ$truedates
head(dates)

## ---- fig.width=4, fig.height=3.3, fig.cap='\\small Elo-ratings of 7 individuals across one month. \\label{fig:adv1}'----
plot(0, 0, xlim = range(dates), ylim = range(ratings, na.rm = T), axes = FALSE, xlab = "Date", 
     ylab = "Elo-ratings")
for(i in 1:ncol(ratings)) points(dates, ratings[, i], type = "l")
axis.Date(1, x = dates, cex.axis = 0.8)
axis(2, las = 1, cex.axis = 0.8)
box()

## ---- fig.width=4, fig.height=3.3, fig.cap='\\small Elo-ratings of 7 individuals across one month. Individuals are coded by color and line type. \\label{fig:adv2}'----
plot(0, 0, xlim = range(dates), ylim = range(ratings, na.rm = T), axes = FALSE, xlab = "Date", 
     ylab = "Elo-ratings")
mycols <- c("red", "green", "blue", "gold", "black", "grey", "darkred")
myltys <- c(1, 2, 3, 1, 2, 3, 1)
for(i in 1:ncol(ratings)) points(dates, ratings[, i], type = "l", col = mycols[i], lty = myltys[i])
axis.Date(1, x = dates, cex.axis = 0.8)
axis(2, las = 1, cex.axis = 0.8)
box()

## ---- fig.width=5, fig.height=3.3, fig.cap='\\small Elo-ratings of 7 individuals across one month. Individuals are coded by color and line type, which are noted in the legend. \\label{fig:adv3}'----
layout(matrix(c(1, 2), ncol = 2), heights = c(5, 5), widths = c(4, 1))
plot(0, 0, xlim = range(dates), ylim = range(ratings, na.rm = T), axes = FALSE, xlab = "Date", 
     ylab = "Elo-ratings")
mycols <- c("red", "green", "blue", "gold", "black", "grey", "darkred")
myltys <- c(1, 2, 3, 1, 2, 3, 1)
for(i in 1:ncol(ratings)) points(dates, ratings[, i], type = "l", col = mycols[i], lty = myltys[i])
axis.Date(1, x = dates, cex.axis = 0.8)
axis(2, las = 1, cex.axis = 0.8)
box()
# set margins for legend plot
par(mar = c(5, 0.5, 3.8, 0.5))
plot(1:2, 1:2, xaxt = "n", yaxt = "n", type = "n", bty = "n", ylab = "", xlab = "")
legend(x = 1, y = 2.04, colnames(ratings), cex = 0.8, bty = "n", pch = 16, pt.cex = 1, col = mycols, 
       lty = myltys)

## ---- fig.width=5, fig.height=3.3, fig.cap='\\small Elo-ratings of 7 individuals across one month. Individuals are coded by color and line type, which are noted in the legend. For each individual, ratings after every third interaction are highlighted. \\label{fig:adv4}'----
ias <- apply(SEQ$nmat, 2, cumsum)
stp <- 3
layout(matrix(c(1, 2), ncol = 2), heights = c(5, 5), widths = c(4, 1))
plot(0, 0, xlim = range(dates), ylim = range(ratings, na.rm = T), axes = FALSE, xlab = "Date", ylab = "Elo-ratings")
mycols <- c("red", "green", "blue", "gold", "black", "grey", "darkred")
myltys <- c(1, 2, 3, 1, 2, 3, 1)
mysymbs <- c(15:21)
for(i in 1:ncol(ratings)) {
  points(dates, ratings[, i], type = "l", col = mycols[i], lty = myltys[i])
  pos <- sapply(unique(ias[, i] %/% stp),
                function(X)min(which(ias[, i] %/% stp == X))
                )[-1]
  points(dates[pos], ratings[pos, i], pch = mysymbs[i], col = mycols[i])
}
axis.Date(1, x = dates, cex.axis = 0.8)
axis(2, las = 1, cex.axis = 0.8)
box()
par(mar = c(5, 0.5, 3.8, 0.5))
plot(1:2, 1:2, xaxt = "n", yaxt = "n", type = "n", bty = "n", ylab = "", xlab = "")
legend(1, 2.04, colnames(ratings), cex = 0.8, bty="n", pch = mysymbs, pt.cex = 1, col = mycols, lty = myltys)

## ---- eval=FALSE------------------------------------------------------------------------------------------------------
#  # run model (note that for simplicity this is a GLM and not a mixed model)
#  mod <- glm(parasites ~ elo, data = parasites, family = "poisson")
#  # calculate predicted values
#  pdata <- data.frame(elo = seq(min(parasites$elo), max(parasites$elo), length.out = 51))
#  pdata$par <- predict(mod, newdata = pdata, type = "r")
#  # add some colour
#  xcols <- rainbow(n = 9, alpha = 0.5)[parasites$id]
#  # plot and draw model on top
#  plot(parasites$elo, parasites$parasites, pch = 16, col = xcols,
#       xlab = "Elo rating", ylab = "parasite count", las = 1)
#  points(pdata$elo, pdata$par, type = "l")

## ---- eval=FALSE------------------------------------------------------------------------------------------------------
#  plot(0, 0, "n", xlim = c(1,10), ylim = c(500, 1500), xlab = "prior ordinal rank",
#       ylab = "custom startvalue", cex.axis = 0.8, cex.lab = 0.8, las = 1)
#  shapes <- c(0, 0.1, 0.3, 0.5, 1)
#  xcols <- c("black", "grey", "red", "yellow", "blue")
#  for(i in 1:length(shapes)) {
#    points(myranks, createstartvalues(ranks = myranks, shape = shapes[i])$res, type = "l",
#           col = xcols[i], lwd = 2)
#  }
#  legend("topright", lty = 1, col = xcols, legend = shapes, lwd = 2, title = "shape", bty = "n",
#         cex = 0.7)

## ---- eval=FALSE------------------------------------------------------------------------------------------------------
#  par(mfrow = c(1, 3))
#  
#  dates <- res1$truedates
#  mycols <- c("red", "blue", "gold", "black", "grey", "green", "darkred")
#  
#  
#  ratings1 <- res1$cmat
#  ratings1 <- ratings1[, sort(colnames(ratings1))]
#  plot(0, 0, type = "n", xlim = range(dates), ylim = c(600, 1400), axes = FALSE, xlab = "Date",
#       ylab = "Elo-ratings", cex.lab = 0.7)
#  for(i in 1:ncol(ratings1)) points(dates, ratings1[, i], type = "l", col = mycols[i])
#  axis.Date(1, x = dates, cex.axis = 0.7)
#  axis(2, las = 1, cex.axis = 0.7)
#  box()
#  
#  ratings2 <- res2$cmat
#  ratings2 <- ratings2[, sort(colnames(ratings2))]
#  plot(0, 0, type = "n", xlim = range(dates), ylim = c(600, 1400), axes = FALSE, xlab = "Date",
#       ylab = "Elo-ratings", cex.lab = 0.7)
#  for(i in 1:ncol(ratings2)) points(dates, ratings2[, i], type = "l", col = mycols[i])
#  axis.Date(1, x = dates, cex.axis = 0.7)
#  axis(2, las = 1, cex.axis = 0.7)
#  box()
#  
#  
#  plot(0, 0, type = "n", xlim = range(dates), ylim = c(0, 1), axes = FALSE, xlab = "Date",
#       ylab = "correlation coefficient", cex.lab = 0.7)
#  for(i in 1:nrow(ratings1)) points(dates[i], cor(ratings1[i, ], ratings2[i, ]), pch = 16)
#  axis.Date(1, x = dates, cex.axis = 0.7)
#  axis(2, las = 1, cex.axis = 0.7)
#  box()

## ---- eval=FALSE------------------------------------------------------------------------------------------------------
#  xdata <- read.table(system.file("ex-sequence.txt", package = 'EloRating'), header = TRUE)
#  # no prior knowledge
#  s1 <- elo.seq(winner = xdata$winner, loser = xdata$loser, Date = xdata$Date)
#  
#  # use the above calculated ratings as known 'ranks'
#  myranks <- 1:length(extract_elo(s1))
#  names(myranks) <- names(extract_elo(s1))
#  mystart <- createstartvalues(myranks, startvalue = 1000, k = 100)
#  s2 <- elo.seq(winner = xdata$winner, loser = xdata$loser, Date = xdata$Date,
#                 startvalue = mystart$res)
#  
#  # reverse
#  myranks[1:10] <- 10:1
#  mystart <- createstartvalues(myranks, startvalue = 1000, k = 100)
#  s3 <- elo.seq(winner = xdata$winner, loser = xdata$loser, Date = xdata$Date,
#                 startvalue = mystart$res)
#  
#  par(mfrow = c(1, 3))
#  
#  dates <- s1$truedates
#  mycols <- c("red", "blue", "gold", "black", "grey", "green", "darkred", "darkblue", "pink", "cyan")
#  
#  # do the plots
#  ratings1 <- s1$cmat
#  ratings1 <- ratings1[, sort(colnames(ratings1))]
#  plot(0, 0, type = "n", xlim = range(dates), ylim = c(400, 1600), axes = FALSE, xlab = "Date",
#       ylab = "Elo-ratings", cex.lab = 0.7)
#  for(i in 1:ncol(ratings1)) points(dates, ratings1[, i], type = "l", col = mycols[i])
#  axis.Date(1, x = dates, cex.axis = 0.7)
#  axis(2, las = 1, cex.axis = 0.7)
#  box()
#  
#  ratings2 <- s2$cmat
#  ratings2 <- ratings2[, sort(colnames(ratings2))]
#  plot(0, 0, type = "n", xlim = range(dates), ylim = c(400, 1600), axes = FALSE, xlab = "Date",
#       ylab = "Elo-ratings", cex.lab = 0.7)
#  for(i in 1:ncol(ratings2)) points(dates, ratings2[, i], type = "l", col = mycols[i])
#  axis.Date(1, x = dates, cex.axis = 0.7)
#  axis(2, las = 1, cex.axis = 0.7)
#  box()
#  
#  ratings3 <- s3$cmat
#  ratings3 <- ratings3[, sort(colnames(ratings3))]
#  plot(0, 0, type = "n", xlim = range(dates), ylim = c(400, 1600), axes = FALSE, xlab = "Date",
#       ylab = "Elo-ratings", cex.lab = 0.7)
#  for(i in 1:ncol(ratings3)) points(dates, ratings3[, i], type = "l", col = mycols[i])
#  axis.Date(1, x = dates, cex.axis = 0.7)
#  axis(2, las = 1, cex.axis = 0.7)
#  box()

## ---- eval = FALSE----------------------------------------------------------------------------------------------------
#  plot(0, 0, type = "n", xlim = c(0, 500), ylim = c(-220, -100), las = 1, xlab = bquote(italic(k)),
#       ylab = "log likelihood", yaxs = "i")
#  points(ores$complete$k, ores$complete$loglik, type = "l", col = "blue", lwd = 2)
#  arrows(x0 = ores$best$k, y0 = -230, x1 = ores$best$k, y1 = -220, col = "blue", lwd = 2, xpd = TRUE, length = 0.1)
#  
#  pdata <- ores2$complete[ores2$complete$fight == ores2$best$fight, ]
#  points(pdata$threat, pdata$loglik, type = "l", col = "red")
#  arrows(x0 = ores2$best$threat, y0 = -230, x1 = ores2$best$threat, y1 = -220, col = "red", lwd = 2, xpd = TRUE, length = 0.1)
#  
#  pdata <- ores2$complete[ores2$complete$fight == 10, ]
#  points(pdata$threat, pdata$loglik, type = "l", col = "red", lty = 3)
#  x <- pdata$threat[which.max(pdata$loglik)]
#  arrows(x0 = x, y0 = -230, x1 = x, y1 = -220, col = "red", lwd = 2, xpd = TRUE, length = 0.1, lty = 3)
#  
#  pdata <- ores2$complete[ores2$complete$threat == ores2$best$threat, ]
#  points(pdata$fight, pdata$loglik, type = "l", col = "grey")
#  arrows(x0 = ores2$best$fight, y0 = -230, x1 = ores2$best$fight, y1 = -220, col = "grey", lwd = 2, xpd = TRUE, length = 0.1)
#  
#  pdata <- ores2$complete[ores2$complete$threat == 500, ]
#  points(pdata$fight, pdata$loglik, type = "l", col = "grey", lty = 3)
#  x <- pdata$fight[which.max(pdata$loglik)]
#  arrows(x0 = x, y0 = -230, x1 = x, y1 = -220, col = "grey", lwd = 2, xpd = TRUE, length = 0.1, lty = 3)
#  
#  legend("bottom", legend = c("one interaction type", "threat (fight fixed)", "fight (threat fixed)"),
#         col = c("blue", "red", "grey"), lty = 1, cex = 0.7)

## ---- eval=FALSE------------------------------------------------------------------------------------------------------
#  elotable <- list(0:3, 4:10, 11:17, 18:24, 25:31, 32:38, 39:45, 46:52, 53:59, 60:66, 67:74, 75:81, 82:88, 89:96,
#                   97:103, 104:111, 112:119, 120:127, 128:135, 136:143, 144:151, 152:159, 160:168, 169:177,
#                   178:186, 187:195, 196:205, 206:214, 215:224, 225:235, 236:246, 247:257, 258:269, 270:281,
#                   282:294, 295:308, 309:323, 324:338, 339:354, 355:372, 373:391, 392:412, 413:436, 437:463,
#                   464:494, 495: 530, 531:576, 577:636, 637:726, 727:920, 921:1000)
#  alberstable <- list(0:3, 4:10, 11:17, 18:25, 26:32, 33:39, 40:46, 47:53, 54:61, 62:68, 69:76, 77:83, 84:91,
#                      92:98, 99:106, 107:113, 114:121, 122:129, 130:137, 138:145, 146:153, 154:162, 163:170,
#                      171:179, 180:188, 189:197, 198:206, 207:215, 216:225, 226:235, 236:245, 246:256, 257:267,
#                      268:278, 279:290, 291:302, 303:315, 316:328, 329:344, 345:357, 358:374, 375:391, 392:411,
#                      412:432, 433:456, 457:484, 485:517, 518:559, 560:619, 620:735, 736:1000)
#  
#  elotable <- data.frame(rtgdiff = unlist(elotable),
#                         P = rep(seq(0.5, 1, by = 0.01), unlist(lapply(elotable, length))))
#  alberstable <- data.frame(rtgdiff = unlist(alberstable),
#                            P = rep(seq(0.5, 1, by = 0.01), unlist(lapply(alberstable, length))))
#  
#  w <- rep(0, 1001) # winner rating: constant
#  l <- w - 0:1000 # loser rating: varying
#  
#  elonorm <- numeric(length(w))
#  eloexpo <- numeric(length(w))
#  eloopti <- numeric(length(w))
#  
#  for(i in 1:length(w)) {
#    elonorm[i] <- winprob(w[i], l[i], normprob = TRUE)
#    eloexpo[i] <- winprob(w[i], l[i], normprob = FALSE)
#    eloopti[i] <- winprob(w[i], l[i], normprob = FALSE, fac = 0.01)
#  }
#  
#  plot(0, 0, type = "n", las = 1, yaxs = "i",
#       xlim = c(0, 1000), ylim = c(0.5, 1),
#       xlab = "rating difference",
#       ylab = "winning probability")
#  points(abs(l), elonorm, "l", col = "red")
#  points(abs(l), eloexpo, "l", col = "gold")
#  points(abs(l), eloopti, "l", col = "grey")
#  
#  points(alberstable$rtgdiff, alberstable$P, type="l", col="red")
#  points(elotable$rtgdiff, elotable$P, type="l", col="gold")
#  legend("bottomright",
#         legend = c("normal", "exponential", "exponential (alternative)"),
#         col = c("red", "gold", "grey"),
#         lwd = 2,
#         cex = 0.9)

## ---- eval=FALSE------------------------------------------------------------------------------------------------------
#  set.seed(123)
#  n <- 100
#  xres <- data.frame(nid = sample(8:26, n, TRUE),
#                     r1 = NA, r2 = NA,
#                     k = sample(50:300, n, TRUE))
#  
#  for (i in 1:length(xres$nid)) {
#    xd <- randomsequence(nID = xres$nid[i], avgIA = sample(3:100, 1),
#                         reversals = runif(1, 0, 0.4))$seqdat
#    allids <- letters[1:xres$nid[i]]
#    w <- allids[xd$winner]
#    l <- allids[xd$loser]
#    kvals <- rep(res$k[i], length(w))
#    svals <- rep(1000, length(allids))
#    xres1 <- fastelo(w, l, allids, kvals, svals, NORMPROB = TRUE)
#    xres2 <- fastelo(w, l, allids, kvals, svals, NORMPROB = FALSE)
#    xind <- sample(1:xres$nid[i], 1)
#    xres$r1[i] <- xres1[[1]][xind]
#    xres$r2[i] <- xres2[[1]][xind]
#  }
#  
#  plot(xres$r1, xres$r2, xlab = "normal", ylab = "exponential", las = 1)
#  abline(0, 1)

