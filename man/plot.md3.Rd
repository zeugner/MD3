% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/md3plot.R
\name{plot.md3}
\alias{plot.md3}
\alias{lines.md3}
\alias{points.md3}
\title{Plot md3 time series}
\usage{
\method{plot}{md3}(
  x,
  y = NULL,
  plot.type = c("single", "multiple"),
  xlab = NULL,
  ylab = NULL,
  col = 1,
  ...,
  plotflags = FALSE,
  plotlegend = TRUE
)

\method{lines}{md3}(x, y = NULL, ...)

\method{points}{md3}(x, y = NULL, ...)
}
\arguments{
\item{x, y}{an md3 object}

\item{plot.type}{either 'single' or 'multiple'. Also wiorks by partial matching ('s' or 'm')}

\item{xlab}{same as in \code{\link[stats]{plot.ts}}}

\item{ylab}{same as in \code{\link[stats]{plot.ts}}}

\item{col}{specfies color (same as in \code{\link[graphics]{par}}) default integer sequence}

\item{...}{arguments passed on to \code{\link[stats]{plot.ts}}}

\item{plotflags}{boolean. If TRUE then the charts indicats any flags (obsrvation status) on top of the data}

\item{plotlegend}{boolean. plots a legend for convenience if plot.type=='single'}
}
\description{
Plotting method for objects inheriting from class \code{md3}. Converts md3 to time series of class \code{ts} and applies \code{\link[stats]{plot.ts}}
}
\examples{

str(eupop)
plot(eupop[SE.T.TOTAL.y:1990])
plot(eupop[AT+SE.T.TOTAL.])
plot(eupop[AT+SE.T.TOTAL.],plot.type="single",col=1, plotlegend = FALSE)
plot(eupop[AT+SE.T.TOTAL.], plot.type='multiple', main= 'population')
plot(eupop[LT.T.TOTAL.],eupop[LV.T.TOTAL.], xlab="Lithuania",ylab="Latvia",col=1)

graphics.off()
plot(eupop[DE+ES.T.TOTAL.1990:y]/1000000)
lines(eupop[FR.T.TOTAL.]/1e6,lwd=2,col='blue')

#barplot contribution of major countries to EU population
barplot(eupop[DE+FR+IT+ES+PL.T.TOTAL.2000:y])
}
\seealso{
\code{\link[stats]{plot.ts}} for the underlying method, \code{\link{barplot.md3}}
}
