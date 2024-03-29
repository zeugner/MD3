% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/md3object.R
\name{drop}
\alias{drop}
\title{Dropping dimensions}
\usage{
drop(x, ...)
}
\arguments{
\item{x}{an array or md3 object with n dimensions}

\item{\dots}{other arguments}
}
\value{
an array or md3 object with n dimensions or less
}
\description{
Drops any dimensions from an array or similar object that contain only a single element
}
\examples{
#data(euhpq)
ww=euhpq[TOTAL.I15_Q..,drop=FALSE]
dim(ww)# dimensions
w2=drop(ww)
dim(w2)
}
\seealso{
\code{\link{drop.md3}}
}
