% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/md3object.R, R/md3utilities.R
\name{as.data.table.md3}
\alias{as.data.table.md3}
\alias{as.data.frame.md3}
\alias{as.md3.zoo}
\alias{as.md3.data.frame}
\alias{asmd3}
\alias{as.md3}
\title{Functions to check if an object is md3, or coerce it if possible.}
\usage{
\method{as.data.table}{md3}(x, ..., na.rm = FALSE, sep = "_", .simple = FALSE)

\method{as.data.frame}{md3}(x, ..., na.rm = FALSE)

\method{as.md3}{zoo}(x, split = ".", name_for_cols = character(0))

\method{as.md3}{data.frame}(
  x,
  id.vars,
  name_for_cols = NULL,
  split = ".",
  obsattr = character(0),
  ...
)

as.md3(x, ...)
}
\arguments{
\item{x}{an md3 object or data.frame/data.table}

\item{...}{additional parameters: for \code{as.data.frame} this is the optional formula for dcast, for \code{as.md3} this are the parameters forwarded to specific methods, notably those for as.md3.data.frame. See 'Details' below}

\item{na.rm}{If \code{FALSE} then the resulting 'molten' data.frame omits any observations with NA values. See also \link[data.table]{melt.data.table}}

\item{sep}{see also \link[data.table]{melt.data.table}}

\item{.simple}{Use only in nerdy purposes. If this logical is TRUE, then the function assumes that the data.table provided in x already contains an attribute with dimension names}

\item{split}{if a column name pertains to more than one dimension (e.g., \code{UK.GDP}), this character is used to 'cast' into more than one dimension}

\item{name_for_cols}{this is a character vector with the names of the dimensions embedded in the data.frame}

\item{id.vars}{see also \link[data.table]{melt.data.table}}

\item{obsattr}{this is to indicate the columns that contain observation attributes. see also \code{\link{flags}}}
}
\value{
an md3 or data.table/data.frame
}
\description{
Functions to check if an object is md3, or coerce it if possible.
}
\details{
TBD
}
\section{Functions}{
\itemize{
\item \code{as.data.table(md3)}: Convert MD3 to data.table, optionally with dcast

\item \code{as.data.frame(md3)}: Convert MD3 to data.frame, optionally with dcast

\item \code{as.md3(zoo)}: Convert from zoo to an md3 object

\item \code{as.md3(data.frame)}: Convert a data.frame to an md3

}}
\examples{
#data(euhpq) #house prices for EU countries

testdt=as.data.table(euhpq[1,1,1:2,])
testmd3=as.md3(testdt)
testmd3
flags(testmd3)

as.md3(LifeCycleSavings)

as.data.table(euhpq[1,1,"PL:RO","2021:"])
as.data.table(unflag(euhpq[1,1,"PL:RO","2021:"]))
as.data.table(euhpq[1,1,"PL:RO","2021:"] , geo ~ TIME)
as.data.table(euhpq[1:2,1,"PL:RO","2021:"] , TIME ~ ...)

zz=as.zoo(euhpq[1:2,1,"PL:RO","2021:"],sep=';')
z2=as.md3(zz,split='.'); dimcodes(z2)
z3=as.md3(zz,split='.',name_for_cols=c('type','country')); dimcodes(z3)

ff=as.md3(as.data.frame(UCBAdmissions),id.vars=1:3)
dimnames(ff)

testdt2=as.data.table(euhpq[,'I15_Q',1:4,],TIME~ ..., sep=";")
testdt2
testmd2 = as.md3(testdt2,split=';')
dimcodes(testmd2)
testmd2[".AT.2021:"]

}
\seealso{
\code{\link{indexMD3}}
}
