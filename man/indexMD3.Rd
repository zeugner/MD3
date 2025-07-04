% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/md3object.R
\name{indexMD3}
\alias{indexMD3}
\alias{[.md3}
\title{Extract or replace parts of an md3 object}
\usage{
\method{[}{md3}(
  x,
  ...,
  drop = TRUE,
  as = c("md3", "array", "numeric", "data.table"),
  .obs = "value",
  DT = FALSE
)
}
\arguments{
\item{x}{an md3 object}

\item{...}{indexes, see 'Details' below}

\item{drop}{whether to drop singleton dimensions (see \code{\link{drop.md3}})}

\item{as}{how to return the result, see also \code{\link{as.data.table.md3}}}

\item{.obs}{used when extracting or replacing not the observation values, but observation attributes such as flags (see below)}

\item{onlyna}{only update those elements that are NA}

\item{justval}{only update those elements that are not NA}

\item{usenames}{when replacing parts of x, update x by using the dimension codes, rather than the order, of elements in value}
}
\value{
an md3, array, numeric, zoo, or data.table, data.frame depending on parameter \code{as}
}
\description{
Extract or replace parts of an md3 object
}
\section{Internal structure}{

an md3 actually is a data.table that contains one column for each dimension,
and at least one column for observations (there can be additional columns for flags and confidentiality labels).
You can see that  by running e.g. \code{as.data.table(euhpq)}

or in an SDMX API-like notation (\code{euhpq["TOTAL.I15_Q.BE+FR.2021q1:"]} resp \code{euhpq["TOTAL..SI.2021q1:"]}),
or a mix thereof (\code{euhpq[3,2,"BE+FR","2021q1:"]})
}

\examples{
#data(euhpq) #house prices for EU countries

#Retrieving values
#Austrian & Slovak HP growth year-on-year
#these five commands are equivalent:
euhpq["TOTAL","RCH_A",c("AT","SK"),]
euhpq["TOTAL.RCH_A.AT+SK."]
euhpq[3,3,c(1, 35),]
euhpq[3,3,c("AT", "SK"),]
euhpq[3,3,"AT+SK",]

#Using + and :

euhpq[TOTAL..FR+AT.y2011q4] #time periods should be prefixed by 'y'
euhpq[TOTAL..FR.y2011q1:y2017q4]
euhpq[TOTAL..FR.2011q2:2017] #but dropping the prefix 'y'is mostly possible
#note the way  2017 is used to encompass all periods of that year.
euhpq[TOTAL.RCH_A.AT+SK.y2020q1:y]
euhpq[TOTAL.RCH_A.FR:PL.y2022q1+y2023q1]

#Time periods: the following are equivalent:
euhpq['TOTAL.I15_Q.FR+NL.:2007q4']
euhpq[TOTAL.I15_Q.FR+NL.y:y2007q4]
euhpq["TOTAL.I15_Q.FR+NL.y:y2007"]
euhpq[3,2,c("FR","NL"),1:12]


#Setting values:
# Slovak and Austrian house price growth for 2005-2006
#these four commands are equivalent:
euhpq[.RCH_A.AT+SK.:2006]=0
euhpq[.RCH_A.AT+SK.y:y2006]=0
euhpq[".RCH_A.AT+SK.:2006"] <- 0
euhpq[,"RCH_A",c(1,33),1:8]=0

# See result of value assignment
euhpq[.RCH_A.SK.]

#Adding elements:
euhpq[,,"Dummy",]=0 #add a dummy country
euhpq[2,'I15_Q',,"2011"] #see effect


#Note that calling an element that does not exist will return NA,
#similar to data.frames with rownames
mm=euhpq[1,1,1:4,'2022']
testmat=as.data.frame(as.matrix(mm))
testmat['AT',] #has data
mm['AT',,drop=FALSE]
testmat['CH',] #has empty data
mm['CH',]
testmat[c('GG','FF'),]
mm[c('GG','FF'),]

#Argument usenames, onlyna and justval still to be described
}
\seealso{
\code{\link{dimcodes}}
}
