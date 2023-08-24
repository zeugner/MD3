#' MD3: A package providing for multi-dimensional data manipulation
#'
#' The md3 object is meant to natively work with data from SDMX sources.
#' It is a hybrid between multdimensional arrays and data.table, allowing data to be addressed with SDMX-like syntax
#' In addition it provides the following functionality:
#' \itemize{
#' \item \code{Nomics} loading data from db.nomics.world
#' \item \code{imputena} and \code{interpolna}: imputing data
#' }
#'
#'
#' @section MD3 functionality:
#' The main strength of MD3 is its indexing system. Check \link{indexMD0} for an explanation
#'
#' It also provides for metadata: See \code{\link{dimcodes}} for that
#'
#' It provides for working with mixed frequencies. Check e.g. \code{aggregate.md3} (to be built later)
#'
#' It can convert to and from: \code{data.frame}, \code{array}, \code{zoo}, \code{xts}, \code{ts}
#'
#' Package \code{MDstats} uses MD0 a lot
#'
#' @section Further work:
#' MD0 does not yet harness the full power of data.table, so beyond a few million data points it is getting slow.
#'
#' In addition this packge is still under development; there are certainly some bugs lurking around
#'
#' Conversion to and from \code{data.table}, tibble, and \code{pdata.frame} is to be added
#'
#' @section Author:
#' Stefan Zeugner ECFIN.B1
#'
#' @import data.table
#' @import bit64
#' @exportClass timo
#' @exportClass timdif
#' @exportClass timord
#' @exportClass md3
#' @docType package
#' @name MD3
NULL
