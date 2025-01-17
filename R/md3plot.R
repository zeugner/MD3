

#' Plot md3 time series
#'
#' Plotting method for objects inheriting from class \code{md3}. Converts md3 to time series of class \code{ts} and applies \code{\link[stats]{plot.ts}}
#' @param x,y an md3 object
#' @param plot.type either 'single' or 'multiple'. Also wiorks by partial matching ('s' or 'm')
#' @param ... arguments passed on to \code{\link[stats]{plot.ts}}
#' @param plotflags boolean. If TRUE then the charts indicats any flags (obsrvation status) on top of the data
#' @return NULL
#' @seealso \code{\link[stats]{plot.ts}} for the underlying method, \code{\link{barplot.md3}}
#' @examples
#' data(euca)
#' plot(euca[PC_GDP.SE.:2010])
#' plot(euca[PC_GDP.AT+SE.])
#' plot(euca[PC_GDP.AT+SE.],plot.type="single",col=1:2)
#' plot(euca[PC_GDP.LT.],euca[PC_GDP.LV.], xlab="Lithuania",ylab="Latvia")
#' lines(euca[PC_GDP.FR.],lwd=2,col='blue')
#'
#' #barplot contribution of major countries to euro area current account
#' barplot(euca[MIO_NAC.DE+FR+IT+ES.])
#' @export
plot.md3 = function (x, y = NULL, plot.type = c("single", "multiple"),
        xlab = NULL, ylab = NULL, ..., plotflags=FALSE)
{
  if (missing(xlab)) {
    xlab = ""
  }
  plot.type = c("single", "multiple")[pmatch(plot.type[[1]],
                                             c("single", "multiple"))]
  if (!is.null(y))
    getS3method("plot", "ts")(as.ts.md3(x), as.ts(y),
                              xlab = xlab, ylab = ylab, ...)
  else yy = as.ts.md3(x)
  if (missing(ylab)) {
    ylab = paste(colnames(yy), collapse = "+")
  }
  if (NCOL(yy) > 6L) {
    message("Many lines in one plot. Consider using plot.type=\"multiple\"")
  }
  getS3method("plot", "ts")(yy, plot.type = plot.type,
                            xlab = xlab, ylab = ylab, ...)
  if (!plotflags || is.null(y)) { return(invisible(NULL))}
  dd=.dt_class(x)
  warnings('plotflags not implemnted yet')
  return(invisible(NULL))

}



#' @rdname plot.md3
#' @export
lines.md3 = function(x, y=NULL,...) {
  invisible(lapply(as.list(as.ts.md3(x)),getS3method("lines","ts"),col=4))
}



#' Plot md3 time series
#'
#' Plotting method for objects inheriting from class \code{md3}. Converts md3 to time series of class \code{ts} and applies \code{\link[graphics]{barplot}}
#' @param height an md3 object, zoo, or matrix
#' @param ... arguments passed on to \code{\link[graphics]{barplot}}
#' @param main title
#' @param col if left empty, then a grey colour palette
#' @param legend if TRUE, plots a legend for bars below the plot area
#' @param legend.text custom text for legend
#' @param legend.cex cex paramter for legend
#' @return a vector with mid-points for bars. USe for additional plotting
#' @seealso \code{\link[stats]{plot.ts}} for the underlying method, \code{\link[stats]{sd}}
#' @examples
#' data(euca)
#' barplot(euca[PC_GDP.SE.:2010])
#'
#' #barplot contribution of major countries to euro area current account
#' temp=barplot(euca[MIO_NAC.DE+FR+IT+ES+NL.]/1000, ylim=c(-100,500), ylab='bn EUR')
#' eaca=apply(euca['MIO_NAC',cgrp(EA),],2,sum,na.rm=TRUE)/1000
#' lines(temp,eaca,col='red',lwd=2) # adding a line with EA CA
#'
#' @export
barplot.md3 = function (height, ..., main = NULL, col = NULL, legend = TRUE,
          legend.text = NULL, legend.cex = 0.7, ylim = NULL)
{
  if (.md3_is(height))
    height = t(as.zoo.md3(height))
  indots = .dotsaslist(...)
  matplus = height
  matplus[height < 0] = 0
  matminus = height
  matminus[height > 0] = 0
  matplus[is.na(matplus)] = 0
  matminus[is.na(matminus)] = 0
  if (is.null(legend.text)) {
    legend.text = rownames(matplus)
  }
  else {
    legend.text = as.character(legend.text)
  }
  if (is.null(col)) {
    cols = gray.colors(nrow(matplus))
  }
  else {
    cols = col
  }
  if (is.null(ylim)) {
    ylim = c(min(colSums(matminus, na.rm = TRUE)), max(colSums(matplus,
                                                               na.rm = TRUE)))
  }
  df.bar = graphics::barplot.default(matplus, ylim = ylim,
                                     beside = FALSE, main = main, col = cols, ...)
  olist = graphics::barplot.default(matminus, beside = FALSE,
                                    add = TRUE, col = cols, ...)
  if (!legend) {
    return(invisible(df.bar))
  }
  for (i in 1:3) {
    ix = 1:(ncol(height)%/%2) + (ncol(height)%/%2) * (i -
                                                        1)
    ix = ix[!(ix > nrow(height))]
    if (length(rownames(matplus)[ix])) {
      legend(ifelse(i == 1, "bottomleft", ifelse(i ==
                                                   2, "bottom", "bottomright")), legend.text[ix],
             cex = legend.cex, bty = "n", fill = cols,
             inset = c(par()$plt[1] * (i - 2), -par()$plt[3]/diff(par()$plt[3:4])),
             xpd = TRUE)
    }
  }
  return(invisible(df.bar))
}
