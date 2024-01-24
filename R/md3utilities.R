#setdiff(gsub('md0$','',methods(class = 'md0')),gsub('md3$','',methods(class = 'md3')))
#deal with f=='h'  such as xx=mds('ECB/EXR/H.ARS.EUR.SP00.A') resp xx=mds('ECB/EXR/.ARS.EUR.SP00.A')





.getas =function(x, as=c("md3", "array", "numeric","data.table","data.frame","zoo","2d","1d","pdata.frame")) {
  if (!.md3_is(x)) { stop ('needs to be md3 object!')}
  as=.asget(as)
  if (as=='1d') {as='data.table'}
  if (as=='md3') return(x)
  if (as=='array') return(.md3get(x, as = "array", drop = FALSE))
  if (as=='numeric') return(as.numeric(.md3get(x, as = "array", drop = FALSE)))
  if (as=='data.table') return(as.data.table.md3(x))
  if (as=='data.frame') return(as.data.frame.md3(x))
  if (as=='zoo') return(as.zoo.md3(x))
  if (as=='2d') {
    if (!.dn_findtime(x)) {return(.dt_class(x))}
    temp=.dt_class(x)[,c(names(attr(x,'dcstruct')),.md3resnames('value')),with=FALSE]
    return(dcast(temp,...~TIME,value.var= '_.obs_value'))
  }

  stop('Conversion to ',as,' to be done')

}


.couldbetimo= function(x) {
  #x is a list, usually a dcsimp attribute
  if (is.atomic(x)) { x =list(x)}
  which(unlist(lapply(lapply(x, function(y) class(try(.char2timo(head(y,20)),silent=TRUE))),function(z) any(z=='timo'))))
}

#' @export
as.zoo.md3 = function(x,...) {
  if (!require('zoo')) stop('requires package zoo to be installed.')
  ixt =.dn_findtime(.getdimnames(x)); if (ixt<1) stop('could not find a time dimension in X')

  ixf=unique(.timo_frq(.getdimnames(x,TRUE)[[ixt]])); if (length(ixf)!=1) stop('cannot do this with mixed frequencies')
  x=.dt_class(unflag(x));  colnames(x)[[ixt]] ='TIME'

  dxts=data.table(dcast(x,TIME~ ..., sep='.', value.var= '_.obs_value'))


  zoofnc=c('Q'=as.yearqtr.timo,'M'=as.yearmon.timo,'A'=data.table::year,'D'=as.Date.timo,'B'=as.Date.timo,'N'=as.POSIXct.timo,'H'=as.POSIXct.timo,'W'=as.Date,'S'=function(x) {(data.table::quarter(x)-1)/4+data.table::year(x)})
  y=zoo::zoo(as.matrix(dxts[,2:NCOL(dxts),with=FALSE]), frequency=.cttim$basetbl()[ixf,'frqzoo'], order.by=zoofnc[[ixf]](dxts[['TIME']]))
  #attr(y,'oclass') = 'md3'
  y

}

#' @export
as.md3.zoo = function (x, split = ".", name_for_cols = character(0))
{
  #require(zoo)
  xtime = as.timo(zoo::index(x))
  x = zoo:::as.data.frame.zoo(x, stringsAsFactors = FALSE)
  #rownames(x) = .timo2char(xtime)
  out = suppressWarnings(.df2md3(cbind(x,TIME=xtime), split = split, name_for_cols = name_for_cols))
  nn = names(attr(out, "hihi"))
  if (length(nn) > 1)
    out = aperm.md3(out, c(nn[-1], "TIME"))
  out
}

#' @export
as.md3.ts = function(x, split = ".", name_for_cols = character(0), ...) {
  as.md3.zoo(zoo:::as.zoo(x,...), split=split,name_for_cols=name_for_cols)
}

#' @export
as.ts.md3 = function(x,...,.obs = "value") {
  ixt =.dn_findtime(x); if (ixt<1) stop('could not find a time dimension in X')
  .obs=.md3resnames(.obs[1L])
  mycols=c(names(attr(x,'dcstruct')),.obs)
  ixf=unique(.timo_frq(.getdimnames(x,TRUE)[[ixt]])); if (length(ixf)!=1) stop('cannot do this with mixed frequencies')
  x=.dt_class(x); colnames(x)[[ixt]] ='TIME'
  x=x[,mycols,with=FALSE]
  dxts=dcast(x,TIME~ ..., sep='.', value.var= .obs)


  tsfrq=c(N=1140,D=1,B=1,W=1/7,.namedvecfrommat(.cttim$basetbl(),'frqzoo'))[ixf]
  if (any(names(tsfrq) %in% c('N','B','D','W'))) {
    tsstart= as.Date.timo(min(x[[ixt]]))
  } else{
    tsstart = as.numeric(strsplit(as.character.timo(min(x[[ixt]])),split='[A-z]')[[1L]])
  }
  y=stats::ts(as.matrix(dxts[,-1,with=FALSE]), frequency=tsfrq, start=tsstart)
  #attr(y,'oclass') = 'md3'
  y

}

#' @export
as.xts = function(x,...) {
  if (!require('xts')) stop('requires package xts to be installed.')
  xts::as.xts(as.ts.md3(x,...))
}


#' Applies a function to each times series of an objects
#'
#' This function is focused on applying functions from the zoo package, such as rollmean
#' @param bigmd3 an md3 object, array, or anything that may be converted to an md3
#' @param \dots a numeric, array, or md3 with which to fill out all values of bigmd3. Alternatively, the indexing reference to a subset of the md3 (see \link{indexMD3})
#' @return an md3 object (or an array if bigmd3 is an array)
#' @seealso \code{\link{aggregate.md3}}, \link{indexMD3}, \code{\link{imputena}}, \code{\link{zapply}}, \code{\link[zoo]{rollapply}}, , \code{\link{rollmean.md3}}
#' @examples
#'
#'
#' zapply(euhpq[1,1,1:4,],mean)
#'
#' zapply(euhpq[1,1,1:4,],rollmean, k=2)
#'
#' rollmean.md3(euhpq[1,1,1:4,],k=4)
#' rollsum.md3(euhpq[1,1,1:4,],k=4)
#' rollmedian.md3(euhpq[1,1,1:4,],k=3)
#'
#' @export
zapply = function (X, FUN, ..., apply2indiv=TRUE)
{
  #require(zoo)
  if (!.md3_is(X)) X=as.md3(X)
  myf=unique(.timo_frq(time(X)))
    #mdin = as.md3(X)

  zapply_perfrq = function(mdin, FUN, ...) {
    y0 = as.zoo.md3(mdin)
    FUN2 = FUN
    if (is.function(FUN2)) {
      FUN2 = deparse(substitute(FUN))

    }



    if (FUN2=="FUN") { yf=FUN} else { yf = getS3method(FUN2, "zooreg", optional = TRUE)}
    if (is.null(yf)) { yf=FUN }

    if (apply2indiv) {
      temp= apply(y0, 2, yf, ...)
      if (!is.null(dim(temp)) && all(dim(temp)==dim(y0))) { y0[, ] = temp; y1=y0 } else {
        if (length(dim(temp))==2 && is.null(rownames(temp))) { rownames(temp) = utils::tail(.timo2char(time(mdin)),NROW(temp))}
        y1=temp
      }

    } else {
      y1 = yf(y0, ...)
    }
    if ((length(dim(temp))<length(dim(y0))) || !zoo::is.zoo(y1)) {
      #y2=as.md3.numeric(y1,dimname = setdiff(names(.dim(mdin)),'TIME'))
      if (is.null(dim(y1))) {
        y2=drop.md3(as.md3.data.frame(as.list(y1), name_for_cols = setdiff(names(.dim(mdin)),'TIME')))
        attr(y2,'dcstruct') <- attr(mdin,'dcstruct')[names(.dim(y2))]
      } else {
        ff=data.table(TIME=rownames(y1),y1)
        y2=as.md3.data.table(ff,id.vars = 'TIME',split = '.',name_for_cols = setdiff(names(.dim(mdin)),'TIME'))
      }

    } else {
      y2 = as.md3.zoo(y1, name_for_cols = setdiff(names(.dim(mdin)),'TIME'))
    }


    return(y2)
  }

  if (length(myf) == 0L)
    stop("requires time series")
  if (length(myf) == 1L)
    return(zapply_perfrq(X, FUN, ...))


  emptyrest = sapply(.dim(X), function(x) "")
  for (ff in myf) {
    mysel = emptyrest
    mysel["FREQ"] = ff
    mysel = paste0(mysel, collapse = ".")
    temp = zapply_perfrq(X[mysel, drop = FALSE], FUN, ...)
    X[.dimnames(temp)] = temp
  }
  return(X)
}

#' @rdname zapply
#' @export rollmean.md3
#' @export
rollmean.md3 = function (x, k, na.pad = TRUE, align = c("center", "left", "right"), ...) {
  zapply(x,rollmean, k=k, na.pad=na.pad, align=align,...)
}


#' @rdname zapply
#' @export
movav=rollmean.md3

#' @rdname zapply
#' @export rollsum.md3
#' @export
rollsum.md3 = function (x, k, na.pad = TRUE, align = c("center", "left", "right"), ...) {
  zapply(x,rollsum, k=k, na.pad=na.pad, align=align,...)
}

#' @rdname zapply
#' @export rollmax.md3
#' @export
rollmax.md3 = function (x, k, na.pad = TRUE, align = c("center", "left", "right"), ...) {
  zapply(x,rollmax, k=k, na.pad=na.pad, align=align,...)
}

#' @rdname zapply
#' @export rollmedian.md3
#' @export
rollmedian.md3 = function (x, k, na.pad = TRUE, align = c("center", "left", "right"), ...) {
  zapply(x,rollmedian, k=k, na.pad=na.pad, align=align,...)
}





.melt_check = function (data, id.vars, measure.vars, variable.name, value.name)
{
  is.string = function(x) {
    is.character(x) && length(x) == 1
  }
  varnames <- names(data)
  if (!missing(id.vars) && is.numeric(id.vars)) {
    id.vars <- varnames[id.vars]
  }
  if (!missing(measure.vars) && is.numeric(measure.vars)) {
    measure.vars <- varnames[measure.vars]
  }
  if (!missing(id.vars)) {
    unknown <- setdiff(id.vars, varnames)
    if (length(unknown) > 0) {
      vars <- paste(unknown, collapse = ", ")
      stop("id variables not found in data: ", vars,
           call. = FALSE)
    }
  }
  if (!missing(measure.vars)) {
    unknown <- setdiff(measure.vars, varnames)
    if (length(unknown) > 0) {
      vars <- paste(unknown, collapse = ", ")
      stop("measure variables not found in data: ",
           vars, call. = FALSE)
    }
  }
  if (missing(id.vars) && missing(measure.vars)) {
    discrete <- sapply(data, function(x) is.factor(x) ||
                         is.character(x) || is.logical(x))
    id.vars <- varnames[discrete]
    measure.vars <- varnames[!discrete]
    if (length(id.vars) != 0) {
      message("Using ", paste(id.vars, collapse = ", "),
              " as id variables")
    }
    else {
      message("No id variables; using all as measure variables")
    }
  }
  else if (missing(id.vars)) {
    id.vars <- setdiff(varnames, measure.vars)
  }
  else if (missing(measure.vars)) {
    measure.vars <- setdiff(varnames, id.vars)
  }
  if (!is.string(variable.name))
    stop("'variable.name' should be a string", call. = FALSE)
  if (!is.string(value.name))
    stop("'value.name' should be a string", call. = FALSE)
  list(id = id.vars, measure = measure.vars)
}




.df2md3 = function (data, id.vars, name_for_cols = NULL, split = NULL, ...) {
  splitmissing=FALSE
  if (!length(split)) {splitmissing=TRUE; split='.'}
  #require(data.table)
  data = as.data.frame(data, stringsAsFactors = FALSE)
  data = .fixfactors(data)
  if (missing(id.vars)) {
    id.vars = colnames(data)[sapply(data, function(x) {.timo_is(x) | is.character(x)})]
  }
  if (!length(id.vars)) {
    data = cbind(as.character(rownames(data)), data, stringsAsFactors = FALSE)
    colnames(data)[[1]] = "rownames"
    id.vars = "rownames"
  }
  dcl = sapply(data, mode)
  if (any(!(dcl %in% c("character", "logical",
                       "numeric")))) {
    stop("dsfdasdf")
  }
  if (ncol(data) - length(id.vars) < 2L & !missing(id.vars)) {
    dd = data
    tempdn = colnames(data)
    names(tempdn) = tempdn
    tempdn = .subset(tempdn, id.vars)
  }
  else {
    if (missing(id.vars)) {
      vars <- suppressMessages(data.table:::.melt_check(data, variable.name = .md3resnames('value'),
                                           value.name = "value"))
    }
    else {
      vars <- suppressMessages(.melt_check(data, id.vars,
                                           variable.name = .md3resnames('value'),
                                           value.name = "value", measure.vars = character(0)))
    }
    id.ind <- match(vars$id, names(data))
    temp = apply(data[, id.ind, drop = FALSE], 1, paste,
                 collapse = ".")
    if (anyDuplicated(temp))
      stop("The codes provided are not unique: There are duplicates when using columns ", paste(names(data)[id.ind],
                                                 collapse = ","),"  to identify observations.")
    dd = suppressMessages(data.table:::as.data.frame.data.table(data.table::melt.data.table(data.table::as.data.table(data),
                                                            id.vars = id.ind, na.rm = FALSE, value.name = .md3resnames('value'),
                                                            variable.name = "_stuff_from_THEcolnames_", variable.factor = FALSE, value.factor = FALSE)))
    dd = .fixfactors(dd)
    if (split != "") {
      ix0 = strsplit(dd[, "_stuff_from_THEcolnames_"], split = split,
                     fixed = TRUE)
      ix0lengths = unique(sapply(ix0, length))
      if (length(ix0lengths) == 1L) {
        if (ix0lengths > 1)
          if (splitmissing & !length(name_for_cols)) message('Column names contain the delimiter "',split[1],'". This has been intepreted as denoting ', ix0lengths, ' different dimensions.')
          dd = data.frame(dd[, vars$id, drop = FALSE],
                          as.data.frame(matrix(unlist(ix0),ncol = ix0lengths,byrow = T),stringsAsFActors=FALSE),
                          dd[, .md3resnames('value'),drop=FALSE], stringsAsFactors = FALSE)
      }
      else {
        warning("did not manage to split column names")
      }
    }
    if (!length(name_for_cols))
      name_for_cols = LETTERS
    tempdn = c(id.vars, name_for_cols[1:length(ix0[[1]])])
    if (anyNA(tempdn))
      tempdn[is.na(tempdn)] = LETTERS[0:sum(is.na(tempdn))]
  }
  #browser()
  lix = list()
  for (i in seq_along(tempdn)) {
    lix[[tempdn[[i]]]] = unique(dd[, i])
  }
  #ixt = .dn_findtime(lix, TRUE, TRUE)
  ixt=.couldbetimo(lix)
  if (length(ixt)) {
    if (length(ixt)>1) { warning('was not clear which dimension was the time dimension.'); ixt=ixt[[1L]]}
    if (is.numeric(lix[[ixt]])) {lix[[ixt]] = as.timo.numeric(lix[[ixt]])}

    #in case name_for_cols named the wrong column TIME:
    if (any(toupper(names(lix))=='TIME')) { if (which(toupper(names(lix))=='TIME')!=ixt) { names(lix)[which(toupper(names(lix))=='TIME')] = names(lix)[[ixt]]   }}
    names(lix)[[ixt]] = "TIME"
    dd[[ixt]] = as.timo(dd[[ixt]])

    colnames(dd) = c(names(lix), .md3resnames('value'))
    if (ixt<length(lix)) {
      lix=lix[c(setdiff(seq_along(lix),ixt),ixt)]
      dd=dd[,c(names(lix), .md3resnames('value'))]

    }
  } else {
    colnames(dd) = c(names(lix), .md3resnames('value'))
  }

  if (anyNA(dd[[.md3resnames('value')]])) { dd=dd[!is.na(dd[[.md3resnames('value')]]),]}
  dout=.stackeddf2md3(dd)
  #.setdimcodes(dout,lix)
  dout




}

#' @export
anyNA.md3 = function(x, recursive = FALSE) {
  if (length(x[[1]]) < prod(.dim(x))) {
    return(TRUE)
  }
  return(FALSE)
}


#' @export
as.md3.data.table  = function(x,id.vars, name_for_cols = NULL, split = ".", obsattr=character(0), ...) {
  if (is.data.frame(x)) { x= data.table:::as.data.table.data.frame(x)}
  dcstruct=attr(x,'dcstruct')
  if (length(obsattr)) {
    if (!missing(split) | missing(name_for_cols)) {stop('observation attributes can only be passed along values in a fully stacked data.frame/data.table. Try using melt() before this function.')}
      y=.stackeddf2md3(x,isdf = FALSE)
  } else {
      y=.df2md3(x,id.vars = id.vars,name_for_cols = name_for_cols, split=split )
  }
  if (length(dcstruct))  attr(y,'dcstruct')=.dimcodesrescue(.getdimnames(y,FALSE),dcstruct)
  return(y)

}

#' @export
as.md3.data.frame = as.md3.data.table

#' @export
as.md3.numeric = function(x,xnames=NULL,dimname=NULL) {
  if (length(xnames)) {
    if (length(xnames) == length(x)) {
      names(x) = xnames
    } else {
      warning('xnames must have same length as x')
    }
  }
  if (is.null(names(x))) {
    names(x) = paste0('X',as.character(seq_along(x)))
  }

  if (!length(dimname)) { dimname='A'}
  dout=data.table::data.table(names(x),x)

  ldc = list(names(x));
  if (length(.couldbetimo(ldc))) {
    dimname='TIME'
    if (is.character(ldc[[1]])) {
      ldc[[1]]=as.timo.character(ldc[[1]])
      dout[[1]] =as.timo(dout[[1]])
    }

  }
  names(ldc)=dimname
  #setattr(dout,'dcsimp',ldc)
  setattr(dout,'dcstruct',.dimcodesrescue(ldc))

  setnames(dout, c(dimname,.md3resnames('value')))
  .md3_class(dout)
}
#' @export
as.md3.integer64 = function(x,...) {
  as.md3.numeric(as.numeric(x),...)
}

#' @export
as.md3 = function(x,...) {
  UseMethod('as.md3')
}

#' @export
is.md3 = .md3_is
#
#
# olag.md3 = function (x, k = 1, na.pad = TRUE, ...) {
#   mytime=time.md3(x); mytimemink=mytime-k
#   mydc=attr(x,'dcsimp')
#   lix=list(); lix[[which(names(attr(x,'dcsimp'))=='TIME')]]= mytimemink
#   dy=.md3get(x,lix,as = 'DT')
#   dy$TIME = dy$TIME +k
#   if (!na.pad) { browser();mydc$TIME=.timo_class(setdiff(mydc$TIME,setdiff(mytimemink,mytime)))}
#    attr(dy,'dcsimp') = mydc
#   .md3_class(dy)
#
# }
#
#

.lagmd3time = function (x, k = 1, consttimerange=TRUE, na.pad = TRUE, ...) {
  mytime=.timo_within(time.md3(x),referstoend = FALSE)
  if (is.null(mytime)) return(x)

  mydc=.getdimnames(x,TRUE)

  #lix=list(); lix[[which(names(attr(x,'dcsimp'))=='TIME')]]= mytimemink
  dy=.dt_class(x)
  dy$TIME = .timo_within(dy$TIME +k,referstoend = FALSE)
  mydc$TIME=.timo_within(mytime +k,referstoend = FALSE)

  if (consttimerange | na.pad) { mydc$TIME=sort(c(.timo_class(setdiff(mytime,mydc$TIME)),mydc$TIME))}
  if (consttimerange) {

    addedobs=.timo_class(setdiff(mydc$TIME,mytime));
    dy=dy[!(TIME %in% addedobs)]
    mydc$TIME=.timo_class(setdiff(mydc$TIME,addedobs))
  }
  if (!na.pad & k!=0) {
    if (k<0) {mydc$TIME=mydc$TIME[seq_len(length(mydc$TIME)-k+1)]} else{mydc$TIME=mydc$TIME[-seq_len(k)]}
  }

  attr(dy,'dcstruct') = .dimcodesrescue(mydc,x)
  .md3_class(dy)

}

.md3anylag=  function(x, k = 1, whichdim=length(dim(x)),constrange=TRUE,na.pad=TRUE,...) {
  if (is.numeric(whichdim)) {whichdim=names(.getdimnames(x,TRUE))[[whichdim]]}
  if (toupper(whichdim)=='TIME') { return(.lagmd3time(x,k = k,consttimerange = constrange,na.pad = na.pad,...) )}
  mydn=.getdimnames(x,TRUE)
  dx=.dt_class(x)
  mydict = mydn[[whichdim]]; names(mydict)=data.table::shift(mydn[[whichdim]],k)
  if (!constrange) {
    leftoverelems=setdiff(mydict,names(mydict))
    mydict = make.names(c(mydict,rep('X',length(leftoverelems))), unique = TRUE)
    names(mydict) = c(data.table::shift(mydn[[whichdim]],k),leftoverelems)
  } else {leftoverelems=character()}


  dx[[whichdim]]=mydict[dx[[whichdim]]]
  dx=dx[!is.na(dx[[whichdim]])]
  if (!constrange | !na.pad) {#
    mydn[[whichdim]] =  base::unname(mydict)
    if (!na.pad) {
      mydn[[whichdim]] =  base::unname(mydict[!is.na(names(mydict))])
    } else {
      mydn[[whichdim]] =  base::unname(mydict)
    }
    attr(dx,'dcstruct') <- .dimcodesrescue(mydn,.getdimcodes(x))
  }

  .md3_class(dx)
}




#' lag MD3 objects
#'
#' Lags,  differences, and growth rates of MD3 objects akin to the lag function operating on other R objects
#' @param x an md3 object, or anything that may be converted to an md3
#' @param k,lag Integer or character. If integer, the number of backward lags used (or if negative the number of forward lags). Can also be a character time differnce corresponding to \code{timo}, such as \code{2M} for two months or \code{1W} for one week
#' @param whichdim which dimension of x to lag over (referred to as integer number of dimension name). By default the last dimension (typically \code{TIME}) but could be any other.
#' @param constrange logical. If TRUE, then the time range is expande to reflect the 'new'  periods. If FALSE (default), only time periods originally present in x will be preserved.
#' @param na.pad logical. IF TRUE (default) then the periods at the beginning o(ro end) of the original time range will be returned as NAs. See examples.
#' @param differences an integer for \code{diff} indicating the order of the difference.
#' @param logcompounding logical for \code{growth}, default FALSE. If TRUE, the result of \code{growth} is akin to log differences.
#' \dots not used
#' @return an md3 object
#' @seealso \code{\link{zapply}}, \code{\link[zoo]{rollapply}},  \code{\link{rollmean.md3}}
#' @examples
#' xx=euhpq[TOTAL.I15_Q.AT:CY.y2005:y2008]
#' xx
#'
#' lag(xx,k=4) #lagging by four quarters
#' lag(xx,k=4, constrange=FALSE) #lagging by 4 Q, and creating new time periods
#' lag(xx,k=4, na.pad=FALSE) #lagging by 4 Q, and not keeping old time periods
#'
#' lag(xx,k='6M') #lagging by 6 months (using another frequency)
#' lag(xx,k=-4) #reverse lagging
#'
#' growth(xx,lag = 4) #year-on-year growth rates (careful: these are not in percent)
#' diff(xx,lag = 4) #year-on-year differences
#'
#' lag(xx,whichdim = 'geo') # lagging along countries
#' lag(xx,whichdim = 'geo', constrange=FALSE) # lagging along countries with constrange off
#'
#' @export lag.md3
#' @export
lag.md3= .md3anylag


#' @rdname lag.md3
#' @export
diff.md3 = function(x, lag = 1L, whichdim=length(dim(x)),na.pad=TRUE, differences = 1L,...) {
  if (differences < 0) stop('argument differences has to be a positive integer')
  differences=as.integer(differences)
  if (differences > 1L) { x= diff.md3(x,lag=lag,differences = differences-1L,whichdim=whichdim,na.pad=na.pad)}
  if (is.numeric(whichdim)) {whichdim=names(.getdimnames(x,TRUE))[[whichdim]]}
   if (toupper(whichdim)=='TIME') {
     xout=x-.lagmd3time(x,k = lag,consttimerange = TRUE, na.pad=TRUE)
   } else {
    xout=x-.md3anylag(x,k = lag,whichdim = whichdim,constrange = TRUE)
   }

  if (!na.pad) {warning('na.pad=FALSE does not work for now...')}
  xout
}

#' @rdname lag.md3
#' @export
growth  = function(x,lag=1L, whichdim=length(dim(x)),logcompounding=FALSE) {
   xlaggd=.md3anylag(x,k = lag,whichdim=whichdim)
   yy=x/xlaggd
   if (logcompounding) return(sign(yy)*log(abs(yy)))
   yy-1
   #yy=(log(abs(x))-log(abs(xlaggd)))*(sign(x)*sign(xlaggd))
   #if (logcompounding) return(yy)
   #exp(yy)-1
}

.adddim_md3 = function(x,.dimname='XY', .dimcodes=NULL,.fillall=FALSE,...) {
  if (!.md3_is(x)) stop("needs md3")
  if (length(.dimname)>1) stop('dimname needs to be a single string element')
  #if (length(.dimcodes)>1) browser()
  mydn=.getdimnames(x,TRUE); olddc=attr(x,'dcstruct')
  if (!length(.dimcodes)) {.dimcodes=1}
  if (.timo_is(.dimcodes)) {
    .dimname='TIME'
  } else {
    .dimcodes = make.names(.dimcodes, unique = TRUE)
    .dimname = tail(make.names(c(names(mydn), .dimname), unique = TRUE), 1)
  }

  temp = c(list(.dimcodes), mydn)
  names(temp)[[1]] = .dimname
  x=.dt_class(x)
  if (.fillall) {
    .dimcodes=unlist(lapply(as.list(.dimcodes),rep,NROW(x)))
    if (.dimname=='TIME') {.dimcodes=.timo_class(.dimcodes)}
  } else {
    .dimcodes=.dimcodes[[1L]]
  }
  x=data.table:::cbind.data.table(.dimcodes,x)


  names(x)[[1L]] =.dimname
  attr(x,'dcstruct') = .dimcodesrescue(temp,olddc)
  .md3_class(x)
}



#' Adds a new dimension to an MD3 array
#'
#' Adds new dimension to an MD3 object, optionally to be filled with a number of new elements
#' @param x an md3 object
#' @param .dimname the name of the new dimension. Has to be a single character element
#' @param .dimcodes an optional character vector with the code names of the new elements in that dimension
#' @param .fillall if FALSE and length(.dimcodes)>1 then the first element will contain the existing data while the others will be NA
#'                if TRUE and length(.dimcodes)>1 then each new element will be filled with the same data as the first one
#' @param \dots unused
#' @return an md3
#' @examples
#'
#' p1=add.dim(euhpq,'Residence',c('Primary','Secondary'))
#' print(p1[,1,1,4,])
#' p2=add.dim(euhpq,'Residence',c('Primary','Secondary'),.fillall=TRUE)
#' print(p2[,1,1,4,])
#'
#' @export
add.dim=.adddim_md3

.adddim_andfill =  function(x,.dimname='XY', .dimcodes=NULL,...) {
  .adddim_md3(x,.dimname = .dimname,.dimcodes = .dimcodes, .fillall = TRUE)
}


### MERGING COALESCING
#' Concatenates two or more md3 objects
#'
#' @param x an MD3 object with n dimensions
#' @param y an MD3 object with n or n-1 dimensions
#' @param \dots further MD3 objects with n or n-1 dimensions
#' @param along name of an existing or new dimension along which to combine x and y. If unspecified, then it tries to guess along whcih deimnsion to paste, depending on the value of \code{overwrite}
#' @param newcodes name of element names for the thing to be added (see \code{\link{dimcodes}}).
#' @param overwrite relevant if x and y have the same number of dimnesions. if TRUE, then key combinations from y are used to overwrite key combinations from x. If FALSE, then the function tries to resolve conflicts by adding a new dimension to x
#' @param verbose if FALSE, suppress  success messages from the merging
#' @return an md3 object with n or n+1 dimensions
#' @seealso \code{\link{drop.md3}}, \code{\link{aggregate.md3}}
#' @examples
#'
#' a1=euhpq['TOTAL','I15_Q',1:3,]
#'
#' a2=euhpq[DW_NEW.I15_Q.BG.y:y2010]*2
#' myres=merge(a1,a2) #assumes this is a new country, adds a new country X1
#' print(myres)
#'
#' myres=merge(a1,a2, newcodes='AB') #same but with a code
#' myres
#'
#' merge(a1,a1['BE',,drop=FALSE]*2, overwrite=TRUE) #overwite Belgium with Belgium*2. Compare with a1
#' a1['BE',] = a1['BE',] *2
#' a1
#'
#' merge(a1,a2, overwrite=FALSE) #is the same as
#' c(a1,a2)
#'
#' @export
merge.md3=function (x, y, ..., along = NULL, newcodes = character(0), overwrite = NULL, verbose=TRUE) {
  ix = list()
  ctargs = nargs() - as.integer(!missing(x)) - as.integer(!missing(y)) -
    as.integer(!missing(along)) - as.integer(!missing(newcodes)) -
    as.integer(!missing(overwrite))
  if (ctargs)
    for (i in 1:ctargs) {
      if (eval(parse(text = paste0("missing(..",
                                   i, ")")))) {
        ix[[i]] = integer(0)
      }
      else {
        ix[[i]] = try(eval(parse(text = paste0("..",
                                               i))), silent = TRUE)
        if (grepl("error", class(ix[[i]])[[1]])) {
          stop("cannot understand element ", i)
        }
      }
    }
  z = .merge2md3(x, y, along = along, newcodes = newcodes,
                 overwrite = overwrite,verbose=verbose)
  for (i in seq_along(ix)) {
    newcodes2 = ifelse(is.na(newcodes[i + 2]), paste0("X",
                                                      i + 2), newcodes[i + 2])
    z = .merge2md3(z, ix[[i]], along = along, newcodes = newcodes2,
                   overwrite = overwrite,verbose=verbose)
  }
  z
}

#' @export
c.md3=merge.md3

.merge2md3 = function (x, y, along = NULL, newcodes = character(0), overwrite = NULL, verbose=TRUE) {
  if (!is.null(along)) {
    if (!is.character(along)) {
      stop("along must be a single character element")
    }
    if (length(along) > 1) {
      if (verbose) warning("along has been specfied as a vector with ", length(along), "elements. I only took the first one")
    }
  }
  if (!.md3_is(x))
    x = as.md3(x)
  xdn = .getdimnames(x,TRUE); xoldc=attr(y,'dcstruct')
  xndnt = names(xdn)
  names(xndnt) = xndnt
  if (!.md3_is(y)) {
    if (!is.null(along))
      if (!is.na(xndnt[along]))
        xndnt = xndnt[-match(.subset2(xndnt, along),
                             xndnt)]
    if (is.null(names(dimnames(y))) & (length(xndnt) ==
                                        length(.dim(y)))) {
      names(dimnames(y)) = xndnt
    }
    y = as.md3(y)
  }
  ydn = .getdimnames(y,TRUE); yoldc=attr(y,'dcstruct')
  if (is.null(along)) {
    if (abs(length(xdn) - length(ydn)) == 1L) {
      along = c(setdiff(names(xdn),names(ydn)),setdiff(names(ydn),names(xdn)))
      if (verbose) warning("along was not specified - I assume it is ", along)
    }
    else if (length(xdn) == length(ydn)) {
      if (length(overwrite))
        if (!overwrite) {
          #stop("£$)")

            along = gsub("\\.", "", tail(make.unique(c(names(xdn),
                                                       names(ydn), "X")), 1))
            if (verbose) warning("along was not specified, yet overwriting was set to FALSE - I thus made a new dimension and called it ", along)
            return(.merge2md3(x,y,along = along,newcodes = newcodes,overwrite = FALSE,verbose = verbose))

        }
    }
    else {
      stop("objects to be merged must have either the same number of dimensions, or 'along' specified")
    }
  }

  namnotin = function(a1, a2) {
    setdiff(names(a1), names(a2))
  }
  if (length(namnotin(ydn, xdn))) {
    temp = data.table:::copy(x)
    x = data.table:::copy(y)
    y = temp
    rm(temp)
    xdn = .getdimnames(x,TRUE)
    ydn = .getdimnames(y,TRUE)
  }
  if (length(namnotin(ydn, xdn))) {
    if (all(which(names(ydn) %in% names(xdn)) == which(names(xdn) %in%
                                                       names(ydn)))) {
      tix = which(!(names(ydn) %in% names(xdn)))
      if (verbose) warning("Dimension names were somewhat confusing: I interpreted ", paste(paste(names(ydn), "as", names(xdn))[tix], collapse = ","), ".")
      names(ydn) = names(xdn)
      attr(y, "hihi") = ydn
    }
    else {
      stop("dimension names in objects are confusing: dimensions ",
           namnotin(ydn, xdn), " exist in first but not in second.\n\n dimensions ",
           namnotin(xdn, ydn), " exist in second but not in first.\n")
    }
  }
  if (!(is.null(along))) {
    if (length(along) > 1) {
      along = along[[1]]
      if (verbose) warning("along can be only a character singleton")
    }
    if (!(along %in% names(ydn))) {
      if (!(along %in% names(xdn))) {
        temp = newcodes
        if (length(temp) < 2L) {
          temp = make.names(1:2, unique = TRUE)
        }
        x = .adddim_md3(x, along, temp[[1]])
        y = .adddim_md3(y, along, temp[[2]])
        ydn=.getdimnames(y,TRUE); xdn=.getdimnames(x,TRUE)
      }
      else {
        if (!length(newcodes))
          newcodes = tail(make.names(c(xdn[[along]],
                                       length(xdn[[along]]) + 1), unique = TRUE),
                          1)
        newcodes = newcodes[[1]]
        y = .adddim_md3(y, along, newcodes)
        ydn=.getdimnames(y,TRUE)
      }
    }
  }
  #y = aperm.md3(y, names(xdn))

  zdn=lapply(as.list(names(xdn)),function(i) { union(xdn[[i]],ydn[[i]])})
  names(zdn)  =names(xdn)
  if (any(names(zdn)=='TIME')) zdn[['TIME']]=.timo_class(zdn[['TIME']])
  z=data.table:::rbind.data.table(x,y,fill=TRUE)
  attr(z,'dcstruct') = .dimcodesrescue(.dimcodesrescue(zdn,xoldc),yoldc)
  return(.md3_class(z))

}


.imputeproxyintoarray = function(a2impute,aproxy,whichdim='TIME',backward=FALSE) {
  if (!identical(dimnames(a2impute),dimnames(aproxy))) stop('need same dimensions')
  if (is.null(names(dim(a2impute)))) { stop("need named dimensions")}
  tix=which(names(dim(a2impute))==whichdim)[1]


  selixvec=as.character(dim(a2impute)); selixvec[-tix]=""

  myix=function(j) {vv=selixvec; vv[vv!=""]=j; vv}
  ixofnottime=t(.index1d2mdint(dim(a2impute)[-tix],seq_len(prod(dim(a2impute)[-tix]))))
  colnames(ixofnottime) = names(dim(a2impute)[-tix])
  ixofnottime=cbind(ixofnottime,NA_integer_);
  #browser()
  colnames(ixofnottime)[NCOL(ixofnottime)]=whichdim;
  ixofnottime=ixofnottime[,names(dim(a2impute)),drop=FALSE]
  if (backward) revincase=rev else revincase =function(x) x
  if (backward) ilag= function(j) j+1 else ilag=function(j) j-1
  for (i in revincase(seq_len(as.integer(max(selixvec))))[-1]) {
    temp=.arrIndex(aproxy,myix(i))-.arrIndex(aproxy,myix(ilag(i)))+.arrIndex(a2impute,myix(ilag(i)))
    temp[!is.na(.arrIndex(a2impute,myix(i)))] = .arrIndex(a2impute,myix(i))[!is.na(.arrIndex(a2impute,myix(i)))]

    ixtemp=ixofnottime; ixtemp[,whichdim] = i
    a2impute[ixtemp]=temp

  }
  a2impute
}

  #dforward=x
  # mydn0=list(); tix=.dn_findtime(.getdimnames(x))
  # mydn=function(mytp,k=0) { ll=mydn0; ll[[tix]] = .timo_class(mytp); if (k!=0) {ll[[tix]]=ll[[tix]]-k}; ll }
  # for (tp in sort(time(x))) {
  #   insertarr=.md3get(dforward,mydn(tp,1),as='a',drop=FALSE)+.md3get(mm,mydn(tp),as='a',drop=FALSE)-.md3get(mm,mydn(tp,1),as='a',drop=FALSE)
  #   dforward=.md3set(dforward,mydn(tp),value = insertarr,onlyna = TRUE,usenames=FALSE)
  # }



#' Drop observation attributes
#'
#' By default, this removes obs attributes other than obs_value from an MD3 or associated data.table
#' @param x an md3 object or stacked data.table
#' @param asDT logical. Default \code{FALSE} returns an md3. \code{TRUE} returns a data.table
#' @param attr2keep\code{"value"} by default, but could also be "status" or "conf". If this is not 'value' or 'obs_value', asDT is assumed to be TRUE.
#' @return an md3 object or dat.table, depending on \code{asDT}
#' @seealso \code{\link{fill.md3}},  \code{\link{as.data.table}}
#' @examples
#'
#' as.data.table(euhpq['TOTAL.I15_Q.BG.'])
#' as.data.table(unflag(euhpq['TOTAL.I15_Q.BG.']))
#' unflag(euhpq['TOTAL.I15_Q.BG.'], asDT=TRUE)
#' @export
unflag = function(omd3,asDT=FALSE,attr2keep='obs_value') {
  #asDT=NA: same as .dt_class(unflag(x))
  wasdt=is.data.table(omd3)
  dx=data.table::copy(.dt_class(omd3))
  orincol=NCOL(dx)
  if (wasdt) {
    tempix=tolower(colnames(dx)) %in% tolower(.md3resnames())
    if (length(tempix)) colnames(dx)[tempix]=paste0('_.',colnames(dx)[tempix])
  }
  dx=dx[,names(dx) %in% c(names(.getdimnames(omd3)),.md3resnames(attr2keep)),with=FALSE]
  if (NCOL(dx)==orincol) {
    if (anyNA(dx[[.md3resnames(attr2keep)]])) {stop('omd3 is a faulty md3 object. Do as.md3(as.data.table(omd3)) to repair it')}

  } else {
    dx=dx[!is.na(dx[[.md3resnames(attr2keep)]]),]
  }
  if (.md3resnames(attr2keep)!=.md3resnames('value' )) asDT=TRUE
  if (is.na(asDT)) {return(dx)}
  if (wasdt & asDT) {colnames(dx)=gsub('^_\\.','',colnames(dx))}
  if (asDT) return(dx)
  .md3_class(dx)
}






#' Impute NAs in MD3 objects with proxy data or interpolation
#'
#' For each time series in the MD3 array, this function fills intermittent NAs using the delta of a proxy, or (geometric) interpolation
#' @param x an md3 object
#' @param proxy either a vector (applied to each time series), or an array or an md3 with the same dimensions or dimension names as x. If proxy left to NULL then this function will merely interpolate any gaps it finds
#' @param method 'dlog' denotes geometric interpolation, 'diff' denotes linear interpolation
#' @param maxgap an integer denoting the maximum number of consecutive NAs to be filled
#' @param direction only necessary where proxy is not NULL. See details below
#' @return an md3 object
#' @details
#' This refers to interpolation cases where an annual time series e.g. has values for 2000 and 2005, but not for 2001:2004
#'
#' If maxgap =3 or more, then interpol will fill the NAs of 2001:2004
#'
#' Note that interpolna does not work for mixed frequencies
#'
#' As regards imputation by proxy, the function will impute NAs in time series that have a least one non-missing value with growth rates
#' or deltas of the proxy. direction=='forward' means it does this for any NAs after non-missing observations, direction==' backward' for
#' any NAs before. Direction 'both' is a weighted avearge of both methods wherever they overlap
#' @seealso \code{\link{fill.md3}},  \code{\link{aggregate.md3}}, \link{indexMD3}
#' @examples
#'
#' \dontrun{w1= euhpq[TOTAL.I15_Q.AT:CY.y2005:y2008] #make small md3}
#' w1["BE.2006q4:2007q1"]=NA
#' w1["CY.2006:2007"]=NA
#' w1['AT.y2006q1']=100
#' print(w1)
#' #just interpolate
#' imputena(w1) #note the difference 'maxgap'  makes for CY here
#' imputena(w1,maxgap=10) #note the difference 'maxgap'  makes for CY here
#'
#'
#'
#' using a lower-dimensional proxy
#' \dontrun{imputena(w1, euhpq['TOTAL.I10_Q.AT:CY.y2005:y2008'])}
#'
#' #with trend
#' imputena(w1, 1:16)
#'
#' @export
imputena = function(x,proxy=NULL,method=c('dlog','diff'), maxgap=6, direction=c('both','forward','backward')) {
  if(pmatch(method[[1L]],c('dlog','log','growth','diff','delta','shift','level')) < 4) {method='dlog'} else {method='diff'}
  if (is.null(proxy) & !missing(direction)) {stop('The argument "direction" only makes sense for imputing proxy data')}
  if (!is.null(proxy) & !missing(maxgap)) {warning('The argument "maxgap" only makes sense for interpolating, not with proxy data. maxgap will be ignored')}


  if (is.null(time.md3(x))) { stop('this only works with time series so far. A TIME dimension is necessary')}
  myf=unique(.timo_frq(time.md3(x)))
  if (length(myf)!=1) stop('cannot do this on mixed frequencies')
  if (!is.null(proxy)) if (!is.vector(proxy)) proxy=as.array(proxy)

  dx=unflag(x,asDT=NA)

  dimlab=names(.dim(x))
  setkeyv(dx,dimlab)

  idgap=dx[,list(TIME,V2=as.integer(diff.timo(TIME,na.pad = TRUE))),by=setdiff(names(.dim(x)),'TIME')]

  idgap=idgap[V2!=1]
  if (is.null(proxy)) {idgap=idgap[V2<=maxgap,]; if (!NROW(idgap)) return(x)}

  idgap[,startperiod:=TIME-V2]
  idgapfirst=copy(idgap)
  colnames(idgapfirst)[match(c('TIME','startperiod'),colnames(idgapfirst))]=c('endperiod','TIME')


  idgap=dx[idgap,,on=.NATURAL]
  names(idgap)[names(idgap)==.md3resnames('value')] = 'endvalue'

  idgapfirst=dx[idgapfirst,,on=.NATURAL];
  names(idgapfirst)[names(idgapfirst)==.md3resnames('value')] = 'startvalue'
  idgapfirst=idgapfirst[,TIME:=endperiod][,c(dimlab,'startvalue'),with=FALSE]
  idgap=merge(idgap,idgapfirst,by=dimlab)
  rm(idgapfirst)

  if (!is.null(proxy)) {
    mm=.md3_class(dx[0],dn = .getdimnames(x))
    if (is.array(proxy)) {dimnames(proxy)<- .matchixsubset2dn(proxy,.getdimnames(mm)); proxy=drop(proxy)}
    mm=.md3set(mm,value=proxy)
    ax=as.array(x); am=as.array(mm)
    if (method=='dlog') { ax=log(ax); am=log(am); unpack=exp} else unpack=function(x) x
    aforward=unpack(.imputeproxyintoarray(ax,am))
    abackward=unpack(.imputeproxyintoarray(ax,am,backward = TRUE))
  } else {
    aforward=abackward=as.array(x)[0]
  }
  #browser()

  if (!NROW(idgap)) {
    idgap2=idgap; idgap2[,period:=timo()]
    if (!('fact'%in% colnames(idgap2)) & ('V2' %in% colnames(idgap2)) ) {
      colnames(idgap2)[colnames(idgap2)=='V2'] <- 'fact'
    }

  } else {
    #idgap2=idgap[,cbind(.SD[,c(setdiff(dimlab,'TIME'),'startperiod','startvalue','endvalue'),with=FALSE],period=TIME-V2:0,fact=(0:V2)/V2),by='TIME']
    idgap[,fact:=0]; idgap2=idgap; idgap2[,period:=startperiod]
    for (glen in unique(idgap$V2)) {
      for (i in 1:glen) {
        temp=idgap[V2==glen,]; temp[['period']]=temp[['startperiod']]+i
        temp[['fact']]=i/glen
        idgap2=rbind(idgap2,temp); rm(temp)
      }
    }

  }


  colnames(idgap2)[match(c('TIME','period'),colnames(idgap2))]=c('endperiod','TIME')

  if (is.null(proxy)) {
    if (method=='dlog') {
      idgap2[,.md3resnames('value'):=exp(fact*log(endvalue)+(1-fact)*log(startvalue))]
    } else {
      idgap2[,.md3resnames('value'):=fact*endvalue+(1-fact)*startvalue]
    }
    dout=rbind(dx,idgap2[fact!=0 & fact!=1,colnames(dx),with=FALSE])
    return(.md3_class(dout,dn=dimcodes(x)))
  }



  hh=merge(merge(merge(dx,.dt_class(as.md3.array(aforward)),by = dimlab,all = TRUE),.dt_class(as.md3.array(abackward)),by = dimlab,all = TRUE),idgap2,by = dimlab,all = TRUE)
  colnames(hh)[length(dimlab)+1:3]=paste0('_val_',c('d','f','b'))

    hh[is.na(`_val_d`) & !is.na(`_val_b`),`_val_d`:=`_val_b`]
    hh[is.na(`_val_d`) & !is.na(`_val_f`),`_val_d`:=`_val_f`]
    if (!all(is.na(hh[,fact]))) {
      hh[!is.na(fact),`_val_d`:=(1-fact)*`_val_f`+fact*`_val_b`]
    }
    dout=hh[,seq_len(length(dimlab)+1),with=FALSE]
    colnames(dout)[NCOL(dout)]=.md3resnames('value')
    .md3_class(dout,dn=.getdimcodes(x))

}

#' @export
sort.md3 = function(x, decreasing = FALSE, na.last = NA, ...) {
  sort(.md3get(x, as = "array", drop = TRUE), decreasing, na.last, ...)
}

#' @export
order.md3 = function (..., na.last = TRUE, decreasing = FALSE) {
  order( .md3get(..., as = "array", drop = TRUE), decreasing=decreasing, na.last=na.last)
}



.FUNfixer = function(FUN=c(sum, mean, last, first)) {
  if (is.list(FUN)) {FUN=FUN[[1L]]}
  if (is.character(FUN)) FUN=get(trimws(FUN))
  if (any(grepl('UseMethod\\("end"\\)',deparse(body(FUN))[1:5]))) FUN = function(x) utils::tail(x,1)
  if (any(grepl('UseMethod\\("last"\\)',deparse(body(FUN))[1:5]))) FUN = function(x) utils::tail(x,1)
  if (identical(FUN,data.table::last)) FUN = function(x) utils::tail(x,1)
  if (any(grepl('UseMethod\\("start"\\)',deparse(body(FUN))[1:5]))) FUN = function(x) utils::head(x,1)
  if (any(grepl('UseMethod\\("first"\\)',deparse(body(FUN))[1:5]))) FUN = function(x) utils::head(x,1)
  if (identical(FUN,data.table::first)) FUN = function(x) utils::head(x,1)
  FUN
}

#' Fill out an MD3 or array with a number or a certain subset
#'
#' This function is useful when one wants to use the same sequence of numbers along a certain dimension, e.g. when computing an index
#' @param bigmd3 an md3 object, array, or anything that may be converted to an md3
#' @param \dots a numeric, array, or md3 with which to fill out all values of bigmd3. Alternatively, the indexing reference to a subset of the md3 (see \link{indexMD3})
#' @return an md3 object (or an array if bigmd3 is an array)
#' @seealso \code{\link{aggregate.md3}}, \link{indexMD3}, \code{\link{imputena}}, \code{\link{zapply}}, \code{\link{rollapply.md3}}, , \code{\link{movav}}
#' @examples
#'
#'
#' mm=euhpq[1,1,1:4,as.character(2013:2016)]
#'
#' # these four are equivalent:
#' fill(mm,mm[,'2014q4'])
#' fill(mm,'.2014q4')
#' fill(mm,'BE+BG.2014q4')
#' fill(mm,c('BE','BG'),'2014q4')
#'
#' mm/fill(mm,rowMeans(mm[,'2014']))*100 # e.g. re-base the index with the av of 2014 being 100
#'
#' fill(mm,100)
#' fill(mm,c('AT'=1,'BG'=2))
#'
#' fill(mm,c('2014q1'=1,'2014q2'=2))
#'
#'
#' fill(mm,1:2)
#'
#'
#'
#' @export
fill = function(bigmd3,...) {
  retasarr=FALSE
  if (is.array(bigmd3)) {retasarr=TRUE; bigmd3=as.md3.array(bigmd3) }
  if (!.md3_is(bigmd3)) {bigmd3=as.md3(bigmd3)}


  smallmd3=.dotsaslistws(...)
  if (length(smallmd3)==1) { smallmd3=smallmd3[[1L]]}

  if (.md3_is(smallmd3)) {smallmd3 = as.array.md3(smallmd3)}
  if (!is.array(smallmd3) && !is.numeric(smallmd3)) {
    temp=try(.md3get(bigmd3,smallmd3),silent=TRUE)
    if (!any(grepl('err',class(temp)))) {smallmd3=temp};
    rm(temp)
  }

  dx=.dt_class(bigmd3)
  mm=.md3_class(dx[0],dn = .getdimnames(bigmd3))
  sdn=.matchixsubset2dn(smallmd3,.getdimnames(mm))
  if (is.array(smallmd3)) {dimnames(smallmd3)<- sdn; smallmd3=drop(smallmd3)}
  if (!is.array(smallmd3) && length(smallmd3)>1) {
    if (all(setdiff(dim(as.array(smallmd3)),1)==unlist(lapply(sdn,length)))) {
      smallmd3=as.array(smallmd3); dimnames(smallmd3)=sdn
    }
  }
  dd=.getdimnames(bigmd3)
  dd[names(sdn)] = sdn
  mm=.md3set(mm,dd,value=smallmd3)

  if (retasarr) {return(.md3get(mm, as = "array", drop = FALSE))}
  if (.md3_is(mm)) {return(mm)}
  stop('cannot use fill on this object')
}

#p0=euhpq[2:3,1:2,1:3,1:12];p0[1,1,2,'2005q4:2006q3']=NA;p0[1,1,'ZZ',]=NA;  p0[1,1,1,'2005q4+2007q1:y']=100:104; #p0[1,1,,]
#imputena(p0)
# pp=diff(euhpq) #
# #
# Rprof(tmp <- tempfile())
# p2=euhpq-euhpq[,,,time(euhpq)-1]
# o3=.dimcodesrescue(o2,olddc = oo)
# Rprof()
# summaryRprof(tmp)
# unlink(tmp)


.subsetspectime = function(x,what,drop=TRUE) {
  ixt=.dn_findtime(x)
  if (ixt<1) return(x)
  if (is.function(what)) { what=what(time.md3(x))}
  dx=.dt_class(x)

  dx=dx[TIME==what,]
  if (drop) {dx=dx[,TIME:=NULL]}
  mdc=attr(dx,'dcstruct');
  if (drop) { mdc[['TIME']]<-NULL} else {mdc[['TIME']] = unique(dx[['TIME']])}
  attr(dx,'dcstruct')=.dimcodesrescue(mdc,attr(x,'dcstruct'))


  .md3_class(dx)
}

#' @export
start.md3 = function(x,...,drop=TRUE) {
  .subsetspectime(x,start.timo,drop=drop)
}

#' @export
end.md3 = function(x,...,drop=TRUE) {
  .subsetspectime(x,end.timo,drop=drop)
}

.timeaggregate = function(x, frq, FUN = c(sum,mean,end,start), na.rm=TRUE, ..., complete.periods=!na.rm, drop=drop) {
  FUN=.FUNfixer(FUN)
  vt=time.md3(x)
  if (is.null(vt)) return(x)

  of=.timo_frq(vt)
  ofunique = unique(of)

  if (length(frq)!=1) stop('Argument frq must be a single character like A, M, W, B, or D')
  frq=substr(trimws(toupper(as.character(frq[1L]))),0,1)
  frqpos=base::match(frq,.cttim$frqcodes[,1,drop=TRUE],nomatch=1000)
  ofpos=base::match(ofunique,.cttim$frqcodes[,1,drop=TRUE],nomatch=1000)
  if (any(ofpos<=frqpos)) {
    stop('Cannot aggregate an object of frequencies ',paste(unique(of,collapse=', ')),' to a lower frequency like ', frq, '.\nUse function disaggregate instead.')
  }

  xdn=.getdimnames(x)
  tgrp=.timo_cfrq(vt,frq=frq, referstoend=TRUE)

  if (na.rm & !missing(complete.periods)) {complete.periods=FALSE}
  if (complete.periods) {
    vtcomplete=timo()
    for (ff in ofunique) {
      vtcomplete=c(vtcomplete,.timo_class(unlist(lapply(as.list(unique(tgrp)),.timo_subperiods,ff))))
    }
  vt=vtcomplete=sort(vtcomplete)
  xdn[['TIME']] = vtcomplete
  attr(x,'dcstruct') = .dimcodesrescue(xdn,attr(x,'dcstruct'))
  tgrp=.timo_cfrq(vt,frq=frq, referstoend=TRUE)
  }


  tdict=.timo2char(tgrp); names(tdict) = .timo2char(vt)



  if (!na.rm) {
    dx=as.data.table.md3(unflag(x,FALSE),na.rm = FALSE)
    colnames(dx)[NCOL(dx)]<-'_.obs_value'
  } else {
    dx=unflag(x,asDT=NA)
  }





  nxdn=names(xdn)
  data.table::setkeyv(dx,nxdn)

  nxdn[nxdn=='TIME']='_.perdaggs'
  dx[,`_.perdaggs`:=tdict[.timo2char(TIME)]]
  dy=dx[,list(`_.obs_value`=FUN(`_.obs_value`,...)),by=nxdn]

  ydn=xdn
  ydn[['TIME']] =sort(unique(tgrp))
  dy[,TIME:=.char2timo(`_.perdaggs`,guess = FALSE)]
  dy[['_.perdaggs']]<-NULL
  attr(dy,'dcstruct') = .dimcodesrescue(ydn,attr(x,'dcstruct'))
  y=.md3_class(dy)
  if (drop) { y=drop.md3(y)}
  y
}



#' Aggregate an MD3 over time and/or multiple other dimensions
#'
#' Can sum, average, etc. to lower frequencies, or along entire dimensions, or along user-defined aggregates (such as country groups)
#' @param x an md3 object,
#' @param frq_grp what to aggregate to: A single character frequency code like \code{"A"},  \code{"Q"} or  \code{"B"} when aggregating along time, or a user-defined aggregate list (see details)
#' @param along character vector: the dimensions along which to aggregate. Default is \code{TIME}
#' @param FUN the function to use for aggregation. Default is \code{sum}, but \code{mean},  \code{end} and  \code{start} are also popular. Can be any function (passed along as a function or the string name of that function)
#' @param na.rm logical, default \code{TRUE}. Should missing values be removed?
#' @param \dots other arguments to FUN
#' @param complete.periods logical. Applies only in case of time aggregation and \code{na.rm=FALSE}. if e.g. \code{FALSE} and \code{FUN=end} then this takes the last value of the last available subperiod in case x ends within a period  (see examples).
#' @param drop Logical, default \code{TRUE}. See \code{\link{drop.md3}}
#' @return an md3 object. Note that any flags or observation metadata from the original MD3 will be dropped and not contained in the resulting md3.
#' @seealso \code{\link{fill.md3}},  \code{\link{disaggregate}}, \link{indexMD3}, \code{\link{imputena}}
#' @examples
#' #data("oecdgdp_aq")
#'
#' #Aggregating along the time dimension
#'
#' yy=oecdgdp_aq['Q','AU:BE',,'2011q2:2015q2']
#' yy[AU..y2011q2+y2011q3]=NA #make some artificial NAs for demo purposes
#' print(yy[,'GDP_USD',])
#' aggregate(yy,'A')[,'GDP_USD',] # sum quarterly GDP figures to annual ones
#' aggregate(yy,'A',FUN=sum, na.rm=FALSE)[,'GDP_USD',] # sum quarterly GDP figures to annual ones.
#' #Note the NAs for 2011 and 2015 because 2011q1 and 2015q3:2015q4 are missing completely
#'
#' #Alternatively sum those years where not all periods are available by setting complete.periods=FALSE
#' aggregate(yy,'A',FUN=sum, na.rm=FALSE, complete.periods=FALSE)[,'GDP_USD',]
#'
#'
#' aggregate(yy,'A',FUN=end)[,'GDP_USD',] #use the last period only
#' aggregate(yy,'A',FUN=end, na.rm=FALSE)[,'GDP_USD',] #use the last period only, but be careful about NAs
#'
#' #Aggregating along other dimensions
#'
#' aggregate(yy,along=c('LOCATION','TIME'),FUN=mean) # average GDP of countries
#' aggregate(yy,along=c('LOCATION'),FUN=sum) #GDP time series, sum of all three countries in yy
#'
#' aggregate(oecdgdp_aq['Q..GDP_USD.y2015q1:'],list(LOCATION=list(G7=c('CA','DE','ES','FR','IT','JP','UK','US'),Baltics=c('EE','LV', 'LT'))))
#' aggregate(oecdgdp_aq['Q..GDP_USD.y2015q1:'],list(LOCATION=list(G7=c('CA','DE','ES','FR','IT','JP','UK','US'),Baltics=c('EE','LV', 'LT'))), along = 'LOCATION') # along is superfluous here
#' aggregate(oecdgdp_aq['Q..GDP_USD.y2015q1:'],list(G7=c('CA','DE','ES','FR','IT','JP','UK','US')), along = 'LOCATION') # but along is not superfluous here,  because frq_grp does not contain enough info
#' aggregate(oecdgdp_aq['Q..GDP_USD.y2015q1:'],c('CA','DE','ES','FR','IT','JP','UK','US'), along = 'LOCATION') # but along is not superfluous here,  because frq_grp does not contain enough info
#' aggregate(oecdgdp_aq['Q..GDP_USD.y2015q1:'],list(LOCATION=list(G7=c('CA', 'DE','ES','FR','IT','JP','UK', 'US'),Baltics=c('EE','LV', 'LT')),  TIME=character()), FUN=mean) #mean GDP across Time
#'
#' #data(eupop)
#' aggregate(eupop['...y2019:y'],list(sex=list(all=c('F','M')),age=list(nwa=c('Y_LT15','Y_GE65'))))
#' #non-working age people in EU countries
#'
#' @export aggregate.md3
#' @export
aggregate.md3 = function(x, frq_grp, along='TIME', FUN = c(sum,mean,end,start), na.rm=TRUE,
                         ..., complete.periods=!na.rm, drop=TRUE) {
  if (missing(frq_grp)) {frq_grp=NULL}
  if (na.rm & !missing(complete.periods)) {warning('Argument complete.periods is being ignored when na.rm=TRUE'); complete.periods=FALSE}
  if (length(along)==1 & length(frq_grp)==1) if (all(along=='TIME') & is.character(frq_grp)) return(.timeaggregate(x,frq=frq_grp,FUN,na.rm,...,complete.periods = complete.periods, drop=drop))
  FUN=.FUNfixer(FUN)


  xdn=.getdimnames(x)
  if (!na.rm) {
    dx=as.data.table.md3(unflag(x,FALSE),na.rm = FALSE)
    colnames(dx)[NCOL(dx)]<-'_.obs_value'
  } else {
    dx=unflag(x,asDT=NA)
  }

  if (missing(along) & is.list(frq_grp)) {
    along=names(frq_grp)
  }

  nxdn=names(xdn)
  data.table::setkeyv(dx,nxdn)
  nalong=setdiff(nxdn,along);
  if (length(nalong)==length(nxdn)) {stop('Argument along (',paste(along,collapse=', '),') does not relate to any dimension of x')}

  if (!length(frq_grp)) {
    dy=dx[,list(`_.obs_value`=FUN(`_.obs_value`,...)),by=nalong]
    if (!length(nalong)) { return(as.numeric(dy[[1L]])) }
    attr(dy,'dcstruct') = .dimcodesrescue(xdn[nalong],attr(x,'dcstruct'))
    return(.md3_class(dy))
  }


  #so frq_grp must be something like list(G7=c('CA', 'DE','ES','FR','IT','JP','UK', 'US')) or list(geo=list(G4=c('ES','DE','FR','IT')))
  #start user checks frq_grp
  if (!is.list(frq_grp)& length(along)==1) {
      frq_grp=list(list(frq_grp));
      names(frq_grp) = along
  } else if (!is.list(frq_grp[[1L]]) & length(along)==1) {
    frq_grp=list(frq_grp);
    names(frq_grp) = along
  } else if (is.list(frq_grp)) {
    if (any(!unlist(lapply(frq_grp,length)))) {
      for (i in which(!unlist(lapply(frq_grp,length)))) frq_grp[[i]]=list()
    }
  } else {
    temp=lapply(lapply(dimnames(x), head,2),function(z) paste0('list(X=c("',z[1],'","',z[length(z)],'"))'))
    stop('You specified to aggregate along ',length(along),' dimensions with specfic code groups (frq_grp), but did not specify frq_grp correctly\n',
         'try setting frq_grp to something like ', paste0('list(', paste(unlist(lapply(as.list(names(temp)),function(z) paste0(z,'=',temp[[z]]))),collapse=', '), ')'))
  }
  for (sdim in along) {
    if (!length(frq_grp[[sdim]])) next
    mygrp=frq_grp[[sdim]]
    if (is.null(names(mygrp))) {names(mygrp)=NA_character_ }
    if (anyNA(names(mygrp))) {  temp=names(mygrp); temp[is.na(temp)]=''; names(mygrp)=gsub('\\.','',make.names(temp,unique = TRUE))}
    if (any(grepl('\\.',names(mygrp)))) {warning('No dots in element names that you provide to argument frq_grp'); names(mygrp)=gsub('\\.','',names(mygrp))}
    ixix=unlist(lapply(mygrp,function(z) {is.numeric(z) || is.logical(z)}))
    if (any(ixix)) for (i in which(ixix)) { mygrp[[i]]=xdn[[sdim]][mygrp[[i]]]  }
    frq_grp[[sdim]]=mygrp
  }


  #end user checks frq_grp

  #here TIME as characterz


  ydn=xdn[nalong]
  for (sdim in along) {
    if (!length(frq_grp[[sdim]])) next
    sdict=unlist(lapply(as.list(names(frq_grp[[sdim]])), function(z) rep(z,length(frq_grp[[sdim]][[z]]))))
    names(sdict) = unlist(frq_grp[[sdim]], use.names = FALSE, recursive = FALSE)
    sadim=paste0('_.agg_',sdim)
    dx[[sadim]] <- sdict[dx[[sdim]]]
    dx = dx[!is.na(dx[[sadim]])]
    nalong=c(nalong,sadim)
    ydn[[sdim]] = names(frq_grp[[sdim]])
    dx[[sdim]]<-NULL
  }


  dy=dx[,list(`_.obs_value`=FUN(`_.obs_value`,...)),by=nalong]
  if (!length(nalong)) { return(as.numeric(dy[[1L]])) }
  colnames(dy)=gsub('^_\\.agg_','',colnames(dy))
  attr(dy,'dcstruct') = .dimcodesrescue(ydn,attr(x,'dcstruct'))
  my=.md3_class(dy)
  if(drop) my=drop.md3(my)
  return(my)
}






.tsdisagg=function (tso, to, proxy = NULL, conversion = "sum", method = "chow-lin-maxlog",
          truncated.rho = 0, fixed.rho = 0.5, criterion = "proportional",
          h = 1, ...)
{
  tso = as.vector(tso)
  myformu = as.formula(tso ~ proxy)
  method = method[[1L]]
  if (!length(proxy)) {
    if (is.null(proxy))
      method = "denton-cholette"
    if (is.na(match(method, c("uniform", "denton-cholette",
                              "denton")))) {
      method = "denton-cholette"
      warning("for disaggregation without a proxy, only few methods are possible. I have chosen denton-cholette for now.")
    }
    myformu = as.formula(tso ~ 1)
  }
  else {
    proxy = as.vector(proxy)
  }
  tsout = suppressWarnings(tempdisagg:::predict.td(tempdisagg::td(myformu,
                                                                  to = to, conversion = conversion, method = method, truncated.rho = truncated.rho,
                                                                  fixed.rho = fixed.rho, criterion = criterion, h = h)))
  if (!length(proxy))
    return(tsout)
  tspr = proxy
  tspr[] = NA
  tspr[index(tspr) %in% index(tsout)] = tsout
  return(tspr)
}

.timedisaggregate = function(x, frq, FUN = c(sum,mean,end,start), method = "chow-lin-maxlog",criterion = "proportional",...) {

  FUN=.FUNfixer(FUN)
  conversion='sum'
  if (length(body(FUN))) {
    if (any(grepl('head',head(body(FUN),10)))) { conversion='first'}
    if (any(grepl('tail',head(body(FUN),10)))) { conversion='last'}
    if (any(grepl('mean',head(body(FUN),10)))) { conversion='mean'}
  }


  if (!require('tempdisagg')) stop('temporal disaggregation requires package "tempdisagg" to be installed.')
  x=unflag(x)
  frq=toupper(trimws(frq[[1]]))
  vt=time.md3(x)
  vf=.timo_frq(vt)

    of=unique(vf)
  if (length(of)!=1) stop('mixed frq not allowed for disaggregation')
  ofb=.cttim$basedon(of); nfb=.cttim$basedon(frq)
  if (ofb!=nfb) { stop('x has frequency ',of,' (a multiple of ',ofb,') -  while frq is specified as ',frq, ' (a multiple of ',nfb,').\nCannot disaggregate into irergular subperiods')}
  xdn=.getdimnames(x)
  mz=as.zoo.md3(x)

  lz=lapply(mz,.tsdisagg,to=attr(ofb,'multiple')/attr(nfb,'multiple'),conversion=conversion,method=method,criterion=criterion,...)

  nt=seq(min(vt),max(vt),frq=frq)
  ntmatch=.timo_class(unlist(lapply(as.list(vt),rep,length(nt)/length(vt))))
  lt=lapply(lapply(mz,function(z) vt[!is.na(z)]),function(t)  nt[ntmatch %in% t])
  ly=lapply(names(lz),function(i) {y=data.table(.mdrest2codes(i),lt[[i]],lz[[i]]); colnames(y)=c(names(xdn),.md3resnames('value')); y})
  #.timo_subset(nt,lt[[1]])
  #y=.drop(as.md3(data.table(TIME=nt,as.data.frame(lz)),name_for_cols = setdiff(names(.dim(x)),'TIME')))
  dy=data.table::rbindlist(ly)
  ydn=xdn
  ydn[['TIME']] = nt
  attr(dy,'dcstruct') = .dimcodesrescue(ydn,.getdimcodes(x))
  .md3_class(dy)
}
#.timedisaggregate(euhpq[1,1,1:3,18:22],'M')
#ee=.timedisaggregate(eupop[1:3,1,1,12:15],'M')
#drop(as.md3(data.table(TIME=seq(timo('2005m01'),'2008m12'),as.data.frame(ee))))


#disaggregate(oecdgdp_aq['A.BG+RO.GDP.y2011:y2020'],'Q',FUN=sum)



#' Disaggregate an MD3 over time and/or  other dimensions
#'
#' Can decompose sums, averages, etc. to higher frequencies, or to constituent parts (e.g. countries in country groups)
#' @param x an md3 object,
#' @param frq_grp what to disaggregate to: A single character frequency code like \code{"M"},  \code{"W"} or  \code{"B"} when aggregating along time, or a user-defined aggregate list (see \link{frequency.timo})
#' @param along character vector: the dimensions along which to aggregate. Default is \code{TIME} - Note that all otehr dimensions are not possible for the moment
#' @param FUN the function to use for aggregation. Default is \code{sum}, but \code{mean},  \code{end} and  \code{start} are also possible (passed along as a function or the string name of that function)
#' @param \dots other arguments to FUN
#' @return an md3 object. Note that any flags or observation metadata from the original MD3 will be dropped and not contained in the resulting md3.
#' @seealso \code{\link{fill.md3}}, \link{indexMD3}, \code{\link{imputena}}, \code{\link{aggregate.md3}}
#' @examples
#' #data("oecdgdp_aq")
#'
#' #Take some quarterly data: e.g. house price index for Austria and Belgium
#' x0=euhpq[TOTAL.I15_Q.AT+BE.y2020:y]
#' x0
#'
#' #now disaggregate to monthly data
#' x1=disaggregate(x0,'M', FUN=mean)
#' x1
#' plot(x1)
#'
#'
#' @export
disaggregate = function(x, frq_grp, along='TIME', FUN = c(sum,mean,end,start), ...) {
  if (!length(along)) stop('along needs to be specfied')
  if (length(along)>1) { warning('along needs to be a single dimension.'); along=along[1]}
  if (is.numeric(along) || is.logical(along)) { along=names(.dim(x))[along]}
  along=trimws(as.character(along))
  if (!(along %in% names(.dim(x)))) stop('along="',along, '" cannot be found among dimension names of x')
  if(along!='TIME') stop('only disaggregating along TIME works for now')
  .timedisaggregate(x,frq=frq_grp,FUN,...)

}
