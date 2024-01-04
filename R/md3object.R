#problem eupop=mds("Eurostat/demo_pjanbroad",ccode='iso2m')
#problem oecdgdp_aq[1:3,1,'2019q1']




.onLoad <- function(libname, pkgname) {
  suppressPackageStartupMessages(require(data.table,quietly = TRUE))
  requiresilent=function(...) {
    suppressMessages(suppressPackageStartupMessages(require(...)))
  }
  if (!requiresilent("MDcountrycode")) warning("MD3 depends a lot on package MDcountrycode. Please install it and then reload MD3.")
  #if (!require("zoo")) warning("MD2 depends a lot on package zoo. Please install it and then reload MD0.")
  if (!requiresilent("data.table")) warning("MD3 depends a lot on package data.table. Please install it and then reload MD3.")
  if (!requiresilent("bit64")) warning("MD3 depends a lot on package bit64. Please install it and then reload MD3.")
  #message('hihi')
}


.md3resnames = function(inname)  {
  #helper function to convert auxiliary column names
  if (substr(inname,0,2)=='_.') return(inname)
  inname=tolower(inname)
  inname=gsub('_[0-9]$','',gsub('^obs','',gsub('^obs_','',gsub('^_','',gsub('^\\.','',inname)))))
  inname=tolower(inname)
  if (any(grepl('^flag',inname))) {inname='status'}
  if (any(grepl('^conf',inname))) {inname='conf'}
  if (!inname %in% c('value','status','conf','pre_break','footnote','qual','ref','type')) warning('.obs should be value, status (flag), or conf')
  return(paste0('_.obs_',tolower(inname)))
}







.md3_class = function(x, force=FALSE , dn=NULL) {
  #assign md3 class to data.table
  attr(x,'class') = c('md3', attr(x,'class'))
  if (force) { attr(x,'class') = c('md3', 'data.table', 'data.frame') }
  if (is.null(dn)) { return(x)}
  attr(x,'dcstruct') = .dimcodesrescue(dn)
  x
}

.dt_class = function(x,...,aschar=FALSE) {
  #takes some x and classes it as data.table
  if (aschar) {ixt=.dn_findtime(x); if (ixt>0) x[[ ixt]] = as.character(x[[ixt]])}
  class(x) = c('data.table', 'data.frame')
  x
}


.md3_is = function(x) {
  return(inherits(x,'md3'))
}


#' @export
as.md3.array = function(x,...) {
  .stackeddf2md3(.arraymelt(x))
}

.stackeddf2md3 = function(x,isdf=NA,order=FALSE) {
  if (data.table::is.data.table(x)) {

    ixf = grep('FRE?Q',toupper(colnames(x)))
    if (length(ixf)) { if (all(nchar(x[[ixf]])==1)) { x = x[,-ixf,with=FALSE]} }
    if ('v_a_l' %in% colnames(x)) { colnames(x)[colnames(x)=='v_a_l'] =.md3resnames('value') }

    vix = match(.md3resnames('value'), colnames(x),nomatch = 0)
    if (vix==0) { vix=which(sapply(x,is.numeric)); if (length(vix)!=1) stop('Could not clearly identify which col is the data col') }
    if (!is.na(vix)) {
      dix = 1:(vix-1)
      tix = match('TIME', colnames(x))

      #if (!is.na(tix) & tix != vix-1) stop ('time issue')
      if (order) setkeyv(x, colnames(x)[dix])
      if (!is.na(tix)) {
        try(x[[tix]] <- as.timo(x[[tix]]),silent=TRUE)
      }
      if (data.table:::anyDuplicated.data.table(x[,dix, with=FALSE])) stop ('duplicates in obs identifiers')
      dcsimp =lapply(as.list(x)[1:(vix-1)],unique)
      if (!is.na(tix)) { dcsimp[[tix]] = .timo_class(dcsimp[[tix]]); dcsimp[[tix]]=sort(dcsimp[[tix]]) }
      attr(x, 'dcsimp') = dcsimp
      attr(x, 'dcstruct') = .dimcodesrescue(dcsimp)
      x=x[!is.na(x[[.md3resnames('value')]]),]
      return(.md3_class(x))
    }
  } else if  (is.data.frame(x) & missing(isdf)) {
    return(.stackeddf2md3(data.table:::as.data.table.data.frame(x),isdf=TRUE))
  }
  stop('Converting to md3 did not work out')
}


.namedvecfrommat = function(x,ix) {
  #extracts a column from a matrix or data.frame that is named with rownames
  ix=ix[[1]]
  y=x[,ix,drop=TRUE]
  names(y)=rownames(x)
  y

}

.dotsaslist =function(...) {
  ctarg=nargs()
  ix=try(list(...),silent=TRUE)
  if (class(ix)!='try-error') { return(ix)}
  ix=list()
  if (ctarg>0) {
    for (i in 1:ctarg) {
      if (eval(parse(text=paste0("missing(..",i,")")))) {
        ix[[i]] = integer(0)
      } else {
        ix[[i]] =suppressWarnings(try(eval(parse(text=paste0("..",i))),silent=TRUE))
        if (grepl("error",class(ix[[i]])[[1]])) { return(list(gsub("\\s","",deparse(substitute(...))))) }

      }
    }
  }
  return(ix)
}



#' Converts factors to strings in a data.frame or list
#'
#'
#' @param data a data .frame or other list
#' @return a data.frame/list
#' @seealso \code{\link{as.character}}
#' @examples
#' data("ToothGrowth")
#' ToothGrowth[["supp"]]
#' tg1=.fixfactors(ToothGrowth)
#' tg1[,"supp"]
#' @export
.fixfactors = function(data) {
  dcl = sapply(data,class)
  if (any(grepl("factor",dcl))) {
    for (i in which(dcl %in% "factor")) {
      data[,i]= as.character(data[,i])
    }
  }
  return(data)
}



 .asget = function (as = c("md3", "array", "numeric","data.table","zoo","2d","1d","pdata.frame"),defaultas="md3") {
  possibs=c("md3", "array", "numeric","data.table","zoo","2d","1d","pdata.frame")
   if (length(as)> 1L) { as = as[[1]] }
  if (!length(as)) {return(defaultas)}
  if (is.na(as)) {return(defaultas)}
   as=trimws(as)

   ix=pmatch(tolower(as), possibs, nomatch = 0)
   if (ix>0) {
     as = possibs[[ix]]
     return(as)
   }
   as=tolower(as)
   if (substr(as,0,1)=='a') return("array")
   if (substr(as,0,1)=='n') return("numeric")
   if (substr(as,0,1)=='d') return("data.table")
   if (substr(as,0,1)=='z') return("zooreg")
   return("md3")

}




#### CODE MANAGEMENT
#' Concatenates columns in a matrix into a SXDM Restful code equivalent
#'
#'
#' @param x a data.frame or matrix
#' @return a character vector
# @seealso \code{\link{as.character}}
#' @examples
#' dd=cbind(LETTERS,letters,rev(letters))
#' .mdcodes2rest(dd)
#' @export
.mdcodes2rest = function(x) {
  if (!length(x)) { return(character(0))}
  if (is.data.table(x)) { if (any(colnames(x)=='TIME')) {x[['TIME']]=as.character(x[['TIME']])}}
  do.call(paste, c(as.data.frame.matrix(x,stringsAsFactors=FALSE), sep="."))
  #apply(as.data.frame(x,stringsAsFactors=FALSE),1,paste,collapse=".")
}



.mdrest2codes = function(vix,defaultlength=NULL,asDT=FALSE) {
  #returns a matrix with acharacter codes
  if (!length(vix)) {
    if (!is.null(defaultlength)) { return(matrix(NA,0,defaultlength))}
    return(character(0))
  }
  splitix=strsplit(vix,split="\\.",perl=TRUE)
  dimmax=max(sapply(splitix,length))
  ixna=sapply(vix,function(x) length(x)==1L & anyNA(x))
  if (any(ixna)) { if (dimmax>1) { splitix[ixna] = list(rep(NA_character_,dimmax)) }  }
  xout=matrix(unlist(splitix,recursive = FALSE,use.names = FALSE),ncol = length(splitix[[1]]),byrow = TRUE)
  if (!asDT) return(xout)
  dout=data.table(xout)
  if (NROW(dout)>0) { if (grepl('^[1-2]',dout[[NCOL(dout)]][1L])) {
    dout[[NCOL(dout)]]=.char2timo(dout[[NCOL(dout)]],guess = FALSE)
    }}
  dout
}

.arrInd2rest = function(dn,x=NULL) {
  #returns a vector with character IDs
  #x: a matrix with integer indexes
  #.dimnames: dcsimp object where codes should be searched for
  # EXAMPLE: .arrInd2rest(list(LETTERS[1:4],c('R','T')))
  #.dimnames=.dc2dn(dn)
  out=NULL
  if (is.null(x)) {
    for (i in (seq_along(dn))) {
      out=unlist(lapply(as.list(dn[[i]]),function(y) stringr:::str_c(out,y,sep=".")),recursive = FALSE,use.names = FALSE)
    }
    return(out)
  }

  for (i in 1:ncol(x)) {
    out=stringr:::str_c(out,dn[[i]][x[,i]],sep=".")
  }
  return(out)
}


.arrInd2names=function(x, .dimnames) {
  #returns a matrix with character IDs
  #x: a matrix with integer indexes
  #.dimnames: dcsimp object where codes should be searched for
  #EXAMPLE: .arrInd2names(matrix(rep(1:3,2),3),list(LETTERS[1:4],c('R','T')))
  .dimnames=.dc2dn(.dimnames)
  if (!is.matrix(x)) x=t(x)
  out=NULL
  for (i in 1:ncol(x)) {
    out=cbind(out,.dimnames[[i]][x[,i]])
  }
  return(out)
}





.trafoRestQuery = function(instring,ohihi=NULL,alsosingleton=TRUE) {
  #this function converts a RESTful query string (such as AT+BE.RR) into a  list (such as list(c('AT','BE'),'RR'))
  #instring: a query string (if vector, only first element is taken)
  #ohihi: a hihi object to be validated against
  #alsosingleton: If false, then return instring if it contains no a single '+'
  #EXample: .trafoRestQuery('RR.TT+YY')
  if (!is.character(instring)) if (!.timo_is(instring)) return(instring)
  instring2=instring[[1]]
  temp=trimws(strsplit(paste0(" ",instring2," "),split="\\.")[[1]])
  temp=as.list(temp)
  #temp=lapply(temp,function(x) {strsplit(gsub("\\%","+",x),split="\\+")[[1]]})
  temp=lapply(temp,function(x) {strsplit(x,split="\\+")[[1]]})
  frqshifter = character(0)
  if (!is.null(ohihi)) {
    ixt=match('TIME',toupper(names(ohihi)), nomatch = 0)
    if ((length(temp)== 1+length(ohihi)) & as.logical(ixt)) { ixt = ixt +1 }
    if (ixt> length(temp)) {if (length(temp)!= length(ohihi)) stop("md3 object has ",length(ohihi),  " dimensions, but you specified ",length(temp))}
    if (ixt>0) temp[[ixt]] = gsub("y","",temp[[ixt]])
    if ((length(temp)== 1+length(ohihi)) & as.logical(ixt)) {
      frqshifter=toupper(temp[[1]])
      if (length(setdiff(frqshifter,.cttim$frqcodes$fcode))) { stop('frequency code ', setdiff(frqshifter,.cttim$frqcodes$fcode), 'not permitted')}
      ixt=ixt-1

      temp=temp[-1]
    }
    for( i in 1:length(temp)) {
      if (.timo_is(ohihi[[i]])) {
        if (!length(temp[[i]]) & !length(frqshifter)) { temp[[i]] = ohihi[[i]]; next }
        if (!length(temp[[i]])) { temp[[i]] = ohihi[[i]] }
        if (length(temp[[i]])==1L) {
          if (any(grepl(':$',temp[[i]]))) { temp[[i]] = seq.timo(gsub(':$','',temp[[i]]),tail(ohihi[[i]],1)) }
          if (any(grepl('^:',temp[[i]]))) { temp[[i]] = seq.timo(head(ohihi[[i]],1), gsub(':','',temp[[i]])) }
        }
        temp[[i]] = .timo_subset(ohihi[[i]], temp[[i]], addifmiss = TRUE)
        if (length(frqshifter)) temp[[ixt]] = temp[[ixt]][.timo_frq(temp[[ixt]]) %in% frqshifter]
        next
      }
      ohihi[[i]] = as.character(ohihi[[i]])
      while (any(grepl(":",temp[[i]]))) {
        j = head(grep(":",temp[[i]]),1)
        bla = trimws(strsplit(temp[[i]][j],split=":")[[1]])
        if (length(bla)==1 & grepl(":$",temp[[i]][j])) { bla=c(bla,"") }
        if (length(bla)!=2) { stop("Indexing error: something wrong with using : in referring to codes, in ",temp[[i]][j])}
        if (bla[[1]]=="") { bla[[1]] = head(ohihi[[i]],1)}; if (bla[[2]]=="") { bla[[2]] = tail(ohihi[[i]],1)}
        bla=try(ohihi[[i]][match(bla[[1]],ohihi[[i]]): match(bla[[2]],ohihi[[i]])],silent=TRUE)
        if (any(grepl("error",class(bla)))) { stop("Indexing error: could not determine range for ", temp[[i]][j])}
        #browser()
        temp[[i]] = unname(c(temp[[i]][0:(j-1)], bla,temp[[i]][-(0:(j))]))
      }
    }

  }
  if (length(frqshifter)) { attributes(temp)[['frqshifter']]<-frqshifter}

  if (!alsosingleton) if(prod(sapply(temp,length))==1) {
    return(gsub("\\+","",gsub(":","",instring)))
  }

  if (!is.null(ohihi)) {
    if (length(temp)!= length(ohihi)) stop("md object has ",length(ohihi),  " dimensions, but you specified ",length(temp))
    names(temp) = names(ohihi)

  } #perhaps not needed
  #temp=lapply(temp,function(x) x[x!=""])
  return(temp)
}

.rnfromdim = function(ohihi) {
  myn=ohihi[[length(ohihi)]]
  for (i in rev(seq_along(ohihi))[-1]) {
    myn = unlist(lapply(as.list(ohihi[[i]]),paste,myn,sep="."))
  }
  unname(myn)
}


### CORE NAMED VECTORS

.naomitvec = function (object, ...) {
  #this is a fast function to omit NAs, yet keep names
  if (!is.atomic(object))
    return(object)
  if (length(dim(object)) > 0L)
    return(object)
  omit <- seq_along(object)[!is.finite(object)]
  if (length(omit) == 0L)
    return(object)

  nm <- names(object)
  object <- object[-omit]

  object
}

#".setnamesvec<-" = function(x,value,ix=NULL) {
#  if (is.null(ix)) ix=seq_along(value)
#  if (is.null(attr(x,"names"))) {attr(x,"names")=character(0)}
#  .Call(data.table:::Csetcharvec, attr(x, "names"), ix, value)
#  return(invisible(x))
#}

.naomitvecnew = function (object, ...)
{
  if (!is.atomic(object))
    return(object)
  if (length(dim(object)) > 0L)
    return(object)

  lout=data.table:::na.omit.data.table(list(object))
  vout=lout[[1]]
  attr(vout,"names")=character(0)
  .Call(data.table:::Csetcharvec, attr(vout, "names"), seq_along(vout), attr(lout,"row.names"))
  vout

}



.arrIndex = function(x,ixvec=NULL,drop=FALSE) {
  #allows to subset an array with a siongleton vector
  #an array
  #ixvec: a character vector with indexes, such as c(1,"","") or c("","AT","TOTAL")
  #tempix=grepl("[^0-9.]",ixvec) & grepl("^[^']",ixvec)
  #if (any(tempix)) ixvec[tempix]=paste0("'",ixvec[tempix],"'")
  eval(parse(text=paste0("x[",paste0(ixvec,collapse=","),",drop=",drop,"]")))
}



.arraystack = function(arr,ixf=integer(0), ixt=integer(0)) {
  if (is.vector(arr)) aa= as.array(arr)
  if (!length(ixf)) return(.arraystacksimple(arr))
  if (!length(ixt)) return(.arraystacksimple(arr))
  tvec=.timo_frq(.dimnames(arr)[[ixt]])
  ixvec = character(length(.dim(arr)))
  aout = NULL; ixf=ixf[[1]]
  for (ff in .dimnames(arr)[[ixf]]) {
    ixvec[[ixf]] = paste0('"',ff,'"')
    ixvec[[ixt]] = paste0("c(",paste(which(names(tvec) == ff),collapse=","),")")
    aout=c(aout,.arraystacksimple(.arrIndex(arr,ixvec)))
  }
  aout
}





.arraystacksimple = function(aa) {
  #this function takes an array and puts it into a named vector
  #if (is.vector(aa)) aa= as.array(aa)
  a2=as.vector(aa)
  lix=dimnames(aa)
  if (any(unlist(lapply(lix,length))!=dim(aa))) { for (i in which(unlist(lapply(lix,length))!=dim(aa))) { lix[[i]] =paste0('X',seq_len(dim(aa)[[i]]))   }}
  names(a2) = .arrInd2rest(lix)
  a2
}


.arraymelt = function(aa,obs='_.obs_value',split='\\.') {
  #this function takes an array and puts it into a named vector
  #if (is.vector(aa)) aa= as.array(aa)
  obs=.md3resnames(obs[[1]])

  lix=dimnames(aa)
  if (is.null(lix)) { lix = .fixhihi(NULL,dim(aa), ignore.time = FALSE)}
  if (is.null(attr(lix,'names'))) { tix0=rep(TRUE, length(lix))} else {tix0=is.null(names(lix))}
  if (any(tix0)) { names(lix)[tix0] = make.names(LETTERS[seq_along(lix)])[tix0]}
  if (any(unlist(lapply(lix,length))!=dim(aa))) {
    for (i in which(unlist(lapply(lix,length))!=dim(aa))) {
    lix[[i]] =paste0(substr(names(lix)[[i]],0,1),sprintf(paste0('%0',floor(log10(dim(aa)[[i]]))+1,'d'),seq_len(dim(aa)[[i]])))
    }
  }
  dimnames(aa)<-lix
  a3=.arraystacksimple(aa)


  nspl=strsplit(names(a3),split=split)
  dout=data.table(matrix(unlist(nspl),nrow = length(nspl),byrow=TRUE),a3)
  names(dout) = c(names(lix),obs)
  dout
}

.index1d2mdint = function(.dim, vindex) {
  vindex=as.vector(vindex)
  mydim = .dim
  if (any(vindex==0L,na.rm=TRUE)) vindex = vindex[-which(c(0,NA)==0)]
  if (any(vindex<0L,na.rm=TRUE)) {
    if (!all(vindex<1L,na.rm=TRUE)) { stop("Indexing error: only 0's may be mixed with negative subscripts")}
  }
  vindex=seq_len(prod(mydim))[vindex] # this is necessary to convert any indexes outside of seq_len(prod(mydim))
  return(t(arrayInd(vindex,mydim,useNames = FALSE)))
}































.fixhihi = function(hihi,.dim=NULL, ignore.time=TRUE) {
  #takes in a list that is supposed to dimnames-like
  # if there are no names for dims or elements, it makes names
  # it sorts the time dimension
  if (is.null(hihi)) hihi = lapply(as.list(.dim),seq_len)
  if (is.null(names(hihi))) {
    temp = seq_along(hihi); temp[1:min(26,length(temp))]=LETTERS[1:min(26,length(temp))]; names(hihi)=temp
  }

  if (!ignore.time) {
    timedim=match("time",tolower(names(hihi)))
    if (is.na(timedim)) {
      #if (!ignore.time) stop("could not find time dimensions")
    } else {
      hihi[[timedim]]=sort(hihi[[timedim]])
    }
  }
  for (i in seq_along(hihi)) {
    names(hihi[[i]]) = hihi[[i]]
  }
  return(hihi)
}



.mdsel2rest = function(lix) {


  sout=lix[[length(lix)]]

  for (i in rev(seq_along(lix))[-1]) {
    sout=unlist(lapply(as.list(lix[[i]]),paste,sout,sep="."),recursive = FALSE,use.names = FALSE)
    #sout=vapply(as.list(lix[[i]]),paste,character(length(sout)),sout,sep=".",USE.NAMES = FALSE)

  }
  return(sout)
}

.mdsel2codes = function(lix, asdt=TRUE, aschar=FALSE, bylast=FALSE) {

  .setdim = function(x,nbcol=1) {
    attr(x,'dim') = c(length(x)/nbcol,nbcol)
    x
  }
  if (bylast) {lix=rev.default(lix)}
  if (any(sapply(lix,length)==0)) {
    vout = matrix(NA_character_,0,length(lix),dimnames = list(character(0),names(lix)))
    if (asdt) vout = data.table::as.data.table(vout)
    if (bylast) {vout=vout[,NCOL(vout):1L,with=FALSE]}
    return(vout)
  }
  if (aschar) {lix=lapply(lix,as.character)}
  vout=as.character(lix[[length(lix)]])
  if (asdt) vout = data.table(lix[[length(lix)]])

  j=0
  for (i in rev(seq_along(lix))[-1]) {
    j = j+1

    if (asdt) {
      #vout = cbind(lix[[i]], rbindlist(rep(list(vout),length(lix[[i]]))) )
      vout = data.table:::cbind.data.table( .unlist_keepclass(lapply(as.list(lix[[i]]),rep,nrow(vout))), rbindlist(rep(list(vout),length(lix[[i]]))) )
    } else {
      vout = cbind(as.character(lix[[i]]),matrix(rep(vout,length(lix[[i]])),ncol = j, byrow=TRUE))
        stop('to be fixed')
    }
    #sout=unlist(lapply(as.list(lix[[i]]),cbind,sout,sep="."),recursive = FALSE,use.names = FALSE)
    #sout=vapply(as.list(lix[[i]]),paste,character(length(sout)),sout,sep=".",USE.NAMES = FALSE)

  }
  names(vout) = names(lix)
  if (bylast) {vout=rev.default(vout)}
  return(vout)
}

#.dn_findfreq =function(xdn) { if (!any(toupper(names(xdn))=='FREQ')) return(0) else return(match('FREQ',toupper(names(xdn))))}
.dn_findtime = function(xdn) { if (!any(toupper(names(xdn))=='TIME')) return(0) else return(match('TIME',toupper(names(xdn))))}
.dn_getfreq = function(...) return("")



.match2dim = function(ix,xdn,addifmiss=TRUE, frq=NULL, dotimosubset=TRUE) {
  #takes a list of indexes, matches against a hihi, and returns a dimname-like list of character codes,
  #ARGUMENTS:
  #ix: list of vectors that are character, integer or logical
  #xdn: dcsimp object to be matched against
  #addifmiss: if parts of ix are not in xdn, then xdn is expanded to take them in
  #OUTPUT
  #returns a hihi

  lix=list();
  missadded=FALSE
  #if (any(names(ix)=='TIME')) ix[[which(names(ix)=='TIME')]] = as.character(ix[names(ix)=='TIME'][[1]])
  #if (any(names(xdn)=='TIME')) xdn[[which(names(xdn)=='TIME')]] = as.character(xdn[names(xdn)=='TIME'][[1]])

  if (is.null(frq)) if (!is.null(attr(ix,'frqshifter'))) { frq=attr(ix,'frqshifter') }
  if (!is.null(frq)) ixt=.dn_findtime(xdn) else ixt=0
  for (i in 1:length(ix)) {
    if (length(ix[[i]])) {
      nvec=xdn[[i]]; names(nvec)=xdn[[i]]
      if (.timo_is(xdn[[i]]) ) {
        if (!.timo_is(ix[[i]])) if (length(ix[[i]])==1L) { if (any(grepl(':',ix[[i]]))) { ix[[i]] = .trafoRestQuery(ix[[i]],xdn[i])[[1L]] }}
        if (!is.null(frq)) { ix[[i]]=ix[[i]][.timo_frq(ix[[i]])==frq]}
        lix[[i]]=.timo_subset(nvec,ix[[i]], coverhigherfrqs = dotimosubset, addifmiss=TRUE) #hier eingreifen
        if (.timo_is(ix[[i]])) { lix[[i]] = unique.timo(c(ix[[i]],lix[[i]]))}
      } else {
        lix[[i]]=.subset(nvec,ix[[i]])
        if (is.character(ix[[i]])) if (any(grepl(":|\\+",ix[[i]]))) {
          lix[[i]] = .trafoRestQuery(ix[[i]],list(nvec))[[1L]]
        }

      }



      if (addifmiss & any(is.na(lix[[i]]))) {
        missadded=TRUE

        if (is.numeric(ix[[i]])) { #deal with stuff like c(4,1334)
          tempix=xdn[[i]][ix[[i]]]; tempix[is.na(tempix)] = as.character(ix[[i]][is.na(tempix)])
          ix[[i]]=tempix
        }
        lix[[i]]=c(as.vector(na.omit(lix[[i]])),
                   ix[[i]][!(ix[[i]] %in% xdn[[i]])])
        if (mode(ix[[i]])!="character") {warning("part of ",mode(ix[[i]])," indexes ", "cannot be found. Better use character codes to create new elements. \nFor now, I created elements with names such as '",tail(lix[[i]],1),"'")}
      }
    } else {
      #lix[[i]]=.subset(xdn[[i]])
      lix[[i]]=xdn[[i]]
    }
    if (ixt==i) {
      lix[[i]]= lix[[i]][frequency.timo(lix[[i]]) %in% toupper(frq)]
    }

  }

  names(lix) = names(xdn)

  if (any(names(lix)=='TIME')) {
    ixt=which(names(lix)=='TIME'); ixtnna=!is.na(names(lix[[ixt]]))
    lix[[ixt]][ixtnna] = .char2timo(lix[[ixt]][ixtnna])
    if (any(!ixtnna)) lix[[ixt]][!ixtnna] = .char2timo(lix[[ixt]][!ixtnna])
    #!? to be checked later
  }
  #  if (missadded) lix=.mdcheckcodes(lix,sorttime = FALSE,stopatwrongfrqpair=TRUE)
  return(lix)
}



.oldmatch2dim = function(ix,xdn,addifmiss=TRUE) {
  #takes a list of indexes, matches against a hihi, and returns a dimname-like list of character codes,
  #ARGUMENTS:
  #ix: list of vectors that are character, integer or logical
  #xdn: hihi object to be matched against
  #addifmiss: if parts of ix are not in xdn, then xdn is expanded to take them in
  #OUTPUT
  #returns a hihi

  lix=list();
  missadded=FALSE
  if (any(names(ix)=='TIME')) ix[[which(names(ix)=='TIME')]] = as.character(ix[names(ix)=='TIME'][[1]])
  if (any(names(xdn)=='TIME')) xdn[[which(names(xdn)=='TIME')]] = as.character(xdn[names(xdn)=='TIME'][[1]])
  for (i in 1:length(ix)) {
    if (length(ix[[i]])) {
      nvec=xdn[[i]]; names(nvec)=xdn[[i]]
      lix[[i]]=.subset(nvec,ix[[i]])
      if (is.character(ix[[i]])) if (any(grepl(":|\\+",ix[[i]]))) {
        lix[[i]] = .trafoRestQuery(ix[[i]],list(nvec))[[1L]]
      }


      if (addifmiss & any(is.na(lix[[i]]))) {
        missadded=TRUE
        lix[[i]]=c(as.vector(na.omit(lix[[i]])),
                   ix[[i]][!(ix[[i]] %in% xdn[[i]])])
        if (mode(ix[[i]])!="character") {warning("part of ",mode(ix[[i]])," indexes ", "cannot be found. Better use character codes to create new elements. \nFor now, I created elements with names such as '",tail(lix[[i]],1),"'")}
      }
    } else {
      #lix[[i]]=.subset(xdn[[i]])
      lix[[i]]=xdn[[i]]
    }
  }

  names(lix) = names(xdn)
  if (any(names(lix)=='TIME')) lix[[which(names(lix)=='TIME')]] = .char2timo(lix[[which(names(lix)=='TIME')]])
#  if (missadded) lix=.mdcheckcodes(lix,sorttime = FALSE,stopatwrongfrqpair=TRUE)
  return(lix)
}

.matchgen2dim = function(ix1,ix2,xdn, permittedfrq=NULL) {
  #merges indexes from ix1 and ix2, and validates against xdn
  fixix=function(ix,extchec=TRUE) {
    if (is.null(names(ix))) { names(ix) = names(xdn)[seq_along(ix)]}
    temp = ix[names(xdn)];
    if (any(is.na(names(temp)))) {
      if (any(!(names(ix) %in% names(xdn)))) {
        names(temp)[is.na(names(temp))] = names(ix)[!(names(ix) %in% names(xdn))]
      } else {
        names(temp)=names(xdn)
      }
    }

    ix = lapply(temp,function(x) if (is.null(x)) numeric(0) else x)
    #fdim=.dn_findfreq(ix);

    #if (length(fdim)) {
    #  tdim=.dn_findtime(ix)
    #  if (!length(ix[[fdim]]) & length(ix[[tdim]])) { ix[[fdim]] = .dn_getfreq(ix[[tdim]])  }
    #}

    if (extchec) ix = .mdcheckcodes(ix,stopatwrongfrqpair=TRUE) else ix = .fixhihi(ix)

    return(ix)
  }
  ix1=fixix(ix1, TRUE)
  ix2=fixix(ix2, TRUE)

  dimnotin2 = names(ix1)[!(names(ix1) %in% names(ix2))]
  if (length(dimnotin2)) stop("superfluous dimension '",dimnotin2[[1]],"' supplied. I do not know what to do with that.")

  for (i in names(ix2)) {
    if (!length(ix1[[i]])) {
      ix1[[i]]=ix2[[i]]
    } else {
      if (length(ix2[[i]])) {
        if (!all(ix1[[i]]==ix2[[i]])) warning("names in dimension ", i, " do not match")
      }
    }
  }
  for (i in seq_along(ix1)) { if (!length(ix1[[i]])) { ix1[[i]]=xdn[[i]]}}
  return(ix1)
}




.md3getasdt = function(x,...,drop=TRUE,as=c("md3","array","numeric","data.table"), DT=TRUE, .obs='value') {
  y=data.table:::`[.data.table`(.dt_class(x),...)
  .obs=.md3resnames(.obs[[1L]])
  as = .asget(as[1L])
  if (as=='data.table') {  attributes(y)[['dcstruct']] = NULL; return(y)}
  if (as=='data.frame') { return(as.data.frame(y))}
  if (!drop) { return(.md3_class(y)) }

  mdc=mdcold=.dc2dn(attributes(y)[['dcstruct']])
  mdc=mdc[names(mdc) %in% colnames(y)]
  if (length(mdc)) {
    for (i in seq_along(mdc)) { mdc[[i]] = intersect(mdc[[i]],unique(y[[i]]))}
     if (length(mdc[['TIME']])) { mdc[['TIME']] = .timo_class(mdc[['TIME']])}
       attributes(y)[['dcstruct']] = .dimcodesrescue(mdcmdcold)
  }

  y =.drop(y)
  if (as=='md3') {  return(y) }
  mdc=.dc2dn(attr(y,'dcstruct'))
  if (!length(mdc)) {return(rep(y[[.obs]],0))}
  ll = list(numeric=NA_real_,logical=NA, character=NA_character_)
  aout=array(ll[[mode(x[[.obs]])]],
             dim=sapply(mdc,length),dimnames=lapply(mdc,as.character))
  #aout[.mdrest2codes(names(x[[obs]]),defaultlength = length(xdn))] = x[[obs]]
  aout[as.matrix(.dt_class(y, aschar=TRUE)[,names(mdc),with=FALSE])] = y[[.obs]]
  aout


}

.md3get = function(x,...,drop=TRUE, as=c("md3","array","numeric","data.table"), .obs="value", DT=FALSE) {


  obs=.md3resnames(.obs[[1L]])
  asnaifmissing = ifelse(missing(as), NA, as)

  if (!is.na(asnaifmissing)) asnaifmissing=as
  as = .asget(as[1])
  if (obs!='_.obs_value') { if (as=='md3') {as='array'}}
  #if (obs!=.md0resnames("value") & as=="md0") { as="array"}



  if ((!missing('DT') & any(as.logical(DT)!=FALSE)) | any(all.names(match.call(expand.dots = TRUE))=='DT')) {
    return(.md3getasdt(x,...,drop=drop,as=as,.obs=.obs))
  }

  subsetdonealready=FALSE #important for selecting from mixed frqs



  ix=.dotsaslist(...)

  if (length(ix)>1L) if (all(sapply(ix,length)==1L)) { #just for speed, redirect singelton requests
    if (all(sapply(ix,is.character))) {
      reqr=stringr:::str_c(unlist(ix),collapse = ".")
      if (!any(grepl(':|\\+',reqr))) { return(.md3getelem(x,reqr,.obs = .obs,drop = drop, as=asnaifmissing) )}
    }
  }

  #xdn=.fixhihi(attr(x,"hihi"),ignore.time = FALSE)
  xdn=.getdimnames(x,TRUE); xdc=.getdimcodes(x)
  class(x) = "list"
  if (obs!='_.obs_value') { if (!match(obs,names(x),nomatch = 0))  {
    x=data.table::copy(x)
    x[[obs]]=NA_character_

  }}


  if (!length(ix) | (length(ix)==1 & missing(..1))) {

    xasarray = function(.obs="value") {
      obs=.md3resnames(.obs[[1]])
      if (!length(xdn)) { return(numeric(0))}
      #ll = list(numeric=numeric(0),logical=logical(0), character=character(0))
      ll = list(numeric=NA_real_,logical=NA, character=NA_character_)
      aout=array(ll[[mode(x[[obs]])]],
                 dim=sapply(xdn,length),dimnames=lapply(xdn,as.character))
      #aout[.mdrest2codes(names(x[[obs]]),defaultlength = length(xdn))] = x[[obs]]
      aix=as.matrix(.dt_class(x, aschar=TRUE)[,names(xdn),with=FALSE])
      aout[aix] = x[[.obs]]
      if (length(.dn_getfreq(xdn))>1) {
        aout = drop(.mdfoldfreqarray(aout))
      }

      return(aout)
    }



    if (as == "array") {
      xout=try(xasarray(obs),silent=TRUE)
      if (any(grepl('err',class(xout)))) stop('Error in displaying md3 object as array. The object likely contains observations whose dimension codes are not covered by dimnames(x)')
      return(xout)
    } else if (as == "md3") {
      #class(x) = "md3"
      return(.md3_class(x))
    } else if (as == "data.table") {
      x=.dt_class(x); attributes(x)[['dcstruct']]<-NULL
      return(x)
    } else  { #as=='numeric'
      temp=xasarray(.obs); tempdn=NULL
      if (length(dim(temp))==1L) {
        tempdn=attr(temp,'dimnames')[[1]]
        dim(temp)=NULL; names(temp) = tempdn
      }
      return(temp)
    }
    #if (!any(grepl("[^0-9]",head(rownames(x))))) { x=.setrnmdmd(x)}
  }


  if (length(ix)==1) {
    #if (is.md3(ix[[1]])) {ix[[1]] = names(which(.subset2(ix[[1]],.md0reserved[["value"]])))}
    if (!is.null(dim(ix[[1]]))) if(length(dim(ix[[1]]))==2 & ncol(ix[[1]])==length(xdn)) { # if indexed with a matrix
      if (!(mode(ix[[1]]) %in% c("integer","numeric"))) {
        ix[[1]] = apply(ix[[1]],1,paste,collapse=".")
      }
    }
    if (length(ix[[1]])==1) {
      if (is.character(ix[[1]])) {
        ix[[1]] = .trafoRestQuery(gsub("\\s","",ix[[1]]),xdn,alsosingleton=TRUE)
        subsetdonealready=TRUE
      }


    }
    if (is.list(ix[[1]]) ) {

      ix =ix[[1]]
    } else {

      return(.md3getelem(.dt_class(x),ix[[1]],drop = drop,.obs=obs, as=asnaifmissing))

    }


  }






  if (all(sapply(ix,class)=="data.frame")) { ix = lapply(ix,"[[","code") } #if dimcodes instead of hihi has been supplied

  if (length(ix)!= length(xdn)) {
    if (length(ix)!= 1+length(xdn)) stop("md3 object has ",length(xdn),  " dimensions, but you specified ",length(ix))
    frqshifter = .trafoRestQuery(ix[[1L]])[[1L]]
    if (length(setdiff(frqshifter,.cttim$frqcodes$fcode))) { stop("md3 object has ",length(xdn),  " dimensions, but you specified ",length(ix)) }
    ix = ix[-1]
    attr(ix,'frqshifter') = frqshifter
  }

  #lix=.match2dim(ix,xdn,addifmiss = TRUE) #initially, this was set to FALSE
   lix=.match2dim(ix,xdn,addifmiss = TRUE,frq = attr(ix,'frqshifter'), dotimosubset = !subsetdonealready)
  #lix = .mdfixindexfreq(lix,stopifwrong = FALSE)

  if (as %in% c('md3','data.table','data.frame')) {
    if (obs=='_.obs_value') {
      dx=.dt_class(x,aschar=FALSE)[.mdsel2codes(lix,aschar=FALSE),,on=.NATURAL]
    } else {
      dx=.dt_class(x,aschar=FALSE)[.mdsel2codes(lix,aschar=FALSE),c(names(lix),obs),on=names(lix),with=FALSE]
    }
    dx=dx[!is.na(dx[[obs]])]
    x = .md3_class( dx, dn=.dimcodesrescue(lix,xdc));
    if (drop) x= .drop(x)
    if (as=='data.table') { return(.dt_class(x))}
    if (as=='data.frame') { return(data.table:::as.data.frame.data.table(.dt_class(x)))}
    if (.md3_is(x)) if (data.table:::dim.data.table(x)[1]==0) {
     # return(rep(NA_real_,prod(unlist(lapply(lix,length)))))
    }
    return(x)
  }


  #tempidx=.mdrest2codes(tempix)
  aout=array(numeric(0),dim=sapply(lix,length),dimnames=lapply(lix,as.character))
  #aout[tempidx]=subset(x[[.md0reserved[["value"]]]],tempix)
  #aout[tempidx]=.subset(x[[obs]],tempix)
  tempdt = .dt_class(x, aschar=TRUE)[.mdsel2codes(lix,aschar=TRUE),,on=names(lix)]
  if (.dn_findtime(lix)>0) tempdt[[match('TIME',colnames(tempdt))]] = as.character(tempdt[[match('TIME',colnames(tempdt))]])
  aout[as.matrix(tempdt[,names(lix), with=FALSE])]=tempdt[[obs]]
  if (drop) aout=drop(aout)
  if (as=='numeric') {return(as.numeric(aout))}
  return(aout)



}







.mdindexelem = function(vix,dn) {
  #takes a vector vix (e.g. 1:2, c(T,F), or "Q.E.A.F"), or an integer matrix with nb rows = length(dn)
  #and returns a character vector of restful codes

  dnorig=dn
  mydimorig=mydim = sapply(dn,length)
  #ixfrq=.dn_findfreq(dn)
  ixfrq=0
  errifnot=TRUE
  #if (ixfrq>0) {mydim=mydim[-ixfrq]; dn=dn[-ixfrq]}

  if (length(vix)==1) if (is.na(vix)) return(NA_character_)

  if (is.logical(vix)) {
    if (length(vix)<prod(mydim))
      if (prod(mydim)%%length(vix) != 0) warning("index has length ",length(vix)," and length of x (",prod(mydim),") is not a multiple of that")
    vix=rep(vix,prod(mydim)/length(vix))
    vix=which(vix,arr.ind = TRUE,useNames=FALSE)
  }


  if (is.atomic(vix) & is.null(dim(vix))) {



    if (is.numeric(vix)) { vix=as.integer(vix) }

    if (is.integer(vix)) {
      if (length(vix)==1) {
        #if(!is.na(vix) & vix==0) { return(character(0)) }
        if(vix==0) { return(character(0)) }
      } else if (length(vix)==0) {
        return(character(0))
      }
      intrng=range(vix,na.rm=TRUE,finite=TRUE)
      if (prod(sign(intrng)) < 0) stop("only 0's may be mixed with negative subscripts")
      if (intrng[[1]]>prod(mydim)) return(rep(NA,length(vix)))
      if (-intrng[[2]]>prod(mydim)) vix=(integer(0))


      vix=.index1d2mdint(mydim,vix)
      if (mode(vix)=="list") { idx=character(0)}



      #idx=.index1d2mdint(mydim[-match("FREQ",names(mydim),nomatch = 0)],vix) #!!!???!!!



      #tempix=apply(as.data.frame(lix,stringsAsFactors = FALSE),1,paste,collapse=".")
      #tempix=apply(as.data.frame(lapply(lix,"names<-",NULL),stringsAsFactors = FALSE),1,function(x) if(any(is.na(x))) NA else paste(x,collapse="."))

    } else if (is.character(vix)) {
      tempix=vix
    } else {
      if (errifnot) {stop("problem with indexing - index must be integer, logical, or character")} else {return(NULL)}
    }

  }

  if (is.matrix(vix)) {
    if (is.character(vix)) {
      tempix=.mdcodes2rest(vix)
    } else if (is.integer(vix)|is.numeric(vix)) {
      #lix=list()

      lix=as.data.frame(matrix(NA_character_,ncol(vix),length(dn)+sign(ixfrq)),stringsAsFactors = FALSE)
      colnames(lix)=names(mydimorig)
      #browser()
      if (ixfrq>0) {
        if (nrow(vix)==length(mydimorig)) { vix=vix[-ixfrq,,drop=FALSE]}
        ixt=.dn_findtime(dnorig)
        fvec=names(.timo_frq(dnorig[[ixt]]))
        rownames(vix) = names(mydim)
        for (i in names(mydimorig)) {
          #if (i!="FREQ") lix[[i]]=dnorig[[i]][vix[i,]]
          lix[[i]]=dnorig[[i]][vix[i,]]
          if (i=="TIME") lix[[ixfrq]]=fvec[vix[i,]]
        }
      } else {
        for (i in seq_along(mydimorig)) {
          lix[[i]]=dnorig[[i]][vix[i,]]

        }
      }
      tempix = do.call(paste,c(lix,sep="."))
      if (anyNA(lix)) {
        tempix[apply(lix,1,anyNA)]=NA
      }

    } else {
      lix = .match2dim(as.list(as.data.frame(vix)),dn,FALSE)
      tempix = apply(as.data.frame(lapply(lix,"names<-",NULL),stringsAsFactors = FALSE),1,function(x) if(any(is.na(x))) NA else paste(x,collapse="."))
      #tempix = apply(as.data.frame(lapply(lix,"names<-",NULL),stringsAsFactors = FALSE),1,paste,collapse=".")
    }

  } else if (!is.character(vix)) {
    if (errifnot) {stop("not implemented")} else {return(NULL)}

  }



  return(tempix)
}



.md3getelem = function(x,vix,drop=TRUE,.obs="_.obs_value",as=c('numeric','md3','data.table')) {
  #work.horse function for [.md0 when only the first index was supplied
  #class(x) = "data.frame"
  as=.asget(as,defaultas = 'numeric');
  if (is.na(as)) { as = 'numeric'}
  errifnot = TRUE
  mydn = .getdimnames(x,TRUE); mydc=.getdimcodes(x)
  #  mydn = .fixhihi(attr(x, "hihi"))
  #mydc = .getdimcodes(x)
  tempix = .mdindexelem(vix, mydn)
  if (!length(tempix)) {
    if (drop)
      return(numeric(0))
    else return(.mdget(x, as = as))
  }
  ixt = .dn_findtime(mydn)
  ixf = 0 #.dn_findfreq(mydn)
  ff=data.table(.mdrest2codes(tempix))
  ff[apply(ff,1,anyNA)]<-NA
  if (ixt>0) ff[[ixt]] = as.timo(ff[[ixt]])
  if (ixf==1) { ff=cbind(FREQ=.timo_frq(ff[[ixt-1]]), ff)} else if (ixf>1) stop('FREQ must be first dimension')
  colnames(ff) = names(mydn)

  colnames(ff) <- names(mydn)
  for (i in seq_along(mydn)) { mydn[[i]] = mydn[[i]][mydn[[i]] %in% ff[[i]]]}
  if (as=='md3') {
    x=(.md3_class(.dt_class(x)[ff,,on=.NATURAL],dn =.dimcodesrescue(mydn,mydc)))
    if (!drop) {return(x)}
    return(.drop(x))
  }
  x = .dt_class(x)
  if (as=='data.table') {  return(x[ff,,on=.NATURAL]) }
  .obs=.md3resnames(.obs)
  myvec=x[ff,,on=.NATURAL][[.obs]]

  if (ixf>0) tempix=paste0(ff[['FREQ']],'.' ,tempix)
  names(myvec)=tempix
  return(myvec)


}




















.md3_printasdt = function (x, topn = getOption("datatable.print.topn"),
          nrows = getOption("datatable.print.nrows"), class = getOption("datatable.print.class"),
          row.names = getOption("datatable.print.rownames"),
          col.names = getOption("datatable.print.colnames"),
          print.keys = getOption("datatable.print.keys"), trunc.cols = getOption("datatable.print.trunc.cols"),
          quote = FALSE, timezone = FALSE, ...)
{
  #not sure what this function is needed for, the following shourld do the same
  #data.table:::print.data.table(.dt_class(x))
  x=.dt_class(x)
  if (!col.names %chin% c("auto", "top", "none"))
    stop("Valid options for col.names are 'auto', 'top', and 'none'")
  if (col.names == "none" && class)
    warning("Column classes will be suppressed when col.names is 'none'")
  if (!shouldPrint(x)) {
    SYS = sys.calls()
    if (length(SYS) <= 2L || (length(SYS) >= 3L && is.symbol(thisSYS <- SYS[[length(SYS) -
                                                                             2L]][[1L]]) && as.character(thisSYS) == "source") ||
        (length(SYS) > 3L && is.symbol(thisSYS <- SYS[[length(SYS) -
                                                       3L]][[1L]]) && as.character(thisSYS) %chin% mimicsAutoPrint)) {
      return(invisible(x))
    }
  }
  if (!is.numeric(nrows))
    nrows = 100L
  if (!is.infinite(nrows))
    nrows = as.integer(nrows)
  if (nrows <= 0L)
    return(invisible(x))
  if (!is.numeric(topn))
    topn = 5L
  topnmiss = missing(topn)
  topn = max(as.integer(topn), 1L)
  if (print.keys) {
    if (!is.null(ky <- key(x)))
      cat("Key: <", paste(ky, collapse = ", "),
          ">\n", sep = "")
    if (!is.null(ixs <- indices(x)))
      cat("Ind", if (length(ixs) > 1L)
        "ices"
        else "ex", ": <", paste(ixs, collapse = ">, <"),
        ">\n", sep = "")
  }
  if (any(dim(x) == 0L)) {
    class = if (is.data.table(x))
      "table"
    else "frame"
    if (all(dim(x) == 0L)) {
      cat("Null data.", class, " (0 rows and 0 cols)\n",
          sep = "")
    }
    else {
      cat("Empty data.", class, " (", dim(x)[1L],
          " rows and ", length(x), " cols)",
          sep = "")
      if (length(x) > 0L)
        cat(": ", paste(head(names(x), 6L), collapse = ","),
            if (length(x) > 6L)
              "...", sep = "")
      cat("\n")
    }
    return(invisible(x))
  }
  if ((topn * 2L + 1L) < nrow(x) && (nrow(x) > nrows || !topnmiss)) {
    toprint = rbindlist(list(head(x, topn), tail(x, topn)),
                        use.names = FALSE)
    rn = c(seq_len(topn), seq.int(to = nrow(x), length.out = topn))
    printdots = TRUE
  }
  else {
    toprint = x
    rn = seq_len(nrow(x))
    printdots = FALSE
  }
  toprint = data.table:::format.data.table(toprint, na.encode = FALSE, timezone = timezone,
                              ...)
  data.table:::require_bit64_if_needed(x)
  if (isTRUE(row.names))
    rownames(toprint) = paste0(format(rn, right = TRUE, scientific = FALSE),
                               ":")
  else rownames(toprint) = rep.int("", nrow(toprint))
  if (is.null(names(x)) || all(names(x) == ""))
    colnames(toprint) = rep("", ncol(toprint))
  if (isTRUE(class) && col.names != "none") {
    class_abb = c(list = "<list>", integer = "<int>",
                  numeric = "<num>", character = "<char>",
                  Date = "<Date>", complex = "<cplx>",
                  factor = "<fctr>", POSIXct = "<POSc>",
                  logical = "<lgcl>", IDate = "<IDat>",
                  integer64 = "<i64>", raw = "<raw>", expression = "<expr>",
                  ordered = "<ord>")
    classes = vapply_1c(x, function(col) class(col)[1L],
                        use.names = FALSE)
    abbs = unname(class_abb[classes])
    if (length(idx <- which(is.na(abbs))))
      abbs[idx] = paste0("<", classes[idx], ">")
    toprint = rbind(abbs, toprint)
    rownames(toprint)[1L] = ""
  }
  if (isFALSE(class) || (isTRUE(class) && col.names == "none"))
    abbs = ""
  if (quote)
    colnames(toprint) <- paste0("\"", old <- colnames(toprint),
                                "\"")
  if (isTRUE(trunc.cols)) {
    widths = dt_width(toprint, class, row.names, col.names)
    cons_width = getOption("width")
    cols_to_print = widths < cons_width
    not_printed = colnames(toprint)[!cols_to_print]
    if (!any(cols_to_print)) {
      trunc_cols_message(not_printed, abbs, class, col.names)
      return(invisible(x))
    }
    toprint = toprint_subset(toprint, cols_to_print)
  }
  if (printdots) {
    toprint = rbind(head(toprint, topn + isTRUE(class)),
                    `---` = "", tail(toprint, topn))
    rownames(toprint) = format(rownames(toprint), justify = "right")
    if (col.names == "none") {
      cut_top(print(toprint, right = TRUE, quote = quote))
    }
    else {
      print(toprint, right = TRUE, quote = quote)
    }
    if (trunc.cols && length(not_printed) > 0L)
      trunc_cols_message(not_printed, abbs, class, col.names)
    return(invisible(x))
  }
  if (nrow(toprint) > 20L && col.names == "auto")
    toprint = rbind(toprint, matrix(if (quote)
      old
      else colnames(toprint), nrow = 1L))
  if (col.names == "none") {
    cut_top(print(toprint, right = TRUE, quote = quote))
  }
  else {
    print(toprint, right = TRUE, quote = quote)
  }
  if (trunc.cols && length(not_printed) > 0L)
    trunc_cols_message(not_printed, abbs, class, col.names)
  invisible(x)
}




.drop = function(x) {
  x=.dt_class(x)
  xdim=.dim(x)
  if (!any(xdim>1)) {
    dims2drop=names(xdim)
    dims2drop=dims2drop[-utils::tail(which(xdim==1L),1)]
  } else {
    dims2drop=names(xdim)[xdim<2]
  }
  y = data.table:::`[.data.table`(x,j=!(colnames(x) %in% dims2drop), with =FALSE)
  attr(y,'dcstruct') = attr(x,'dcstruct')[setdiff(names(xdim),dims2drop)]
  #if (NROW(y)==1) {
    #if (NROW(y)<=prod(sapply(attr(y,'dcstruct'),length))) return(as.numeric(y[[1L]]))
  #}
  .md3_class(y,force=TRUE)
}


#' @export
drop=function(x,...) {
  UseMethod('drop')
}

#' @export
drop.default = base::drop



#' Dropping dimensions
#'
#' Drops any dimensions that contain only a single element
#' @param omd0 an md0 object with n dimensions
#' @return and md0 obnject with n dimensions or less
#' @seealso \code{\link{aperm.md0}}, \code{\link{adddim}}
#' @examples
#' data(euhpq)
#' ww=euhpq[TOTAL.I15_Q..,drop=FALSE]
#' dim(ww)# dimensions
#' w2=drop(ww)
#' dim(w2)
#' @export
drop.md3 = function(x,...) {
  .drop(x)
}





#' @export
dim.md3=function(x) {
  unlist(lapply(attr(x,'dcstruct'),NROW))
}
.dim = function(x) {
  unlist(lapply(attr(x,'dcstruct'),NROW))
}




#' @export
print.md3 = function (x, ..., max = NULL, as=c('array','data.table')) {
  if (is.null(max)) {
    temp = .dim(x)
    if (length(temp) > 1)
      max = prod(c(temp[0:2], 2))
    else if (!length(temp))
      max = NULL
    else max = temp
    max = base:::min(base:::max(max, 100L), 10000L)
  }
  print.default(.md3get(x, as = .asget(as), drop = FALSE),
                ..., max = max)
}




# p0=mdGet('Eurostat/prc_hpi_q/Q...')
# fwrite(as.data.table(p0),'prc_hpi_q.csv')

# setwd('U:/Data/Development/MD3')
# euhpq=as.md3(fread('prc_hpi_q.csv'))

#data(mhp, package = 'MD0');fwrite(data.table(MD0:::as.data.frame.md0(mhp)),'prc_hpi_qa.csv')
#euhpm=as.md3(fread('prc_hpi_qa.csv'))



#.md3getelem(euhpq,28943)



#
# #.md3get(euhpq,1,1,2,10:15,drop=F)
# .md3get(euhpq,1,1,'SE',10:15,as='a')
# .md3get(euhpq,1,1,'SE',)
# .md3get(euhpq,1,1,,'2014q4')
# .md3get(euhpq,'TOTAL.I15_Q.SE+AT.2014q4')
# .md3get(euhpq,TOTAL.I15_Q.SE+AT.2014q4)
# .md3get(euhpq,TOTAL.I15_Q.SE+AT.y2014q4)
# .md3get(euhpq,TOTAL.I15_Q.BE:DK.2014q4)
# .md3get(euhpq,'TOTAL..IE.2015q3:2016q1',as='a')
# .md3get(euhpq,'TOTAL.I15_Q.SE+AT.2012q2:2012q1+2014+2011')
#
#
# .md3get(euhpq,TOTAL..BE:DK.2014q4,as='a')
# .md3get(euhpq,'TOTAL.I15_Q.DK.2014q4:2019q4',as='a')
# .md3get(euhpq,'TOTAL..oasch.2015q3:2016q1',as='a')
#
# #problems
# .md3getelem(euhpq,c(4,50000)) #solved
# euhpq['TOTAL.I10_Q.AT.2023q2'] #ok
# euhpq['TOTAL.I10_Q.AT.2016q2+2023q1'] #ok
# euhpm[Q..INX.AT.y2020:y] #ok
# uu=aperm(euhpm[Q..INX.AT:BG.y2018q1:y],3:1)[,'BE',] #solved



 #  setwd('U:/Data/Development/MD3')
 #  euhpq=as.md3(fread('prc_hpi_q.csv'))
 #
 #data(mhp, package = 'MD0');fwrite(data.table(MD0:::as.data.frame.md0(mhp)),'prc_hpi_qa.csv')
 #euhpm=as.md3(fread('prc_hpi_qa.csv'))


#source("C:/Users/zeugnst/rpackages/MD3/sourceCode/md3object.R");
#debugonce(.md3set);.md3set(euhpq,'TOTAL.I15_Q.BE.2013:2015',value=1)
#p5=.md3set(euhpq,'TOTAL.I10_Q.AT.',value=euhpq['TOTAL.I10_Q.EU28.']); p5[TOTAL.I10_Q.AT+EU28.]
#p5=.md3set(euhpq,'TOTAL.I10_Q.AT.',value=euhpq['TOTAL.I10_Q.EU28.'], onlyna=TRUE); p5[TOTAL.I10_Q.AT+EU28.]
#p5=.md3set(euhpq,'TOTAL.I10_Q.AT.',value=euhpq['TOTAL.I10_Q.EU28.'], justval=TRUE); p5[TOTAL.I10_Q.AT+EU28.]
#  Rprof(tmp <- tempfile())
# # p5=.md3set(euhpq,'TOTAL.I10_Q.AT.',value=euhpq['TOTAL.I10_Q.EU28.']);
# euhpm['TOTAL..AT.']
#  Rprof()
#  summaryRprof(tmp)
#  unlink(tmp)
.md3set = function(x, ..., value, onlyna=FALSE, justval=FALSE, usenames=NULL, .obs = "value") {
  #.obs: valaname, flag or conf
  #usenames: whether to use the dimnames of value to determinate subset of x to update
  #onlyna: only update where x is NA
  #justval: only update where value is not na

  #!!!DO TIME that partially exists and parially doesnt .md3set(euhpq,'TOTAL.I15_Q.BE.2013:2023q3',value=1)
  #do md3setelem
  frqshifter=NULL
  #xdn=.fixhihi(attr(x,"hihi"))
  xdn=.getdimnames(x,TRUE); xdc=attr(x,'dcstruct')
  x=.dt_class(x)
  ix=.dotsaslist(...)
  obs=.md3resnames(.obs[[1L]])

  if (length(ix)==1L) {
    if (missing(..1)) {
      ix = lapply(as.list(1:length(xdn)),function(x) numeric(0))


      #   aout=array(numeric(0),dim=sapply(xdn,length),dimnames=xdn)
      #    aout[as.matrix(x[,1:length(xdn)])] = x[,.md0reserved[["value"]]]
    } else {
      #ix[[1]] = ..1
      #if (is.md0(ix[[1]])) {ix[[1]] = names(which(.subset2(ix[[1]],.md0reserved[["value"]])))}
      if (!is.null(dim(ix[[1]]))) if( length(dim(ix[[1]]))==2 & ncol(ix[[1]])==length(xdn)) {
        if (!(mode(ix[[1]]) %in% c("integer","numeric"))) {
          ix[[1]] = apply(ix[[1]],1,paste,collapse=".")
        }
      }
      if (length(ix[[1]])==1) if (is.character(ix[[1]])) ix[[1]] = .trafoRestQuery(ix[[1]],xdn,alsosingleton=FALSE)
      if (!is.list(ix[[1]])) {
        #if (is.array(ix[[1]])) { ix[[1]]=as.vector(ix[[1]])}
        #if (anyNA(value)) value[is.na(value)]=log("a")
        ugl=.md3setelem(x,ix[[1]],value,.obs=.obs,onlyna=onlyna, justval=justval)
        #if (!anyNA(value)) return(ugl)
        #ugl=.md3_class(data.table:::na.omit.data.table(.dt_class(ugl)))
        return(ugl)

      } else {
        ix=ix[[1]]
      }
    }
  } else if (length(ix) == length(xdn) +1) {
    if (length(ix[[1L]])) {
      frqshifter=toupper(strsplit(trimws(ix[[1L]]),split='\\+')[[1]])
      if (length(setdiff(frqshifter,.cttim$frqcodes$fcode))) { stop('frequency code ', setdiff(frqshifter,.cttim$frqcodes$fcode), 'not permitted')}
      ix=ix[-1]
      attributes(ix)[['frqshifter']]<-frqshifter
    } else {
      ix=ix[-1]
    }

  }

  if (!length(ix)) {
    ix=lapply(xdn,function(x) integer(0))
  }



  #if (all(sapply(ix,class)=="data.frame")) { ix = lapply(ix,"[[","code") } #if dimcodes instead of hihi has been supplied



  # if (usenames) {
  #   sublix=.dimnames(value)
  #   if (is.null(names(sublix))) {
  #     names(sublix) = names(xdn)[!sapply(ix,length)]
  #   }
  #   if (!all(names(sublix) %in% names(xdn))) {
  #     dimnameinquestion = setdiff(names(sublix),names(xdn))
  #     pctinnames =0
  #     if (length(dimnameinquestion)==1L) {
  #       pctinnames = unlist(lapply(xdn[setdiff(names(xdn),names(sublix))],
  #                                  function(x) sum(sublix[[dimnameinquestion]] %in% x)/length(sublix[[dimnameinquestion]])))
  #       #TBD
  #     }
  #     if (any(pctinnames> .49)) {
  #       mynewname=names(pctinnames)[(pctinnames>.49)][1]
  #       warning('Dimension names were somewhat confusing: I assume that ', dimnameinquestion, ' matches dimension name ', mynewname)
  #       names(sublix)[names(sublix)==dimnameinquestion] = mynewname
  #
  #     } else {
  #       stop("dimension named ",paste(names(sublix)[!(names(sublix) %in% names(xdn))],collapse=", "),"cannot be found")
  #
  #     }
  #   }
  #   #if (length(sublix)!=length(ix)) stop("wrongnumber of dimensions assigned when usenames==TRUE")
  #   lix = .matchgen2dim(sublix,ix,xdn)
  #   if (length(value) > 1L) if (prod(unlist(lapply(lix,length)))> length(value)) { warning('number of items to replace is longer than length of update. Values will be recycled.')}
  #   .dimnames(value) = lix[names(sublix)]
  #
  #
  # } else {
    lix=.match2dim(ix,xdn,TRUE)
  # }





  # if (!identical(lix,.match2dim(ix,xdn,FALSE))) {
  #   for (i in seq_along(xdn)) {
  #     missstuff=lix[[i]][!(lix[[i]] %in% xdn[[i]])]
  #     if (length(missstuff)) {
  #       xdn[[i]]=c(xdn[[i]],missstuff)
  #     }
  #   }
  #   #attr(x,"hihi") = .fixhihi(xdn)
  # }

  #xdn = .mdfixindexfreq(xdn, stopifwrong =  TRUE)
  #attr(x,"hihi") = .fixhihi(xdn, ignore.time=FALSE) #this is in case a new time period got added. things need to be sorted right
  #ixf =  .mdfindfreqdim(xdn)
  ixt =  .dn_findtime(xdn)



  expanding=FALSE
  codesinxdn=lapply(as.list(names(lix)),function(dname) lix[[dname]] %in% xdn[[dname]]);
  if (all(!unlist(lapply(codesinxdn,length)))) { codesinxdn= lapply(as.list(seq_along(lix)),function(dix) ix[[dix]] %in% xdn[[dix]]);  }
  if (any(!unlist(codesinxdn)))  {
    names(codesinxdn) = names(lix)
    lix=lix[names(xdn)]; ixd=
      for (ixd in which(unlist(lapply(codesinxdn,function(d) any(!d))))) {
        xdn[[ixd]] = c(xdn[[ixd]], lix[[ixd]][!codesinxdn[[ixd]]])
      }

    attr(x,'dcstruct') <- .dimcodesrescue(xdn,xdc)
    expanding=TRUE
  }


  if (!length(usenames)) {
    if (inherits(value,c('md3','array'))) usenames=TRUE else usenames=FALSE
  }

  if (!match(obs,colnames(x),nomatch = 0)) {
    x[[obs]] = NA_character_
  }


  tempan=intersect(c('dcstruct','dcsimp','dimcodes'),names(attributes(x))) #!!!
  #now value can be an md3, a basic atomic, a named vector or array, a data.frame or data.table
  if (usenames)  {if (.md3_is(value)|is.array(value)) {dimval=.getdimnames(value,.md3_is(value))} else {dimval=as.list(value)}}
  if (!.md3_is(value)) {
    if (!is.data.frame(value) & !is.atomic(value) & !is.array(value)) {
      #try to convert to something we can deal with
      value = as.data.frame(value,stringsAsFactors=FALSE)
    }

    if (is.array(value)) {
      value=.arraymelt(value)
    }
  }

  changeddimnames=FALSE
  if (.md3_is(value) | is.data.frame(value) | is.array(value)) {
    tempdno=names(lix[unlist(lapply(lix,length))>1])
    tempdnn=colnames(value); tempdnn=tempdnn[substr(tempdnn,0,2)!='_.']
    if (length(tempdnn)==length(tempdno) & !all(tempdno %in% tempdnn)) {
      warning('Dimension names differ: \n', paste(tempdno, collapse = ', '),' ...vs... ', paste(tempdnn, collapse = ', '),'.',
              '\n This operation therefore ignores the latter dimnames and indeed assumes the order of dimensions to match in both objects.')
      colnames(value)[colnames(value) %in% tempdnn] <- tempdno
      changeddimnames=TRUE
    }

  }


  if (.md3_is(value) | is.data.frame(value)) {
    if (!.md3_is(value)) {
      tempn=setdiff(names(value),names(xdn))
      if (length(tempn) == 1) {
        names(value)[names(value)==tempn] <- obs
      } else {
        if (any(!(tempn %in% names(x)))) stop('Could not identify what of these columns in X refers to values: ', paste(tempn, collapse=', '))
      }


    }
    if (usenames) {

      if (length(lix)<length(dimval)) { stop('You seem to have overidentified what you want to change. Try usenames=FALSE.')}
      lixsub=lix[setdiff(names(lix),names(dimval))]
#message('???'); browser()
      if (length(lixsub)==1L) {
        dtlixsub=data.table(unlist(lapply(as.list(lixsub[[1L]]),rep,data.table:::dim.data.table(value)[[1]])))
        names(dtlixsub) =names(lixsub)
        dtval=cbind(dtlixsub,.dt_class(value))
      } else if (!length(lixsub)) {
        #this means all ahve been selected
        dtval=.dt_class(value)
      } else {
       dtval=cbind(.mdsel2codes(lixsub,aschar=FALSE),.dt_class(value))
      }
    } else {
      dtval=.mdsel2codes(lix,aschar = FALSE,bylast=TRUE)
      if (.md3_is(value)) {
        dtval[[obs]]=as.data.table.md3(value,na.rm = FALSE)[[gsub('^_\\.','',obs)]]
      } else {
        dtval[[obs]]=value[[obs]]
      }
      if (changeddimnames ) {
        if (NROW(value)==NROW(dtval)) value[,tempdno] = dtval[,tempdno, with=FALSE]
      }
    }

    vtix2=.dn_findtime(value); if (vtix2) value[[vtix2]]=as.timo(value[[vtix2]])
    vtix1=.dn_findtime(dtval); if (vtix1) dtval[[vtix1]]=as.timo(dtval[[vtix1]])


    tempselix=x[dtval[,names(xdn),with=FALSE],,on=.NATURAL]
    tempselnew=tempselix[is.na(tempselix[[obs]]) & !is.na(dtval[[obs]])]
    tempselremove=tempselix[!is.na(tempselix[[obs]]) & is.na(dtval[[obs]])]
    if (onlyna | justval) { tempselremove=tempselremove[0,]}
    if (usenames) {
      #warning('have to deal with setting NA under usenames')

      uebrigbleiber=lapply(names(dimval),function(x) {dimval[[x]][!(dimval[[x]] %in% dtval[[x]])]}); names(uebrigbleiber)=names(dimval)
      if (any(as.logical(unlist(lapply(uebrigbleiber,length))))) {

        #tempselremove=data.table::as.data.table(lix)[data.table::as.data.table(uebrigbleiber),,on=.NATURAL]
        tempselremove= .mdsel2codes(lix)[ .mdsel2codes(uebrigbleiber),,on=.NATURAL]
      }
    }
    if (NROW(tempselremove)) {
      #browser()
      x<-x[!tempselremove,,on=.NATURAL]
    }
    if (NROW(tempselnew)) {

      tempattr=attributes(x)[tempan]
      x=merge(x,tempselnew[,names(xdn),with=FALSE],all=TRUE)

      attributes(x)[tempan] <- tempattr[tempan]
    }

    if (onlyna) {
      dtval=merge(tempselnew[,names(xdn),with=FALSE],dtval,all.x=TRUE,by=names(xdn))
    }

    if (justval) {
      dtval=dtval[!is.na(dtval[[obs]])]
    }
    x[dtval[,names(xdn),with=FALSE],unlist(list(obs)):=dtval[[obs]],on=.NATURAL]
    if (anyNA(dtval[[obs]])) {
      x=x[!is.na(`_.obs_value`)]
    }
    return(.md3_class(x))
    #attributes(x)[tempan] <- tempattr[tempan]

  } else if ( is.atomic(value)) {
    if (usenames) {
      if (!is.null(names(value)) & length(xdn)==1) {
        if (all(names(value) %in% xdn[[1L]])) {
          stop('fix this')
        }
      }
    }
    tempsel=.mdsel2codes(lix,aschar=FALSE)
    value=.recycle(value,rep(NA,NROW(tempsel)))[[1L]]
    tempsel[,`_._NEW`:=value]

    tempselix=x[tempsel,,on=.NATURAL];

    tempselnew=tempselix[is.na(tempselix[[obs]]) & !is.na(value)]
    tempselremove=tempselix[!is.na(tempselix[[obs]]) & is.na(value)]
    if (NROW(tempselnew)) {
       if (obs=='_.obs_value')  {
          tempattr=attributes(x)[tempan]
          x=rbind(x,tempselnew[,names(xdn),with=FALSE],fill=TRUE)
          attributes(x)[tempan] <- tempattr[tempan]
       } else {
         warning('Not possible to set flags or other attributes on observations with missing values.\n If your really want to do that, then set those obs to Inf first.')
       }
    }


    if (onlyna) {
      tempsel=tempselnew[,names(tempsel),with=FALSE]
    }
    if (justval) {
      tempselremove=NULL
      tempsel=tempsel[!is.na(`_._NEW`)]
    }

    x[tempsel[,names(xdn),with=FALSE],unlist(list(obs)):=tempsel[['_._NEW']],on=.NATURAL]



    if (obs=='_.obs_value' & NROW(tempselremove)) {
      x=x[!is.na(`_.obs_value`)]
    }

    return(.md3_class(x))

  } else {
    stop('cannot deal with this')

  }


}

.md3setelem = function(x,vix,value,.obs="_.obs_value",onlyna=FALSE, justval=FALSE) {
  #setting elements of x equal to value when vix is a vector of integers logical, or IDs

  mydn = .getdimnames(x,TRUE)
  mydc = attr(x,"dcstruct")
  class(x)="list"



  tempix = .mdindexelem(vix,mydn)
  ixt=.dn_findtime(mydn)

  vixhead=head(tempix,5L)
  if (any(grepl("[\\+:]",vixhead))) {
    stop("indexing error:  + and : are not permitted when retrieving single elements")
    #tempix = .mdsel2rest(.trafoRestQuery(tempix))
  }



  matix=.mdrest2codes(tempix)
  #this is to adjust for the y prefix when time periods are referred to in name
  if (any(grepl("^y[0-9]|[\\.]y[0-9]",vixhead))) {
    matix[,ixt]=gsub("y","",matix[,ixt,drop=TRUE])
    tempix=apply(matix,1,paste,collapse=".")
  }


  for (i in seq_along(mydn)) {
    missstuff=matix[,i,drop=TRUE][!(matix[,i,drop=TRUE] %in% mydn[[i]])]
    if (length(missstuff)) {

      mydn[[i]]=c(mydn[[i]],missstuff)
    }
  }





  colnames(matix) = names(mydn); # temp=.mdfixindexfreq(as.list(as.data.frame(matix,stringsAsFactors = FALSE)),TRUE); rm(temp)

  datix=data.table(matix);
  ixt=.dn_findtime(datix)
  datix[[ixt]] = as.timo(datix[[ixt]])
  value=.recycle(value,rep(NA,NROW(datix)))[[1L]]
  dx=.dt_class(x)


  #browser()


  obs = .md3resnames(.obs[[1L]])
  if (!(obs %in% names(x))) {
    x[[obs]] = logical(0)
  }

  tempsel=dx[datix,,on=.NATURAL]



  tempselnew=   tempsel[is.na(`_.obs_value`) & !is.na(value),-NCOL(tempsel),with=FALSE]
  tempselremove=tempsel[!is.na(`_.obs_value`) & is.na(value),-NCOL(tempsel),with=FALSE]
  datix=datix[!is.na(value)]; value=value[!is.na(value)]

  if (NROW(tempselnew)) {
    if (obs!='_.obs_value') {
      warning('Not possible to set flags or other attributes on observations with missing values.\n If your really want to do that, then set those obs to Inf first.')
    } else {


    #dx=rbind(dx,tempselnew,fill=TRUE)
    dx=data.table::merge.data.table(dx,tempselnew,all=TRUE)
    #attr(dx,"dcsimp") = mydn
    attr(dx,"dcstruct")=mydc
    }

  }


  if (obs=='_.obs_value' & NROW(tempselremove)) {
    dx=dx[-.naomit_atomic(dx[tempselremove,on=.NATURAL,which=TRUE])]

  }



  dx[datix[,names(mydn),with=FALSE],unlist(list(obs)):=value,on=.NATURAL]

  return(.md3_class(dx))


  ####NOT TO BE USED:

  if (length(value)) if (length(tempix)%%length(value)!=0) stop("Indexing error: number of items to replace is not a multiple of replacement length")
  #dval=numeric(0)
  #dval[tempix] = value
  dval=rep(unname(value),length(tempix)/length(value)); names(dval)=tempix



  if (onlyna) {
    dval=dval[!(names(dval) %in% names(x[[obs]]))]
  }




  if(anyNA(dval)) {
    if (!justval) {
      tempNA=names(dval)[is.na(dval)]
      x[[obs]] = x[[obs]][-match(tempNA,names(x[[obs]]),nomatch=0)]
    }
    dval=.naomitvec(dval)
  }


  x[[obs]][names(dval)] = dval






  return(.md3_class(x))


}







#' Extract or replace parts of an md3 object
#'
#'
#' @name indexMD3
#' @param x an md3 object
#' @param ... indexes, see 'Details' below
#' @param drop whether to drop singleton dimensions (see \code{\link{drop.md3}})
#' @param as how to return the result, see also \code{\link{as.data.table.md3}}
#' @param onlyna only update those elements that are NA
#' @param justval only update those elements that are not NA
#' @param usenames when replacing parts of x, update x by using the dimension codes, rather than the order, of elements in value
#' @param .obs used when extracting or replacing not the observation values, but observation attributes such as flags (see below)
#' @return an md3, array, numeric, zoo, or data.table, data.frame depending on parameter \code{as}
#' @section Internal structure:
#' an md3 actually is a data.table that contains one column for each dimension,
#' and at least one column for observations (there can be additional columns for flags and confidentiality labels).
#' You can see that  by running e.g. \code{as.data.table(euhpq)}
#'
#' or in an SDMX API-like notation (\code{euhpq["TOTAL.I15_Q.BE+FR.2021q1:"]} resp \code{euhpq["TOTAL..SI.2021q1:"]}),
#' or a mix thereof (\code{euhpq[3,2,"BE+FR","2021q1:"]})
#' @seealso \code{\link{dimcodes}}
#' @examples
#' data(euhpq) #house prices for EU countries
#'
#'#Retrieving values
#' #Austrian & Slovak HP growth year-on-year
#' #these five commands are equivalent:
#' euhpq["TOTAL","RCH_A",c("AT","SK"),]
#' euhpq["TOTAL.AT+SK."]
#' euhpq[3,3,c(1, 35),]
#' euhpq[3,3,c("AT", "SK"),]
#' euhpq[3,3,"AT+SK",]
#'
#'#Using + and :
#'
#' euhpq[TOTAL..FR+AT.y2011q4] #time periods should be prefixed by 'y'
#' euhpq[TOTAL..FR.y2011q1:y2017q4]
#' euhpq[TOTAL..FR.2011q2:2017] #but dropping the prefix 'y'is mostly possible
#' #note the way  2017 is used to encompass all periods of that year.
#' euhpq[TOTAL.RCH_A.AT+SK.y2020q1:y]
#' euhpq[TOTAL.RCH_A.FR:PL.y2022q1+y2023q1]
#'
#' #Time periods: the following are equivalent:
#' euhpq['TOTAL.I15_Q.FR+NL.:2007q4']
#' euhpq[TOTAL.I15_Q.FR+NL.y:y2007q4]
#' euhpq["TOTAL.I15_Q.FR+NL.y:y2007"]
#' euhpq[3,2,c("FR","NL"),1:12]
#'
#'
#'#Setting values:
#' # Slovak and Austrian house price growth for 2005-2006
#' #these four commands are equivalent:
#' euhpq[.RCH_A.AT+SK.:2006]=0
#' euhpq[.RCH_A.AT+SK.y:y2006]=0
#' euhpq[".RCH_A.AT+SK.:2006"]=0
#' euhpq[,"RCH_A",c(1,33),1:8]=0
#'
#'
#'#Adding elements:
#' euhpq[,,"Dummy",]=0 #add a dummy country
#' euhpq[2,'I15_Q',,"2011"] #see effect
#'
#'#Argument usenames, onlyna and justval still to be described
# @describeIn indexMD3 Get subelements
#' @rdname indexMD3
#' @export
`[.md3` = .md3get

# @describeIn indexMD3 Set subelements
#' @export
`[<-.md3` = .md3set



#' Extract the labels of the time dimensions
#'
#' @param x an md3 object
#' @param ... not used, only there for compatitibilty reasons
#' @return a timo object
#' @seealso \code{\link[stats]{time}}
#' @examples
#'
#' time(euhpq)
#' time(euhpq)+10
#' frequency(time(euhpq),'A') #convert frequency
#' euhpq[3,3,'AT',time(euhpq)-3] #3 year lag
#'
#' @export
time.md3 = function(x,...) {
  if (is(x,'md3')) x = .getdimnames(x,TRUE)
  if (is.null(x)) return(NULL)
  if (!is.list(x)) stop('x needs to be md3 or list')
  ixt=.dn_findtime(x)
  if (ixt<1) return(NULL)
  return(x[[ixt]])
}



#' transpose a twodimensional md3 object
#'
#' @param x an md3 object
#' @return an md3
#' @seealso \code{aperm}
#' @examples
#' data(euhpq) #house prices for EU countries
#' euhpq[3,3,1:2,'2020:']
#' t(euhpq[3,3,1:2,'2020:'])
#'
#' @export
t.md3 = function(x) {
  if (length(.getdimnames(x,TRUE)) !=2) { stop('transpose only works with 2 dims')}
  attr(x,'dcstruct')= attr(x,'dcstruct')[2:1]
  x
}


#' @export
as.array.md3 = function(x, ...) {
  .md3get(x, as = "array", drop = FALSE)
}

#' @export
as.matrix.md3 = function(x, ...) {
  if (length(attr(x,'dcstruct')) > 2) { stop('transpose only works with 2 dims or less')}
  as.matrix(.md3get(x, as = "array", drop = FALSE) )
}

#' @export
aperm.md3 = function(a, perm = NULL, resize = TRUE, ...) {
  if (!resize) stop('resize=FALSE makes no sense with md3!')
  if (is.null(perm)) return(a)
  if (length(attr(a,'dcstruct')) !=length(perm)) { stop('perm needs to be an integer or character vector of length ',length(attr(a,'dcstruct')))}
  if (is.character(perm)) perm=match(perm,names(attr(a,'dcstruct')),nomatch = NA)
  if (anyNA(perm)) stop('perm cannot contain NA')
  a=.dt_class(a)
  attr(a,'dcstruct')=attr(a,'dcstruct')[perm]
  a[,seq_along(perm)]<- a[,perm,with=FALSE]
  names(a)[seq_along(perm)]<- names(a)[perm]
  .md3_class(a)
}

#' @export
as.data.table.md3 = function(x, ..., na.rm=FALSE, .simple=FALSE) {
  if (na.rm) { return(.dt_class(x))}
  dcstruct =attr(x,'dcstruct')
  y=.dt_class(x)
  if (!.simple) {  y=y[.mdsel2codes(.getdimnames(x,TRUE),bylast = TRUE),,on=.NATURAL] }

  if (length(dcstruct)) attr(y,'dcstruct') =dcstruct
  colnames(y)=gsub('^_\\.','',colnames(y))
  if (missing(...)) return(y)
  data.table::dcast(y,...)
}

#' @export
as.data.frame.md3 = function(x, ..., na.rm=FALSE) {
  data.table:::as.data.frame.data.table(as.data.table.md3(x,...,na.rm=na.rm))
}



#### DIMCODES ########
#' @export
dimnames.md3=function(x) {  .getdimnames(x) }

#' @export
"dimnames<-.md3" <- function(x,value) {
  .setdimcodes(x,value)
}

.setdimcodes =function(x,value,ignore.old=FALSE) {
  #mydn=attr(x,"dcsimp")
  olddc=.getdimcodes(x)

  mydim=sapply(olddc,length)
  #if (ignore.old) olddc=NULL else olddc = .getdimcodes(x)
  #olddc = attr(x,"dcstruct")
  #class(x)="list"
  #user checks
  if (!is.list(value)) stop("invalid 'dimcodes' given for array")
  #if (!is.md0(x)) x=as.md0(x)
  newdc=.dimcodesrescue(value,olddc = olddc)
  if (identical(.dc2dn(newdc),.dc2dn(olddc))) {
    attr(x,'dcstruct') = newdc
    return(x)
  }


  #so we ned to change something
  ndn=.getdimnames(newdc); odn=.getdimnames(olddc)
  if (!identical(names(ndn),names(odn))) {
    if (all(match(names(ndn) ,names(odn), nomatch = 0))) {
      ndnsorted=sort(names(ndn))
      if (identical(ndn[ndnsorted],odn[ndnsorted])) {
        #just a reordering
        dx=.dt_class(x)
        neworder=c(names(ndn),setdiff(colnames(dx),names(ndn)))
        dx=dx[,neworder,with=FALSE]
        return(.md3_class(dx,dn=newdc))
      }
    } else {
      #likley renaming a dimnesion
      if (identical(unname(ndn),unname(odn))) {
        #simply renaming a dimension
        dx=.dt_class(x)
        vdict=union(names(ndn),setdiff(colnames(dx),names(odn))); names(vdict)=vdict; names(vdict)[seq_along(odn)]=names(odn)
        colnames(dx)= vdict[colnames(dx)]

        attr(dx,'dcstruct') = newdc
        x=.md3_class(dx)
        return(x)
      }
    }
    stop('You can change or reorder dimension names, but not both.')
  }
  if (identical(unlist(lapply(ndn,length)),unlist(lapply(odn,length)))) { #we are dealing with a change of order or a code chance}
    ixchgcode=lapply(as.list(names(newdc)),function(i) setdiff(ndn[[i]],odn[[i]]))
    if (!any(unlist(lapply(ixchgcode,length)))) {
      #so we just changed the order of codes, not a problem
      attr(x,'dcstruct') = newdc
      return(x)
    }
    #we must have changed a code
    dx=.dt_class(x)
    for (i in names(ndn)[as.logical(unlist(lapply(ixchgcode,length)))]) {
      dictvec=ndn[[i]]; names(dictvec)=odn[[i]]
      dx[[i]]=dictvec[dx[[i]]]
    }
    return(.md3_class(dx,dn = newdc))
  }

  #'subsetting or supersetting:

  return(.md3get(x,ndn,drop = FALSE))


}


.getdimcodes =function(x) {

  ldc = attr(x,"dcstruct")
  if (is.null(ldc)) { ldc=.dimcodesrescue(attr(x,"dcstruct")) }
  ldc

}
#
# .getdimcodesold =function(x) {
#   #mydn = .fixhihi(attr(x,"dcsimp"))
#   mydn = attr(x,"dcsimp")
#   ldc = attr(x,"dcstruct")
#   if (is.null(ldc)) { x=.dimcodesrescue(x,list()); ldc=attr(x,"dcstruct") }
#
#
#   for (l in names(mydn)) {
#     if (is.null(ldc[[l]])) {
#       ldc[[l]] = data.frame("code"=mydn[[l]],"label:en"=mydn[[l]],row.names=mydn[[l]],check.names = FALSE, fix.empty.names = FALSE, stringsAsFactors=FALSE)
#
#     }
#     temp=ldc[[l]][as.character(mydn[[l]]),,drop=FALSE]
#     rownames(temp) = mydn[[l]]
#     temp[,"code"] = mydn[[l]]
#     ixlabel = grep("^label:",names(temp))
#     if (!length(ixlabel)) {
#       temp[,paste0("label:",ifelse(Sys.getenv("LANGUAGE")!="",tolower(Sys.getenv("LANGUAGE")),"en"))]=NA_character_
#       ixlabel = grep("^label:",names(temp))
#     }
#     for (j in ixlabel) {
#       if (any(is.na(temp[,j]))) {
#         temp[is.na(temp[,j]),j] = temp[is.na(temp[,j]),"code"]
#       }
#     }
#     ldc[[l]]=temp
#   }
#   return(ldc[names(mydn)])
#
# }
.dc2dn = function(indimcodes) {

  if (!is.list(indimcodes)) stop('indimcodes needs to be a list')
  dcclasses=unlist(lapply(indimcodes,function(x) class(x)[1]))
  outdc=list()
  if (any(dcclasses=='data.frame')) {
    outdc=c(outdc,lapply(indimcodes[dcclasses=='data.frame'],'[[','code'))
    outdc[!unlist(lapply(outdc,length))] = lapply(outdc[!unlist(lapply(outdc,length))],rownames)
  }

  outdc=c(outdc,indimcodes[dcclasses!='data.frame'])
  outdc=outdc[names(indimcodes)]
  return(outdc)

}

.getdimnames = function(x,simplecase=FALSE) {
  if (simplecase) {
    if (is(x,'data.table')) {x=attr(x,'dcstruct')}
    return(lapply(x,function(x) if(is.data.frame(x)) return(x[[1]]) else return(x)))
  }
  if(is.array(x)) {adn=attr(x,'dimnames'); if(is.null(adn)) {stop('x has no dimanems set')}; tix=.dn_findtime(adn)[1]; if(tix>0) {adn[[tix]]=as.timo(adn[[tix]])}; return(adn) }
  if (!is.null(attr(x,'dcstruct'))) return(.dc2dn(.dimcodesrescue(x)))
  if (!is.list(x)) {stop('object does not have dimcodes set') }
  xclas=unlist(lapply(x,function(i) head(class(i),1)))
  if (all(xclas=='character'|xclas=='timo')) { return(x)}
  return(.dc2dn(.dimcodesrescue(x)))


}

.guessdimnames = function(x) {
  if (!is.list(x)) { x=as.data.frame(x,stringsAsFactors=FALSE)}
  ixnn=which(!unlist(lapply(x,is.numeric)))
  dixnn=diff(ixnn); if (any(dixnn>1)) { ixnn=head(ixnn,which(dixnn>1)[1L])}
  lout=lapply(as.list(x)[ixnn],unique)
  tix=.dn_findtime(lout)[1]
  if (tix>0) { lout[[tix]]=as.timo(lout[[tix]])}
  return(lout)
}

.dimcodesrescue = function(ohihi,olddc=list()) {
  #if ('md3' %in%  class(ohihi)) { if (missing(olddc)) olddc=attr(ohihi,'dcstruct'); ohihi=attr(ohihi,'dcsimp'); }
  if (.md3_is(ohihi) | is(ohihi,'data.table')) {  ohihi=attr(ohihi,'dcstruct')}
  if (.md3_is(olddc) | is(olddc,'data.table')) {  olddc=attr(olddc,'dcstruct')}

  hihiclasses=unlist(lapply(ohihi,function(x) class(x)[[1L]]))
  .mdgetlang = function() {ifelse(Sys.getenv("LANGUAGE")!="",tolower(Sys.getenv("LANGUAGE")),"en")}
  rescuevector=function(ohihi,olddc) {
    outhihi=ohihi
    for (i in names(ohihi)) {
      if (i %in% names(olddc)) { temp=olddc[[i]] } else { temp = ohihi[[i]]}
      if (.timo_is(ohihi[[i]])) { outhihi[[i]]=ohihi[[i]]; next; }
      if (!is.data.frame(temp)) {
        dim(temp)=c(length(temp),1); temp=as.data.frame(temp,stringsAsFactors=FALSE); colnames(temp)="code"; rownames(temp)=temp[,1]
      }
      if (ncol(temp)==1L) {temp=data.frame(temp,temp,stringsAsFactors=FALSE); colnames(temp)[[2]]=paste0("label:",.mdgetlang())}
      things2add=setdiff(ohihi[[i]],temp[,1])
      if (length(things2add)) {temp[things2add,]=matrix(things2add,nrow=length(things2add),ncol = NCOL(temp))}
      temp=temp[ohihi[[i]],]
      #if(length(ohihi[[i]])==11) browser()
      temp[is.na(temp[,1]),] = ohihi[[i]]
      rownames(temp) = temp[,1]
      outhihi[[i]]  =temp
    }
    return(outhihi)
  }


  if (!length(names(ohihi))) {names(ohihi)=rep(NA_character_,length(ohihi))}
  if (anyNA(names(ohihi))) {
    names(ohihi)[is.na(names(ohihi))]=make.names(seq_len(length(ohihi)))[is.na(names(ohihi))]
    if (!any(names(ohihi)=='TIME') && any(hihiclasses=='timo')) {
      names(ohihi)[head(which(hihiclasses=='timo'),1)]='TIME'
    }
  }
  if (anyDuplicated(toupper(names(ohihi)))) {
    warning('duplicate names not allowed for dimension names.names were adjsuted to make them unique')
    names(ohihi) = gsub('\\.','_',make.names(names(ohihi),unique = TRUE))
  }


  outdc=list()
  ix2fix=names(ohihi)[!(hihiclasses %in% c('data.frame','timo'))]
  if (length(ix2fix)) outdc[ix2fix]=rescuevector(ohihi[ix2fix],olddc[ix2fix])
  # if (length(ix2fix)==sum(hihiclasses!='timo')) {
  #   outdc=c(outdc,ohihi[hihiclasses=='timo'])
  #   outdc=outdc[names(ohihi)]
  #   return(outdc)
  # }
  outdc=c(outdc,ohihi[setdiff(names(ohihi),ix2fix)])
  outdc=outdc[names(ohihi)]

  mydim=unlist(lapply(ohihi,NROW))
  mydn=.dc2dn(outdc)
  ###this is for dimcodesrescue
  for (l in seq_along(outdc)) {
    if (.timo_is(outdc[[l]])) {
      if (!any(names(outdc)=='TIME')) stop('when using timo to label dmimesnsions, at least one dimension must be called TIME')
      next
    }
    if (is.null(dim(outdc[[l]]))) {
      dim(outdc[[l]]) = c(length(outdc[[l]]),1)
      rownames(outdc[[l]])=outdc[[l]][,1]; colnames(outdc[[l]])="code"
    }
    if (length(dim(outdc[[l]]))==2L) {
      idc=as.data.frame(outdc[[l]],stringsAsFactors=FALSE)
      if ((nrow(idc)!=mydim[l]) & (ncol(idc)==mydim[l])) { idc = t(idc) }
      if (nrow(idc)!=mydim[l]) { stop(paste0("dimension codes for '",names(mydim)[l], " should have length ",mydim[l]))}
      #if (!is.null(rownames(outdc))) {
      #  if ((ncol(outdc)==1) & (any(rownames(outdc)!=outdc[,1]))) outdc=cbind(rownames(outdc),outdc)
      #}
      rownames(idc)=as.character(idc[,1]); colnames(idc)[[1L]]="code"

      if (as.logical(length(grep("^V[0-9]$",colnames(idc))))) colnames(idc)=NULL
      if (is.null(colnames(idc))) {
        if (ncol(idc)>2) { warning("language of dimension descriptions has not been defined")}
        colnames(idc) = c("code",ifelse(Sys.getenv("LANGUAGE")!="",tolower(Sys.getenv("LANGUAGE")),"en"), rep(NA, ncol(idc)-2))
      }

    }

    tempexists = idc[,"code"][as.logical(idc[,"code"]!=mydn[[l]])]; tempexists=tempexists[tempexists %in% mydn[[l]]]
    if (length(tempexists)) stop("code to be renamed already exists. USe [ or merge( to permute elements.")

    outdc[[l]] = idc
  }


  outdc
}



#' Set/get dimension and element names, labels and attributes
#'
#'
#' @param x an md0 object or any base object
#' @param value sets dimcodes equal to value
#' @param ... no effect
#' @return list of dimcodes (for getting) resp. md0/generic object (for setting)
#' @details \code{dimcodes} is an attribute of an md0 that is a list with one data.frame per dimnesion name.
#'   Each data.frame contains at least one column that holds the "code"/element name for that identifier (e.g. country codes).
#'   The other columns hold attributes for those codes, most notable labels/descriptions.
#'
#'   Note that R will automatically generate the column label:en, respectively label:YOURLANGUAGE to hold such labels
#'   Note that all such labels need to follow the structure label:LANG
#'
#'   You can add arbitrary columns and with attributes for each code.
#'   The following column names, however, have special meaning: 1) \code{label:LANG}, 2) \code{parent}
#' @seealso \code{\link{dimnames}}
#' @examples
#' data(euca)
#' names(dimcodes(euca)) # dimension names
#' dimcodes(euca) # element names
#' dimcodes(euca)[[1]][,"label:fr"] = c("Millions euro","% PIB") #add labels in French language
#' dimcodes(euca)[[2]][1:3,2] = paste(dimcodes(euca)[[2]][1:3,2], "cool") #adjust labels
#' dimcodes(euca)[["unit"]]["PC_GDP",1] = "PC" #rename element
#' names(dimcodes(euca))[[2]] = "country" #rename dimension
#' @export
dimcodes <- function(x,...) {
  UseMethod("dimcodes")
}


#' @rdname dimcodes
#' @export
"dimcodes<-" <- function(x,value,...) {
  UseMethod("dimcodes<-")
}

#' @describeIn dimcodes Returns \code{dimnames} for generic objects
#' @export
dimcodes.default <- function(x,...) {
  mydn=  .dc2dn(attr(x,"dcstruct"))
  if (!is.null(mydn)) return(mydn)
  mydn=  attr(x,"hihi") #legacy from MD0
  if (!is.null(mydn)) return(attr(x,"hihi"))
  .Primitive("dimnames")(x)

}

#' @describeIn dimcodes Sets \code{dimnames} for generic objects
#' @export
"dimcodes<-.default" <- function(x,value) {
  get("dimnames<-")(x, value)
}

#' @describeIn dimcodes Returns dimcodes for md0 objects
#' @export
dimcodes.md3 <- function(x,...) {
  .getdimcodes(x)
}

#' @describeIn dimcodes Sets dimcodes for md0 objects
#' @export
"dimcodes<-.md3" = function(x, value) {
  .setdimcodes(x,value)
}



#PROBLEM: time(euhpq)+'1q'

#.trafoRestQuery('2019q1:2023q1',attr(euhpq,'dcsimp')[4])
#.timo_subset(time(euhpq),'2019q1:2023q1',addifmiss=TRUE)

# Rprof(tmp <- tempfile())
# #.timo_subset(time(euhpq),'2005q1:',addifmiss=TRUE)
# #p5=.md3set(euhpq,'TOTAL.I10_Q.AT.',value=ii);
# ii=euhpq['TOTAL.I10_Q.EU28.']
# Rprof()
# summaryRprof(tmp)
# unlink(tmp)
#
#

#p6=.md3set(euhpq,c(T,rep(F,28942),T),value=0)
#p6=.md3set(euhpq,c(T,rep(F,28942),T),value=NA)
#p6=.md3set(euhpq,10000:20000,value=0)

#' @export
length.md3 = function(x) {prod(.dim(x))}

#' @export
Ops.md3=function(e1,e2) {
  obs='_.obs_value'
  #getdfromdt=function(x) {if (is.data.table(x)) return(x[[obs]]) else return(x) }
  #if (.md3_is(e1)) {e1=.dt_class(e1)}
  #if (.md3_is(e2)) {e2=.dt_class(e2)}
  #if (NROW(e1) ==NROW(e2) | NROW(e1)==1 | NROW(e2)==1) {
  #  return(get(.Generic)(getdfromdt(e1),getdfromdt(e2)))
  #}
  #if (is.data.table(e1) & is.data.table(e2)) {
  #  return(get(.Generic)(as.vector(as.array.md3(e1)),as.vector(as.array.md3(e2))))
  #}
  if (missing(e2)) { e2=e1; e1=0 }
  if (.md3_is(e1)) o1=as.array.md3(e1) else o1=e1
  if (.md3_is(e2)) o2=as.array.md3(e2) else o2=e2
  y=try(get(.Generic)(o1,o2),silent=TRUE)
  if (any(grepl('error',class(y)))) {
    if(.md3_is(e1) & !is.null(names(dimnames(o2)))) {
      surplusdim=setdiff(names(dimnames(o1)),names(dimnames(o2)))
      if  (length(surplusdim)==1L) {

        e2a=.adddim_andfill(e2,.dimname = surplusdim,.dimcodes = .getdimnames(e1,TRUE)[[surplusdim]])
        e2a=aperm(e2a,perm =names(.getdimnames(e1,TRUE)))
        y=get(.Generic)(o1,as.array.md3(e2a))

      } else {
        stop('Indexing  error.')
      }
    } else {
      stop(y)
    }
  }
  if (any(grepl('=|>|<|!',.Generic))) {return(y)}
  if (inherits(y,'array')) {return(as.md3.array(y))}
  y

}

#euhpq[1,1,1,]<100
#euhpq[1,1,1,]>euhpq[1,1,2,]
#euhpq[1,1,1,][euhpq[1,1,1,]>euhpq[1,1,2,]]


#' @export
is.na.md3 = function(x) {
  ds=.mdsel2codes(.getdimnames(attr(x,'dcstruct'))) #$$$$$$$$$
  dx=.dt_class(x)[ds,,on=.NATURAL]
  is.na(dx[['_.obs_value']])
}
#is.na(euhpq)

#' @export
Math.md3 = function(x,...) {
  x=as.array.md3(x)
  y=get(.Generic)(x,...)
  if (inherits(y,'array')) return(as.md3.array(y))
  y


}

#p5=.md3set(euhpq,TOTAL.HAHA..,value=euhpq[TOTAL.I15_Q..,as='a']/100,)
#p5=.md3set(euhpq,TOTAL.HAHA..,value=euhpq[TOTAL.I15_Q..]/100,); p5[TOTAL..PL.]


#' @export
Summary.md3 = function(x,...) {
  x=as.array.md3(.dropflags(x))
  y=get(.Generic)(x,...)
  if (inherits(y,'array')) return(as.md3.array(y))
  y


}


#' @export
flags = function(x,... ) {
  .md3get(x,...,.obs='status')
}

#' @export
`flags<-` = function(x,value) {

  .md3set(x,value=value,.obs='status')
}

#' @export
str.md3 = function (object,...) {
  mydims = dim.md3(object)
  cat('An MD3 object of', length(mydims),'dimensions with',prod(mydims), 'elements: ', paste(paste(names(mydims),mydims,sep=": "),collapse=', '))
  cat("\nIt contains",data.table:::dim.data.table(object)[1], "non-empty elements")
  cat("\nThe ID of the first element is",paste(lapply((lapply(.getdimnames(object,TRUE),"[",1)),as.character),collapse = '.'),
      'and of the last element',paste(lapply((lapply(.getdimnames(object,TRUE),function(x) x[length(x)])),as.character),collapse = '.'))
}




.matchixsubset2dn = function(ix,xdn) {
  #this is a counterpart to match2dim
  #the code elements in  ix must exist somewhere in xdn
  #examples
  #m0=euhpq['TOTAL.I15_Q.AT:BG.y2005:y2007']
  #.matchixsubset2dn(matrix(3,3,12),m0)
  #.matchixsubset2dn(c('BG','AT'),m0)
  #.matchixsubset2dn(c('BG','AT'),dimnames(m0))
  #.matchixsubset2dn(matrix(NA,1,12),m0)

  xdn=.getdimnames(xdn)
  ixclass=utils::head(class(ix),1)
  stopfn=function() stop('Cannot match this ',ixclass,' to ', paste(names(xdn),collapse=', '))

  dimlen=NULL
  temp=dimnames(ix)
  if (is.array(ix)) {
    dimlen=dim(ix)
    ix=temp
  } else if (is.numeric(ix) & !length(names(ix))) {
    return(xdn)

  } else if (is.numeric(ix) ) {
    ix=list(names(ix))
  } else {
    if (!is.null(temp)) {ix=temp};
    if (!is.list(ix) & length(ix)) {  ix=list(ix)}
  }; rm(temp)

  if (is.null(names(ix))) if (identical(unname(unlist(lapply(xdn,length))),unname(unlist(lapply(ix,length))))) {
    names(ix) = names(xdn)
  }
  if (!is.null(names(ix))) if (all(names(ix) %in% names(xdn) )) {
    return(.match2dim(ix,xdn[names(ix)]))

  }
  #could be that we have an unnamed subset
  if (!length(ix) & length(dimlen)) {
    if (all(dimlen==unlist(lapply(xdn,length)))) {return(xdn)}
    dimlen=dimlen[dimlen>1]
    if (!length(dimlen)) {browser()}
    dimss=base:::match(dimlen,unlist(lapply(xdn,length)),nomatch = NA_integer_)
    if (anyNA(dimss)) stopfn()
    return(xdn[dimss])
  }


  #so we are dealing with a case where dimensions are labelled differntly, or we have a subset, etc.

  .finddim=function (x,xdn) utils::tail(names(xdn)[unlist(lapply(xdn,function(z) all(base::match(x,z,nomatch = 0))))],1)
  if (length(ix)) {
    temp=unlist(lapply(ix,.finddim,xdn))
    if (!length(temp)) {stopfn()}
    if (anyDuplicated(temp)) {stopfn()}
    names(ix)= temp
    return(ix)
  }

  stopfn()
}




#sts_trtu_m




#### DATA
