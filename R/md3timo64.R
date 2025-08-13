#problem time(p3)+6m
#suppressPackageStartupMessages(require('bit64',quietly = TRUE))
#eutemp
.naomit_atomic = function(x) {
  if (!is.atomic(x)) stop('x must be atomic')
  if (!anyNA(x)) return(x)
  x[!is.na(x)]
}



.recycle =function(...,force=FALSE) {
#makes all input arguments the same length by recycling
# except if the input is a scalar/singleton
  if (nargs()==1 & is.list(..1)) {
    vecl=..1
  } else if (nargs()==2 & is.list(..1) & !missing(force)) {
    vecl=..1
  } else {
    vecl=list(...)
  }
  vcl=sapply(vecl,length)
  if (!length(force)) {
    if (length(unique(vcl[vcl>1]))==1) { return(.recycle(vecl,force=FALSE))} else {return(.recycle(vecl,force=TRUE)) }
  }
  if (sum(vcl > 1-force) <2) return(vecl)
  if (length(unique(vcl[vcl>(1-force)]))==1) return(vecl)
  if (any(max(vcl) %% (vcl[vcl>1])!=0)) warning('longer object length is not a multiple of shorter object length')
  for (i in which(vcl>(1-force) & vcl<max(vcl))) {
    vecl[[i]]=rep_len(vecl[[i]],max(vcl))
  }
  vecl
}


.dotsaslistws=function (...)
{
  ctarg = nargs()
  ix = try(list(...), silent = TRUE)
  if (class(ix) != "try-error") {
    return(ix)
  }


  larg=trimws(strsplit(gsub('\\)' ,'',gsub('^list\\(','',deparse(substitute(list(...))))),split=',')[[1]])
  largass=grepl('=',larg)
  if (any(largass)) names(larg)[largass]=trimws(gsub('=.*$' ,'', larg[largass]))
  larg[largass] = trimws(gsub('^.*=' ,'', larg[largass]))
  if (any(!largass)) names(larg)[!largass]=''
  ix = list()
  if (ctarg > 0) {
    for (i in 1:ctarg) {
      if (eval(parse(text = paste0("missing(..",
                                   i, ")")))) {
        ix[[i]] = integer(0)
      }
      else {
        ix[[i]] = suppressWarnings(try(eval(parse(text = paste0("..", i))), silent = TRUE))
        if (grepl("error", class(ix[[i]])[[1]])) {
          ix[[i]] = larg[[i]]
        } else {

        }
      }
    }
  }
  names(ix)=names(larg)
  return(ix)
}

.unlist_keepclass = function(x, recursive = FALSE, use.names = FALSE) {
  lclasses=rapply(x,class,how = 'list')
  temp=unlist(x, recursive=recursive,  use.names= use.names)
  if (!(class(temp)[1] %in% lclasses)) {
  if (length(unique(lclasses))>1) stop('not possible to mix classes here')
  class(temp) = lclasses[[1]]
  }
  temp
}

.vec2POSIXct = function(S=0,M=0,H=0,d=1,m=0,Y=0) {
  as.POSIXct.POSIXlt( list(sec=S,min=M,hour=H,mday=d,mon=m-1,year=Y-1900,wday=NA_integer_,yday=NA_integer_,isdst=0,zone='UTC',gmtoff=NA), tz='UTC')
}

.asint64 = function(x) {
  class(x) = 'integer64'
  x
}

.cttim_create=function(){
  basetbl0=data.frame()
  basetblini = rbind(
    A=c("A","P1Y","Annual","1","year","6","%Y", '1e6',12,'M'),
    S=c("S","P6M","Semi-annual","2","semester","8","",'2',6,'M'),
    Q=c("Q","P3M","Quarterly","4","quarter","10","%Yq%q",'4',3,'M'),
    M=c("M","P1M","Monthly","12","month", "12","%Ym%m",'12',1,'M'),
    W=c("W","P7D","Weekly","1","week","16","%Yw%W", '53', 1440*7,'N'),
    B=c("B","P1B","Business-daily","0","business day","20","%F", '31',1,'B'),
    D=c("D","P1D","Daily","1","day","22","%F", '31',1440,'N'),
    N=c("N","PT1M","Minutely","1440","minute","28","%Y-%m-%dt%H:%M",'60',1,'N')
  )
  yearstarts=bit64::as.integer64.double(unclass(as.Date.character(paste0(1800:2200,'-01-01'))))*86400
  temp=lapply(as.list(yearstarts[101:301]), function(y) .asint64(y)+cumsum(c(0,31,28,31,30,31,30,31,31,30,31,30))*86400)
  #temp[bit64:::c.integer64(diff(yearstarts),365*86400)==31622400] = lapply(temp[bit64:::c.integer64(diff(yearstarts),365*86400)==31622400],function(z) {z[3:12]=z[3:12]+86400; z})
  temp[bit64:::diff.integer64(yearstarts[101:302])==31622400] = lapply(temp[bit64:::diff.integer64(yearstarts[101:302])==31622400],function(z) {z[3:12]=z[3:12]+86400; z})
  monthstarts=.unlist_keepclass(temp); rm(temp)


  makebasetbl = function() {

    basetbl = as.data.frame(basetblini,stringsAsFactors=FALSE)
    colnames(basetbl)=c('fcode','frqsdmx','desc','frqzoo', 'periodname', 'timosuffix', 'formatc', 'maxminor','multiple', 'baseunit')
    for (i in c('frqzoo','timosuffix','maxminor','multiple')) class(basetbl[[i]]) = 'integer'
    basetbl0 <<- basetbl
    return(basetbl)
  }
  outlist=list(frqcodes=data.frame(), frqmatch=character(), fcode=integer())
  outlist$frqcodes=makebasetbl()
  outlist$frqmatch[as.integer(outlist$frqcodes[,6])] =outlist$frqcodes[,1]
  outlist$fcode=as.integer(outlist$frqcodes[,6]); names(outlist$fcode) = tolower(rownames(outlist$frqcodes))
  outlist$frqmatch[60-seq_along(outlist$frqmatch)] = outlist$frqmatch
  outlist$allbasedon=function(bunit='M') { basetbl0[basetbl0$baseunit==toupper(bunit),'fcode'] }
  outlist$basedon=function(fid) {   y=as.character(basetbl0[fid,c('baseunit'),drop=TRUE]); attr(y,'multiple')=basetbl0[fid,c('multiple'),drop=TRUE]; y  }
  outlist$basetbl = function() {basetbl0}
  outlist$yearstartposixfrom1800 = function() {yearstarts}
  outlist$monthstartposixfrom1900 = function() {monthstarts}
  return(outlist)
}


#' Settings and options for timo time object
#'
#'
#' @name MD3-settings
#' @section Indexing in md3
#' @section observation attributes
#' @details hehe
#' @export
.cttim = .cttim_create()


.timo_frq =function(tvec)  {
  if (any(grepl('POSIXct',class(tvec)))) { tvec=as.numeric(tvec) }
  if (!length(tvec)) return(character(0)) # is needed because bit64::asinteger64() %% 60 is 8 !?
  .cttim$frqmatch[abs(.asnum(tvec %% 60))]
}


.timo_class = function(tvec, only=FALSE) {
  attr(tvec,'class') = c('timo', 'timord','integer64')
  if (only) attr(tvec,'class') = 'timo'
  #.Primitive("class<-")(tvec,c("timo",'integer64'))
  tvec
}

.timo_num2class = function(tvec) {
  tvec=suppressWarnings(bit64::as.integer64(tvec))
  tvec=.Primitive("class<-")(tvec,c("timo", 'timord','integer64'))
  tvec
}


#' @export
as.integer64.timo = .asint64
.asnum= function(x) {
  if (bit64::is.integer64(x)) return(suppressWarnings(bit64::as.double.integer64(x)))
  return(as.numeric(x))
}

.timo_is = function(x) {
  if (!is.atomic(x)) return(FALSE)
  if (!any('timo' %in% class(x))) return(FALSE)
  return(TRUE)
}

.timdif_is = function(x) {
  if (!is.atomic(x)) return(FALSE)
  if (!any('timdif' %in% class(x))) return(FALSE)
  return(TRUE)
}




.char2timo_simplest = function(x, frq=NULL) {
  if (anyNA(x)) stop('no NAs permitted.')
  x=trimws(x)
  z=bit64::integer64(length(x))
  dictinteger= c(0:9,0:55)
  names(dictinteger)= c(0:9,sprintf('%02d',0:55))

  aa=.cttim[['yearstartposixfrom1800']]()
  ww=(7-as.integer(format(as.Date.timo(aa),'%u')))*86400+aa

  gurgl=1800L:2200L; names(gurgl)=gurgl
  #kurkl=c(1L:12L,1L:4L); names(kurkl) = c(sprintf('%02d',1:12),1:4)
  fdict=c('Q'='q','S'='s','H'='s','B'='s','M'='m','W'='w','0'=NA,'1'=NA); fdict2= fdict; names(fdict2)=tolower(names(fdict)); fdict=c(fdict,fdict2); rm(fdict2)

  monthbased=setdiff(.cttim[['frqcodes']][.cttim[['frqcodes']][,'baseunit']=='M','fcode'],'A')


  if (!length(frq)) {
    #y1=as.integer(substr(x,0,4))

    frql=substr(x,5,5)
    istiret = {frql=='-' | frql==' '}
    frql0=substr(x,5+istiret,5+istiret)
    frql=fdict[frql0]; frql[frql0=='']='a'



    strrest=substr(x,5+istiret+!is.na(frql),50)
    frql[nchar(strrest)<3 & is.na(frql)] = 'm'
    ym=substr(strrest,0,2)
    frql[nchar(strrest)<6 & is.na(frql)] = 'd'
    frql[is.na(frql)] = 'n'
    frq=frql
  } else {
    if (length(frq) ==1) frq=rep(frq,length(x))
    if (length(frq) !=length(x)) stop('argument frq has to have the same length as x, or be a singleton')
    permissiblef =rep(tolower(.cttim[['frqcodes']][,'fcode']),2); names(permissiblef) =c(tolower(.cttim[['frqcodes']][,'fcode']),toupper(.cttim[['frqcodes']][,'fcode']))
    frq=permissiblef[frq]
    if (anyNA(frq)) stop('argument frq has to be a character vector containing elements ',paste(.cttim[['frqcodes']][,'fcode'], collapse = ', '))
    #strrest=gsub('^[-A-z ]*','',substr(x,5,50))
    #strrest=substr(x,5,50)
    #strrest=ifelse (substr(strrest,0,1)==' ' | substr(strrest,0,1)=='-', substr(strrest,2,49), strrest)
    #strrest=ifelse (!is.na(substr(strrest,0,1)), substr(strrest,2,49), strrest)
    ttt=c(LETTERS,letters,' ', '-'); names(ttt)=ttt
    strrest=substr(x,5+!is.na(ttt[substr(x,5,5)])+!is.na(ttt[substr(x,6,6)]),50)
  }
  yy=gurgl[substr(x,0,4)]


  #frq2u=toupper(frq)
  f2U = .cttim[['frqcodes']][,"fcode"]; names(f2U) = tolower(f2U)
  frq2u=f2U[frq]
  ixmb= frq %in% tolower(monthbased)
  ixyb= frq=='a'
  ixhf= !ixmb & !ixyb

  if (any(ixyb)) {

    ix=(yy[ixyb] - 1800L +1L)
    z[ixyb]=(aa[ix] + .cttim[['frqcodes']]['A','timosuffix'])
    if (all(ixyb)) {return(.timo_class(z))}
  }

  if (any(ixmb)) {
    ix=(dictinteger[strrest[ixmb]]-1)*.cttim[['frqcodes']][frq2u[ixmb],'multiple'] + 1 + (yy[ixmb] - 1900)*12
    mm=.cttim[['monthstartposixfrom1900']]()
    z[ixmb]=(mm[ix] + .cttim[['frqcodes']][frq2u[ixmb],'timosuffix'])
    if (all(ixyb|ixmb)) {return(.timo_class(z))}
  }


  if (any(ixhf)) {
    if (any(frq=='w')) {
      z[frq=='w'] = ww[yy[frq=='w']-1800 + 1L ] + dictinteger[strrest[frq=='w']]*86400L*7L  + .cttim$frqcodes['W','timosuffix']
    }
    ixd=frq %in% c('d','b')
    if (any(ixd)) {
      temp =  list(sec=.cttim$frqcodes['D','timosuffix'], min=0, hour=0, mday=as.integer(gsub('^[^- ]*[- ]','',strrest[ixd])), mon=as.integer(substr(strrest[ixd],0,2))-1, year=yy[ixd]-1900,
                   wday=NA_integer_,yday=NA_integer_,isdst=0,zone='UTC',gmtoff=NA)
      class(temp) = 'POSIXlt'
      z[ixd] = bit64::as.integer64(as.POSIXct.POSIXlt(temp,tz = 'UTC'))
    }
    if (any(frq=='n')) {
      temp=strsplit(gsub(':','-',x[frq=='n']),split='[A-z -]')
      ltemp=lapply(temp,function(q) { p=list(sec=.cttim$frqcodes['N','timosuffix'], min=as.integer(q[5]), hour=as.integer(q[4]), mday=as.integer(q[3]), mon=as.integer(q[2])-1, year=as.integer(q[1])-1900,
                                             wday=NA_integer_,yday=NA_integer_,isdst=0,zone='UTC',gmtoff=NA); class(p)='POSIXlt'; p})
      z[frq=='n']=.unlist_keepclass(lapply(ltemp,function(r) as.timo.POSIXct(as.POSIXct.POSIXlt(r))))
    }

  }
  .timo_class(z)
}


.asmd3f = function(md3c) {
  dx=.dt_class(md3c)
  mydc=.getdimcodes(md3c)
  mydn = lapply(mydc,\(x) if(.timo_is(x)) return(x) else return(x[,1,drop=TRUE]))
  for (i in setdiff(names(mydn),'TIME')) {
    temp=match(dx[[i]], mydn[[i]])
    attr(temp,'levels') <- mydn[[i]]
    class(temp)='factor'
    dx[[i]] = temp
  }
  .md3_class(dx)
}





.char2timo_standard= function(xstring, frq=NULL, guess =TRUE) {
  fixnas =function(invec) {
    ixna=which(is.na(ixna))
    if (!length(ixna)) ixna=grep( '^N',invec)
    if (!length(ixna)) return(invec)
    invec[ixna] = NA
    return(invec)
  }
  if (anyNA(xstring)) {
    ixna=which(is.na(xstring)); xstring[ixna]='1970'
    if (length(ixna)==length(xstring)) {return(.timo_class(rep(NA_real_,length(ixna))))}
  } else {ixna=NULL}

  xstring=gsub('Z$|z$','',gsub('^y','',trimws(xstring)))
  xstring=gsub('\\s','',xstring) # NEW INSERTION
  x = gsub('\\s','-', xstring)
  .guessit =function() .char2timoguessfurhter(.recycle(xstring,frq,force = TRUE))
  if (!length(x)) return(.timo_class(bit64::integer64()))
  if (any(grepl('e-3[0-9][0-9]$',x))) {ix=grepl('e-3[0-9][0-9]$',x); x[ix]= .timo2char(.timo_class(as.numeric(x[ix])))}

  slenconv=character(); slenconv[4] = 'a'; slenconv[16]='n'; slenconv[8]='d'; slenconv[10]='d'; slenconv[19]='n';
  if (guess) if (any(grepl('[A-z][A-z]',x))) { return(fixnas(.guessit()))}

  xs=strsplit(x,split="[^0-9]")
  xs=lapply(xs,function(x) x[nchar(x)>0])
  if (is.null(frq)) ff=tolower(substr(x,5,5)) else ff=tolower(frq)

  slen=nchar(xstring); fvec=character()
  if (!is.null(frq)) {
    fvec  = rep(NA_character_,length(x))
    fvec[] = tolower(frq)
  } else {
    if (nchar(trimws(head(xstring,1)))==nchar(head(x,1)))  {
      fvec = slenconv[slen]
      fvec[is.na(fvec)] = ff[is.na(fvec)]
    }

  }
  #if (length(fvec)) {fvec[fvec=='h']<-'s'}

  if (any(na.omit(ff)=='w' & na.omit(fvec!='w'))) { fvec[which(ff=='w')] = 'w'}
  fpermitted = toupper(fvec) %in% toupper(names(.cttim$fcode))
  if (any(!fpermitted)) { fvec[!fpermitted] =NA}

  if (guess) {
    if (anyNA(match(fvec,tolower(rownames(.cttim$frqcodes)),NA))) {
      if (any(is.na(fvec) & !is.na(x)))   return(.guessit())
    }
    if (length(xstring)>0 & length(fvec)==0) { return(.guessit()) }
  }

  xs[slen==0] = NA
  if (all(unlist(lapply(xs,length))==1L)) {
    if (all(nchar(xs)>4)) {
      if (all(grepl('^[0-9]*$',xs))) {
        if (!is.null(frq)) if (frq %in% c('d','b')) {
          return(.char2timo(paste0(substr(xs,0,4),'-',substr(xs,5,6),'-',substr(xs,7,8)),frq = frq))
        }
          xs=lapply(xs,function(g) c(substr(g,0,4),substr(g,5,100)))

          if (is.null(frq) & all(nchar(xs)==6)) { #likely months 201204
            return(.char2timo(paste0(substr(xs,0,4),'-',substr(xs,5,6)),frq = 'm'))
          }

      }
    }
  }
  vyear=as.integer(substr(unlist(lapply(xs,'[[',1)),0,4))-1900L
  if (any(is.na(vyear) & !is.na(x))) { return(fixnas(.guessit())) }
  if (guess) if (any(na.omit(abs(vyear))>290)) { return(fixnas(.guessit())) }
  vmon=as.integer(unlist(lapply(xs,'[',2)))-1
  vmon[is.na(vmon)] = 0
  if (all(vmon>(.cttim$frqcodes[toupper(fvec),'maxminor'])*2)) {return(fixnas(.guessit()))}
  if (any(na.omit(vmon)>(.cttim$frqcodes[toupper(fvec),'maxminor'])-1)) {
    tempix=head(which(vmon>(.cttim$frqcodes[toupper(fvec),'maxminor'])-1));
    warning('Characters like ', paste(paste(.cttim$frqcodes[toupper(fvec[tempix]),'desc'],x[tempix],sep=': '), collapse=', '), ' seem strange' )
  }
  vmonmult=c(q=3,s=6,m=1,"-"=1,w=NA,'/'=1,'.'=1,d=1,b=1,n=1)[ff];
  vmon=vmon*vmonmult +1; vmon[is.na(vmon)]=1
  vday=as.integer(unlist(lapply(xs,'[',3))); vday[is.na(vday)]=1
  vhour=as.integer(unlist(lapply(xs,'[',4))); vhour[is.na(vhour)]=0
  vmin=as.integer(unlist(lapply(xs,'[',5))); vmin[is.na(vmin)]=0

  #sapply(xs,[[1]])
  #.Internal(as.POSIXct(list(0,0,0,1,0,117:119,0,0,0,'GMT',NA), 'GMT'))
  #vout=.Internal(as.POSIXct(list(.cttim$fcode[fvec],vmin,vhour,vday,vmon,vyear,0,0,0,'GMT',NA), 'GMT'))
   #vout=.Internal(as.POSIXct(list(.cttim$fcode[fvec],vmin,vhour,vday,vmon-1,vyear,0,0,0,'GMT',NA), 'GMT'))
   vout=.Internal(as.POSIXct(list(sec=.cttim$fcode[fvec],min=vmin,hour=vhour,mday=vday,mon=vmon-1,year=vyear,wday=NA_integer_,yday=NA_integer_,isdst=0,zone='UTC',gmtoff=NA), 'GMT'))
  if (any(fvec=='w')) {
    temp=as.POSIXct(paste0(unlist(lapply(xs[fvec=='w'],'[',1)),'-01-01'),tz='GMT',format='%F')
    temp=temp + {(8-as.integer(format(temp,'%u'))) %%7 + 7*(as.integer(unlist(lapply(xs[fvec=='w'],'[',2))) -1)}*86400 + .cttim$fcode['w']
    vout[fvec=='w'] = temp
  }

   .timo_num2class(fixnas(vout))
}

#
# www=c('2011w12','2021w33')
# xs=list(sapply(strsplit(www,'[^0-9]'),'[',1),as.numeric(sapply(strsplit(www,'[^0-9]'),'[',2)))
# temp=as.POSIXct(paste0(xs[[1]],'-01-01'),tz='GMT',format='%F')
# temp=temp + (7-as.integer(format(temp,'%w')) + 7*xs[[2]] )*86400 + .cttim$fcode['w']
# format(temp+86400*3,'%Yw%W')
#
#
#




.char2timoguessfurhter = function(x, fq=NULL) {
  #this function taxes a string vector (like paste0('2020-Q',1:3))
  if (is.list(x)) { fq=x[[2]]; x=x[[1]]}
  fq=NULL # this is a temproary fix
  x = gsub('^y','',trimws(x))
  monthnamecandidates=c(month.name,month.abb,format(ISOdate(2000, 1:12, 1), "%b"),format(ISOdate(2000, 1:12, 1), "%B"))
  if (is.null(fq)) {

        strangtypes=unique(gsub('[0-9]| ','',trimws(x)))
    if (head(strangtypes,1) == '') {
      if (!anyNA(bit64::as.integer64(gsub(' ','',x)))) {
        if (all(nchar(x)==14)) {temp=.asnum(as.POSIXct(gsub(' ','',x),format='%Y%m%d%H%M',tz='UTC')); return(.timo_num2class(temp-temp%%60 +.cttim$fcode['n'])) } else
        if (all(nchar(x)==12)) {temp=.asnum(as.POSIXct(gsub(' ','',x),format='%Y%m%d%H%M',tz='UTC')); return(.timo_num2class(temp-temp%%60 +.cttim$fcode['n'])) } else
        if (all(nchar(x)==10)) {temp=.asnum(as.POSIXct(gsub(' ','',x),format='%Y%m%d%H',tz='UTC')); return(.timo_num2class(temp-temp%%60 +.cttim$fcode['n'])) } else
        if (all(nchar(x)==8)) {
          temp=.asnum(as.POSIXct(gsub(' ','',x),format='%Y%m%d',tz='UTC'));
          if (all(!is.na(temp))) {return(.timo_num2class(temp-temp%%60 +.cttim$fcode['d'])) } else { x = gsub('\\s','-',trimws(x))}
        }

      } else {
        return(timo())
      }

    } #else

    if (head(strangtypes,1) == '-') {
       temp=gsub('^[0-9][0-9][0-9][0-9]-','',gsub(' ','',x))

       if (all(as.integer(temp)<13)) {
          if (sum(nchar(temp)>1)/length(temp)>.1) {
            return(.char2timo(gsub('-','m',x),frq='M',guess = FALSE)) #must be monthly
          } else if (all(as.integer(temp)<5) & all(as.integer(temp)>0)) {
            return(.char2timo(gsub('-','q',x),frq='Q',guess = FALSE)) #probably quarterly
          } else if (length(temp) < 24) {
            return(.char2timo(gsub('-','m',x),frq='M',guess = FALSE)) #probably monthly
          } else {
            stop(x)
          }
      } else {
        stop(1)
      }

    } else
    if (head(strangtypes,1) == '--:')  { temp=.asnum(as.POSIXct(x,format='%Y-%m-%d %H:%M',tz='UTC')); return(.timo_num2class(temp-temp%%60 +.cttim$fcode['n'])) } else
    if (head(strangtypes,1) == '--::') { temp=.asnum(as.POSIXct(x,format='%Y-%m-%d %H:%M:%S',tz='UTC')); return(.timo_num2class(temp-temp%%60 +.cttim$fcode['n'])) }


    if (all(tolower(strangtypes) %in% tolower(monthnamecandidates))) { x = paste0(strangtypes,'-',gsub('[A-z]','',x))}
    lmy=strsplit(gsub('\\s','-',trimws(x)),split='[^0-9A-z]+')

    nbparts=unique(unlist(lapply(lmy,length)))

    if (head(strangtypes,1) %in% c('h','-h')) { #most likley ECB-style half year 2013h1
      temphs=matrix(as.integer(unlist(strsplit(gsub('[^0-9A-z]+','',x),split='h'))),2)
      if (anyNA(temphs)) { stop('cannot understand') }
      if (any(temphs[2,]>2)) { stop('Seems like half year, but numbers do not make sense') }
      return(.timo_num2class(.vec2POSIXct(.cttim$fcode['s'],0,0,1,(temphs[2,]-1)*6+1,temphs[1,])))

    }



    if (length(unique(nbparts))>1) { stop('cannot mix stuff when I need to guess') }
    temp=suppressWarnings(matrix(as.integer(unlist(lmy)),nrow=nbparts))

    ixm= logical(0)
    if (anyNA(temp)) {
      ltranspo=lapply(as.list(1:nbparts),function(x) unlist(lapply(lmy,'[[',x)))

      monthnames=which(unlist(lapply(ltranspo,function(x) all(tolower(x) %in%  tolower(monthnamecandidates)))))
      if (length(monthnames)) {
        temp=match(tolower(ltranspo[[monthnames]]),tolower(month.abb))
        for (m in 1:3) {
          if (!anyNA(temp)) {break}
          temp[is.na(temp)]=match(tolower(ltranspo[[monthnames]]),tolower(monthnamecandidates[m*12+1:12]))[is.na(temp)]
        }
        ltranspo[[monthnames]]=temp
        temp=matrix(as.integer(unlist(ltranspo)),nrow=nbparts,byrow = TRUE)
        ixm=logical(3); ixm[monthnames]=TRUE
        #stop(apply(sapply(ltranspo,paste,sep=''),1,paste,collapse='-'))
      }
    }
      if (nbparts==3) {


          mmtemp=apply(temp,1,range)
          ixy=apply(mmtemp,2,function(x) any(x>31))
          if (!length(ixm)) ixm=apply(mmtemp,2,max)<13 & !ixy
          if (anyNA(ixm)) stop('Do not mix date formats for guesswork. try to make sure they are all the same format')
          if (sum(ixm)!=1) { if (!any(ixy)) { ixy=c(TRUE,FALSE,FALSE)}; if (which(ixy) %in% c(1,3)) { ixm=c(FALSE,TRUE,FALSE)} else stop('Dates like ',head(x),' seem strange') }
          if (!any(ixy)) { ixy[ifelse(which(ixm)==3,1,3)]=TRUE}
          if (all(temp[ixy,] < 100 & temp[ixy,] > -1)) {temp[ixy,] = temp[ixy,]+2000}
          return(.timo_num2class(.asnum( .vec2POSIXct(Y=temp[ixy,,drop=TRUE],m=temp[ixm,,drop=TRUE],d=temp[!ixy & !ixm,,drop=TRUE],S=.cttim$fcode['d']))))

      }

      if (nbparts==2 & length(ixm)) {
        #return(.char2timo(paste0(temp[3-which(ixm),],'m',temp[which(ixm),]), guess = FALSE))
        return(.timo_num2class(.vec2POSIXct(.cttim$fcode['m'],0,0,1,temp[which(ixm),],temp[3-which(ixm),])))
      }

      if (nbparts==2 & is.null(fq)) {
        myfq=toupper(substr(ltranspo[[2L]],0,1))
        if (!length(setdiff(myfq,.cttim$basetbl()[,'fcode']))) {return(.char2timo(x,myfq))}

      }


  }

  #format(x,format=myf,tz='UTC')))

  #ixbad=!(fq %in% tolower(rownames(.cttim$frqcodes)))
  #strangtypes=unique(gsub('[0-9]| ','',x[ixbad]))
  stop('Could not identify what a period like ', paste(head(x,2),collapse=', '),' could refer to')
  #browser()
  #return(.timo_num2class(vout))
}





.char2timo = function (xstring, frq = NULL, guess = FALSE) {



  if (!length(xstring)) {
    return(.timo_class(bit64::integer64()))
  }


  if (base::is.numeric(xstring)) {
    return(as.timo(xstring,frq=frq))
  }

  xstring=as.character(xstring)
  xfactor=as.factor(xstring)
  xperiods=levels(xfactor)
  mytimo=.timo_class(bit64::integer64())
  if (!guess & !anyNA(utils::head(xperiods,2000))) {
    mytimo=.char2timo_simplest(as.character(xperiods))
    if (anyNA(mytimo)) {mytimo=.timo_class(bit64::integer64())  }
  }

  if (!length(mytimo)) {
    mytimo = .char2timo_standard(xperiods,guess=guess)
    if (anyNA(mytimo)) {mytimo=.timo_class(bit64::integer64())  }
  }


  if (!length(mytimo)) {
    mytimo = .char2timo_standard(xstring,frq=frq,guess=guess)
  }

  if (anyNA(mytimo)) {
    stop('Cannot interpret time periods like ', head(xperiods[is.na(mytimo)],1), ifelse(length(frq),'\n.Try pass into another format.',' \nTry specifying argument frq'))
  }


  mytimo[as.integer(xfactor)]


}


.timo2char = function(intimo, possperiods=NULL) {

  if (length(possperiods)) {
    possperiods=.asint64(possperiods)
  } else {
    possperiods=unique(.asint64(intimo))
  }
  if (anyNA(possperiods)) return(.timo2char_native(possperiods))
  ixperiods=match(.asint64(intimo),possperiods)

  tout=.timo2char_fast(possperiods)[ixperiods]
  if (!anyNA(tout)) return(tout)

  tout[is.na(tout)] = .timo2char_native(intimo[is.na(tout)])
}


.timo2char_fast = function(intimo) {


  if (length(intimo)) if (any(utils::head(.asint64(intimo),100)>1e12)) { return(rep(NA_character_,length(intimo)))}
  hadnas=integer()
  if (anyNA(intimo)) {
    if (all(is.na(.asint64(intimo)))) return(rep(NA_character_,length(intimo)))
    hadnas=which(is.na(.asint64(intimo)))
    intimo[hadnas]= .cttim$frqcodes[1,'timosuffix']
  }
  myf=.timo_frq(intimo)
  if (anyNA(myf)) myf[is.na(myf)] = ''

  # mformat=.cttim$frqcodes[toupper(myf),'formatc']
  # if (anyNA(mformat)) { mformat[is.na(mformat)]=""}
  # if (!length(mformat)) {if (!length(myf)) return(character()) else stop('unknown frequency')}
  #intimoct=as.POSIXct.timo(intimo)
  mdict=.cttim$monthstartposixfrom1900()
  mwhich=findInterval(intimo,mdict)
  monthbased=.cttim$frqcodes[myf,'baseunit']=='M'
  yyy=(mwhich-1) %/% 12 + 1900
  mmm=(mwhich-1) %% 12  + 1
  cout =rep(NA_character_,length(intimo))
  if (any (monthbased)) {
    qqss=.cttim$frqcodes[myf[monthbased],'baseunit']=='M' & myf[monthbased]!='M'
    subper=mmm[monthbased]
    subper[qqss] = (mmm[monthbased][qqss]-1) %/% .cttim$frqcodes[myf[monthbased][qqss],'multiple'] +1
    subper[!qqss] = sprintf('%02d',   subper[!qqss] )
    f2low=names(.cttim$fcode); names(f2low) = toupper(f2low)
    cout[monthbased] =sprintf('%04d%s%s',yyy[monthbased],f2low[myf[monthbased]],subper)
  #  cout[monthbased] =stringi::stri_c(yyy[monthbased],f2low[myf[monthbased]],subper)
   # cout[monthbased]=paste0(yyy[monthbased], f2low[myf[monthbased]], subper )
    cout[myf=='A'] = yyy[myf=='A']
    if (length(hadnas)) {cout[hadnas] = NA_character_}
    if (all(monthbased)) return(cout)
  }


  doybased = myf %in% 'W'
  if (any (doybased)) {
    startday=mdict[(yyy-1899)*12-11]

    dayoy=(.asint64(intimo)-startday +1) %/% 86400 +1
    wdict=c(0,6:1); names(wdict)=1:7
    www=(dayoy-wdict[(format.Date(as.Date.timo(startday),'%u'))]) %/% 7 +1
    cout[doybased] = sprintf('%04dw%02d',yyy[doybased],as.integer(www))
  }


  minbased=(.cttim$frqcodes[myf,'baseunit']=='N' | .cttim$frqcodes[myf,'baseunit']=='B') & myf!='W'
  if (!any(minbased)) {
    if (length(hadnas)) {cout[hadnas] = NA_character_}
    return(cout)
  }
  dayom=as.integer((.asint64(intimo)-mdict[mwhich]) %/% 86400 +1)
  cout[minbased] = sprintf('%04d-%02d-%02d',yyy[minbased],mmm[minbased], dayom[minbased] )
  cout[myf=='N'] = sprintf('%st%02d:%02d',cout[myf=='N'],as.integer((.asint64(intimo)[myf=='N'] ) %% 86400 %/% 3600),
                           as.integer(.asint64(intimo)[myf=='N']  %% 86400 %% 3600 %/% 60))


  if (length(hadnas)) {cout[hadnas] = NA_character_}
  return(cout)
}



.timo2char_native = function(intimo) {
  myf=.timo_frq(intimo)
  if (anyNA(myf)) myf[is.na(myf)] = ''
  mformat=.cttim$frqcodes[toupper(myf),'formatc']
  if (anyNA(mformat)) { mformat[is.na(mformat)]=""}
  if (!length(mformat)) {if (!length(myf)) return(character()) else stop('unknown frequency')}
  intimoct=as.POSIXct.timo(intimo)
  sout=format.POSIXct(intimoct,mformat, tz='GMT')
  if (!(any(c('S','Q','W') %in% .naomit_atomic(myf)))) return(sout)
  if (any(.naomit_atomic(myf)=='W')) { tempix=grepl('w00$',sout); if (any(tempix)) { temp2=as.POSIXct(intimoct[tempix]); sout[tempix] = format(temp2-86400, .cttim$frqcodes['W','formatc'])};}
  if (!(any(c('S','Q') %in% .naomit_atomic(myf)))) return(sout)
  sout[myf=='S'] = paste0(format.POSIXct(intimoct[myf=='S'],"%Y", tz='GMT'),'s',(as.integer(format(intimoct[myf=='S'],"%m", tz='GMT'))-1) %/% 6 + 1)
  sout[myf=='Q'] = paste0(format.POSIXct(intimoct[myf=='Q'],"%Y", tz='GMT'),'q',(as.integer(format(intimoct[myf=='Q'],"%m", tz='GMT'))-1) %/% 3 + 1)
  sout
}


.checkdigit = function(intimo) {
  if (any(is.na(.cttim$frqmatch[abs(.asnum(intimo %% 60))]))) stop('Frequency not recognized')
  intimo
}

.checkbizday =function(intimo) {

  ixbd=.cttim$frqmatch[abs(.asnum(intimo %% 60))] == 'B'
  if(!any(ixbd)) { return(intimo)}
  ixweekend = format.POSIXct(as.POSIXct.timo(intimo),format = '%u') > '5'
  if (any(ixweekend)) { stop('Cannot transfer to business day frequency. Dates like ', .timo2char(head(intimo[ixweekend],1)), ' are on a weekend.') }
  intimo
}

#
# .timo_withinold = function(xtimo, referstoend = FALSE, classit=TRUE) {
#
#   #if (!referstoend) return(switch(classit+1,.asnum,function(x) x)(.char2timo(as.character(as.timo(xtimo)))))
#
#   if (!referstoend) {
#     return(switch(classit+1,.asnum,return)(.char2timo(as.character(as.timo(xtimo)))))
#
#   }
#   temp=.asnum(.timo_addnumeric(.char2timo(as.character(as.timo(xtimo))),1, TRUE) )
#   temp = temp - .cttim$frqcodes[.timo_frq(temp),'timosuffix']*2
#   #temp = as.POSIXct(temp, tz='GMT')
#   if (classit) temp = .timo_num2class(temp)
#   return(temp)
#
#
# }

.timo_within = function(xtimo, referstoend = FALSE, classit=TRUE) {

  xtimo=as.timo(xtimo)
  temp2=.recycle(xtimo,referstoend)
  xtimo=temp2[[1]]; referstoend = temp2[[2]]


  xfrq= .timo_frq(xtimo)
  xfrb=.cttim$basedon(xfrq)
  temp=.asnum(xtimo)
  for (bb in na.omit(unique(xfrb))) { #CHANGED RECENTLY !!???
    if (bb=='N') {
       temp[xfrb=='N'] = temp[xfrb=='N'] -temp[xfrb=='N'] %% (60*attr(xfrb,'multiple')[xfrb=='N']) + .cttim$fcode[tolower(xfrq)[xfrb=='N']]
       if (length(referstoend)>1) { rtetemp= referstoend & xfrb=='N'} else {rtetemp=as.logical(referstoend*(xfrb=='N'));}
       if (any(rtetemp)) { temp[rtetemp]=temp[rtetemp] - 2* .cttim$fcode[tolower(xfrq)[rtetemp]] + (60*attr(xfrb,'multiple')[rtetemp]) }
    }
    if (bb=='M') {
      ltimo=as.POSIXlt(xtimo)[xfrb=='M']
      rtetemp=referstoend; if (length(referstoend)>1) { rtetemp=referstoend[xfrb=='M'] }

      #templ=as.POSIXct.POSIXlt( list(0,0,0,1,ltimo$mon - ltimo$mon %% attr(xfrb,'multiple')[xfrb=='M'] + rtetemp*attr(xfrb,'multiple')[xfrb=='M'],  ltimo$year,NA,NA,NA), tz='UTC')
      templ=.vec2POSIXct(0,0,0,1, ltimo$mon - ltimo$mon %% attr(xfrb,'multiple')[xfrb=='M'] + rtetemp*attr(xfrb,'multiple')[xfrb=='M']+1, ltimo$year+1900L)

      temp[xfrb=='M']=.asnum(templ) + 2*(.5-rtetemp)*.cttim$fcode[tolower(xfrq)[xfrb=='M']]
    }
    if (bb=='B') {
      temp[xfrb=='B']=(temp[xfrb=='B'] - temp[xfrb=='B'] %% 86400 + .cttim$fcode[tolower(xfrq)[xfrb=='B']])
      if (length(referstoend)>1) { rtetemp= referstoend & xfrb=='B'} else {rtetemp=as.logical(referstoend*(xfrb=='B'));}
      #if (any(xfrb=='B' & referstoend)) { temp[xfrb=='B' & referstoend]=temp[xfrb=='B' & referstoend] - 2* .cttim$fcode[tolower(xfrq)[xfrb=='B']] + 86400 }
      if (any(rtetemp)) { temp[rtetemp]=temp[rtetemp] - 2* .cttim$fcode[tolower(xfrq)[rtetemp]] + (60*attr(xfrb,'multiple')[rtetemp]) }

    }

  }
  if (classit) temp = .timo_num2class(temp)
  return(temp)


}


# .timo_cfrqold =function(xtimo, frq, refersto = c('start','end','middle'), classit=TRUE) {
#   refersto=c('end', 'start','middle')[pmatch(tolower(trimws(refersto[[1]])),c('end','start','middle'))]
#   #tout=unclass(xtimo)
#   tout=.asnum(xtimo)
#   if (refersto!='end') {
#     tout=.timo_within(tout , referstoend= FALSE, classit=FALSE)
#     tout=tout- tout%% 60 + .cttim$frqcodes[toupper(frq),'timosuffix']
#   } else {
#     tout=.timo_within(tout , referstoend= TRUE, classit=FALSE)
#     tout=tout - (tout%% 60) + 60-.cttim$frqcodes[toupper(frq),'timosuffix']
#   }
#   if (classit) tout = .timo_num2class(tout)
#   .checkbizday(tout)
#
# }


.timo_cfrq =function(xtimo, frq, referstoend = FALSE, classit=TRUE) {
  ltmp=.recycle(xtimo,frq,referstoend,force=TRUE[length(referstoend)>1]); xtimo=ltmp[[1]]; frq=ltmp[[2]]; referstoend=ltmp[[3]]
  #tout=unclass(xtimo)
  tout=.asnum(xtimo)
  if (any(!referstoend)) {
    tout[!referstoend]=.timo_within(tout[!referstoend] , referstoend= FALSE, classit=FALSE)
    #tout[!referstoend]=tout[!referstoend]- tout[!referstoend]%% 60 + .cttim$frqcodes[toupper(frq[!referstoend]),'timosuffix']
  }
  if (any(referstoend)) {
    tout[referstoend]=.timo_within(tout[referstoend] , referstoend= TRUE, classit=FALSE)
    #tout=tout - (tout%% 60) + 60-.cttim$frqcodes[toupper(frq),'timosuffix']
  }
  tout=tout - (tout%% 60) + referstoend*60 + (1-2*referstoend)*.cttim$frqcodes[toupper(frq),'timosuffix']
  if (anyNA(tout)) {
    whicharecrazy=setdiff(unique(frq),rownames(.cttim$frqcodes))
    if (length(whicharecrazy)) stop('The provided frequency codes like: ', paste(whicharecrazy, collapse=','),' are not defined.')
  }
  if (classit) tout = .timo_num2class(tout)
  if (all(is.na(tout))) return(tout)
  .checkbizday(tout)

}




#' @export
format.timo = function(x, format = "", tz = "", usetz = FALSE, ...) {
  xout= format.POSIXct(as.POSIXct.timo(x,tz=tz),format,tz,usetz,...)
  if (!(any(trimws(format)==''))) { return(xout )}
  if (length(format)==1L) if(trimws(format[1L])=='') { return(.timo2char(x))}


  xl=.recycle(x,trimws(format))
  xout= format.POSIXct(as.POSIXct.timo(xl[[1]],tz=tz),xl[[2]],tz,usetz,...)
  if (!(any(xl[[2]]==''))) {return(xout)}
  xout[xl[[2]]==''] = .timo2char(xl[[1]][xl[[2]]==''])
  xout


}

#' @export
print.timo = function(x, ...) {
  temp=.timo2char(x)
  if (length(temp)) {
    print.default(temp,...)
  } else {
    cat("timo(0)\n")
  }
  invisible(temp)
}


#' @export
is.numeric.timo = function(x) { FALSE}





#' A time period class for mixed frequencies
#'
#' @param x vector to be converted to timd vector. Can be character, date, PSOIXct, etc.
#' @param frq frequency code like "A" or "M" or "B"
#' @param ... other arguments passed on to subsequent methods
#' @details The timo object is a vector denoting mixed frequency periods like '2013', '2013m06', or '2013-06-24'.
#'
#' The following denotes the existing frequency codes and their notation
#' \itemize{
#' \item \code{A} annual, notation \code{'2011'}
#' \item \code{S} semi-annual, notation \code{'2011s2'}, second half of 2011
#' \item \code{Q} quarterly, notation \code{'2011q3'}, third quarter of 2011
#' \item \code{M} monthly, notation \code{'2011m10'}, October 2011
#' \item \code{W} weekly, notation \code{'2011w39'}, 39th week of 2011
#' \item \code{B} business-day, notation \code{'2011-10-05'}, 5 October 2011
#' \item \code{D} daily, notation \code{'2011-10-02'}, 2 October 2011
#' \item \code{N} minutely, notation \code{'2011-10-02t13:24'}, 2 October 2011, 13:24h (default GMT)
#' }
#'
#' @seealso \code{\link{seq.timo}}, \code{\link{frequency.timo}}, \code{\link{Sys.timo}}
#' @examples
#' timo(c('2011q1','2012q1'))
#' as.timo(c('2011q1','2012q1'))
#' as.timo('2011q1',frq='M')
#' as.timo('2011q1') + 0:8
#'
#' as.timo(c('2011q1', '2011')) + 1
#' as.timo('2011/9/23')
#'
#' timo('2011/9/23',frq='B') #is the same as
#' as.timo('2011/9/23',frq='B') #Note that B means business day frequency as opposed to D for daily frequency
#'
#' frequency(timo('2011m03'),'W') # convert to weekly frq
#'
#' timo(y2019q4) -'2q' #substract 2 quarters
#' timo(y2019q4) -'2a' #substract 2 years
#' timo("2019q4") +'12m' #add 12 months
#'
#' seq(as.timo('2011m03'),'2012m01') #all months from March 2011 to Jan 2012
#' seq(timo('2014q1'),'2014q2',frq='B') #all business days in the first half of 2014
#'
#' sort(as.timo(c("2012m01","2011","2011q2","2011s2","2011m03","2011w51",
#'    "2011-08-23","2012", '2011-08-23 11:11')))
#'
#' sort(as.timo('2011q1') + 0:8, decreasing=TRUE)
#'
#' data(euhpq)
#' time(euhpq)
#' time(euhpq)+1
#' time(euhpq)+'1Q'
#' time(euhpq)+'1M'
#' frequency(time(euhpq),'D')
#' frequency(time(euhpq),'D')+'2W'
#'
#'
#' @export
as.timo= function (x,frq=NULL,...) UseMethod("as.timo")




#' @describeIn as.timo convert to timo object of time periods
#' @export
timo = function(..., frq=NULL) {
  temp=try(.timo_num2class(unlist(lapply(lapply(list(...),as.timo,frq=frq),.asnum))),silent=TRUE)
  if (!any(grepl('error',class(temp)))) return(temp)
  suppressWarnings(as.timo(unlist(.dotsaslistws(...)),frq=frq))
}


#' @export
as.timo.default = function (x, frq=NULL,...){
  if (is.null(frq)) if (any(grepl('POSIX',class(x)))) {frq='N'} else {frq='D'}
  tout=unclass(as.POSIXct(x))
  if (length(frq)) if (is.character(frq)) if (any(frq=='H'|frq=='h')) { frq[tolower(frq)=='h']<-'S'}
  tout= tout - tout%% 60 + .cttim$frqcodes[toupper(frq),'timosuffix']
  if (!is.null(frq)) { tout=.timo_cfrq(tout,frq,referstoend=FALSE,classit=FALSE)}
  .timo_num2class(tout)
}


#' @export
as.timo.numeric = function (x, frq=NULL, ...) {
  myfq = function(otherdefault) {
    if (is.null(frq)) return(otherdefault)
    frq
  }
  mm=suppressWarnings(range(x,na.rm = TRUE))
  if (any(is.infinite(mm))) return(.timo_class(rep(bit64:::NA_integer64_,length(x))))
  if (all(abs(mm) > 0 & abs(mm) < 1e-300)) { return(.timo_class(.asint64(x))) }
  if (mm[[1]]> 900 & mm[[2]] < 2500) {
  return(.timo_class(.cttim$yearstartposixfrom1800()[as.integer(x)-1799]+.cttim$fcode['a'])) } #return(.char2timo(as.character(x),frq=myfq('a')))
  #if (mm[[1]]> 900 & mm[[2]] < 2500) { return(.char2timo(as.character(x),frq=myfq('a')))}
  if (mm[[1]]> -26000 & mm[[2]] < 30000) { class(x) = 'Date'; return(as.timo.default(as.character(x),myfq('d')))}
  if (mm[[1]]> 1.8e7 & mm[[2]] < 2.2e7) {  return(.timo_num2class(.vec2POSIXct(Y=as.numeric(substr(x,0,4)),m=as.numeric(substr(x,5,6)),d=as.numeric(substr(x,7,8)),S=.cttim$fcode['d'])))}
  if (mm[[1]]> 1.8e11 & mm[[2]] < 2.2e11) {  return(as.timo.default(strptime(x,'%Y%m%d%H%M'),myfq('n')))}
  if (mm[[1]]> 1.8e13 & mm[[2]] < 2.2e13) {  return(as.timo.default(strptime(x,'%Y%m%d%H%M%S'),myfq('n')))}

  .timo_num2class(x)
}


#' @export
as.timo.character = function (x, frq=NULL,...){
  if (is.null(frq)) return(.char2timo(x,guess = TRUE))
  if (length(frq)) if (is.character(frq)) {
    if (any(frq=='H'|frq=='h')) { frq[tolower(frq)=='h']<-'S'}
    if (any(frq=='B'|frq=='b')) {
      if (length(frq)==1) { frq=rep(frq,length(x))}
      xb=x[tolower(frq)=='b']
      if (all(nchar(head(xb,100))<8)) { frq[tolower(frq)=='b']<-'S' }
    }
  }
  as.timo.default(.char2timo(x, frq), frq)
}


#' @export
as.timo.yearmon = function(x,frq=NULL, ...) {
  if (is.null(frq)) frq = 'M'
  as.timo.default(x,frq)
}

#' @export
as.timo.yearqtr = function(x,frq=NULL, ...) {
  if (is.null(frq)) frq = 'Q'
  as.timo.default(x,frq)
}

#' @export
as.POSIXct.timo = function(x,tz='',...) {
  x=suppressWarnings(.asnum(x))
  class(x) = c("POSIXct","POSIXt")
  if (tz=='') attr(x, "tzone") ='UTC'
  x
}

#' @export
as.timo.POSIXct = function(x,tz='',frq=NULL,...) {
  x=suppressWarnings(bit64::as.integer64.double(x))
  as.timo.timo(.timo_class(x-x%%60 + .cttim$fcode['n']),frq=frq)

}



#' @export
as.POSIXlt.timo = function(x,tz='',...) {
  as.POSIXlt.POSIXct(as.POSIXct.timo(x))
}


#' @export
as.Date.timo = function(x,tz='',...) {
  as.Date(as.POSIXct.timo(x))
}

#' @export
as.timo.Date = function(x,tz='',...) {
  .timo_num2class(unclass(x) * 86400 + .cttim$fcode['d'])
}

#' @export
as.integer64.POSIXct = function(x, ...) {
  bit64::as.integer64.double(x)
}

#' @export
as.timo.timo = function(x, frq=NULL,...) {
  if (!is.null(frq)) { return(.timo_cfrq(x,frq,referstoend = FALSE, classit = TRUE))}
  return(x)
}

#' @export
as.timo.integer64 = function(x, ...) {
  .timo_class(x)
}


#' @export
as.character.timo = function(x,format="",...) {
  if (nchar(format)) {
    format.POSIXct(x,format=format,...)
  }

  .timo2char(x)
}



#' @export
as.yearmon.timo = function(x,tz='',...) {
  zoo::as.yearmon(as.POSIXct.timo(x))
}

#' @export
as.yearqtr.timo = function(x,tz='',...) {
  zoo::as.yearqtr(as.POSIXct.timo(x))
}

#' @export
as.data.frame.timo=as.data.frame.numeric


#' Get current date and tiem as timo
#'
#' @param frq optional paramter to be set to either NULL (default) or a frequency notation like "A", "S", "Q", "M","D", "B" or "N". Used for converting frequencies
#' @details The timo object is a vector denoting mixed frequency periods like '2013', '2013m06', or '2013-06-24'.
#'
#' The following denotes the existing frequency codes and their notation
#' \itemize{
#' \item \code{A} annual, notation \code{'2011'}
#' \item \code{S} semi-annual, notation \code{'2011s2'}, second half of 2011
#' \item \code{Q} quarterly, notation \code{'2011q3'}, third quarter of 2011
#' \item \code{M} monthly, notation \code{'2011m10'}, October 2011
#' \item \code{W} weekly, notation \code{'2011w39'}, 39th week of 2011
#' \item \code{B} business-day, notation \code{'2011-10-05'}, 5 October 2011
#' \item \code{D} daily, notation \code{'2011-10-02'}, 2 October 2011
#' \item \code{N} minutely, notation \code{'2011-10-02t13:24'}, 2 October 2011, 13:24h (default GMT)
#' }
#'
#' @seealso \code{\link{timo}}, \code{\link{frequency.timo}}, \code{\link{seq.timo}}
#' @examples
#' Sys.timo()
#' Sys.timo(frq='M')
#' Sys.timo(frq='W') + 0:5 #current week plus the next four ones
#' Sys.timo() -"1Q" # this minute one quarter ago
#'
#' @export
Sys.timo = function(frq=NULL) {
  as.timo.POSIXct(.Internal(Sys.time()), frq=frq)
}

.addbizday = function(xtimoc, xnump) {

  xtimoc=.asnum(xtimoc); oldClass(xtimoc) = 'POSIXct'
  #tempwstart=unclass(xtimoc)-unclass(xtimoc) %% (1440*7*60) + 1440*4*60 # start of the week in unix time
  tempweek=(unclass(xtimoc)- 1440*3*60) %/% (1440*7*60) #number of week in epoch
  temp=tempweek + round((as.integer(format(xtimoc,'%u'))-1)/5 + xnump/5,1)
  #temp = ({(temp %% 1 * 5 +4) * (1440*60) + temp%/%1*(1440*7*60)} )
  temp = ({(temp %% 1 * 5 +4)  + temp%/%1*(7)} )
  #return(.timo_class(temp - temp %% 60 + .cttim$frqcodes['B','timosuffix']))
  return(.timo_num2class(temp*86400  + .cttim$frqcodes['B','timosuffix']))

}


.timo_addnumeric = function(xtimo,xnum, addit=TRUE) {

  addper = function(xtimop, xnump, xfrqp) {
    xplt=as.POSIXlt.POSIXct(xtimop, tz='UTC')
    xplt$mon = xplt$mon + sign(addit-.5)* .cttim$frqcodes[xfrqp,'multiple'] * xnump
    (as.POSIXct.POSIXlt(xplt))
  }

  if (length(xnum) > 1 & length(xnum) != length(xtimo)) {
    #explicit recycling
    if (length(xnum) < length(xtimo)) {xnum <- rep_len(xnum,length(xtimo))}
    if (length(xnum) > length(xtimo)) {xtimo <- rep_len(xtimo,length(xnum))}

  }

  ytimo=xtimo; xtimo=as.POSIXct(xtimo)
  vfrqs= .timo_frq(xtimo)
  ixf = vfrqs %in% .cttim$allbasedon('M')
  xnump=xnum; if (length(xnump)>1) { xnump=xnum[ixf]}
  if (any(ixf)) { ytimo[ixf] = addper(xtimo[ixf], xnump, vfrqs[ixf]) }
  if (all(ixf)) return(ytimo)

  xnumq=xnum; if (length(xnumq)>1) { xnumq=xnum[!ixf]}
  ytimo[!ixf] = (xtimo[!ixf]) + sign(addit-.5)* .cttim$frqcodes[vfrqs[!ixf],'multiple'] * 60* xnumq
  ixb = vfrqs %in% .cttim$allbasedon('B')
  if (!any(ixb)) return(ytimo)

  ytimo[ixb] = .addbizday(xtimo[ixb], switch(2-addit,1,-1)*xnum)
  return(ytimo)


}




#.timo_addnumeric(as.timo(Sys.Date(),'m'),6,T)
#.timo_addnumeric(as.timo(Sys.Date()),1:2,T)
#.timo_addnumeric(timo('2010-01-05'),0:15,T)
#.timo_addnumeric(timo('2010-01-05',frq='B'),0:15,F)






.timo_subset = function(x, ..., coverhigherfrqs=TRUE, addifmiss=FALSE, coverlowerfrqs=FALSE) {
  # tt=MD3:::.timo_seq('1999m06','2003m05')
  # MD3:::.timo_subset(tt,paste0('1999m0',7:9))
  # MD3:::.timo_subset(tt,23:27)
  # MD3:::.timo_subset(tt,'2001m02+2001m05')
  # MD3:::.timo_subset(tt,'2001m02:2001m05+2002m02')
  # MD3:::.timo_subset(tt,'2001q2:2001q4')
  # MD3:::.timo_subset(tt,'2001q2:y')
  # MD3:::.timo_subset(tt,'2004:y')
  # MD3:::.timo_subset(tt,'2014')
  # MD3:::.timo_subset(tt,'2002-01-02',cover=T)
  #  MD3:::.timo_subset(tt,'2002-01-02',cover=F)
  # MD3:::.timo_subset(tt,c('2002-01-02','2002-01-03'),coverhigherfrqs =F) #porblemm
  # MD3:::.timo_subset(tt,'2002-01-02',coverhigherfrqs =F) #porblemm

  ixl=list(...)


  if (!length(ixl) | (length(ixl)==1 & missing(..1))) { return(x)}


  if (length(ixl)!=1L) {
    #stop('sdafasdf')
    return(.timo_class(unlist(lapply(ixl,function(y) .timo_subset(x,y,coverhigherfrqs=coverhigherfrqs, coverlowerfrqs = coverlowerfrqs)))))
  }

  if (!length(coverlowerfrqs)) {coverlowerfrqs = FALSE}

  ix=ixl[[1]]

  if (.timo_is(ix)) {
    if (identical(ix,x)) { return(ix)}

    idix = bit64::match(ix,x)
    if (!anyNA(idix) & !coverhigherfrqs) {
      if (!is.character(coverlowerfrqs)) {
        if (!coverlowerfrqs) return(x[idix])
      } else {
        if (all(.timo_frq(ix) %in% coverlowerfrqs)) return(x[idix])
      }
    }


    ixl = as.list(ix)

  } else {

    if (!length(coverlowerfrqs)) {coverlowerfrqs = FALSE}
    if (!is.character(ix) ) return(x[unlist(ixl)])
    if (length(ix)!=1L) {
      #stop('sdafasdf')
      return(.timo_class(unlist(lapply(as.list(ix),function(y) .timo_subset(x,y,coverhigherfrqs=coverhigherfrqs, coverlowerfrqs = coverlowerfrqs)))))
    }

    ix=gsub("y","",ix)
    ixl=as.list(strsplit(ix,split="\\+")[[1]])
  }
    #xPOSIXct = character()

  for( i in 1:length(ixl)) {
    ixl[[i]] = as.character(ixl[[i]])


    bla=list()
    colonpresent=FALSE
    while (any(grepl(":",unclass(ixl[[i]])))) {
        colonpresent=TRUE
        j = head(grep(":",ixl[[i]]),1)
        bla = .char2timo(trimws(strsplit(ixl[[i]][j],split=":")[[1]]))
        if (length(bla)==1 & grepl(":$",ixl[[i]][j])) { bla=c(bla,tail(x,1)) }
        if (length(bla)!=2) { stop("Indexing error: something wrong with using : in referring to codes, in ",ixl[[i]][j])}
        if (is.na(bla[[1]])) { bla[[1]] = head(x,1)}; if (is.na(bla[[2]])) { bla[[2]] = tail(x,1)}
        ixl[[i]]=bla
        #if (!length(xPOSIXct)) { xPOSIXct = .mdt_asPOSIXct(x) }
    }
    if (!length(bla)) bla[[2]] = bla[[1]] = .char2timo(ixl[[i]])
    if (is.character(coverlowerfrqs)) {
      xmy=x[.timo_frq(x) %in% coverlowerfrqs]
    } else if (!coverlowerfrqs[[1L]]) {
      xmy=x[.timo_frq(x) == .timo_frq(bla[[1L]])]

    } else {
      xmy=x
    }
    if (!length(xmy)) { xmy =x}

    maxfrq=NULL; if (!coverhigherfrqs) maxfrq=toupper(names(.cttim$fcode))[match(.timo_frq(c(bla[[1]],bla[[2]])),toupper(names(.cttim$fcode)))]
    ixsmaller = 1L + bit64:::`>.integer64`(.asint64(bla[[1]]),.asint64(bla[[2]]))

    xout=try(xmy[.timo_within(.timo_class(bla[[3-ixsmaller]]), TRUE, FALSE)>=xmy & xmy >=.timo_within(.timo_class(bla[[ixsmaller]]), FALSE, FALSE)],silent=TRUE)

    if (!length(xout)) if (all(.timo_frq(.unlist_keepclass(bla)) %in% .timo_frq(x) )) {
      xout=.timo_subperiods(bla[[1L]],.timo_frq(bla[[1L]]),bla[[2L]])
    }
    if (length(maxfrq) && !coverhigherfrqs ) {
      #xout=xout[.timo_frq(xout) %in% maxfrq]
      xout=xout[match(.timo_frq(xout),.cttim$frqcodes[['fcode']])>=max(match(maxfrq,.cttim$frqcodes[['fcode']]))]
    }

    #xout=try(x[xPOSIXct>=.mdt_asPOSIXct(bla[[1]]) & xPOSIXct <= .mdt_asPOSIXct(bla[[2]], last=TRUE)],silent=TRUE)
    if (any(grepl("error",class(xout)))) { stop("Indexing error: could not determine range for ", ixl[[i]][j])}
    #browser()

    if (addifmiss & colonpresent) {
      tempadd=seq.timo(bla[1],bla[2]);
        if (all(unique(.timo_frq(tempadd)) %in% .timo_frq(xmy))) {
          xout=sort(unique(c(tempadd,xout)))
        }
    }

    if (ixsmaller==2) xout=rev(xout)
    ixl[[i]] = xout
  }
  return(.timo_class(unlist(ixl)))

}





.timo_seq = function (from, to, frq = NULL, ...) {


  s_monthbased=function(from,to, xfrq) {
    #mfrq = month multiple
    mfrq=.cttim$frqcodes[xfrq,'multiple']
    lfrom = as.POSIXlt.timo(from, tz = 'UTC')
    lto = as.POSIXlt.timo(to, tz = 'UTC')
    sl=(lto$year-lfrom$year)*12 + (lto$mon-lfrom$mon)
    #sl = sl %/% mfrq + sign(sl %% mfrq)
    #mn=lfrom$mon + 1 + sign(sl)*seq.int(0,abs(sl),mfrq)
    #mn = lfrom$mon + sign(sl)*c(0,seq_len(abs(sl) %/% mfrq + sign(abs(sl) %% mfrq)))*mfrq
    mn = lfrom$mon + sign(sl)*c(0,seq_len(abs(sl) %/% mfrq))*mfrq
    as.POSIXct.POSIXlt(list(sec=.cttim$fcode[tolower(xfrq)],min=0,hour=0,mday=1,mon=mn,year=lfrom$year,wday=NA_integer_,yday=NA_integer_,isdst=0,zone='UTC',gmtoff=NA), tz='UTC')

  }


  from= as.timo(head(from,1))

  if (missing(to)) to = from else to=as.timo(head(to,1));
  if (.timo_frq(from) =='B' & .timo_frq(to) =='D') { to=.timo_cfrq(to,frq='B', referstoend=TRUE)}

  #vfrq= .cttim$frqmatch[c(lfrom$sec, lto$sec) %% 30]
  vfrq= .timo_frq(c(from, to))

  if (!is.null(frq)) {

    if (length(frq)>1) stop('frq has to be singleton!')

    xfrq = .cttim$frqcodes[pmatch(toupper(trimws(frq)), .cttim$frqcodes[,'fcode']),'fcode']
    if (is.na(xfrq)) stop('Did not understand frequency code "',frq,'"')
    xfrq=trimws(toupper(xfrq))

  } else {
    if (vfrq[1]!=vfrq[2]) {xfrq = vfrq[which.max(match(vfrq,rownames(.cttim$frqcodes)))] } else {xfrq=vfrq[1]}
  }

  if (xfrq %in% .cttim$allbasedon('B')) {
    if (xfrq!='B') stop('Multiples of biz days not yet implemented')
    temp=.timo_seq(from,to,frq='D')
    return(temp[format(temp,'%u')<'6'])
  }



  temp=match(c(xfrq,vfrq),rownames(.cttim$frqcodes))
  if (any(temp[2:3]<temp[1])) {
    if (.asint64(from) > .asint64(to)) {
      #lfrom = as.POSIXlt.POSIXct(.timo_within(from,TRUE), tz = 'UTC'); lto = as.POSIXlt.POSIXct(.timo_within(to,FALSE), tz = 'UTC')
      from = .timo_within(from,TRUE); to = .timo_within(to,FALSE)
    } else {
      from = .timo_within(from,FALSE); to = .timo_within(to,TRUE)
    }
  }






  if (.cttim$frqcodes[xfrq,'baseunit']=='M') { return(.timo_num2class(s_monthbased(from,to,xfrq)))}
  from=.asint64(from); to=.asint64(to);
  from=(from) - (from) %% 60  + .cttim$fcode[tolower(xfrq)]
  to=(to)- (to) %% 60  + .cttim$fcode[tolower(xfrq)]
  mfrq=.cttim$frqcodes[xfrq,'multiple']
  .timo_class(bit64::seq.integer64(from, to, ifelse(from<to,1,-1)*mfrq*60))


}


#' Getting, setting or converting the frequency of a timo period object
#'
#' @param x a \code{\link{timo}}  period vector
#' @param frq a frequency character like '\code{A}', 'M' or 'D' to convert x to
#' @param refersto an optional character \code{'start'}, \code{'middle'}, or \code{'end'}. If frq is higher frequency than the original x, then this chooses the first a last eligible subperiod.
#' @return a timo obejct if frq is specifed. A character vector with frequency codes (like 'D','B','A') otherwise
#' @details
#'
#' The following denotes the existing frequency codes and their notation
#' \itemize{
#' \item \code{A} annual, notation \code{'2011'}
#' \item \code{S} semi-annual, notation \code{'2011s2'}, second half of 2011
#' \item \code{Q} quarterly, notation \code{'2011q3'}, third quarter of 2011
#' \item \code{M} monthly, notation \code{'2011m10'}, October 2011
#' \item \code{W} weekly, notation \code{'2011w39'}, 39th week of 2011
#' \item \code{D} daily, notation \code{'2011-10-02'}, 2 October 2011
#' \item \code{N} minutely, notation \code{'2011-10-02t13:24'}, 2 October 2011, 13:24h (default GMT)
#' }
#'
#' @seealso \code{\link{timo}}, \code{\link{seq.timo}}, \code{\link{Sys.timo}}
#' @examples
#' frequency(timo(c('2011q1','2023m11','2014-12-09')))
#' aa <- timo(y2022q3,y2022q4)
#' frequency(aa)
#' frequency(aa) <- 'm'
#' aa
#' frequency(aa,'D')
#' frequency(aa,frq='D',refersto='end')
#' frequency(seq(timo(2013),2014,by='Q'),frq='D',refersto=c('start','end'))
#' @export
frequency.timo = function(x, frq=NULL,refersto='start',...) {
  if (is.null(frq)) return(.timo_frq(x))
  if (length(refersto)==1) {refersto2=c(TRUE,FALSE)[pmatch(tolower(trimws(refersto[[1]])),c('end','start','middle'))]} else { refersto2 = (tolower(refersto)=='end')}
  .timo_cfrq(x,frq=trimws(frq), referstoend=refersto2)
}

#' @export
frequency.timdif = function(x, frq=NULL, ...) {
  if (!length(frq)) return(attributes(x)[['units']])
  if (length(frq)>1) stop('only one frequency code can be specified. You specfified ',length(frq), 'different ones')
  bonew=.cttim$basedon(frq)
  boold=.cttim$basedon(attributes(x)[['units']])
  if (boold==bonew) {
    myf=attr(boold,'multiple')/attr(bonew,'multiple')
    x=x*myf; attr(x,'units') = frq
    return(x)
  }
  stop('cannot convert time difference of ', .cttim$frqcodes[toupper(attributes(x)[['units']]),'desc'], ' frequency to ', .cttim$frqcodes[toupper(frq),'desc'], ' without context')
}

#' @rdname frequency.timo
#' @export
`frequency<-`= function (x,frq=NULL,...) UseMethod("frequency<-")

#' @rdname frequency.timo
#' @export
`frequency<-.timo` = function(x, value, refersto=c('start','end')) {
  if (length(refersto)==1) {refersto2=c(TRUE,FALSE)[pmatch(tolower(trimws(refersto[[1]])),c('end','start','middle'))]} else { refersto2 = (tolower(refersto)=='end')}
  .timo_cfrq(x,value,referstoend = refersto2)
}

#' @export
`frequency<-.default` = function(x, value) {
  NextMethod()
}


#' @export
`frequency<-.timdif` = function(x, value) {
  frequency.timdif(x,frq=value)
}



.timo_subperiods = function(from, frq=NULL, to=from, incloverlapping=TRUE) {
  from2=.timo_cfrq(from,frq,referstoend = FALSE)
  to2=.timo_cfrq(to,frq,referstoend = TRUE)
  tout=.timo_seq(from2,to2,frq)
  if (incloverlapping) return(tout)
  tout=tout[.timo_within(tout,referstoend = TRUE) <= to2]
  if (length(tout)) tout=tout[.timo_within(tout,referstoend = FALSE) >= from2]
  tout
}


#' @export
c.timo = function(..., recursive=FALSE) {
  if (!all(unlist(lapply(list(...), .timo_is)))) {
    return(.timo_class(bit64:::c.integer64(.timo_class(unlist(lapply(list(...),as.timo))))))
  }
  .timo_class(bit64:::c.integer64(...,recursive=recursive))

}

#' @export
range.timo = function(..., na.rm = FALSE, finite = FALSE) {
  .timo_class(bit64:::range.integer64(..., na.rm = na.rm, finite = finite))
}


.timo_seqlen = function(from, length.out,  by=NULL) {
  if (is.null(by)) by=1 else if (length(by)>1) stop('by must be of length 1')
  if (is.numeric(by)) return(.timo_addnumeric(from,seq.int(0,length.out=length.out,by=by)))
  return(.timo_addtd(.timo_cfrq(from,by),paste0(seq.int(0,length.out=length.out),by)))
}

#' Sequencing
#'
#' @param from a starting \code{\link{timo}}  period. Required.
#' @param to end period (\code{\link{timo}} or any character, Date etc. that can be converted to a timo.  Optional.
#' @param by increment of the sequence. Integer. Default = 1
#' @param frq instead of by, use here the frequency code (like 'Q' or 'D') to define increments
#' @param length.out integer, optional. Desired length of the sequence.
#' @details
#'
#' The following denotes the existing frequency codes and their notation
#' \itemize{
#' \item \code{A} annual, notation \code{'2011'}
#' \item \code{S} semi-annual, notation \code{'2011s2'}, second half of 2011
#' \item \code{Q} quarterly, notation \code{'2011q3'}, third quarter of 2011
#' \item \code{M} monthly, notation \code{'2011m10'}, October 2011
#' \item \code{W} weekly, notation \code{'2011w39'}, 39th week of 2011
#' \item \code{D} daily, notation \code{'2011-10-02'}, 2 October 2011
#' \item \code{N} minutely, notation \code{'2011-10-02t13:24'}, 2 October 2011, 13:24h (default GMT)
#' }
#'
#' @return a timo vector
#' @seealso \code{\link{timo}}, \code{\link{frequency.timo}}, \code{\link{Sys.timo}}
#' @examples
#' seq(timo('2014m03'),'2015m06')
#' seq(timo('2014m03'),'2015')
#' seq(timo('2014m03'),'2015m06',by=3)
#' seq(timo('2014q1'),'2014q2',frq='W')
#' seq(timo('2014m03'),frq='B')
#' @export
seq.timo = function(from, to, by=NULL, frq=NULL,length.out=NULL,...) {
  if (is.null(frq) & all(is.character(by))) {frq=by; by=NULL}
  if (!is.null(length.out)) {
    if (!missing(to)) stop('can specify to and length.out at once');
    if (is.null(frq)) myby = by else myby= frq[[1]]
    return(.timo_seqlen(from,length.out,by=myby))
  }

  if (missing(to)) { to=from}

  temp=.timo_seq(as.timo(from), as.timo(to), frq=frq)

  if (is.null(by)) {return(temp)}
  return(temp[seq(1,length(temp),by=by)])
}
#`[.timo` = function(x,... ) {
#  .timo_subset(x,...)
#}

#' @export
as.POSIXct.integer64 = function (x, tz = "", origin='1970-01-01', ...) {
  as.POSIXct.numeric(.asnum(x),tz=tz,origin=origin)
}
#
#
# lm64=methods(class='integer64')
# for (i in lm64) {
#   if (!exists(paste0(i,'.timo'))) {
#     assign
#   }
# }


#' @export
quarters.timo = function(x,...) {
  paste0("Q", (as.POSIXlt.timo(x)$mon)%/%3 + 1)
}


#' @export
months.timo = function(x, abbreviate=FALSE, ...) {
  format.POSIXct(as.POSIXct.timo(x), ifelse(abbreviate, "%b", "%B"))
}

#' @export
weekdays.timo = function(x, abbreviate=FALSE, ...) {
  format.Date(as.Date.timo(x), ifelse(abbreviate, "%u", "%A"))
}





######### timdif ########
.getunits = function(units) {
  u2 = match(toupper(trimws(units)),rownames(.cttim$frqcodes),nomatch = 0)
  if (any(u2<1)) {

    u3 = unlist(lapply(as.list(toupper(trimws(units[u2<1]))),function(x) pmatch(x,toupper(paste0(.cttim$frqcodes[['periodname']],'s')),nomatch = 0,duplicates.ok=TRUE)));
    if (any(u3<1)) stop('could not identify ', units)
    u2[u2<1] = u3
    #warning(units)
  }

  rownames(.cttim$frqcodes)[u2]

}
.timdif_class = function(x) {
  oldClass(x) = c('timdif', 'timord','difftime')
  x
}
.timdif = function(x) {
  #if (bit64:::is.integer64(x)) x= .asnum(x)

  oldClass(x) = c('timdif', 'timord','difftime')
  x
}

#something like difftime, but working for lower frq too
.char2timdif = function(x,units=NULL) {
  x=gsub('\\s','',x)

  if (any(!(grepl('^[0-9.]*[A-z]$',x) | grepl('^[0-9.]$',x)))) {
    if (any(grepl('^[\\+-]',x) | grepl('^[0-9.]$',x))) {
      x=gsub('^\\+','',x)
      presign=rep(1,length(x))
      presign[grepl('^-',x)]=-1
      return(.timdif(presign*.char2timdif(gsub('^-','',x))))
    }

  }
  if (any(grepl('^[0-9.]$',x)) & is.null(units)) stop('')
  u0 = toupper(gsub('[^A-z]','',x))
  if (!is.null(units)) {
    units=.getunits(units)
    u1=u0; u1[] = units; u0[!nchar(u0)] =u1[!nchar(u0)]
  }

  tout=as.numeric(gsub('[A-z]','',x))
  attr(tout,'units') = .getunits(u0)

  .timdif_class(tout)
}

#aa=(.char2timdif(c('2 w',4,7,'3a'),c('S','quarters'))); unclass(aa)


#' @export
`units<-.timdif` = function(x,value) {
  newx=unclass(x)
  attr(newx,'units') = .getunits(value)
  .timdif_class(newx)
}


.num2timdif = function(x,units=NULL) {
  tout=as.numeric(x)
  attr(tout,'units') = .getunits(units)
  .timdif_class(tout)
}

#' @export
print.timdif = function(x,digits=NULL,...) {
  if (is.null(digits)) { digits=2}
  cat('[',length(x),'] ',paste0(round(unclass(x),digits),attr(x,'units'),collapse=' '), '\n',sep= '')
}

#' @export
as.timdif= function (x,units=NULL,...) UseMethod("as.timdif")

#' @export
as.timdif.default = function(x, units=NULL,...) {
  .char2timdif(x,units=units)
}


#' @export
as.timdif.numeric= function(x, units=NULL,...) {
 .num2timdif(x,units)
}

#' @export
as.timdif.difftime= function(x, units=NULL,...) {
  newx=unclass(x)
  ixs=which(attr(newx,'units')=='secs')
  if (length(ixs)>0) {
     attr(newx,'units')[ixs] = 'minutes'
     newx[ixs] = newx[ixs]/60
  }

  attr(newx,'units') = .getunits(attr(newx,'units'))
  .timdif_class(newx)
}


.timo_addtd = function(xtimo,xtd, addit=TRUE) {
   xtd=as.timdif(xtd)
   fff=.cttim$basedon(units(xtd))
   ytd=unclass(xtd)*unclass(attr(fff,'multiple'))
   attributes(fff) = NULL
   attr(ytd,'units') = unclass(fff)

   #ytimo=.timo_cfrq(xtimo,attr(ytd,'units')) #.cttim$basedon(.timo_frq(xtimo)))
   #ytimo=.timo_addnumeric(ytimo,ytd)
   #if (any(attr(ytd,'units') !=.cttim$basedon(.timo_frq(xtimo)))) warning('')
   #.timo_cfrq(ytimo,.timo_frq(xtimo))
   if (length(xtimo)<length(xtd)) {templ=.recycle(xtimo,xtd,force=TRUE); xtimo=templ[[1]]; xtd=templ[[2]]; rm(templ)}

   #if (any(fff!='M')) {xtimo[fff!='M']=.timo_within(xtimo[fff!='M'],referstoend = addit)}
   #if (any(fff=='M')) {xtimo[fff=='M']=.timo_within(xtimo[fff=='M'],referstoend = FALSE)}
   xtimo=.timo_within(xtimo,referstoend = addit)
   ltimo = as.POSIXlt.timo(xtimo)


   if (any('N' %in% fff) ) {ltimo$min[fff=='N']=.Primitive(switch(2-addit,"+","-"))(ltimo$min[fff=='N'], ytd[fff=='N'])   }
   ytimo=as.timo.POSIXct(as.POSIXct.POSIXlt(ltimo));
   if (any('N' %in% fff) ) {ytimo[fff=='N'] =  .timo_cfrq(ytimo[fff=='N'],.timo_frq(xtimo[fff=='N'])) }
   if (any('M' %in% fff) ) {
     if (addit) {ltimo$mday[fff=='M']=ltimo$mday[fff=='M']-4}
     #temp=ltimo$year[fff=='M']*12+ltimo$mon[fff=='M'] + sign(addit-0.5)*ytd[fff=='M']
     #ytimo[fff=='M'] =  .timo_cfrq(.char2timo(paste0(temp%/% 12 + 1900, 'm', temp%%12 +1)),.timo_frq(xtimo[fff=='M']))
     ltimo$mon[fff=='M'] = ltimo$mon[fff=='M']  + sign(addit-0.5)*ytd[fff=='M']
     ytimo[fff=='M'] = .timo_num2class(as.POSIXct.POSIXlt(ltimo[fff=='M']))
     if (addit) {ytimo[fff=='M'] = .timo_within(ytimo[fff=='M'],referstoend = TRUE) }
   }
   if (any('B' %in% fff) ) {
     ytimo[fff=='B']=.addbizday(ytimo[fff=='B'],switch(2-addit,1,-1)*ytd[fff=='B'])

     ytimo[fff=='B'] = .timo_cfrq(ytimo[fff=='B'],.timo_frq(xtimo[fff=='B']))
   }
   ytimo
}



.monthnum=function(l) {
  #number of the month since year 0 (sic)
  l=.timo_class(l)
  vout=findInterval(.asint64(l),.cttim$monthstartposixfrom1900())
  if (all(vout>0 & vout < 201)) return(vout+1899)


  m=as.Date.timo(l); return(as.numeric(base::format.Date(m,'%Y'))*12 + as.numeric(base::format.Date(m,'%m')))
}
.timo_calctd = function(te1,te2, units=NULL) {
  te1=as.timo(te1) ; te2=as.timo(te2)
  if (is.null(units)) {
    tu0 =  toupper(names(which.max(.cttim$fcode[tolower(.cttim$frqmatch[.asnum(abs(c(te1,te2)) %% 60)])]))) # pick the higher frequency of the two
  } else {
    tu0 = .getunits(units)
  }
  tub=.cttim$basedon(tu0)
  te1islarger = unclass(te1 > te2)
  if (!(length(te1) & length(te2))) {return(.timo_num2class(numeric()))}
  be1 = .timo_cfrq(te1,tub,referstoend = !te1islarger)
  be2 = .timo_cfrq(te2,tub,referstoend = !te1islarger)

  if (tub=='N') {
    yout=.asint64(be2) - .asint64(be1) #+ switch(2-te1islarger,-2,2)*.cttim$fcode[tolower(tub)]
    return(.num2timdif(.asnum(yout/(60*attr(tub,'multiple'))),tu0))

  }
  if (tub=='B') {
    return(.num2timdif(length(.timo_subperiods(be1,'B',be2))/attr(tub,'multiple'),tu0))
  }
  #so tub=='M
  #tec=as.POSIXct(c(te1,te2))
  #return(.num2timdif(diff(as.numeric(format.POSIXct(tec,'%Y'))*12 + as.numeric(format.POSIXct(tec,'%m')))/attr(tub,'multiple'),tu0))


  return(.num2timdif((.monthnum(be2)-.monthnum(be1))/attr(tub,'multiple'),tu0))

}



.timord_plusminus = function(e1,e2, addit=TRUE) {

  fixchar = function(ee) {
   if (!inherits(ee,c('timord'))) {
     if (is.character(ee)) {
       ee=gsub('\\s' ,'',ee)
       if (all(grepl('^[0-9.\\+-][0-9.]*[A-z]$',ee))) { return( .char2timdif(ee))}
       return( .char2timo(ee))
     }
    }
   return(ee)
  }


  e1=fixchar(e1); e2=fixchar(e2)
  if (inherits(e1,c('timdif','timo')) & inherits(e2,c('timdif','timo'))) {
    if(.timo_is(e1) & .timo_is(e2) & !addit) { return(.timo_calctd(e2,e1))}
    if(.timo_is(e1) & .timo_is(e2) & addit) { return(.timo_class(.asint64(e1)+.asint64(e2)))}
    if(.timo_is(e1) & !.timo_is(e2)) { return(.timo_addtd(e1,e2,addit = addit))}
    if(!.timo_is(e1) & .timo_is(e2)) { return(.timo_addtd(e2,e1))}
    if(!.timo_is(e1) & !.timo_is(e2)) { return(e1 + sign(addit-.5)*e2)}
  }

    if (all(is.numeric(e1) | is.logical(e1))) {
      if (all(.timo_is(e2))) return(.timo_addnumeric(e2,e1,addit))
      if (all(inherits(e2,'timdif'))) {return(e1+sign(addit-.5)*e2)}
      stop('problema')
    }



    if (all(is.numeric(e2) | is.logical(e2))) {
      if (all(.timo_is(e1))) return(.timo_addnumeric(e1,e2,addit))
      if (all(inherits(e1,'timdif'))) {return(.timo_addtd(as.timo(e2),e1,addit))}
      stop('problema')
    }


  stop('df')


}

#' @export
c.timdif = function(...) {

  tout=(unlist(list(...)))
  attr(tout,'units') = sapply(list(...),attr,'units')
  oldClass(tout) = c('timdif')
  tout
}

.as_timord = function(x) {
  if (.timo_is(x)) return(x)
  if (.timdif_is(x)) return(x)
  y=suppressWarnings(try(as.timo(x), silent=TRUE))
  if (!(any(grepl('error',class(y))))) { return(y)}
  y=suppressWarnings(try(as.timdif(x), silent=TRUE))
  if (!(any(grepl('error',class(y))))) { return(y)}
  stop('cannot convert stuff like ', head(x,1L), '  to timo or timdif.')
}


#' @export
`==.timord` = function(e1,e2) {

  if (!.timo_is(e1) & !.timdif_is(e1)) { e1 <- .as_timord(e1) }
  if (!.timo_is(e2) & !.timdif_is(e2)) { e2 <- .as_timord(e2) }
  return(.asint64(e1)==.asint64(e2))
}


#' @export
`%in%.timord` = function(e1,e2) {

  if (!.timo_is(e1) & !.timdif_is(e1)) { e1 <- .as_timord(e1) }
  if (!.timo_is(e2) & !.timdif_is(e2)) { e2 <- .as_timord(e2) }
  return(.asint64(e1) %in% .asint64(e2))
}

#
#' @export
`+.timord` = function(e1,e2) {.timord_plusminus(e1,e2,TRUE)}

#' @export
`-.timord` = function(e1,e2) {.timord_plusminus(e1,e2,FALSE)}


#Summary.timo = function(...,na.rm=FALSE) {
#  .timo_class(
#    getS3method(.Generic,'integer64')(...,na.rm = na.rm))
#}


#' @export
min.timo = function(...,na.rm=FALSE) {
  .timo_class(bit64:::min.integer64(...,na.rm = na.rm))
}


#' @export
max.timo = function(...,na.rm=FALSE) {
  .timo_class(bit64:::max.integer64(...,na.rm = na.rm))
}


#' @export
range.timo = function(...,na.rm=FALSE) {
  .timo_class(bit64:::range.integer64(...,na.rm = na.rm))
}




# as.timo.timdif = function(x) {
#   stop(1)
# }


#' @export
xtfrm.timord = function(x) .asnum(x)


#' @export
diff.timo = function(x, lag=1, na.pad=FALSE, ...) {
  nl=length(x)
  if (lag<0) stop('lag has to be positive')
  xout=.timord_plusminus(tail(x,nl-lag),head(x,nl-lag),addit=FALSE)
  if (!na.pad) return(xout)
  xout=c(rep(NA_real_,lag),xout)
  xout
}


#PROBLEMS:
#.timo_cfrq(seq(timo('2014w12'),'2014w52'),'D',c('end','start'))
#.timo_cfrq(seq(timo('2014w12'),'2014w51'),'D',c(F,T)) #fixed
#class(seq(timo('2014w12'),'2014w52')-timo(2013)) #fixed


.coerce2timo=function(from,to,strict=TRUE) as.timo(from,frq=NULL)
#setAs('numeric','timo',.coerce2timo)
#setAs('character','timo',.coerce2timo)
#for (j in setdiff(gsub('as\\.timo\\.','',as.character(methods('as.timo'))),'timo')) setAs(j,to='timo',def = .coerce2timo)

.iso2timdif = function(x) {
  if (!any(grepl('^p',tolower(x)))) stop('ISO periods need to be prefixed by "P"')
  jj=strsplit(gsub('^P','',x),split='[0-9.,]+')[[1]]
  kk=character()
  for (i in jj[jj!='']) {
    kk[i]=paste0(gsub('^.*[A-z]','',gsub(paste0(i,'.*$'),'',x)),i)
  }
  ll=lapply(kk,.char2timdif)
  tbu=.cttim$basedon(unlist(lapply(ll,units)))
  if (length(unique(tbu))>1) {stop(1)}
  temp=sum(attr(tbu,'multiple')*unlist(ll))
  fout=sum(temp)
  attr(fout,'units') =unique(tbu)
  fout
}


#' @export
as.list.timord = function(x, ...) {

    y=.Internal(as.vector(.asint64(x), "list"))
    if (.timo_is(x)) {
    y=lapply(y,.timo_class)
    } else {
      y=lapply(y,.timdif_class)
    }
    return(y)

}

#' @export
as.vector.timord = function(x,mode='any') {
  if (mode=='any') { return(x)}
  if (mode=='list') { return(as.list.timord(x))}
  if (mode=='character') { return(.Internal(as.vector(as.character(x),"character")))}
  if (mode=='raw') { return(.Internal(as.vector(x, "raw")))}
  return(as.vector(as.character(x),mode))
}


#' @export
`>.timo` = function(e1,e2) {
  as.logical(.asint64(.timo_within(e1,TRUE))>.asint64(.timo_within(e2,TRUE)))
}

#' @export
`>=.timo` = function(e1,e2) {
  as.logical(.asint64(.timo_within(e1,TRUE))>=.asint64(.timo_within(e2,FALSE)))
}


#' @export
`<.timo` = function(e1,e2) {
  as.logical(.asint64(.timo_within(e1,FALSE))<.asint64(.timo_within(e2,FALSE)))
}

#' @export
`<=.timo` = function(e1,e2) {
  as.logical(.asint64(.timo_within(e1,FALSE))<=.asint64(.timo_within(e2,TRUE)))
}

#' @export
unique.timo = function (x, incomparables = FALSE, distinctstartend=FALSE,... ) {
  if (distinctstartend) return(.timo_class(bit64:::unique.integer64(x, incomparables=incomparables, ...)))
  return(.timo_class( bit64:::unique.integer64(.timo_within(x,referstoend=FALSE), incomparables=incomparables, ...)))
}


#' @export
year.timo = function(x) {
  x=.timo_class(x)
  vout=findInterval(.asint64(x),.cttim$yearstartposixfrom1800())
  if (all(vout>0 & vout < 401)) return(vout+1799)
  as.numeric(base::format.Date(as.Date.timo(x),'%Y'))
  #unlist(lapply(as.list( x) ,  function(z) sum(.asint64(z)>.cttim$yearstartposixfrom1800())))+1800
}


#' @export
start.timo = function(x,...) {utils::head(x,1)}

#' @export
end.timo = function(x,...) {utils::tail(x,1)}

#too slow:
#uu=.timo_within(seq(timo(y2014w14), 2015,frq='n'),TRUE) $ fixed






#fine
#
# Rprof(tmp <- tempfile())
# kk=as.timo(rep(Sys.time(),1000000))
# kk=kk+'1M'
# Rprof()
# summaryRprof(tmp)
# unlink(tmp)



#probelm
#frequency(sort(timo(y2119m1)+5:1),'b',refersto = 'end')

# ### EXAMPLES ###
# .timo_seq('2011m04','2011m05')
# .timo_seq('2011','2012','m')
# #.timo_seq('2011',,'m')
# #debug(.timo_seq)
#
# .timo_seq('2011-04-01','2011-05-11')
#
#
# aa=sort(timo(paste0('2012m',1:12),paste0('2011m',1:12)))
# .timo_subset(aa,'2011m06:2011m10+2012')
# .timo_subset(aa,'2011m06:+2012')
# .timo_subset(aa,'2011q1+2012m12:') # PROBLEM!!! fixed
#
# .timo_subset(.timo_seq('2011',,'m'),c('2011q1','2011q4'))
#
#
#
# #as.POSIXct.POSIXlt(list(sec=0,min=0,hour=0,mday=1,mon=1:34,year=119,yday=1,isdst=0,wday=NA, tzone='UTC'))
#
#
# #
# # .timo_within(timo(1960:2021),TRUE)
# # .timo_cfrq(timo(1998:2002), 'D', TRUE)
# #
# #
# #
# #debugonce(.char2timo)
# .timo2char(.char2timo(c(paste0(2001:2020,'m03'),paste0(2001:2020,'q1'))))
#
# .timo2char(.char2timo(c('2011m14','2019q5')))
#
# format(.timo_within(.timo_cfrq(as.timo(Sys.Date()),'M')),'%F',tz='GMT')
# format(.timo_within(.timo_cfrq(as.timo(Sys.Date()),'Q'), F),'%F %H:%M:%S',tz='GMT')
# format(.timo_within(.timo_cfrq(as.timo('2021'),'Q',T),F),'%F',tz='GMT')
# .timo_cfrq(.timo_cfrq(timo('2011m04'),'A'),'W',T)
#
# #problem: format(timo('2010-02',frq='Q'),'%F')
# timo(paste0('2012w',1:15))
#
# xx=sort(timo(2011:2014,'2013-01-01t15:23',paste0('2013m',1:5),paste0('2013w',1:15))); names(xx) = format(xx,'%F'); xx; rm(xx)
# #timo(paste0(2010:2020,'w1'))
# format(as.POSIXct(timo(paste0(2010:2020,'w1'))),'%F',tz='GMT')
#


### subsetting ####
#aa=sort(timo(paste0('2012m',1:12),paste0('2011m',1:12)))
# #PROBLEM:
# .timo_addnumeric(timo('2010-01-05',frq='B'),0:15,T)
# .timo_addnumeric(as.timo(timo('2010-01-05'),frq='B'),0:15,T)
#

#.timo_addtd(timo('2013m03'),'1M',T)

#.timord_plusminus('2013m3','2012m6',T)/2
# .timord_plusminus('2013m3','2012m6',F)
# .timord_plusminus('2013m3','1M',T)
# .timord_plusminus('2013m3','1M',F)
# .timord_plusminus('2013m3',12,T)
# .timord_plusminus('2013m3',12,F)
# .timord_plusminus(14,'2013m3',F)
# .timord_plusminus(14,'2M',F)


#.timo_addtd(as.timo(c('2019-08-23','2012-04-09','2024m03')),c('2M','3w','5a'))
#.timo_addtd(as.timo(c('2019-08-23','2012-04-09','2024m03','2013')),c('2M','3D'),T)
#.timo_addtd(as.timo(c('2019-08-23','2012-04-09','2024m03')),c('2M','3w','1m'),T)
#aa=.timo_addtd(1,c('2M','3w','5a'))
