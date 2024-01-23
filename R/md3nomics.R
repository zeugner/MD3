#' Load data from db nomics world
#'
#' @param query a character query string with a RestFul SDMX query (such as ECB/EXR/A.GBP+CHF.EUR.SP00.A). See Details
#' @param as how to as the result: md0: as md0 object with full metadata, 2d: as a data.frame with periods as column names, 1d: as fully stacked data.frame, \code{array} as multi-dim array,  \code{zooreg} as \code{zoo}-type time series,  \code{pdata.frame} as panel data frame from the \code{plm} package, with time and countries along rows, and all other indicators along columns
#' @param drop if TRUE, drop any singleton dimensions (see also \code{drop.md0})
#' @param flags omit metadata, currently not operational
#' @param ccode If this is not \code{NULL}, the the function will attempt to indetfy any coountrycode and try to convert them to the desired format (e.g. \code{iso3c} or \code{ec})
#' @param silent do not post messages about loading progress
#' @param pattern pattern to match against the dataflow id or description. This uses in-built REgex expresions, but remains case-insensitive. If a pattern is not provided, all dataflows (or codes with labels) are returned .
#' @param dim dimension name or number (integer) denoting the dimension for which all codes and labels should be listed
#' @param ... etc.
#' @return an md0, data.frame, or array, as specfied by argument 'as'.
#' Note that if used currectly, helpNomics invisibly returns either an md0 object, or lists with definitions
#' @details A restful SDMX query combines Provider, Dataflow and Dimension selectors
#' Take the example 'ECB/EXR/A.GBP+CHF.EUR.SP00.A'
#' \itemize{
#'  \item 'ECB' designates the provider ECB (see \code{helpNomics()} for a list of providers)
#'  \item 'EXR' designates the ECB's dataflow 'EXR' about exchange rates (see \code{helpNomics('ECB')} for a list of ECB dataflows)
#'  \item A.GBP+CHF.EUR.SP00.A is the dimension selector: See \code{helpNomics('ECB/EXR')} for what dimensions exist in ECB/EXR)
#'  }
#'  Here,
#'  \itemize{
#'  \item 'A'  denotes annual frequency from the FREQ dimension
#'  \item 'GBP+CHF' denote British pound and Swiss frank from the currency dimension
#'  \item 'EUR' denotes the currency denominator
#'  \item 'SP00' denots spot rates
#'  \item 'A' denotes average over periods (here annual averages)
#'  }
#' Omit dimension codes to download all elements of a dimension, i.e.
#' ECB/EXR/A..EUR.SP00.A to download exchange rates of the euro vis-a-vis all currencies
#'
#' Note that a query is normally taken as Provider/Dataflow/Selectors, but Provider.Dataflow.Selectors is also tolerated.
#'
#' Note also that db.nomics queries are case-sensitive
#' @section Finding query codes:
#'
#' To find query codes, the easiest is to go to \url{http://db.nomics.world} and search for codes there
#' Alternatively, you can use \code{\link{helpNomics}}
#'
#' \itemize{
#'  \item \code{helpNomics()} returns a vector with available providers
#'  \item \code{helpNomics("PROVIDER")} returns a vector with the dataflows available for the provider
#'  \item \code{helpNomics("PROVIDER", pattern="PATTERN")} looks for a dataflow whose name or code matches the string pattern
#'  \item \code{helpNomics("PROVIDER/DATAFLOW")} prints the structure of the dataflow dimensions, as well as an example query, and invisible returns the structure of code names within the dataflow
#'  \item \code{helpNomics("PROVIDER/DATAFLOW/SELECTORS")} invisibly returns the query result. If not successful, it provides clues whwere the query might be wrong
#'  \item \code{helpNomics("PROVIDER/DATAFLOW", dim="NumberOrName")} prints the code names and labels of the selected dimension, and invidibly returns a data.frame with those codes and labels
#'  \item \code{helpNomics("PROVIDER/DATAFLOW", dim="NumberOrName", pattern="PATTERN")} searches the codes and labels of that dimension for a certain pattern
#' }
#'
#' @examples
#' w1=Nomics('ECB/EXR/A..EUR.SP00.E')
#' print(w1[.y2018])
#'
#' w2=Nomics('ECB/EXR/A..EUR.SP00.')
#' w2[ARS..y2012:y] #show Argentina since 2012
#'
#' w3=Nomics('ECB/EXR/A.USD+JPY+CNY.EUR.SP00.E',as="2d")
#' w3
#'
#' w4=Nomics('ECB/EXR/A.PLN.EUR.SP00.E',as="1d")
#' w4
#'
#' helpNomics('ECB/EXR')
#' helpNomics('ECB/EXR/A.PLN.EUR.XXX.E')
#'
#' #show available codes for fifth dimension
#' xx=helpNomics('ECB/EXR', dim=5)
#'
#' #looking for the 'output gap' indicator in OECD's Economic Outlook dataflow
#' helpNomics('OECD/EO', dim='VARIABLE', pattern='gap')
#'
#' @export
Nomics = function(query, as=c("md3","2d","1d","array","zooreg","pdata.frame"), drop=NA,
                  ccode=getOption('defaultcountrycode'), flags = FALSE, silent = FALSE, ...) {
  #EXAMPLE: Nomics('Eurostat/prc_hpi_a/A.DW_EXST.INX_A_AVG.BE+BG')
  # ww=Nomics('ECB/EXR/A..EUR.SP00.A',as="m",drop=TRUE)

  as=.asget(as)
  if (is.na(drop)) { if (as %in% c("1d", "2d")) drop= FALSE else drop = TRUE}

  if (!require(jsonlite)) stop("requires package 'jsonlite'")

  if (missing(query)) {
    stop("you have to provide the query argument.\n Look for series codes here:\nhttp://db.nommics.world")
  }

  if (length(query)>1L) { warning('query has to be singleton. Only the first element was taken'); query=query[[1]]}

  #query=sub(".","/",query,fixed=TRUE)


  #Eurostat/prc_hpi_a/A.DW_EXST.INX_A_AVG.CZ+CY
  #query='Eurostat/prc_hpi_a/A.DW_EXST.INX_A_AVG.BE+BG'

  makeDBeconURL = function(query,observations=TRUE,metadata=TRUE) {
    baseurl="https://api.db.nomics.world/v22/series/"
    paste0(baseurl,query,"?format=json&","observations=",tolower(observations),"&metadata=",tolower(metadata))
  }

  myquery=.adjquery(query,checkProv = TRUE)
  if (tolower(gsub('/.*$','',myquery[1])) %in% tolower(.altnicksNomicsProviders)) {
    myquery=paste0(names(.altnicksNomicsProviders)[tolower(.altnicksNomicsProviders)==tolower(gsub('/.*$','',myquery))], '/',gsub('^[^/]*/','',myquery))
  }

  myurl = makeDBeconURL(.adjquery(query,checkProv = TRUE))
  if (!silent) message("Retrieving data...")
  #mycon=url(myurl,method='libcurl')
  #on.exit(close(mycon))
  htreqres = .readLinesFromUrl(myurl) #suppressWarnings(try(readLines(mycon,warn = FALSE),silent = TRUE))
  if (grepl("try-error",class(htreqres))) {
    #either no connection or 404 error, find out which one=
    #try(close.connection(mycon),silent=TRUE); try(rm(mycon),silent=TRUE);
    #mycon=url('http://api.db.nomics.world/v22/apidocs',method='libcurl')
    #htreqres = suppressWarnings(try(readLines(mycon, warn=FALSE),silent = TRUE))
    #if (grepl("try-error",class(htreqres))) {try(close.connection(mycon),silent=TRUE); try(rm(mycon),silent=TRUE); stop('Could no tconnect to db.nomics.world.')}

    mytest=.readLinesFromUrl('https://api.db.nomics.world/v22/apidocs')
    if (grepl("try-error",class(mytest))) { stop('Could no tconnect to db.nomics.world.')}
    #return(invisible(helpNomics(query)))
    stop('The query "', query, '" does not return a result. Enter\n helpNomics("' ,query, '")\n  to inspect the reasons why')
  }

  #try(close.connection(mycon),silent=TRUE); try(rm(mycon),silent=TRUE);
  #read JSON result
  #bb=try(fromJSON(htreqres),silent = TRUE)
  dd=try(parse_json(htreqres),silent = TRUE)
  if (grepl("try-error",class(dd))) {
    stop(gsub("</string>","",gsub('<string.*\\">',"",htreqres)))
  }
  if (!silent) message("Got data. Now parsing ...")
  closeAllConnections()

  if (!is.null(dd[["message"]])|!is.null(dd[["errors"]])) {
    stop(dd[["message"]])
  }
  if (dd$series$num_found > dd$series$limit) stop("Requested  number of series (",dd$series$num_found,") exceeds permissible maximum of ",dd$series$limit,".")

  mydims=(dd$dataset$dimensions_codes_order);names(mydims) = unlist(mydims); xdn = lapply(mydims,function(X) character(0))


  ddt=data.table(matrix(NA_character_,0,length(xdn),dimnames = list(NULL,names(xdn))))
  xdn[["TIME"]] = timo()
  ddt[,TIME:=timo()]
  ddt[[.md3resnames('value')]]=numeric()

  #.cttim$frqcodes

  yesfill=FALSE
  warnflagct=0L
  for (i in seq_len(dd$series$num_found)) {
    myser = dd[["series"]][["docs"]][[i]]
    myserdims=myser$dimensions[names(mydims)]
    ixfd=grep('^freq$',names(myserdims),ignore.case = TRUE)
    tvec=.char2timo(unlist(myser$period), frq = myserdims[[ixfd]], guess = FALSE)
    ovec=unlist(myser$value)
    ovec = suppressWarnings(as.numeric(ovec)) # if nonumeric strings are converted to NA, so be it

    l2add=myser$dimensions
    l2add[['TIME']] = tvec; l2add[[.md3resnames('value')]]=ovec

    if (flags & length(myser$observations_attributes)) {

      for (j in seq_along(myser$observations_attributes)) {
        if (length(myser$observations_attributes[[j]][[2]])==NROW(tvec)) {
          myflagname=.md3resnames(myser$observations_attributes[[j]][[1]][1L])
          l2add[[myflagname]]= unlist(myser$observations_attributes[[j]][[2]])
          l2add[[myflagname]][!nchar(l2add[[myflagname]])]=NA_character_
          yesfill=TRUE

        }
      }
    }

    ddt = data.table:::rbind.data.table(ddt,l2add,fill=yesfill)
    #
    #
    #
    # flagvec = rep(NA_character_,length(tvec))
    # myfreq=grep("FREQ",names(myserdims),ignore.case=TRUE);
    # browser()
    # if (length(myfreq)) {
    #   myfreq=toupper(myserdims[[myfreq]])
    # } else {
    #   myfreq=names(.mdt_frqguess(tvec))[[1]]
    # }
    #
    # #tvec = .mdt_convertperiodgivenfreq(tvec,myfreq)
    # #idvec=paste0(myser$series_code,".",tvec)
    # idvec=paste0(paste(myserdims,collapse="."),".",tvec)
    # names(ovec) = idvec
    # temp=myserdims
    # temp[["TIME"]]=tvec
    #
    # for (nn in names(temp)) {
    #   if (is.null(xdn[[nn]])) xdn[[nn]]=temp[[nn]] else xdn[[nn]][temp[[nn]]] <- temp[[nn]]
    # }
    #
    # if ('observations_attributes' %in% names(myser))  (flagvec=unlist(myser$observations_attributes[[1]][[2]]))
    # lout[[.md0resnames("value")]] = c(lout[[.md0resnames("value")]], .naomitvec(ovec))
    # if (length(flagvec)==1L) if (!is.na(flagvec)) if (flagvec=="") flagvec=character(0)
    # if (length(flagvec)) {
    #   if (length(flagvec)==length(idvec)) {
    #     names(flagvec) = idvec; flagvec[nchar(flagvec)==0L]=NA
    #     lout[[.md0resnames("flag")]] = c(lout[[.md0resnames("flag")]], na.omit(flagvec))
    #   } else {
    #     warnflagct=warnflagct+1L
    #   }
    # }
  }


  xdn2=lapply(ddt[,seq_len(length(xdn)-1),with=FALSE],unique)
  for (i in names(xdn2)) {
    xdn2[[i]]=intersect(names(dd$dataset$dimensions_values_labels[[i]]),xdn2[[i]])
  }
  xdn2[['TIME']]=sort(unique(ddt$TIME))
  xdn2=xdn2[names(xdn2)!='FREQ']
  ddt[['FREQ']]<-NULL

  ldn=dd$dataset$dimensions_values_labels
  ldn=lapply(ldn,function(x) data.frame(code=names(x),`label:en`=unlist(x),check.names=FALSE,stringsAsFactors=FALSE))
  attr(ddt,'dcstruct') = .dimcodesrescue(xdn2,olddc = ldn)
  #browser()
  #attr(ddt,'dcstruct') = .dimcodesrescue(xdn2)
  ddm=.md3_class(ddt)


  if (drop) { ddm = drop.md3(ddm)}

  #if (!is.null(ccode)) { lout=.fixcountrycode.md0(lout,provider =   .adjquery(query,sepProvider = TRUE,checkProv = TRUE)[[1]], tocode = ccode)}
  as=.asget(as)
  if (as=="zooreg") { return(as.zoo.md3(ddm))}
  if (as=="arr") { return(as.array.md3(ddm))}
  if (as=='md3') {return(ddm)}


  args(data.table::dcast.data.table)
    ddt=.dt_class(ddm)
    colnames(ddt)<-gsub('^_\\.','',colnames(ddt))
  if (as=='data.table')  return(ddt)
  if (as=='1d')  return(as.data.frame(ddt) )

    if (any(colnames(ddt)=='TIME'))    return(data.table::dcast.data.table(ddt,...~ TIME))
    stop('not sure about retun format')

  #if (as == "2d") { return(as.data.frame.md0(lout,.~TIME))}


  #
  #
  # if (warnflagct>0L ) warning("Problems with interpreting DBnomics flag attribution in ",warnflagct," time series. Dropped flags for those values.")
  # xdn=xdn[c(unlist(dd$dataset$dimensions_codes_order),"TIME")]
  #
  # xdn=.mdcheckdimnames(.fixhihi(xdn,ignore.time = FALSE),TRUE)
  # mydimcodes = function(dimref) {
  #   tempmat=data.frame(matrix(NA,0,2,dimnames = list(NULL,c("code","label:en"))),stringsAsFactors=FALSE, check.names = FALSE);
  #   temp=dd$dataset$dimensions_values_labels[[dimref]];
  #   tempmat[names(temp),1:2]=t(sapply(names(temp),function(x) c(x,temp[[x]])))
  #   return(tempmat)
  # }
  # xdc = lapply(as.list(names(xdn)),mydimcodes); names(xdc) = names(xdn)
  # xdc[['TIME']][xdn[['TIME']],1:2] = xdn[['TIME']]
  # #xdimdescr = rbind(t(sapply(names(xdn)[-length(xdn)],function(x) c(x,dd$dataset$dimensions_labels[[x]]))),TIME=c("TIME","TIME"))
  # #colnames(xdimdescr) = c("code","label:en")
  #
  # attr(lout,"hihi") = xdn
  # attr(lout,"dimcodes") = xdc
  # #attr(lout,"dimdescr") = xdimdescr
  # class(lout) = "md0"
  #
  # if ("dimensions_labels" %in% names(dd$dataset)) {
  #   dimdescr(lout)[-length(xdn),"label:en"] = sapply(names(mydims)[-length(xdn)],function(x) dd$dataset$dimensions_labels[[x]])
  # }
  #
  # if (drop) { lout = drop.md3(lout)}
  #
  # if (!is.null(ccode)) { lout=.fixcountrycode.md0(lout,provider =   .adjquery(query,sepProvider = TRUE,checkProv = TRUE)[[1]], tocode = ccode)}
  #
  # if (as=="zooreg") { return(zooreg.md0(lout))}
  # if (as=="arr") { return(as.array.md0(lout))}
  # if (as=="1d") { return(.md0df(lout)) }
  # if (as == "2d") { return(as.data.frame.md0(lout,.~TIME))}
  # return(lout)
}

#dimcodes(Nomics('Eurostat/prc_hpi_a/A.TOTAL..'))


.Nomicsqueryadj = function(query) {
  temp=strsplit(query,split="[./]")[[1]]
  if (length(temp)==0L) return(query)
  if (length(temp)==1L) return(paste0(temp[[1]],"/"))
  if (length(temp)==2L) return(paste0(temp[[1]],"/",temp[[2]],"/"))
  paste0(temp[[1]],"/",temp[[2]],"/",paste(temp[-(1:2)],collapse="."))
}

.adjquery = function (query, provider = "", sepProvider=FALSE,checkProv=FALSE) {
  #reforms any SDMX Restful query strings to slash slash dot format
  # e.g. ECB.BSI.A.AA+DD.YY to ECB/BSI/A.AA+DD.YY
  # or  Ecb/BSI.A.AA+DD.YY to ECB/BSI/A.AA+DD.YY
  # or ECB.BSI.A|AA+DD|YY to ECB/BSI/A.AA+DD.YY
  # with sepProvider=TRUE, ECB.BSI.A|AA+DD|YY returns c('ECB','BSI/A.AA+DD.YY'))
  #Arguments:
  # query: a query like PROVIDER/FLOW/A..C+D.E or FLOW/A..C+D.E or PROVIDER/FLOW.A..C+D.E, etc.
  # provider: optional; if provided, this provider will pe prefixed to final result
  # sepProvider: see below
  # checkProv: if TRUE then adjusts the letter cases of provider to conform to SDMX providers (e.g. ECB not Ecb)
  #  if alternatively checkProv is a vector of strings, then the function validates the provider against that vectors
  #  if the provider is not found in checkProv then it is set to ''
  # REturns:
  # if sepProvider = FALSE, a string; if sepProvider=TRUE a list of two strings (proviider and query)
  if (is.logical(checkProv)) {
    if (checkProv)
      myprov = .NomicsproviderCodes$get()
  } else {
    myprov = checkProv
    checkProv = TRUE
  }
  adjquerysub = function(query) {
    query = gsub("\\|", ".", query)
    temp = strsplit(query[[1]], split = "[./]")[[1]]
    if (substring(query[[1]], nchar(query[[1]])) == ".") {
      temp = c(temp, "")
    }
    if (length(temp) == 0L)
      return(query)
    if (checkProv) {
      temp[[1]] = c(temp[[1]], myprov)[match(toupper(temp[[1]]),
                                             nomatch = 1, c("/", toupper(myprov)))]
    }
    if (length(temp) == 1L)
      return(paste0(temp[[1]], "/"))
    if (length(temp) == 2L)
      return(paste0(temp[[1]], "/", temp[[2]], "/"))
    paste0(temp[[1]], "/", temp[[2]], "/", paste(temp[-(1:2)],
                                                 collapse = "."))
  }
  if (nchar(provider)) {
    query = paste0(provider, "/", gsub(paste0("^",
                                              provider, "[.\\/]"), "", query, ignore.case = TRUE))
  }
  query = adjquerysub(query)
  if (!sepProvider) {
    return(query)
  }
  return(c(provider = gsub("/.*$", "", query),
           query = gsub("^[^/]*/", "", query)))
}


.altnicksNomicsProviders=c('Eurostat'='ESTAT', 'BUBA'='BBK','UN'='UNSD')

.Nomicsproviders = function(id=NULL) {
  if (!require(jsonlite)) stop("requires package 'jsonlite'")
  #htreqres = suppressWarnings(readLines(url("http://api.db.nomics.world/v22/providers/",method='libcurl'),warn = FALSE))
  htreqres = .readLinesFromUrl("https://api.db.nomics.world/v22/providers/")
  ee=fromJSON(htreqres)
  mout=ee$providers$docs[,c("code","name","indexed_at")]
  rownames(mout)=mout[,1]; mout=mout[,2:3]
  mout=cbind(mout,'altnicks'=unname(.altnicksNomicsProviders[rownames(mout)]))

  if (!is.null(id)) return(mout[id,])
  return(mout)
}
.NomicsproviderCodesBuilder = function() {
  .internvec = NULL
  .updateit = function() {
    if (is.null(.internvec)) {
      myvec=try(rownames(.Nomicsproviders()),silent=TRUE)
      if (grepl('error',class(myvec)[[1]])) return(FALSE)
      .internvec <<- myvec
      return(TRUE)
    }
  }
  return(list(
    get = function() {
      .updateit()
      return(.internvec)
    },
    set = function(vec) {
      .internvec <<- vec
    }
  ))
}
.NomicsproviderCodes = .NomicsproviderCodesBuilder() #this is to keep the provider codes ready




.Nomicsdataflows = function(idprov, categ=FALSE) {


  #__ subfunctions start
  getkids = function(node) {
    if (is.list(node)) if ("children" %in% names(node)) {

      temp=lapply(node$children,getkids)
      names(temp)=sapply(node$children,"[[","code")
      return(temp)

    }
    #myvec=sapply(node,"[[",'name')
    #names(myvec) = sapply(node,"[[",'code')
    myvec=character(0)
    myvec[node$code] = node$name
    return(myvec)
  }


  remnam= function(x) {
    if (is.list(x)) {
      names(x)=NULL
      return(lapply(x,remnam))
    }
    return(x)
  }
  #___ subfunctions end



  if (!require(jsonlite)) stop("requires package 'jsonlite'")
  htreqres = suppressWarnings(try(.readLinesFromUrl(paste0("http://api.db.nomics.world/v22/providers/",idprov,"/")),silent=TRUE))
  if (class(htreqres)=="try-error") {
    temp=suppressWarnings(try(.Nomicsproviders(),silent=TRUE))
    if (class(temp)=="try-error") { stop("No connection") }
    stop("Provider '",idprov,"' is not available on http://db.nomics.world")
  }



  ee=parse_json(htreqres)
  lout=lapply(ee$category_tree,getkids)
  names(lout) = sapply(ee$category_tree,"[[","code")
  if (categ) return(lout)

  vout=unlist(remnam(lout))
  return(vout[sort(names(vout))])

}

.Nomicsdataflowinfo = function(idprov, idflow) {
  if (!require(jsonlite)) stop("requires package 'jsonlite'")
  htreqres = suppressWarnings(.readLinesFromUrl(paste0("http://api.db.nomics.world/v22/series/",idprov, "/",idflow,"?limit=1&metadata=true&observations=true&format=json"),warn = FALSE))
  ee=parse_json(htreqres)
  mydims= ee$dataset$dimensions_labels[unlist(ee$dataset$dimensions_codes_order)]
  if (is.null(mydims)) { mydims = unlist(ee$dataset$dimensions_codes_order)}
  list(id=idflow,name = ee$dataset$name, nb_series=ee$dataset$nb_series,
       dims=mydims,
       dn = ee$dataset$dimensions_values_labels[unlist(ee$dataset$dimensions_codes_order)],
       example_query = paste0(idprov,"/",idflow,"/",ee$series[[1]][[1]]$series_code))

}

#' @rdname Nomics
#' @export
helpNomics = function(query, pattern="", dim=NULL, verbose=TRUE) {
  if (missing(query)) { query=""}
  query = strsplit(.adjquery(query,checkProv=TRUE),split="/")[[1]]

  if (length(query)==0L) {
    message("DBnomics has the following Data Providers.\nRun e.g. helpNomics('ECB') to learn more about querying ECB data")
    return(.Nomicsproviders()[,1,drop=FALSE])
  }

  if (length(query)==1L) {
    temp=suppressWarnings(try(.Nomicsdataflows(query[[1]],categ=FALSE),silent=TRUE))
    if (nchar(pattern)>0L) {
      temp=temp[grepl(pattern,names(temp),ignore.case = TRUE) | grepl(pattern,temp,ignore.case = TRUE)]
      if (verbose) cat(paste(names(temp),temp,sep="\t"),sep = "\n")
      return(invisible(temp))
    }
    if (length(temp)< 100L) {
      if (verbose) cat("The following dataflows are available for provider", query[[1]],":\n")
      if (verbose) cat(paste(names(temp),temp,sep="\t"),sep = "\n")
    } else {
      if (verbose) cat("Provider ", query[[1]]," has ",length(temp)," dataflows. Only the first 100 are shown:\n")
      if (verbose) cat(head(paste(names(temp),temp,sep="\t"),100),sep = "\n")
      if (verbose) cat('Use print(helpNomics("',query[[1]],'")) to show all dataflows\n')
    }

    return(invisible(temp))
  }


  dfinfo=try(.Nomicsdataflowinfo(query[[1]],query[[2]]),silent=TRUE)
  if (grepl('error',class(dfinfo))) {
    temp=try(.Nomicsdataflows(query[[1]],categ=FALSE),silent=TRUE)
    if (class(temp)=="try-error") { stop("No connection") }
    stop("Dataflow '",query[[2]],"' is not available for provider ",query[[1]],'.')
  }
  if (length(query)==2L & !length(dim)) {
    if (nchar(pattern)>0L) {
      #strall=lapply(dfinfo$dn,function(x) as.matrix(unlist(x)))
      #names(strall) =names(dfinfo$dn)
      #for (i in names(strall)) rownames(strall[[i]] ) = paste0(i,": ",rownames(strall[[i]]))
      str=as.matrix(unlist(dfinfo$dn))
      colnames(str) = paste0(query[[1L]],"/", query[[2L]],": ",dim)
      str=str[grepl(pattern,rownames(str),ignore.case = TRUE) | grepl(pattern,str[,1],ignore.case = TRUE),,drop=FALSE]
      if (!NROW(str)) {stop('No code matching pattern ', pattern, ' can be found in dimension ',dim,' of ', query[[1]],'/',query[[2]])}
      print(substr(str,0L,100L))
      return(invisible(str))
    } else {
      str = paste0(query[[1]]," dataset ", dfinfo$id, ": ", dfinfo$name,"\n")
      str = paste0(str, " Nb series: ", dfinfo$nb_series,"\n")
      str = paste0(str, " Dimensions: ", length(dfinfo$dims), " (excl. time): \n")
      for (i in names(dfinfo$dims)) {
        #x2add =""; if (toupper(i)=="FREQ") { x2add = paste0("(",")"); browser()}
        str = paste0(str, "  * ",i, ": ",dfinfo$dims[[i]]," [",ifelse(length(dfinfo$dn[[i]]),length(dfinfo$dn[[i]]),"?"), " elements]","\n")
      }
      tempilludim=names(dfinfo$dn)[[which.max(unlist(lapply(dfinfo$dn,NROW)))]]
      str=paste0(str, 'Use helpNomics("',query[[1]],"/", dfinfo$id,'", dim="',tempilludim,'"), for instance, to see which codes are avaible for dimension ',tempilludim,'.\n'); rm(tempilludim)
      str = paste0(str, " Example query: ", dfinfo$example_query)
      if (verbose) cat(str)
      return(invisible(dfinfo))
    }
  }


  if (length(dim)) { #so the user wants information about dimension codes
    dim=dim[[1]]
    if (is.character(dim)) {
      dimchosen=names(dfinfo$dn)[match(tolower(trimws(dim)),tolower(names(dfinfo$dn)),nomatch = integer(0))]
    } else {
      dimchosen=names(dfinfo$dn)[dim]; if (is.na(dimchosen)) dimchosen=character(0)
    }
    if (length(dimchosen)) { dim =dimchosen} else {
      stop("Dimension '",dim,"' does not exist in ",query[[1L]],"/",query[[2L]],". Type helpNomics('",query[[1L]],"/",query[[2L]],"') to see what dimensions exist.")
    }

    str=as.matrix(unlist(dfinfo$dn[[dim]]))
    colnames(str) = paste0(query[[1L]],"/", query[[2L]],": ",dim)
    if (nchar(pattern)) {
      str=str[grepl(pattern,rownames(str),ignore.case = TRUE) | grepl(pattern,str[,1],ignore.case = TRUE),,drop=FALSE]
      if (!NROW(str)) {stop('No code matching pattern ', pattern, ' can be found in dimension ',dim,' of ', query[[1]],'/',query[[2]])}
    }
    print(substr(str,0L,100L))
    return(invisible(str))

  }



  dftest=try(Nomics(paste0(query[[1]],"/",query[[2]],"/",query[[3]]),drop=FALSE,silent = TRUE), silent=TRUE)
  if (!grepl("error",class(dftest))) {
    str = paste0("The query ",query[[1]],"/",query[[2]],"/",query[[3]], " was sucessful.\n")
    str = paste0(str,query[[1]]," dataset ", dfinfo$id, " - ", dfinfo$name,"\n Dimension codes:\n")
    dfdc=dimcodes(dftest)
    for (dc in names(dfdc)) {
      if (dc!="TIME") {
        str=paste0(str,"   ",dc,": ",ifelse(nrow(dfdc[[dc]])==1L, paste0(dfdc[[dc]][1,"code"]," - ",dfdc[[dc]][1,2]), "Various"),"\n")
      }
    }
    if (verbose) cat(str)
    return(invisible(dftest))
  }

  #so the query is complete, yet does not work
  lquery = .trafoRestQuery(query[[3]],alsosingleton = TRUE) #this is to split the . and +
  if (length(lquery)!=(length(dfinfo$dims))) {
    stop('The query you provides suggests ', length(lquery), ' dimensions (excluding TIME) for the dataflow.\n',
         'Yet the ', query[[1]],' dataflow ',query[[2]],' has ',length(dfinfo$dims),' dimensions (excl. TIME)')
  }
  for (i in 1:length(lquery)) {
    if (length(names(dfinfo$dn[[i]])) & length(lquery[[i]])) {
      if (!any(lquery[[i]] %in% names(dfinfo$dn[[i]]))) {

        stop('You specified the following codes  for dimension ',i, ' (',names(dfinfo$dims)[[i]],'):  ',
             paste(lquery[[i]],collapse=', '),
             '\n  Yet none of those codes exists in dataflow ', dfinfo$id ,' dimension ',names(dfinfo$dims)[[i]],
             ifelse(!is.null(dfinfo$dn[[i]]),paste0(', which only contains ',length(dfinfo$dn[[i]]),' codes such as those: \n',paste(head(names(dfinfo$dn[[i]]),10),collapse=', ')),""),
             '\n  Type helpNomics("',query[[1L]],'/',query[[2L]],'", dim = "',names(dfinfo$dims)[[i]],'")  to learn more about what those codes mean.')
      }
    }

  }

  stop("Could not determine what's wrong with your query.\n Does it conform to this example?\nExample query: ", dfinfo$example_query )
}





#!!!!!!!!!!!!!!!!!!!!
.fixcountrycode.md0 = function (mdo, provider = NULL, tocode = getOption("defaultcountrycode"),
                                cols2fix = NULL, permsrc = c("iso2c", "iso3c","name.en", "iso3n", "imf")) {
  if (!is.md0(mdo)) return(MDcountrycode:::.fixcountrycode(mdo,provider = provider, tocode = tocode, cols2fix =   cols2fix, permsrc = permsrc))

  xdnorig=attr(mdo,'hihi')
  xdn=MDcountrycode:::.fixcountrycode(xdnorig,provider = provider, tocode = tocode, cols2fix =   cols2fix, permsrc = permsrc)

  if (identical(xdn,xdnorig)) { return(mdo)}
  class(mdo)='list'
  countrydims=which(unlist(lapply(as.list(names(xdn)),function(i) !identical(xdn[[i]],xdnorig[[i]]))))
  for (j in countrydims) {
    if (anyDuplicated(xdn[[j]])) {warning('could not convert to unique country codes.'); class(mdo)='md0'; return(mdo)}
  }


  for (i in names(mdo)) {
    if (!length(.subset2(mdo,i))) { break}
    tempcodes=MD0:::.mdrest2codes(names(.subset2(mdo,i)))

    for (j in countrydims) {
      tempdict = xdn[[j]]; names(tempdict) = xdnorig[[j]]
      tempcodes[,j] = tempdict[tempcodes[,j]]
    }
    names(mdo[[i]])=MD0:::.mdcodes2rest(tempcodes)
  }
  tempdc=dimcodes.m30(mdo)
  attr(mdo,'hihi') = xdn
  for (j in countrydims) {
    tempdc[[j]][["code"]] = xdn[[j]]
    rownames(tempdc[[j]]) = xdn[[j]]
  }
  attr(mdo,'dimcodes') = tempdc
  class(mdo)='md0'
  return(mdo)

}



.readLinesFromUrl = function(myurl,...) {

  mzcon=url(myurl,method='libcurl')
  on.exit(try(close(mzcon),silent=TRUE))
  htreqres = suppressWarnings(try(readLines(mzcon,warn = FALSE),silent = TRUE))
  close(mzcon)
  htreqres
}
