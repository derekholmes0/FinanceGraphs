# =======================================================================================================
#' Date Utilities
#'
#' @name gendtstr
#' @rdname fg_date_utilities
#' @description COnverts a generic relative string defining one or two endpoints to exact dates or datestrings
#' @param x String describing generalized date as of today
#' @param today Default `Sys.Date()`
#' @param rtn What to return, Default is string of form `yyyy-mm-dd::yyyy-mm-dd`.  Can also be `list`, `first`, `days`
#' @returns an exact start date `startdt` and an exact end date `enddt`, in the following forms:
#'    If `rtn="list"` returns `c(startdt,enddt)`, if `rtn="first"` then `startdt`, if `rtn="days` then an integer number of days from `startdt` to `today`  otherwise (by default) `"startdt::enddt"`
#' @examples
#' gendtstr("-3m::")
#' gendtstr("-2y::-3m",today=as.Date("2025-03-15"))
#' @export
gendtstr<-function(x,today=Sys.Date(),rtn="dtstr") {
  if(is.numeric(x)) { return(paste0(today-lubridate::ddays(x),"::")) }
  gendtsplit <- c(strsplit(x,"::")[[1]],"")[1:2]
  repfn<-function(y,today, default=NULL) {
    y <- tolower(y)
    aa<-stringr::str_replace(y,"y",""); if (aa != y) { return(as.Date(today+lubridate::dyears(as.numeric(aa)))) }
    aa<-stringr::str_replace(y,"d",""); if (aa != y) { return(as.Date(today+lubridate::ddays(as.numeric(aa)))) }
    aa<-stringr::str_replace(y,"m",""); if (aa != y) { return(as.Date(lubridate::add_with_rollback(today,months(as.numeric(aa))))) }
    aa<-stringr::str_replace(y,"w",""); if (aa != y) { return(as.Date(today+lubridate::dweeks(as.numeric(aa)))) }
    if(nchar(y)<=2) { return(default)}
    else { return(y) }
  }
  gendt1 <- repfn(gendtsplit[[1]], today, default="1980-01-01")
  gendt2 <- repfn(gendtsplit[[2]], today, default=today)
  if(rtn=="days") { return(as.numeric(today-as.Date(gendt1))) }
  else if (rtn=="list") { return(c(as.Date(gendt1),as.Date(gendt2)))}
  else if (rtn=="first") { return(as.Date(gendt1)) }
  else {  return(paste0(gendt1, "::", gendt2)) }
}

#' Narrow dataset by date string
#' @name narrowbydtstr
#' @rdname fg_date_utilities
#' @param xin  Input `data.frame` or `data.table` with a Date column
#' @param dtstr Generalized Date string of the form `<yyyy-mm-dd>::<yyyy-mm-dd>` or e.g. `-3m::`
#' @param includetoday (Default: TRUE) pass either today `Sys.Date()` or `Sys.Date()-1` to `gendtstr`
#' @param windowdays (Default: 0)Number of additional days to add at beginning of series
#' @param invert  (Default: FALSE) Return dates not in `dtstr`
#' @param addindicator (Default: FALSE) Returns original dataset with logical variable `inrange` if date is in desired range.
#' @returns Same form as `xin`, i.e. a `data.table` or `data.frame`
#' @examples
#' narrowbydtstr(eqtypx,"-2m::-1m")
#' @export
narrowbydtstr<-function(xin,dtstr="",includetoday=TRUE, windowdays=0, invert=FALSE, addindicator=FALSE) {
  dtname <- find_col_bytype(xin,lubridate::is.instant)
  if(dtstr=="") { return(xin) }
  this_dtstr <- ifelse(windowdays>0, extenddtstr(dtstr,begchg=-windowdays), dtstr)
  dtlist <- gendtstr(this_dtstr,today=as.Date(ifelse(includetoday,Sys.Date(),Sys.Date()-1)),rtn="list")
  if( is.data.frame(xin) ) {
    dtcol <- xin[[dtname]]
  }
  else {
    dtcol <- xin
  }
  toget <- data.table::between(dtcol,dtlist[1],dtlist[2])
  if(invert) { toget <- !toget }
  if(addindicator & is.data.frame(xin)) { xin$inrange <- toget }
  else { xin <- xin[toget] }
  return(xin)
}

#' Extend a generic date string in varioud ways
#' @name extenddtstr
#' @rdname fg_date_utilities
#' @param instr Input generalized date string, `data.table` or `xts` dataset
#' @param begchg (Default: 0) Number of calendar days to extend beginning
#' @param endchg (Default: 0) Number of calendar days to extend end
#' @param mindt Minimum date to return
#' @param maxdt Maximum date to return
#' @param rtn string describing what to do: (`list`,`datelist`,`fromtoday`,`totoday`)
#' @param rtnstyle REturn datestring or list
#' @returns `character` string or `list` with new dates
#' @examples
#' extenddtstr("-2m::-1m")
#' extenddtstr("-2m::-1m",begchg=-10,endchg=5)
#' @export
extenddtstr <- function(instr,begchg=0,endchg=0,mindt=NULL,maxdt=NULL,rtn="",rtnstyle="string") {
  index=NULL
  if(xts::is.xts(instr)) {
    dtindex <- lubridate::as_datetime(xts::.index(instr),origin = lubridate::origin)
    instr <- paste(range(lubridate::as_date(dtindex),na.rm=T),collapse="::") }
  if(is.data.frame(instr)) {
    instr <- paste0(range(instr$DT_ENTRY,na.rm=T),collapse="::") }
  spl <- gendtstr(instr,rtn="list")
  if(is.na(spl[1])) { spl[1] <- as.character(Sys.Date())}
  if(is.na(spl[2])) { spl[2] <- as.character(Sys.Date())}
  if(lubridate::is.instant(as.Date(spl[1]))) { spl[1] <- as.character(as.Date(spl[1])+begchg) }
  if(lubridate::is.instant(as.Date(spl[2]))) { spl[2] <- as.character(as.Date(spl[2])+endchg) }
  if(rtn=="list") {
    rtnstyle <- "list"
  }
  if(rtn=="datelist") {
    spl <- list(as.Date(spl[1])+begchg,as.Date(spl[2])+endchg)
    rtnstyle <- "list"
  }
  if(rtn=="fromtoday") {
    spl <- as.numeric(list(Sys.Date()-(as.Date(spl[1])+begchg),Sys.Date()-(as.Date(spl[2])+endchg)))
    rtnstyle <- "list"
  }
  if(rtn=="totoday") { spl[2] <- NA_real_ }
  if(lubridate::is.instant(mindt)) { spl[1] <- max(spl[1],mindt) }
  if(lubridate::is.instant(maxdt)) { spl[2] <- min(spl[2],maxdt) }
  if(rtnstyle=="list") { return(spl) }
  return(paste0(as.character(spl[1]),'::',data.table::fcoalesce(as.character(spl[2]),"")))
}

# =======================================================================================================
#' Other utititlies internal to this package
#'@noRd
optString_parse <-function(x,item,default="TRUE") {
    ops<-beg<-end<-NULL
    x1=tibble::tibble(ops=s(x)) |>
            tidyr::separate_wider_delim(ops,",",names=c("beg","end"),too_many="merge",too_few="align_start") |>
            dplyr::filter(grepl(item,beg)) |> dplyr::pull(end)
    if(identical(x1,character(0))) {
      return(FALSE)
      }
    else {
      return( dplyr::coalesce(x1,default) )
    }
}
# s(plit) converts string to list, but passes logical
s<-function(x,sep=";",fixed=TRUE,rtn=NULL) {
    if(is.logical(x)) { return(x) }
    y=unlist(strsplit(x,sep,fixed=fixed))
    if(is.numeric(rtn)) { if(length(y)>=rtn) { y=y[rtn] } }
    return(y)
    }

message_if <- function(reallydothis,...) {
  if(reallydothis) { message(list(...)) }
}

form_xlist <- function(instring) {
  todo <- NULL
  if(is.data.frame(instring)) return(instring)
  suppressWarnings(tibble::tibble(todo=s(instring)) |>  # Column names are case sensitive
                   tidyr::separate_wider_delim(todo,",",names= c("todo","a1","a2","a3","a4","a5"),
                                               too_many="drop",too_few="align_start"))
}

get_fromlist <- function(indta,grepstr) {
    todo<-NULL
    dplyr::filter(indta,grepl(grepstr,todo))
  }

#' @importFrom purrr map2
lineAssign<-function(xline) {
  if(nrow(xline)>1) message("lineAssign cannot assigm more than oneline")
  aaa1 <- map2(names(as.list(xline[1,])),as.list(xline[1,]), function(x,y){assign(x,y,pos=sys.frame(1))})
}

find_col_bytype <- function(indt,typeoffn,firstonly=TRUE,takeout=NA_character_) {
    rtn <- names(indt)[sapply(indt, typeoffn)]
    if(length(rtn)==0) { return(NULL)}
    rtn=setdiff(rtn,takeout)
    if(firstonly) return (rtn[1])
    else return(rtn)
}

xts2df <- function(x) {
  DT_ENTRY<-NULL
  if(is.data.frame(x)) {
    return(data.table::as.data.table(x)) }
  if(is.null(colnames(x))) {
    if(is.null(ncol(x))) {
      return(data.table::data.table())
    }
    data.table::setnames(x,paste0("V",1:ncol(x)))
  }
  rtna = data.table::setnames(data.table::as.data.table(x),"index","DT_ENTRY")
  rtna[,DT_ENTRY:=as.Date(DT_ENTRY)]
  return(rtna)
}

# =======================================================================================================
#' data table utilities
#' @noRd
DTappend <- function(indta,newdta) { data.table::rbindlist(list(indta,newdta),use.names=TRUE,fill=TRUE) }
DTUpsert<-function(a,b,keys, fill=FALSE,verbose="") { # DT kind of tough to use this replaces old data
  if(!data.table::is.data.table(b)) {
    b<- data.table::data.table(b) }
  if (is.character(a)) { aandb <- b }
  else if(nrow(a)<=0) { aandb <-b }
  else if(nrow(b)<=0 | length(setdiff(keys,colnames(b)))>0) { aandb <-a }
  else {
    data.table::setkeyv(a,keys)
    data.table::setkeyv(b,keys)
    aandb<- data.table::rbindlist(list(a[!b],b),use.names=TRUE,fill=fill)
    if(nchar(verbose)>1) { message("DTUpsert(",verbose,"): adds ",nrow(b)," rows, now ",nrow(aandb)) }
    if(  any(grepl(".x",colnames(aandb),fixed=TRUE)) ) {
      stop(" ERORR in DTUpser... colnames: ",paste0(colnames(aandb),collapse=","))
    }
  }
  data.table::setkeyv(aandb,keys)
  return(aandb)
}

coalesce_DT<-function(DT1,DT2) { # Adds columns as necessary, either row by row or single row
  `.` <- jrep <- NULL
  if (!(nrow(DT2)==1 | nrow(DT1)==nrow(DT2))) {
    stop("coalesce_dt incompatile sizes")
  }
  DT1cols <- colnames(DT1)
  DT2 <-data.table::copy(DT2)[,let(jrep=.I)]
  DT2 <- DT2[,.SD,.SDcols=setdiff(colnames(DT2),colnames(DT1))]  # Should always include jrep
  DT1 <-data.table::copy(DT1)[,let(jrep=seq(1,nrow(DT2)))]
  DT3 <- DT2[DT1,on=.(jrep)][,let(jrep=NULL)]
  setcolorder(DT3,DT1cols)
  return(DT3[])
}


coalesce_DT_byentry<-function(DT1,DT2) { # Adds columns as necessary, either row by row or single row
  `.` <- jrep <- NULL
  if (!(nrow(DT2)==1 | nrow(DT1)==nrow(DT2))) {
    stop("coalesce_DT_byentry incompatible sizes, either DT2 must be 1 row or nrow(DT1) rows")
  }
  DT1cols <- colnames(DT1)
  DT2cols <- colnames(DT2)
  DTcommoncols <- intersect(DT1cols,DT2cols)
  DTfinalcols <- setdiff(union(DT1cols,DT2cols),c("jrep"))
  DT1copy <- data.table::copy(DT1)
  DT3 <-  data.table::merge.data.table(DT1copy[,let(jrep=seq(1,nrow(DT2)))],DT2[,let(jrep=.I)],by=c("jrep"))
  DT3 <- DT3[,(DTcommoncols):=lapply(DTcommoncols, \(x) data.table::fcoalesce(.SD[[paste0(x,".x")]],.SD[[paste0(x,".y")]])), by=.(jrep)] # Common columns
  DT3 <- DT3[,.SD,.SDcols=DTfinalcols]
  return(DT3[])
}

#DT1 <- data.table::data.table(x=rep(c("b","a","c"),each=3), y=c(1,NA_integer_,6), v=1:9)
#DT2 <- data.table::data.table(x=rep(c("b","a","c"),each=3), y=c(1,100,6), v=1:9,zz="ZZZ")
#coalesce_DT_byentry(DT1,DT2)

#DT1 = data.table(x=seq(1,3),v1=letters[seq(1,3)])
#DT21 = data.table(x=1,v1="A",v2="B")
#DT2n = data.table(x=rep(2,3),V1=letters[seq(1,3)+10],V2=letters[seq(1,3)+20])
#coalesce_DT(DT1,DT21)
#coalesce_DT(DT1,DT2n)

runs_from_value <- function(indta, addrunlength=FALSE) {
  '.' <- DT_ENTRY <- N <- value <- END_DT_ENTRY <- runasnum <- NULL
  if(!data.table::is.data.table(indta)) { indta <- data.table::data.table(indta) }
  if(nrow(indta[,.N,by=.(DT_ENTRY)][N>1])>0) {
    stop("runs_from_value needs one observation per date") }
  levs <- indta[,.(runasnum=max(as.numeric(value))),by=.(value)]
  rlenc <- rle(as.numeric(indta[["value"]]))
  runs1 <- data.table::data.table(END_DT_ENTRY=indta[cumsum(rlenc$lengths),]$DT_ENTRY, runasnum=(rlenc$values))
  runs1 <- runs1[,let(DT_ENTRY=data.table::shift(END_DT_ENTRY,n=1,type="lag")+1)]
  runs1[1,"DT_ENTRY"] <- indta[1,"DT_ENTRY"]
  if(addrunlength) { runs1$runlen <- rlenc$lengths }
  runs1 <- levs[runs1,on=.(runasnum)][!is.na(value)][,let(runasnum=NULL)]
  return(runs1[])
}

# Merges a set of days back with specific dates
generalbreaks<-function(dtset,dtds,rtnasoffset=TRUE) {  # Kind of ugly, but I need this.
  if(is.data.frame(dtds)) { u2dt=sort(unique(dtds$DT_ENTRY)) } else { u2dt = dtds}
  if(is.character(dtset)) { dtset=lapply(dtset,function(x){if(grepl("-",x)){x=as.Date(x)}else{x=as.numeric(x)};x}) }
  dtset2=as.Date(unlist(lapply(dtset,function(x){dtmxx=ifelse(x<min(u2dt),max(u2dt)-x,x); u2dt1=u2dt[u2dt<=dtmxx]; ifelse(length(u2dt1)<=0,min(u2dt),max(u2dt1))})))
  if(rtnasoffset) { dtset2=max(u2dt)-dtset2 }
  dtset2
}

#' @import data.table
# Take input and puts into expected form: DT_ENTRY, data.table, keyed.  Use meltvar = eventid e.g f neces.
# Assumes a date column is always there, not true for scatter plots
generic_to_melt <- function(indata,newnames="",meltvar="variable",rtn="all") {
  # Preprocessing: get into data.table format
  if( xts::is.xts(indata) ) { indt <- xts2df(indata) }
  if(dplyr::is.tbl(indata)) { indt <- dplyr::ungroup(indata) }
  if(!is.data.table(indata)) { indt <- data.table(indata) }  # Try setDT ?
  else {
    indt <- data.table::copy(indata)
    }
  colt <- function(cls) { names(indt)[grep(cls,lapply(indt,class))] }
  dt_colnames <- list(
    'date' = grep("end",colt("Date|POSIX"),ignore.case=TRUE,invert=TRUE,value=TRUE)[1],
    'enddate' = grep("end",colt("Date|POSIX"),ignore.case=TRUE,value=TRUE)[1],
    'meltvar' = meltvar)
  if(!(is.na(meltvar)) & !(meltvar %in% colnames(indt))) {
    indt <- data.table::melt(indt,id.var=dt_colnames[['date']])
  }
  cvars <- setdiff(colt("character"),meltvar)
  if( length(cvars)<=0 ) { cvars <- NA_character_ }
  dt_colnames <- c(dt_colnames,list(
    'cvar' = cvars,
    'value' = colt("numeric")[1]
    )
  )
  if(rtn=="dtc") { return(dt_colnames) }
  if(!is.na(dt_colnames[['date']])) {
    ndtcols <- ifelse(is.na(dt_colnames[['enddate']]),1,2)
    setnames(indt,unlist(dt_colnames[seq(1:ndtcols)]),c("DT_ENTRY","END_DT_ENTRY")[1:ndtcols])
    keycols <- ifelse(is.na(meltvar),c("DT_ENTRY"),c("DT_ENTRY",meltvar))
    setcolorder(indt, keycols)
    setkeyv(indt,keycols)
  }
  newlist <- stats::setNames(list(indt,dt_colnames),s(c(newnames,"",""))[1:2])
  return(newlist)
}

form_breakset <- function(alldts,breaks,dropset="") {
  dtlimits <- range(alldts)
  if (is.character(breaks) && nrow( bktmp <- fg_get_dates_of_interest(breaks,totoday=dtlimits[2]) )>0 ) {
    break_set <- bktmp[END_DT_ENTRY>=dtlimits[1],.(BEG_DT_ENTRY=DT_ENTRY,END_DT_ENTRY,daysback=dtlimits[2]-DT_ENTRY,histcat=eventid)] }
  else if (length(breaks)>1) {
    if(all(range(breaks)==c(0,1))) {
      breaks <- sort(breaks[which(breaks>0.001)])
      bks <- alldts[pmax(1,floor(length(alldts)*(1-breaks)))] |> rev()
      break_set<- data.table(BEG_DT_ENTRY=bks,daysback=as.numeric(dtlimits[2]-bks),histcat=NA_character_)
    }
    else {
      break_set <- data.table(BEG_DT_ENTRY=dtlimits[2]-breaks,daysback=breaks)
      end_break <- data.table(daysback=as.numeric(dtlimits[2]-dtlimits[1]),BEG_DT_ENTRY=dtlimits[1],histcat=NA_character_)
      break_set <- rbindlist(list(break_set,end_break),fill=TRUE,use.names=TRUE)
    }
  }
  tlbl <- function(d1) { fifelse(d1<=365,paste0(d1,"d"),paste0(floor(d1/31),"m")) }
  break_set <- break_set[order(-BEG_DT_ENTRY)]
  break_set <- break_set[,let(dtlag=shift(daysback, n=1, fill=0, type="lag"),END_DT_ENTRY=shift(BEG_DT_ENTRY, n=1, fill=dtlimits[2]+1, type="lag"))]
  break_set <- break_set[,histcat:=fcoalesce(histcat,fifelse(BEG_DT_ENTRY==dtlimits[1],paste0("<-",tlbl(dtlag)),paste0("-",tlbl(dtlag),":-",tlbl(daysback))))]
  if(length(s(dropset))>0) {
    message_if( length(tlevels<-intersect(s(dropset),break_set$histcat))>0, "Dropping level(s) ",tlevels)
    break_set <- break_set[!data.table(histcat=s(dropset)),on=.(histcat)]
  }
  return(break_set)
}

# ---------------------------------------------------------------
# GGPLot utilities
lm_eqn = function(df,ynm,xnm,tformula=formula("y~x"),rtnstyle=""){
  df2<-dplyr::select(df,x=!!{xnm},y=!!{ynm})
  m = lm(tformula, df2);
  llm<-list(y=ynm, x=xnm,a = format(coef(m)[1], digits = 2),
            b = format(coef(m)[2], digits = 2),
            r2 = format(summary(m)$r.squared, digits = 3),
            lastresid=utils::tail(resid(m),1),
            lastsresid=utils::tail(rstandard(m),1))
  if(rtnstyle=="list") { return(llm) }
  else if (rtnstyle=="simple") {
    return(paste0("y=",llm[4]," x + ",llm[3]," rsq: ",llm[5]))        }
  else if (rtnstyle=="simplewitht") {
    return(paste0("y=x*",llm[4]," (",format(summary(m)$coefficients[6],digits=2),")+ ",llm[3]," (",
                  format(summary(m)$coefficients[5],digits=2), ") rsq: ",llm[5], " eLast:",format(llm[6],digits=2),"/z:",
                  format(llm[7],digits=2)))        }
  else {
    eq <- substitute(expression(y == a + b * x ~~rsq == r2),
                     list(y=ynm, x=xnm,a = format(coef(m)[1], digits = 2),
                          b = format(coef(m)[2], digits = 2),
                          r2 = format(summary(m)$r.squared, digits = 3)))
    return(as.character(as.expression(eq)))
  }
}
