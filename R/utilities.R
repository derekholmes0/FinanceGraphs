#' Form exact datestring from relative datestring
#'
#' @param x String describing generalized date as of today
#' @param today Default `Sys.Date()`
#' @param rtn What to return, Default is string of form `yyyy-mm-dd::yyyy-mm-dd`.  Can also be list`, `first`, `days`
#'
#' @returns depends on value of `rtn`
#'
#' @examples
#' gendtstr("-3m::")
#'
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
#'
#' @param xin  Input `data.frame` or `data.table` with a Date column
#' @param dtstr Generalized Date string of the form `<yyyy-mm-dd>::<yyyy-mm-dd>` or e.g. `-3m::`
#' @param includetoday (Default: TRUE) pass either today `Sys.Date()` or `Sys.Date()-1` to `gendtstr`
#' @param windowdays (Default: 0)Number of additional days to add at beginning of series
#' @param invert  (Default: FALSE) Return dates not in `dtstr`
#' @param addindicator (Default: FALSE) Returns original dataset with logical variable `inrange` if date is in desired range.
#'
#' @returns dataset in same form as `xin`
#'
#' @examples
#' narrowbydtstr(eqtypx,"-2m::-1m")
#'
#' @export
narrowbydtstr<-function(xin,dtstr="",includetoday=TRUE, windowdays=0, invert=FALSE, addindicator=FALSE) {
  dtname <- find_col_bytype(xin,lubridate::is.Date)
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
#'
#' @param instr Input generalized date string, `data.table` or `xts` dataset
#' @param begchg (Default: 0) Number of calendar days to extend beginning
#' @param endchg (Default: 0) Number of calendar days to extend end
#' @param mindt Minimum date to return
#' @param maxdt Maximum date to return
#' @param rtn string describing what to do: (`list`,`datelist`,`fromtoday`,`totoday`)
#' @param rtnstyle REturn datestring or list
#'
#' @returns String or list with new dates
#'
#' @examples
#' extenddtstr("-2m::-1m")
#' extenddtstr("-2m::-1m",begchg=10,endchg=5)
#'
#' @export
extenddtstr <- function(instr,begchg=0,endchg=0,mindt=NULL,maxdt=NULL,rtn="",rtnstyle="string") {
  if(xts::is.xts(instr)) {
    instr <- paste0(min(zoo::index(instr),na.rm=T),"::",max(zoo::index(instr),na.rm=T)) }
  if(is.data.frame(instr)) {
    instr <- paste0(min(instr$DT_ENTRY,na.rm=T),"::",max(instr$DT_ENTRY,na.rm=T)) }
  spl <- gendtstr(instr,rtn="list")
  if(is.na(spl[1])) { spl[1] <- as.character(Sys.Date())}
  if(is.na(spl[2])) { spl[2] <- as.character(Sys.Date())}
  if(lubridate::is.Date(as.Date(spl[1]))) { spl[1] <- as.character(as.Date(spl[1])+begchg) }
  if(lubridate::is.Date(as.Date(spl[2]))) { spl[2] <- as.character(as.Date(spl[2])+endchg) }
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
  if(lubridate::is.Date(mindt)) { spl[1] <- max(spl[1],mindt) }
  if(lubridate::is.Date(maxdt)) { spl[2] <- min(spl[2],maxdt) }
  if(rtnstyle=="list") { return(spl) }
  return(paste0(as.character(spl[1]),'::',data.table::fcoalesce(as.character(spl[2]),"")))
}


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

# s(plit) converts string to list
s<-function(x,sep=";",fixed=TRUE,rtn=NULL) {
    y=unlist(strsplit(x,sep,fixed=fixed))
    if(is.numeric(rtn)) { if(length(y)>=rtn) { y=y[rtn] } }
    return(y)
    }

message_if <- function(reallydothis,...) {
  if(reallydothis) { message(list(...)) }
}

lineAssign<-function(xline) {
  if(nrow(xline)>1) message("lineAssign cannot assigm more than oneline")
  aaa1 <- purrr::map2(
    names(as.list(xline[1,])),as.list(xline[1,]), function(x,y){assign(x,y,pos=sys.frame(1))})
}

find_col_bytype <- function(indt,typeoffn,firstonly=TRUE) {
    rtn <- names(indt)[sapply(indt, typeoffn)]
    if(firstonly) return (rtn[1])
    else return(rtn)
}

ts2df <- function(x,prefix="",adddate=TRUE) {
  rtn <- data.table::as.data.table(x)
  data.table::setnames(rtn,paste0(prefix,names(rtn)))
  if(adddate) {
    rtn$DT_ENTRY = as.Date(zoo::index(x))
    data.table::setcolorder(rtn,"DT_ENTRY")
  }
  return(rtn)
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


## -- For Debusgging only
cAssign<-function(x,dbg=TRUE,silent=FALSE,copytodisk=FALSE,copysilent=FALSE,trace=FALSE,dpath=tempdir(),dbgkey="zz",suffix="",
                  skipsaveiftoday=FALSE, nbig=10000,title="",usefst=TRUE,pframe=3,tmp=F) {
  #if(nchar(title)>1) { message("cAssign ---------------------------------------: ",title) }
  newfilename=""
  if(!is.character(x)) { stop("cAssign x must be character string for a variable name, not the actual variable..") }
  if(tmp || dpath=="t") { dpath="c:/t/" }
  if(copytodisk | copysilent) { silent=TRUE } # eliminated reduncany
    x=unlist(strsplit(x,";")[[1]])
    ppp=lapply(x,function(y){
      if(exists(y,envir=parent.frame(n=pframe))) {
        cadtmp=get(y,pos=parent.frame(n=pframe))
        if(nchar(suffix)>0) {
          ynew = paste0(y,"_",suffix)
          ymessage = sprintf("%10s as %10s",y,ynew)
        }
        else{
          ynew=y
          ymessage = sprintf("%10s",y)
        }
        if(!silent) {
          thistrace=ifelse(trace,try(traceback(max.lines=1),silent=T),"--notrace--")
          message("Assigning: ",ymessage, "(",paste(dim(cadtmp),collapse=";"),") ",
                    paste(class(cadtmp),collapse=";"), " from ",utils::tail(thistrace,1),">",title); }
##>        assign(ynew,cadtmp,envir=.GlobalEnv)
        }
      else {
        if(!silent) { print(paste("cAssign: CANNOT FIND ",y)) } }
      } )
}


