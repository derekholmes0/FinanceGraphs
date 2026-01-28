#' Maintain Constants and Dates of Interest
#'
#' @name fg_dates_of_interest
#' @description
#' `fg_get_dates_of_interest()` gets a set of time events for use in fg time series graphs
#' `fg_update_dates_of_interest()` updates a set of time events for future use in time series graphs
#' `fg_get_colors()` gets default color sets for graphs
#' `fg_update_colors()` updates or replaces default colors
#'
#' @param search_categories Grep string of categories to return
#' @param use_default (Default TRUE) use dedault dates if none else found
#' @param startdt MInimum date for events to be returned
#' @param item (Default: "") A grep string for categories desired.
#' @param replace (Default: FALSE) If TRUE, replaces existing dates of interest with new set provided, otherwise replaces/inserts new rows only.
#' @param use_default (Default: TRUE) Use package level dates of interest
#'
#' @returns Filtered datasets
#'
#' @details
#' Retrieves default dates of interest given a grepstring of categories.  There are a default set of categories provided which may not be up to date.
#'
#' New data passed into [fg_update_dates_of_interest()] or [fg_update_colors()] persists across future loads of the package. Any duplicates in the new file will be taken out.
#'
#' New doi datasets must have at least three columns:
#'
#' | Column | Meaning |
#' |:-------|:--------|
#' |`category`| Grouping name (string) for a given set of dates of interest|
#' |`eventid`| Character string to be displayed at each event.|
#' |`DT_ENTRY`| Start Date of event |
#' |`END_DT_ENTRY`| Optional end of period to define regimes or ranges of events.|
#'
#'
#' @seealso [fgts_dygraph()]
#'
#' @examples
#' \dontrun{
#' tail(fg_get_dates_of_interest("fedmoves"),3)
#' # To add (for example) a new FOMC cut of 50bps on 6/16/2026:
#' newdoi <-data.table::data.table(category="fedmoves",eventid="F:-50",
#'             DT_ENTRY=as.Date("6/16/2026",format="%m/%d/%Y"))
#' fg_update_dates_of_interest(newdoi)
#' tail(fg_get_dates_of_interest("fedmoves"),3)
#'
#' fg_get_colors("lines")
#' # To switch out second (blue) line and 6th line (red).
#' # Note use of sortable character variables to define the order of the set.
#' fg_update_colors(data.table::data.table(category=rep("lines",2),variable=c("D02","D06"),
#'                 color=c("red","blue")))
#' fg_get_colors("lines")
#' }
#'
#'
the <- new.env(parent = emptyenv())
the$cachedir <- tools::R_user_dir("FinanceGraphs", which = "cache")
if(!dir.exists(the$cachedir)) {
  newd <- dir.create(the$cachedir)
}

load("./R/sysdata.rda",envir=the)
#  loads tevents_defaults and ratingsmapmelt
the$doifn <- paste0( the$cachedir, "/fg_doi.RD")
the$colorfn <- paste0( the$cachedir, "/fg_colors.RD")
the$doi_dates <-  the$doi_default
the$default_colors <- the$colors_default

if(file.exists(the$doifn)) {
  load(the$doifn)
  the$doi_dates <- newdoi
  }
if(file.exists(the$colorfn)) {
  load(the$colorfn)
  the$default_colors <- newcolors
  }

#'
#' @rdname fg_dates_of_interest
#' @export
fg_get_dates_of_interest <- function(search_categories="",use_default=TRUE,startdt=NULL) {
  rtn <- the$doi_dates[grepl(search_categories,the$doi_dates$category,ignore.case=TRUE),][order(DT_ENTRY)]
  if(!is.null(startdt)) {
    rtn <- rtn[DT_ENTRY>=as.Date(startdt),]
  }
  return(rtn)
}


#' @rdname fg_dates_of_interest
#' @export
fg_get_colors <- function(item="") {
  rtn <- the$default_colors[which(the$default_colors$category==item),]
  if(item=="") { rtn <- the$default_colors }
  return(rtn)
}

fg_get_colorstring <- function(item="") {
  return( fg_get_colors(item)[["color"]] )
}

#' @rdname fg_dates_of_interest
#' @export
fg_update_dates_of_interest <- function(indta,replace=FALSE) {
  mincolset <- c("category","eventid","DT_ENTRY")
  mincolsmissing <- setdiff(mincolset,names(indta))
  if( length(mincolsmissing)>0 ) {
    stop("fg_create_dates_of_interest: Need to add column(s) ",mincolsmissing)
  }
  indta <- data.table::data.table(indta)
  indta <- indta[order(category,DT_ENTRY)][,':='(END_DT_ENTRY=ifelse("END_DT_ENTRY" %in% names(indta),data.table::fcoalesce(END_DT_ENTRY,DT_ENTRY),as.Date(DT_ENTRY)))]
  indta <- indta[,.SD[1],by=.(category,DT_ENTRY,END_DT_ENTRY)]
  if(replace==TRUE) {
    newdoi <- indta
  }
  else {
    newdoi <- DTUpsert(the$doi_dates,indta,c("category","DT_ENTRY"),fill=TRUE)
  }
  save(newdoi,file=the$doifn)
  assign("doi_dates",newdoi,envir=the)
  assign("doi_dates_update",Sys.Date(),envir=the)
  message("Saved dates of interest file to ",the$doifn)
  invisible(newdoi)
}


#' @rdname fg_dates_of_interest
#' @export
fg_update_colors <- function(indta,replace=FALSE) {
  mincolset <- c("category","variable","color")
  mincolsmissing <- setdiff(mincolset,names(indta))
  if( length(mincolsmissing)>0 ) {
    stop("fg_create_colors: Need to add column(s) ",mincolsmissing)
  }
  if(replace==TRUE) {
    newcolors <- indta
  } else {
    newcolors <- DTUpsert(the$default_colors,indta,c("category","variable"))
  }
  newcolors <- newcolors[order(category,variable)]
  save(newcolors,file=the$colorfn)
  assign("default_colors",newcolors,envir=the)
  message("Saved Colors of interest file to ",the$colorfn)
  invisible(newcolors)
}

# =========================================================================
# Unexported functions, but still needed with the package

fg_reset_to_default_state <- function() {
  file.remove(the$doifn)
  file.remove(the$colorfn)
  the$doi_dates <- data.table::copy(doi_default)
  the$default_colors <- data.table::copy(colors_default)
  the$tevents_defaults <- data.table::copy(tevents_defaults)
}

fg_create_defaults <- function() {
  category <- variable <- NULL
  dtmap  <- make_dtmap()
  datecols <- c("DT_ENTRY","END_DT_ENTRY")

  doi_default <- data.table::fread("./inst/extdata/doidates.csv",na.strings="")[,(datecols):=lapply(.SD,\(x) as.Date(x,"%m/%d/%Y")), .SDcols=datecols][]
  colors_default <- data.table::fread("./inst/extdata/fg_colors.csv")[order(category,variable)]
  tevents_defaults <- data.table::data.table(END_DT_ENTRY=as.Date(NA_real_),eventonly=FALSE,
                                              axis="x",color="#00cc99",strokePattern="dashed",loc="bottom",series=NA_character_)
  ratingsmapmelt <- data.table::fread("./inst/extdata/ratingsmapmelt.csv")
  usethis::use_data(doi_default,colors_default,dtmap,tevents_defaults,ratingsmapmelt, internal=TRUE,overwrite=TRUE)
}

# ----------------------- Dates

.addseasonaldates<- function(x,dtname="DT_ENTRY",toadd="all",freqvarname="") {
  # Really slow
  toaggdt<-function(x,to="yrwk") {
    convstr=list('yrwk'="%Y%V","yrweek"="%Y%V","yrmo"="%Y%m","dt"="%Y%m%d","wk"="%V","filedt"="%y%m%d")
    as.numeric(strftime(x,convstr[[to]])) }
  if(!is.data.frame(x)) {
    if(x=="vars") { return("doy|yr|qtr|doq|yrwk|week") }
    else { message("addseasonaldates(x,toadd =(all|yr|qtr|doq|ywk|week),dtname)")} }
  if (sum(grepl(dtname,colnames(x)))<=0 ) { xdt=as.Date(rownames(x)) }
  else { xdt=as.Date(x[[dtname]],use.names=FALSE) }
  if(grepl("doy|all",toadd)) { x$doy<-as.numeric(format(xdt,"%j")) }
  if(grepl("^(yr|all)$",toadd)) { x$yr<-as.numeric(format(xdt,"%Y")) }
  if(grepl("doq|all",toadd)) { x$doq<-as.numeric(xdt-as.Date(paste0( as.character(floor( (lubridate::month(xdt)-1)/3)*3+1),"/1/",x$yr),"%m/%d/%Y")) }
  if(grepl("(qtr)|yrqt|all|dfagg",toadd)) { x$yrqtr<-as.numeric(format(xdt,"%Y") )*10+as.numeric(substr(quarters(xdt),2,2)) }
  if(grepl("yrwk|all|dfagg",toadd)) { x$yrwk<-toaggdt(xdt) }
  if(grepl("yrmo|all|dfagg",toadd)) { x$yrmo<-toaggdt(xdt,to="yrmo") }
  if(grepl("week|all",toadd)) { x$wk<-toaggdt(xdt,to="wk")  }
  if(nchar(freqvarname)>0) { x[,freqvarname] = x[[toadd]] }
  return(x)
}

# Make datemap, very helpuful for narrowing dates.
#
make_dtmap <- function(yrs_ahead=5) {
  # All Dates
  tmp<-lapply(s("DT_ENTRY;isday;rolldt;isholiday;yr;yrmo;frino;yrqtr;optexp;xoptexp;isweek;ismo;isqtr;isyr"),
                      \(x) assign(x,NULL,pos=1) )
  dtmap <- data.table::data.table(DT_ENTRY=seq(from =as.Date("1970-03-20"), to = Sys.Date()+yrs_ahead*365, by = "day")) |> .addseasonaldates()
  dtmap <- dtmap[,':='(isholiday=!timeDate::isBizday(timeDate::as.timeDate(DT_ENTRY), holidays =  timeDate::holidayNYSE(1970:2060), wday = 1:5))]
  cdsendpoints <-c(lubridate::ymd('1970-03-20'),lubridate::ymd(paste0(lubridate::year(Sys.Date())+yrs_ahead,'-09-20')))
  u2dts <-data.table::data.table(DT_ENTRY=seq(cdsendpoints[1],cdsendpoints[2], by = '6 month'))[,':='('rolldt'=DT_ENTRY)]
  dtmap <- u2dts[dtmap,on=.(DT_ENTRY)]
  data.table::setnafill(dtmap,"locf",cols=c('rolldt'))
  data.table::setkeyv(dtmap,c("DT_ENTRY"))
  # Business days and end periods
  dtmap <- dtmap[,'isday':=data.table::between(lubridate::wday(DT_ENTRY),2,6)]
  dtmapc <- data.table::copy(dtmap)
  dtmapc <- dtmapc[isday==TRUE,]
  dtmapc <- dtmapc[,'isweek':=(DT_ENTRY==max(DT_ENTRY)),by="yrwk"]
  dtmapc <- dtmapc[,'ismo':=(DT_ENTRY==max(DT_ENTRY)),by="yrmo"]
  dtmapc <- dtmapc[,'isqtr':=(DT_ENTRY==max(DT_ENTRY)),by="yrqtr"]
  dtmapc <- dtmapc[,'isyr':=(DT_ENTRY==max(DT_ENTRY)),by="yr"]
  dtmapc[,'daysfromroll':=.I-min(.I),by='rolldt'][,'rollpd':=format(rolldt,"%Y%m")]
  dtmapc <- dtmapc[,':='('bdoy'=cumsum(isholiday==FALSE)), by=.(yr)]
  # Roll Dates (CDS)
  dtmap <- dtmapc[,c('DT_ENTRY','isweek','ismo','isqtr','isyr','daysfromroll','rollpd','bdoy')][dtmap,on=.(DT_ENTRY)]
  data.table::setnafill(dtmap,"locf",cols=c("daysfromroll"))
  dtmap <- dtmap |> tidyr::fill('rollpd') # tidyr bc of character
  # Option Expirations (Equities)
  moexp <- dtmap[lubridate::wday(DT_ENTRY)==6,][,':='('frino'=.I-min(.I)),by=.(yrmo)][frino==2,][,.(DT_ENTRY,optexp="mo")]
  qexp <- dtmap[isholiday==FALSE & isday==TRUE,][,.SD[.N],by=.(yrqtr)][,.(DT_ENTRY,xoptexp="qtr")]
  dtmap <- moexp[dtmap,on=.(DT_ENTRY)][,':='(optexp=data.table::fcoalesce(optexp,""))]
  dtmap <- qexp[dtmap,on=.(DT_ENTRY)][,':='(optexp=paste0(optexp,data.table::fcoalesce(xoptexp,"")))][,':='(xoptexp=NULL)]
  dtmap <- dtmap[,':='('isweek'=data.table::fcoalesce(isweek,FALSE),'ismo'=data.table::fcoalesce(ismo,FALSE),
                       'isqtr'=data.table::fcoalesce(isqtr,FALSE),'isyr'=data.table::fcoalesce(isyr,FALSE))][]
  return(dtmap)
}
