#' Maintain Aethestics  and Dates of Interest
#'
#' @name fg_dates_of_interest
#' @description
#' `fg_get_dates_of_interest()` gets a set of time events for use in fg time series graphs
#' `fg_update_dates_of_interest()` updates a set of time events for future use in time series graphs
#' @param search_categories Grep string of categories to return.
#' @param use_default (Default TRUE) use dedault dates if none else found.
#' @param startdt Minimum date for events to be returned.
#' @param totoday (Default: FALSE) Ends last date set returned (if applicable) with `totoday` if a date, `Sys.Date()`
#' @param indta `data.table` with columns as shown in details.
#' @param replace (Default: FALSE) If TRUE, replaces existing dates of interest with new set provided, otherwise replaces/inserts new rows only.
#' @returns Filtered datasets
#'
#' @details
#' Retrieves default dates of interest given a grepstring of categories.  There are a default set of categories provided which may not be up to date.
#' New data passed into [fg_update_dates_of_interest()] or [fg_update_aes()] persists across future loads of the package. Any duplicates in the new file will be taken out.
#'
#' New doi `data.frames` must have at least three columns:
#'
#' | Column | Meaning |
#' |:-------|:--------|
#' |`category`| Grouping name (string) for a given set of dates of interest|
#' |`eventid`| Character string to be displayed at each event.|
#' |`DT_ENTRY`| Start Date of event |
#' |`END_DT_ENTRY`| Optional end of period to define regimes or ranges of events.|
#'
#' @seealso [fgts_dygraph()]
#' @examples
#' \dontrun{
#' require(utils)
#' tail(fg_get_dates_of_interest("fedmoves"),2)
#' # To add (for example) a new FOMC cut of 50bps on 6/16/2026:
#' newdoi <-data.table(category="fedmoves",eventid="F:-50",
#'             DT_ENTRY=as.Date("6/16/2026",format="%m/%d/%Y"))
#' fg_update_dates_of_interest(newdoi)
#' # Since this is in the future, we have to make the future now.
#' fg_get_dates_of_interest("fedmoves",totoday=as.Date("2026-12-31"))
#' fg_reset_to_default_state("doi")
#'}
#'

the <- new.env(parent = emptyenv())
the$cachedir <- tools::R_user_dir("FinanceGraphs", which = "cache")
if(!dir.exists(the$cachedir)) {
  newd <- dir.create(the$cachedir)
}

load("./R/sysdata.rda",envir=the)
#  loads tevents_defaults and ratingsmapmelt
the$doifn <- paste0( the$cachedir, "/fg_doi.RD")
the$aesfn <- paste0( the$cachedir, "/fg_aes.RD")
the$themefn <- paste0( the$cachedir, "/fg_theme.RD")
the$doi_dates <-  the$doi_default
the$aesset <- the$aes_default
the$curr_theme <- the$theme_default
the$gpname <- NULL
the$verbose <- FALSE
the$cassign <- FALSE

if(file.exists(the$doifn)) {
  load(the$doifn)
  the$doi_dates <- newdoi
  }
if(file.exists(the$aesfn)) {
  load(the$aesfn)
  the$aesset <- newaes
  }
if(file.exists(the$themefn)) {
  load(the$themefn)
  the$curr_theme <- newTheme
}

.datatable.aware = TRUE

#' @import data.table
#' @rdname fg_dates_of_interest
#' @export
fg_get_dates_of_interest <- function(search_categories="",use_default=TRUE,startdt=NULL,totoday=FALSE) {
  DT_ENTRY<-NULL
  message_if(the$verbose,"fg_get_dates_of_interest(",search_categories,")")
  rtn <- the$doi_dates[grepl(search_categories,the$doi_dates$category,ignore.case=TRUE),][order(DT_ENTRY)]
  enddt <- ifelse(is.logical(totoday), Sys.Date(),lubridate::as_date(totoday))
  if(!is.null(startdt)) {
    rtn <- rtn[END_DT_ENTRY>=as.Date(startdt),]
  }
  rtn <- rtn[DT_ENTRY<=enddt,]
  if(nrow(rtn)>0 & !(totoday==FALSE)) {
    if(!is.na(rtn[.N][["END_DT_ENTRY"]])) {
      newdate <- ifelse(totoday==TRUE, Sys.Date(),lubridate::as_date(totoday))
      rtn[.N,let(END_DT_ENTRY=newdate)]
    }
  }
  return(rtn[])
}

#' @import data.table
#' @rdname fg_dates_of_interest
#' @export
fg_update_dates_of_interest <- function(indta,replace=FALSE) {
  `.`<-category<-variable<-DT_ENTRY<-END_DT_ENTRY<-NULL
  mincolset <- c("category","eventid","DT_ENTRY")
  mincolsmissing <- setdiff(mincolset,names(indta))
  if( length(mincolsmissing)>0 ) {
    stop("fg_create_dates_of_interest: Need to add column(s) ",mincolsmissing)
  }
  indta <- data.table(indta)
  indta <- indta[order(category,DT_ENTRY)]
  if("END_DT_ENTRY" %in% names(indta)) {
    indta <- indta[,':='(END_DT_ENTRY=fcoalesce(END_DT_ENTRY,DT_ENTRY))]
  } else {
    indta <- indta[,':='(END_DT_ENTRY=DT_ENTRY)]
  }
  indta <- indta[,.SD[1],by=.(category,DT_ENTRY,END_DT_ENTRY)]
  indta <- indta[,.SD,.SDcols=intersect(colnames(indta),colnames(the$doi_dates))]
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

# =======================================================================================================
#' Maintain Colors
#'
#' @name fg_update_aes
#' @description
#' `fg_get_aes()` gets aethestic  `data.frame` for use in graphs.
#' `fg_get_aesstring()` takes a column from the `data.frame` retrieved by `fg_get_aes()`
#' `fg_print_aes_list()` prints names of aesthetics used internally in FinanceGraph functions.
#' `fg_update_aes()` updates or replaces default aesthestics (e.g. colors, linestyles, etc).
#' `fg_display_colors()` Shows a plot with current colors.
#' `fg_update_line_colors()` replaces line colors only
#' `fg_reset_to_default_state()` resets colors and/or dates of interest
#' `fg_verbose()` Toggles printing of aesthetics
#'
#' @param item (Default: "") A grep string for categories desired.
#' @param aestype (Default: `NA`) character string with type of aesthetic requested.  If not provided in `[fg_Update_aes()]` the
#' aesthetic associated with `item` is inferred from existing data.
#' @param toget Column in the aes `data.frame` to paste together as a string.
#' @param grepstr narrow list of internal aesthetics sets to functions from `grepstr`
#' @param n_max Maximum number of rows or entries to return.
#' @param asdataframe (Default: FALSE) Return dataframe of parameters regardless of type. (See details)
#' @param indta `data.table` aesthetic `data.fram` with columns as shown in details.
#' @param colorlist List with up to 14 new colors just for line (series) coloring
#' @param replace (Default: FALSE) Replaces existing dates of interest with new set provided, otherwise replaces/inserts new rows only.
#' @param persist (Default: TRUE) Keep changes across invocations of the package.
#' @param reset (Default: "all"), options in ("all","colors","doi") to reset to defaults with the package.
#' @param rtnifnotfound Return `NA_character_` if aes not found
#'
#' @returns `data.frame`, `string` or message
#'
#' @details
#' For colors,
#' New data passed into [fg_update_aes()] persists across future loads of the package unless `persist=FALSE`.
#' New color datasets must have at least three columns:
#' |Column|Meaning|
#' |:-------|:---------------------|
#' |`category`| Arbitrary aestehtic category, e.g. `"lines"` for line colors.
#' |`variable`| Any string that can be sorted or grepped to map to data.
#' |`type`| Aesthetic type, in `c("color","colorrange","linetype","symbol","alpha")`
#' |`value`| String with value detired (e.g a color)|
#'
#' `variable` is used to prioritize colors, so (e.g. `D01` will be the color of the first series in an input dataset)
#'
#' If `aestype=="colorrange"` then a sequential scale of size `n_max` will be returned using details saved from  [fg_update_aes()]. See [scales::brewer_pal]
#' and [colorbrewer](https://colorbrewer2.org/#type=sequential&scheme=Greens&n=7)
#'
#' @seealso [fgts_dygraph()]
#' @examples
#' # Data set, String
#' head(fg_get_aes("lines"),3)
#' fg_get_aesstring("lines")
#' #  Gradient colors are stored in a `data.frame` as in a set of "Blue Greens"
#' fg_get_aes("espath_gp",asdataframe=TRUE)
#' # To get the actual colors, we need to know how many:
#' fg_get_aes("espath_gp",n_max=8)
#' # To add aesthetics or update them, get the original data (or make from scratch) with
#' head(oldcolors <- fg_get_aes("lines"),3)
#' # then change as needed.  For example, to make the second line blue, and the 4th line red,
#' oldcolors[c(2,3),"value"] <- c("blue","red")
#' fg_update_aes( oldcolors )
#' head( fg_get_aes("lines"),3)
#' # to create a new category, make a similar `data.frame`, as in
#' newcolors <- data.frame(category=rep("mylines",3),variable=c("D01","D02","D03"),
#'                 value=c("red","black","green"))
#' fg_update_aes( newcolors, aestype="color")
#' fg_get_aesstring("mylines")
#' require(scales)
#' show_col(fg_get_aesstring("mylines"))
#' fg_reset_to_default_state("aes")
#'
#'
#' @import data.table
#' @import scales
#' @rdname constants
#' @export
fg_get_aes <- function(item="",n_max=NA_integer_,asdataframe=FALSE) {
  if(item=="") { return(the$aesset) }
  message_if(the$verbose,"fg_get_aes(",item,ifelse(is.na(n_max),"",paste0(", n_max=",n_max)),")")
  rtn <- the$aesset[category==item,]
  if(nrow(rtn)<=0) {
    message(paste("fg_get_aes Cannot find (",item,") in aesthetics db"))
    stopifnot(sys.nframe()>0)
    return()
  }
  if(asdataframe==TRUE) {
    return(rtn)
  }
  if(rtn[[1,"type"]]=="colorrange") {
    stopifnot("fg_get_aes(brewer) needs n_max" = !is.na(n_max))
    brewer_dets <- s(rtn[[1,"value"]],sep=",")
    brewer_range <- as.numeric(rtn[[1,"const"]])
    # Need a better hack
    brewer_direction <- sign(brewer_range)
    cols <- scales::pal_brewer(brewer_dets[[1]],palette=brewer_dets[[2]],direction=-brewer_direction)(abs(brewer_range))
    if(brewer_direction>0) {
        colors <- pal_gradient_n(cols)(seq(0, 0.6, length.out = n_max)) }
    else {
        colors <- pal_gradient_n(cols)(seq(0.4, 1, length.out = n_max)) } # Dont want to go all the way to white
    rtn <- data.table(category=item,variable=paste0("C",1:n_max),type="color",value=colors,const=NA_character_)
  }
  else {
    if(!is.na(n_max)) {
      if(n_max>0) {rtn <- rtn[1:min(.N,n_max),] }
    }
  }
  return(rtn)
}

#' @import data.table
#' @rdname constants
#' @export
fg_get_aesstring <- function(item="",n_max=NA_integer_,toget="value",rtnifnotfound=FALSE) {
  if(rtnifnotfound==TRUE & nrow(the$aesset[category==item,])<=0) {
    return(NA_character_)
  }
  else {
    return( fg_get_aes(item,n_max=n_max)[[toget]] )
  }
}

fg_get_aeslist <- function(item="",toget="value") {
  fgtmp <-fg_get_aes(item)
  return( setNames(fgtmp[[toget]],fgtmp$type) )
}


#' @import data.table
#' @rdname constants
#' @export
fg_update_aes <- function(indta,aestype=NA_character_,persist=TRUE,replace=FALSE) {
  category<-variable<-NULL
  mincolset <- c("category","variable","value")
  mincolsmissing <- setdiff(mincolset,names(indta))
  if(length(mincolsmissing)>0) {
    stop("fg_create_colors: Need to have (category,variable,value) at minimmum")
  }
  if( !("type" %in% colnames(indta))) {  # Infer from whats there
    oldaestypes = the$aesset[,.N,by=.(category,type)][indta,on=.(category)]
    indta <- the$aesset[,.N,by=.(category,type)][indta,on=.(category)][,let(N=NULL)]
    if(is.na(aestype) & nrow(indta[is.na(type)])>0) {
      stop(" fg_update_aes.  Cannot infer aesthetic type from either data or input parameter, speciffy `aestype=..`")
    }
    else {
      indta <- indta[,type:=fcoalesce(type,aestype)]
    }
  }
  if(replace==TRUE) {
    newaes <- indta
  } else {
    newaes <- DTUpsert(the$aesset,indta,c("type","category","variable"),fill=TRUE)
  }
  newaes <- newaes[order(type,category,variable)]
  assign("aesset",newaes,envir=the)
  if(persist==TRUE) {
    save(newaes,file=the$aesfn)
    message("Saved updates to ",the$aesfn)
  }
  invisible(newaes)
}

#' @import data.table
#' @rdname constants
#' @export
fg_update_line_colors <- function(colorlist,replace=FALSE,persist=TRUE) {
  ncolors <- length(colorlist)
  if (ncolors>14) {
    message("Only taking first 14 colors...")
    ncolors <- 14
  }
  old_colors <- fg_get_aes("lines",n_max=ncolors)
  old_colors$value = colorlist
  fg_update_aes(old_colors,aestype="color",replace=replace,persist=persist)
  invisible(old_colors)
}

# --- Helpers
#' @import ggplot2
#' @import data.table
#' @rdname constants
#' @export
fg_display_colors <- function(item="") {
  category=variable=color=x=y=ztext=i.DT_ENTRY=i.END_DT_ENTRY=NULL
  tcolors <- fg_get_aes(item, n_max=100)
  tcolors <- tcolors[,let(x=20-(.I %% 20), y=floor(.I/20)+1,ztext=paste0(category,",",variable,":",color))]
  g1 <- ggplot(tcolors,aes(x,y,fill=value,label=ztext))+geom_tile()+geom_label(fill="white",size=3)
  g1 <- g1 +coord_flip()+scale_fill_identity()+labs(title=paste0("Aesthetic Set '",item,"' colors used"))+theme_bw()
  return(g1)
}

#' @rdname constants
#' @import knitr
#' @export
fg_print_aes_list <- function(grepstr="") {
  used=NULL
  grepstr <- paste0(grepstr,"|all")
  rtn <- the$aesset[grepl(grepstr,used),]
  rtn <- rtn[,.(helpstr=.SD[1][["helpstr"]],default=.SD[1][["value"]],N=.N),by=.(used,category)]
  rtn <- rtn[order(used,category)][,used:=NULL]
  return(kable(rtn))
}

#' @rdname constants
#' @export
fg_verbose<- function(item="") {
  the$verbose <- !the$verbose
  if(item=="all") {
    the$cassign <- !the$cassign
  }
}


# =======================================================================================================
#' Group Synchronization
#'
#' @name fg_sync_group
#' @description  Sets, gets, or resets a common name to be passed into  [fgts_dygraph()] for synchronization.
#' @param gpname A string or NULL
#' * `gpname=NULL` turns of dygraphs synchronization.
#' * `gpname=<string>` set the common group to `<string>`
#' * `gpname=""` (Default), just returns the current common group name.
#' @returns current groupname
#' @details
#' Use thie to set a common groupname for time scale synchronization (for Markdown or shiny apps),  Only set it in the beginning,
#' or when needed, and call with NULL to turn synchronization off.
#' @seealso [fgts_dygraph()]
#' @examples
#' fg_sync_group()
#' fg_sync_group("common")
#' fg_sync_group()
#' fg_sync_group(NULL)
#'
#' @export
fg_sync_group <- function(gpname="") {
  if(!is.null(gpname) && gpname=="") { return(the$gpname) }
  the$gpname <- gpname
}

# =======================================================================================================
#' Theme Management
#'
#' @name fg_replace_theme
#' @description  Sets, gets, or resets default ggplot themes to be used.
#' @param newTheme  A new ggplot2 theme
#' @param persist (Default: TRUE) Keep changes across invocations of the package.
#' @returns Acknowledgement message if saved
#' @seealso [fgts_dygraph()], [fg_scatplot()]
#' @examples
#' require(ggplot2)
#' fg_replace_theme(ggplot2::theme_dark(),persist=FALSE)
#' @export
fg_replace_theme <- function(newTheme,persist=TRUE) {
  stopifnot("ggplot2::theme" %in% class(newTheme))
  assign("curr_theme",newTheme,envir=the)
  if(persist==TRUE) {
    save(newTheme,file=the$themefn)
    message("Saved Theme to ",the$themefn)
  }
}

fg_current_theme <- function() {
  return(the$curr_theme)
}

#' @import data.table
#' @rdname constants
#' @export
fg_reset_to_default_state <- function(reset="all") {
  if(reset %in% c("all","doi","dates")) {
    suppressWarnings(file.remove(the$doifn))
    message("Removing dates file and reverting to defaults of package")
    the$doi_dates <- copy(the$doi_default)
  }
  if(reset %in% c("all","aes","color")) {
    suppressWarnings(file.remove(the$aesfn))
    message("Removing Aesthetics file and reverting to defaults of package")
    the$aesset <- copy(the$aes_default)
  }
  if(reset %in% c("all","aes","theme")) {
    suppressWarnings(file.remove(the$themefn))
    message("Removing User-made Themes and reverting to defaults of package")
    the$curr_theme <-fgts_BaseTheme()
    the$theme_default <-fgts_BaseTheme()

  }
  the$tevents_defaults <- copy(tevents_defaults)
  message("fg_reset_to_default_state(",reset,") completed")
}

# =========================================================================
# =========================================================================

# Unexported functions, but still needed with the package
#
#' @import data.table
fg_create_defaults <- function() {
  category <- variable <- NULL
  dtmap  <- make_dtmap(yrs_ahead=10)
  datecols <- c("DT_ENTRY","END_DT_ENTRY")
  doi_default <- fread("./inst/extdata/doidates.csv",na.strings="")[,(datecols):=lapply(.SD,\(x) as.Date(x,"%m/%d/%Y")), .SDcols=datecols][]
  tevents_defaults <- data.table(END_DT_ENTRY=as.Date(NA_real_),eventonly=FALSE,
                                              axis="x",color="#00cc99",strokePattern="dashed",loc="bottom",series=NA_character_)
  ratingsmapmelt <- fread("./inst/extdata/ratingsmapmelt.csv")
  aes_default <- fread("./inst/extdata/fg_aesdefault.csv")[order(type,category,variable)]
  the$aesset <- aes_default
  theme_default <-fgts_BaseTheme()
  #usethis::use_data(doi_default,aes_default,theme_default,dtmap,tevents_defaults,ratingsmapmelt, internal=TRUE,overwrite=TRUE)
}

# ----------------------- Dates
#' @import data.table
.addseasonaldates<- function(x,dtname="DT_ENTRY",toadd="all",freqvarname="") {
  # Really slow
  toaggdt<-function(x,to="yrwk") {
    convstr=list('yrwk'="%Y%V","yrweek"="%Y%V","yrmo"="%Y%m","dt"="%Y%m%d","wk"="%V","filedt"="%y%m%d")
    as.numeric(strftime(x,convstr[[to]])) }
  if(!is.data.frame(x)) {
    if(x=="vars") { return("doy|yr|qtr|doq|yrwk|week") }
    else {  message("addseasonaldates(x,toadd =(all|yr|qtr|doq|ywk|week),dtname)")} }
  if (sum(grepl(dtname,colnames(x)))<=0 ) { xdt=as.Date(rownames(x)) }
  else { xdt=as.Date(x[[dtname]],use.names=FALSE) }
  if(grepl("doy|all",toadd)) { x$doy<-as.numeric(format(xdt,"%j")) }
  if(grepl("^(yr|all)$",toadd)) { x$yr<-as.numeric(format(xdt,"%Y")) }
  if(grepl("doq|all",toadd)) { x$doq<-as.numeric(xdt-as.Date(paste0( as.character(floor( (data.table::month(xdt)-1)/3)*3+1),"/1/",x$yr),"%m/%d/%Y")) }
  if(grepl("(qtr)|yrqt|all|dfagg",toadd)) { x$yrqtr<-as.numeric(format(xdt,"%Y") )*10+as.numeric(substr(quarters(xdt),2,2)) }
  if(grepl("yrwk|all|dfagg",toadd)) { x$yrwk<-toaggdt(xdt) }
  if(grepl("yrmo|all|dfagg",toadd)) { x$yrmo<-toaggdt(xdt,to="yrmo") }
  if(grepl("week|all",toadd)) { x$wk<-toaggdt(xdt,to="wk")  }
  if(nchar(freqvarname)>0) { x[,freqvarname] = x[[toadd]] }
  return(x)
}

# Make datemap, very helpuful for narrowing dates.
#
#' @import data.table
make_dtmap <- function(yrs_ahead=5) {
  # All Dates
  `.`<-DT_ENTRY<-isday<-rolldt<-isholiday<-yr<-yrmo<-frino<-yrqtr<-optexp<-xoptexp<-isweek<-ismo<-isqtr<-isyr<-NULL
  dtmap <- data.table::data.table(DT_ENTRY=seq(from =as.Date("1970-03-20"), to = Sys.Date()+yrs_ahead*365, by = "day")) |> .addseasonaldates()
  dtmap <- dtmap[,':='(isholiday=!timeDate::isBizday(timeDate::as.timeDate(DT_ENTRY), holidays =  timeDate::holidayNYSE(1970:2060), wday = 1:5))]
  cdsendpoints <-c(base::as.Date("1970-03-20"), base::as.Date(paste0(max(dtmap$yr),"-03-20")))
  u2dts <- data.table::data.table(DT_ENTRY=seq(cdsendpoints[1],cdsendpoints[2], by = '6 month'))[,':='('rolldt'=DT_ENTRY)]
  dtmap <- u2dts[dtmap,on=.(DT_ENTRY)]
  data.table::setnafill(dtmap,"locf",cols=c('rolldt'))
  data.table::setkeyv(dtmap,c("DT_ENTRY"))
  # Business days and end periods
  dtmap <- dtmap[,'isday':=data.table::between(data.table::wday(DT_ENTRY),2,6) & !isholiday] # weekdays
  dtmapc <- data.table::copy(dtmap)
  dtmapc <- dtmapc[isday==TRUE,]
  dtmapc <- dtmapc[,'isweek':=(DT_ENTRY==max(DT_ENTRY)),by="yrwk"]
  dtmapc <- dtmapc[,'ismo':=(DT_ENTRY==max(DT_ENTRY)),by="yrmo"]
  dtmapc <- dtmapc[,'isqtr':=(DT_ENTRY==max(DT_ENTRY)),by="yrqtr"]
  dtmapc <- dtmapc[,'isyr':=(DT_ENTRY==max(DT_ENTRY)),by="yr"]
  dtmapc[,'daysfromroll':=.I-min(.I),by='rolldt'][,'rollpd':=format(rolldt,"%Y%m")]
  dtmapc <- dtmapc[,':='('bdoy'=cumsum(isday==TRUE)), by=.(yr)]
  # Roll Dates (CDS)
  dtmap <- dtmapc[,c('DT_ENTRY','isweek','ismo','isqtr','isyr','daysfromroll','rollpd','bdoy')][dtmap,on=.(DT_ENTRY)]
  data.table::setnafill(dtmap,"locf",cols=c("daysfromroll"))
  dtmap <- dtmap |> tidyr::fill('rollpd') # tidyr bc of character
  # Option Expirations (Equities)
  moexp <- dtmap[data.table::wday(DT_ENTRY)==6,][,':='('frino'=.I-min(.I)),by=.(yrmo)][frino==2,][,.(DT_ENTRY,optexp="mo")]
  qexp <- dtmap[isholiday==FALSE & isday==TRUE,][,.SD[.N],by=.(yrqtr)][,.(DT_ENTRY,xoptexp="qtr")]
  dtmap <- moexp[dtmap,on=.(DT_ENTRY)][,':='(optexp=data.table::fcoalesce(optexp,""))]
  dtmap <- qexp[dtmap,on=.(DT_ENTRY)][,':='(optexp=paste0(optexp,data.table::fcoalesce(xoptexp,"")))][,':='(xoptexp=NULL)]
  dtmap <- dtmap[,':='('isweek'=data.table::fcoalesce(isweek,FALSE),'ismo'=data.table::fcoalesce(ismo,FALSE),
                       'isqtr'=data.table::fcoalesce(isqtr,FALSE),'isyr'=data.table::fcoalesce(isyr,FALSE))][]
  return(dtmap)
}


# Original colors:
# mktregimes	+	#FFE6E6
# mktregimes	=	white
# mktregimes	default	white


