# =======================================================================================================
#' Maintain Colors
#'
#' @name fg_update_aes
#' @rdname set_constants
#' @description
#' `fg_update_aes()` updates or replaces default aesthestics (e.g. colors, linestyles, etc).
#' `fg_update_line_colors()` replaces line colors only
#' `fg_reset_to_default_state()` resets colors and/or dates of interest
#' `fg_replace_theme()` Replaces default theme used in static plots
#' `fg_verbose()` Toggles printing of aesthetics
#'
#' @param indta `data.table` aesthetic `data.fram` with columns as shown in details.
#' @param aestype (Default: `NA`) character string with type of aesthetic requested.  If not provided in `[fg_Update_aes()]` the
#' @param colorlist List with up to 14 new colors just for line (series) coloring
#' @param newTheme  A new ggplot2 theme
#' @param persist (Default: TRUE) Keep changes across invocations of the package.
#' @param replace (Default: FALSE) Replaces existing dates of interest with new set provided, otherwise replaces/inserts new rows only.
#' @param item (Default: "") A grep string for categories desired.
#' @param reset (Default: "all"), options in ("all","colors","doi") to reset to defaults with the package.
#' @returns No return value, as these are called for the side effects of adding to or replacing aesthetic sets.
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
#' @seealso [fgts_dygraph()], [fg_scatplot()], [fg_get_aes()]
#' @examples
#' # Data set, String
#' head(oldcolors <- fg_get_aes("lines"),3)
#' # then change as needed.  For example, to make the second line blue, and the 4th line red,
#' oldcolors[c(2,3),"value"] <- c("blue","tomato")
#' fg_update_aes( oldcolors )
#' head( fg_get_aes("lines"),3)
#' # to create a new category, make a similar `data.frame`, as in
#' newcolors <- data.frame(category=rep("mylines",3),variable=c("D01","D02","D03"),
#'                 value=c("red","black","green"))
#' fg_update_aes( newcolors, aestype="color")
#' fg_get_aesstring("mylines")
#' #Theme replacement
#' require(ggplot2)
#' fg_replace_theme(ggplot2::theme_dark(),persist=FALSE)
#' fg_reset_to_default_state("all")
#'
#' @import data.table
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
    if(!dir.exists(the$cachedir)) {
      newd <- dir.create(the$cachedir)
    }
    save(newaes,file=the$aesfn)
    message("Saved aesthetic updates to ",the$aesfn)
  }
  invisible(newaes)
}

#' @import data.table
#' @rdname set_constants
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

#' @rdname set_constants
#' @export
fg_replace_theme <- function(newTheme,persist=TRUE) {
  stopifnot("ggplot2::theme" %in% class(newTheme))
  assign("curr_theme",newTheme,envir=the)
  if(persist==TRUE) {
    if(!dir.exists(the$cachedir)) {
      newd <- dir.create(the$cachedir)
    }
    save(newTheme,file=the$themefn)
    message("Saved Default Theme to ",the$themefn)
  }
}

#' @rdname set_constants
#' @export
fg_verbose<- function(item="") {
  the$verbose <- !the$verbose
  if(item=="all") {
    the$cassign <- !the$cassign
  }
}

#' @import data.table
#' @rdname set_constants
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
  if(reset %in% c("all")) {
    unlink(the$cachedir, force=TRUE,recursive=TRUE)
    message("Removing cache Directory")
  }
  message("fg_reset_to_default_state(",reset,") completed")
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
#' UNexported helpers
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

fg_setdbg <- function() {
  assign("cassign",TRUE,envir=the)
  assign("verbose",TRUE,envir=the)
}


