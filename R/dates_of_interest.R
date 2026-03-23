#' Maintain Aethestics  and Dates of Interest
#'
#' @name fg_dates_of_interest
#' @rdname fg_dates_of_interest
#' @description
#' `fg_get_dates_of_interest()` gets a set of time events for use in fg time series graphs
#' `fg_update_dates_of_interest()` updates a set of time events for future use in time series graphs
#' @param search_categories Grep string of categories to return.
#' @param use_default (Default TRUE) use dedault dates if none else found.
#' @param startdt Minimum date for events to be returned.
#' @param totoday (Default: FALSE) Ends last date set returned (if applicable) with `totoday` if a date, `Sys.Date()`
#' @param indta `data.table` with columns as shown in details.
#' @param replace (Default: FALSE) If TRUE, replaces existing dates of interest with new set provided, otherwise replaces/inserts new rows only.
#' @returns [data.table::data.table()] of date or date ranges, or `NULL``if new dates are added.
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
#' require(utils)
#' require(data.table)
#' tail(fg_get_dates_of_interest("fedmoves"),2)
#' # To add (for example) a new FOMC cut of 50bps on 6/16/2026:
#' newdoi <-data.table(category="fedmoves",eventid="F:-50",
#'             DT_ENTRY=as.Date("6/16/2026",format="%m/%d/%Y"))
#' fg_update_dates_of_interest(newdoi)
#' # Since this is in the future, we have to make the future now.
#' fg_get_dates_of_interest("fedmoves",totoday=as.Date("2026-12-31"))
#' fg_reset_to_default_state("doi")
#' fg_reset_to_default_state("all")
#'
#' @import data.table
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
  if(!dir.exists(the$cachedir)) {
    newd <- dir.create(the$cachedir)
  }
  save(newdoi,file=the$doifn)
  assign("doi_dates",newdoi,envir=the)
  assign("doi_dates_update",Sys.Date(),envir=the)
  message("Saved dates of interest file to ",the$doifn)
  return(NULL)
}
