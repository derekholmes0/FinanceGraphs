#' Event Helpers : fg_addbreakouts
#'
#' @title Event_Helpers
#' @name fg_addbreakouts
#' @description
#' `fg_get_dates_of_interest()` gets a set of time events for use in fg time series graphs
#' `fg_update_dates_of_interest()` updates a set of time events for future use in time series graphs
#' `fg_get_colors()` gets default color sets for graphs
#' `fg_update_colors()` updates or replaces default colors
#'

#' @param indta Time series `data.table` with a date as the first column and a value series as the second column.
#' @param annotationstyle String in set (`singleasdate`,`singleasvalue`,'breakno')
#'
#' @returns `data.table` suitable for passing into [fgts_dygraph()] via the `eventdataset` parameter
#'
#' @examples
#' dta <- eqtypx[,.(date,QQQ,TLT)]
#' fgts_dygraph(dta,event_ds=fg_addbreakouts(dta),title="With Breakouts")
#'
#' @import data.table
#' @import knitr
#' @export
fg_addbreakouts<-function(indta,annotationstyle="singleasdate") {
  indt2 <- stats::na.omit(as.numeric(indta[[2]]))
  res <- BreakoutDetection::breakout(indt2, min.size=24, method='multi', degree=1, beta=0.005, plot=FALSE)
  bo_only <- indta[res$loc,]
  thiscolor <- fg_get_colorstring("breakout")
  tortn <- data.frame(DT_ENRY=bo_only[[1]],text="Bad Anotation Style",loc="top",color=thiscolor)
  rtntxt <- switch(annotationstyle,
                   "singleasdate" = paste0("Brk:",lapply(bo_only[[1]],as.character)),
                   "breakno"      = paste0("Brk:",seq(1,nrow(res$loc))),
                   "singleasvalue"= paste0("Brk:",format(bo_only[[2]],digits=3))
  )
  tortn$text <- rtntxt
  return(data.table(tortn))
}

#' Event Helpers : fg_findTurningPoints
#'
#' @name fg_findTurningPoints
#' @param indta Time series `data.table` with a date as the first column and a value series as the second column.
#' @param rtn string with what to return ('dates','data','all')
#' @param method string in 'pctchg' (default) or 'cpm' [cpm-package].  'pctchg' looks for up to 'npts' dates with the largest positive and negative changes over threshold 'pctabovemin'
#' @param npts Number of change points to find
#' @param pts_of_interest string in 'change' (default) or 'value'
#' @param pctabovemin Minimum percentage change to look for.
#' @param maxwindow  Integer (default -1) which limits (if positive) the minimum number of observations between change points.
#' @param cpmmethod String (default: "GLM") passed to [cpm::processStream()]
#' @param addlast Logical (default: FALSE) to add an event with final observation.
#' @param ... Additional parameters passed to [cpm-package]
#'
#' @returns `data.table` suitable for passing into [fgts_dygraph()] via the `eventdataset` parameter
#'
#' @examples
#' dta <-eqtypx[,.(date,QQQ,TLT)]
#' fgts_dygraph(dta,event_ds=fg_findTurningPoints(dta),title="With turningPoints")
#'
#' @import data.table
#' @export
fg_findTurningPoints<-function(indta,rtn="dates",
                            method="pctchg",npts=10,pts_of_interest="change",pctabovemin=0.05,maxwindow=-1,addlast=FALSE,cpmmethod="GLR",...) {
  `.`<-DT_ENTRY<-value<-daysfrommin<-goodpt<-ino<-text<-color<-loc<-pctchg_func<-NULL
  pts <- data.table()
  v1a <- copy(indta)[,c(1,2)]
  setnames(v1a,c("DT_ENTRY","value"))
  v1a  <- v1a[,let(DT_ENTRY=as.numeric(DT_ENTRY),goodpt=1,daysfrommin=0,origvalue=value,value=value-min(value,na.rm=T))]
  tcolors <- fg_get_colorstring("turningpoints")
  if(method=="pctchg") { # Not perfect, on values
    pctchg_func <- function(pts,v1a,dir,npts,maxwindow,pctabovemin) {
      u1a <- copy(v1a);
      for( i in 1:ceiling(npts/2)) {
        thiswindow <- maxwindow
        if(maxwindow<0) { thiswindow <- nrow(indta)/(10*(i/2)**0.7) }
        u2 <- u1a[order(dir*value)][,.SD[1]][,let(dir=dir,ino=i)]
        pts <- DTappend(pts,u2)
        u1a <- u1a[,let(daysfrommin=abs(DT_ENTRY-u2[[1,"DT_ENTRY"]]))][order(daysfrommin)]
        u1a$goodpt <- fifelse(u1a$daysfrommin>thiswindow,1,0)
        u1a$goodpt <- fifelse(u1a$goodpt==1 & (dir*u1a$value/u2$value<dir*(1+dir*pctabovemin)),0,u1a$goodpt)
        u1a <- u1a[goodpt==1,]
        if(nrow(u1a)<=0) { break; }
      }
      return(pts)
    }
    pts <- pctchg_func(pts,v1a,+1,npts,maxwindow,pctabovemin)
    pts <- pctchg_func(pts,v1a,-1,npts,maxwindow,pctabovemin)
    pts <- pts[,let(text=paste0("TP_",ifelse(dir==1,"BOT","TOP"),"_",ino),color=ifelse(dir==1,tcolors[[2]],tcolors[[1]]),loc="bottom")]
  }
  if(grepl("cpm",method)) { # Needs to be on returns
    message("fg_findTurningPoints: Only non-missing values used for cpm")
    indta <- stats::na.omit(indta)
    cpts <- cpm::processStream(indta[[2]],cpmmethod,startup=floor(nrow(indta)/20),...)
    pts_toget <- ifelse(pts_of_interest=="detect","detectionTimes","changePoints")
    pts <- indta[cpts[[pts_toget]]][,1]
    setnames(pts,c("DT_ENTRY"))
    pts <- pts[.(DT_ENTRY,text="CP",color=tcolors[[2]],loc="bottom")]
  }
  if(addlast & !any(grepl("CURR",pts$text))) { 0
    pts <- rbindlist(list(pts,indta[DT_ENTRY==max(DT_ENTRY),][,.(DT_ENTRY,text="CURR",color="black",loc="bottom")]),fill=TRUE)
  }
  if(rtn=="dates") {  tortn <- pts[,.(DT_ENTRY,text,color,loc)] }
  else if(rtn=="data") { tortn  <- pts[,.SD,.SDcols=!s("value;origvalue")][indta,on=.(DT_ENTRY)][order(DT_ENTRY)] }
  else if(rtn=="all") { tortn  <- pts[order(DT_ENTRY)] }
  tortn$DT_ENTRY=as.Date(tortn$DT_ENTRY)
  return(tortn)
}


#' Event Helpers : fg_ratingsEvents
#'
#' @name fg_ratingsEvents
#' @param credit String with name of credit to look up in 'ratings_db'
#' @param ratings_db A 'data.table' or 'data.frame' with the all of the following columns:
#' | column | type | description |
#' |: --- |: --- :|: --- |
#' | CREDIT | character | Name of credit  |
#' | AGENCY | character | Name of ratings agency  |
#' | RATING   | character | Rating assigned  |
#' | WATCH   | character | Watch denoted by anything with "+" or "-" in the string |
#' | DT_ENTRY  | Date | Date which ratings or ratings change was issued |

#' @param agency String (default 'S.P') with 'AGENCY to look up in 'ratings_db'
#'
#' @returns `data.table` suitable for passing into [fgts_dygraph()] via the `eventdataset` parameter
#'
#' @examples
#' fg_ratingsEvents("COLOM",ratings_db,agency="S.P")
#'
#' @import data.table
#' @rdname Event_Helpers
#' @export
fg_ratingsEvents<-function(credit,ratings_db,agency="S.P") { # CERDIT,AGENCY,RATINGS,DT_ENTRY
  tmp <- lapply(s("CREDIT;AGENCY;WATCH;WATCHNUM;NUMRAT;DT_ENTRY;RATING;END_DT_ENTRY;color"), \(x) assign(x,NULL,pos=1) )
  CREDIT <- AGENCY <- WATCH <- WATCHNUM <- NUMRAT <- DT_ENTRY <- RATING <- END_DT_ENTRY <- color <- NULL
  trats = ratings_db |> dplyr::filter(CREDIT==credit & AGENCY==agency)
  trats = trats |> dplyr::left_join(ratingsmapmelt,by=c("AGENCY",c("RATING"="RATCHAR"))) |> dplyr::mutate(WATCH= stringr::str_extract(WATCH,"(\\-|\\+)"))
  trats = trats |>  dplyr::left_join(dplyr::tibble(WATCH=s("+;-"),WATCHNUM=c(-0.5,+0.5)), by="WATCH") |>
    dplyr::mutate(WATCHNUM=dplyr::coalesce(WATCHNUM,0)) |>
    dplyr::mutate(NUMRAT12=12*(NUMRAT+WATCHNUM)) |> dplyr::arrange(CREDIT,AGENCY,DT_ENTRY)
  trats = trats |> dplyr::group_by(CREDIT,AGENCY) |> dplyr::mutate(END_DT_ENTRY=dplyr::lead(DT_ENTRY,1,default=Sys.Date()))
  ratingscolors = rbindlist(list(
    data.frame(NUMRAT12=12*8+seq(0,3*12,1),color=grDevices::colorRampPalette(c("#ffffff","#6161ff"),alpha=0.4)(37)),
    data.frame(NUMRAT12=12*11+seq(1,5*12,1),color=grDevices::colorRampPalette(c("#f56462","#ffffff"),alpha=0.4)(60))     ))
  trats <- trats |> dplyr::left_join(ratingscolors,by="NUMRAT12") |> dplyr::ungroup()
  tdates <- trats |> dplyr::transmute(category="ratings",text=RATING,DT_ENTRY,END_DT_ENTRY,color,loc="bottom")
  return(tdates)
}


#' Event Helpers: overlay_eventset
#'
#' @name overlay_eventset
#' @param indta Time series `data.table` with a date as the first column and a value series as the second column.
#' @param ncutsperside : Integer with number of colors to use on each side of 'center'
#' @param center : String or Double as follows:
#' * Double (default 0) Normalize data by subtracting `center`
#' * "median" Normalize data by subtracting median of all observations.
#' * "zscore" Normalize data by using standard [scale()] function
#'
#' @param invert Use opposite color schemes for data, i.e. "red" for good outcomes
#'
#' @returns `data.table` suitable for passing into [fgts_dygraph()] via the `eventdataset` parameter
#'
#' @examples
#' overlay_eventset(consumer_sent,center="zscore")
#'
#' @details
#' Always uses first date column and first numeric columns in data.  If `indta` has multiple series, filter them before calling the function.
#'
#' @import data.table
#' @export
overlay_eventset<-function(indta,ncutsperside=4,center=0,invert=FALSE) {
  `.` <- value <- tmpcat <- DT_ENTRY <- END_DT_ENTRY <- NULL
  dt_colname <- find_col_bytype(indta,lubridate::is.Date)
  val_colname <- find_col_bytype(indta,is.numeric)
  tmpdta <- data.table(indta)[,.(DT_ENTRY=get(dt_colname), value=get(val_colname))]
  tcolors <- fg_get_colorstring("eventset")
  xcenter<-0
  if(is.numeric(center)) { xcenter <-center }
  else if (center=="median") { xcenter <- stats::median(tmpdta[["value"]]) }
  else if (center=="zscore") { tmpdta[["value"]] <- as.vector(scale(tmpdta[["value"]])) }
  else {
    stop("overlay_eventset: dont know how to deal with ",center)
  }
  sgn_mult <- ifelse(invert==TRUE,-1,1)
  tmpp <- tmpdta[value>xcenter,][,tmpcat := sgn_mult*as.numeric(ggplot2::cut_interval(value,ncutsperside))]
  tmpn <- tmpdta[value<=xcenter,][,tmpcat := -1*sgn_mult*( as.numeric(ggplot2::cut_interval(-value,ncutsperside)))]
  tmpall <- DTappend(tmpn,tmpp)[order(DT_ENTRY)]
  colorset <- rbind( data.frame(value=seq(1,ncutsperside),color=grDevices::colorRampPalette(c("#ffffff",tcolors[[2]]),alpha=TRUE)(ncutsperside)),
                    data.frame(value=seq(-ncutsperside,-1),color=grDevices::colorRampPalette(c(tcolors[[1]],"#ffffff"),alpha=TRUE)(ncutsperside)))
  colorset <- data.table(colorset)
  tmpruns = colorset[tmpall[,runs_from_value(.SD[,.(DT_ENTRY,value=tmpcat)],addrunlength=TRUE)],on=.(value)][,END_DT_ENTRY:=END_DT_ENTRY+1][]
  return(tmpruns)
}

