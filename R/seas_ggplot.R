#' Seasonality Graphs
#' @name fg_seasonalstudy
#' @title Flexible Seasonality Graphs
#' @description Flexible and general seasonality graphs.
#' @param indta A data.frame with at least one date column and a numeric column with the name given by `yvar`
#' @param seasonaltype (default: `"yr"`) A string denoting the periodicity of the analysis, must be in one of
#' `c("yr","qtr","mo","wk",IMMroll","optmo","optqtr")`.  Not used if `seasonaldataset` is specified instead
#' @param seasonaldateset (default:NULL) An optional dataset with two columns: A Date column defining each period, and possibly a character
#' variable with a period identifier.
#' @param day_offset (default: 0) Number of days (determined by bdaysonly) to offset each period.  Applies only to `c("yr","qtr","mo","wk")`
#' @param bdaysonly (default:TRUE)  Only consider NYSE business days.
#' @param graphtype (default: "flex")  One of the following
#' |`graphtype`|Description|
#' |:-------:|:---------------------|
#' |`line`|A line for every period, with identifiers placed near the end|
#' |`hex`|A density plot for each day of the seasonal period, with the last seasonal  period  kept as a line|
#' |`flex`|A line graph if there are less than `n_hex_switch` seasonal periodss|
#' |`stat`|A line graph showing 10tyh, 50th, and 90th percentiles of values for each day in a seasonal period|
#' @param normalize (default "").  How to normalize each period.  Default is no normalization.  Other options are
#' |`normalize`|Description|
#' |:-------:|:---------------------|
#' |`relative`|Older seasonal periods adjusted to match beginning of latest  period|
#' |`index`|Each seasonal period is expressed as index from beginning of each period|
#' @param projectfwd (default "")  Project forward based on same number of days in seaonal period.  Default is no projection.
#' |`normalize`|Description|
#' |:-------:|:---------------------|
#' |`mean`|Use mean (by days in seasonal period) of each cumulative percentage change from the start of the period|
#' |`weighted`|Weight previous period cumulative percantge change with an expoential decay using `projfwd_wt` ** (Periods back)|
#' @param projfwd_wt See above
#' @param yvar (default:`"value"`) Series to use in `indta`
#' @param title (default: NULL) Title for graph.
#' @param yrange (default: NULL) y axis Range for which to focus data.
#' @param n_color_switch (default 7) Number of periods past which lines will be colored by a descending scale.
#' @param n_hex_switch (default 20) Number of periods past which a `hex` graph will be used, if `flex` is chosen above.
#' @param line_aes_set (default `"lines"`) Aes color set for discrete lines.
#' @param line_on_lastdate (default TRUE) Add a vertical line at the last observations day in period.
#' @param killbad_eop (default FALSE):  DO not show periods for which there are at least 60% of the mean number of observations
#' per day of the seasonal period.  If used, this helps to curtail  extreme moves at the end of a period.  For example,
#' this would redact the 366th day of the year.
#' @param return_dates (default: FALSE)  Return `list(graph,dates)` instead of just the graph.
#' @examples
#' require(data.table)
#' assetcols <- c("EEM","IBM","QQQ","TLT")
#' eqtyidx<-eqtyrtn[,(assetcols):=lapply(.SD,\(x) 100*(exp(cumsum(fcoalesce(x,0))))),
#'             .SDcols=assetcols]
#' fg_seasonalstudy(eqtyidx,yvar="TLT",seasonaltype="yr",normalize="index",projectfwd="mean")
#' # Earnings seasons
#' earnings_dates <- earnings_ibm[,.(reportedDate,divdt=format(reportedDate,"%Y%m%d"))]
#' fg_seasonalstudy(eqtyidx,yvar="IBM",seasonaldateset = earnings_dates,graphtype="stat",
#'    normalize="index",projectfwd="mean")
#' @returns a [ggplot()] object displaying seasonality
#' @import data.table
#' @importFrom stats weighted.mean
#' @export
fg_seasonalstudy <-function(indta,
                            seasonaltype="yr", seasonaldateset=NULL,day_offset=0,bdaysonly=TRUE,
                            graphtype="flex",normalize="",projectfwd="",projfwd_wt=0.9,
                            yvar="value",title=NULL,yrange=NULL,
                            n_color_switch=7,n_hex_switch=20,
                            line_aes_set="lines",
                            line_on_lastdate=TRUE,killbad_eop=FALSE,return_dates=FALSE) {
  ishol_nyse=isbday=ctr=inttype=optexp=rollpd=fg_value=islastpd=rollsback=daysfromroll=N=frtn=baseval=ww=NULL
  indta=copy(data.table(indta))
  dtcolname <- find_col_bytype(indta,lubridate::is.instant)
  setnames(indta,  dtcolname, "DT_ENTRY")
  setnames(indta,  yvar, "fg_value")
  tdtmap <- dtmap[between(DT_ENTRY,min(indta$DT_ENTRY),max(indta$DT_ENTRY)),]
  if(bdaysonly) {
    tdtmap = tdtmap[ishol_nyse==FALSE & isbday==TRUE,]
  }
  ts_dates <- projfwd <- data.table()
  captionlab <- ""
  seasonal_map <- data.table(inttype=s("yr;qtr;wk;mo;immroll;optmo;optqtr"),dtmapvar=s("yr;yrqtr;yrwk;yrmo;rollpd;yrmo;yrqtr"),
                             dtmaptype=c(rep("direct",5),rep("opt",2)))
  if(is.data.frame(seasonaldateset) && nrow(seasonaldateset)>0) {
    seasonaldateset <- as.data.table(seasonaldateset)
    idname <- find_col_bytype(seasonaldateset,is.character) %||% "ctr"
    if(is.null( dtname <- find_col_bytype(seasonaldateset,lubridate::is.instant) )) {
      message("fg_seasonalStudy: No dates in seasonaldateset, returning nothing")
      return()
    }
    if(!(idname=="ctr") && length(unique(seasonaldateset[[idname]]))<nrow(seasonaldateset)) {
      message("fg_seasonalStudy: Identifiers for each event must be unique, returning nothing")
      return()
    }
    ts_dates <- seasonaldateset[between(get(dtname),min(indta$DT_ENTRY),max(indta$DT_ENTRY)),]
    ts_dates <- ts_dates[,ctr:=paste0("e",.I)][,.SD,.SDcols=c(dtname,idname)]
    seasonaltype <- ""
    setnames(ts_dates,c("DT_ENTRY","rollpd"))
  }
  else {
    if( nrow(tseas<-seasonal_map[inttype==tolower(seasonaltype),])>0 ) {
      if (tseas[1,]$dtmaptype=="direct") {
        ts_dates <- tdtmap[,.SD[day_offset+1],by=c(tseas[1,]$dtmapvar)][,.(DT_ENTRY,rollpd=as.character(get(tseas[1,]$dtmapvar)))]
      }
      if (tseas[1,]$dtmaptype=="opt") {
        toptexp <- sub("opt","",seasonaltype)
        ts_dates <- tdtmap[optexp==toptexp,][,.(DT_ENTRY,rollpd=paste0(toptexp,"_",as.character(get(tseas[1,]$dtmapvar))))]
      }
    }
  }
  nrollpds <- length(unique(ts_dates$rollpd))
  # Argh: DT 1.18.99 will deal with character directly.. just not released yet. Meantime, a workaround
  # xdtmap <- ts_dates[tdtmap[,.(DT_ENTRY,ishol_nyse)],on=.(DT_ENTRY)]
  # setnafill(xdtmap,type="locf")
  # ------------------------------------
  tsdates <- ts_dates[,ii:=.I]
  xdtmap <- tsdates[,.SD,.SDcols=!c("rollpd")][tdtmap[,.(DT_ENTRY,ishol_nyse,isbday)],on=.(DT_ENTRY)]
  setnafill(xdtmap,type="locf",cols=c("ii"))
  xdtmap <- tsdates[,.(ii,rollpd)][xdtmap,on=.(ii)][,ii:=NULL]
  dttmp=xdtmap[,`:=`(daysfromroll=.I-.I[which.min(DT_ENTRY)],rolldt=min(DT_ENTRY)),by=.(rollpd)]
  indta = dttmp[indta,on="DT_ENTRY",nomatch=NULL]

  if( length(unique(indta$rollpd))<=1 ) {
    message("PlotOneSeasonal not enough data")
    return(NULL) }
  lastindex = indta[rollpd==max(rollpd),.SD[1]]$fg_value
  if(normalize=="relative") {
    indta = indta[,fg_value:=(fg_value-first(fg_value)), by=.(rollpd)]
    captionlab = "Older cycles adjusted to match beginning of latest cycle"
  }
  if(normalize=="index") {
    indta = indta[,fg_value:=100*(fg_value/first(fg_value)), by=.(rollpd)]
    captionlab = "Expressed as index from beginning of each period"
  }
  ulast    = indta[,.SD[.N],by=.(rollpd)][,':='(rollsback=.N-.I+1)][,islastpd:=fifelse(rollsback==1,"LAST","--")]
  ulastdfr = ulast[,.SD[.N]][["daysfromroll"]]
  ulastval = ulast[,.SD[.N]]$fg_value
  indta = ulast[,.(rollpd,rollsback,islastpd)][indta, on=.(rollpd)]
  if(killbad_eop) {
    baddays = indta[,.N,by=.(daysfromroll)][N<0.6*mean(N),]
    indta = indta[!baddays[,.(daysfromroll)],on=.(daysfromroll)]
    message("Eliminating ",baddays$daysfromroll, "days from Roll leaving", nrow(indta), "rows")
  }
  lastpdreturn=indta[islastpd=="LAST",.(pdrtn=last(fg_value)-first(fg_value))]$pdrtn
  if( (pfwdcode <- tolower(projectfwd)) %in% c("mean","weighted")) {
    fwdidx <- indta[daysfromroll==ulastdfr,][,.(rollpd,baseval=fg_value)]
    avgfwd <- indta[rollsback>1 & daysfromroll>ulastdfr,]
    avgfwd <- suppressWarnings(fwdidx[avgfwd,on=.(rollpd)][,frtn:=log(fg_value)-log(baseval)][!is.na(frtn)][,ww:=1/.N,by=.(daysfromroll)])
    if(pfwdcode=="weighted") { avgfwd <- avgfwd[,ww:=projfwd_wt^(seq(nrow(.SD)-1,0)), by=.(daysfromroll)] }
    projfwd <- avgfwd[,.(wapctrtn=weighted.mean(frtn,ww), fg_value=ulastval*exp( weighted.mean(frtn,ww))), by=.(daysfromroll)]
  }
  tcolors <- fg_get_aesstring(ifelse(nrollpds>n_color_switch,"espath_gp",line_aes_set),n_max=nrollpds)
  graphtype <- fifelse(graphtype=="flex", fifelse(nrollpds<n_hex_switch,"line","hex"),graphtype)
  sectitle <-  fifelse(nchar(seasonaltype)>0,paste(yvar," by ",seasonaltype),yvar)
  if(grepl("stat|aggregate",graphtype)) {
    qtiles1 = indta[rollsback>1,][,lapply(c(0.1,0.5,0.9), \(x) quantile(.SD$fg_value,x,na.rm=T)), by=.(daysfromroll)]
    setnames(qtiles1,s("daysfromroll;p10;p50;p90"))
    qtiles2 = melt(qtiles1,"daysfromroll",variable.name="rollpd",value.name="fg_value")
    ulast <- ulast[islastpd=="LAST",]
    indta <- rbindlist(list(indta[rollsback==1,],qtiles2[,rollsback:=2]),use.names=TRUE,fill=TRUE)
    graphtype = "line"
    nrollpds <- 4
    tcolors <- fg_get_aesstring("seas_qt_color",n_max=4)
    captionlab = paste(captionlab,"10,50,90th percentiles shown")
  }
  if(length(yrange)==2) { indta = indta[between(fg_value,yrange[[1]],yrange[[2]]),] }
  indta = indta[,':='(islastpd=fcoalesce(islastpd,"-"))]
  label_bg <- fg_get_aesstring("seas_label_bg")
  label_fg <- fg_get_aesstring("seas_label_fg")
  label_size <- fifelse(nrollpds>15,3,5) # Prob shoudl parametersize
  if(graphtype=="line") {
    g1=ggplot(indta,aes(x=daysfromroll,y=fg_value,color=rollpd,linewidth=islastpd))+geom_line(aes(group=rollpd),show.legend=FALSE)
    g1=g1+scale_discrete_manual("linewidth",values=as.numeric(fg_get_aes("seas_linewidth")$value))
    labeldt <- ulast
  }
  if(grepl("hex",graphtype)) {
    nbins = as.numeric(c(s(graphtype),30)[[2]])
    g1=ggplot(indta,aes(x=daysfromroll,y=fg_value))+geom_line(aes(group=rollpd),linewidth=2,data=indta[rollsback==1,])
    g1=g1+geom_hex(data=indta[rollsback>1,],alpha=0.7,bins=nbins)+scale_fill_gradient(low="gray70",high="gray10")
    labeldt <- ulast[rollsback==1]
  }
  if(nrow(projfwd)>0) {
    g1=g1+geom_line(aes(x=daysfromroll,y=fg_value),data=projfwd, color=tcolors[1], linewidth=1.5,linetype=fg_get_aesstring("seas_projlt"))
  }
  g1=g1+suppressWarnings(geom_label_repel(aes(x=daysfromroll,y=fg_value,label=rollpd),data=labeldt,linewidth=0,
                         fill=label_bg,color=label_fg,size=label_size,max.overlaps=50))
  g1=g1+guides(linewidth="none")+labs(x="Days from Beg of Period",y=yvar)
  g1=g1+scale_color_manual(values=rev(tcolors)) + scale_x_continuous(n.breaks=20)
  if(line_on_lastdate) {
    g1=g1+gline_y(int=ulastdfr)
  }
  g1 <- g1 + labs(caption=captionlab, title=title %||% sectitle) + fgts_BaseTheme()
  if(return_dates==TRUE) {
    setnames(ts_dates,  "DT_ENTRY",dtcolname)
    return(list(g1,ts_dates[,.SD,.SDcols=!("ii")]))
  }
  else {
    return(g1)
  }
}
