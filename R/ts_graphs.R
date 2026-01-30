#' TIme series in Dygraph form
#'
#' @name fgts_dygraph
#'
#' @param indt Input data in long or wide format.  THere must be at least one date column, one
#' character column and one numeric column.  Ideal format is `date,variable,value`
#' @param title Title to put on top of graph
#' @param ylab Label for y axis
#' @param roller Initial moving average value to smooth graphs.  (See [dygraphs::dyRoller()])  Options are
#'  * `default` (Default) chose a smoothing parameter consistent with the length of the input series
#'  * `finest` No smoothing
#'  * integer >= 0 : User specified moving average length.
#' @param pointers  Pointer options to use with mouse movement. (See [dygraphs::dyCrosshair()])
#' @param splitcols,stepcols,hidecols  String or list of data series to show on a second y axis, to be shown as step plots, or to be hidden.
#' Can also be `TRUE` in which case first series in the data is affected.
#' @param hilightcols String or list of data series to plot in different style than other series.
#' @param hilightwidth (Default: 2) relative width of series specified in `hilightcols`
#' @param hilightstyle (Default: solid).  Line style of series specified in `hilightcols`.
#' Options are (`solid`,`dashed`,`dotted`,`dotdash`)
#' @param events String with possible events to add to graph.  Options can be added together
#' with `;` and include
#' * `doi,eventsetname`  : Events in internal event list `eventsetname` from list maintained by [fg_update_dates_of_interest()].
#' * `seasonal,type` : Regularly spaced intervals of `type`.  For options, see details.
#' * `minmax` : Locations of highest and lowest observations per series.
#' * `dt,text,d1,<d2>` : Text events starting at `d1` and possibly ending at `<d2>`,both
#'      of the form `yyyy-mm-dd`.
#' * `break,labelform` : Breakouts as determined by [fg_addbreakouts()] with `labelform` in ("singleasdate","singleavalue","breakno")
#' * `tp,n` : Turning points as determined by [fg_findTurningPoints()]
#' @param event_ds `data.frame` of events to be added to graph.  See details and
#' examples for specification.
#' @param annotations string with annotations on individual series or along `y` axes.  Options can be added
#' together with `;`  and can include
#' * `lastvalue`  : Value of latest observation for each series, placed at the end of the series
#' * `lastlabel`  : Name of each series, placed at the end of the series.
#' * `hline,y : Horizontal line at `y`
#' * `range,ybeg,yend` : Band placed between `ybeg` and `yend`
#' @param annotation_ds `data.frame` of annotations added to graph. See details for specification.
#' @param forecast_ds `data.frame` of forecasts to be displayed after the end of those in `indt`. Those typically are in wide format, with at minimum
#' a (first) date column and series names of the form `series.f``, `series.flo`` and/or `series.fhi``, where `series` is one of the
#' plotted series in `indt`
#' @param ylimits  Two number vector of lower and upper limits of data to be displayed. Alternatively,
#' a string of the form `<seriesnm>,<q>` will limit displayed data to the (q,1-q) quantiles of `seriesnm`
#' @param dtstartfrac Fraction in (0,1] of dates in `indt` to start the range selector.
#' See [dygraphs::dyRangeSelector()]
#' @param dtwindow String to specify date ranges applied [dygraphs::dyRangeSelector()] of the
#' form `begin::end` where either end can take the form "yyyy-mm-dd" or a relative date to the other end of the
#' series, e.g `-3m` or `-2w`.  Example: `"-3m::-1m"` defines a 2 month period 1 month back from the end of the series.
#' @param rebase String of the form `yyyy-mm-dd,<value>` with `<value>` assumed 100 if not specified.  This normalizes all series to `<value>`
#' as of the given date.  See examples.
#' @param exportevents String of name of `data.frame` to create in  `.GlobalEnv` with event dates displayed on graph.
#' @param meltvar (Default: `variable`) Column name in `indt` with series names, if melted.
#' @param dylegend (Default: TRUE) include legend in graph
#' @param groupnm  (Default: `common`)  Group name used in `shiny` or `RMarkdown` to synchonize graphs
#' @param fillGraph (Default: FALSE) Shade area underneath each series.
#' @param verbose (Default: FALSE) Print extra details about what will be graphed.
#' @param extraoptions Additional options passed to [dygraphs::dyOptions()]
#'
#' @import data.table
#'
#' @returns Dygraph [dygraphs](https://rstudio.github.io/dygraphs/) of input data, with annotations and other customizations.
#'
#' @details
#'  Input data can either be in wide ('date' ,'series1',...) format or normalized (long) format
#' ('date','variable','value') format.  This package infers date columns names from column types and seeks to be as agnostic
#' as possible as to column names.
#' Colors can be managed using [fg_update_colors()] and will persist across R sessions,
#' Series are grouped together into bands around a series `series` if their names end as in 'series.lo' or 'series.hi'.  See examples and vignette for details.
#'
#' **Events** are dates and date ranges to be highlighted in the graph.   Days of interest (doi) can be added
#' using [fg_update_dates_of_interest()] which will persist across R sessions.  See examples and vignette for further details.
#' Events can also be added using a `data.frame` passed via `event_ds` with the following columns:
#'
#' | column | type | description |
#' |:---|:---:|:---|
#' | `date` | Date | (Required) Start date |
#' | `date_end` | Date |End date to specify range of a colored band |
#' | `text` | character | (Required) Text to display |
#' | `color` | character | Color for line and text |
#' | `eventonly` | logical | Only draw line for for start of event, no band |
#' | `strokePattern` |  character | `dashed` (default), one of ('solid','dashed','dotted','dotdash') |
#' | `loc` |  character | 'bottom' (default), one of ('top','bottom') |
#' | `series` |  character | Name of series to apply event to, if needed |
#' | `category` | character | Optional string used for exceptions. See notes below.  |
#'
#' Many times, events depend on outside data or statistical analysis on the original data.  The `event_ds` to be passed
#' in can come from event helpers in [fg_cut_to_events()], [fg_addbreakouts()], [fg_findTurningPoints()], or  [fg_ratingsEvents()].
#' EVent columns are processed as is, with one current exception
#' * `category=="series_color"` then `color` is replaced by the color of the series currently in the `color` column.
#'    Tis allows annotations to be same color as the series to which they apply.
#'
#' **Annotiations** include any notes or highlights added to the graph on the 'y' axis or on an individual series.  In addition to those passed
#' via the `annotations` parameter, annotations can be added using a `data.frame` with the following columns:

#' | column | type | description |
#' |:---|:---:|:---|
#' | `date` | Date | (Required) Start date |
#' | `date_end` | Date |End date to specify range of a colored band |
#' | `text` | character | (Required) Text to display |
#' | `color` | character | Color for line and text |
#' | `eventonly`  | logical | Only draw line for for start of event, no band |
#'
#' @examples
#' # See Vignette for more extensive examples.
#' # Basic Example
#' fgts_dygraph(eqtypx, title="Stock Prices", ylab="Adjusted Close")
#'
#' # With series Highlights, finer resolution and focused date range
#' fgts_dygraph(eqtypx, dtstartfrac=0.8,hilightcols="IBM",hilightwidth=4,roller=3)
#'
#' # Rebasing to 1/1/2022
#' fgts_dygraph(eqtypx, title="Rebased Prices", ylab="Adjusted Close",rebase="2022-01-01")
#'
#' # Using bands (.lo, .hi)
#' toplot <- reerdta[REGION=="LATAM",.(cop=sum(value*(IMFCC=="COL")),
#'               cop.lo=min(value),cop.hi=max(value)),by=.(date)]
#' fgts_dygraph(toplot,title="COP REER vs Latam peers",roller=3)
#'
#' # Events Examples.  Notice how roller shortens with the series.
#' # See Vignette for more extensive examples
#' require(data.table)
#' smalldta <- narrowbydtstr(eqtypx[,.(date,TLT,EEM)],"-3y::")
#' fgts_dygraph(smalldta,events="doi,regm;doi,fedmoves")
#' fgts_dygraph(smalldta,events="date,FOMO,2025-01-01,2025-06-01;date,xmas,2025-12-25")
#'
#' # Events passed in as data.frames
#' myevents = data.frame(end_date =as.Date(c("2024-03-10","2024-01-10")),
#'             date=as.Date(c("2024-01-10","2024-04-10")),
#'             text=c("range","event"),color=c("green","red"))
#' fgts_dygraph(smalldta,events="doi,fedmoves",event_ds=myevents)
#'
#' # Annotations on y axis
#' fgts_dygraph(eqtypx,annotations="last,linevalue")
#' fgts_dygraph(eqtypx,annotations="hline,100,at100,red;hline,200,at200;range,300,400")
#'
#' # use with helpers
#' require(data.table)
#' smalldta <- narrowbydtstr(eqtypx[,.(date,IBM,QQQ)],"-2y::")
#' fgts_dygraph(smalldta,title="W TurnPts",event_ds=fg_findTurningPoints(smalldta[,.(date,QQQ)]))
#' fgts_dygraph(smalldta,title="W Sentiment",event_ds=fg_cut_to_events(consumer_sent,center="zscore"))
#' fgts_dygraph(smalldta,title="W dividends",event_ds=fg_tq_divs(c("IBM","QQQ")))
#'
#' # Other helpers for use with credit ratings, breakouts, and earnings data are available.
#'
#' # use with forecasts
#'
#' require(forecast)
#' smalldta <- narrowbydtstr(eqtypx[,.(date,IBM,QQQ)],"-2y::")
#' fcst_one <- function(ticker) {
#'   t1_ts <- zoo::zoo(smalldta[[ticker]],smalldta[["date"]])
#'   forecast::ets(t1_ts) |> forecast::forecast(h=36) |>  fg_predict(seriesnm=ticker)
#'   }
#' fpred <- merge(fcst_one("QQQ"),fcst_one("IBM"),by="date")
#' fgts_dygraph(smalldta,title="With Forecasts", dtstartfrac=0.7,forecast_ds=fpred)
#'
#' @export
#'
fgts_dygraph<-function(indt,title="",ylab="",roller="default",pointers="hair,both",
                        splitcols=FALSE,stepcols=FALSE,hidecols=FALSE,
                        hilightcols=FALSE,hilightwidth=2,hilightstyle="solid",
                        events="",event_ds=NULL,
                        annotations="",annotation_ds=NULL,
                        forecast_ds=NULL,
                        ylimits=NULL,dtstartfrac=0,dtwindow="",rebase="",
                        exportevents=NULL, meltvar="variable",dylegend="always",groupnm="common",
                        fillGraph=FALSE,verbose=FALSE,
                        extraoptions=list()) {

  # NSE crap.  There has to be a better way
  `.`=gpnm=suffix=seriesnm=display=color=axis=series_no=variable=eventid=direct=tcolor=optexp=DT_ENTRY=NULL
  value=a2=a3=labelloc=a1=text=END_DT_ENTRY=category = NULL

  # Preprocessing: get into data.table format
  if( xts::is.xts(indt) ) { indt <- xts2df(indt) }
  if(dplyr::is.tbl(indt)) { indt <- dplyr::ungroup(indt) }
  if(!is.data.table(indt)) { indt <- data.table(indt) }  # Try setDT ?

  # Local helpers
  fcoal <- function(indta,...) { fcoalesce(indta,...) }
  form_xlist <- function(instring) { todo <- NULL
    if(is.data.frame(instring)) return(instring)
    suppressWarnings(tibble::tibble(todo=s(instring)) |>
                                tidyr::separate_wider_delim(todo,",",names= c("todo","a1","a2","a3","a4","a5"),
                                too_many="drop",too_few="align_start"))
  }
  get_fromlist <- function(indta,grepstr) { todo<-NULL;
    dplyr::filter(indta,grepl(grepstr,todo)) }
  add_titles <- function(what,...) {
    style="small";
    stylednote = paste0("<",style,">",paste(...),"</",style,">")
    titleadds <<- DTappend(titleadds,data.table(axis=what,note=stylednote))  }

  # Wrangle original input
  # Figure out date name and place first
  dt_colnames <- list()
  dt_colnames['date'] <- find_col_bytype(indt,lubridate::is.Date)
  dt_colnames['value'] <- find_col_bytype(indt,is.numeric)
  dt_colnames['meltvar'] <- meltvar
  if(is.na(dt_colnames['date'])) {
    stop("fgts_dygraph must have a date column")
  }

  setcolorder(indt, dt_colnames[['date']])

  # Misc date stuff
  col_date_list <- c(dt_colnames[['date']])
  alldts <- sort(unique(indt[[1]]))
  dtlimits <- range(alldts)
  dtsrange_todisplay <- c(alldts[length(alldts)*dtstartfrac+1], max(alldts))
  if(nchar(dtwindow)>1) dtsrange_todisplay <- gendtstr(dtwindow,rtn="list")

  wasmelted <- meltvar %in% colnames(indt)
  # make indtnew is WIDE FORMAT, indt can be either
  if(wasmelted) {
    lastoset <- indt[,.SD[.N],by=meltvar]
    lastobs  <- as.vector(lastoset$value)
    lastlabs <- as.vector(lastoset[[meltvar]])
    form1 <- paste0(dt_colnames[["date"]],"~ factor(", dt_colnames[["meltvar"]],", levels=unique(",dt_colnames[["meltvar"]],"))")
    indtnew<- dcast(indt,eval(form1),value.var=dt_colnames[["value"]])
  }
  else {
    lastobs <- as.vector(indt[nrow(indt),])[2:ncol(indt)]
    lastlabs <- colnames(indt)[2:ncol(indt)]
    indtnew <- indt
  }

  titleadds <- data.table()
  tevents <-data.table()
  elist <- form_xlist(events)
  alist <- form_xlist(annotations)

  # Add new columns used as highlights
  # Add new series (e.g.) filters as necessary  For Future, make this a piped function
  # to add from previous code: lmregine, sig, var, stregime

    # Need to group together col.lo and .hi for color an style
    all_series_names <- colnames(indtnew)[-1]
    series_dets <- data.table( seriesnm=all_series_names, gpnm=gsub("(.lo|.hi)","",all_series_names), display=TRUE)

    gps_series <- unique(series_dets[grepl("(lo|hi)$",series_dets$seriesnm),]$gpnm)
    if(length(gps_series)>0) {
      gps_tofillin <- CJ(gpnm=gps_series,suffix=c("lo","hi"))
      gps_tofillin <- gps_tofillin[,.(gpnm,seriesnm=paste0(gpnm,".",suffix))]
      gps_tofillin <- series_dets[gps_tofillin,on=.(seriesnm,gpnm)][is.na(display)]
      # Copy old data to make sure both .lo and .hi exist; work with the downstream packages
      indtnew <-  indtnew[,(gps_tofillin$seriesnm):=.SD, .SDcols= gps_tofillin$gpnm]
    }

    series_dets <- data.table( seriesnm=colnames(indtnew)[-1], gpnm=gsub("(.lo|.hi)","",colnames(indtnew)[-1]),
                                           axis='y',stepplot=FALSE,display=TRUE,width=1,style="solid")
    curr_colors <- fg_get_colorstring("lines")
    series_dets <- series_dets[,':='(color=curr_colors[.GRP]),by=.(gpnm)]

    # Style setup
    if(!(hidecols[1]==FALSE)) {
      t_colnos <- match(hidecols,series_dets$gpnm,nomatch=0) # Can match more than one
      series_dets[t_colnos]$display <- FALSE
    }

    if(!(splitcols[1]==FALSE)) {
      t_colnos <- match(splitcols,series_dets$gpnm,nomatch=1) # Can match more than one
      series_dets[t_colnos]$axis <- "y2"
    }

    if(!(stepcols[1]==FALSE)) {
      t_colnos <- match(stepcols,series_dets$gpnm,nomatch=0) # Can match more than one
      if(t_colnos[1]==0) { t_colnos<- seq(1,nrow(series_dets)) }
      series_dets[t_colnos]$stepplot <- TRUE
    }

    if(!(hilightcols[1]==FALSE)) {
      t_colnos <- match(hilightcols,series_dets$seriesnm,nomatch=0) # Can match more than one
      series_dets[t_colnos]$width <- hilightwidth
      series_dets[t_colnos]$style <- hilightstyle
    }

    # Now forecasts
    if(is.data.frame(forecast_ds)) {
        dt_colnames['fdate'] <- find_col_bytype(forecast_ds,lubridate::is.Date)
        fcst_series <- rbindlist(lapply(colnames(forecast_ds),
                                          \(x) { y=s(x,sep="."); data.frame(gpnm=y[1],seriesnm=x)}))
        sdets_base <- series_dets[gpnm==seriesnm, .SD,.SDcols=!c("seriesnm")]
        fcst_dets <- sdets_base[fcst_series,on=.(gpnm)][,let(style="dashed")][!is.na(color)] # No date
        fcst_dets <- fcst_dets[,let(gpnm=gsub("(lo|hi)$","",seriesnm))]
        series_dets <- DTappend(series_dets,fcst_dets)
        indtnew = merge(indtnew,forecast_ds,by.x=dt_colnames[['date']], by.y=dt_colnames[['fdate']],all=TRUE)
        alldts <- indtnew[[1]]
        dtsrange_todisplay <- c(alldts[length(alldts)*dtstartfrac+1], max(alldts))
    }
    # Rebase if desired
    if( nchar(rebase)>0 && length( rebtmp <- s(rebase,sep=",") ) <=2 ) { # Overengineering
       rebdate <- fcoal(lubridate::as_date(rebtmp[1]),dtlimits[1])
       rebval <- ifelse(length(rebtmp)==1 & lubridate::is.Date(rebdate),100,as.numeric(utils::tail(rebtmp,1)))
       rebloc <- max(which(indtnew[[1]]<=rebdate))
       indtnew <- indtnew[,names(.SD):=lapply(.SD,\(x) rebval*x/x[[rebloc]]),.SDcols=!c(1)]
       add_titles("x","Rebased data to ",rebval," as of ",rebdate)
       tevents <- DTappend(tevents,data.table(DT_ENTRY=as.Date(rebdate),text="",color=fg_get_colorstring("rebase"),strokePattern="solid"))
       message_if(verbose,"fgts_dygraph: Rebased data to ",rebval," as of ",format(rebdate,"%m/%d/%Y"))
    }

   # Set display ranges.  Focus in if dtstartfrac is specified
    yRange<-NULL
    if(length(ylimits)==2) {
        yRange <-ylimits
    }
    else if (is.character(ylimits)) { # Find wuantiles on given series
      if( length( ylimsplit<-s(ylimits,","))==2 ) {
        qlimit<-c(as.numeric(ylimsplit[2]),0.01)[1]
        yRange <- stats::quantile(indtnew[[ylimsplit[1]]],c(qlimit,1-qlimit),na.rm=T) |> as.numeric()
        message_if(verbose,"fgts_dygraph: Displayed range limited to ",qlimit," quantiles on ",ylimsplit[1], " or ",yRange[1], ":",yRange[2])
        add_titles("title","(Winsored@",qlimit,")")
      }
    }
    else if (dtstartfrac>0 | nchar(dtwindow)>1) {  # FOcus y axis if x is
      focusdta <- narrowbydtstr(indtnew,paste0(dtsrange_todisplay,collapse="::"))
      yrangetmp <-  range(focusdta[,2:ncol(focusdta)])
      yRange <- c(yrangetmp[1]*0.8,yrangetmp[2]*1.05)
    }

    if(verbose) {
      print(series_dets)
    }
    # Only way to take a series out is to take the data out.
    indtnew <- indtnew[,.SD,.SDcols=!(series_dets[display==FALSE,]$seriesnm)]

    add_titles("y",ylab)
    alltitles = paste0(title,paste0(titleadds[axis=="title"]$note,collapse=","))
    ## cAssign("indtnew;alltitles;series_dets;dt_colnames")
    g1 <- dygraphs::dygraph(indtnew,main=alltitles,group=groupnm)
    for(seriesgp in sort(unique(series_dets[display==TRUE,]$gpnm))) {
        trw <- series_dets[get("gpnm")==seriesgp,]
        serset <- seriesgp
        if(nrow(trw)>1) {
          serset <- trw[,.(seriesnm,series_no=fcase(grepl("lo$",seriesnm),1,grepl("hi$",seriesnm),3,default=2))]
          serset <- serset[data.table(series_no=c(1,2,3)),on=.(series_no)][,seriesnm:=fcoal(seriesnm,seriesgp)]
          serset <- serset[order(series_no)]$seriesnm
        }
        trw <- trw[gpnm==seriesnm,]
        ## message(">>         group: ",seriesgp," Series Set: ",paste(serset,sep=","))
        g1 = g1 |> dygraphs::dySeries(serset,color=trw[1,]$color,axis=trw[1,]$axis,stepPlot=trw[1,]$stepplot,strokeWidth=trw[1,]$width,
                        strokePattern=trw[1,]$style,fillGraph=FALSE)
    }

    g1 <- g1 |> dygraphs::dyLegend(width=600, show="always",hideOnMouseOut = FALSE)
    g1 <- g1 |> dygraphs::dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.4,
                                      highlightSeriesOpts = list(strokeWidth = 3))
    g1 <- g1 |> dygraphs::dyOptions(labelsKMB=TRUE,rightGap=2,fillAlpha=0.2,fillGraph=fillGraph,axisLabelFontSize=10)

# EVents on x axis

    # Statistical data turning points (dyEvent)
    # ======================================================================================
    # event Types:
    # in { [doi,startof|]doicategory ; break ; [tp,method,npts,maxwindow] ; data_frame  }
    # ======================================================================================
    #  Events: df (DT_ENTRY,END_DT_ENTRY,text,loc,color,strokePattern)
    # Dates of interest in memory.
    if(nrow( trow<-get_fromlist(elist,"doi") )>0 ) {
      dirbars <- fg_get_colors("mktregimes")[,.(direct=variable,tcolor=color)]
      for(irow in seq(1,nrow(trow))) {
          thiseventstr<- fcoal(trow[irow,]$a1, "")
          eventonly   <- grepl("startof",thiseventstr,ignore.case=T)
          thiseventstr<- tolower(gsub("startof","",thiseventstr))
          tdates <- fg_get_dates_of_interest(thiseventstr,startdt=min(indtnew[[1]]))
          if(nrow(tdates)>0) {
              tdates1 <- tdates[, let(direct=stringr::str_sub(eventid,-1), rno=.I)]
              tdates1 <- dirbars[tdates1,on=.(direct)][,let(color=fcoal(color,tcolor))]
              tdates1 <- coalesce_DT_byentry(tdates1,data.table(color="gray70",loc="bottom",strokePattern="dashed"))
              tdates1 <- tdates1[,let(text=eventid)][,.SD,.SDcols=!c("tcolor","direct","eventid","eventid2")]
              tevents <- DTappend(tevents,tdates1)
          }
      }
      add_titles("x","Shaded Events:",paste(trow[["a1"]],collapse=" "))
    }

    # Dates; from dtmap (roll,optexp,doy,doq) e.g. "seasonal,optexp,mo"
    if(nrow( trow<-get_fromlist(elist,"seasonal") )>0) {
      dttmp <- dtmap[between(get("DT_ENTRY"),dtlimits[1],dtlimits[2]),]
      for(irow in seq(1,nrow(trow))) {
        eventtype <- tolower(trow[irow,]$a1)
        if(eventtype=="optexp") {
          opttype <- trow[irow,][["a2"]]
          dt_oi <- dttmp[grepl(opttype,optexp),][,.(DT_ENTRY,text=opttype)]
        }
        if(eventtype=="rolldates") {
          dt_oi <- dttmp[,.SD[1],by=.(rollpd),][,.(DT_ENTRY,text="cdsroll")]
        }
        if(eventtype %in% c("doy","doq","bdoy","roll")) {
            valoi <- dttmp[,.SD[.N]][[eventtype]]
            dt_oi <- dttmp[get(eventtype)==valoi,][,.(DT_ENTRY,text=eventtype)]
        }
        dt_oi <- dt_oi[,':='(color=fg_get_colorstring(eventtype),loc="top")]
        tevents <- DTappend(tevents,dt_oi)
      }
    }
    if(nrow( trow<-get_fromlist(elist,"(minmax|extremes)") )>0) {
      dt_melt <- indt
      if(!wasmelted) {
          dt_melt <- melt(indtnew,id.vars= dt_colnames[['date']]) }
      color_dt <- series_dets[,.SD[1],by=.(gpnm)][,.(variable=gpnm,color)]
      minevents <- dt_melt[,.SD[which.min(value)],by=.(variable)][,c(1,2)][,let(text=paste("min",variable),loc="bottom")]
      maxevents <- dt_melt[,.SD[which.max(value)],by=.(variable)][,c(1,2)][,let(text=paste("max",variable),loc="top")]
      allevents <- rbindlist(list(minevents,maxevents))
      allevents <- color_dt[allevents,on=.(variable)]
      setnames(allevents,dt_colnames[['date']],"DT_ENTRY")
      tevents <- DTappend(tevents,allevents)
    }
    if(nrow( trow<-get_fromlist(elist,"^(dt|date)")  )>0) {  # date,name,dtstart,dtend
      trow <- trow |> dplyr::mutate(a2=lubridate::as_date(a2),a3=lubridate::as_date(a3))
      for(irow in seq(1,nrow(trow))) {
        this_nm <- fcoal(trow[[irow,"a1"]],"event")
        start_dt <- trow[[irow,"a2"]]
        if( lubridate::is.Date(start_dt) ) {
            end_dt <- fcoal(trow[[irow,"a3"]],start_dt)
            colornm <- fg_get_colorstring(fifelse(end_dt>start_dt,"date_range","date"))
            newevent <- data.table(DT_ENTRY=start_dt,END_DT_ENTRY=end_dt,text=this_nm,loc="bottom",color=colornm)
            tevents <- DTappend(tevents,newevent)
         }
      }
    }
    # "sig,<variable>,<level> colors variable according to whether <variable> is between (-inf,-sig,+sig,Inf) : taken out: Needs runs_from_value

    # Statistical things ==================================

      # Breakouts: Done
      if(nrow( trow<-get_fromlist(elist,"break") )>0) {
        bodates <- fg_addbreakouts(indtnew, annotationstyle=fcoal(trow[1,]$a1,"singleasdate")) # Renames first col to DT_ENTRY
        bodates <- bodates[,let(color= fg_get_colorstring("breakout"),loc="top")]
        tevents <- DTappend(tevents,bodates)
      }
        # "tp,nn" adds turning points as dotted vertical lines
      if(nrow( trow<-get_fromlist(elist,"^(tp)") )>0) {
        tmpmsg <- paste0(trow$a1, "turning Points on ", colnames(indt)[1])
        tp <- fg_findTurningPoints(indtnew[,c(1,2)],npts=as.numeric(fcoal(trow$a1,"5")),rtn="dates",maxwindow=as.numeric(fcoal(trow$a2,"-1")),method=fcoal(trow$a3,"pctchg"))
        message_if(verbose,"FInding Turning Points, w ",nrow(tp), "rows")
        tevents <- DTappend(tevents,tp[,let(category="tp", END_DT_ENTRY=DT_ENTRY)])
    }

    # Events: df (DT_ENTRY,END_DT_ENTRY,text,loc,color,strokePattern)
    ## cAssign("indt;series_dets")
    if(is.data.frame(event_ds)) {
        # Rename columns smartly, only first two date columns taken
        event_ds <- data.table(event_ds)[between(get("DT_ENTRY"),dtlimits[1],dtlimits[2]),]
        dtcols <- utils::head(find_col_bytype(event_ds,lubridate::is.Date,firstonly=FALSE),2)
        dtcolnewnames <- sapply( dtcols, \(x) ifelse(grepl("end",x,ignore.case=TRUE),"END_DT_ENTRY","DT_ENTRY"))
        setnames(event_ds,dtcols,dtcolnewnames)
        if("category" %in% colnames(event_ds)) {  # Need to document
            event_to_map <- event_ds[category=="series_color",let(gpnm=color)]
            event_to_map <- series_dets[,.(gpnm,tcolor=color)][event_to_map,on=.(gpnm)][,let(color=fcoal(tcolor,color))]
            event_ds <- event_to_map[,let(tcolor=NULL)]
        }
        tevents <- DTappend(tevents,event_ds)
    }

    if(is.character(exportevents)) {
      assign(exportevents,tevents,envir=.GlobalEnv)
      message_if(verbose,"Copied events as ",exportevents," to Global Namespace")
    }

    # last,<label> ; last,value ; last,line ; hline, no; range lo,hi
    # Add horizontal annotations
    if(nrow( trow<-get_fromlist(alist,"^(last)")  )>0) { # last or last,line, can only specify one
      labelstr <- fcoal(trow$a1,"label") |> tolower() # label,value,labelline,valueline
      labeltype <- ifelse(grepl("value",labelstr),"value","gpnm")
      labelcat <- ifelse(grepl("line",labelstr),"line","anno")
      lastvals <- melt(indtnew[.N],id.vars=dt_colnames[['date']], variable.name = "gpnm")[,let(value=as.character(round(value,1)))]
      h_annos <- lastvals[series_dets[gpnm==seriesnm,],on=.(gpnm)]
      h_annos <- h_annos[,.(category=labelcat,color,text=get(labeltype),seriesnm=gpnm,axis,value,loc=labelloc,DT_ENTRY=get(dt_colnames[['date']]))]
      tevents <- DTappend(tevents,h_annos)
    }

    if(nrow( trow<-get_fromlist(alist,"^(hline)"))>0) { #hline,no
        thiscolor <-  fg_get_colorstring("hline")
        h_annos <- data.table(trow)[,let(text=fcoal(a2,""),value=as.numeric(a1),color=fcoal(a3,thiscolor))]
        h_annos <- h_annos[,.(category="hline",color,text,value,DT_ENTRY=dtlimits[2],axis="y")]
        tevents <- DTappend(tevents,h_annos)
    }

    if(nrow( trow<-get_fromlist(alist,"^(range)"))>0) { #range,lo,hi,<color>
      thiscolor <-  fg_get_colorstring("range")
      h_annos <- data.table(trow)[,':='(a1=as.numeric(a1),a2=as.numeric(fcoal(a2,a1)))]
      h_annos <- h_annos[,.(category="range",color=fcoal(a3,thiscolor),text="",value=pmin(a1,a2),value_2=pmax(a1,a2),axis="y",DT_ENTRY=dtlimits[2])]
      tevents <- DTappend(tevents,h_annos)
    }

    if (is.data.frame(annotation_ds)) { # date,series,text
      ds_signature <- sapply(annotation_ds,class)[1:3] == c("Date","character","character")
      annotation_ds <- data.table(annotation_ds)
      if( all(ds_signature)==TRUE ) {
        setnames(annotation_ds,c("DT_ENTRY","seriesnm","text"))
        h_annos <- annotation_ds[,.(category="anno",DT_ENTRY,text,seriesnm,axis="y")]
        tevents <- DTappend(tevents,h_annos)
      } else {
        message("fgts_dygraph problem: annotation_ds no in format (date,series,text)")
      }
    }

    if(nrow(tevents)>0) {
       tevents <- coalesce_DT_byentry(tevents,the$tevents_defaults)
       tevents <- tevents[, let(END_DT_ENTRY=as.Date(fcoal(as.integer(END_DT_ENTRY),as.integer(DT_ENTRY))))]  # What is NA?
       for(irow in 1:nrow(tevents)) {
        trw<-tevents[irow,]
        if(trw$axis=="x") {
          if(trw$eventonly | (trw$END_DT_ENTRY-trw$DT_ENTRY)<=3) {
            g1 = g1 |> dygraphs::dyEvent(trw$DT_ENTRY,trw$text,labelLoc=trw$loc,color=trw$color,strokePattern=trw$strokePattern)  }
          else {
            g1 = g1 |> dygraphs::dyShading(from=trw$DT_ENTRY, to=trw$END_DT_ENTRY, color=trw$color) }
        }
        if(trw$axis=="y" | trw$axis=="y2") {
          if("value_2" %in% colnames(trw) && !is.na(trw$value_2)) {
            g1 = g1 |> dygraphs::dyShading(from=trw$value, to=trw$value_2, color=trw$color, axis=trw$axis) }
          else if (trw$category %in% c("anno","last")) {
            g1 = g1 |> dygraphs::dyAnnotation(x=trw$DT_ENTRY,text=trw$text,series=trw$seriesnm,width=6*nchar(trw$text)) }  # MOre options to explore
          else {
            g1 = g1 |> dygraphs::dyLimit(limit=trw$value,label=trw$text,labelLoc="right",color=trw$color,strokePattern="dashed") }
        }
       }
    }

    # Rollers
    suggest_rollpd <- c(1L,5L,10L,20L)[findInterval(as.numeric(diff(range(indtnew[[1]]))),c(1,360,2520,3600,+Inf))]
    rollpd <- suppressWarnings(fcase(
      roller=="default", suggest_rollpd,
      roller=="finest", 1L,
      is.numeric(roller), as.integer(roller),
      default=NA_integer_ ))

    if(!is.na(rollpd)) {
      g1 = g1 |> dygraphs::dyRoller(rollPeriod=rollpd) }

    # Legends
    if(nchar(dylegend)>0)  {
      g1= g1 |>  dygraphs::dyLegend(width=600, show=dylegend,hideOnMouseOut = FALSE)  }

    # Axes
    g1 <- g1 |> dygraphs::dyAxis('y',valueRange=yRange,label=paste0(titleadds[axis=="y"]$note,collapse="<br>"))
    g1 <- g1 |> dygraphs::dyAxis("x",paste0(titleadds[axis=="x"]$note,collapse="<br>"))

    if( nrow(y2dta <- series_dets[axis=="y2",])>0 ) {
      g1 = g1 |> dygraphs::dyAxis('y2',independentTicks=TRUE, drawGrid = FALSE, label=paste(y2dta$seriesnm,collapse=","),
                                  axisLineColor=y2dta[1,]$color, axisLabelColor = y2dta[1,]$color)
    }

    # Errata
    if(!is.logical(hairopts <- optString_parse(pointers,"cross|hair"))) {
      g1 <- g1 |> dygraphs::dyCrosshair(direction = hairopts)
    } # horizontal,both,veretical

    if(!(optString_parse(pointers,"norange")=="TRUE")) {
      g1 = g1 |> dygraphs::dyRangeSelector(height=20,dateWindow=dtsrange_todisplay)
    }

    return(g1)
}






