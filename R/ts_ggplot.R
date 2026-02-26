#' fg_tsboxplot:  Boxplots of time series
#' @name fg_tsboxplot
#' @description
#' Plots static summaries of time series in boxplot form.

#' @usage
#' fg_tsboxplot(indt,title="",xlab="",ylab="",
#'      breaks=c(7,30,90,360), doi="last", normalize="", orderby="",
#'      boxtype= "",
#'      dropset="",hilightcats="",
#'      addline="", #last/mean
#'      facetform="",
#'      ycoord=NULL,trimpctile=0,
#'      legend="insidetop",meltvar="variable",flip=FALSE,ptsize=3)
#'
#' @param indt  Input data.frame with at least one date variable and one or more vategorical variables, if melted.
#' @param title,xlab,ylab  TItles and Labels
#' @param breaks A list or text as follows
#'  * `<doiset>` : A dates of interest category, see [fg_get_dates_of_interest()]
#'  * list of integers: A list of days for which to go back in time, e.g. `c(7,30,360)` creates intervals for the last week, 1 week to 1 month, etc.
#'  * list of reals in `[0,1]` fractions of the dates in each category, e.g `c(0.2,1)` creates intevals with the last 20pct of dates, and any older.
#' @param doi Points or segments to overlay with latest observations, or changes since a particular date.
#' * `"last"` (Default)  Last date as a dot
#' * `"last,<d1>"` Segment from date `d1` to last date in input data.
#' * `"last,n"` Segment from `n`th date from the end to the end.
#' * `"date,<d1>"` Levels as of date `d1`
#' * `"none"`  No points or segments.
#' @param normalize Normalize data in some way prior to plotting.  Choices are
#' * `"byhistcat"`Transform data into percentiles within each variable and historical category
#' * `"byvar","zbyvar"`Transform data into percentiles (or z-scores) within each variable and historical category
#' @param orderby (Default `""`) Underlying categories are by default ordered as in `indt`, unless
#' * `"value","-value"` : Order by last value in series for each category or descending if `"-value"`
#' * `"date,<d1>","-date,<d1>"` : Order by value (or decreasing value) at date `<d1>`
#' * `"alpha","-alpha"` Order alphabetically in ascending or descnding orer.
#' @param boxtype Formatting of boxplots.  If in `"violin`,`"viobycat"` make a violin plot, otherwise show a full boxplot (with outliers turned off by default), with any aspects in `c("nostaple","nomedian","nobox")` taken out.
#' @param dropset  String or list with underlying categories to drop from graph
#' @param hilightcats String or list of underlying categories to highlight with differnt color in label.
#' @param addline in `c("mean","last")` Add a horizontal line across the mean of all observations or a smooth like across last observations
#' @param facetform (Default: "") Any faceting formula which includes text or factor columns in `indt`. See examples and note that facets can also be added using [ggplot2::facet_grid()] to the output graph.
#' @param ycoord (Default NULL) a two element list with limits on y corrdinates
#' @param trimpctile (Default 0) trims data before any plotting to fall within `c(trimpctile,1-trimpctile)` percentiles within each variable.
#' @param legend (Default `"insidetop"`) Where to put the legend
#' @param meltvar (Default: `"variable"` Name of variable with unit category.
#' @param flip (Default `FALSE`) If `TRUE` then categories are arranged vertically
#' @param ptsize (Default: 3) Size of points for `doi` parmeter
#'
#' @returns A [ggplot2::ggplot()] object
#'
#' @examples
#' fg_tsboxplot(eqtypx,breaks=c(7,30,360),normalize="byvar",hilightcats="QQQ",
#'        title="Equity prices, within ranges")
#' fg_tsboxplot(narrowbydtstr(eqtypx,"-2y::"),breaks="regm",normalize="byvar",
#'         hilightcats="QQQ",title="Equity prices, in regimes")
#' fg_tsboxplot(reerdta,breaks=c(0,0.2,0.5,1),doi="last",orderby="value",
#'         boxtype="nowhisker",facetform=". ~ REGION",title="Real Eff. Exch Rates")
#' fg_tsboxplot(reerdta,breaks=c(0,0.2,0.5,1),doi="last",orderby="value",
#'         addline="last",boxtype="violin",title="Real Eff. Exch Rates (Violin)")
#'
#' @import data.table
#' @importFrom forcats fct_reorder
#' @importFrom ggtext element_markdown
#'
#' @export
fg_tsboxplot<-function(indt,title="",xlab="",ylab="",
                      breaks=c(7,30,90,360), doi="last", normalize="", orderby="",
                      boxtype= "",
                      dropset="",hilightcats="",
                      addline="",
                      facetform="",
                      ycoord=NULL,trimpctile=0,
                      legend="insidetop",meltvar="variable",flip=FALSE,ptsize=3) {
    # Rename if necessary, premelt is if already in melted form, otherwise melt whatever we get
  vmin=vmax=vminalldta=vmaxalldta=normid=xlabel=ii=qlo=qhi=R1=R2=vmn=dtrolled=xo=`..tcollist`=NULL

  # Preprocessing: get into data.table format
  # Create dt and dt_colnames
  list2env(generic_to_melt(indt,"dtm;dt_colnames",meltvar=meltvar),envir=environment(NULL))
  if(is.na(dt_colnames['date'])) {
    stop("fg_tsboxplot must have a date column")
  }

  # Misc date stuff
  alldts <- sort(unique(dtm[[1]]))
  dtlimits <- range(alldts)
  titadd <- list()

  break_set <- form_breakset(alldts,breaks) # reused code!
  break_set <- break_set[,histcolor:= fg_get_aesstring("boxplot",.N)][]
  tcollist <- dt_colnames[setdiff(names(dt_colnames),"date")]
  tcollist <- c("DT_ENTRY","histcat",unname(unlist(tcollist[!is.na(tcollist)])))
  dtm <- break_set[dtm,on=.(BEG_DT_ENTRY<=DT_ENTRY,END_DT_ENTRY>DT_ENTRY),j=..tcollist][!is.na(histcat) & is.finite(value)]
  dtm <- dtm[,let(variable=fctr(variable),histcat=fctr(histcat))]
  if(grepl("byhistcat|byvar|zbyvar",normalize)) {
    cols_to_keep = names(dtm)
    dtm <- dtm[!is.na(value)][,`:=`(vmin=value,vmax=value)]
    dtmb <- dtm[,`:=`(vminalldta=min(vmin),vmaxalldta=max(vmax)),by=.(variable)]
    dtm  <- dtmb[,`:=`(pctbyhistcat=100*(value-min(vmin))/(max(vmax)-min(vmin)),
                    pctbyvar=100*(value-vminalldta)/(vmaxalldta-vminalldta),
                    pctzbyvar=(value-mean(value))/stats::sd(value) ),by=.(variable,histcat)]
    dtm <- dtm[,value:=NULL]
    dtm <- dtm[,c("value"):=.SD,.SDcols=paste0("pct",normalize)][!is.nan(value)]
    dtm <- dtm[,.SD,.SDcols=cols_to_keep]
    exp_set <- data.table(normid=s("byhistcat;byvar;zbyvar"),normexp=s("Pctile within hist category;Pctile within variable;ZScore by hist cat, variable"))
    #titadd["norm"] <- paste0("Scaled to ",exp_set[normid==normalize]$normexp)
    ylab <- paste0(ylab, exp_set[normid==normalize]$normexp)
    }

  # NEed to get highlights done before doi
  labelcol<-"variable"
  if(length(s(hilightcats))>0) { # DOenst reorder properly, need to move to front
    hilightcolor <- fg_get_aesstring("boxplotcat")
    labelcol <- "xlabel"
    dtm <- dtm[,let(xlabel=variable)]
    dtm[data.table(variable=s(hilightcats)),xlabel:=paste0("<span style='color:",hilightcolor,"'>",xlabel,"</span>"),on=.(variable)]
  }  # COuld combine the two and not get fancy

    # orderby: last;  value,dt: alpha: DEfault order it ins
    orderbyargs <- c(s(tolower(orderby),sep=","),0,0)
    direc <- ifelse(substr(orderbyargs[[1]],1,1)=="-",-1,1)
    orderds <- dtm[,.SD[.N],by=.(variable)][,ii:=.I]
    if(grepl("value",orderbyargs[[1]])) {
      orderds <- orderds[order(direc*value)][,ii:=.I]  # Default: Last
      titadd['order']<-"sorted on last"
    }
    if(grepl("date",orderbyargs[[1]])) {
      bdt <- suppressWarnings(lubridate::as_date(orderbyargs[2]))
      orderds <- dtm[DT_ENTRY<=bdt,.SD[.N],by=.(variable)][order(direc*value)][,ii:=.I]
      titadd['order']<-paste("sorted as of ",format(bdt,"%m-%d-%y"))
    }
    if(grepl("alpha",orderbyargs[[1]])) {
      orderds <- orderds[order(as.character(variable),decreasing=(direc<0))][,ii:=.I]
    }
    # Reduce outliers if desired
    if(trimpctile>0) {
      dtaq = dtm[,.(qlo=stats::quantile(value,trimpctile,na.rm=T),qhi=stats::quantile(value,1-trimpctile,na.rm=T)),by=.(variable)]
      dtm  = dtaq[dtm,on=.(variable)][data.table::between(value,qlo,qhi)]
    }
    dtm <- orderds[,.(variable,ii)][dtm,on=.(variable)]
    tlabels<-break_set$histcat
    dtoi=data.table()

        # doi separate from orderby, just for flexibility
    doiargs = c(s(doi,sep=","),0,0)
    if(!(tolower(doiargs[1]) %in% c("none","nolast"))) {
      bdt <- suppressWarnings(lubridate::as_date(doiargs[2]))
      if( is.na(bdt) ) bdt<-alldts[length(alldts)-as.numeric(doiargs[2])]
      dtoi <- dtm[DT_ENTRY==bdt,][,rno:=1]
      titadd["doi"] <- paste0("Dot: ",ifelse(bdt<dtlimits[2],format(bdt,"%Y-%m-%d"),"Last"))
      if( !doiargs[1]=="date" & bdt<dtlimits[2]) {
        dtoi <- rbindlist(list(dtoi, dtm[,.SD[.N][,rno:=2],by=.(variable)]),use.names=TRUE)
        titadd["doi"] <- paste0("Bar from ",min(dtoi$DT_ENTRY)," to Last")
      }
    }

    # Labels if needed
    boxlist <- s(tolower(boxtype))
    whisker_color=staple_color=box_color=NULL
    median_color <- "gray50"
    outlier_shape <- ifelse("outlier" %in% boxlist, 16,NA_integer_) # default off
    if( "nowhisker" %in% boxlist) { whisker_color  <- NA_character_ }
    if( "nostaple" %in% boxlist) { staple_color <- NA_character_ }
    if( "nomedian" %in% boxlist) { median_color <- NA_character_ }
    if( "nobox" %in% boxlist) { box_color <- NA_character_ }

    # g1 <- ggplot(dtm,aes(x=forcats::fct_relevel(!!sym(labelcol),as.character(orderds$variable)),y=value,fill=histcat))
    dtm <- dtm[,xo:=forcats::fct_reorder(get(labelcol),ii)]
    g1 <- ggplot(dtm,aes(x=xo,y=value,fill=histcat))
    if( any(grepl("violinbycat",boxlist)) ) {
      g1 <- g1 + geom_violin(trim=TRUE)
    }
    else if (any(grepl("vio",boxlist))) {
      g1 <- g1 + geom_violin(aes(x=xo,y=value),inherit.aes=FALSE,trim=TRUE)
    }
    else {
      g1 <- g1+geom_boxplot(outlier.shape=outlier_shape,whisker.color=whisker_color,median.color=median_color,
                            staple.color=staple_color,aes(fill=histcat))
    }
    g1 <- g1+scale_fill_manual(values=with(break_set,stats::setNames(histcolor,histcat)))
    if(nrow(dtoi)>0) {
      last_color <- fg_get_aesstring("boxplotlast")
      ptscast <- dcast(dtoi[,let(rno=paste0("R",rno))], ...  ~ rno)
      ptscast <- ptscast[,xo:=forcats::fct_reorder(get(labelcol),ii)]
      dotvar  <- ifelse("R2" %in% names(ptscast),"R2","R1")
      g1<-g1+geom_point(aes(x=xo,y=!!sym(dotvar)),color=last_color,size=ptsize,data=ptscast)
      if(dotvar=="R2") {   # Line from date
        g1<-g1+geom_segment(aes(x=xo,xend=xo,y=R2,yend=R1),data=ptscast,color=last_color,linewidth=1)
      }
      if(addline=="last") {  # Line through last
        g1<-g1+geom_smooth(aes(x=as.numeric(xo),y=!!sym(dotvar)),color=last_color,data=ptscast,alpha=0.2)
      }
    }
    # Legends
    g1 <- g1+labs(title=title, caption=paste(titadd,collapse="\n"), x=xlab,y=ylab)
    g1 <- g1+fg_current_theme()
    if(nchar(facetform)>0) {
        g1 <- g1+facet_grid(facetform,drop=TRUE,scales="free",space="free")
        g1 <- g1+theme(strip.text.y = element_text(size=11,color="black", face="bold"),
                          strip.background=element_rect(fill=fg_get_aesstring("boxplotfacet")))
        }
    if(addline=="mean") {
        dmn <- dtm[,.(vmn=mean(value,na.rm=T)),by=.(histcat)]
        g1 <- g1+geom_hline(aes(yintercept=vmn),data=dtm[,.(vmn=mean(value,na.rm=T))],
                            color=fg_get_aesstring("boxplotlast"), linewidth=1.5)
    }
    if( labelcol=="xlabel") {
      g1 <- g1 +  theme(axis.text.x = element_markdown(hjust = 1))
    }
    if(flip==TRUE) {
      g1 <- g1+coord_flip(ylim = ycoord)
    }
    else {
      g1 <- g1 + scale_y_continuous(limits=ycoord)
    }
    return(g1)
}
