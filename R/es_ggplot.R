#' Event Studies
#' @title Event Studies
#' @name fg_eventStudy
#' @description
#' Summarizes and plots moves in data from a given set of event dates. Plots are designed to maintain reasonable aesthetics with as
#' either time series or event dates increase.
#' @usage
#'fg_eventStudy(indata,dtset,output="path",changeas="diff",
#'              nbd_back=10,nbd_fwd=20,n_color_switch=5,
#'              title="Events",maxdelta=+Inf,meltvar="variable",verbose=FALSE)
#'
#' @param indata A data.frame with at least one date column and multiple numeric columns.  If melted,
#' must also contain the character column specified in parameter `meltvar`#'
#' @param dtset A list of dates or a `data.table` with a column of (event) dates and a character column with unique names for each date.
#' @param output (Default `path`) : String with type of output desired.  Choices are shown below by category:
#' * `data`,`summary`,`stats` : `data.table` with eVent moves by asset and business day relative event, a summary of events by asset and eventid,
#' or statistics relative to a crossing of events and assets.
#' * `path`, `pathbyvar`, `pathbyevent` Show paths of time series moves, by both events and time series, just by time series, or just by event.
#' * `lmbyvar`, `lmbyevent` Show paths of time series moves by time series, or by event, but include linear regression of move ~ time.
#' * `loessbyvar`, `loessbyevent` Show paths of time series moves by time series, or by event, but smoothed loess curves of move ~ time.
#' * `medbyvar`,`medbyevent` Median moves by time series or by event.
#' * `box`, `boxbyvar`,`boxbyevent` Box plots of moves by both events and time series, just by time series, or just by event.
#' * `scatter` Scatter plot of cumulative move from `event-nbd_back` vs `event+nbd_fwd`, with medians and regions of movement.
#' @param changeas (Default `diff`) Character string in `c("diff","return","returnbps")` describing how changes are displayed.  Log returns are used.
#' @param nbd_back (Default 10) Positive integer for number of days prior to event are considered.
#' @param nbd_fwd (Default 20) Positive integer for number of days after event are considered.
#' @param n_color_switch (Default 5) A positive integer after which colors are displayed as gradients instead of separate colors. See Examples.
#' @param title Character string for title of graph
#' @param maxdelta (Default `+Inf`) Integer to cut off the number of days forward shown, useful if you want to calculate full period statistics.
#' @param meltvar (Default `variable`)  Name of column describing distinct time series if `indata` is in long (melted) format,
#' @param verbose (Default `FALSE`)  Print Progress of calculations.
#' @examples
#' dtset <- fg_get_dates_of_interest("fedmoves",startdt="2024-01-01")[,.(DT_ENTRY,text=eventid2)]
#' fg_eventStudy(yc_CMSUST,dtset,title="Fed Cuts",output="stats")
#' fg_eventStudy(yc_CMSUST,dtset,title="Fed Cuts",output="pathbyevent")
#' fg_eventStudy(yc_CMSUST,dtset,title="Fed Cuts",output="medbyvar")
#' fg_eventStudy(yc_CMSUST,dtset,title="Fed Cuts",output="lmbyvar",n_color_switch=0)
#' fg_eventStudy(yc_CMSUST,dtset,nbd_back=3,nbd_fwd=10,title="Fed Cuts",output="boxbyvar")
#' fg_eventStudy(yc_CMSUST,dtset,title="Fed Cuts",output="scatter")
#'
#' @returns a [ggplot()] object with the  events analysis requested by `output` parameter, or a `data.frame` with statistics if `output` in `c("data"","summary","stats")`
#' @import data.table
#' @export
fg_eventStudy<-function(indata,dtset,output="path",changeas="diff",
                                nbd_back=10,nbd_fwd=20,n_color_switch=5,
                                title="Events",
                                maxdelta=+Inf,meltvar="variable",verbose=FALSE) {

  s20=s50=s80=fidelta=ps=pe=mps=mpe=NULL
  stopifnot(changeas %in% c("diff","return","returnbps"))
  stopifnot(nbd_back>0 & nbd_fwd>0)
  mapassign <- function(dta) {  lapply(names(dta), \(x) { assign(x,as.Date(dta[[x]]),pos=sys.frame(1)) })}
  list2env(generic_to_melt(indata,"alldata;dt_colnames",meltvar=meltvar),envir=environment(NULL))

  if(nrow(baddata <- alldata[is.na(value)])>0) {
    message("Warning: ",nrow(baddata), " values in melted data are NA. Could be holidays?   They will be filled in locf as necessary")
  }
  alldata<- alldata[!is.na(value)]
  alldts <- sort(unique(alldata$DT_ENTRY))
  dtsrange <- range(alldts)
  match_events_to_GP <- FALSE # Analysis with facets, for future expansion
  gmatch_vars = fifelse(match_events_to_GP,dt_colnames[["cvar"]],NA_character_)
  dtds <- es_dtset_to_dtds(dtset,nbd_back,nbd_fwd,"EVENT_DT",group_match_var=gmatch_vars,
                           mindt=dtsrange[1]+nbd_back,orderdesc=TRUE) # Sets up matchvar anyway
 # stoprifnot( match_events & length(intersect(colnames(alldata),gmatch_vars))>0, " Match events true, but in both dates and data")
  if(is.na(gmatch_vars)) { # Set up anyway
    alldata <- alldata[,NGP:=1]
    gmatch_vars <- "NGP"
  }
    # Rare case when a loop is probably better
  finaldta <-data.table()
  xdtmap <- data.table(DT_ENTRY=alldts)[,rno:=.I]
  alldata<- xdtmap[alldata,on=.(DT_ENTRY)]
  pctmult <- ifelse(grepl("bps",changeas),10000,100)
  curlabs <- list(ctitle=title,ccap=NA_character_,cx="Business days from event",
                  cy=fcase(changeas=="diff","Change from event", changeas=="returnbps","bps Return from Event",default="Pct return from Event"))
  changeas <- gsub("bps","",changeas)

    for(irow in 1:nrow(dtds)){
      thisevent <- dtds[irow,]
      #mapassign(thisevent[,c("EVENT_BEG_DT","EVENT_DT","EVENT_END_DT")])
      EVENT_BEG_DT <- thisevent[[1,"EVENT_BEG_DT"]]
      EVENT_DT <- thisevent[[1,"EVENT_DT"]]
      EVENT_END_DT <- thisevent[[1,"EVENT_END_DT"]]
      dtstring <- extenddtstr(paste0(EVENT_BEG_DT,"::",EVENT_END_DT),begchg=0,endchg=0)
      message_if(verbose,"eventStudy.givendates(",irow,"): ",dtstring)
      adta <- narrowbydtstr(alldata,dtstr=dtstring)
      bdta<- thisevent[,.SD,.SDcols=c(gmatch_vars,"eventid")][adta,on=c(gmatch_vars)]
      keepgpvars <- c(gmatch_vars,"variable")
      if(nrow(bdta)<=0) { message("No data for event : ",dtds[[irow,"eventid"]]," skipping") }
      else if(nrow(adta)>0 & min(adta$DT_ENTRY)<=EVENT_BEG_DT) {
        startrow <- adta[DT_ENTRY<=EVENT_DT,.SD[.N]]$rno
        bdta <- bdta[,idelta:=rno-startrow]
        bdta <- bdta[data.table(idelta=seq(min(bdta$idelta),max(bdta$idelta))),on=.(idelta)]
        setorderv(bdta,c(keepgpvars,"idelta"))
        bdta <- bdta[,value:=nafill(value),by=keepgpvars]
        bdta_start <- bdta[idelta==0,][,let(startval=value)][,.SD,.SDcols=c(keepgpvars,"startval")]
        cdta <- bdta_start[bdta,on=keepgpvars][,let(val_diff=value-startval,val_return=pctmult*(log(value)-log(startval)))]
        finaldta <- rbindlist(list(finaldta,cdta[idelta<=maxdelta,.SD,.SDcols=!s("rno;startval")]))
      }
  }
  finaldta <- finaldta[,eventval:=get(paste0("val_",changeas))]
  nvars <- length(unique(finaldta$variable))
  nevents <- length(unique(finaldta$eventid))

  if(output=="data") { return(finaldta[]) }
  if(output=="summary") {
    sumset<-finaldta[,.(firstval=data.table::first(value),eventval=sum((idelta==0)*value),lastval=data.table::last(value),
                        minbddt=min(idelta),maxbddt=max(idelta)),by=c(gmatch_vars,"variable","eventid")]
    sumset<-sumset[,let(move_window=lastval-firstval,move_fwd=lastval-eventval)][]
    return(sumset)
  }
  what_to_plot <- c(s(tolower(output),sep="by"),"xx")
  # Path: All data, pathbygp: picture by asset with medians, pathacrossgp, picture across assets and events
  evstats <- groupingsets(finaldta,.(s50=stats::median(eventval),s20=stats::quantile(eventval,0.2),s80=stats::quantile(eventval,0.8),.N),
                              by=c("idelta","variable","eventid"),
                              sets=list(c("variable","idelta"),c("eventid","idelta")))

  if(output=="stats") { return(evstats[]) }
  lt <- fg_get_aesstring("espath_ls")
  var_colors <- fg_get_aes(ifelse(nvars>n_color_switch,"espath_gp","lines"),n_max=nvars)$value
  e_colors <- fg_get_aes(ifelse(nevents>n_color_switch,"espath_gp","lines"),n_max=nevents)$value
  lm_color <- fg_get_aesstring("espath_line")
  lm_fill <- fg_get_aesstring("espath_fill")
  if(grepl("path",what_to_plot[1])) {
    if(what_to_plot[[2]]=="var") {
      g1<- ggplot(evstats[is.na(eventid),],aes(x=idelta,color=variable,fill=variable))
      g1<- g1 + geom_ribbon(aes(ymin=s20,ymax=s80),alpha=0.2)
      g1<- g1 + geom_line(aes(y=s50),linewidth=1.2)
      g1<- g1 + scale_color_manual(values=var_colors,guide=legd_guide("bottomright",title=NULL,ncats=nvars))
      g1<- g1 + scale_fill_manual(values=var_colors,guide=legd_guide("bottomright",title=NULL,ncats=nvars))
      curlabs[c("ccap")]<-c("Across Events, Median move (w/20th and 80th percentiles)")
      }
    else if(what_to_plot[[2]]=="event") {
      g1<- ggplot(evstats[is.na(variable),],aes(x=idelta,color=eventid,fill=eventid))
      g1<- g1 + geom_ribbon(aes(ymin=s20,ymax=s80),alpha=0.4)
      g1<- g1 + geom_line(aes(y=s50),linewidth=1.2)
      g1<- g1 + scale_color_manual(values=e_colors,guide=legd_guide("bottomleft",title="events",ncats=nevents))
      g1<- g1 + scale_fill_manual(values=e_colors,guide=legd_guide("bottomleft",title="events",ncats=nevents))
      curlabs[c("ccap")]<-c("Across Tickers, Median move (w/20th and 80th percentiles)")
    }
    else {
      g1<- ggplot(finaldta,aes(x=idelta,y=eventval,color=variable,linetype=eventid)) + geom_line(linewidth=1)
      g1<- g1 + scale_color_manual(values=var_colors,guide=legd_guide("bottomright",title=NULL,ncats=nvars))
      g1<- g1 + scale_linetype_manual(values=lt,guide=legd_guide("bottomleft",title="events",ncats=nevents))
      curlabs["ccap"]<-"All Paths"
      }
    }
  if(what_to_plot[1] %in% c("lm","loess")) {
    tformula <- ifelse(what_to_plot[1]=="lm","y~0+x:(x>0)","y~x")
    if(what_to_plot[[2]]=="var") {
      g1<- ggplot(finaldta,aes(x=idelta,y=eventval,color=variable))+geom_point(alpha=0.5)
      g1<- g1 + geom_smooth(method=what_to_plot[1], formula=tformula,alpha=0.2,linewidth=1)
      g1<- g1 + scale_color_manual(values=var_colors,guide=legd_guide("topright",ncats=nvars))
      g1<- g1 + geom_smooth(aes(x=idelta,y=eventval),method=what_to_plot[[1]], formula=tformula,
                            color=lm_color, fill=lm_fill,alpha=0.2,linewidth=1.5)
      curlabs[c("ccap")]<-c("Change across tickers, with fit line")
      }
    else if(what_to_plot[[2]]=="event") {
      g1<- ggplot(finaldta,aes(x=idelta,y=eventval,color=eventid))+geom_point(alpha=0.5)
      g1<- g1 + geom_smooth(method=what_to_plot[1], formula=tformula,alpha=0,linewidth=1)
      g1<- g1 + scale_color_manual(values=e_colors,guide=legd_guide("topright",ncats=nevents))
      g1<- g1 + geom_smooth(aes(x=idelta,y=eventval),method=what_to_plot[[1]], formula=tformula,
                            color=lm_color, fill=lm_fill,alpha=0.2,linewidth=1.5)
      curlabs[c("ccap")]<-c("Change across events, with fit line")
      }
    }
  if(grepl("med",what_to_plot[1])) {
    if(what_to_plot[[2]]=="var") {
      g1<- ggplot(evstats[is.na(eventid),],aes(x=idelta,color=variable))
      g1<- g1 + geom_line(aes(y=s50),linewidth=1.2)
      g1<- g1 + scale_color_manual(values=var_colors,guide=legd_guide("insidetop",title=NULL,ncats=nvars))
      curlabs[c("ccap")]<-c("Median Move by ticker")
    }
    else if(what_to_plot[[2]]=="event") {
      g1<- ggplot(evstats[is.na(variable),],aes(x=idelta,color=eventid))
      g1<- g1 + geom_line(aes(y=s50),linewidth=1.2)
      g1<- g1 + scale_color_manual(values=e_colors,guide=legd_guide("insidetop",title="events",ncats=nevents))
      curlabs[c("ccap")]<-c("Median Move by event")
    }
    else {
      stop("Choose medbyevent or medbyvar..")
    }
  }
  if(what_to_plot[1]=="box") {
    finaldta <- finaldta[,let(pastevent=(idelta>0),fidelta=fctr(as.character(idelta)))]
    if(what_to_plot[[2]]=="var") {
      g1<- ggplot(finaldta,aes(x=fidelta,y=eventval,color=variable,fill=variable))+geom_boxplot(outlier.shape=NA,fill=0.2)
      g1<- g1 + scale_color_manual(values=var_colors,guide=legd_guide("insidetop",title=NULL,ncats=nvars))
      g1<- g1 + scale_fill_manual(values=var_colors,guide="none")
      curlabs[c("ccap")]<-c("Move by ticker")
    }
    else if(what_to_plot[[2]]=="event") {
      g1<- ggplot(finaldta,aes(x=fidelta,y=eventval,color=eventid))+geom_boxplot(outlier.shape=NA,fill=0.2)
      g1<- g1 + scale_color_manual(values=e_colors,guide=legd_guide("insidetop",title="eventid",ncats=nevents))
      curlabs[c("ccap")]<-c("Move by ticker")
    }
    else {
      g1<- ggplot(finaldta,aes(x=fidelta,y=eventval))+geom_boxplot(outlier.shape=NA,fill=0.2)
      curlabs[c("ccap")]<-c("Moves across tickers and variables")
    }
    g1<- g1 + gline_y(color=fg_get_aesstring("espath_y"),linetype="dotted",int=as.factor(0),
                      linewidth=fg_get_aesstring("espath_y",toget="const")  |> as.numeric())
  }
  if(what_to_plot[1]=="scatter") {
    pdt_1<- finaldta[,.SD[c(1,.N),.(eventval,id=fifelse(sign(idelta)<0,"ps","pe"))],by=.(variable,eventid)]
    pdt_2<- dcast(pdt_1,variable+eventid~id,value.var = "eventval")
    pdt_3<- pdt_2[,lapply(.SD,stats::median),by=.(variable),.SDcols=c("ps","pe")]
    g1 <- ggplot(pdt_2,aes(x=ps,y=pe,color=variable))+geom_point(size=1.5)
    g1 <- g1 + geom_polygon(aes(color=variable,fill=variable),alpha = 0.1,data = pdt_2[,.SD[grDevices::chull(ps,pe)], by=.(variable)])
    g1 <- g1 + ggrepel::geom_text_repel(aes(x=ps,y=pe,label=variable,color=variable),data=pdt_3,show.legend=FALSE)
    # New Outlier
    pdt_4 <- pdt_3[,.(variable,mps=ps,mpe=pe)][pdt_2,on=.(variable)][,xdist:=dist(rbind(c(mps,mpe),c(ps,pe))),by=.I]
    pdt_4 <- pdt_4[,.SD[which.max(xdist)],by=.(variable)]
    g1 <- g1 + geom_label(aes(x=ps,y=pe,label=eventid,color=variable),data=pdt_4,show.legend=FALSE)
    g1 <- g1 + scale_color_manual(values=var_colors,guide=legd_guide("bottomright",title=NULL,ncats=nvars))
    g1 <- g1 + scale_fill_manual(values=var_colors,guide="none")
    g1 <- g1 + gline_x(color="black")+gline_y(color="black")+scale_x_continuous(n.breaks=10)
    curlabs[c("ccap","cx","cy")]<-c("Start move vs End Move, Labels at Median moves and extreme events",paste(changeas,nbd_back,"bdays prior to event"),
                                    paste(changeas,nbd_fwd,"bdays post event"))

  }
  if(!(what_to_plot[1] %in% c("box","scatter"))) {
    g1<- g1 + gline_y(color=fg_get_aesstring("espath_y"),linetype="dotted",linewidth=fg_get_aesstring("espath_y",toget="const")  |> as.numeric())
    g1<- g1 + scale_x_continuous(breaks=seq(-nbd_back,nbd_fwd))
  }
  if(!(what_to_plot[1] %in% c("scatter"))) {
    g1<- g1 + gline_x(color=fg_get_aesstring("espath_x"),linewidth=fg_get_aesstring("espath_x",toget="const")  |> as.numeric())
  }
  g1<- g1 + scale_y_continuous(n.breaks=10)
  g1<- g1 + fg_current_theme()
  g1<- g1 + labs(title=curlabs[["ctitle"]],y=curlabs[["cy"]],x=curlabs[["cx"]],caption=curlabs[["ccap"]])
  return(g1)
}

#Assumes eventname is "text"
es_dtset_to_dtds<- function(indtset,nbd_back,nbd_fwd, orderby="EVENT_DT",mindt=0,
                            group_match_var=NA_character_, orderdesc=FALSE) {
  isday=mergecd=offset=id=EVENT_ORDER=N=NULL
  if(is.data.frame(indtset)) { # Rename to my conventions
    list2env(generic_to_melt(indtset,"dtset;dt_colnames",meltvar=group_match_var),envir=environment(NULL))
    #setnames(dtset,c("DT_ENTRY","text"),c("EVENT_DT_ENTRY","eventid"),skip_absent=TRUE)  # includes cvar, eventid
    setnames(dtset,dt_colnames[["cvar"]][1],c("eventid"),skip_absent=TRUE)  # includes cvar, eventid, ok if eventid already thre
  }
  else if (lubridate::is.instant(dtset)) {
    dtset <- data.table(DT_ENTRY=indtset,eventid=paste0("EV:",format(indtset,"%Y-%m-%d")), EVENT_ORDER=indtset)
  }
  else {
    stop("es_dtset_to_dtds unknown event dates format in first argument")
  }
  dtset<- dtset[DT_ENTRY>=mindt,]
  # Check for duplicate eventids
  if(nrow( dtset[,.N,by=.(eventid)][N>1])>0 ) {
    message("fg_eventStudy: EVent Ids are not unique, replacing with unique text labels")
    dtset<-dtset[,text:=NA_character_][,eventid:=NULL]
  }
  cln <- colnames(dtset)
  if( !any(grepl("eventid", cln)) ) {
      dtset <- dtset[,eventid:=fcoalesce(text,paste0("EV:",format(DT_ENTRY,"%Y-%m-%d")))] }
  if( !is.na(group_match_var) & !(group_match_var %in% cln) ) {
      stop("You want to match events to ",group_match_var," but it is not in date set")
    }
  if(is.na(group_match_var)) {  # Set up a match var anyw
    group_match_var="NGP"
    dtset <- dtset[,NGP:=1]
  }
  dttmp <- dtmap[isday==TRUE,.(DT_ENTRY,isday)][,rno:=.I] # Bdays only
  dtset <- dttmp[dtset,on=.(DT_ENTRY)][,let(DT_ENTRY=NULL)]
  dt_off <- data.table(offset=c(-nbd_back,0,nbd_fwd),id=c("BEG_DT","DT","END_DT"),mergecd=1)
  dtset_2 <- merge(dt_off,dtset[,mergecd:=1],by="mergecd",all=TRUE,allow.cartesian=TRUE)
  dtset_3 <- dttmp[dtset_2[,rno:=rno+offset],on=.(rno)][,let(mergecd=NULL,id=paste0("EVENT_",id))]
  dtds <- dcast(dtset_3,stats::formula(paste("eventid+",group_match_var,"~id")),value.var="DT_ENTRY")
  dtds <- dtds[,let(eventid=fctr(eventid),EVENT_ORDER = get(orderby))]
  dtds <- dtds[order(fifelse(orderdesc,-1,1)*as.numeric(EVENT_ORDER))][!is.na(EVENT_DT )]
  return(dtds)
}

full_dtset<-function(freq="yrwk", begdt=0, enddt=NA_real_, startofweek=T, extenddays=0,add="") {
  DT_ENTRY=NULL
  lastdt= fcoalesce(enddt,Sys.Date()+extenddays)
  fdts= dtmap[data.table::between(DT_ENTRY,begdt,lastdt),.SD,keyby=freq]
  if(!(freq=="day" | freq=="days" | freq=="DT_ENTRY")) {
      fdts <- fdts[,.SD[fifelse(startofweek,1,.N)],by=freq,.SDcols=c("DT_ENTRY")]
  }
  if(nchar(add)>0) { fdts[[add]]<-seq(1,nrow(fdts)) }
  return(fdts)
}
