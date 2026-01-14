#' TIme Series Graphs
#'
#' @name fgts_dygraph
#'
#' @param indt Input data
#'
#' @returns Dygraph
#'
#' @details See Rd
#'
#' @examples
#' library(tidyquant)
#' dta = tq_get(c("IBM","ORCL","NVDA")) |> select(DT_ENTRY=date, variable=symbol,value=adjusted)
#' fgts_dygraph(dta, title="Tech Stock Prices", ylab="Adjusted Close Price", roller="default",dtstartpct=0.8)
#'
#' @export
fgts_dygraph<-function(indt,annovar="",titlevar="",splitfirst=FALSE,title="",meltvar="variable",ylab="",roller="default",
                        events="",eventdataset=NULL,pointers="hair,horizontal",ylimits=NULL,miscannotation="",
                        dtstartpct=0,dtwindow="",exportevents=FALSE,hlinedsn=NULL,hideseries=NULL,
                        lownm="",hinm="",colors=hicPaletteDygraph, stroke=NULL, strokewidth=1,forecast="",forecast_periods=10,
                        dylegend="always",groupnm="common",pvalue=NULL,addextraspace=0.1,stepPlot=NULL,fillGraph=FALSE,
                        extraoptions=list(),...) {

 # Preprocessing: get into data.table format
    if(is.xts(indt)) { indt <- xts2df(indt) }
    if(dplyr::is.tbl(indt)) { indt <-ungroup(indt) }
    if(!data.table::is.data.table(indt)) { indt <- data.table::data.table(indt) }
    annolistnames <- c("todo","a1","a2","a3","a4","a5")
    tevents <-data.frame()
    if(is.data.frame(events))  {
        elist = events }
    else {
        elist = suppressWarnings(tibble::tibble(todo=s(events)) |>
                      tidyr::separate_wider_delim(todo,",",names=annolistnames,too_many="drop",too_few="align_start"))
        }
    flist = suppressWarnings(tibble::tibble(todo=s(forecast)) |>
                      tidyr::separate_wider_delim(todo,",",names=annolistnames,too_many="drop",too_few="align_start"))

    hairopts =  optString_parse(pointers,"cross|hair")
    hlineopts =  optString_parse(pointers,"hline")
    hbaropts =  optString_parse(pointers,"hbar")
    shadedates = data.table::data.table()

    if(meltvar %in% colnames(indt)) {
        wasmelted=TRUE
        lastoset = indt[,.SD[.N],by=meltvar]
        lastobs  = as.vector(lastoset$value)
        lastlabs = as.vector(lastoset[[meltvar]]) }
    else {
        wasmelted=FALSE
        lastobs<-as.vector(indt[nrow(indt),])[2:ncol(indt)]
        lastlabs=colnames(indt)[2:ncol(indt)]
    }


    # Set siglo and sighi if needed
    # Preprocessing tstats
    if(length(pvalue)==2) {
        valrange=diff(range(indt$value,na.rm=T))
        indt$siglo = as.vector(ifelse(indt[,pvalue[[1]]]<pvalue[[2]],indt$value-0.02*valrange,indt$value))
        indt$sighi = as.vector(ifelse(indt[,pvalue[[1]]]<pvalue[[2]],indt$value+0.02*valrange,indt$value))
        lownm="siglo"; hinm="sighi"
    }
    if(!is.logical(hbaropts) & is.logical(hairopts)) {
        hbarranges = c(s(hbaropts,sep=","),NA_real_,NA_real_)
        indt$siglo = fcoalesce(as.numeric(hbarranges[[1]]),min(indt[,1],na.rm=T))
        indt$sighi = fcoalesce(as.numeric(hbarranges[[2]]),max(indt[,1],na.rm=T))
        lownm="siglo"; hinm="sighi"
    }

    # unMelt it all
    #cAssign("meltvar;indt")
    if(meltvar %in% colnames(indt)) {
        indtnew=data.table::dcast(indt[,.(DT_ENTRY,variable=get(meltvar),value)],DT_ENTRY ~ variable)
        }
    else { indtnew=indt }

    # Forecasts and Other things -------------------------------------------------------------------------------------
    if (wasmelted) {
        firstseries = indt[get(meltvar)==indt[[1,meltvar]],] }
    else{
        firstseries = xts2df(indt) |> relocate(DT_ENTRY) |> select(1:2) |> set_colnames(s("DT_ENTRY;value"))
        }

    if( nrow( trow<-filter(flist,grepl("^lmregime",todo)) )>0) {
        firstseries = firstseries[,time:=as.numeric(DT_ENTRY-min(DT_ENTRY))+1]
        maxk = fcoalesce(as.integer(as.numeric(trow$a1)),nrow(firstseries))
        regimeset = optimalRegimes(as.data.frame(firstseries), varnm="variable",max_k=maxk)
        message("optRegimes maxk:",maxk)
        if(grepl("res",trow$todo)) {
            indtnew[,1] = regimeset$d$resid }
        else {
            indtnew$pred = regimeset$d$estimate }
    }
    if(nrow( trow<-filter(elist,todo=="sig") )>0) { # ALso added to events
        title=paste(title,"<br><small>",trow$a1,"shaded at",trow$a2,"sig</small>")
    }
    if(nrow( trow<-filter(elist,todo=="stregime") )>0) { # ALso added to events
        thiscat= fcoalesce(trow$a1,"")
        title=paste(title,"<small>(Bars:",thiscat,"regimes)</small>")
    }
    if( nrow( trow<-filter(flist,todo=="var") )>0 & ncol(indtnew)>=2) {
        #var_params=c(trow$a1,1)
        #fc1=group_by(cdta,GP) |> do( { x=select(.,DT_ENTRY,S); as.data.frame(forecast(auto.arima(df2xts(x)),h=10)) |> mutate(DT_ENTRY=max(x$DT_ENTRY,na.rm=T)+row_number()) })
        #fcst_set = xather(xts2df(indtnew,variable,value) |> group_by(variable) |> filter( (n()-row_number()) %% arima_params[[2]] == 0)
        indtfct=copy(indtnew)
        indtfct = df2xts(indtfct[complete.cases(indtfct),])
        varfct=vars::VAR(indtfct,p=3,type="trend")
        fcst = predict(varfct,n.ahead=forecast_periods)
        fcstpts = as.data.frame(fcst$fcst) |> dplyr::select(ends_with("fcst")) |> magrittr::set_colnames(colnames(indtfct))
        fcstpts$DT_ENTRY = seq(as.Date(end(indtfct)),by=unclass(periodicity(indtfct))$label,length.out=forecast_periods+1)[-1]
        indtnew=bind_rows(indtnew,fcstpts)
        title=paste(title,"<small><small>w",forecast_periods,"pd VARfcst</small></small>")
        tevents=bind_rows(tevents,data.frame(text="TODAY",as.Date(end(indtfct)),loc="top",color="red", category="TODAY"))
    }
    if( nrow( trow<-filter(flist,grepl("^hp",todo)) )>0) {
        library(mFilter)
        hptmp = mFilter::hpfilter(indtnew[,1],freq=fcoalesce(as.integer(as.numeric(trow$a1)),as.integer(100)),type="lambda")
        if(grepl("res",trow$todo)) {
            indtnew[,1] = hptmp$cycle }
        else {
            indtnew$pred = as.numeric(hptmp$trend) }
    }

    #cAssign("wasmelted;indt;indtnew;lastoset;lastobs;lastlabs")
    usehilo=(nchar(lownm)>0 | nchar(hinm)>0) & is.logical(hbaropts)
    if(usehilo) {
        hilownms=colnames(indtnew)[-1]
        # hacks,order of hack important
        if(is.xts(indt)) { indt=xts2df(indt) }
        if(!wasmelted) { indt$variable=colnames(indt)[2]}
        indt = as.data.table(indt)
        lowdt=dcast(indt[,v2:=paste0(variable,".lo")],DT_ENTRY ~ v2, value.var=ifelse(lownm %in% colnames(indt), lownm, "value") )
        hidt=dcast(indt[,v2:=paste0(variable,".hi")],DT_ENTRY ~ v2, value.var=ifelse(hinm %in% colnames(indt), hinm, "value"))
        indtnew=cbind(indtnew,hidt[,`:=`(DT_ENTRY=NULL)])
        indtnew=cbind(indtnew,lowdt[,`:=`(DT_ENTRY=NULL)])
    }

    fmtdate <- 'function(d){ return d.getMonth() + "-" + d.getYear() }'
    if(!is.null(hideseries)) {
        indt = df2xts(indtnew |> select(-!!rlang::sym(hideseries)))
    }
    else {
        indt=df2xts(indtnew)
    }
    indtdts=zoo::index(indt)
    if(length(ylimits)==2) {
        indt=pmin(indt,ylimits[2])
        indt=pmax(indt,ylimits[1])
    }
    if(is.character(ylimits)) {
        qlimit=as.numeric(c(s(ylimits),0.01)[[2]])
        qset  =quantile(indtnew[[2]],c(qlimit,1-qlimit),na.rm=T)
        title=paste(title,"<small><small>(Winsored@",qlimit,")</small></small><br>test")
        indt=pmin(indt,qset[2])
        indt=pmax(indt,qset[1])
    }
    if(nchar(dtwindow)>1) {
        dtsrange = gendtstr(dtwindow,rtn="list")
        }
    else {
        alldts = sort(unique(indtdts))
        dtsrange = c(as.Date(alldts[length(alldts)*dtstartpct+1]), as.Date(indtdts[length(alldts)]))
    }
    #cAssign("dtwindow;indt;indtnew;title;ylab;groupnm;splitfirst;indtdts;dtstartpct;dtsrange")
    #g1=dygraph(indt,xlab=NULL,ylab=ylab,group="variable") |> dyOptions(labelsKMB=TRUE,rightGap=2,colors=tcolors[1:5])

        g1=dygraphs::dygraph(indt,main=title,xlab=NULL,ylab=ylab,group=groupnm) |>
            dygraphs::dyLegend(width=600, show="always",hideOnMouseOut = FALSE) |>
            dygraphs::dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.4, highlightSeriesOpts = list(strokeWidth = 3))

    if(usehilo) {for(nm in hilownms) {
        g1=g1 |> dygraphs::dySeries(c(paste0(nm,".lo"), nm, paste0(nm,".hi")), label=nm, stepPlot=stepPlot) }    }
    if(!is.logical(hairopts)) {
        g1 =g1 |> dygraphs::dyCrosshair(direction = hairopts)
    } # horizontal,both,veetical

    g1 = g1 |> dygraphs::dyOptions(labelsKMB=TRUE,rightGap=2,colors=s(colors), fillAlpha=0.25,strokePattern=stroke,strokeWidth=strokewidth,stepPlot=stepPlot, axisLabelFontSize=10,fillGraph=fillGraph,...)

    # Statistical data turning points (dyEvent)
    # ======================================================================================
    # event Types:
    # in { [doi,startof|]doicategory ; break ; [tp,method,npts,maxwindow] ; data_frame  }
    # ======================================================================================
    if(nrow( trow<-filter(elist,todo=="doi") )>0) {
        thiseventstr= fcoalesce(trow$a1, "")
        eventonly   = grepl("startof",thiseventstr,ignore.case=T)
        g1 = g1 |> dygraphs::dyAxis("x",paste("Shaded Events:",thiseventstr),labelHeight=9)
        tdates = filter(doidates,tolower(category)==tolower(gsub("startof","",thiseventstr)) & DT_ENTRY>=min(zoo::index(indt)))
        tdates %<>% ungroup |> mutate(direct=str_sub(eventid,-1), rno=row_number())
        if(nrow(tdates)>0) {
            tdates %<>% left_join(tibble(direct=s("-;+;="), tcolor=s("#FFE6E6;#CCEBD6;white")), by="direct") |> mutate(color = fcoalesce(tcolor,ifelse(rno %% 2, "#FFE6E6","lightblue"))) |> select(-tcolor)
            shadedates = copy(tdates)
            for(irow in 1:nrow(tdates)) {
                lineAssign(tdates[irow,])
                #message("irow ",irow, "color = ",tcolor, " id: ",tdates[[irow,"eventid"]], " from : ",tdates[[irow,"DT_ENTRY"]], " to ",tdates[[irow,"END_DT_ENTRY"]])
                if( eventonly | (END_DT_ENTRY - DT_ENTRY)<2 ) {
                    g1=g1 |> dygraphs::dyEvent(as.Date(DT_ENTRY), eventid, labelLoc="bottom") }
                else {
                    g1=g1 |> dygraphs::dyShading(from=as.Date(DT_ENTRY), to=as.Date(END_DT_ENTRY),color=color)
                }
            }
        }
    }
    # Need to add drivers dataset from EMFM_Correlation
    # "break" adds statistical break levels as vertical dashed lines
    if(nrow( trow<-filter(elist,todo=="KNN") )>0) {
        thiscat= fcoalesce(trow$a1,"ALL")
        tdates = filter(doidates,grepl("KNN",category)  & grepl(thiscat,category) & DT_ENTRY>=min(zoo::index(indt)))
        tevents =bind_rows(tevents,transmute(tdates,text=paste0(thiscat,gsub("knnloc","",eventid)),DT_ENTRY,loc="top",color="darkgreen", category="KNN"))
    }
    if(nrow( trow<-filter(elist,todo=="scenset") )>0) {
        if(!exists("scenario_dates")) { unzip_dfset("scenarios")}
        tdates = filter(scenario_dates,grepl(trow$a1,scenario,fixed=TRUE)  & DT_ENTRY>=min(zoo::index(indt)))
        tevents = bind_rows(tevents,transmute(tdates,DT_ENTRY,text=gsub("knnloc","",text),color="black",loc="bottom",category="scenarios"))
    }
    if(nrow( trow<-filter(elist,todo=="stregime") )>0) {
        thiscat= fcoalesce(trow$a1,"")
        title=paste(title,"<small><small>(Colors:",thiscat,"regimes)</small></small>")
        tevents= bind_rows(tevents,inv.regimecuts(paste0("reglast,",thiscat))[,.(DT_ENTRY,END_DT_ENTRY,color)][,category:=paste0("ST:",thiscat)])
    }

    # Still to do ...  Add to shadedates ...
    if(nrow( trow<-filter(elist,todo=="break") )>0) {
        firstcnm=grep("DT_ENTRY",colnames(indt),value=TRUE,invert=TRUE)
        bodates=addonebreakout(xts2df(indt[,c(firstcnm)])[], annotationstyle="periodsonly")
        bodates$nshadset=((1:nrow(bodates))-1) %% 3
        bodates %<>% left_join( tibble(nshadset=c(0,1,2), tcolor=s("#FFE6E6;#CCEBD6;white")), by="nshadset")
        for(irow in 1:nrow(bodates)) {
            g1=g1 |> dyShading(from=as.Date(bodates[[irow,"DT_BEG"]]), to=as.Date(bodates[[irow,"DT_ENTRY"]]), color=bodates[[irow,"tcolor"]])
        }
    }
    # "roll" adds Rolls as vertical dashed lines
    if(nrow( trow<-filter(elist,todo=="roll") )>0) {
        tdates= tibble::tibble(END_DT_ENTRY=seq(ymd('2001-03-20'),ymd('2027-03-20'), by = '6 month'))
        if(is.na(trow$a1)) {
            tevents=bind_rows(tevents,filter(tdates,END_DT_ENTRY>=min(zoo::index(indt))) |> transmute(DT_ENTRY=END_DT_ENTRY, text="roll",loc="top",color="#40995c",strokePattern="dotted",category="roll")) }
        else {
            tdates= tdates |> mutate(DT_ENTRY=END_DT_ENTRY-as.numeric(trow$a1)) |> filter(END_DT_ENTRY>=min(zoo::index(indt)) & DT_ENTRY<=max(zoo::index(indt)))
            for(irow in 1:nrow(tdates)) {
                g1=g1 |> dygraphs::dyShading(from=as.Date(tdates[[irow,"DT_ENTRY"]]), to=as.Date(tdates[[irow,"END_DT_ENTRY"]]),color="#CCEBD6") }
            }
    }
    # --------------------------
    if(nrow( trow<-filter(elist,todo=="minmax" | todo=="extremes") )>0) {
        indt_dt = melt(xts2df(indt),id.var="DT_ENTRY")
        tp=rbindlist(list(
            indt_dt[,.SD[which.min(value)],by=.(variable)][,.(DT_ENTRY,text=paste("min",variable),color="red",loc="bottom",category="extr")],
            indt_dt[,.SD[which.max(value)],by=.(variable)][,.(DT_ENTRY,text=paste("max",variable),color="blue",loc="top",category="extr")]
        ))
        tevents=bind_rows(tevents,tp)
    }
    # "tp,nn" adds turning points as dotted vertical lines
    if(nrow( trow<-filter(elist,todo=="tp" | todo=="tpa") )>0) {
        tpds = df2xts(firstseries)
        tmpmsg = paste0(trow$a1, "turning Points on ", colnames(indt)[1])
        tp=findTurningPoints(tpds,npts=as.numeric(fcoalesce(trow$a1,"5")),rtn="dates",maxwindow=as.numeric(fcoalesce(trow$a2,"-1")),method=fcoalesce(trow$a3,"pctchg"))
        #message("FInding Turning Points, converting ",tpset, " to tpset w ",nrow(tp), "rows")
        tevents=bind_rows(tevents,tp |> mutate(category="tp", END_DT_ENTRY=DT_ENTRY))
    }
    # "dt,<datelist>" adds inddivudal dates as dotted vertical lines
    if(nrow( trow<-filter(elist,todo=="dt") )>0) {
        for(icol in 2:ncol(trow)) {
            tdt = trow[[icol]]
            if( !is.na(tdt) & nchar(tdt)>0) { g1=g1 |> dygraphs::dyEvent(as.Date(tdt), as.character(tdt), labelLoc="bottom",color="black") } }
    }
    # "sig,<variable>,<level> colors variable according to whether <variable> is between (-inf,-sig,+sig,Inf)
    if(nrow( trow<-filter(elist,todo=="sig") )>0) {
        valruns = runs_from_value(as.data.table(transmute(indtnew,DT_ENTRY,value=(!!rlang::sym(trow$a1)<=as.numeric(trow$a2)))))
        tevents = bind_rows(tevents,filter(valruns,value==TRUE) |> mutate(category="sig", text="sig"))
    }
    # ratings,CREDIT
    if(nrow( trow<-filter(elist,todo=="ratings") )>0) {
        LIFN("ratings")
        thiscredit = fcoalesce(trow$a1, "XX")
        colorcol   = paste0(fcoalesce(trow$a2, ""),"color")
        if(thiscredit!="XX") {
            dtrg=range(index(indt))
            tdates = ratings$ratruns[CREDIT==thiscredit & AGENCY=="COMP" & dtrg[1]<=END_DT_ENTRY][,`:=`(END_DT_ENTRY=pmin(END_DT_ENTRY,dtrg[2]),DT_ENTRY=pmax(DT_ENTRY,dtrg[1]))]
            # To add both lines and colors, add colors here, text later
            for(irow in 1:nrow(tdates)) {
                #message(" Adding ",irow," .. ", as.Date(tdates[[irow,"DT_ENTRY"]]),"::",as.Date(tdates[[irow,"END_DT_ENTRY"]])," >> " , tdates[[irow,"color"]])
                g1=g1 |> dygraphs::dyShading(from=as.Date(tdates[[irow,"DT_ENTRY"]]), to=as.Date(tdates[[irow,"END_DT_ENTRY"]]),color=tdates[[irow,colorcol]]) }
            }
            tevents = bind_rows(tevents, tdates[,.(DT_ENTRY,text=RTGTXT,loc=ifelse(c(0,diff(value,1))>0,"bot","top"),color="black",vcetegory="ratings")] )
        }
    if(nrow( trow<-filter(elist,todo=="rtbubble") )>0) {
        library(psymonitor)
        ydta = indt[,1]
        obs  = length(ydta)
        swindow0 <- floor(obs * (0.01 + 1.8 / sqrt(obs))) # set minimal window size
        yr       <- 2
        Tb       <- 12*yr + swindow0 - 1  # Set the control sample size
        sadf     <- PSY(ydta, swindow0 = swindow0, IC = 2,adflag = 6)  # estimate the PSY test statistics sequence
        quantilesBsadf <- cvPSYwmboot(ydta, swindow0 = swindow0, IC = 2, adflag = 6, Tb = Tb, nboot = 40, nCores = 4)
        dim          <- obs - swindow0 + 1
        monitorDates <- zoo::index(indt[swindow0:obs])
        quantile95   <- quantilesBsadf %*% matrix(1, nrow = 1, ncol = obs - swindow0 + 1 )
        ind95        <- (bsadf > t(quantile95[2, ])) * 1
        bubperiods   <- psymonitor::locate(ind95, monitorDates)  # Locate crisis periods
        xevents = rename(bubperiods, DT_ENTRY=start,END_DT_ENTRY=end) |> mutate(category="rtbubble")
        tevents = bind_rows(tevents,xevents)
    }

    if(is.data.frame(eventdataset)) {
        if(!("color" %in% colnames(eventdataset))) { eventdataset %<>% mutate(color="#FFE6E6") }
        if(!("text" %in% colnames(eventdataset))) { eventdataset %<>% mutate(text="") }
        if(!("loc" %in% colnames(eventdataset))) { eventdataset %<>% mutate(loc="top") }
        if(!("END_DT_ENTRY" %in% colnames(eventdataset))) { eventdataset %<>% mutate(END_DT_ENTRY=DT_ENTRY) }
        tevents=bind_rows(tevents,transmute(eventdataset,DT_ENTRY=as.Date(DT_ENTRY),END_DT_ENTRY=as.Date(END_DT_ENTRY),color,text,loc) |> mutate(category="eventdataset") )
    }

#    if(nrow( trow<-filter(elist,todo=="lmregime") )>0) {
#        regimeset = optimalRegimes(xts2df(indtnew[,1]) |> xather(variable,value,-DT_ENTRY) |> mutate(time=as.numeric(DT_ENTRY-min(DT_ENTRY)+1)))
#        g1 = g1 |> dySeriesData("linregime",regimeset$dtopplot$value)
#        g1 = g1 |> dySeries("linregime")
#         #g1=g1 |> dySeries(c(paste0(nm,".lo"), nm, paste0(nm,".hi")), label=nm, stepPlot=stepPlot) }    }
#        tevents=bind_rows(tevents,regimeset$tevents)
#    }
    # Events: df (DT_ENTRY,END_DT_ENTRY,text,loc,color,strokePattern)

    if(exportevents) {
        dyg_dates = shadedates
        if(nrow(tevents)>0) {
            dyg_dates = rbindlist(list(rename(tevents,eventid=text),shadedates),use.names=TRUE,fill=TRUE) }
        cAssign("dyg_dates",silent=FALSE)
    }

    if(nrow(tevents)>0) {
        if("END_DT_ENTRY" %in% colnames(tevents)) {
            tevents=coalescedf(tevents,new_tibble(list(DT_ENTRY=Sys.Date(),END_DT_ENTRY=Sys.Date(),color="#FFE6E6"),nrow=1)) } # |> as.data.frame()
        else {
            tevents=coalescedf(tevents,new_tibble(list(DT_ENTRY=Sys.Date(),text="POI",loc="bottom",color="red",strokePattern="dashed"),nrow=1)) |> as.data.frame()
        }
        for(irow in 1:nrow(tevents)) {
            tdtstart = tevents[[irow,"DT_ENTRY"]]
            tdtend = tevents[[irow,"END_DT_ENTRY"]]
            if(is.na(tdtend) | (tdtstart==tdtend)) {
                g1=g1 |> dygraphs::dyEvent(as.Date(tdtstart), tevents[[irow,"text"]], labelLoc = fcoalesce(tevents[[irow,"loc"]],"top"),
                        color=fcoalesce(tevents[[irow,"color"]],"green"), strokePattern=fcoalesce(tevents[[irow,"strokePattern"]],"dotted"))
                }
            else {
                g1=g1 |> dygraphs::dyShading(from=as.Date(tdtstart), to=as.Date(tdtend), color=tevents[[irow,"color"]])
                }
            }
        }
    if(addextraspace>0) {
        indttmp =indt[paste0(dtsrange[1],"::",dtsrange[2])]
        rangeall=c(min(apply(indt,2,min, na.rm=T)), max(apply(indt,2,max, na.rm=T)) )
        rangelast=c(min(apply(indttmp,2,min, na.rm=T)), max(apply(indttmp,2,max, na.rm=T)) )
        #cAssign("indt;dtsrange;indtdts;indttmp;colsformaingraph;rangeall;rangelast")
        if( rangelast[1]>0.9*rangeall[2]) {
            g1 = g1 |> dygraphs::dyAxis('y',valueRange=c(rangeall[1],rangeall[2]+addextraspace*(rangeall[2]-rangeall[1]))) }
    }
    if(!(splitfirst==FALSE)) {
        splitcol = ifelse( splitfirst %in% colnames(indt), splitfirst, setdiff(colnames(indt),"DT_ENTRY")[[1]] )
        g1 = g1 |> dygraphs::yAxis("y2", independentTicks=TRUE, drawGrid = FALSE, label=paste0(splitcol,sep=" "))  |> dySeries( splitcol, axis="y2", strokeWidth=2)
    }
    rollpd=NA
    if(roller=="default") {
        rollpd=as.numeric(as.character(cut(as.numeric(max(indtdts,na.rm=T)-min(indtdts,na.rm=T)),breaks=c(1,360,3600,999999),labels=c(1,5,20)))) }
    if(roller=="finest") { rollpd=1 }
    # Annotations: last,last | last,label | data.frame
    if (is.data.frame(miscannotation)) {
        #cAssign("indt;lastlabs;miscannotation")
        lastlabdf = data.frame(lastlabel=lastlabs, tlabeladd=rep("",length(lastlabs)))
        lastlabdf %<>% left_join(miscannotation,by="lastlabel") |> mutate(lastlabel = paste(lastlabel,value))
        lastlabs = dplyr::pull(lastlabdf,lastlabel)
        miscannotation="last,label"
    }
    if(grepl("^last",miscannotation)) {
        thisanno = gsub("last,","",miscannotation)
        annodate = min(indtdts[length(indtdts)],dtsrange[2])
        #cAssign("miscannotation;lastobs;g1;dtsrange;thisanno;annodate")
        for(irow in 1:length(lastobs)) {
            if(thisanno=="label") {
                lastlabs[irow] = paste(lastlabs[irow], round(lastobs[irow],digits=1)) }
            else {
                g1 = g1 |> dygraphs::dyAnnotation(annodate, ifelse(thisanno=="last",round(lastobs[irow],digits=1), thisanno), series=lastlabs[irow])
                }
        }
    }
    if(!is.logical(hlineopts) & is.logical(hairopts)) {
        if(any(grepl("last",hlineopts)) & length(lastobs)<=5) {
            if(wasmelted) {
                for(irow in 1:length(lastobs)) {
                    g1 = g1 |> dygraphs::dyLimit(as.numeric(lastobs[irow]), label=lastlabs[irow], labelLoc="left", color=colors[irow]) }
            }
            else if (length(lastobs)>=1) {
                if(is.data.frame(lastobs)) {
                    ltmp=as.numeric(select(lastobs,-starts_with("DT")) |> slice(1))
                    lastlabs = grep("^DT",colnames(lastobs),value=T,invert=T)
                    }
                else {
                    ltmp=as.numeric(lastobs)
                    lastlabs = round(ltmp,digits=1)
                    colors=rep("blue",length(ltmp))
                    }
                for(irow in seq(1,length(ltmp))) {
                    g1 = g1 |> dygraphs::dyLimit(as.numeric(ltmp[irow]), label=lastlabs[irow], labelLoc="left", color=colors[irow]) }
            }
        }
        if(any(grepl("^[[:digit:]]+$",hlineopts))) {
            hnums=hlineopts[grepl("^[[:digit:]]+$",hlineopts)]
            for(irow in hnums) {
                g1 =g1 |> dyLimit(as.numeric(irow),color="red") }
            }
        if(any(grepl("hlinedsn",hlineopts)) & is.data.frame(hlinedsn)) { # of form Variable,value
            for(irow in 1:nrow(hlinedsn)) {
                tcolor = ifelse("color" %in% colnames(hlinedsn), hlinedsn[[irow,"color"]],"red")
                tpos = ifelse("loc" %in% colnames(hlinedsn), hlinedsn[[irow,"loc"]],"left")
                g1 = g1 |> dygraphs::dyLimit(hlinedsn[[irow,"value"]], label=hlinedsn[[irow,"variable"]], labelLoc=tpos, color=tcolor)
                }
        }
    }
    g1 = g1 |> dygraphs::dyRangeSelector(height=20,dateWindow=dtsrange)

    if(suppressWarnings(!is.na(as.numeric(roller)))) { rollpd = as.numeric(roller)}
    if(!is.na(rollpd)) { g1 = g1 |> dygraphs::dyRoller(rollPeriod=rollpd) }
    if(nchar(dylegend)>0)  { g1= g1 |>  dygraphs::dyLegend(width=600, show=dylegend,hideOnMouseOut = FALSE)  }
    return(g1)
}


#Line plots
fgts_ggplot <- function(dta,title="",ylabel="",xlabel="",cutdata="",cutset="",cutname="",cutcolors="",linetypename="",ycoord=c(-Inf,+Inf),hlines=c(),panelvar="",colorvar="variable",
                printlastdata="",boldline="",cutsuffix="",savetitle="",lastoffset=5,legend="topleft",alreadymelted=FALSE,fcstpds=0,
                cornerannotations=NULL, colorset=c(s("black;red;green;blue;magenta;grey30;purple2;turquoise;pink;orange;dark green;light blue"),cbbPalette)) {
    mindt<-min(zoo::index(dta))
    dta<-xts2df(dta)
    if(!("DT_ENTRY" %in% colnames(dta))) { dta$DT_ENTRY<-as.Date(rownames(dta)) }
    if(alreadymelted) {
        dtx<-dta }
    else {
        dtx<-dta |> select(which(sapply(.,class)=="numeric" | sapply(.,class)=="Date")) |> pivot_longer(-DT_ENTRY,names_to="variable")
        # SOmetimes  I fucking hate Hadley
        }
    yrng<-range(dtx$value,na.rm=T)
    drng<-range(dtx$DT_ENTRY,na.rm=T)
    if(nchar(boldline)>0) { dtx$sizfactor<-as.factor(grepl(boldline,dtx[,"variable"],perl=T)) }
    else {        dtx$sizfactor<-as.factor(1)        }
    # ----------------------------- linetype
    if (alreadymelted & linetypename %in% colnames(dta)) { dtx$ltype=dtx[,linetypename] }  else { dtx$ltype=1 }
    if (alreadymelted & panelvar %in% colnames(dta)) { dtx$panel=dtx[,panelvar] }  else { dtx$panel=NA }
    if(nchar(cutname)>0 & grepl(cutname,paste(colnames(dta),collapse=" "))) {
        cutdata<-dta[,c("DT_ENTRY",cutname)]
        cutdata$dtlag<-as.Date(c(NA,head(cutdata$DT_ENTRY,-1)))
        cutdata$cid	<-dta[,c(cutname)]
        cutnm <- cutname
        cids<-unique(cutdata$cid)
        cutdata$variable<-""
        cutdata$value<-0
    }
    else if(is.xts(cutdata) | is.data.frame(cutdata)) {
        cutdata<-as.data.frame(cutdata[paste0(mindt,'::'),])
        cutnm <- ifelse(nchar(cutname)>1,cutname,colnames(cutdata)[1])
        cutdata$DT_ENTRY<-as.Date(rownames(cutdata))
        cutdata$dtlag<-as.Date(c(NA,head(cutdata$DT_ENTRY,-1)))
        cutset<-c(-Inf,cutset,Inf)
        cids<-paste0("<",cutset[-1]," ",cutsuffix,sep="")
        cids[length(cids)]<-paste0(">",cutset[length(cutset)-1]," ",cutsuffix)
        cutdata$cid	<-cids[as.numeric(cut(cutdata[,1],cutset))]
        cutdata$variable<-""
        cutdata$value<-0
        }
    if (fcstpds>0) {  # New 21 JUl 2014
        fcstfunc=function(x) {
            fc1=try( forecast(as.ts(df2xts(x[,s("DT_ENTRY;value")])),h=fcstpds), silent=TRUE)
            if(!inherits(fc1, "try-error")) {
                fc2=as.data.frame(fc1)
                fc2$DT_ENTRY=bdayseq(max(x$DT_ENTRY),nrow(fc2))
                fcfirst=head(subset(x,select=s("variable;sizfactor;panel")),1)
                print(paste("FCST>", fcfirst[1,"variable"]," > ", fc1$model$method, " MAPE:",accuracy(fc1)[4]))
                fc2=merge(select(fc2,DT_ENTRY,value=contains("Point Forecast")),fcfirst) |> mutate(ltype=2) }
            else {  fc1=data.frame() }
        }
        dtxf=group_by(dtx,variable) |> do(fcstfunc(.))
        dtx=rbind(dtx,as.data.frame(dtxf))
    }
    #dtx$ltype=as.factor(dtx$ltype)
    dtx %<>% mutate(xcolorvar = !!rlang::sym(colorvar))
    #dtx$xcolorvar=dtx[,colorvar]
    u1<-ggplot(dtx,aes(x=DT_ENTRY,y=value))
    if(nchar(printlastdata)>0) {
        ulast=group_by(dtx,variable) |> filter(DT_ENTRY==max(DT_ENTRY)) |> select(variable,yy=value,DT_ENTRY)
        if(grepl("^var",printlastdata,perl=T)) { ulast$ss = ulast$variable }
        else { ulast$ss = sprintf(printlastdata,ulast$yy) }
        if(abs(lastoffset)>0) { ulast$DT_ENTRY=ulast$DT_ENTRY+lastoffset}
        u1<-u1+geom_text(aes(x=DT_ENTRY,y=yy,color=variable,label=ss,group="Region"),data=ulast, check_overlap=TRUE,position = position_jitter(width=5, height=0))
    }
    if(length(unique(dtx$ltype))>1) {
        u1<-u1+geom_line(aes(size=sizfactor,color=xcolorvar,linetype=ltype))+scale_linetype_manual(values=c("solid", "dotted","dotdash")) }
    else {
        u1<-u1+geom_line(aes(size=sizfactor,color=xcolorvar))    } # Easier than turning guides off
    if(is.data.frame(cutdata)) {
        u1<-u1+geom_rect(data=cutdata,aes(xmin=dtlag,xmax=DT_ENTRY,fill=cid),ymin=-Inf,ymax=Inf,alpha=0.2,linetype=0)
        if(length(cutcolors)>1) {
            u1<-u1+scale_fill_manual(name=cutnm,values=cutcolors) }
        else {
            if(length(cids)<=2) {	u1<-u1+scale_fill_manual(name=cutnm,values=c("red","green")) }
            else if (length(cids)==3) { u1<-u1+scale_fill_manual(name=cutnm,values=c("red","white","green")) }
            else if (length(cids)==4) { u1<-u1+scale_fill_manual(name=cutnm,values=c("red","white","green","blue")) }
            else {	 u1<-u1+scale_fill_brewer(name=cutnm,type="seq");		}
        }
    }
    if(length(ycoord)==2) { u1<-u1+scale_y_continuous(limits=ycoord)}
    else if (!is.na(dtx[1,"panel"])) {
        u1=u1+facet_grid(panel~., scales="free_y")+theme(strip.text.y = element_text(size=11,color="black", face="bold"), strip.background=element_rect(fill="wheat")) }
    else {
        u1<-u1+coord_cartesian(ylim = c(0.95,1.05)*yrng) }
    if(length(levels(dtx$sizfactor))>1) { u1<-u1+scale_size_manual(values=c(1,1.6)) }
    else { u1<-u1+scale_size_manual(values=c(1,1.6),guide="none") }
    u1<-u1+BaseTheme()+scale_color_manual(values=colorset)
    for(ln in hlines) { u1<-u1+geom_hline(yintercept=ln,color="blue")    }
    title=c(title,"")
    u1=u1+labs(title=title[[1]],subtitle=title[[2]])
    psave(savetitle,u1)
}
# mtsrecentboxplot:
# colrename=c("fromcol","tocol"), premeltcols=c(melted cols), cutcol= factor to overrride breaks on date
# ycoord = trim coordinates



# labs doesnt work
# Bugs here with colors

BaseDefaults <- function(colordefaults=hicPalette, flip=FALSE,...) {
    bd=list(
        scale_fill_manual(values=colordefaults),
        scale_color_manual(values=colordefaults),
        scale_shape_manual(values=c(16,1,17,2,15,0,18,5)),
        BaseTheme(...))
    if(flip) {
        bd=c(bd,coord_flip()) }
    # if(nchar(title)>0) { bd=c(bd,labs(title=title)) }
    bd
}


dottedGrid<-function() { theme(
            panel.grid.minor.y=element_blank(), panel.grid.major.y=element_line(colour = 'gray', linetype = 'dashed'),
            panel.grid.minor.x=element_blank(), panel.grid.major.x=element_line(colour = 'gray', linetype = 'dashed')
            ) }




# adddoi: add recents to date stream
# annotionstyle : continuous, single, singleasdate
addonebreakout<-function(x,annotationstyle="continuous") {
    require(BreakoutDetection)
    res=breakout(na.omit(as.numeric(x[[2]])), min.size=24, method='multi', degree=1, beta=0.005, plot=FALSE)
    #print(paste("Breakout determines ", length(res$loc), "breakouts"))
    #print(paste("Breakout determines ", length(res$loc), "breakouts"))
    x$breakout<-NA;
    if(annotationstyle=="singleasdate") {
        x[res$loc,"breakout"]<-lapply(x[res$loc,"DT_ENTRY"],as.character) }
    if(annotationstyle=="single") {
        x[res$loc,"breakout"]<-"Brk" }
    if(annotationstyle=="continuous") {
        x[res$loc,"breakout"]<-paste0("rgm:",1:length(res$loc))
        x$breakout <- na.locf(x$breakout) }
    if(annotationstyle=="periodsonly") {
        x = x[res$loc,"DT_ENTRY"][,`:=`(DT_BEG=fcoalesce(lag(DT_ENTRY,1),min(x$DT_ENTRY)))] }
    x
    }

overlay_eventset<-function(indta,valname="value",ncutsperside=4,colorsign="redisnegative",center=0,melted=FALSE,alpha=0.2) {
    if(melted) {
        tmpdta = copy(indta)[variable==valname][,.(DT_ENTRY,xval=value)]
    }
    else {
        tmpdta = copy(indta)[,xval:=get(valname)]
    }
    if(is.numeric(center)) { xcenter=center }
    else if (center=="median") { xcenter = median(tmpdta[["xval"]]) }
    else if (center=="zscore") {
        xcenter = 0
        tmpdta[["xval"]]=as.vector(scale(tmpdta[["xval"]]))
    }
    else if (center=="zscorezero") {
        xcenter = 0
        tmpdta[["xval"]]=tmpdta[["xval"]]/sd(tmpdta[["xval"]],na.rm=T)
    }
    tmpp = tmpdta[xval>xcenter,][,tmpcat := as.numeric(cut_interval(xval,ncutsperside))]
    tmpn = tmpdta[xval<=xcenter,][,tmpcat := -1*( as.numeric(cut_interval(-xval,ncutsperside)))]
    tmpall = rbindlist(list(tmpn,tmpp))[order(DT_ENTRY)]

    hicolor = ifelse(colorsign=="redisnegative", "#6161ff", "#f56462")
    locolor = ifelse(colorsign=="redisnegative", "#f56462", "#6161ff")
    colorset = rbind( data.table(value=seq(1,ncutsperside),color=colorRampPalette(c("#ffffff",hicolor),alpha=alpha)(ncutsperside)),
                        data.table(value=seq(-ncutsperside,-1),color=colorRampPalette(c(locolor,"#ffffff"),alpha=alpha)(ncutsperside)))
    tmpruns = colorset[tmpall[,runs_from_value(.SD[,.(DT_ENTRY,value=tmpcat)],addrunlength=TRUE)],on=.(value)][,END_DT_ENTRY:=END_DT_ENTRY+1]
    return(tmpruns)
}

