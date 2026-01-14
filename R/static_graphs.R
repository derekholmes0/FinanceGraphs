library("ggplot2")
suppressPackageStartupMessages(library(plotly))
suppressPackageStartupMessages(library(rmarkdown))
library("DT")


fg_scatplot<-function(type,dta,xnm="",ynm="",
# Big picture aggregators
        doi="",datecuts=c(7,66),datecutlabels=NULL,aesds=NULL,
# Aesthetic columns
        colorcol="",labelcol="",labelcolor="",symbolcol="",tooltipcol="",circlesizecol="",alphacol="",colorcol2="",
# Aesthetic options
        tsize=3,psize=1,ellipse=F, repel=F,glinex=NA_character_,gliney=NA_character_, glineidentify=FALSE, xlabeldecoration="",
# Decorations/labels
        title="",caption=NULL,boundbox="",boundboxtype="none",jitter=c(0,0),keepcols="",gridstyle="",legend="inside",axislabels="",
        colorset=NULL,method="loess",tformula=formula("y~x"),sizetrans="identity",returnregresults=FALSE,pointformoverride=NA_character_,
        labelhilight="", labelhilightcolor="yellow",outlinetext="none",cornercolors="",annotatecorners="",
# DEbug/Misc
        showregline="lm", savetitle="",verbose=F) {
    if(nrow(dta)<=0) { return(ggplot()) }
    require("splines")
    sizefac = alphafac = xcolfaclist = c(1)
    addscales = list("size"=TRUE, "color"=TRUE, "alpha"=TRUE,"shape"=FALSE)
    dolastanyway=FALSE

    a2<-copy(as.data.table(dta))
    regres=data.table()
    dtcolno = purrr::detect_index(a2,is.Date)[[1]]
    colnamesnodate = colnames(a2)[setdiff(1:ncol(a2),dtcolno)]
    if( !(xnm %in% colnamesnodate)) { xnm=colnamesnodate[1] }
    if( !(ynm %in% colnamesnodate)) { ynm=colnamesnodate[2] }
    if( dtcolno>0 ) { a2$dt=a2[,..dtcolno] }
    else {
        if(is.na(lubridate::parse_date_time(rownames(a2)[1], "%Y-%m-%d", quiet=TRUE))) { a2$dt=rownames(a2) }
        else {
            dateinrownames = as.Date(rownames(a2))
            if( !is.Date(dateinrownames[[1]])) { dateinrownames=seq(1,nrow(a2)) }
            a2$dt<-dateinrownames
        }
    }
    do_not_fill = grepl("nofill",type)
    pointform <- case_when(
        labelcol %in% colnames(a2) ~ "text",
        tooltipcol %in% colnames(a2) ~ "tips",
        TRUE ~ "point"
    )

    a2<-a2[!is.na(a2$dt)]
    a2=dtsetnm(a2,xnm,"xx",existordie=TRUE)
    a2=dtsetnm(a2,ynm,"yy",existordie=TRUE)
    a2=dtsetnm(a2,colorcol,"colfactor",nullcol="1")
    a2=dtsetnm(a2,symbolcol,"symbolfactor",nullcol="1")
    a2=dtsetnm(a2,labelcol,"labels",nullcol="1")
    a2=dtsetnm(a2,tooltipcol,"tooltips",nullcol="1")
    a2=dtsetnm(a2,labelcolor,"labelcolor",nullcol="1")
    a2=dtsetnm(a2,labelhilight,"labelhilight",nullcol="")
    a2=dtsetnm(a2,circlesizecol,"sizefactor",nullcol="1")
    a2=dtsetnm(a2,alphacol,"alphafactor",nullcol="1")

    thismaxdt=max(a2$dt)
    if(is.data.frame(doi)) {
        a2 = left_join(a2, mutate(doi,dt=doi,doino=row_number()), by="dt")
        a2[1,c("colfactor","colfactorval")]<-c("-",a2[1,"dt"])
        a2$colfactor<-na.locf(a2$doidesc)
        a2$colfactorval<-na.locf(a2$doi)
    }

    if(nchar(doi)>1) {
        datecutlabels=c()
        if(grepl("rec",doi)) {
            pointform <- "hex"
            datecuts = c(0,datecuts[1:2]) }
        if(grepl("last",doi)) {
            datecuts = c(0) }
        if(grepl("text",doi)) {
            pointform <- "text"
        }
        if(grepl("^regm",doi)) {
            xdates   = filter(doidates,grepl(paste0("^",s(doi)[[2]],"$"),category))
            if( nrow(xdates)>0) {
                xdates %<>% ungroup %>% top_n(2,DT_ENTRY) %>% mutate(daysback=as.numeric(thismaxdt)-as.numeric(DT_ENTRY))
                datecuts = xdates %>% pull(daysback)
                datecuts = c(0,sort(as.numeric(datecuts)))
                datecutlabels = c("Last",pull(xdates,eventid),"older")
                dolastanyway<-TRUE
                pointform <- "point"
            }
        }
        a2$colfactor = make_datecutlabels(a2,datecuts, lastonly=grepl("last",doi),labels=datecutlabels)
    }
    a3<-a2[,.(xx,yy,colfactor,labels,labelcolor,sizefactor,alphafactor,symbolfactor,dt,tooltips,labelhilight)]
    a3<-a3[,`:=`(colfactor=as.factor(colfactor),symbolfactor=as.factor(symbolfactor))]

    if(nchar(keepcols)>0) {
        a3 = cbind(a3, select(dta,!!{intersect(keepcols,colnames(dta))}))
    }
    bbx<-range(a3$xx,na.rm=T)*c(0.99,1.01)
    bby<-range(a3$yy,na.rm=T)*c(0.99,1.01)
    if(length(boundbox)==2) {
        if(grepl("value",boundboxtype)) { bby=boundbox }
        else {
            bbx<-quantile(a3[["xx"]],probs=c(boundbox[1],1-boundbox[1]),na.rm=T)*c(0.95,1.05)
            bby<-quantile(a3[["yy"]],probs=c(boundbox[2],1-boundbox[2]),na.rm=T)*c(0.95,1.05)
        }
    }
    if(length(boundbox)==4) {
        if(grepl("value",boundboxtype)) {
            bbx=c(boundbox[1],boundbox[2])
            bby=c(boundbox[3],boundbox[4])
            }
        else {
            bbx<-quantile(a3[["xx"]],probs=c(boundbox[1],boundbox[2]),na.rm=T)*c(0.95,1.05)
            bby<-quantile(a3[["yy"]],probs=c(boundbox[3],boundbox[4]),na.rm=T)*c(0.95,1.05)
        }
    }
    if(length(boundbox)>1 & verbose) { message("Bounding Boxes ",boundboxtype," gives (x,y)=",paste(round(c(bbx,bby),4),collapse=" "))}
    #a3[which(a3[,1]>=bbx[1] & a3[,1]<=bbx[2] & a3[,2]>=bby[1] & a3[,2]<=bby[2]),"inbox"]<-TRUE
    bbx = sort(bbx); bby=sort(bby)
    a3[,inbox:=(between(xx,bbx[1],bbx[2]) & between(yy,bby[1],bby[2]))]
    if(!grepl("graphidentify|none",boundboxtype,ignore.case=T)) {
        a3<-a3[inbox==TRUE,]
        if(verbose) { message("bbx, bby:",bbx," : ",bby, " gives ",nrow(a3)," obs out of ",nrow(a2)," orig") }
        }
    if (grepl("^(path)",type)) {# --- Need to finish -- at some point. idea is to make a path out of weekly or monthly observations
        pointform<-""
        addscales$size<-FALSE
        tmpfunc<-function(x) {
            u1=mutate(x,islast=(dt>(max(dt)-6)))
            bind_rows(toseasonaldates(filter(u1,!islast),dtname="dt",toadd="yrwk",rtn="last"),filter(u1,islast) %>% mutate(yrwk=0,colorfac="dly")) }
        a2a=group_by(a3,colfactor) %>% do({tmpfunc(.)}) %>%
            mutate(colorfac=case_when(row_number()<(n()-3) ~ "-", row_number()==n() ~ "lastx", TRUE ~ "last3"))
        a2a$colorfac=as.factor(a2a$colorfac)
        a3$colfactor = make_datecutlabels(a2,c(0))
        a2a = adddoidates(ungroup(a2a) %>% mutate(DT_ENTRY=dt))
        colorset=s("blue;red")
        p<-ggplot(a2a[,dtback:=.I],aes(x=xx,y=yy))+geom_path(aes(linewidth=dtback))+scale_linewidth(range=c(0.5,1.5))
        p<-p+geom_point(aes(x=xx,y=yy,color=colorfac),data=subset(a2a,!(colorfac=="-")),size=4)
        }
    else {
        p<-ggplot(a3,aes(x=xx,y=yy,show.legend=TRUE)) }
   # Actual Plot
    pointform = fcoalesce(pointformoverride,pointform)

    if(length(s(annotatecorners))==4) {
        xrange=bbx*0.8; yrange=bby*0.8
        annodf = data.frame(x=c(xrange[1],xrange[2],xrange[2],xrange[1]),y=c(yrange[2],yrange[2],yrange[1],yrange[1]),text=s(annotatecorners))
        annodf %<>% filter(!(text=="." | text=="null"))
        #if(length(s(cornercolors))==4) {  annodf$xcolor = s(cornercolors) }
        #else { annodf$xcolor=rep("orange",4) }
        #p<-p+geom_text(aes(x,y,label=text),data=annodf,vjust="inward",hjust="inward",color="gray",size=tsize,alpha=0.8,fontface="bold",family="sans")
        p<-p+geom_label(aes(x,y,label=text),data=annodf,vjust="inward",hjust="inward",color="darkgreen",fill="yellow",size=tsize*0.8,alpha=0.8,fontface="bold",family="sans")
    }


    tsizes=c(6,3,2,rep(1,10))
    if(is.null(colorset)) {
        tcolors=c(s("black;red;magenta;dark green"),rev(hicPalette)) }
    else {
        tcolors = colorset
    }
    xcolfaclist_1=a3[,.N,by=.(colfactor)][,usehex:=(N>200)] %>% separate(colfactor,c("orig_cf","dt_cf"),sep=",",fill="left",remove=F) %>% as.data.table  # Broke 1/30 with new tidyr
    # A hack, cuz I bit off mroe than I can chew
    if(length(unique(xcolfaclist_1$orig_cf))>1) {
        xcolfaclist_agg = xcolfaclist_1[,.SD[1][,.(xx=N)],by=.(orig_cf)][,`:=`(sizeno=tsizes[.N-.I+1], colorno=tcolors[.N-.I+1])][]
        xcolfaclist = xcolfaclist_agg[xcolfaclist_1,on=.(orig_cf)] }
    else {
        #xcolfaclist_agg = xcolfaclist_1[,.SD[1][,.(xx=N)],by=.(colfactor)][,`:=`(sizeno=tsizes[.N-.I+1], colorno=tcolors[.N-.I+1])][]
        xcolfaclist_agg = xcolfaclist_1[,.SD[1][,.(xx=N)],by=.(colfactor)][,`:=`(sizeno=tsizes[.N-.I+1], colorno=tcolors[.I])][]
        xcolfaclist = xcolfaclist_agg[xcolfaclist_1,on=.(colfactor)]
        #cAssign("xcolfaclist_1;xcolfaclist_agg;xcolfaclist")

    }
    a3=xcolfaclist[,.(colfactor,colorno,sizeno,orig_cf)][a3,on=.(colfactor)][]
    lastdta = a3[nchar(as.character(colfactor))>1,][,.SD[.N],by=.(colorno)]

    lmcolorcol="colorno"
    #cAssign("p;xcolfaclist;lmcolorcol;pointform;a3")

    if(pointform=="text") {
        if(!(labelcolor %in% colnames(dta))) { a3=a3[,`:=`(labelcolor=colfactor)] }
        pjit = position_jitterdodge(jitter.width=jitter[1],dodge.width=jitter[2])
        lmcolorcol="labelcolor"
        #cAssign("xcolfaclist;xcolfaclist_1;xcolfaclist_agg;a3;pointform;labelcolor;lmcolorcol")
        if(outlinetext=="none" & repel==TRUE) {
                p<-p+geom_text_repel(aes(x=xx,y=yy,label=labels,color=labelcolor),size=tsize,fontface="bold",max.overlaps=60,data=a3) }
        if(outlinetext=="none" & repel==FALSE) {
                p<-p+geom_text(aes(x=xx,y=yy,label=labels,color=labelcolor),size=tsize,fontface="bold",data=a3,check_overlap=!(repel=="nooverlap"),position=pjit)  }

        if(outlinetext=="box" & repel==TRUE) {
                p<-p+geom_label_repel(aes(x=xx,y=yy,label=labels,color=labelcolor),size=tsize,fontface="bold",max.overlaps=60,data=a3) }
        if(outlinetext=="box" & repel==FALSE) {
                p<-p+geom_label(aes(x=xx,y=yy,label=labels,color=labelcolor),size=tsize,fontface="bold",data=a3,position=pjit) }

        if(outlinetext=="hilight" & repel==TRUE) {
                a3= a3[,':='(runhilight=!(is.na(labelhilight) | labelhilight==""))]
                p<-p+geom_text_repel(aes(x=xx,y=yy,label=labels,color=labelcolor),size=tsize,fontface="bold",max.overlaps=60,data=a3[runhilight==FALSE])
                p<-p+geom_label_repel(aes(x=xx,y=yy,label=labels,color=labelcolor),size=tsize,fontface="bold",max.overlaps=60,fill=labelhilightcolor,alpha=0.6,data=a3[runhilight==TRUE])
                }
        if(outlinetext=="hilight" & repel==FALSE) {
                a3= a3[,':='(runhilight=!(is.na(labelhilight) | labelhilight==""))]
                p<-p+geom_text(aes(x=xx,y=yy,label=labels,color=labelcolor),size=tsize,fontface="bold",data=a3[runhilight==FALSE])
                p<-p+geom_label(aes(x=xx,y=yy,label=labels,color=labelcolor),size=tsize,fontface="bold",fill=labelhilightcolor,alpha=0.6,data=a3[runhilight==TRUE])
                }
        }
    else {
        if(grepl("join",type)) {
            addscales$shape=TRUE
            p<-p+geom_line(aes(x=xx,y=yy,color=colfactor,linewidth=sizefactor,alpha=alphafactor,shape=symbolfactor),show.legend=TRUE,data=a3)
            pointform <-""
            }
        if(grepl("dens",type)) {
            if( do_not_fill ) {
                p<-p+geom_density_2d(aes(x=xx,y=yy,color=colfactor),size=1.2,data=a3) }
            else {
                p<-p+geom_density_2d_filled(aes(x=xx,y=yy,color=colfactor),size=1.2,data=a3) }
            pointform <-""
            }
        if( pointform=="point") {
            addscales$shape=TRUE
            addscales$color=FALSE
            p<-p+geom_point(aes(x=xx,y=yy,color=colorno,size=sizefactor,alpha=alphafactor,shape=symbolfactor),data=a3)  # THIS SUCKS
            cset = xcolfaclist$colorno
            names(cset)=xcolfaclist$colorno
            p<-p+scale_color_manual("XXXX",values=cset, labels=xcolfaclist$colfactor[order(xcolfaclist$colorno)],guide="legend")
            }
        if( pointform=="tips") {
            p<-p+geom_point_interactive(aes(tooltip=tooltips, data_id=tooltips), size=2, color="gray", alpha=0.01, show.legend=FALSE)
            }
        if( pointform=="hex") {  # Heres where it gets complicated.  I need diffent colors/scales for each factor
            fillcolors = c("lightblue","lightgreen","pink","orange")
            for( j in 1:nrow(xcolfaclist)) {
                tdata = a3[colfactor==xcolfaclist[j]$colfactor]
                if (xcolfaclist[j]$usehex) {
                    addscales$alpha<-FALSE
                    if(j==1) {  p<-p+geom_hex(aes(x=xx,y=yy),data=tdata[,.(xx,yy)],alpha=0.7)+scale_fill_gradient(low="gray80",high="gray20") }
                    else { p<-p+geom_hex(data=tdata[,.(xx,yy)],fill=fillcolors[[j-1]],alpha=0.3) }
                }
                else { # Points
                    addscales$shape=TRUE
                    p<-p+geom_point(aes(x=xx,y=yy,alpha=alphafactor,shape=symbolfactor),data=tdata,size=xcolfaclist[[j,"sizeno"]],color=xcolfaclist[[j,"colorno"]])
                }
            }
        }
    }
    #cAssign("p;pointform;a3;xcolfaclist;bbx;bby;p",title="Aftr Points")
    # summary lines
    if(grepl("(lm|loess)",type)) {
        if(returnregresults) {
            if(grepl("one",type)) {
                regres = a3 %>% do({lma=lm(tformula,data=rename(.,y=yy,x=xx)); broom::tidy(lma)}) %>% mutate(p.value=round(p.value,5)) }
            else {
                regres = group_by(a3,colfactor) %>% do({lma=lm(tformula,data=rename(.,y=yy,x=xx)); broom::tidy(lma)}) %>% mutate(p.value=round(p.value,5))
            }
        }
        actmethod =  fcoalesce(stringi::stri_extract(regex="(lm|loess)",type),"lm")
        talpha    = ifelse(do_not_fill,0,0.2)
        if(verbose & !(tformula==formula("y~x"))) {      print(summary(lm(tformula,data=a3[,.(x=xx,y=yy)]))) }
        if( grepl("one",type) ) {
            addscales$alpha<-FALSE
            p<-p+geom_smooth(method=actmethod,formula=tformula,colour="black",alpha=talpha,show.legend=FALSE,data=a3) }
        else {
            p<-p+geom_smooth(method=actmethod,formula=tformula,aes(colour=.data[[lmcolorcol]]),alpha=talpha,show.legend=FALSE,data=a3)
        }
        if(actmethod==showregline & !grepl("noeq",type)) {
            legx<-data.frame(xx=quantile(a3[["xx"]],probs=c(0.2),na.rm=T),yy=quantile(a3[["yy"]],probs=c(0.99),na.rm=T),labels=lm_eqn(a3,"xx","yy",rtnstyle="simplewitht"))
            p<-p+geom_text(aes(x=xx,y=yy,label=labels,hjust=0),data=legx,size=2,show.legend=FALSE)
        }
    }


    if(FALSE) { paftsum = p; cAssign("paftsum;doi;addscales"); }
    # Additional figures
    if(ellipse) {   p<-p+stat_ellipse(aes(color=colfactor))   }
    if(grepl("last",doi) | dolastanyway) {
        p<-p+geom_point(aes(x=xx,y=yy,color=.data[[lmcolorcol]]), data=lastdta, size=5)
        p<-p+geom_label_repel(aes(x=xx,y=yy,label=orig_cf,color=.data[[lmcolorcol]]),size=tsize,max.overlaps=40,data=lastdta)
        legend=paste0(legend,"nocolor")
    }

     if(addscales$size) {
        nsizes =length(unique(a3$sizefac))
        if(nsizes<=10) {
            p<-p+scale_size_manual(values=psize*seq(nsizes)/nsizes,name="ms2_size") }
        else {
            p<-p+scale_size_continuous(range=c(2,8),trans=sizetrans)
        }
    }

    if(addscales$alpha) {
        nalphas=length(unique(a3$alphafac))
        p<-p+scale_alpha_manual(values=seq(nalphas)/nalphas,name="ms2_alp")
    }

    if(grepl("graphidentify",boundboxtype,ignore.case=T)) {
        a3b=filter(a3,is.na(inbox) | !inbox) %>% rowwise %>% mutate(xx=pmax(pmin(xx,bbx[[2]]),bbx[[1]]), yy=pmax(pmin(yy,bby[[2]]),bby[[1]]))
        if(pointform=="text") {
            pjit = position_jitterdodge(jitter.width=jitter[1],dodge.width=jitter[2])
            #p<-p+geom_text_repel(aes(x=xx,y=yy,label=labels,color=labelcolor),size=tsize*0.6,fontface="italic",max.overlaps=40,data=a3b)
            caption=paste(caption,",Beyond borders in italics")
            }
        else {
            p<-p+geom_point(aes(x=xx,y=yy),data=a3b,size=3)
            }
        p<-p+coord_cartesian(xlim=round(bbx,0),ylim =round(bby,0))
        }
    else if(grepl("graph",boundboxtype,ignore.case=T)) {
        p<-p+coord_cartesian(xlim=round(bbx,0),ylim =round(bby,0)) }

    if(length(s(axislabels))==2) { xnm<-axislabels[1]; ynm<-axislabels[2] }
    if(length(s(xlabeldecoration))==2) { xnm<-paste0(s(xlabeldecoration)[1]," <-- ",xnm," --> ",s(xlabeldecoration)[2]) }
    if(is.numeric(glinex)) { p<-p+gline_x(int=glinex) }
    if(is.numeric(gliney)) { p<-p+gline_y(int=gliney) }
    if(!is.na(glinex=="last" || gliney=="last")) {
        for (irow in 1:nrow(lastdta)) {
            if (glinex=="last") { p<-p+gline_y(int=lastdta[[irow,"xx"]]) }
            if (gliney=="last") { p<-p+gline_x(int=lastdta[[irow,"yy"]]) }
        }
    }

    if(is.numeric(glineidentify)) { p<-p+gline_identify(p,glineidentify[1],glineidentify[2]) }
    if(glineidentify==TRUE) {
        # Hack: programmed myself into a corner..
        if(nchar(doi)>0) { lastdta = a3[nchar(as.character(colfactor))>2,][,.SD[.N],by=.(colorno)] }
        else {        lastdta = a3[,.SD[.N],by=.(colorno)] }
        for (irow in 1:nrow(lastdta)) {    p<-p+gline_identify(p,lastdta[[irow,"xx"]],lastdta[[irow,"yy"]],color=lastdta[[irow,"colorno"]]) }
    }

    title=c(title,"")
    p<-p+labs(title=title[[1]],subtitle=title[[2]], caption=caption,x=xnm, y=ynm)
    if(addscales$shape) {  p<-p+scale_shape_manual(values=c(16,1,17,2,15,0,18,5),name="ms2_shape",labels=levels(a3$symbolfactor)) }
    if(addscales$color) {  p<-p+scale_color_manual(values=tcolors,name="ms2_color",labels=levels(a3$colfactor)) }
    #pcopy<-p
    #cAssign("legend;addscales;pcopy")
    p<-p+BaseTheme(gridstyle=gridstyle,legend=legend)
    p<-p+guides(alpha="none",
                fill=ifelse((grepl("nofill",legend) | length(unique(a3$colfactor))==1),"none","legend"),
                colour=ifelse((grepl("nocolor",legend) | length(unique(a3$colfactor))==1),"none","legend"),
                size=ifelse((grepl("nosize",legend) | length(unique(a3$sizefactor))==1),"none","legend"),
                shape=ifelse((grepl("noshape",legend) | length(unique(a3$symbolfactor))==1),"none","legend"))
     # UL corner, UR corner, LR corner, LL corner
    if(returnregresults) {  return(list(p,regres))    }
    else {    psave(savetitle,p) }
}




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



#timeit<-function(x) { if(x=="start"){ xtstamp<<-Sys.time(); message("start") } else { message("Time of (",x,"): ", Sys.time()-xtstamp); xtstamp<<-Sys.time() } }
# Latest
fgts_seasonal <-function(xindta,yvar,title="",seasonaltype="roll",normalize="", graphtype="line",projectfwd=F, captionlab="", periodset=NULL,
                yrange=NULL,colorscaletype="",dbg=FALSE,weekdaysonly=FALSE,line_on_lastdate=TRUE,killbad_eop=FALSE) {
    indta=data.table(xindta)
    if(!("rollpd" %in%  colnames(indta))) {
        if(seasonaltype=="roll") {
            #this takes a long time, and is already calcualted now
            #u2dts = full_join(
            #        tibble(DT_ENTRY=seq(ymd('2001-03-20'),ymd('2022-03-20'), by = '6 month'), rolldt=DT_ENTRY),
            #        tibble(DT_ENTRY=seq(min(indta$DT_ENTRY),max(indta$DT_ENTRY),by="day")), by="DT_ENTRY") %>% as.data.frame() %>%
            #            arrange(DT_ENTRY) %>% mutate(rolldt=na.locf(rolldt)) %>% mutate(rollpd=format(rolldt,"%Y%m"))
            #u2dts %<>% filter(DT_ENTRY<=Sys.Date()) %>% rowwise %>% mutate(daysfromroll=Nweekdays_dtset(rolldt,DT_ENTRY))
            #indta = left_join(indta,u2dts,by="DT_ENTRY")
            indta = dtmap[,.(DT_ENTRY,rolldt,rollpd,daysfromroll,yrwk)][indta,on="DT_ENTRY"]
        }
        else {
            varlist=c("DT_ENTRY","yrwk","isholiday",seasonaltype)
            xdtmap=dtmap[,..varlist]
            if(weekdaysonly) {   xdtmap = xdtmap[isholiday==FALSE,] }
            setnames(xdtmap,seasonaltype,"rollpd")
            dttmp=xdtmap[,rollpd:=as.character(rollpd)][,`:=`(daysfromroll=.I-.I[which.min(DT_ENTRY)],rolldt=min(DT_ENTRY)),by=.(rollpd)]
            #dttmp=xdtmap[,rollpd:=as.character(rollpd)][,`:=`(daysfromroll=Nweekdays(min(DT_ENTRY),DT_ENTRY),rolldt=min(DT_ENTRY)),by=.(rollpd)]
            indta = dttmp[indta,on="DT_ENTRY",nomatch=NULL]
        }
    }
    if( length(unique(indta$rollpd))<=1 ) {
        message("PlotOneSeasonal not enough data")
        return(NULL) }
    lastindex = indta[rollpd==max(rollpd),.SD[1]][[yvar]]
    if(normalize=="relative") {  # Data.table 6.5x faster
        #indta = group_by(indta,rollpd) %>% arrange(DT_ENTRY) %>% mutate(!!rlang::sym(yvar) := !!rlang::sym(yvar) - first(!!rlang::sym(yvar)) + lastindex)
        indta = indta[,zz:=(get(yvar) - first(get(yvar))), by=.(rollpd)]
        indta[,(yvar):=zz]
        colorscaletype="manual"
        captionlab = "Older cycles adjusted to match beginning of latest cycle"
    }
    if(normalize=="index") {
        indta = indta[,zz:=100*(get(yvar)/first(get(yvar))-1), by=.(rollpd)]
        indta[,(yvar):=zz]
        colorscaletype="manual"
        captionlab = "Expressed as index from beginning of each period"
    }
    if(is.data.frame(periodset)) {  # Still always want last period
        message("limiting to select periods")
        indta = inner_join(indta,periodset,by="rollpd")
    }
    ulast    = indta[,.SD[.N],by=.(rollpd)][,':='(rollsback=.N-.I+1)][,islastpd:=fifelse(rollsback==1,"LAST","--")]
    ulastdfr = ulast[,.SD[.N]][["daysfromroll"]]
    ulastval = ulast[,.SD[.N]][[yvar]]
    indta = ulast[,.(rollpd,rollsback,islastpd)][indta, on=.(rollpd)]
    if(killbad_eop) {
        baddays = indta[,.N,by=.(daysfromroll)][N<0.3*mean(N),]
        indta = indta[!baddays[,.(daysfromroll)],on=.(daysfromroll)]
    }
    lastpdreturn=filter(indta,islastpd=="LAST") %>% summarise(pdrtn=last(!!rlang::sym(yvar))-first(!!rlang::sym(yvar))) %>% pull(pdrtn)
    if(projectfwd) {
        wts = filter(indta, rollpd<max(rollpd)) %>% group_by(rollpd) %>% summarise(maxdt=max(DT_ENTRY)) %>% ungroup %>% arrange(rollpd) %>% mutate(ww=0.9^((n()-1):0))
        avgfwd = filter(indta,daysfromroll>ulastdfr[[1]]) %>% inner_join(select(wts,rollpd,ww),by="rollpd") %>%
                    group_by(rollpd) %>% mutate(rollchg=!!rlang::sym(yvar)-first(!!rlang::sym(yvar))) %>% group_by(daysfromroll) %>% summarise(mnrollchg=ulastval+weighted.mean(rollchg,ww))
        avgfwd = cbind(ungroup(avgfwd),ulast[.N,.(islastpd,rollsback,rollpd)])
    }
    if(grepl("stat|aggregate",graphtype)) {
        indta_tmp = copy(indta)[,.(DT_ENTRY,rollpd,daysfromroll,rolldt,yrwk,value=get(yvar),islastpd,variable=yvar,rollsback)]
        qtiles1   = indta_tmp[rollsback>1,][,as.list(quantile(value,c(0.1,0.5,0.9),na.rm=T)),by=.(variable,daysfromroll)]
        qtiles2   = indta_tmp[rollsback>1,][,.(DT_ENTRY=min(DT_ENTRY),rolldt=min(rolldt)),by=.(variable,daysfromroll)]
        qtiles    = qtiles2[melt(qtiles1,id.vars=c("variable","daysfromroll"),variable.name="rollpd"),on=.(variable,daysfromroll)]
        ulast     = indta[rollsback==1,.SD[.N]]
        indta     = rbindlist(list(indta_tmp[rollsback==1,],qtiles),use.names=T,fill=T)[order(rollpd,daysfromroll)]
        setnames(indta,"value",yvar)
        graphtype = "line"
        captionlab = paste(captionlab,"10,50,90th percentiles shown")
    }
    cAssign("indta;yrange;ulast",dbg=dbg)
    title=paste0(title, " PdToDt:",format(lastpdreturn,digits=3))
    if(length(yrange)==2) { indta = indta[between(get(yvar),yrange[[1]],yrange[[2]]),] }
    indta = indta[,':='(islastpd=fcoalesce(islastpd,"-"))]
    if(graphtype=="line") {
        g1=ggplot(indta,aes(x=daysfromroll,y=!!rlang::sym(yvar),color=rollpd,linewidth=islastpd))+geom_line(aes(group=rollpd))+scale_discrete_manual("linewidth",values=c(0.5,2,1,1,1))
        g1=g1+geom_label_repel(aes(x=daysfromroll,y=!!rlang::sym(yvar),label=rollpd),data=ulast,size=2.5,max.overlaps=50)+legendPosition("none")+labs(x="Days from Beg of Period")
    }

    if(graphtype=="hex") {
        nbins = as.numeric(c(s(graphtype),30)[[2]])
        g1=ggplot(indta,aes(x=daysfromroll,y=!!rlang::sym(yvar)))+geom_line(aes(group=rollpd),linewidth=2,data=indta[rollsback==1,])+geom_hex(data=indta[rollsback>1,],alpha=0.7,bins=nbins)+scale_fill_gradient(low="gray80",high="gray20")
        g1=g1+geom_label_repel(aes(x=daysfromroll,y=!!rlang::sym(yvar),label=rollpd),data=ulast[rollsback==1],size=2.5,max.overlaps=50)+legendPosition("none")+labs(x="Days from Beg of Period")
    }

    if(colorscaletype=="viridis") {
        g1=g1+scale_color_viridis_d(direction=-1) }
    else {
        g1=g1+scale_color_manual(values=hicPalette)
    }
    if(line_on_lastdate) {
        g1=g1+gline_y(int=ulastdfr)
    }
    if(projectfwd) {
        g1=g1+geom_line(aes(x=daysfromroll,y=mnrollchg),data=avgfwd, color="red", linewidth=1.5)
    }
    #cAssign("indta;last2rolls;colorscaletype;yvar;ulastdfr")
    if(nchar(captionlab)>0) { g1=g1+labs(caption=captionlab)}
    g1=g1 +ggtitle(title)+BaseTheme(legend="bottomleft")
    g1
}

catheatmap<-function(dta,catname,fillcol,facetname,title="",savetitle="",qset=c(0.15,0.3,0.7,0.85),normalize=F,print=F,invert=F,
            na.rm=T, labels="", revdt=F, placelabelsonlast=T) {
    dta=dta %>% rename(category=!!rlang::sym(catname),chg=!!rlang::sym(fillcol),GROUP=!!rlang::sym(facetname))
    if(na.rm) { dta=subset(dta,!is.na(category))}
    if(normalize) {
        dta=group_by(ungroup(dta),GROUP,DT_ENTRY) %>% mutate(chg=chg-mean(chg,na.rm=T))
        title=paste(title,"normalized by gp mean")  }
    if(nchar(head(labels,1))<=1) { labels=c(paste0("q",qset*100),"XHI") }
    dta2=group_by(ungroup(dta),GROUP,category) %>% filter(!is.na(chg)) %>% mutate(chg_ptle=cut(chg,quantile(chg,c(0,qset,1),na.rm=T),include.lowest=T,labels=labels))
    if(print) {
        dranges2=group_by(dta2,GROUP,category) %>% do(data.frame(qtile=quantile(.$chg,c(0,qset,1),include.lowest=T,na.rm=T), qname=c("LO",labels)))
        print(group_by(dranges2,GROUP,category) %>% do(pivot_wider(.,names_from="qname",values_from="qtile"))) }
    ddt=mutate(data.frame(DT_ENTRY=sort(unique(dta2$DT_ENTRY))),ww=c(0,diff(DT_ENTRY)))
    dta2=merge(dta2,ddt,by="DT_ENTRY")
    if(any(abs(qset-0.5)<0.0001)) {  colors=c("red","pink3","yellow","lightgreen","green1","green4") } else { colors=c("red","pink","white","lightgreen","green") }
    if(invert) { colors=rev(colors) }
    if((revdt)) { dta$DT_ENTRY=rev(as.factor(dta$DT_ENTRY)) }
    #cAssign("dta;labels;qset;dta2")
    g1=ggplot(dta2,aes(DT_ENTRY,category,fill=chg_ptle,width=ww))+geom_tile()
    if(placelabelsonlast) {
        d2=group_by(dta2,GROUP,chg_ptle,category) %>% summarise(ww=max(ww),DT_ENTRY=as.Date(floor(quantile(as.numeric(dta2$DT_ENTRY),0.9))))
        g1=g1+geom_text(aes(x=DT_ENTRY,label=category,width=ww),data=d2,alpha=0.7,size=3, check_overlap=TRUE,color="black")
    }
    g1=g1+scale_x_date(labels = date_format("%m-%d"), date_breaks = "1 week", date_minor_breaks = "1 week")
    g1=g1+facet_grid(GROUP~.,scales="free",drop=TRUE)+theme(strip.text.y = element_text(size=11,color="black", face="bold"), strip.background=element_rect(fill="wheat"))
    g1=g1+scale_fill_manual(values=colors)+BaseTheme()+theme(strip.text.y = element_text(size = 12,color="blue",face="bold"))+labs(title=title)
    #cAssign("g1")
    psave(savetitle,g1)
}

fgts_boxplot <- function(dt,title="",xlabel="value",ylabel="index",lastlabel="label",flip=F,reverse=F,colorstr="", violin=F, savetitle="",outliers=F,legend="") {
# GEt rid of bad columns
    if( ("variable" %in% colnames(dt)) & ("value" %in% colnames(dt)) ) {
        dml=group_by(dt,variable) %>% slice(n())
        dm=as.data.frame(dt)  }
    else {
       dt<-dt[,!is.na(apply(dt,2,sum))]
	   dm<-data.table::melt(as.data.table(dt))
	   dml<-data.table::melt(as.data.table(tail(dt,1)))
    }
	if(globaldebug[["grdbg"]]) { cAssign("dm;dml;dt")}
    if(nchar(colorstr)>0) { dm$col<-grepl(colorstr,dm$variable,perl=TRUE)}
    else { dm$col<-"" }
    if(violin) {
        b1<-ggplot(dm,aes(variable,value,color=col))+geom_violin(outlier.shape=ifelse(outliers,16,NA)) }
    else {
        b1<-ggplot(dm,aes(variable,value,color=col))+geom_boxplot(outlier.shape=ifelse(outliers,16,NA)) }
	if(reverse) { b1<-b1+scale_x_discrete(limits=rev(levels(dm$variable))) }
	if(grepl("label",lastlabel)) {
		dgno<-ifelse(min(dml$value,na.rm=T)<1,4,1)
		dml$valtxt<-ifelse(is.na(dml$value),"-",format(dml$value,digits=dgno))
		b2<-b1+geom_text(aes(variable,value,label=valtxt),data=dml,color="red",fontfamily='Helvetica', check_overlap=TRUE,fontface="bold",size=4)
    	title<-paste(title,"\nDot/No: Last observation")
	}
	else {
		if(grepl("point",lastlabel)) {
			dgno<-ifelse(min(dml$value)<1,4,1)
			dml$valtxt<-format(dml$value,digits=dgno)
			b2<-b1+geom_point(aes(variable,value),data=dml,color="red",size=4)
			title<-paste(title,"\nDot/No: Last observation")		}
    }
    title=c(title,"")
	b2<-b2+labs(title=title[[1]], x=xlabel, y=ylabel)
    b2<-b2+guides(fill=ifelse(grepl("empty|nofill",legend),"none","legend"), colour=ifelse(grepl("empty|nocolor",legend),"none","legend"), size=ifelse(grepl("empty|nosize",legend),"none","legend"), shape=ifelse(grepl("empty|noshape",legend),"none","legend"))
	if(flip) { b2<-b2+coord_flip()}
	b2<-b2+BaseTheme(gridstyle="dotted")
    psave(savetitle,b2,w=5,h=8)
}

moveBarChart<-function(dta,xname,yname,gpname,ynamedot="",ylim="",xlabel="",ylabel="",title="",usedot=TRUE) {
    if(nchar(ynamedot)<=1) { ynamedot=yname; usedot=FALSE }
    dt = dta[,c(xname,yname,ynamedot,gpname)]; colnames(dt)=c("x","y1","y2","gp")
    if(length(ylim)==2) { dt[,2:3]=apply(dt[,2:3],c(1,2),function(x){min(max(x,ylim[1]),ylim[2])}) }
    g1=ggplot(dt,aes(x=x,y=y1))
    g1=g1+geom_bar(aes(fill=factor(gp)),position="dodge",stat="identity")+scale_fill_grey(start=0,end=0.6)
    if(usedot) { g1=g1+geom_point(aes(x=x,y=y2,color=factor(gp),fill=factor(gp)),size=6)+scale_color_manual(values=c("red","pink","magenta")) }
    g1=g1+labs(title=title,x=xlabel,y=ylabel)+BaseTheme(base_size=14,tit_mult=1.2,axiscolor="black")+geom_hline(yintercept=0,color="blue",linewidth=1)+coord_flip()
    return(g1)
}

# RGraphs
fgts_biplot <- function(PC, x="PC1", y="PC2", rownames=c(),title="", showpoints="all",opts="repelarrows", xcolors=c("blue","black","grey80","grey60","grey40","grey20"),
                    vectorcolormap="red",  xsizes=c(2,1,0.5,0.2,0.1,0.1), footnote="", labelsincludehr=FALSE, rtn="graph") {
    # PC being a prcomp object
    library(ggnewscale)
    rown=row.names(PC$x)
    if(length(rownames)==nrow(PC$x)) { obsnames=rownames; }
    else { obsnames= c(rep("x",max(1,nrow(PC$x)-4)),rep("rec",3),"last") }
    data <- data.frame(obsnames=obsnames, PC$x)
    plot <- ggplot(data, aes(x=.data[[x]], y=.data[[y]]))
    if(showpoints=="all" || showpoints==TRUE) {
        plot<-plot + geom_point(aes(color=obsnames,size=obsnames))+scale_color_manual(values=xcolors)+scale_size_manual(values=xsizes)
    }
    if(showpoints=="last") {
        plot<-plot + geom_point(aes(color=obsnames,size=obsnames),data=tail(data,1))+scale_color_manual(values=xcolors)+scale_size_manual(values=xsizes)
    }

    plot <- plot+ geom_hline(yintercept=0, linewidth=.2) + geom_vline(xintercept=0, linewidth=.2)
    datapc <- data.frame(variable=rownames(PC$rotation), PC$rotation)
    mult <- min(
        (max(data[,y]) - min(data[,y])/(max(datapc[,y])-min(datapc[,y]))),
        (max(data[,x]) - min(data[,x])/(max(datapc[,x])-min(datapc[,x])))
        )
    datapc <- transform(datapc,            v1 = .7 * mult * (get(x)),            v2 = .7 * mult * (get(y))            )
    expl <- summary(PC)$importance[2,]
    if(rtn=="data") { return(datapc)}
    if(grepl("repel",opts)) {
        plot <- plot + geom_label_repel(data=mutate(datapc,v1=1.05*v1,v2=1.05*v2), aes(x=v1, y=v2, label=variable), size = 2, ,max.overlaps=40) }
    else {
        plot <- plot + geom_text(data=mutate(datapc,v1=1.05*v1,v2=1.05*v2), aes(x=v1, y=v2, label=variable), size = 3, vjust=1,  color="black")
    }

    # plot <- plot + coord_equal()
    if(is.data.frame(vectorcolormap)) { datapc %<>% left_join(vectorcolormap,by="variable")}
    else { datapc %<>% mutate(color=vectorcolormap) }
    #cAssign("data;xcolors;xsizes;datapc;plot")
    # plot <- plot + ggnewscale::new_scale_color()
    if(grepl("arrows",opts)) {
        plot <- plot + new_scale_color()+ geom_segment(data=datapc, aes(x=0, y=0, xend=v1, yend=v2,color=color), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, linewidth=1.2) + scale_color_identity()
        plot <- plot + geom_segment(data=datapc, aes(x=0, y=0, xend=v1, yend=v2,color=color), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, linewidth=1.2) + scale_color_manual(values=c("red","blue","green"))
    }
    plot <- plot + BaseTheme(legend="insidebottom",title="bdays back")
    plot <- plot + labs(title=title,  x=paste(x,round(expl[1]*100,1),"pct"), y=paste(y,round(expl[2]*100,1),"pct"))
    if(labelsincludehr) {
        plot<-plot + labs(caption="Labels may include 1st PC hedge ratio") }
    else {
        plot<-plot + labs(caption=paste0("N:",length(obsnames)," obs"))
        }
    plot
}

# Multiple plot function from
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, cols=1, layout=NULL, widths=rep_len(1,cols)) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout), widths=unit(widths,"null"))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

multiplot_ggplot <- function(..., plotlist=NULL, cols=1, file=NULL, savetitle=NULL, width=9, height=12, basedir=paste0(respath,"rres",filesep), saveas="ggp") {
  saveHash<-hash("ggp"=c(".pdf",1200), "ggj"=c(".jpeg",400),"ggg"=c(".png",600),"ggb"=c(".bmp",400))
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  m1=marrangeGrob(plots,nrow=ceil(numPlots/cols),ncol=cols,top=NULL)
  if( !exists("saveas") ) { saveas="ggg" }
  if(!is.null(file) | !is.null(savetitle)) {
        if(!is.null(savetitle)) {  fn<-paste0(format(lubridate::now(),"%y%m%d"),"_",savetitle) }
        else { fn=file }
        ggsave(filename=paste0(fn,saveHash[[saveas]][1]),plot=m1,dpi=as.numeric(saveHash[[saveas]][2]),path=basedir, width=width,height=height)
        }
  else { return(m1)}
}



grrvindex <- function(inid,what) {
  d1<-dIndex[dIndex$SHORTID==inid,]
  d1<-d1[order(as.vector(d1[,"DT_ENTRY"]),decreasing=FALSE),]
  d1<-na.omit(d1)
  d2<-as.xts(d1,order.by=d1[,"DT_ENTRY"])
  plot(d2[,I(what)])
}
#tit=(m|s;title;yaxis)
#colstr="wid,color,type=(0,1,2,3,4);..."


fgts_recentboxplot<-function(dt,premeltcols="",outliers=F,isordered=F,cutcol="",breaks=c(7,30,90,360),gridsize=0,hlines=c(),
    doi="last",title=NA,xlabel="",ylabel="",flip=T,ordercol="",colorstr="",facetcol="",facetloc="vert",fakefacetcol="",colrename="",ycoord=c(),coordpercentile=0.03,
        trimpctile=0, boxtype="",tlabels="",legend="",normalize="",addmeans=F,normdtoverride=c(NA,NA),dataonly=F,dropset="",
        savetitle="",colorsetoverride=NULL,ptsize=3,labels=NULL) {
    # Rename if necessary, premelt is if already in melted form, otherwise melt whatever we get
    if(doi=="") { doi="last"}
    if (!is.data.table(dt)) { dt=data.table(dt)}
    if(nrow(dt)<=0) { message("mtsrecentboxplot NO DATA "); return()}
    tit <- titadd <- ifelse(is.character(title),title, "")
    if (is.character(breaks) && breaks %in% unique(doidates$category)) {
        breaks<- inv.doi(breaks,exact=TRUE,maxdates=1) %>% pull(daysfromlast) }
    origbreaks=breaks
    if(length(s(colrename))==2) { colnames(dt)<-gsub(colrename[1],colrename[2],colnames(dt),perl=T)}
    ismelted = all(c("variable","value") %in% colnames(dt))
    if(length(premeltcols)==2 & !ismelted) {
        dt=dtsetnm(dt,premeltcols[[1]],"variable",nullcol=1)
        dt=dtsetnm(dt,premeltcols[[2]],"value",nullcol=1)
        ismelted=TRUE
        }
    dt=dtsetnm(dt,ordercol,"orderby",nullcol=1)
    dt=dtsetnm(dt,cutcol,"daysback",nullcol=1)
    dt=dtsetnm(dt,facetcol,"facetcol",nullcol=1)
    if(fakefacetcol %in% colnames(dt)) {
            dt=dtsetnm(dt,fakefacetcol,"fakecol",nullcol=1)
            dt=dt[,variable:=paste(fakecol,variable)][,`:=`(fakecol=NULL)]
    }
    if(ismelted) { dtm = dt    }
    else {
        dtm <- data.table::melt(dt,id.vars=c("DT_ENTRY"), value.name = "value")[is.finite(value),]
    }
#        data.table::melt(dt,id.vars=c("DT_ENTRY")) }
    if(cutcol %in% colnames(dt)) {
        # infer that I want a quantile based on range of breaks, need to create quantiles across credits, add in current levels
        if(all(range(breaks)==c(0,1))) {
            qnames=apply(embed(breaks,2),1,function(x){paste(x,collapse=":")})
            qf<-function(x){
                message("Applying quantiles to ",unique(as.character(x$variable)))
                db1=cut(x$daysback,quantile(x$daysback,probs=breaks,na.rm=T)+seq(1,length(breaks))/10^7)
                levels(db1)=qnames
                data.frame(dq=db1)}
            dtmd=group_by(dtm,variable) %>% do({qf(.)})
            dtm=rbind(subset(dtm,select=-c(daysback)),dtmd[,"daysback"]) }
        else {
            breaks=generalbreaks(breaks,dtm)
            dtm$daysback<-cut(as.numeric(dtm$daysback),c(-1,breaks,+Inf)) }
        }
    else {
        breaks=generalbreaks(breaks,dtm)
        if(length(labels)>0) {
            labels=c(labels,paste0("old",1:10))[1:(length(unique(breaks))+1)]
         }
        dtm$daysback<-cut(as.numeric(max(dtm$DT_ENTRY,na.rm=T)-dtm$DT_ENTRY),c(-1,unique(breaks),+Inf),labels=labels)
        # was c(-1,breaks)
    }
# lets try dplyr again    dtm = subset(as.data.frame(dtm), !is.na(daysback) & is.finite(value))  # dplyr weird
    dtm=dtm[!is.na(daysback) & is.finite(value)]
  #  if(length(ycoord)==2) { dtm$value=min(ycoord[2],dtm$value); dtm$value=max(ycoord[1],dtm$value)} # HACK HACK HACK
    #ydtm=dtm; cAssign("ydtm;normalize")
    if(grepl("bydoi|byvar|zbyvar",normalize)) {
        dtm = dtm[,`:=`(vmin=value,vmax=value)]
        #cAssign("normdtoverride")
        if(!is.na(as.Date(normdtoverride[1]))) {
            dtssmin = as.Date(normdtoverride[1]);
            titadd  = paste(titadd,"MinSprDate=",dtssmin)
            dtm[!which(dtm$DT_ENTRY==dtssmin),"vmin"]=NA }
        if(!is.na(as.Date(normdtoverride[2]))) {
            dtssmax = as.Date(normdtoverride[2]);
            titadd  = paste(titadd,"MaxSprDate=",dtssmax)
            dtm[!which(dtm$DT_ENTRY==dtssmax),"vmax"]=NA }
        dtmb = dtm[,`:=`(vminalldta=min(vmin,na.rm=T),vmaxalldta=max(vmax,na.rm=T)),by=.(variable)]
        dtm  = dtmb[,`:=`(pctbydoi=100*(value-min(vmin,na.rm=T))/(max(vmax,na.rm=T)-min(vmin,na.rm=T)),
                    pctbyvar=100*(value-vminalldta)/(vmaxalldta-vminalldta),
                    pctzbyvar=(value-mean(value,na.rm=T))/sd(value,na.rm=T) ),by=.(variable,daysback)]
        dtm = dtm[,value:=NULL]
        dtm = dtm[,c("value"):=.SD,.SDcols=paste0("pct",normalize)][!is.nan(value)]
        dtm=dtm[!is.nan(value)]
        nfacs=length(unique(dtm$daysback))
        colorset = c("red",dhgreyInvertedPalette[1:max(1,nfacs-2)],"lightgreen")
        if(is.vector(colorsetoverride)) { colorset=colorsetoverride }
        coordpercentile=0
        }
    else {
        hicPal2 = c("grey30","red","green4","blue","red4","magenta","grey30","cyan3","purple2","darkorange2","seagreen4","grey51","dark green","pink","orange")
        colorset = c(rev(gray.colors(max(1,length(unique(dtm$daysback))-1), start=0.4,end=0.8)), "darkorchid1",hicPal2)
        }
    if(nchar(colorstr)>0) { colorset=s(colorstr)}
    if(dataonly) { return(dtm)}
    # Deal with doi and dropset (REfactored 7/15/2022) :
    # doi : date:dt: individual date: INdovidual date only as Dot
    # doi : last:dt : Last + date as Line to Dot

    # doi : last:n : Last + n back (default=1) as Line to Dot
    # doi : nolast: No dots at all
    # doi : default: Last Date as Dot
    nobsback = c(s(doi,sep=":"),1,1)[[2]]
    nobsback = fcoalesce(as.numeric(nobsback),1)
    dtoi = dtm[,.SD[c(.N-nobsback,.N)][,rno:=.I],by=.(variable)]
    if(grepl("-",nobsback)) { # HAd Dtate
        dtoi = dtm[between(DT_ENTRY,as.Date(nobsback),Sys.Date()),][,.SD[c(1,.N)][,rno:=.I],by=.(variable)]
        if(grepl("^last",doi)) {
            titadd = paste0("aDot: Last, Arrow:",as.Date(nobsback))
        }
        else {
            dtoi = dtoi[rno==1,]  # First date
            titadd = paste0("bDot: ",as.Date(nobsback))
        }
    }
    else if (grepl("^last", doi)) {
        nobsback = fcoalesce(as.numeric(nobsback),1)
        if(doi=="last") {
            dtoi = dtoi[rno==2,] # Last Date
            titadd = paste0("cDot: Last")
            }
        else {
            titadd = paste0("dDot: Last, Arrow: ",nobsback," pds back")
        }
    }
    # Ordering original data by dtoiorderby
    if(is.numeric(dtoi$orderby) && (sum(dtoi$value)-sum(dtoi$orderby))<0.001 & !is.na(ordercol) & nchar(ordercol)>1) {
        dtmerge = dtoi[,.SD[1],by=.(variable)][,.(variable,orderby)]
        dtm=dtmerge[dtm[,orderby:=NULL],on=.(variable)]
        isordered=T
    }
    else if(grepl("by",ordercol)) {
        # PROBLEM HERE....
        dtmerge = dtoi[,.SD[.N],by=.(variable)][,.(variable,value)]
        dtmerge[order(dtmerge$value),"orderby"]<-1:nrow(dtmerge)
        dtm=dtmerge[dtm[,orderby:=NULL],on=.(variable)]
        isordered=T
    }
    droplevels(dtm)
    if(length(s(tlabels)) != (length(levels(dtm$daysback))-1)) { tlabels=as.character(levels(dtm$daysback))} else { tlabels=c(s(tlabels),"-Inf-") }
    if(length(dropset)>0) {
        if( !is.na(levels(dtm$daysback)[dropset])) {  message("Dropping level ",levels(dtm$daysback)[dropset]) }
        dtm = subset(dtm,!(daysback %in% levels(dtm$daysback)[dropset]))
        }
    # Start making graph -----
    dtm = suppressWarnings(droplevels(dtm))
    #cAssign("doi;dtoi;nobsback")
    if(grepl("vioandline",boxtype)) {
        labrank = data.table(daysback=tlabels,nrow=seq(1,length(tlabels)))[,keepit:=nrow<.N]
        #dtalastregime = dtm[daysback==tlabels[[1]]][,.(mins=min(value),maxs=max(value),value=last(value)),by=.(variable)]
        dtalastregime = dtm[,.(mins=min(value),maxs=max(value),value=last(value)),by=.(daysback,variable)]
        dtalastregime = labrank[dtalastregime,on=.(daysback)][,`:=`(value=ifelse(nrow>1,NA_real_,value))][(keepit)]
        if(trimpctile>0) {
            dtaq = dtm[,.(qlo=quantile(value,trimpctile,na.rm=T),qhi=quantile(value,1-trimpctile,na.rm=T)),by=.(variable)]
            dtm  = dtaq[dtm,on=.(variable)][between(value,qlo,qhi)]
        }
        g1= ggplot(dtm,aes(x=variable,y=value))+geom_violin(trim=TRUE)
        g1= g1+geom_linerange(aes(ymin=mins,ymax=maxs,color=daysback),data=dtalastregime,position=position_jitterdodge(jitter.width=0.1,dodge.width=0.1),linewidth=0.7)
        g1= g1+geom_point(aes(y=value),data=dtalastregime[nrow==1],color="red",size=2)
    }
    else { # Original
        g1  = ggplot(dtm,aes(x=variable,y=value,fill=daysback))
        if(grepl("nowhisker",boxtype)) {
            ptltmp<-ifelse(boxtype=="nowhisker20",0.2,ifelse(boxtype=="nowhisker10",0.1,0.25))
            dtmx=dtm[,.(ymin=quantile(value,ptltmp,na.rm=T),middle=quantile(value,0.5,na.rm=T),ymax=quantile(value,(1-ptltmp),na.rm=T)),by=.(variable,daysback)][,`:=`(lower=ymin,upper=ymax,value=middle)]
            g1=g1+geom_boxplot(outlier.shape=ifelse(outliers,16,NA),aes(ymin=ymin,lower=lower,middle=middle,upper=upper,ymax=ymax,fill=daysback),data=dtmx,stat="identity",alpha=0.6) }
        else {
            g1=g1+geom_boxplot(outlier.shape=ifelse(outliers,16,NA),aes(fill=daysback),alpha=0.6) }
        #cAssign("dtm;g1;boxtype")
        g1=g1+scale_fill_manual(values=colorset,labels=tlabels)
        if(!(doi=="nolast")) {
            if(max(dtoi$rno)-min(dtoi$rno)>0) {
                ptscast=dcast(dtoi[,.(variable,daysback,value,rno)][,rno:=paste0("R",rno)],variable+daysback~ rno)
                #cAssign("g1;ptscast;dtoi2")
                # Lighter Dot: g1=g1+geom_point(aes(x=variable,y=value),color=I("blue"),size=ptsize,alpha=0.2, data=dtoi2[rno==2])
                # Arrow: g1=g1+geom_segment(aes(x=variable,xend=variable,y=R2,yend=R1),data=ptscast,color="blue",size=1.5,arrow = arrow(length = unit(0.01, "npc")))
                g1=g1+geom_segment(aes(x=variable,xend=variable,y=R2,yend=R1),data=ptscast,color=I("blue"),linewidth=1)+geom_point(aes(x=variable,y=R1),color=I("blue"),size=ptsize,data=ptscast)
            }
            else{
                g1=g1+geom_point(aes(x=variable,y=value),color=I("blue"),size=ptsize,data=dtoi)
            }
        }
    }

    # Ordering
    if(ordercol=="reverse") {
        g1=g1+scale_x_discrete(limits=rev(levels(dtm$variable))) }
    if(isordered) {
        dtm$v2<-reorder(dtm$variable,as.numeric(dtm$orderby))
        g1=g1+scale_x_discrete(limits=levels(dtm$v2))
        dtm$variable<-as.character(dtm$v2)
         }
    ycoordtouse= ycoord
    if( length(ycoord) != 2 ) {  ycoordtouse= quantile(dtm$value, c(coordpercentile, 1-coordpercentile), na.rm=T) }

    if(gridsize>0) {
        trange=gridsize*(floor(range(dtm$value,na.rm=T)/gridsize)+c(0,1))
        #print(paste(" gridize=",gridsize," trange=",trange))
        if(length(ycoord)==2) { g1=g1+scale_y_continuous(minor_breaks=seq(trange[[1]],trange[[2]],gridsize),limits=ycoord) }
        else { scale_y_continuous(minor_breaks=seq(trange[[1]],trange[[2]],gridsize))}
        g1=g1+BaseTheme(base_size=7)+theme(panel.grid.minor = element_line(color="grey", linetype="dotted", linewidth=0.5)) }

    for(ln in hlines) { u=g1<-g1+geom_hline(yintercept=ln,color="blue")    }
    if(flip) {  g1=g1+coord_flip(ylim = ycoordtouse) }
    # Legends
    if(legend=="combine") {
        titadd=paste(titadd,"\nRg",paste0(origbreaks,collapse=","))
        legend="none" }
    g1=g1+labs(title=tit, caption=titadd, x=xlabel,y=ylabel)+BaseTheme(base_size=7, legend=legend)
    if("facetcol" %in% colnames(dtm)) {
        if(facetloc=="vert") {  g1=g1+facet_grid(facetcol~.,drop=TRUE,scales="free",space="free") }
        else {                  g1=g1+facet_grid(.~facetcol,drop=TRUE,scales="free",space="free")}
        g1=g1+theme(strip.text.y = element_text(size=11,color="black", face="bold"), strip.background=element_rect(fill="wheat"))
        }

    if(addmeans=="mean") {
        dmn=group_by(dtm,daysback) %>% summarise(vmn=mean(value,na.rm=T),vsd=sd(value,na.rm=T))
        #print(as.data.frame(dmn))
        g1=g1+geom_hline(aes(yintercept=vmn,color=daysback,linetype=daysback),data=dmn[c(1,nrow(dmn)),],linewidth=1)
        g1=g1+scale_colour_manual(values=c("red","black","green","blue"))+scale_linetype_manual(values=c(3,5))
    }
    if(addmeans=="smooth") {
        dmn=group_by(dtm,variable) %>% slice_max(n=1,-daysback)
        g1=g1+geom_smooth(aes(x=variable,y=value),formula="y~x",data=dmn)+scale_colour_manual(values=c("red","black","green","blue"))+scale_linetype_manual(values=c(3,5))
    }
    if(exists("curvestohighlight") & "CREDIT" %in% colnames(dt)) {
        credits_in_dt = ungroup(dt) %>% mutate(oCREDIT=CREDIT) %>% group_by(CREDIT,oCREDIT) %>% tally()
#        datamap = left_join( tibble(CREDIT=unique(as.character(dtm$variable))), credits_in_dt, by="CREDIT") %>%
#                    left_join(tibble(oCREDIT=curvestohighlight) %>% mutate(color="red"),by="oCREDIT") %>% mutate(color=fcoalesce(color,"black"))
        datamap = left_join( tibble(CREDIT=unique(as.character(dtm$variable))), credits_in_dt, by="CREDIT") %>%
                    left_join(tibble(oCREDIT=curvestohighlight) %>% mutate(color="red"),by="oCREDIT") %>% mutate(color=fcoalesce(color,"black"))

        g1=g1+theme(axis.text.y = element_text(color = datamap$color))
    }

    #g1=g1+theme(axis.text.y=element_text(size=10),plot.title=element_text(size=12))
    psave(savetitle,g1)
}
