optString_parse <-function(x,item) {
    x1=tibble::tibble(ops=s(x)) |>
            tidyr::separate_wider_delim(ops,",",names=c("beg","end"),too_many="merge",too_few="align_start") |>
            filter(grepl(item,beg)) |> dplyr::pull(end)
    if(identical(x1,character(0))) { return(FALSE)}
    else { return(x1)}
}

# s(plit) converts string to list
s<-function(x,sep=";",fixed=TRUE,rtn=NULL) {
    y=unlist(strsplit(x,sep,fixed=fixed))
    if(is.numeric(rtn)) { if(length(y)>=rtn) { y=y[rtn] } }
    return(y)
    }
# g(lue) is glue
g<-function(x,sep=";",...) { if(length(x)>1) { glue_collapse(x,sep=sep)  } else { glue::glue(x,...) } }

gendtstr<-function(x,today=Sys.Date(),rtn="dtstr") {
    if(is.numeric(x)) { return(paste0(today-lubridate::ddays(x),"::")) }
    gendtsplit=c(strsplit(x,"::")[[1]],"")[1:2]
    repfn<-function(y,today, default=NULL) {
        y <- tolower(y)
        aa<-stringr::str_replace(y,"y",""); if (aa != y) { return(as.Date(today+lubridate::dyears(as.numeric(aa)))) }
        aa<-stringr::str_replace(y,"d",""); if (aa != y) { return(as.Date(today+lubridate::ddays(as.numeric(aa)))) }
        aa<-stringr::str_replace(y,"m",""); if (aa != y) { return(as.Date(lubridate::add_with_rollback(today,months(as.numeric(aa))))) }
        aa<-stringr::str_replace(y,"w",""); if (aa != y) { return(as.Date(today+lubridate::dweeks(as.numeric(aa)))) }
        if(nchar(y)<=2) { return(default)}
        else { return(y) }
    }
    gendt1 = repfn(gendtsplit[[1]], today, default="1980-01-01")
    gendt2 = repfn(gendtsplit[[2]], today, default=today)
    if(rtn=="days") { return(as.numeric(today-as.Date(gendt1))) }
    else if (rtn=="list") { return(c(as.Date(gendt1),as.Date(gendt2)))}
    else if (rtn=="first") { return(as.Date(gendt1)) }
    else {  return(paste0(gendt1, "::", gendt2)) }
}

# ------------------------------------ Unsure if neeeded anymore ---------------------------------------------

robustcut <- function(x,quantiles,labels=NULL,...) {
    qtmp=unique(quantile(x,quantiles,na.rm=T))
    if(length(s(labels))==(length(quantiles)-1)) {
        return( cut(x,breaks=qtmp,labels=s(labels)[1:length(qtmp)-1], include.lowest=FALSE,...) ) }
    else {
        return( cut(x,breaks=qtmp,include.lowest=TRUE,...) )
    }
}



print_meltmultiple <- function(inset,catcommon="cat", nocvalue=TRUE,mergeexp=NULL) {
    if(!is.data.frame(inset)) {
        inset = rbindlist(lapply(inset,function(x){meltmultiple(x,idvar=catcommon)}),fill=TRUE)
    }
    if(nocvalue) {
        inset = inset |> filter(is.na(cvalue)) %>% select(-c(cvalue))
    }
    inset %<>% filter(!is.na(variable)) |> mutate(variable=as.character(variable))
    suppressWarnings(inset %<>% mutate(datevalue = fifelse(grepl("DT|date",variable) & !is.na(value), as.character(as.Date(as.numeric(value))), NA_character_)))
    if(!is.null(mergeexp)) {
        inset %<>% left_join(select(mergeexp,cat,variable,explanation),by=c("cat","variable"))
    }
    return(inset)
}

whatsmissing<-function(x,n=5) { group_by(x,DT_ENTRY) %>% tally %>% mutate(dtdif=c(NA,diff(DT_ENTRY))) %>% filter(dtdif>=!!n) }
is.melteddf <- function(x) { all(any(grepl("variable",colnames(x))),any(grepl("value",colnames(x)))) }
robustcoalesce <- function(...) {  Reduce(function(x, y) {    i <- which(is.na(x));    x[i] <- y[i];    x},  list(...))}
#slow: coalesce <- function(...) {  apply(cbind(...), 1, function(x) x[which(!is.na(x))[1]]) } above is faster
#coalesce <- function(a,b) { ifelse(is.na(a),b,a) }
coalescewhich <- function(...) {  apply(cbind(...), 1, function(x) which(!(is.na(x) | nchar(x)<=0))[1]) }
coalesceString <- function(...) {  Reduce(function(x, y) {    i <- which(is.na(x) | nchar(x)<=0);    x[i] <- y[i];    x},  list(...))}
coalescedf<-function(x,y) { # coalesces dataframes
    nx=nrow(x)
    if(nrow(y)==1) { y= left_join(mutate(y,jrep=1), new_tibble(list(jrep=rep(1,nx),irow=1:nx),nrow=nx), by=c("jrep"), multiple="all") %>% select(-jrep) }
    left_join(mutate(ungroup(x),irow=row_number()),y,by="irow") %>% select(-ends_with(".y")) %>% set_colnames(gsub(".x","",colnames(.),fixed=T)) %>% select(-irow)}

 # Superceded by new RollRccp options
padhead <- function(x,to) { c(rep(NA,(length(to)-length(x))),x)}
padtail <- function(x,to) { c(x,rep(NA,(length(to)-length(x))))}
daterangeofdf<-function(indta) { paste(range(indta$DT_ENTRY),collapse="::") }
intervaltrim<-function(x,limits=c(-3,3),rounddigits=0) { rx=round(range(x,na.rm=T)+c(-0.25,0.5),rounddigits); c(max(limits[1],rx[1]), min(limits[2],rx[2]))}
limittorange<-function(x,lo=-Inf,hi=+Inf) {   pmax(pmin(x,as.numeric(hi)),as.numeric(lo)) }
objgrep<-function(x){ ls(envir=parent.frame(n=4))[grep(x,ls(envir=parent.frame(n=4)))]}
dstr <- function(x,level=1) {
    level = ifelse(is_tibble(x), level+1,level)
    str(x,max.level=level,give.attr=FALSE)}
dargs <- function(x) {  uf = formals(x);
    argdefaults = as.matrix(lapply(seq(1,length(names(uf))), function(x) { uf[[x]] } ))
    data.frame(argno = seq(1,length(names(uf))), argname=names(uf), argdefault = argdefaults)
}
dls<-function(x) { data.table(id=ls(.GlobalEnv))[id %plike% x]}
numbercols <- function(df,replace=F) {  colnames(df)<-paste(1:ncol(df),colnames(df),sep=")"); df }
fromClipboard<-function() { read.table(file="clipboard",stringsAsFactors=F,header=T,sep="\t") }
toClipboard<-function(dsn) { write.table(dsn, file="clipboard", row.names = F, col.names=is.data.frame(dsn)) }
line2names<-function(x) { ret=as.character(as.list(x[1,])); names(x)<-colnames(x); x}

logmessage <- function(x,outfile="c:/t/rlog.txt",append=TRUE) {
    message(x)
    cat(paste0(format(Sys.time(),"%Y-%m-%d %H:%M:%S"),": ",x,"\n"), file=outfile, append=append)
}

#factor2char<-function(x,vars="*") {  x %>% map_if(is.factor, as.character) %>% as.data.frame(stringsAsFactors=FALSE) %>% select(one_of(vars)) }
#factor2char<-function(x,vars="*") {  as.data.frame(x,stringsAsFactors=FALSE) %>% select(one_of(vars)) }
#data.frame(a=s("A;A;B;B"),b=1:4) %>% purrr::map_if(is.factor,as.character) %>% tbl_df %>% select(contains("*"))
factor2char<-function(x,vars="*") {  x %>% purrr::modify_if(is.factor, as.character) %>% select(matches(vars)) }
#"%+%" <- function(...){ paste0(...,sep="")}
cleancolumnnames<-function(cn) { cn %>% stringr::str_replace_all(" ","") %>% stringr::str_replace_all("\\(|\\)","") %>% stringr::str_replace_all("%","pct")}
# Can be replaced by janitor

doi_replace <- function(indates,rtn="df", cat="regm") {
    doi1 = filter(doidates,category==cat) %>% select(DT_ENTRY,eventid) %>% filter(DT_ENTRY %in% as.Date(indates))
    doi1 %<>% arrange(DT_ENTRY) %>% mutate(nn=row_number()) %>% mutate(eventid=paste0(nn,",",fcoalesce(eventid,as.character(DT_ENTRY)))) %>% select(-nn)
    if(grepl("df",rtn)) { return(doi1)} else { return(doi1$eventid)}
}
adddoidates <- function(dta,doiname="regm",maxevents=9999) {
  #  if(!is.null(groups(dta))) { stop("adddoidates needs to be within grouping: ")}
    doiset = filter(doidates,category==doiname) %>% ungroup %>% slice_max(n=maxevents,END_DT_ENTRY) %>% mutate(eventid=paste0(row_number(),"_",eventid)) %>%
            mutate(DT_ENTRY=as.Date(ifelse(between(min(dta$DT_ENTRY),DT_ENTRY,END_DT_ENTRY),min(dta$DT_ENTRY),DT_ENTRY))) %>%
            mutate(direc=coalesce(str_extract(eventid,"(\\+|\\-|\\=)"),"xx")) %>% select(-category,eventid2)
    doisetdt = as.data.table(select(doiset,eventid,direc,DT_ENTRY))
    doisetdt[as.data.table(dta),roll=T,on="DT_ENTRY"]
    #dta1=left_join(dta,doiset,by="DT_ENTRY") %>% arrange(DT_ENTRY) %>% tidyr::fill(END_DT_ENTRY) %>% rowwise %>% mutate(eventid=ifelse(DT_ENTRY==END_DT_ENTRY,NA,eventid)) %>% ungroup
    #dta1 %>% tidyr::fill(eventid,direc) %>% filter(!is.na(eventid))
}


lineAssign<-function(xline) {
    if(nrow(xline)>1) message("lineAssign cannot assigm more than oneline")
    aaa1=purrr::map2(names(as.list(xline[1,])),as.list(xline[1,]), function(x,y){assign(x,y,env=.GlobalEnv)})
}





pulltotickerstring<-function(x,var) { paste(pull(x,var) %>% as.character %>% str_trim,collapse=";") }
strtrunc<-function(str,len) { if(nchar(str)>=len) { return(paste0(strtrim(str,len-3),"..."))} else { return(str) }}

# Merges a set of days back with specific dates
generalbreaks<-function(dtset,dtds,rtnasoffset=TRUE) {  # Kind of ugly, but I need this.
    if(is.data.frame(dtds)) { u2dt=sort(unique(dtds$DT_ENTRY)) } else { u2dt = dtds}
    if(is.character(dtset)) { dtset=lapply(dtset,function(x){if(grepl("-",x)){x=as.Date(x)}else{x=as.numeric(x)};x}) }
    dtset2=as.Date(unlist(lapply(dtset,function(x){dtmxx=ifelse(x<min(u2dt),max(u2dt)-x,x); u2dt1=u2dt[u2dt<=dtmxx]; ifelse(length(u2dt1)<=0,min(u2dt),max(u2dt1))})))
    if(rtnasoffset) { dtset2=max(u2dt)-dtset2 }
    dtset2
}


datesfromString<-function(x) {
    dts=as.Date(stringr::str_split(x,'::')[[1]])
    dts[[2]]=coalesce(dts[[2]],Sys.Date())
    dts
}
dtstr_from_vec<-function(x) { paste(range(x),collapse="::") }

# reqorked 11/19/13 to take a data table, a dataframe, or an array
# Little ugly
extenddtstr <- function(instr,begchg=0,endchg=0,mindt=NULL,maxdt=NULL,rtn="",rtnstyle="string") {
    if(is.xts(instr)) { instr=paste0(min(zoo::index(instr),na.rm=T),"::",max(zoo::index(instr),na.rm=T)) }
    if(is.data.frame(instr)) { instr=paste0(min(instr$DT_ENTRY,na.rm=T),"::",max(instr$DT_ENTRY,na.rm=T)) }
    spl=gendtstr(instr,rtn="list")
    if(is.na(spl[1])) { spl[1]=as.character(Sys.Date())}
    if(is.na(spl[2])) { spl[2]=as.character(Sys.Date())}
    if(lubridate::is.Date(as.Date(spl[1]))) { spl[1]=as.character(as.Date(spl[1])+begchg) }
    if(lubridate::is.Date(as.Date(spl[2]))) { spl[2]=as.character(as.Date(spl[2])+endchg) }
    if(rtn=="list") { rtnstyle="list" }
    if(rtn=="datelist") {
        spl=list(as.Date(spl[1])+begchg,as.Date(spl[2])+endchg)
        rtnstyle="list"
        }
    if(rtn=="fromtoday") {
        spl=as.numeric(list(Sys.Date()-(as.Date(spl[1])+begchg),Sys.Date()-(as.Date(spl[2])+endchg)))
        rtnstyle="list"
        }
    if(rtn=="totoday") { spl[2] = NA_Date_ }
    if(is.Date(mindt)) { spl[1] = max(spl[1],mindt) }
    if(is.Date(maxdt)) { spl[2] = min(spl[2],maxdt) }
    if(rtnstyle=="list") { return(spl) }
    return(paste0(as.character(spl[1]),'::',fcoalesce(as.character(spl[2]),"")))
}



lsweights <- function(n,lam,typ="NW") {
    if(typ=="expon") {a1<-maply(seq(1,n,1),function(i){(1-lam)/(1-lam^i)}); return(a1/sum(a1)) }
    if(typ=="NW") {a1<-maply(seq(1,n,1),function(i){1-(i/(n+1))}); return(a1/sum(a1))}
    }


Nweekdays<-function(a,b) {
    if(is.na(a) | is.na(b)) { return(NA) }
    sign(as.numeric(b-a))*nrow(dtmap[between(DT_ENTRY,min(as.Date(a),as.Date(b)),max(as.Date(a),as.Date(b))),])
}


Weekdayoffset <- function(dt,i) { dtmap[min(which(dtmap$DT_ENTRY>dt & dtmap$isday==TRUE)-1+i),]$DT_ENTRY }
#Settlementdt <- function(dt,settlementDays) { dtmap[between(DT_ENTRY,tradedt,tradedt+10) & isday,.SD[settlementDays+1,]]$DT_ENTRY } 3x slowergetD


# Fastest ---------------------------------------------
mergeaggdt<-function(x,...) {
    if(is.data.table(x)) {
        dt2=select(ungroup(dtmap),DT_ENTRY,!!!rlang::syms(rlang::dots_list(...)))
        return(merge(x,data.table(dt2,key=c("DT_ENTRY")),by="DT_ENTRY"))
    }
    else {
        return( left_join(x,select(ungroup(dtmap),DT_ENTRY,!!!rlang::syms(rlang::dots_list(...))), by="DT_ENTRY") )
    }
}

toaggdt<-function(x,to="yrwk") {
        convstr=list('yrwk'="%Y%V","yrweek"="%Y%V","yrmo"="%Y%m","dt"="%Y%m%d","wk"="%V","filedt"="%y%m%d")
        as.numeric(strftime(x,convstr[[to]])) }


addseasonaldates<- function(x,dtname="DT_ENTRY",toadd="all",freqvarname="") {
    if(!is.data.frame(x)) {
        if(x=="vars") { return("doy|yr|qtr|doq|yrwk|week") }
        else { print("addseasonaldates(x,toadd =(all|yr|qtr|doq|ywk|week),dtname)")} }
    if (sum(grepl(dtname,colnames(x)))<=0 ) { xdt=as.Date(rownames(x)) }
    else { xdt=as.Date(x[[dtname]],use.names=F) }
    if(grepl("doy|all",toadd)) { x$doy<-as.numeric(format(xdt,"%j")) }
    if(grepl("^(yr|all)$",toadd)) { x$yr<-as.numeric(format(xdt,"%Y")) }
    if(grepl("doq|all",toadd)) { x$doq<-as.numeric(xdt-as.Date(paste0( as.character(floor( (month(xdt)-1)/3)*3+1),"/1/",x$yr),"%m/%d/%Y")) }
    if(grepl("(qtr)|yrqt|all|dfagg",toadd)) { x$yrqtr<-as.numeric(format(xdt,"%Y") )*10+as.numeric(substr(quarters(xdt),2,2)) }
    if(grepl("yrwk|all|dfagg",toadd)) { x$yrwk<-toaggdt(xdt) }
    if(grepl("yrmo|all|dfagg",toadd)) { x$yrmo<-toaggdt(xdt,to="yrmo") }
    if(grepl("week|all",toadd)) { x$wk<-toaggdt(xdt,to="wk")  }
    if(nchar(freqvarname)>0) { setnames(x,toadd,freqvarname) }
    return(x)
}

toseasonaldates<- function(x,dtname="DT_ENTRY",toadd="all",freqvarname="",rtn="first") {
    if(toadd=="" | grepl("^day",toadd)) { return(x) }
    else {
        if (!grepl("yrwk|yrmo|yrqt",toadd)) { print("invalid toadd=" %+% toadd); stop() }
        dtall = addseasonaldates(x,toadd=toadd,dtname=dtname,freqvarname=freqvarname)
        if(is.data.table(dtall)) {
            setorderv(dtall,c(dtname))
            out=  dtall[,aggdtnm:=get(toadd)]
            alldts = out[,.SD[ifelse(rtn=="last",.N,1)],by=.(aggdtnm), .SDcols=c(dtname)]
            out= alldts[out,on=.(aggdtnm,DT_ENTRY),nomatch=NULL][,aggdtnm:=NULL] }
        else {
            direc = ifelse(rtn=="last",1,-1)
            out = dtall %>% group_by(!!rlang::sym(toadd)) %>% slice_max(n=1,direc*as.numeric(!!sym(dtname))) %>% ungroup

        }
        return(out)
    }
}


lagdataframe<-function(x,datecoln,nlag=0) {
    if(nlag>0) {  x[,datecoln]<-as.Date(c(rep(NA,nlag),x[1:(nrow(x)-nlag),datecoln]))     }
    else {  x[,datecoln]<-as.Date(c(x[nlag:nrow(x),datecoln],rep(NA,nlag)))     }
    x
}

expanddates<-function(indata,qqq,daysback=0,datename="DT_ENTRY") {
    dts<-indata[,datename]
    qqq$bdate<-qqq$begdate
    e2<-by(qqq,qqq$event,function(x){
        y<-indata[(dts>=(x[1,"begdate"]-daysback) & dts<=x[1,"enddate"]),]
        return(merge(y,subset(x,select=-c(begdate)),all.x=T,all.y=T))})
    dtsa<-do.call("rbind",e2)
    dtsa$days<-as.numeric(dtsa[,datename]-dtsa$bdate)
    return(dtsa)
}

# Creates dateset from dtstr
full_dtset<-function(freq="yrwk", begdt=0, enddt=NA_Date_, startofweek=T, extenddays=0,add="") {
    lastdt= fcoalesce(enddt,Sys.Date()+extenddays)
    fdts= dtmap[data.table::between(DT_ENTRY,begdt,lastdt)]
    if(freq=="day" | freq=="days" | freq=="DT_ENTRY") {
        f2=fdts
    }
    else {
        mult = ifelse(startofweek,-1,1)
        setkeyv(fdts,freq)
        f2=fdts[,DT_ENTRY[I(which.min(mult*as.numeric(DT_ENTRY)))],by=freq][,DT_ENTRY:=V1][,V1:=NULL]
    }
    if(nchar(add)>0) { f2[[add]]<-seq(1,nrow(f2)) }
    return(f2)
}


dateset<-function(expr,format="day",includetoday=T) {
    dtlist=gendtstr(expr,today=as.Date(ifelse(includetoday,Sys.Date(),Sys.Date()-1)),rtn="list")
    rtn = full_dtset(freq=format,begdt=as.Date(dtlist[1]),enddt=as.Date(dtlist[2]))
    if(format=="day" | format=="days" | format=="DT_ENTRY") { return(rtn$DT_ENTRY) }
    else { return(rtn[]) }
}


narrowbydtstr<-function(xin,dtstr="",includetoday=T,dtname="DT_ENTRY", windowdays=0, invert=F, addindicator=F) {
    if(dtstr=="") { return(xin) }
    this_dtstr = ifelse(windowdays>0, extenddtstr(dtstr,begchg=-windowdays), dtstr)
    dtlist = gendtstr(this_dtstr,today=as.Date(ifelse(includetoday,Sys.Date(),Sys.Date()-1)),rtn="list")
    if(is.data.table(xin)) {
        toget=data.table::between(xin[[dtname]],dtlist[1],dtlist[2])
        if(invert) { toget=!toget }
        if(addindicator) { xin$inrange=toget }
        else { xin=xin[toget] }
        return(xin)
    }
    else if (is.data.frame(xin)) {
        xin = xin %>% mutate(inrange=between(!!rlang::sym(dtname),dtlist[1],dtlist[2]))
        if(invert) { xin %<>% mutate(inrange=!inrange) }
        if(addindicator) { return(xin) }
        else {  return( filter(xin, inrange==TRUE) %>% select(-inrange)) }
    }
    else {
        return( xin[between(xin,dtlist[1],dtlist[2])] )
    }
}




lmsumresid<-function(lms,origdta, dateonly=F, onemeltvar="", completecases=FALSE) {
    lmaug=suppressWarnings(broom::augment(lms))
    if(completecases) {
        if(dateonly) {  lhs= select(origdta,DT_ENTRY) }
        else {
            colstoget = setdiff(colnames(origdta), colnames(lmaug))
            lhs = select(origdta,all_of(colstoget))
        }
        lhs$rowno=seq(1,nrow(lhs))
        #cAssign("lhs;lmaug;lms")
        v1<-left_join(lhs, lmaug %>% mutate(rowno = as.numeric(rownames(lmaug))), by="rowno") %>% select(-rowno) }  # 10/25: augment chagned
    else {
        v1<- bind_cols(slice(origdta,as.numeric(rownames(lmaug))), lmaug)  # Much faster
    }
    if(onemeltvar %in% colnames(v1)) {
        v1=transmute(v1,DT_ENTRY,variable=onemeltvar,value:=!!rlang::sym(onemeltvar)) }
    v1
}

#rtn=wide:  1.61x faster, rtn: melt: 2.8x faster
lmsum<-function(lms,title="",grepstr="",dbg=F,lastresidual=T,rtn="wide", add="") {
    if(dbg)  { print(paste("lm:",dbg)); cAssign("lms")}
    tid0=as.data.table(broom::glance(lms))
    if("glm" %in% class(lms)) {
        tid1=tid0[,.(term="reg.summary",estimate=AIC,std.error=deviance,statistic=logLik,p.value=0)] }
    else {
        tid1=tid0[,.(term="reg.summary",estimate=adj.r.squared,std.error=sigma,statistic,p.value)]
        }
    #cAssign("tid0;tid1")
    lmout=rbindlist(list(tid1,as.data.table(broom::tidy(lms))))
    lmout=lmout[term %plike% paste0("ntercept|",grepstr),][,':='(p.value=round(p.value,5))]
    if(is.numeric(lastresidual)) { # Averge of last n residuals
        lmout$lastresid=mean(tail(residuals(lms),lastresidual),na.rm=T)
        lmout$lastresidz=mean(tail(rstudent(lms),lastresidual),na.rm=T)
    }
    else if(lastresidual) { # Broom broke
        lmout$lastresid=last(residuals(lms))
        lmout$lastresidz=last(rstudent(lms))
    }
    if(grepl("melt", rtn)) {
        lmout=melt(lmout,id.var=c("term"))
        if (grepl("sd", add)) {
            xx    = lms$model[,2]
            yy    = lms$model[,1]
            firstbeta = coalesce(coef(lms)[2],coef(lms)[1])[1]
            ds1   = data.table(
                    term = rep("reg.summary",3),
                    variable=c("sdresid","sdymx1","sdresid1"),
                    value=c(sd(resid(lms)), sd(yy-xx), sqrt(var(yy)-firstbeta^2 * var(xx)))
                )
            lmout = rbindlist(list(lmout, ds1))
        }
        if(rtn=="melt") { lmout=lmout[,':='(variable=paste0(term,":",variable))][,':='(term=NULL)] }
    }
    if(nchar(title)>0) { lmout=lmout[,title:=title] %>% relocate(title)}
    return( lmout[] )
}


print.money<-function(x){format(x,digits=2,big.mark=",",zero.print="--")}
print.bps<-function(x){format(x,digits=4,big.mark=",",zero.print="--")}


DTUpsert<-function(a,b,keys, fill=FALSE,verbose="") { # DT kind of tough to use this replaces old data
    if(!is.data.table(b)) { b=data.table(b) }
    if (is.character(a)) { aandb=b }
    else if(nrow(a)<=0) { aandb=b }
    else if(nrow(b)<=0 | length(setdiff(keys,colnames(b)))>0) { aandb=a }
    else {
        setkeyv(a,keys); setkeyv(b,keys)
        aandb=rbindlist(list(a[!b],b),use.names=TRUE,fill=fill)
        if(nchar(verbose)>1) { message("DTUpsert(",verbose,"): adds ",nrow(b)," rows, now ",nrow(aandb)) }
        if(  any(grepl(".x",colnames(aandb),fixed=T)) ) {
            message(" ERORR in DTMERge... colnames: ",paste0(colnames(aandb),collapse=","))
            stop()
        }
    }
    setkeyv(aandb,keys)
    return(aandb)
}

# Assumes all dtnew columns already exist and are (if missing) NAs in dto

DTPartialupsert <- function(dto,dtnew, collist=NA_character_, keys=NA_character_) {
  if(any(is.na(keys))) { keys=key(dto) }
  if(any(is.na(collist))) { collist = setdiff(colnames(dtnew),keys) }
  setkeyv(dto,keys)
  dto=dtnew[dto,on=keys]
  for (cn in collist) {  dto[,(cn):= fcoalesce(dto[[cn]],dto[[paste0("i.",cn)]])] }
  return(dto[,.SD,.SDcols=!patterns("^i\\.")])
}




#install.packages("c:/d/src/R/packages/tsoutliers.zip",repos=NULL,type="source")
tsout <-function(y,...) {  require(tsoutliers); tso(as.ts(df2xts(y)),...) }
removetsoutliers <-function(y,...) {  require(tsoutliers);
    tsoutl<-try(tso(as.ts(df2xts(y)),...))
    if(class(tsoutl)=="try-error") {
        print(paste(" ------  removetsoutliers fail, returning origanal data ------ "))
        return(y) }
    else {
        if(length(tsoutl$outliers$time)>0) { y[-tsoutl$outliers$time,] } else {y}
    }
}


emfgenericpivot<-function(dt,replace_text_na="-") {
    colcc=colnames(dt)[grepl("character|factor",sapply(dt,class))]
    dtmelt= melt(as.data.table(dt),colcc)
    if(nchar(replace_text_na)>0) {
        dtmelt[is.na(dtmelt) & rep(sapply(dtmelt,class)=="character",nrow(dtmelt),1)]<-replace_text_na }
    rpivotTable(dtmelt,rows=colcc,cols=c("variable"),aggregatorName="Last", rendererName="Col Heatmap", vals="value",subtotals=FALSE)
}


emfdatatable<-function(df,digits=3,caption="",pagelen=30,filter="",rangehighlight="colornumeric",background="#fdfcff",editable=FALSE, rowhilightset=NULL,inputform="",
                                style="auto",rowhilight=5,eps=0.001,rangecolors=c("red","white","lightblue"),defclass="compact",warnbigcolnames=10,...) {
    df=as.data.frame(df)
    numcols=sapply(df, is.numeric);
    df[,numcols] <-round(df[,sapply(df, is.numeric)],digits=digits)
    #                 initComplete = JS("function(settings, json) {","$('body').css({'font-family': 'Helvetica'});","}"),
    DDF=NULL
    if(nrow(df)<=0) { return(DDF) }
    if(warnbigcolnames>0) {
        bigcols = colnames(df)[nchar(colnames(df))>warnbigcolnames]
        if(length(bigcols)>0) { message("Big column warning for ",bigcols)}
    }
    if(grepl("filter",filter)) {
        DDF=DT::datatable(df,rownames=FALSE,caption=caption,class=defclass,filter='top', extensions = c('KeyTable','Responsive'), editable=editable,
            options=list(autoWidth=TRUE,searchable=FALSE,pageLength=pagelen,scrollY=TRUE,style=style,
                search = list(regex = TRUE, caseInsensitive = FALSE, fixedColumns=FALSE, buttons = I('colvis')))) }
    if (grepl("simple",filter)) {
        DDF=DT::datatable(df,rownames=FALSE,caption=caption, filter="none", class=defclass, editable=editable,
                    options=list(style=style,autoWidth=TRUE,fixedColumns=FALSE,searching=FALSE,scrollY=TRUE,paging=FALSE))
    }
    if (is.null(DDF)) {
        DDF=DT::datatable(df,rownames=FALSE,caption=caption, class=defclass, editable=editable, extensions = c('KeyTable','Responsive'),
            options=list(style=style,autoWidth=TRUE,fixedColumns=FALSE,searchable=FALSE,scrollY=TRUE,pageLength=pagelen, buttons = I('colvis')))
    }
    if(is.character(rangehighlight)) {
        if(rangehighlight=="colornumeric") {
            DDF = DDF %>%  formatStyle(colnames(df)[numcols],color =styleInterval(c(-eps,eps),c("red",background,"black")), fontWeight  =styleInterval(c(-eps,eps),c("bold","normal","normal"))) }
        else {
            DDF = DDF %>%  formatStyle(colnames(df)[numcols],backgroundColor =styleInterval(s(rangehighlight),rangecolors)) }
        }
    else if( is.vector(rangehighlight)) {
        rangecolors =  s(rangecolors,hicPalette)[1:(length(rangehighlight)+1)]
        DDF = DDF %>%  formatStyle(colnames(df)[numcols],backgroundColor =styleInterval(rangehighlight,rangecolors)) }
    else if(is.data.frame(rangehighlight)) {
        rangehighlight=as_tibble(rangehighlight)
        for( irowno in 1:nrow(rangehighlight)) {
            DDF = DDF %>%  formatStyle(.,rangehighlight[[irowno,"colname"]],backgroundColor =styleInterval(s(as.character(rangehighlight[[irowno,"range"]])),rangecolors)) }
        }
    else {
        DDF = DDF %>%  formatSignif(colnames(df)[numcols],digits=digits,zero.print="--")
    }
    if(length(rowhilightset)>1) {
        intermcolor=colorRampPalette(c(background,"#000000"))(20)[3]
        DDF = DDF %>%  formatStyle(.,colnames(df),backgroundColor =styleRow(rowhilightset,rep(intermcolor,length(rowhilightset))))
    }
    else if(rowhilight>0 & nrow(df)>=1) {
        intermcolor=colorRampPalette(c(background,"#000000"))(20)[3]
        irows = seq(1,nrow(df),rowhilight)
        DDF = DDF %>%  formatStyle(.,colnames(df),backgroundColor =styleRow(irows,rep(intermcolor,length(irows))))
    }
    if(grepl("^trlmsum",inputform)) {
        signif=as.numeric(c(s(inputform),0.025)[[2]])
        message("highlighting with signif ",signif)
        DDF =DDF %>% formatStyle(grep("^est",names(df),value=T),valueColumns=grep("^p.value",names(df),value=T),digits=2,backgroundColor=styleInterval(c(-0.0001, signif), c('yellow','lightgreen','white')))
    }
    DDF
}


topnonoverlapping <-function(x,n,window) {
    v1xx<<-x; unlist(lapply(1:n,function(i){im=which.max(v1xx);v1xx[max(1,(im-window)):min(nrow(v1xx),im+window)]=0;assign("v1xx",v1xx,pos=1);im}))}


# From PerformanceAnalytics
# @author Brian Peterson
# @author Peter Carl

rundistn_from_ts <- function(indta, signalcolumn="") { #dt 7.8x
    if(!is.data.table(indta)) { indta=data.table(indta)}
    if(signalcolumn %in% colnames(indta)) {
        indta$sigrtn=indta[[signalcolumn]] }
    else {
        indta$sigrtn=sign(indta$value) }
    rlenc=rle(indta$sigrtn)
    indta[cumsum(rlenc$lengths),"siga"]=rlenc$values
    indta[cumsum(rlenc$lengths),"nrun"]=1:length(rlenc$values)
    indta$nrun=na.locf(indta$nrun,fromLast=TRUE)
    indta[,.(signrtn=sum(siga,na.rm=T), END_DT_ENTRY=max(DT_ENTRY), DT_ENTRY=min(DT_ENTRY), len=.N,cumrtn=sum(value,na.rm=T)), by=.(nrun)]
   }

# gets dataframe of run lengths given value in dataset
runs_from_value <- function(indta, addrunlength=FALSE) {
    if(!is.data.table(indta)) { indta=data.table(indta)}
    if(nrow(indta[,.N,by=.(DT_ENTRY)][N>1])>0) {stop("runs_from_value needs one observation per date")    }
    levs=indta[,.(runasnum=max(as.numeric(value))),by=.(value)]
    rlenc=rle(as.numeric(indta[["value"]]))
    runs1=data.table(END_DT_ENTRY=indta[cumsum(rlenc$lengths),]$DT_ENTRY, runasnum=(rlenc$values))
    if(addrunlength) { runs1$runlen = rlenc$lengths }
    runs1$DT_ENTRY = lag(runs1$END_DT_ENTRY,1)+1
    runs1= levs[runs1,on=.(runasnum)][!is.na(value)][,`:=`(runasnum=NULL)]
    runs1[]
}


# Improved: passes all numeric values, generic Date id (7 dec 2011)
xts2df <- function(x) {
    if(is.data.frame(x)) { return(as.data.table(x)) }
    if(is.null(colnames(x))) {
        if(is.null(ncol(x))) {
            return(data.table())
        }
        colnames(x)=paste0("V",1:ncol(x))
    }
    #data.frame(DT_ENTRY=zoo::index(x), coredata(x), check.names=FALSE)
    rtna = setnames(as.data.table(x),"index","DT_ENTRY")
    rtna[,DT_ENTRY:=as.Date(DT_ENTRY)]
    return(rtna)
}


df2xts <- function(indata,nantozero=FALSE,fmt="%m/%d/%Y") {
    if(is.xts(indata)) { return(indata)}
    if(nrow(indata)==0) { return(xts()) }
    indata=as.data.frame(indata)  # Needed to get out tibbleshit
    #classtypes = sapply(1:ncol(indata),function(i){class(indata[,i])})..does not work with dbl
    #classtypes = map_chr(head(indata,0), base::class)
    classtypes <- sapply(indata,class, simplify="array")
    idxnum<-grep("numeric|int|array",classtypes)
    dtnm<-grep("Date|POSIXct",classtypes)
    # wasdtnm<<-grep("(dt|date)",names(indata),value=TRUE,perl=TRUE,ignore.case=TRUE)
    #dtvec=indata[,dtnm]  #if in tbl form: need as.vector(as.list(indata[,dtm]))
    #dtvec=unlist(indata[,dtnm])  #if in tbl form: need as.vector(as.list(indata[,dtm]))
    if(length(dtnm)>0) {
#        if(!lubridate::is.Date(indata[1,dtnm])) { indata[,dtnm]<-as.Date(as.character(dtvec),format=fmt)}
#        out<-as.xts(indata[,idxnum],order.by=dtvec)  # Make into xts object
        out<-as.xts(indata[,idxnum],order.by=indata[[dtnm]])
    }
    else if(lubridate::is.Date(as.Date(rownames(indata)[1] ))) {
        out<-as.xts(indata[,idxnum],order.by=as.Date(rownames(indata)))  }
    else {
        print(paste(" df2xts Cannot coerce to xts : ",traceback()))
        out<-indata[,idxnum]
    }
   names(out)<-names(indata)[idxnum]
   if(nantozero) {   out[is.nan(out)]<-0 }
   out
}

annvol<-function(x,Time,acadj=FALSE,rtnacf=FALSE) { # Not sure this is right
    mult=sqrt(Time)
    if(acadj | rtnacf) {
        acval = acf(x,plot=FALSE)[1]$acf[1][1]
        mult = sqrt((1+acval)/(1-acval)*(Time-2*acval*(1-acval^Time)/(1-acval^2)))
        #message(" acval:", acval," mult: ",mult)
        if(rtnacf) { return(acval)}
    }
    sd(x,na.rm=T)*mult
}



gt.basetheme<-function(x,gtopts="all",sizepct=70,style=4,digits=2,seps=FALSE,na_format="-",interactive=FALSE) {
    if(gtopts=="all" | gtopts=="fmtnumber") {
        x = x |> tab_style_body(style=cell_text(color="red"),columns=where(is.numeric),fn=function(x) x<0) |> fmt_number(accounting=TRUE,decimals = digits,use_seps=seps)
        x = x |> sub_missing(missing_text=na_format)
    }
    if(gtopts=="all" | gtopts=="theme") {
        x = x  |> opt_stylize(style=style) |> tab_options(table.font.size=sprintf("%2.0f%%",sizepct))
    }
    if(gtopts=="all" | gtopts=="padding") {
        x = x |> opt_vertical_padding(scale=0.6)
    }
    if(interactive==TRUE) {
        x = x |> opt_interactive(use_filters = TRUE,page_size_default=70)
    }

    return(x)
}
