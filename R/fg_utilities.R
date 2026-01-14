# Timing averages on a yield curve : ggp, 306ms 10kb ggj, 2949ms 354kb  ggg@600, 6741ms 203kb ggb@400: 3506ms / 23mb
psave <- function(title,ingr,basedir=paste0(respath,"rres",filesep),w=10,h=8,saveoverride=NA, saveas="ggj") {
  saveHash<-hash("ggp"=c(".pdf",900), "ggj"=c(".jpeg",900),"ggg"=c(".png",600),"ggb"=c(".bmp",400),"plotly"=c(".html",1200))
  if(!is_ggplot(ingr) | nchar(title)==0) { return(ingr) }  # return gracefully
  if(substr(title,1,1)==" " | !exists("saveas")) { suppressWarnings(print(ingr)); return(); }
  if(!is.na(saveoverride)) { saveas=saveoverride }
  if(is.null(saveas) | saveas=="") { suppressWarnings(print(ingr)); }
  else  {
    fn<-paste0(format(lubridate::now(),"%y%m%d"),"_",title)
    if(is_ggplot(ingr) | class(ingr)[[1]]=="arrangelist") {
      if(saveas=="plotly") {
        #Sys.setenv(RSTUDIO_PANDOC="c:\\\\Program Files\\Rstudio\\bin\\quarto\\bin")
        knitr::opts_knit$set(echo=FALSE, message=FALSE, warning=FALSE, fig.width=9);
        #cAssign("title;ingr;basedir;fn;saveHash;saveas")
        #suppressWarnings(htmlwidgets::saveWidget(ggplotly(ingr,tooltip="text"), paste0(basedir,"\\",fn,saveHash[[saveas]][1]), selfcontained=TRUE,libdir ="c:\\d\\res\\rres\\deps"))
        suppressWarnings(htmlwidgets::saveWidget(ggplotly(ingr), paste0(basedir,"\\",fn,saveHash[[saveas]][1]), selfcontained=FALSE,libdir ="c:\\d\\res\\rres\\deps"))
      }

      else {
        suppressWarnings(ggsave(filename=paste0(fn,saveHash[[saveas]][1]),plot=ingr,dpi=as.numeric(saveHash[[saveas]][2]),path=basedir,width=w,height=h))
      }
      if(class(ingr)[[1]]=="arrangelist") { return(fn) }
    }
    if(is.character(ingr)) { ingr=paste0(basedir,fn,saveHash[[saveas]][1]) }
  }
  ingr
}

lm_eqn = function(df,ynm,xnm,rtnstyle=""){
  df2<-select(df,x=!!{xnm},y=!!{ynm})
  m = lm(y ~ x, df2);
  llm<-list(y=ynm, x=xnm,a = format(coef(m)[1], digits = 2),
            b = format(coef(m)[2], digits = 2),
            r2 = format(summary(m)$r.squared, digits = 3),
            lastresid=tail(resid(m),1),
            lastsresid=tail(rstandard(m),1))
  if(rtnstyle=="list") { return(llm) }
  else if (rtnstyle=="simple") {
    return(paste0("y=",llm[4]," x + ",llm[3]," rsq: ",llm[5]))        }
  else if (rtnstyle=="simplewitht") {
    return(paste0("y=x*",llm[4]," (",format(summary(m)$coefficients[6],digits=2),")+ ",llm[3]," (",
                  format(summary(m)$coefficients[5],digits=2), ") rsq: ",llm[5], " eLast:",format(llm[6],digits=2),"/z:",
                  format(llm[7],digits=2)))        }
  else {
    eq <- substitute(expression(y == a + b * x ~~rsq == r2),
                     list(y=ynm, x=xnm,a = format(coef(m)[1], digits = 2),
                          b = format(coef(m)[2], digits = 2),
                          r2 = format(summary(m)$r.squared, digits = 3)))
    return(as.character(as.expression(eq)))
  }
}

gline_identify<- function(g1,xin,yin,color="blue",addpoint=FALSE)  {
  rangex= layer_scales(g1)$x$range$range
  rangey= layer_scales(g1)$y$range$range
  toplot = data.frame(x1=c(xin,xin),y1=c(yin,yin), x2=c(xin,rangex[1]),y2=c(rangey[1],yin), label=c(xin,yin), hj=c("center","center"),vj=c("center","center"))
  gga=list(geom_segment(aes(x=x1,y=y1,xend=x2,yend=y2),data=toplot,linetype=2,color=color),geom_label(aes(x2,y2,label=label,hjust=hj,vjust=vj),data=toplot,size=3))
  if(addpoint) { gga=c(gga,geom_point(aes(x=x1,y=y1,color=color), data=data.frame(x1=xin,y1=yin), size=3))}
  gga
}

gline_x<-function(gridlines=FALSE,color="blue",int=0,label=NULL,...) {   gga=geom_hline(yintercept=int,color=color,...)
if(gridlines) { gga=gga+theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) }
gga
}
gline_y<-function(gridlines=FALSE,color="blue",int=0,label=NULL,...) {  gga=geom_vline(xintercept=int,color=color,...)
if(gridlines) { gga=gga+theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) }
gga
}

legendPosition <- function(legend,title="",background= element_rect(fill=alpha('blue', 0.1)),pctin=0.9) {
  p<-NULL
  if(nchar(title)>1) { mytit=element_text(title) } else { mytit=element_blank() }
  if(legend=="inside") { p<-theme(legend.position="inside",legend.position.inside=c(pctin,pctin),legend.justification=c(1,1), legend.title=mytit,legend.background = background) }
  else if(legend=="topleft") { p<-theme(legend.position="inside",legend.position.inside=c(1-pctin,pctin),legend.justification=c(0,1), legend.title=mytit,legend.background = background) }
  else if(legend=="bottomleft") { p<-theme(legend.position="inside",legend.position.inside=c(1-pctin,1-pctin),legend.justification=c(0,0), legend.title=mytit,legend.background = background) }
  else if(legend=="insidebottom" | legend=="bottomright") { p<-theme(legend.position="inside",legend.position.inside=c(pctin,1-pctin),legend.justification=c(0,0), legend.title=mytit,legend.background = background) }
  else if(legend=="insidetop" || legend=="topright") { p<-theme(legend.position="inside",legend.position.inside=c(pctin,pctin),legend.justification=c(0,1), legend.title=mytit,legend.background = background) }
  else if (grepl("loc:",legend)) {
    ll=as.numeric(s(gsub("loc:","",legend)));
    p<-theme(legend.position.inside=c(ll[1],ll[2]),legend.justification=c(ll[3],ll[4]), legend.title=mytit,legend.background = background) }
  else if (nchar(legend)>0) { p<-theme(legend.position=legend, legend.title=mytit,legend.background = background) }
  else if (legend=="none") { p<-theme(legend.position="none", legend.title=mytit,legend.background = background) }
  return(p)
}

quadrantBoundBox<-function(type,x,y,color="red",alpha=0.2) {
  if(type=="lowerleft") { qbb1=data.frame(x=c(min(x),min(x),0,0),y=c(min(y),0,0,min(y))) }
  if(type=="upperright") { qbb1=data.frame(x=c(max(x),max(x),0,0),y=c(max(y),0,0,max(y))) }
  geom_polygon(data=qbb1,aes(x,y),fill=color,alpha=alpha)
}

knotform<-function(dta,type="quantile",knots="") {
  if(type=="quantile") {
    if(length(knots)<=1) { knots=c(0.2,0.5) }
    hdgknot=quantile(dta,knots) }
  else { hdgknot=knots }
  paste0("y~bs(x,knots=c(",paste(hdgknot,collapse=","),"),degree=1)")
}

make_datecutlabels=function(dta, dtsback=c(7,22),lastonly=FALSE,maxdate=NA_real_,labels=NULL) {
  if(is.Date(dtsback[1])) {
    tortn = cut(dta$DT_ENTRY, c(dtsback,Sys.Date()+20), labels=labels)
  }
  else {
    if(is.null(labels)) {  labels = paste0(c("0",dtsback),":",c(dtsback-1,"x"))}
    maxdate = fcoalesce(as.numeric(as.Date(maxdate)),max(as.numeric(dta$DT_ENTRY)))
    daysvec = maxdate-as.numeric(dta$DT_ENTRY)
    tortn = cut(daysvec,c(-1,dtsback,99999), labels=labels)
    if(lastonly) {  tortn = ifelse(tortn=="0:1",tortn,"")    }
  }
  paste0(as.character(dta$colfactor),",",as.character(tortn))
}
