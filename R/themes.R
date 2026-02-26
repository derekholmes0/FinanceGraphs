#' @import ggplot2
gline_identify<- function(xdata,bbox)  {
  toplot <- data.frame(x2=c(NA_real_,bbox[[1,1]]),y2=c(bbox[[1,2]],NA_real_),inbox=rep(TRUE,2))
  toplot <- merge(xdata,toplot,all=TRUE)[,let(x2=fcoalesce(x2,xx),y2=fcoalesce(y2,yy))]
  gga=list(geom_segment(aes(x=xx,y=yy,xend=x2,yend=y2),data=toplot,linetype=2),
           geom_label(aes(x=x2,y=y2,label=lastlabel),size=3, data=toplot[abs(x2-xx)>0.01,][,let(lastlabel=format(yy,digits=1))]),
           geom_label(aes(x=x2,y=y2,label=lastlabel),size=3, data=toplot[abs(y2-yy)>0.01,][,let(lastlabel=format(xx,digits=1))])
  )
  gga
}

#' @import ggplot2
gline_x<-function(gridlines=FALSE,color="blue",int=0,label=NULL,...) {   gga=geom_hline(yintercept=int,color=color,...)
if(gridlines) { gga=gga+theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) }
gga
}

#' @import ggplot2
gline_y<-function(gridlines=FALSE,color="blue",int=0,label=NULL,...) {  gga=geom_vline(xintercept=int,color=color,...)
if(gridlines) { gga=gga+theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) }
gga
}

#' @import ggplot2
legd_guide <- function(legendstr,guidetype="legend",title=waiver(),pctin=0.9,ncats=0,
                           background= element_rect(fill=alpha('blue', 0.1))) {
  xleg=NULL
  ppos_map <- data.table(xleg=s("top;topleft;topright;bottom;bottomleft;bottomright"),
                         ppos=s("top;left;right;bottom;left;right"))
  ppos <- ppos_map[xleg==legendstr,]
  ppos <- fifelse(nrow(ppos)<=0,"inside",ppos[1,]$ppos)
  ncols <- floor((ncats-1)/5)+1
  if (grepl("loc:",legendstr)) {
    ll=as.numeric(s(gsub("loc:","",legendstr)));
    p<-theme(legend.position.inside=c(ll[1],ll[2]),legend.justification=c(ll[3],ll[4])) }
  else {
    # Prefer left/right isnted of up/down
    ptheme<-switch(legendstr,
        inside = theme(legend.position.inside=c(pctin,pctin),legend.justification=c(1,1)),
        insidebottomleft = theme(legend.position.inside=c(1-pctin,1-pctin),legend.justification=c(0,0)),
        insidebottom = theme(legend.position.inside=c(pctin,1-pctin),legend.justification=c(0,0)),
        insidebottomright = theme(legend.position.inside=c(pctin,1-pctin),legend.justification=c(0,0)), # Same
        insidetopleft= theme(legend.position.inside=c(1-pctin,pctin),legend.justification=c(0,1)),
        insidetop = theme(legend.position.inside=c(pctin,pctin),legend.justification=c(0,1)),
        insidetopright = theme(legend.position.inside=c(pctin,pctin),legend.justification=c(0,1)),
        top = theme(legend.position="top"),
        topleft = theme(legend.position="left"),
        topright = theme(legend.position="right"),
        bottom = theme(legend.position="bottom"),
        bottomleft = theme(legend.position="left"),
        bottomright = theme(legend.position="right"),
        right = theme(legend.position="right"),
        left = theme(legend.position="right"),
        none = theme(legend.position="none")
    )
    }
  if(ncats<=1) {
    return("none")
    }
  else {
    #return(guide_legend(title=title,position=ppos,nrow=min(ncats,5),ncol=ncols,theme=p))
    ptheme <- theme(legend.position="inside") %+replace% ptheme
    if(guidetype=="legend") {
      return(guide_legend(position=ppos,title=title,nrow=min(ncats,5),ncol=ncols,theme=ptheme))
      }
    else if (guidetype=="binned") {
      return(guide_bins(position=ppos,title=title,theme=ptheme))
      }
  }
}

#' @import ggplot2
fgts_set_gridstyle <- function(thistheme,gridstyle=NA_character_) {
  gridstyle <- fcoalesce(gridstyle,"dotted")
  gridcolor <- fg_get_aesstring("gridcolor")
  if( gridstyle=="dotted") {
    thistheme <- thistheme %+replace% theme(
      panel.grid.minor.y=element_blank(), panel.grid.major.y=element_line(colour = gridcolor, linetype = 'dashed'),
      panel.grid.minor.x=element_blank(), panel.grid.major.x=element_line(colour = gridcolor, linetype = 'dashed')
    )
  }
  if( gridstyle=="dotted_x") {
    thistheme <- thistheme %+replace% theme(
      panel.grid.minor.y=element_line(colour = gridcolor, linetype = 'dotted'), panel.grid.major.y=element_line(colour = gridcolor, linetype = 'dashed'),
      panel.grid.minor.x=element_blank(), panel.grid.major.x=element_blank()
    )
  }
  if( gridstyle=="dotted_y") {
    thistheme <- thistheme %+replace% theme(
      panel.grid.minor.x=element_line(colour = gridcolor, linetype = 'dotted'), panel.grid.major.x=element_line(colour = gridcolor, linetype = 'dashed'),
      panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank()
    )
  }
  if( gridstyle=="none") {
    thistheme <- thistheme %+replace% theme(
      panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank(),
      panel.grid.minor.x=element_blank(), panel.grid.major.x=element_blank()
    )
  }
  return(thistheme)
}


#' @import ggplot2
fgts_BaseTheme <- function(base_size = 8,xangle=90,yangle=90, tit_mult=1.3, axiscolor="grey20", gridstyle="dotted",
                        plotbackground="white",
                        legend="inside",title="",subtitle=NULL, caption="",legendbackground="blue",
                        strip_face="bold",strip_mult=1.2,strip_color="black"  ) {
#    if(nchar(title)>0) {    thistheme <- ggtitle(title,subtitle=subtitle)+theme_bw() }
#    if(nchar(caption)>0) {  thistheme <- thistheme + labs(caption=caption)+theme_bw() }
    legendalpha = ifelse(legendbackground=="white",1,0.1)
    thistheme <- ggplot2::theme_bw() %+replace%
	theme(
            axis.line =         element_blank(),
            axis.text.x =       element_text(size = base_size , lineheight = 0.9, colour = axiscolor, hjust = 1, angle = xangle),
            axis.text.y =       element_text(size = base_size , lineheight = 0.9, colour = axiscolor, hjust = 1),
            axis.ticks =        element_line(colour = "grey20"),
            axis.title.x =      element_text(size = base_size),
            axis.title.y =      element_text(size = base_size, angle = yangle),
            #axis.ticks.length = unit(0.15, "cm"),
            #axis.ticks.margin = unit(0.1, "cm"),

            legend.background = element_rect(fill=alpha(legendbackground, legendalpha)),
            legend.key =        element_rect(fill = "grey95", colour = "white"),
            #legend.key.size =   unit(1.2, "lines"),
            legend.text =       element_text(size = base_size * 1),
            legend.title =      element_text(size = base_size * 0.8, hjust = 0),
            legend.spacing = 	unit(base_size * 0.8, "pt"),

            panel.background =  element_rect(fill = plotbackground, colour = NA),
            panel.border =      element_blank(),
            panel.grid.major =  element_line(colour = "grey80"),
            panel.grid.minor =  element_line(colour = "grey80", linewidth = 0.25),
            #panel.margin =      unit(0.25, "lines"),

            strip.background =  element_rect(fill = "wheat", colour = NA),
            strip.text.x =      element_text(size = base_size * strip_mult, face=strip_face, color=strip_color),
            strip.text.y =      element_text(size = base_size * strip_mult, face=strip_face, color=strip_color, angle = -90),

            plot.background =   element_rect(fill = "grey90"),
            plot.title =        element_text(size = base_size * tit_mult, face="bold"),
            plot.subtitle =     element_text(size = base_size, hjust=0),
            plot.caption = 		element_text(size = base_size * 0.9)
            #plot.margin =       unit(c(1, 1, 0.5, 0.5), "lines")`
    )
    thistheme <- fgts_set_gridstyle(thistheme,gridstyle)
    thistheme
}

