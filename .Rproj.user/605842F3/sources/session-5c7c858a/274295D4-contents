#' @import ggplot2
gline_identify<- function(g1,xin,yin,color="blue",addpoint=FALSE)  {
  x1=y1=x2=y2=label=hj=vj=NULL
  rangex= layer_scales(g1)$x$range$range
  rangey= layer_scales(g1)$y$range$range
  toplot = data.frame(x1=c(xin,xin),y1=c(yin,yin), x2=c(xin,rangex[1]),y2=c(rangey[1],yin), label=c(xin,yin), hj=c("center","center"),vj=c("center","center"))
  gga=list(geom_segment(aes(x=x1,y=y1,xend=x2,yend=y2),data=toplot,linetype=2,color=color),geom_label(aes(x2,y2,label=label,hjust=hj,vjust=vj),data=toplot,size=3))
  if(addpoint) { gga=c(gga,geom_point(aes(x=x1,y=y1,color=color), data=data.frame(x1=xin,y1=yin), size=3))}
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
            legend.position =   "right",
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
    if( gridstyle=="dotted") {
        thistheme <- thistheme %+replace% theme(
            panel.grid.minor.y=element_blank(), panel.grid.major.y=element_line(colour = 'gray', linetype = 'dashed'),
            panel.grid.minor.x=element_blank(), panel.grid.major.x=element_line(colour = 'gray', linetype = 'dashed')
            )
    }
    if( gridstyle=="dotted_x") {
        thistheme <- thistheme %+replace% theme(
            panel.grid.minor.y=element_line(colour = 'gray', linetype = 'dotted'), panel.grid.major.y=element_line(colour = 'gray', linetype = 'dashed'),
            panel.grid.minor.x=element_blank(), panel.grid.major.x=element_blank()
            )
    }
    if( gridstyle=="dotted_y") {
        thistheme <- thistheme %+replace% theme(
            panel.grid.minor.x=element_line(colour = 'gray', linetype = 'dotted'), panel.grid.major.x=element_line(colour = 'gray', linetype = 'dashed'),
            panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank()
            )
    }
    if( gridstyle=="none") {
        thistheme <- thistheme %+replace% theme(
            panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank(),
            panel.grid.minor.x=element_blank(), panel.grid.major.x=element_blank()
            )
    }
    if(nchar(legend)>0) {
         thistheme <- thistheme + legendPosition(legend,title=title,background=element_rect(fill=alpha(legendbackground, legendalpha)))    }
    thistheme
}

