fgts_BaseTheme <- function(base_size = 8,xangle=90,yangle=90, tit_mult=1.3, axiscolor="grey20", gridstyle="dotted",
                        plotbackground="white",
                        legend="inside",title="",subtitle=NULL, caption="",legendbackground="blue",
                        strip_face="bold",strip_mult=1.2,strip_color="black"  ) {
#    if(nchar(title)>0) {    thistheme <- ggtitle(title,subtitle=subtitle)+theme_bw() }
#    if(nchar(caption)>0) {  thistheme <- thistheme + labs(caption=caption)+theme_bw() }
    legendalpha = ifelse(legendbackground=="white",1,0.1)
    thistheme <- theme_bw() %+replace%
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

