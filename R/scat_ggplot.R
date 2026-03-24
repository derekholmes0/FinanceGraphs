#' fg_scatplot:  Easy scatterplot generator, with time specific enhancements
#' @name fg_scatplot
#' @description
#' Plots bivariate plots with some time-series specific enhancements.  Rather than programmatically describing
#' graph aesthetics, a simple formula-based approach is used.  This approach allows quick specification of
#' many customizaton options.
#' @usage
#' fg_scatplot(indata,plotform,type="scatter",datecuts=c(7,66),
#'                noscales="",xdecoration="",ydecoration="",annotatecorners="",
#'                tsize=3,psize=1,n_color_switch=7,n_hex_switch=400,repel=TRUE,jitter=c(0,0),
#'                title="",subtitle="",caption="",axislabels="",
#'                boundbox=c(),boundboxtype="",gridstyle=NA_character_,legendinside=FALSE,
#'                tformula=formula("y~x"),returnregresults=FALSE,
#'                keepcols="", meltvar="variable",melted=NULL)
#' @param indata `data.frame` with columns for (x,y) coordinates and possibly other categorical data or a date column. Alternatively,
#' `indata` can be in long format with `meltvar` present.  Note that aesthetic characteristics (if used) must be present for both
#' long and wide input formats.
#'
#'
#' @param plotform  A text formula describing how to set up the graph.  The formula is of the
#' general form
#'
#' `y ~ x + option:<column_name>,<aesthetic category> + ...`
#'
#' where `y` is plotted against `x` and aesthetics for each point are controlled by one or more `option` clauses each followed
#' by zero or more optional parameters.   If the option applies to all points then the first parameter `column_name` must be in `indata`.
#' By default, points or symbols are plotted.  By general category, the options are
#'
#' Aesthetic options:
#'
#'  * `color:column<,aes_set>` sets the color of each point or label by data in `column`
#'  * `symbol:column<,aes_set>` sets the symbol or shape cof each point or label by data in `column`
#'  * `size:column<,aes_set>` sets the size of each point by the data in `column`
#'
#' Date specific options:
#'
#'  * `doi:recent` partitions data by number of days in `datecuts`prior to the last day in `indata`
#'  * `doi:<doiset>` partitions data by date ranges obtained from dates of interest set `<doiset>`.  See [fg_get_dates_of_interest()]
#'  * `point:<value|label|anno><all>` adds highlights for either the last date in `indata` or the last
#'          date for each group (if `all`). `value` gives coordinates, `label` the label in the `color` column, while `anno`
#'          adds lines to each axis.
#'
#' Text options:
#'
#'  * `[text|label|labelhilight|tooltips]:column<,aes_set>` : Plots the text in character `column` as text (without border),
#'        label (with border), filled in label, or mouse-over tooltip. (See details)
#'
#' Other annotations:
#'
#'  * `ellipse` adds an ellipse around the points using [ggplot2::stat_ellipse()]
#'  * `hull<:quantile>` draws a convext hull around points with `<quantile>` (default 0) points removed. (See details)
#'  * `xline<:level=0>` draws a vertical line  at `level`
#'  * `yline<:level=0>` draws a horizontal line  at `level`
#'  * `grid:<dotted|dotted_x|dotted_y|none>` formats background grids
#' @param type character string for the type of graph to plot:
#'  * `scat` plots points, text or labels
#'  * `lm<one><noeqn><nofill>` adds linear regression lines per `color` category or across all points (`lmone`).
#'  * `loess<one><noeqn>`adds loess line per per `color` category or across all points (`loessone`)
#'  * `density` Creates a density plot
#'
#' if `noeqn` is part of the string, equations are suppressed.  If `one` is part of the string, no subcategories are used. `nofill``
#' removes confidence bands.
#'
#' @param noscales String to suppress guides with any of `<color|size|symbol>`
#' @param datecuts list of integers (Default `c(7,66)` for days prior to last date to make date classes. (See examples and `doi:recent` as above.)
#' @param xdecoration,ydecoration 2 element string list to add to either side of an axis label.
#' @param annotatecorners 4 element string list to add notes to each of 4 quadrants of the graph.  See examples.
#' @param tsize default text size (with some scaled variations for graph parts such as titles)
#' @param psize default point size.
#' @param n_color_switch (Default 7): Number of distint color categories beyond which colors are taken from gradient scales,
#' unless a color set is specified in a `color` part of `plotform`
#' @param n_hex_switch (Default 400): Number of data points beyond which points are replaced with binned hexagons (see [geom_hex()])
#' @param repel (Default TRUE) Text and labels are plotted using [ggrepel()]
#' @param jitter Jitter parameters used by [geom_text()] or [geom_label()] if `repel=FALSE`. Default is no jitter.
#' @param title Title to add to graph
#' @param subtitle Subtitle to add to graph
#' @param caption Caption to add to graph
#' @param axislabels Semicolon separated string with x and y labels, e.g `date;OAS`
#' @param boundboxtype,boundbox string describing how to use bounding boxes.  If `"identify"` is part of the string, then data
#' is truncated to the calculated bounding box and notations to that effect are added to the graph.  If not, then points
#' outside the box are dropped.
#'  * `prob|probidentify` calculates bounding boxes from quantilies of `x` and `y` data.  See vignette for details.
#'  * `value|valueidentify` are minimum and maximum values of `x` and `y` axes to show.
#'      If `boundbox` is a list of two numbers, the y axis is truncated to those values,
#'      If `boundbox` is a list of 4 numbers `c(xmin,xmax,ymin,ymax)` data is truncated to that box.
#'
#' @param gridstyle String in `<dotted|dotted_x|dotted_y|none>` to contol grids as in `grid` option above.
#' @param legendinside (Default: TRUE) Put all guides inside the graph.
#' @param tformula (Default `y~x`) Formula used within `lm` or `loess` stats .
#' @param returnregresults Return a two elemet list c(plot,regression `data.frame`). Only available for linear models, and uses the first amoung
#' options `c("color","symbol","size","alpha")` as grouping variables
#' @param keepcols list of `indata` columns to be kept with the graph data, useful for further faceting using [ggplot2::facet_wrap()]
#' @param meltvar (Default `"variable"`) If `indata` is melted, then this is used to create `x` and `y` categories.
#' @param melted (Default:NULL) If `FALSE` forces data not to be melted if `meltvar` in `indata`
#' @returns A [ggplot2::ggplot()] object with desired graph, or a [ggiraph::girafe()] object if `tooltips` is in the `plotform` string.
#' @examples
#' # Simple text examples
#' require(data.table)
#' dt_mtcars=data.table(datasets::mtcars)
#' dt_mtcars$id=lapply(rownames(datasets::mtcars),\(x) last(strsplit(x," ")[[1]]))
#' fg_scatplot(dt_mtcars,"disp ~ hp + color:am + text:id","scatter",title="text basic")
#' fg_scatplot(dt_mtcars,"disp ~ hp + color:carb + label:id","scatter",
#'                 n_color_switch=0,title="scat color switch")
#' # Plotting data with dates:
#' set.seed(1); ndates <- 400;  dlyvol <- 0.2/sqrt(260)
#' rtns <- cbind(cumsum(rnorm(ndates,sd=dlyvol)),cumsum(rnorm(ndates,sd=dlyvol)))
#' dttest <- data.table(date=seq(as.Date("2021-01-01"),as.Date("2021-01-01")+ndates-1),
#'             xtest=100*(1+rtns[,1]),ytest=100*(1+rtns[,2]),
#'             ccat=fifelse(runif(ndates)<=0.2,"Rare","mkt"))
#' # Making categories out of recent data
#' fg_scatplot(dttest,"ytest ~ xtest + doi:recent","scatter",datecuts=c(66,122),title="from recent")
#' fg_scatplot(dttest ,"ytest ~ xtest + color:ccat + doi:recent + point:label","scat",
#'               datecuts=c(7,66),title="recent w label")
#' # Makes categories out of event sets from [fg_get_dates_of_interest()]
#' fg_scatplot(dttest,"ytest ~ xtest + doi:regm","scatter",title="from a regime")
#' # Point graphing switches.
#' fg_scatplot(dttest,"ytest ~ xtest + color:ccat","lm",n_hex_switch=100,title="Hex Switch")
#' # Quick changes to aesthetic sets
#' fg_scatplot(dttest,"ytest ~ xtest + color:ccat,altlines_6","loess",title="Alternate colors")
#' # Extra summarizatons
#' fg_scatplot(dttest,"ytest ~ xtest + color:ccat + hull:0.1 + ellipse","lm",title="Curves")
#' # Annotations
#' fg_scatplot(dttest,"ytest ~ xtest + color:ccat + point:labelall","scat",title="Last Values")
#' fg_scatplot(dttest,"ytest ~ xtest + color:ccat + point:anno","scat",annotatecorners="NW;NE;SE;SW",
#'                 legendinside = FALSE)
#' @details
#' `indata` can either be in wide ('date' ,'series1',...) format or normalized (long) format
#' ('date','variable','value',...) format.  This package infers date columns names from column types and casts or
#' pivot_wider to get `x` and `y` columns.  Note that aesthetic characteristics (if used) must be present for both
#' long and wide input formats.
#'
#' Default aesehetic sets used for portions of the graph each have their own names, which can be seeing by running
#' [fg_print_aes_list()] and modified or added to using [fg_update_aes()].  The default theme can be modified using
#' [fg_replace_theme()].  Both aesthetic changes and theme changes are persistent across R sessions.
#'
#' Use of `doi:` in `plotform` string supercedes color aesthetics otherwise specified.  (Is this true?)
#'
#' If `tooltips` are used, the result of `fg_scatplot` must be viewed using `print(girafe(ggobj=fg_scatplot(...)))`
#' See [ggiraph::geom_point_interactive()]
#'
#' Winsorized hulls with quantile cutoff `q` are formed using the closest (by euclidean distance) `1-q` points to the
#' geograpic center of the entire set.
#'
#' Captions are added if data is truncated or omitted by the bounding box procedure.
#'
#' @importFrom graphics axis
#' @importFrom stringr str_split_1 str_replace_all str_extract
#' @importFrom stats quantile coef dist formula lm na.omit setNames resid rnorm rstandard runif var
#' @importFrom utils read.table
#' @importFrom broom tidy
#' @import ggrepel
#' @import data.table
#'
#' @export
fg_scatplot<-function(indata,plotform,type="scatter",datecuts=c(7,66),
                noscales="",xdecoration="",ydecoration="",annotatecorners="",
                tsize=3,psize=1,n_color_switch=7,n_hex_switch=400,repel=TRUE,jitter=c(0,0),
                title="",subtitle="",caption="",axislabels="",
                boundbox=c(),boundboxtype="",
                gridstyle=NA_character_,legendinside=FALSE,
                tformula=formula("y~x"),returnregresults=FALSE,
                keepcols="", meltvar="variable",melted=NULL) {

    if(nrow(indata)<=0) { return(ggplot()) }
    regres <- data.table()
    titleadds <- data.table(axis=c("title","subtitle","caption","x","y"),
                            note=c(title,subtitle,caption,s(axislabels)[1],s(axislabels)[2])  )
    pextra <- list()
    do_not_fill <- grepl("nofill",type)
    a2 <- data.table::copy(as.data.table(indata))
    dtcolno <- purrr::detect_index(a2,lubridate::is.instant)[[1]]
    colnamesnodate <- colnames(a2)[setdiff(1:ncol(a2),dtcolno)]
    if(dtcolno>0) {
      setnames(a2,dtcolno,"dt")
      a2<-a2[!is.na(a2$dt)]
    }

    # Cast molten data, if meltvar in data UNLESS melted=FALSE
    if(meltvar %in% colnamesnodate) {
      if( is.null(melted) || melted==TRUE ) {
        nonmeltcols <-  paste(setdiff(colnames(a2),c(meltvar,"value")),collapse="+")
        a2 <- dcast(a2,formula(paste(nonmeltcols,"~ ",meltvar)))
      }
    }
    # Keep DT of all possible column elements of the graph
    grparts <- data.table(nm=s("xx;yy;colfactor;labels;text;symbolfactor;labelhilight;tooltips;sizefactor;alphafactor;fill;doi"),
                         item = s("x;y;color;label;text;symbol;labelhilight;tooltip;size;alpha;fill;doi"),
                         ggaes = s("x;x;color;label;label;shape;label;label;size;alpha;x;fill"),
                         required=c(rep(TRUE,2),rep(FALSE,10)))
    # Use read.table to parse compoents neatly
    grparts <- scatform_to_df(plotform)[grparts,on=.(item)][,let(indta=colnm %in% colnames(a2)),by=.I][]

    # Mapping what to to do to what we need
    grparts <- grparts[order(colnm)][,ii:=.I-min(.I),by=.(colnm)]
    grparts <- grparts[,coltodo:=fcase(indta==TRUE & ii>0,"copy",indta==TRUE & ii==0,"rename",indta==FALSE,"not_needed")]
    # Set scales
    grparts <- grparts[,let(ii=NULL,addscales=(coltodo=="rename" & required==FALSE))]
    grparts <- grparts[item %in% s(noscales),let(addscales=FALSE)]
    firstcat <- first_category_nm(grparts)

    #Checks----
    if( nrow( badparts <- grparts[!is.na(colnm) & indta==FALSE & required==TRUE,] )>0 ) {
      message("fg_scatplot: ERROR: All of (",paste(badparts$colnm,collapse=","),") must be indata..  Quitting")
      stop()
    }
    if( nrow( badparts <- grparts[!is.na(colnm) & !(colnm %in% colnames(indata)) & !(item %in% c("doi"))] )>0 ) {
      message("fg_scatplot: WARNING: Modifiers ",paste(badparts$colnm,collapse=",")," not in data, so will be ignored")
    }

    # Category counts
    colcounts <- grparts[required==FALSE & indta==TRUE,.SD[1],by=.(colnm)]
    if(nrow(colcounts)>0) {
      colcounts <- colcounts[,let(ncnt=length(unique(a2[[colnm]]))),by=.I][]
      grparts <- colcounts[,.(colnm,ncnt)][grparts,on=.(colnm)]
    } else {
      grparts[,let(ncnt=0)]
    }
    # Miscellaneous fixes; Color style
    grparts[item=="color",let(style=fcoalesce(style,fifelse(ncnt>n_color_switch,"linebrewer","lines")))]
    # Keep DT of all possible other elements/options of the graph
    gropts <- data.table(item=s("ellipse;hull;grid;doi;point;xline;yline"),
                         include=c(FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE))
    gropts <- scatform_to_df(plotform)[,xinclude:=TRUE][gropts,on=.(item)]
    gropts <- gropts[,let(xinclude=fcoalesce(xinclude,FALSE),opt=colnm,opt2=style,opt3=order)]
    gropts <- gropts[,include := include | xinclude][,.SD,.SDcols=s("item;include;opt;opt2;opt3")]

    # Le Grande Reorganziation
    a2[,grparts[coltodo=="copy"]$nm := .SD,.SDcols=grparts[coltodo=="copy"]$colnm] # Copy dups
    setnames(a2,grparts[coltodo=="rename"]$colnm,grparts[coltodo=="rename"]$nm)  # rename rest
    a2[,grparts[coltodo=="makenull"]$nm := "1"] # Set rest to one element factors
    # Debug only:
    #cAssign("a2;grparts;gropts;boundbox;boundboxtype;colcounts",dbg=get("cassign",envir=the))
    # SHould be able to do group reset and DTmerge rest
    # Helper functions
    use_col <- function(what) { grparts[item==what]$indta }

    if( !is.na(doi<-grparts[item=="doi",]$colnm) & "dt" %in% colnames(a2)) {
      alldts=sort(unique(a2$dt))
      if(grepl("recent",doi)) {
        break_set <- form_breakset(alldts,datecuts)
        doi_aesname <- "sizefactor"
        grparts[item=="size",let(style="doisizemult",colnm="histcat",indta=TRUE,addscales=TRUE,ncnt=nrow(break_set))]
      }
      else { # Must be from dates_of_interest
        break_set <- form_breakset(alldts,doi)
        if(use_col("color")) { # Use color first, then symbol
          doi_aesname <- "symbolfactor"
          grparts[item=="symbol",let(style="scatshape",colnm="histcat",indta=TRUE,addscales=TRUE,ncnt=nrow(break_set))]
          message_if(the$verbose, "scat_ggplot using symbols to denote date of interest categories")
        }
        else {
          doi_aesname <- "colfactor"
          grparts[item=="color",let(style="lines",colnm="histcat",indta=TRUE,addscales=TRUE,ncnt=nrow(break_set))]
          message_if(the$verbose, "scat_ggplot using colors to denote date of interest categories")
        }
      }
      if(nrow(break_set)>0) {
        break_set <- break_set[,{doi_aesname}:=histcat][,.SD,.SDcols=c("BEG_DT_ENTRY","END_DT_ENTRY",doi_aesname)]
        tcollist <- union(names(a2),names(break_set))
        a2 <- break_set[a2,on=.(BEG_DT_ENTRY<=dt,END_DT_ENTRY>dt)][,dt:=BEG_DT_ENTRY][,.SD,.SDcols=tcollist]
        n_hex_switch <- +Inf
      }
      # This is the only place where we want to make factors
    }
    mycols_to_keep <- s("xx;yy;colfactor;text;labels;sizefactor;alphafactor;symbolfactor;dt;tooltips;labelhilight")
    cols_to_toss <- setdiff(colnames(a2),c(mycols_to_keep,s(keepcols)))
    a3<-a2[,.SD,.SDcols=!cols_to_toss]
    cols_to_factor <- intersect(colnames(a3),s("colfactor;symbolfactor"))
    a3<-a3[,(cols_to_factor):=lapply(.SD,fctr),.SDcols=cols_to_factor ][]

    cols_to_factor <- intersect(colnames(a3),s("sizefactor"))
    a3<-a3[,(cols_to_factor):=lapply(.SD,\(x) fctr(x,sort=TRUE)),.SDcols=cols_to_factor ][]

    # Bonding boxes, similar to squish
    bboxq <- function(colnm,xprob) { quantile(a3[[colnm]],xprob,na.rm=T) }
    bbox <- a3[,lapply(.SD,range),.SDcols=c("xx","yy")]
    boundboxtype <- tolower(boundboxtype)

    if(nchar(boundboxtype)>0) {
      if( grepl("value",boundboxtype) & length(boundbox)==2 ) { # y in (bb_1,bb_2)
        bbox$yy <- boundbox
      }
      if( grepl("value",boundboxtype) & length(boundbox)==4 ) { # x in (bb_1,bb_2, y in (bb_3,bb_4)
        bbox <- as.data.table( matrix(boundbox,nrow=2))
        setnames(bbox,c("xx","yy"))
      }
      if( grepl("prob",boundboxtype) & length(boundbox)==2 ) { # Q(x) in (bb1_1,1-bb_1), Q(y) in (bb_2,1-bb_2)
        bbox <- cbind(xx=bboxq("xx",c(boundbox[1],1-boundbox[1])), yy=bboxq("yy",c(boundbox[2],1-boundbox[2])))
        bbox <- as.data.table(bbox)
      }
      if( grepl("prob",boundboxtype) & length(boundbox)==4 ) { # Q(x) in (bb1_1,bb_2), Q(y) in (bb_3,bb_4)
        bbox <- cbind(xx=bboxq("xx",c(boundbox[1],boundbox[2])), yy=bboxq("yy",c(boundbox[3],boundbox[4])))
        bbox <- as.data.table(bbox)
      }
    }
    if(length(boundbox)>1 & the$verbose==TRUE) {
      message("Bounding Boxes ",boundboxtype," gives (x,y)=",paste(round(bbox,4),collapse=" "))}

    a3[,inbox:=(between(xx,bbox[[1,1]],bbox[[2,1]]) & between(yy,bbox[[1,2]],bbox[[2,2]]))]
    if(grepl("identify",boundboxtype) & sum(a3$inbox==FALSE)>0) {
      a3[inbox==FALSE,let(xx=pmax(pmin(xx,bbox[[2,1]]),bbox[[1,1]]), yy= pmax(pmin(yy,bbox[[2,2]]),bbox[[1,2]]))]
      # Include box
      oob_data <- data.table(xxmin=c(bbox[[1,1]]),xxmax=c(bbox[[2,1]]),yymin=c(bbox[[1,2]]),yymax=c(bbox[[2,2]]))
      oob_aes <- setNames(fg_get_aes("oob_box")$value,fg_get_aes("oob_box")$type)
      pextra[['oobbox']]<-geom_rect(aes(xmin=xxmin,xmax=xxmax,ymin=yymin,ymax=yymax),
                     color=oob_aes[['color']],fill=oob_aes[['fill']],alpha=as.numeric(oob_aes[['alpha']]),
                     data=cbind(a3[1,],oob_data))
      if(use_col("text") | use_col("label") | use_col("labelhilight")) {
        a3[inbox==FALSE,names(.SD):=lapply(.SD,\(x) fifelse(is.na(x),x,paste0(x,"**"))),
                          .SDcols=grparts[indta==TRUE & ggaes=="label"]$nm,by=.I]
        titleadds <- DTappend(titleadds, data.table(axis="caption",note="** : Point beyond borders shown"))
      }
      else {
        # Add rects to indicate squished components
        titleadds <- DTappend(titleadds, data.table(axis="caption",note="Box indicates values may be truncated"))
      }
    }
    else if (grepl("value|prob",boundboxtype)) {
      if(nrow(a3[inbox==FALSE,])>0) {
          titleadds <- DTappend(titleadds, data.table(axis="caption",note="Values outside box are dropped"))
      }
      a3 <- a3[inbox==TRUE,]  # Take out
    }
    # Set viewing area
    pct_x <- as.numeric(fg_get_aesstring("expand_x"))
    pct_y <- as.numeric(fg_get_aesstring("expand_y"))
    bbox[,let(xx=c((1-pct_x),(1+pct_x))*xx,yy=c((1-pct_y),(1+pct_y))*yy)]

   # Actual Plot
    tsizes <- as.numeric(fg_get_aesstring("scattextsize"))
    # DO later: colno a new color
    # Separate hex determination by group

    p<-ggplot(a3,aes(x=xx,y=yy))

    # Text -----------------------------------
    # ----------------------------------------
      pjit = position_jitterdodge(jitter.width=jitter[1],dodge.width=jitter[2])
      tface <- fg_get_aesstring("scattextfont")
      tsize_higlight <- as.numeric(fg_get_aesstring("labelhilightsize"))*tsize
      text_aes <- collect_aes(grparts[item %in% s("color;label;text")])
      # Get highlighted dataset
      if(use_col("label")) {
        if(repel==TRUE) {
          p<-p+ggrepel::geom_label_repel(text_aes,size=tsize,fontface=tface,max.overlaps=60)
        }
        else {
          p<-p+geom_label(text_aes,size=tsize,fontface=tface,position=pjit)
        }
      }
      if(use_col("labelhilight")) {
        hilight_aes <- collect_aes(grparts[item %in% s("color;labelhilight")])
        tfill <- fg_get_aesstring("labelhilight")
        if(repel==TRUE) {
          p<-p+ggrepel::geom_label_repel(hilight_aes,size=tsize_higlight,fill=tfill,fontface=tface,max.overlaps=60,data=a3[!is.na(labelhilight)])
        }
        else {
          p<-p+geom_label(hilight_aes,size=tsize_higlight,fontface=tface,fill=tfill,position=pjit,data=a3[!is.na(labelhilight)])
        }
      }
      if(use_col("text")) {
        if(repel==TRUE) {
          p<-p+ggrepel::geom_text_repel(text_aes,size=tsize,fontface=tface,max.overlaps=60)
        }
        else {
          p<-p+geom_text(text_aes,size=tsize,fontface=tface,position=pjit)
        }
      if( use_col("tooltip") ) {
          message("Remember: Show plot using `girafe(ggob=`")
          p<-p+ggiraph::geom_point_interactive(aes(x=xx,y=yy,tooltip=tooltips, data_id=tooltips),
                                               size=psize, color=fg_get_aesstring("ip_color"), alpha=0.2, show.legend=FALSE)
        }
      }
      # Check to see if thre's still points to be done.
      textcols <- grparts[grepl("label|tooltip",ggaes) & indta==TRUE,]$nm
      havetext <- length(textcols)>0
      if(havetext) { # Check for points that need to be plotted anyway
        a4 <- a3[,lapply(.SD,\(x) is.na(x) | nchar(x)<=0),.SDcols=textcols][,.(allbad=length(textcols)==sum(.SD)),by=.I]
        if(sum(a4$allbad)>0) {
          havetext <- FALSE
          a3 <- a3[which(a4$allbad)]
          }
        }
    if( !havetext ) { # Do Points
      # Get AES
      pts_aes <- collect_aes(grparts[!ggaes=="label"])
      hassize <- grparts[ggaes=="size",]$indta
      size_default <- switch(as.character(hassize),"TRUE"=list(),"FALSE"=list(size=psize))
      # Plot styles
      if(grepl("path",type)) {
          p<-p+pts_aes+geom_path(show.legend=TRUE,data=a3)  # Keep size default
      }
      else if (grepl("dens",type)) {  # Want unfilled always.
          p<-p+pts_aes + geom_density_2d()
      }
      else if (grepl("scat|lm|loess",type)) {
        # Now Separate out hex from points: First find the one key'd aethetic
        aes_to_focus <- first_category_nm(grparts,ifnone=NA_character_)
        if(!is.na(aes_to_focus)) {
          dohex_cats <- a3[,.(usehex=(.N>=n_hex_switch)),by=c(aes_to_focus)][usehex==TRUE,][,hexgp:=.I]
          if(sum(dohex_cats$usehex)>0) { # Doing Hex
            a3 <- dohex_cats[a3,on=c(aes_to_focus)][,usehex:=fcoalesce(usehex,FALSE)]
            # Wish I could get this to look better
            p<-p+geom_hex(aes(color=colfactor,fill=after_scale(scales::alpha(color,ncount)),alpha=after_stat(ncount)),
                                  data=a3[usehex==TRUE,],show.legend=FALSE)
            grparts[item=="fill",addscales:=FALSE]
          }
        }
        if(!("usehex" %in% names(a3))){
          a3 <- a3[,usehex:=FALSE]
        }
        p<-p + pts_aes + layer(geom="point",stat="identity",position="identity",params=size_default,data=a3[usehex==FALSE])
      }
    }

      # summary lines
    if(grepl("(lm|loess)",type)) {
      actmethod <-  fcoalesce(str_extract(type,"(lm|loess)"),"lm")
      regform <- fg_get_aesstring("lmeqn")
      if(returnregresults) {
        if(grepl("one",type)) {
          a3[,regfactor:="one"] }
        else {
          a3 <- a3[,regfactor:=get(firstcat)]
          }
        regres <-a3[,{lma=lm(tformula,data=.SD[,.(y=yy,x=xx)]);  tidy(lma)},by=.(regfactor)]
        regres <-regres[,p.value:=round(p.value,5)][]
      }
      talpha    = ifelse(do_not_fill,0,0.2)
      if(the$verbose) {   print(summary(lm(tformula,data=a3[,.(x=xx,y=yy)]))) }
      if( grepl("one",type)) {
        grparts[item=="alpha",addscales:=FALSE]
        p<-p+geom_smooth(aes(x=xx,y=yy),method=actmethod,formula=tformula,colour="black",alpha=talpha,show.legend=FALSE,inherit.aes=FALSE)
        if(!grepl("noeq",type) & grepl("lm",type)) {
          legx <- a3[,.(xx=quantile(xx,probs=c(0.1)),yy=quantile(yy,probs=c(0.1+.GRP/10)),labels=lm_eqn(.SD,"xx","yy",
                            tformula=tformula,rtnstyle=regform))]
          p<-p+geom_label(aes(x=xx,y=yy,label=labels),data=legx,color="black",size=tsize*0.8)
        }
      }
      else {
        lm_aes <- collect_aes(data.table(nm=rep(firstcat,2),ggaes=c("color","fill"),indta=rep(TRUE,2)))
        p<-p+lm_aes+geom_smooth(method=actmethod,formula=tformula,alpha=talpha,show.legend=FALSE,data=a3)
        if(!grepl("noeq",type) & grepl("lm",type)) {
          firstcat <- first_category_nm(grparts)
          legx_x <- 0.2*bbox[[2,1]]+0.8*(bbox[[1,1]])
          legx_y <- 0.2*bbox[[1,2]]+0.8*(bbox[[2,2]])
          legx_ydelta <- (bbox[[2,2]]-bbox[[1,2]])/15
          legx <- a3[,.(xx=legx_x,yy=legx_y - (.GRP-1)*legx_ydelta,
                        labels=lm_eqn(.SD,"xx","yy",tformula=tformula,rtnstyle=regform)),by=c(firstcat)]
          legx <- legx[,labels:=paste0(get(firstcat),":",labels)]
          lab_aes <- collect_aes(data.table(nm=firstcat,ggaes=c("fill"),indta=rep(TRUE,1)))
          grparts[nm=="fill",addscales:=TRUE]
          p<-p+lab_aes+geom_label(aes(x=xx,y=yy,label=labels),data=legx,color="black",size=tsize*0.8)
          #p<-p+annotate("text",x=legx[[1,"xx"]],y=legx[[1,"yy"]],label=legx[[1,"labels"]],size=floor(tsize*0.8))
        }
      }
    }

    # Additional figures and lines
    olist <- split(gropts,by=c("item"))
    if(olist$ellipse$include)  {
      lm_aes <- collect_aes(data.table(nm=firstcat,ggaes=c("color"),indta=TRUE))
      p<-p+stat_ellipse(lm_aes)
    }
    if(olist$hull$include) {
      winsorfac <- fcoalesce(as.numeric(olist$hull$opt),0)
      lm_aes <- collect_aes(data.table(nm=rep(firstcat,2),ggaes=c("color","fill"),indta=rep(TRUE,2)))
      p <- p + stat_chull(lm_aes,alpha=0.1,percentile=winsorfac)
    }
    if(olist$xline$include) {
      thisintercept <- fcoalesce(as.numeric(olist$xline$opt), bbox[[2,1]])
      thiscolor <- fcoalesce(olist$xline$opt2,fg_get_aesstring("xlinecolor"))
      p<-p+geom_vline(xintercept=thisintercept,color=thiscolor)
    }
    if(olist$yline$include) {
      thisintercept <- fcoalesce(as.numeric(olist$yline$opt), bbox[[2,1]])
      thiscolor <- fcoalesce(olist$yline$opt2,fg_get_aesstring("ylinecolor"))
      p<-p+geom_hline(yintercept=thisintercept,color=thiscolor)
    }

    # Viewing Area, need before glein_identify
    if(nchar(boundboxtype)>0) {  # Dont do this unless ncressary
      p<-p+coord_cartesian(xlim=bbox$xx,ylim =bbox$yy)
    }

    if(olist$point$include) {
      pointopts <- olist$point$opt
      todo <- fifelse(grepl("all",pointopts), "group","date")
      firstcat <- first_category_nm(grparts,ifnone="dt")
      lastdta <- rbindlist(list(a3[dt==max(dt),][,let(var="date")],a3[,.SD[.N],by=c(firstcat)][,let(var="group")]),fill=TRUE)
      lastdta <- lastdta[,label:=fifelse(grepl("value",pointopts),paste0(format(xx,digits=1),",",format(yy,digits=2)),""),by=.I]
      lastdta <- lastdta[,label:=fifelse(grepl("label",pointopts),paste(as.character(get(firstcat)),label),label),by=.I]
      if(grepl("anno",pointopts)) {
        p <- p + gline_identify(lastdta[var==todo,],bbox)
      }
      else {
        last_aes  <- collect_aes(grparts[item %in% s("color;xx;yy")])
        if(grepl("value|label",pointopts)) {
          p<-p+last_aes+ggrepel::geom_label_repel(aes(label=label),size=tsize,fill=fg_get_aesstring("scatlabel_bg"),
                                                  data=lastdta[var==todo,])
        }
        else {
          p<-p+last_aes+geom_point(data=lastdta[var==todo,],size=4)
        }
      }
    }
    # Axis Labels
    if(length(s(annotatecorners))==4) {
      # UL corner, UR corner, LR corner, LL corner
      annodf<-data.table(x=bbox[[1]][c(1,2,2,1)],y=bbox[[2]][c(2,2,1,1)],text=s(annotatecorners))
      annodf<-annodf[!(text=="." | text=="null"),]
      annocolors <- fg_get_aeslist("corner_anno")
      p<-p+geom_label(aes(x,y,label=text),data=annodf,vjust="inward",hjust="inward",alpha=0.8,fontface="bold",
                      color=annocolors[["color"]],fill=annocolors[["fill"]],
                      size=tsize*as.numeric(annocolors[["sizemult"]]))
    }

    # Deal with titles
    titleadds[axis=="x",note:=fcoalesce(note,grparts[item=="x",]$colnm)]
    titleadds[axis=="y",note:=fcoalesce(note,grparts[item=="y",]$colnm)]
    if(length(xlabdec<-s(xdecoration))==2) {
      titleadds[axis=="x",note:=paste0(xlabdec[1]," <-- ",note," --> ",xlabdec[2])]
    }
    if(length(ylabdec<-s(ydecoration))==2) {
      titleadds[axis=="y",note:=paste0(ylabdec[1]," <-- ",note," --> ",ylabdec[2])]
    }
    p <- p + class_labels(split(titleadds$note,titleadds$axis))
    # Guides
    guideset <- split( rep("none",5), s("color;alpha;size;symbol;fill"))
    legend_prefix <- ifelse(legendinside==TRUE,"inside","")
    symboldets <- as.list(grparts[item=="symbol",])
    if(symboldets[["addscales"]]) {
      shapeset <- as.numeric(fg_get_aesstring("scatshape"))
      legloc <- paste0(legend_prefix,fg_get_aesstring("legloc_symbol"))
      guideset[['symbol']]<-legd_guide(legloc,title=symboldets[['colnm']],ncats= as.numeric(symboldets[["ncnt"]]))
      p<-p+scale_shape_manual(values=shapeset,name=symboldets[["colnm"]],labels=levels(a3$symbolfactor))
      }
    colordets <- as.list(grparts[item=="color",])
    if(colordets[["addscales"]]) {
      ncolors <- as.numeric(colordets[["ncnt"]])
      tcolors <- fg_get_aesstring(grparts[item=="color",]$style,n_max=ncolors)
      legloc <- paste0(legend_prefix,fg_get_aesstring("legloc_color"))
      guideset[['color']]<- legd_guide(legloc,title=colordets[['colnm']],ncats=ncolors)
      p<-p+scale_color_manual(values=tcolors,name=colordets[["colnm"]],labels=levels(a3$colfactor))
    }
    filldets <- as.list(grparts[item=="fill",])
    if(filldets[["addscales"]]) {  # May be able to eliminate this with after_scale
      ncolors <- as.numeric(colordets[["ncnt"]])
      tcolors <- fg_get_aesstring(grparts[item=="color",]$style,n_max=ncolors) |> alpha(0.2)
      legloc <- paste0(legend_prefix,fg_get_aesstring("legloc_color"))
      p<-p+scale_fill_manual(values=tcolors,name=colordets[['colnm']],labels=levels(a3$colfactor),guide="none")
    }
    alphadets <- as.list(grparts[item=="alpha",])
    if(alphadets[["addscales"]]) {
      nalphas<- length(unique(a3$alphafac))
      legloc <- paste0(legend_prefix,fg_get_aesstring("legloc_alpha"))
      guideset[['alpha']]<- legd_guide(legloc,title=alphadets[['colnm']],ncats=nalphas)
      p<-p+scale_alpha_manual(values=seq(nalphas)/nalphas,name=alphadets[['colnm']],labels=levels(a3$alphafactor))
    }
    sizedets <- as.list(grparts[item=="size",])  # What if size is continuous?
    if(grparts[item=="size"]$addscales) {
      nsizes <- length(unique(a3$sizefac))
      legloc <- paste0(legend_prefix,fg_get_aesstring("legloc_size"))
      if(nsizes<=10) {
        szset <- fcoalesce(grparts[item=="size"]$style,"sizedefault")
        szmults <- fg_get_aesstring(szset,n_max=nsizes)
        if( any(is.na(szmults)) ) {
          szmults <- psize*seq(nsizes)/nsizes
        } else {
          szmults <- psize*as.numeric(szmults)
        }
        guideset[['size']]<-legd_guide(legloc,title=sizedets[['colnm']],ncats=nsizes)
        p<-p+scale_size_manual(values=szmults,name=sizedets[['colnm']])
        }
      else {
        guideset[['size']]<-legd_guide(legloc,title=sizedets[['colnm']],ncats=nsizes,guidetype="binned")
        p<-p+scale_size_binned(range=c(2,8),name=sizedets[['colnm']])
      }
    }
    p<-p + fgts_set_gridstyle(fg_current_theme(),gridstyle) + pextra # captions, etc.
    p<-p + guides(guideset)
    if(returnregresults) {
      return(list(p,regres))
    } else {
      return(p)
    }
}

scatform_to_df <- function(form) {
  cnames <- "item:colnm:style:order"
  fstr1 <- str_split_1(str_replace_all(form,",",":"),pattern="\\~|\\+")
  fstr1[c(1,2)] <- paste(c("y:","x:"),fstr1[c(1,2)])
  dfout <- read.table(textConnection(c(cnames,fstr1)),sep=":",fill=TRUE,strip.white=TRUE,colClasses="character",header=TRUE) |> as.data.table()
  dfout <- dfout[,lapply(.SD, \(x) fifelse(nchar(x)<=0,NA_character_,x)),.SDcols=is.character]
}

first_category_nm <- function(grparts,ifnone="") {
  tortn <- grparts[data.table(item=s("color;symbol;size;alpha")),on=.(item)][indta==TRUE]$nm[1]
  return(fcoalesce(tortn,ifnone))
}

make_datecutlabels=function(dta, dtsback=c(7,22),lastonly=FALSE,maxdate=NA_real_,labels=NULL) {
    if(lubridate::is.instant(dtsback[1])) {
        tortn <- cut(dta$dt, c(dtsback,Sys.Date()+20), labels=labels)
    }
    else {
        if(is.null(labels)) {
            labels <- paste0(c("0",dtsback),":",c(dtsback-1,"x"))
            }
        maxdate <- fcoalesce(as.numeric(as.Date(maxdate)),max(as.numeric(dta$dt)))
        daysvec <- maxdate-as.numeric(dta$DT_ENTRY)
        tortn <- cut(daysvec,c(-1,dtsback,+Inf), labels=labels)
        if(lastonly) {
            tortn <- ifelse(tortn=="0:1",tortn,"")    }
    }
    paste0(as.character(dta$colfactor),",",as.character(tortn))
}

collect_aes <- function(grparts) {
  aes_sets <- grparts[!(ggaes=="x") & indta==TRUE]
  aes_vals <- lapply(aes_sets$nm, sym)
  names(aes_vals) <-aes_sets$ggaes
  return(eval(as.call(c(quote(aes),aes_vals))))
}

