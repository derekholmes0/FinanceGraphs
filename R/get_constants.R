# =======================================================================================================
#' Maintain Colors
#'
#' @name fg_get_aes
#' @rdname get_constants
#' @description
#' `fg_get_aes()` gets aethestic  `data.frame` for use in graphs.
#' `fg_get_aesstring()` takes a column from the `data.frame` retrieved by `fg_get_aes()`
#' `fg_print_aes_list()` prints names of aesthetics used internally in FinanceGraph functions.
#' `fg_display_colors()` Shows a plot with current colors.
#'
#' @param item (Default: "") A grep string for categories desired.
#' @param n_max Maximum number of rows or entries to return.  Required for `Rcolorbrewer` color aesthetics
#' @param asdataframe (Default: FALSE) Return dataframe of parameters regardless of type. (See details)
#' @param toget Column in the aes `data.frame` to paste together as a string.
#' @param rtnifnotfound Return `NA_character_` if aes not found
#' @param grepstr narrow list of internal aesthetics sets to functions from `grepstr`
#'
#' @returns `fg_get_aes()` returns `data.frame` of aesthetics, including sorting columns, help strings, and values,
#' `fg_get_aesstring()` returns a list with just the character values of the requested aesthetic.
#' `fg_print_aes_list()` returns a markdown ready character vector of aesthetic names used in each function
#' `fg_display_colors()` returns a [ggplot2::ggplot()] object with colors and associated names for an aesthetic name
#'
#' @seealso [fgts_dygraph()], [fg_update_aes()]
#' @examples
#' # Data set, String
#' head(fg_get_aes("lines"),3)
#' fg_get_aesstring("lines")
#' #  Gradient colors are stored in a `data.frame` as in a set of "Blue Greens"
#' fg_get_aes("espath_gp",asdataframe=TRUE)
#' # To get the actual colors, we need to know how many:
#' fg_get_aes("espath_gp",n_max=8)
#' fg_display_colors("lines")
#'
#' @import data.table
#' @import scales
#' @export
fg_get_aes <- function(item="",n_max=NA_integer_,asdataframe=FALSE) {
  if(item=="") { return(the$aesset) }
  message_if(the$verbose,"fg_get_aes(",item,ifelse(is.na(n_max),"",paste0(", n_max=",n_max)),")")
  rtn <- the$aesset[category==item,]
  if(nrow(rtn)<=0) {
    message(paste("fg_get_aes Cannot find (",item,") in aesthetics db"))
    stopifnot(sys.nframe()>0)
    return()
  }
  if(asdataframe==TRUE) {
    return(rtn)
  }
  if(rtn[[1,"type"]]=="colorrange") {
    stopifnot("fg_get_aes(brewer) needs n_max" = !is.na(n_max))
    brewer_dets <- s(rtn[[1,"value"]],sep=",")
    brewer_range <- as.numeric(rtn[[1,"const"]])
    # Need a better hack
    brewer_direction <- sign(brewer_range)
    cols <- scales::pal_brewer(brewer_dets[[1]],palette=brewer_dets[[2]],direction=-brewer_direction)(abs(brewer_range))
    if(brewer_direction>0) {
        colors <- pal_gradient_n(cols)(seq(0, 0.6, length.out = n_max)) }
    else {
        colors <- pal_gradient_n(cols)(seq(0.4, 1, length.out = n_max)) } # Dont want to go all the way to white
    rtn <- data.table(category=item,variable=paste0("C",1:n_max),type="color",value=colors,const=NA_character_)
  }
  else {
    if(!is.na(n_max)) {
      if(n_max>0) {rtn <- rtn[1:min(.N,n_max),] }
    }
  }
  return(rtn)
}

#' @import data.table
#' @rdname get_constants
#' @export
fg_get_aesstring <- function(item="",n_max=NA_integer_,toget="value",rtnifnotfound=FALSE) {
  if(rtnifnotfound==TRUE & nrow(the$aesset[category==item,])<=0) {
    return(NA_character_)
  }
  else {
    return( fg_get_aes(item,n_max=n_max)[[toget]] )
  }
}

# --- Helpers
#' @import ggplot2
#' @import data.table
#' @rdname get_constants
#' @export
fg_display_colors <- function(item="") {
  category=variable=color=x=y=ztext=i.DT_ENTRY=i.END_DT_ENTRY=NULL
  tcolors <- fg_get_aes(item, n_max=100)
  tcolors <- tcolors[,let(x=20-(.I %% 20), y=floor(.I/20)+1,ztext=paste0(category,",",variable,":",color))]
  g1 <- ggplot(tcolors,aes(x,y,fill=value,label=ztext))+geom_tile()+geom_label(fill="white",size=3)
  g1 <- g1 +coord_flip()+scale_fill_identity()+labs(title=paste0("Aesthetic Set '",item,"' colors used"))+theme_bw()
  return(g1)
}

#' @rdname get_constants
#' @import knitr
#' @export
fg_print_aes_list <- function(grepstr="") {
  used=NULL
  grepstr <- paste0(grepstr,"|all")
  rtn <- the$aesset[grepl(grepstr,used),]
  rtn <- rtn[,.(helpstr=.SD[1][["helpstr"]],default=.SD[1][["value"]],N=.N),by=.(used,category)]
  rtn <- rtn[order(used,category)][,used:=NULL]
  return(kable(rtn))
}

# Unexported helpers

fg_current_theme <- function() {
  return(the$curr_theme)
}

fg_get_aeslist <- function(item="",toget="value") {
  fgtmp <-fg_get_aes(item)
  return( setNames(fgtmp[[toget]],fgtmp$type) )
}

dump_the <- function() {
  obset = s("cachedir;doifn;aesfn;themefn;doi_dates;aesset;gpname;verbose;cassign")
  nullres <- lapply(obset, function(x) { print(paste(x,"-----------------------"));print(get(x,envir=the))})
}


