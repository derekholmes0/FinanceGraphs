#' @import data.table
#' @importFrom purrr map_dbl
# STraight from Excventding ggplot2 vignette, with winsorization

StatChull <- ggplot2::ggproto("StatChull", Stat,
                     setup_params = function(data,params) {
                       params$percentile = params$percentile %||% 0.0
                       return(params)
                     },
                     compute_group = function(data, scales, percentile=0) {
                       centroid = c(mean(data$x),mean(data$y))
                       distm = purrr::map_dbl(seq(1,nrow(data)),\(i) dist(rbind(c(data[i,]$x,data[i,]$y),centroid))[[1]])
                       data = data[which(distm<=quantile(distm,1-percentile)),]
                       # message("Using percentile: ",percentile, " drops ",length(distm)-nrow(data))
                       data[chull(data$x, data$y), , drop = FALSE]
                     },
                     required_aes = c("x", "y")
)

stat_chull <- function(mapping = NULL, data = NULL, percentile=0, geom = "polygon",
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatChull, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,
                  percentile=percentile,
                  ...)
  )
}

# STraight from Excventding ggplot2 vignette
# ggplot(mpg, aes(displ, hwy)) + geom_point() + stat_chull(fill = NA, colour = "black",)

# https://ggplot2-book.org/extensions.html
# Id like to make a new Geom, but that will have to be in a later version.
# Book not clear on really combining two geomms; I'll have to take a loot at geom_smooth in Github

