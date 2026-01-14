\name{fgts_seasonal}
\alias{fgts_seasonal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A Capitalized Title (ideally limited to 65 characters)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fgts_seasonal(xindta, yvar, title = "", seasonaltype = "roll", normalize = "", graphtype = "line", projectfwd = F, captionlab = "", periodset = NULL, yrange = NULL, colorscaletype = "", dbg = FALSE, weekdaysonly = FALSE, line_on_lastdate = TRUE, killbad_eop = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xindta}{
%%     ~~Describe \code{xindta} here~~
}
  \item{yvar}{
%%     ~~Describe \code{yvar} here~~
}
  \item{title}{
%%     ~~Describe \code{title} here~~
}
  \item{seasonaltype}{
%%     ~~Describe \code{seasonaltype} here~~
}
  \item{normalize}{
%%     ~~Describe \code{normalize} here~~
}
  \item{graphtype}{
%%     ~~Describe \code{graphtype} here~~
}
  \item{projectfwd}{
%%     ~~Describe \code{projectfwd} here~~
}
  \item{captionlab}{
%%     ~~Describe \code{captionlab} here~~
}
  \item{periodset}{
%%     ~~Describe \code{periodset} here~~
}
  \item{yrange}{
%%     ~~Describe \code{yrange} here~~
}
  \item{colorscaletype}{
%%     ~~Describe \code{colorscaletype} here~~
}
  \item{dbg}{
%%     ~~Describe \code{dbg} here~~
}
  \item{weekdaysonly}{
%%     ~~Describe \code{weekdaysonly} here~~
}
  \item{line_on_lastdate}{
%%     ~~Describe \code{line_on_lastdate} here~~
}
  \item{killbad_eop}{
%%     ~~Describe \code{killbad_eop} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or standard data sets, see data().

## The function is currently defined as
function (xindta, yvar, title = "", seasonaltype = "roll", normalize = "", 
    graphtype = "line", projectfwd = F, captionlab = "", periodset = NULL, 
    yrange = NULL, colorscaletype = "", dbg = FALSE, weekdaysonly = FALSE, 
    line_on_lastdate = TRUE, killbad_eop = FALSE) 
{
    indta = data.table(xindta)
    if (!("rollpd" \%in\% colnames(indta))) {
        if (seasonaltype == "roll") {
            indta = dtmap[, .(DT_ENTRY, rolldt, rollpd, daysfromroll, 
                yrwk)][indta, on = "DT_ENTRY"]
        }
        else {
            varlist = c("DT_ENTRY", "yrwk", "isholiday", seasonaltype)
            xdtmap = dtmap[, ..varlist]
            if (weekdaysonly) {
                xdtmap = xdtmap[isholiday == FALSE, ]
            }
            setnames(xdtmap, seasonaltype, "rollpd")
            dttmp = xdtmap[, `:=`(rollpd, as.character(rollpd))][, 
                `:=`(daysfromroll = .I - .I[which.min(DT_ENTRY)], 
                  rolldt = min(DT_ENTRY)), by = .(rollpd)]
            indta = dttmp[indta, on = "DT_ENTRY", nomatch = NULL]
        }
    }
    if (length(unique(indta$rollpd)) <= 1) {
        message("PlotOneSeasonal not enough data")
        return(NULL)
    }
    lastindex = indta[rollpd == max(rollpd), .SD[1]][[yvar]]
    if (normalize == "relative") {
        indta = indta[, `:=`(zz, (get(yvar) - first(get(yvar)))), 
            by = .(rollpd)]
        indta[, `:=`((yvar), zz)]
        colorscaletype = "manual"
        captionlab = "Older cycles adjusted to match beginning of latest cycle"
    }
    if (normalize == "index") {
        indta = indta[, `:=`(zz, 100 * (get(yvar)/first(get(yvar)) - 
            1)), by = .(rollpd)]
        indta[, `:=`((yvar), zz)]
        colorscaletype = "manual"
        captionlab = "Expressed as index from beginning of each period"
    }
    if (is.data.frame(periodset)) {
        message("limiting to select periods")
        indta = inner_join(indta, periodset, by = "rollpd")
    }
    ulast = indta[, .SD[.N], by = .(rollpd)][, `:=`(rollsback = .N - 
        .I + 1)][, `:=`(islastpd, fifelse(rollsback == 1, "LAST", 
        "--"))]
    ulastdfr = ulast[, .SD[.N]][["daysfromroll"]]
    ulastval = ulast[, .SD[.N]][[yvar]]
    indta = ulast[, .(rollpd, rollsback, islastpd)][indta, on = .(rollpd)]
    if (killbad_eop) {
        baddays = indta[, .N, by = .(daysfromroll)][N < 0.3 * 
            mean(N), ]
        indta = indta[!baddays[, .(daysfromroll)], on = .(daysfromroll)]
    }
    lastpdreturn = filter(indta, islastpd == "LAST") \%>\% summarise(pdrtn = last(!!rlang::sym(yvar)) - 
        first(!!rlang::sym(yvar))) \%>\% pull(pdrtn)
    if (projectfwd) {
        wts = filter(indta, rollpd < max(rollpd)) \%>\% group_by(rollpd) \%>\% 
            summarise(maxdt = max(DT_ENTRY)) \%>\% ungroup \%>\% 
            arrange(rollpd) \%>\% mutate(ww = 0.9^((n() - 1):0))
        avgfwd = filter(indta, daysfromroll > ulastdfr[[1]]) \%>\% 
            inner_join(select(wts, rollpd, ww), by = "rollpd") \%>\% 
            group_by(rollpd) \%>\% mutate(rollchg = !!rlang::sym(yvar) - 
            first(!!rlang::sym(yvar))) \%>\% group_by(daysfromroll) \%>\% 
            summarise(mnrollchg = ulastval + weighted.mean(rollchg, 
                ww))
        avgfwd = cbind(ungroup(avgfwd), ulast[.N, .(islastpd, 
            rollsback, rollpd)])
    }
    if (grepl("stat|aggregate", graphtype)) {
        indta_tmp = copy(indta)[, .(DT_ENTRY, rollpd, daysfromroll, 
            rolldt, yrwk, value = get(yvar), islastpd, variable = yvar, 
            rollsback)]
        qtiles1 = indta_tmp[rollsback > 1, ][, as.list(quantile(value, 
            c(0.1, 0.5, 0.9), na.rm = T)), by = .(variable, daysfromroll)]
        qtiles2 = indta_tmp[rollsback > 1, ][, .(DT_ENTRY = min(DT_ENTRY), 
            rolldt = min(rolldt)), by = .(variable, daysfromroll)]
        qtiles = qtiles2[melt(qtiles1, id.vars = c("variable", 
            "daysfromroll"), variable.name = "rollpd"), on = .(variable, 
            daysfromroll)]
        ulast = indta[rollsback == 1, .SD[.N]]
        indta = rbindlist(list(indta_tmp[rollsback == 1, ], qtiles), 
            use.names = T, fill = T)[order(rollpd, daysfromroll)]
        setnames(indta, "value", yvar)
        graphtype = "line"
        captionlab = paste(captionlab, "10,50,90th percentiles shown")
    }
    cAssign("indta;yrange;ulast", dbg = dbg)
    title = paste0(title, " PdToDt:", format(lastpdreturn, digits = 3))
    if (length(yrange) == 2) {
        indta = indta[between(get(yvar), yrange[[1]], yrange[[2]]), 
            ]
    }
    indta = indta[, `:=`(islastpd = fcoalesce(islastpd, "-"))]
    if (graphtype == "line") {
        g1 = ggplot(indta, aes(x = daysfromroll, y = !!rlang::sym(yvar), 
            color = rollpd, linewidth = islastpd)) + geom_line(aes(group = rollpd)) + 
            scale_discrete_manual("linewidth", values = c(0.5, 
                2, 1, 1, 1))
        g1 = g1 + geom_label_repel(aes(x = daysfromroll, y = !!rlang::sym(yvar), 
            label = rollpd), data = ulast, size = 2.5, max.overlaps = 50) + 
            legendPosition("none") + labs(x = "Days from Beg of Period")
    }
    if (graphtype == "hex") {
        nbins = as.numeric(c(s(graphtype), 30)[[2]])
        g1 = ggplot(indta, aes(x = daysfromroll, y = !!rlang::sym(yvar))) + 
            geom_line(aes(group = rollpd), linewidth = 2, data = indta[rollsback == 
                1, ]) + geom_hex(data = indta[rollsback > 1, 
            ], alpha = 0.7, bins = nbins) + scale_fill_gradient(low = "gray80", 
            high = "gray20")
        g1 = g1 + geom_label_repel(aes(x = daysfromroll, y = !!rlang::sym(yvar), 
            label = rollpd), data = ulast[rollsback == 1], size = 2.5, 
            max.overlaps = 50) + legendPosition("none") + labs(x = "Days from Beg of Period")
    }
    if (colorscaletype == "viridis") {
        g1 = g1 + scale_color_viridis_d(direction = -1)
    }
    else {
        g1 = g1 + scale_color_manual(values = hicPalette)
    }
    if (line_on_lastdate) {
        g1 = g1 + gline_y(int = ulastdfr)
    }
    if (projectfwd) {
        g1 = g1 + geom_line(aes(x = daysfromroll, y = mnrollchg), 
            data = avgfwd, color = "red", linewidth = 1.5)
    }
    if (nchar(captionlab) > 0) {
        g1 = g1 + labs(caption = captionlab)
    }
    g1 = g1 + ggtitle(title) + BaseTheme(legend = "bottomleft")
    g1
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
