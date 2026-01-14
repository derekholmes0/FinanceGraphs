\name{fgts_recentboxplot}
\alias{fgts_recentboxplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A Capitalized Title (ideally limited to 65 characters)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fgts_recentboxplot(dt, premeltcols = "", outliers = F, isordered = F, cutcol = "", breaks = c(7, 30, 90, 360), gridsize = 0, hlines = c(), doi = "last", title = NA, xlabel = "", ylabel = "", flip = T, ordercol = "", colorstr = "", facetcol = "", facetloc = "vert", fakefacetcol = "", colrename = "", ycoord = c(), coordpercentile = 0.03, trimpctile = 0, boxtype = "", tlabels = "", legend = "", normalize = "", addmeans = F, normdtoverride = c(NA, NA), dataonly = F, dropset = "", savetitle = "", colorsetoverride = NULL, ptsize = 3, labels = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dt}{
%%     ~~Describe \code{dt} here~~
}
  \item{premeltcols}{
%%     ~~Describe \code{premeltcols} here~~
}
  \item{outliers}{
%%     ~~Describe \code{outliers} here~~
}
  \item{isordered}{
%%     ~~Describe \code{isordered} here~~
}
  \item{cutcol}{
%%     ~~Describe \code{cutcol} here~~
}
  \item{breaks}{
%%     ~~Describe \code{breaks} here~~
}
  \item{gridsize}{
%%     ~~Describe \code{gridsize} here~~
}
  \item{hlines}{
%%     ~~Describe \code{hlines} here~~
}
  \item{doi}{
%%     ~~Describe \code{doi} here~~
}
  \item{title}{
%%     ~~Describe \code{title} here~~
}
  \item{xlabel}{
%%     ~~Describe \code{xlabel} here~~
}
  \item{ylabel}{
%%     ~~Describe \code{ylabel} here~~
}
  \item{flip}{
%%     ~~Describe \code{flip} here~~
}
  \item{ordercol}{
%%     ~~Describe \code{ordercol} here~~
}
  \item{colorstr}{
%%     ~~Describe \code{colorstr} here~~
}
  \item{facetcol}{
%%     ~~Describe \code{facetcol} here~~
}
  \item{facetloc}{
%%     ~~Describe \code{facetloc} here~~
}
  \item{fakefacetcol}{
%%     ~~Describe \code{fakefacetcol} here~~
}
  \item{colrename}{
%%     ~~Describe \code{colrename} here~~
}
  \item{ycoord}{
%%     ~~Describe \code{ycoord} here~~
}
  \item{coordpercentile}{
%%     ~~Describe \code{coordpercentile} here~~
}
  \item{trimpctile}{
%%     ~~Describe \code{trimpctile} here~~
}
  \item{boxtype}{
%%     ~~Describe \code{boxtype} here~~
}
  \item{tlabels}{
%%     ~~Describe \code{tlabels} here~~
}
  \item{legend}{
%%     ~~Describe \code{legend} here~~
}
  \item{normalize}{
%%     ~~Describe \code{normalize} here~~
}
  \item{addmeans}{
%%     ~~Describe \code{addmeans} here~~
}
  \item{normdtoverride}{
%%     ~~Describe \code{normdtoverride} here~~
}
  \item{dataonly}{
%%     ~~Describe \code{dataonly} here~~
}
  \item{dropset}{
%%     ~~Describe \code{dropset} here~~
}
  \item{savetitle}{
%%     ~~Describe \code{savetitle} here~~
}
  \item{colorsetoverride}{
%%     ~~Describe \code{colorsetoverride} here~~
}
  \item{ptsize}{
%%     ~~Describe \code{ptsize} here~~
}
  \item{labels}{
%%     ~~Describe \code{labels} here~~
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
function (dt, premeltcols = "", outliers = F, isordered = F, 
    cutcol = "", breaks = c(7, 30, 90, 360), gridsize = 0, hlines = c(), 
    doi = "last", title = NA, xlabel = "", ylabel = "", flip = T, 
    ordercol = "", colorstr = "", facetcol = "", facetloc = "vert", 
    fakefacetcol = "", colrename = "", ycoord = c(), coordpercentile = 0.03, 
    trimpctile = 0, boxtype = "", tlabels = "", legend = "", 
    normalize = "", addmeans = F, normdtoverride = c(NA, NA), 
    dataonly = F, dropset = "", savetitle = "", colorsetoverride = NULL, 
    ptsize = 3, labels = NULL) 
{
    if (doi == "") {
        doi = "last"
    }
    if (!is.data.table(dt)) {
        dt = data.table(dt)
    }
    if (nrow(dt) <= 0) {
        message("mtsrecentboxplot NO DATA ")
        return()
    }
    tit <- titadd <- ifelse(is.character(title), title, "")
    if (is.character(breaks) && breaks \%in\% unique(doidates$category)) {
        breaks <- inv.doi(breaks, exact = TRUE, maxdates = 1) \%>\% 
            pull(daysfromlast)
    }
    origbreaks = breaks
    if (length(s(colrename)) == 2) {
        colnames(dt) <- gsub(colrename[1], colrename[2], colnames(dt), 
            perl = T)
    }
    ismelted = all(c("variable", "value") \%in\% colnames(dt))
    if (length(premeltcols) == 2 & !ismelted) {
        dt = dtsetnm(dt, premeltcols[[1]], "variable", nullcol = 1)
        dt = dtsetnm(dt, premeltcols[[2]], "value", nullcol = 1)
        ismelted = TRUE
    }
    dt = dtsetnm(dt, ordercol, "orderby", nullcol = 1)
    dt = dtsetnm(dt, cutcol, "daysback", nullcol = 1)
    dt = dtsetnm(dt, facetcol, "facetcol", nullcol = 1)
    if (fakefacetcol \%in\% colnames(dt)) {
        dt = dtsetnm(dt, fakefacetcol, "fakecol", nullcol = 1)
        dt = dt[, `:=`(variable, paste(fakecol, variable))][, 
            `:=`(fakecol = NULL)]
    }
    if (ismelted) {
        dtm = dt
    }
    else {
        dtm <- data.table::melt(dt, id.vars = c("DT_ENTRY"), 
            value.name = "value")[is.finite(value), ]
    }
    if (cutcol \%in\% colnames(dt)) {
        if (all(range(breaks) == c(0, 1))) {
            qnames = apply(embed(breaks, 2), 1, function(x) {
                paste(x, collapse = ":")
            })
            qf <- function(x) {
                message("Applying quantiles to ", unique(as.character(x$variable)))
                db1 = cut(x$daysback, quantile(x$daysback, probs = breaks, 
                  na.rm = T) + seq(1, length(breaks))/10^7)
                levels(db1) = qnames
                data.frame(dq = db1)
            }
            dtmd = group_by(dtm, variable) \%>\% do({
                qf(.)
            })
            dtm = rbind(subset(dtm, select = -c(daysback)), dtmd[, 
                "daysback"])
        }
        else {
            breaks = generalbreaks(breaks, dtm)
            dtm$daysback <- cut(as.numeric(dtm$daysback), c(-1, 
                breaks, +Inf))
        }
    }
    else {
        breaks = generalbreaks(breaks, dtm)
        if (length(labels) > 0) {
            labels = c(labels, paste0("old", 1:10))[1:(length(unique(breaks)) + 
                1)]
        }
        dtm$daysback <- cut(as.numeric(max(dtm$DT_ENTRY, na.rm = T) - 
            dtm$DT_ENTRY), c(-1, unique(breaks), +Inf), labels = labels)
    }
    dtm = dtm[!is.na(daysback) & is.finite(value)]
    if (grepl("bydoi|byvar|zbyvar", normalize)) {
        dtm = dtm[, `:=`(vmin = value, vmax = value)]
        if (!is.na(as.Date(normdtoverride[1]))) {
            dtssmin = as.Date(normdtoverride[1])
            titadd = paste(titadd, "MinSprDate=", dtssmin)
            dtm[!which(dtm$DT_ENTRY == dtssmin), "vmin"] = NA
        }
        if (!is.na(as.Date(normdtoverride[2]))) {
            dtssmax = as.Date(normdtoverride[2])
            titadd = paste(titadd, "MaxSprDate=", dtssmax)
            dtm[!which(dtm$DT_ENTRY == dtssmax), "vmax"] = NA
        }
        dtmb = dtm[, `:=`(vminalldta = min(vmin, na.rm = T), 
            vmaxalldta = max(vmax, na.rm = T)), by = .(variable)]
        dtm = dtmb[, `:=`(pctbydoi = 100 * (value - min(vmin, 
            na.rm = T))/(max(vmax, na.rm = T) - min(vmin, na.rm = T)), 
            pctbyvar = 100 * (value - vminalldta)/(vmaxalldta - 
                vminalldta), pctzbyvar = (value - mean(value, 
                na.rm = T))/sd(value, na.rm = T)), by = .(variable, 
            daysback)]
        dtm = dtm[, `:=`(value, NULL)]
        dtm = dtm[, `:=`(c("value"), .SD), .SDcols = paste0("pct", 
            normalize)][!is.nan(value)]
        dtm = dtm[!is.nan(value)]
        nfacs = length(unique(dtm$daysback))
        colorset = c("red", dhgreyInvertedPalette[1:max(1, nfacs - 
            2)], "lightgreen")
        if (is.vector(colorsetoverride)) {
            colorset = colorsetoverride
        }
        coordpercentile = 0
    }
    else {
        hicPal2 = c("grey30", "red", "green4", "blue", "red4", 
            "magenta", "grey30", "cyan3", "purple2", "darkorange2", 
            "seagreen4", "grey51", "dark green", "pink", "orange")
        colorset = c(rev(gray.colors(max(1, length(unique(dtm$daysback)) - 
            1), start = 0.4, end = 0.8)), "darkorchid1", hicPal2)
    }
    if (nchar(colorstr) > 0) {
        colorset = s(colorstr)
    }
    if (dataonly) {
        return(dtm)
    }
    nobsback = c(s(doi, sep = ":"), 1, 1)[[2]]
    nobsback = fcoalesce(as.numeric(nobsback), 1)
    dtoi = dtm[, .SD[c(.N - nobsback, .N)][, `:=`(rno, .I)], 
        by = .(variable)]
    if (grepl("-", nobsback)) {
        dtoi = dtm[between(DT_ENTRY, as.Date(nobsback), Sys.Date()), 
            ][, .SD[c(1, .N)][, `:=`(rno, .I)], by = .(variable)]
        if (grepl("^last", doi)) {
            titadd = paste0("aDot: Last, Arrow:", as.Date(nobsback))
        }
        else {
            dtoi = dtoi[rno == 1, ]
            titadd = paste0("bDot: ", as.Date(nobsback))
        }
    }
    else if (grepl("^last", doi)) {
        nobsback = fcoalesce(as.numeric(nobsback), 1)
        if (doi == "last") {
            dtoi = dtoi[rno == 2, ]
            titadd = paste0("cDot: Last")
        }
        else {
            titadd = paste0("dDot: Last, Arrow: ", nobsback, 
                " pds back")
        }
    }
    if (is.numeric(dtoi$orderby) && (sum(dtoi$value) - sum(dtoi$orderby)) < 
        0.001 & !is.na(ordercol) & nchar(ordercol) > 1) {
        dtmerge = dtoi[, .SD[1], by = .(variable)][, .(variable, 
            orderby)]
        dtm = dtmerge[dtm[, `:=`(orderby, NULL)], on = .(variable)]
        isordered = T
    }
    else if (grepl("by", ordercol)) {
        dtmerge = dtoi[, .SD[.N], by = .(variable)][, .(variable, 
            value)]
        dtmerge[order(dtmerge$value), "orderby"] <- 1:nrow(dtmerge)
        dtm = dtmerge[dtm[, `:=`(orderby, NULL)], on = .(variable)]
        isordered = T
    }
    droplevels(dtm)
    if (length(s(tlabels)) != (length(levels(dtm$daysback)) - 
        1)) {
        tlabels = as.character(levels(dtm$daysback))
    }
    else {
        tlabels = c(s(tlabels), "-Inf-")
    }
    if (length(dropset) > 0) {
        if (!is.na(levels(dtm$daysback)[dropset])) {
            message("Dropping level ", levels(dtm$daysback)[dropset])
        }
        dtm = subset(dtm, !(daysback \%in\% levels(dtm$daysback)[dropset]))
    }
    dtm = suppressWarnings(droplevels(dtm))
    if (grepl("vioandline", boxtype)) {
        labrank = data.table(daysback = tlabels, nrow = seq(1, 
            length(tlabels)))[, `:=`(keepit, nrow < .N)]
        dtalastregime = dtm[, .(mins = min(value), maxs = max(value), 
            value = last(value)), by = .(daysback, variable)]
        dtalastregime = labrank[dtalastregime, on = .(daysback)][, 
            `:=`(value = ifelse(nrow > 1, NA_real_, value))][(keepit)]
        if (trimpctile > 0) {
            dtaq = dtm[, .(qlo = quantile(value, trimpctile, 
                na.rm = T), qhi = quantile(value, 1 - trimpctile, 
                na.rm = T)), by = .(variable)]
            dtm = dtaq[dtm, on = .(variable)][between(value, 
                qlo, qhi)]
        }
        g1 = ggplot(dtm, aes(x = variable, y = value)) + geom_violin(trim = TRUE)
        g1 = g1 + geom_linerange(aes(ymin = mins, ymax = maxs, 
            color = daysback), data = dtalastregime, position = position_jitterdodge(jitter.width = 0.1, 
            dodge.width = 0.1), linewidth = 0.7)
        g1 = g1 + geom_point(aes(y = value), data = dtalastregime[nrow == 
            1], color = "red", size = 2)
    }
    else {
        g1 = ggplot(dtm, aes(x = variable, y = value, fill = daysback))
        if (grepl("nowhisker", boxtype)) {
            ptltmp <- ifelse(boxtype == "nowhisker20", 0.2, ifelse(boxtype == 
                "nowhisker10", 0.1, 0.25))
            dtmx = dtm[, .(ymin = quantile(value, ptltmp, na.rm = T), 
                middle = quantile(value, 0.5, na.rm = T), ymax = quantile(value, 
                  (1 - ptltmp), na.rm = T)), by = .(variable, 
                daysback)][, `:=`(lower = ymin, upper = ymax, 
                value = middle)]
            g1 = g1 + geom_boxplot(outlier.shape = ifelse(outliers, 
                16, NA), aes(ymin = ymin, lower = lower, middle = middle, 
                upper = upper, ymax = ymax, fill = daysback), 
                data = dtmx, stat = "identity", alpha = 0.6)
        }
        else {
            g1 = g1 + geom_boxplot(outlier.shape = ifelse(outliers, 
                16, NA), aes(fill = daysback), alpha = 0.6)
        }
        g1 = g1 + scale_fill_manual(values = colorset, labels = tlabels)
        if (!(doi == "nolast")) {
            if (max(dtoi$rno) - min(dtoi$rno) > 0) {
                ptscast = dcast(dtoi[, .(variable, daysback, 
                  value, rno)][, `:=`(rno, paste0("R", rno))], 
                  variable + daysback ~ rno)
                g1 = g1 + geom_segment(aes(x = variable, xend = variable, 
                  y = R2, yend = R1), data = ptscast, color = I("blue"), 
                  linewidth = 1) + geom_point(aes(x = variable, 
                  y = R1), color = I("blue"), size = ptsize, 
                  data = ptscast)
            }
            else {
                g1 = g1 + geom_point(aes(x = variable, y = value), 
                  color = I("blue"), size = ptsize, data = dtoi)
            }
        }
    }
    if (ordercol == "reverse") {
        g1 = g1 + scale_x_discrete(limits = rev(levels(dtm$variable)))
    }
    if (isordered) {
        dtm$v2 <- reorder(dtm$variable, as.numeric(dtm$orderby))
        g1 = g1 + scale_x_discrete(limits = levels(dtm$v2))
        dtm$variable <- as.character(dtm$v2)
    }
    ycoordtouse = ycoord
    if (length(ycoord) != 2) {
        ycoordtouse = quantile(dtm$value, c(coordpercentile, 
            1 - coordpercentile), na.rm = T)
    }
    if (gridsize > 0) {
        trange = gridsize * (floor(range(dtm$value, na.rm = T)/gridsize) + 
            c(0, 1))
        if (length(ycoord) == 2) {
            g1 = g1 + scale_y_continuous(minor_breaks = seq(trange[[1]], 
                trange[[2]], gridsize), limits = ycoord)
        }
        else {
            scale_y_continuous(minor_breaks = seq(trange[[1]], 
                trange[[2]], gridsize))
        }
        g1 = g1 + BaseTheme(base_size = 7) + theme(panel.grid.minor = element_line(color = "grey", 
            linetype = "dotted", linewidth = 0.5))
    }
    for (ln in hlines) {
        u = g1 <- g1 + geom_hline(yintercept = ln, color = "blue")
    }
    if (flip) {
        g1 = g1 + coord_flip(ylim = ycoordtouse)
    }
    if (legend == "combine") {
        titadd = paste(titadd, "\nRg", paste0(origbreaks, collapse = ","))
        legend = "none"
    }
    g1 = g1 + labs(title = tit, caption = titadd, x = xlabel, 
        y = ylabel) + BaseTheme(base_size = 7, legend = legend)
    if ("facetcol" \%in\% colnames(dtm)) {
        if (facetloc == "vert") {
            g1 = g1 + facet_grid(facetcol ~ ., drop = TRUE, scales = "free", 
                space = "free")
        }
        else {
            g1 = g1 + facet_grid(. ~ facetcol, drop = TRUE, scales = "free", 
                space = "free")
        }
        g1 = g1 + theme(strip.text.y = element_text(size = 11, 
            color = "black", face = "bold"), strip.background = element_rect(fill = "wheat"))
    }
    if (addmeans == "mean") {
        dmn = group_by(dtm, daysback) \%>\% summarise(vmn = mean(value, 
            na.rm = T), vsd = sd(value, na.rm = T))
        g1 = g1 + geom_hline(aes(yintercept = vmn, color = daysback, 
            linetype = daysback), data = dmn[c(1, nrow(dmn)), 
            ], linewidth = 1)
        g1 = g1 + scale_colour_manual(values = c("red", "black", 
            "green", "blue")) + scale_linetype_manual(values = c(3, 
            5))
    }
    if (addmeans == "smooth") {
        dmn = group_by(dtm, variable) \%>\% slice_max(n = 1, -daysback)
        g1 = g1 + geom_smooth(aes(x = variable, y = value), formula = "y~x", 
            data = dmn) + scale_colour_manual(values = c("red", 
            "black", "green", "blue")) + scale_linetype_manual(values = c(3, 
            5))
    }
    if (exists("curvestohighlight") & "CREDIT" \%in\% colnames(dt)) {
        credits_in_dt = ungroup(dt) \%>\% mutate(oCREDIT = CREDIT) \%>\% 
            group_by(CREDIT, oCREDIT) \%>\% tally()
        datamap = left_join(tibble(CREDIT = unique(as.character(dtm$variable))), 
            credits_in_dt, by = "CREDIT") \%>\% left_join(tibble(oCREDIT = curvestohighlight) \%>\% 
            mutate(color = "red"), by = "oCREDIT") \%>\% mutate(color = fcoalesce(color, 
            "black"))
        g1 = g1 + theme(axis.text.y = element_text(color = datamap$color))
    }
    psave(savetitle, g1)
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
