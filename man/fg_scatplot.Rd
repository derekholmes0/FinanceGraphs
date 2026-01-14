\name{fg_scatplot}
\alias{fg_scatplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A Capitalized Title (ideally limited to 65 characters)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fg_scatplot(type, dta, xnm = "", ynm = "", doi = "", datecuts = c(7, 66), datecutlabels = NULL, aesds = NULL, colorcol = "", labelcol = "", labelcolor = "", symbolcol = "", tooltipcol = "", circlesizecol = "", alphacol = "", colorcol2 = "", tsize = 3, psize = 1, ellipse = F, repel = F, glinex = NA_character_, gliney = NA_character_, glineidentify = FALSE, xlabeldecoration = "", title = "", caption = NULL, boundbox = "", boundboxtype = "none", jitter = c(0, 0), keepcols = "", gridstyle = "", legend = "inside", axislabels = "", colorset = NULL, method = "loess", tformula = formula("y~x"), sizetrans = "identity", returnregresults = FALSE, pointformoverride = NA_character_, labelhilight = "", labelhilightcolor = "yellow", outlinetext = "none", cornercolors = "", annotatecorners = "", showregline = "lm", savetitle = "", verbose = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{type}{
%%     ~~Describe \code{type} here~~
}
  \item{dta}{
%%     ~~Describe \code{dta} here~~
}
  \item{xnm}{
%%     ~~Describe \code{xnm} here~~
}
  \item{ynm}{
%%     ~~Describe \code{ynm} here~~
}
  \item{doi}{
%%     ~~Describe \code{doi} here~~
}
  \item{datecuts}{
%%     ~~Describe \code{datecuts} here~~
}
  \item{datecutlabels}{
%%     ~~Describe \code{datecutlabels} here~~
}
  \item{aesds}{
%%     ~~Describe \code{aesds} here~~
}
  \item{colorcol}{
%%     ~~Describe \code{colorcol} here~~
}
  \item{labelcol}{
%%     ~~Describe \code{labelcol} here~~
}
  \item{labelcolor}{
%%     ~~Describe \code{labelcolor} here~~
}
  \item{symbolcol}{
%%     ~~Describe \code{symbolcol} here~~
}
  \item{tooltipcol}{
%%     ~~Describe \code{tooltipcol} here~~
}
  \item{circlesizecol}{
%%     ~~Describe \code{circlesizecol} here~~
}
  \item{alphacol}{
%%     ~~Describe \code{alphacol} here~~
}
  \item{colorcol2}{
%%     ~~Describe \code{colorcol2} here~~
}
  \item{tsize}{
%%     ~~Describe \code{tsize} here~~
}
  \item{psize}{
%%     ~~Describe \code{psize} here~~
}
  \item{ellipse}{
%%     ~~Describe \code{ellipse} here~~
}
  \item{repel}{
%%     ~~Describe \code{repel} here~~
}
  \item{glinex}{
%%     ~~Describe \code{glinex} here~~
}
  \item{gliney}{
%%     ~~Describe \code{gliney} here~~
}
  \item{glineidentify}{
%%     ~~Describe \code{glineidentify} here~~
}
  \item{xlabeldecoration}{
%%     ~~Describe \code{xlabeldecoration} here~~
}
  \item{title}{
%%     ~~Describe \code{title} here~~
}
  \item{caption}{
%%     ~~Describe \code{caption} here~~
}
  \item{boundbox}{
%%     ~~Describe \code{boundbox} here~~
}
  \item{boundboxtype}{
%%     ~~Describe \code{boundboxtype} here~~
}
  \item{jitter}{
%%     ~~Describe \code{jitter} here~~
}
  \item{keepcols}{
%%     ~~Describe \code{keepcols} here~~
}
  \item{gridstyle}{
%%     ~~Describe \code{gridstyle} here~~
}
  \item{legend}{
%%     ~~Describe \code{legend} here~~
}
  \item{axislabels}{
%%     ~~Describe \code{axislabels} here~~
}
  \item{colorset}{
%%     ~~Describe \code{colorset} here~~
}
  \item{method}{
%%     ~~Describe \code{method} here~~
}
  \item{tformula}{
%%     ~~Describe \code{tformula} here~~
}
  \item{sizetrans}{
%%     ~~Describe \code{sizetrans} here~~
}
  \item{returnregresults}{
%%     ~~Describe \code{returnregresults} here~~
}
  \item{pointformoverride}{
%%     ~~Describe \code{pointformoverride} here~~
}
  \item{labelhilight}{
%%     ~~Describe \code{labelhilight} here~~
}
  \item{labelhilightcolor}{
%%     ~~Describe \code{labelhilightcolor} here~~
}
  \item{outlinetext}{
%%     ~~Describe \code{outlinetext} here~~
}
  \item{cornercolors}{
%%     ~~Describe \code{cornercolors} here~~
}
  \item{annotatecorners}{
%%     ~~Describe \code{annotatecorners} here~~
}
  \item{showregline}{
%%     ~~Describe \code{showregline} here~~
}
  \item{savetitle}{
%%     ~~Describe \code{savetitle} here~~
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
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
function (type, dta, xnm = "", ynm = "", doi = "", datecuts = c(7, 
    66), datecutlabels = NULL, aesds = NULL, colorcol = "", labelcol = "", 
    labelcolor = "", symbolcol = "", tooltipcol = "", circlesizecol = "", 
    alphacol = "", colorcol2 = "", tsize = 3, psize = 1, ellipse = F, 
    repel = F, glinex = NA_character_, gliney = NA_character_, 
    glineidentify = FALSE, xlabeldecoration = "", title = "", 
    caption = NULL, boundbox = "", boundboxtype = "none", jitter = c(0, 
        0), keepcols = "", gridstyle = "", legend = "inside", 
    axislabels = "", colorset = NULL, method = "loess", tformula = formula("y~x"), 
    sizetrans = "identity", returnregresults = FALSE, pointformoverride = NA_character_, 
    labelhilight = "", labelhilightcolor = "yellow", outlinetext = "none", 
    cornercolors = "", annotatecorners = "", showregline = "lm", 
    savetitle = "", verbose = F) 
{
    if (nrow(dta) <= 0) {
        return(ggplot())
    }
    require("splines")
    sizefac = alphafac = xcolfaclist = c(1)
    addscales = list(size = TRUE, color = TRUE, alpha = TRUE, 
        shape = FALSE)
    dolastanyway = FALSE
    a2 <- copy(as.data.table(dta))
    regres = data.table()
    dtcolno = purrr::detect_index(a2, is.Date)[[1]]
    colnamesnodate = colnames(a2)[setdiff(1:ncol(a2), dtcolno)]
    if (!(xnm \%in\% colnamesnodate)) {
        xnm = colnamesnodate[1]
    }
    if (!(ynm \%in\% colnamesnodate)) {
        ynm = colnamesnodate[2]
    }
    if (dtcolno > 0) {
        a2$dt = a2[, ..dtcolno]
    }
    else {
        if (is.na(lubridate::parse_date_time(rownames(a2)[1], 
            "\%Y-\%m-\%d", quiet = TRUE))) {
            a2$dt = rownames(a2)
        }
        else {
            dateinrownames = as.Date(rownames(a2))
            if (!is.Date(dateinrownames[[1]])) {
                dateinrownames = seq(1, nrow(a2))
            }
            a2$dt <- dateinrownames
        }
    }
    do_not_fill = grepl("nofill", type)
    pointform <- case_when(labelcol \%in\% colnames(a2) ~ "text", 
        tooltipcol \%in\% colnames(a2) ~ "tips", TRUE ~ "point")
    a2 <- a2[!is.na(a2$dt)]
    a2 = dtsetnm(a2, xnm, "xx", existordie = TRUE)
    a2 = dtsetnm(a2, ynm, "yy", existordie = TRUE)
    a2 = dtsetnm(a2, colorcol, "colfactor", nullcol = "1")
    a2 = dtsetnm(a2, symbolcol, "symbolfactor", nullcol = "1")
    a2 = dtsetnm(a2, labelcol, "labels", nullcol = "1")
    a2 = dtsetnm(a2, tooltipcol, "tooltips", nullcol = "1")
    a2 = dtsetnm(a2, labelcolor, "labelcolor", nullcol = "1")
    a2 = dtsetnm(a2, labelhilight, "labelhilight", nullcol = "")
    a2 = dtsetnm(a2, circlesizecol, "sizefactor", nullcol = "1")
    a2 = dtsetnm(a2, alphacol, "alphafactor", nullcol = "1")
    thismaxdt = max(a2$dt)
    if (is.data.frame(doi)) {
        a2 = left_join(a2, mutate(doi, dt = doi, doino = row_number()), 
            by = "dt")
        a2[1, c("colfactor", "colfactorval")] <- c("-", a2[1, 
            "dt"])
        a2$colfactor <- na.locf(a2$doidesc)
        a2$colfactorval <- na.locf(a2$doi)
    }
    if (nchar(doi) > 1) {
        datecutlabels = c()
        if (grepl("rec", doi)) {
            pointform <- "hex"
            datecuts = c(0, datecuts[1:2])
        }
        if (grepl("last", doi)) {
            datecuts = c(0)
        }
        if (grepl("text", doi)) {
            pointform <- "text"
        }
        if (grepl("^regm", doi)) {
            xdates = filter(doidates, grepl(paste0("^", s(doi)[[2]], 
                "$"), category))
            if (nrow(xdates) > 0) {
                xdates \%<>\% ungroup \%>\% top_n(2, DT_ENTRY) \%>\% 
                  mutate(daysback = as.numeric(thismaxdt) - as.numeric(DT_ENTRY))
                datecuts = xdates \%>\% pull(daysback)
                datecuts = c(0, sort(as.numeric(datecuts)))
                datecutlabels = c("Last", pull(xdates, eventid), 
                  "older")
                dolastanyway <- TRUE
                pointform <- "point"
            }
        }
        a2$colfactor = make_datecutlabels(a2, datecuts, lastonly = grepl("last", 
            doi), labels = datecutlabels)
    }
    a3 <- a2[, .(xx, yy, colfactor, labels, labelcolor, sizefactor, 
        alphafactor, symbolfactor, dt, tooltips, labelhilight)]
    a3 <- a3[, `:=`(colfactor = as.factor(colfactor), symbolfactor = as.factor(symbolfactor))]
    if (nchar(keepcols) > 0) {
        a3 = cbind(a3, select(dta, !!{
            intersect(keepcols, colnames(dta))
        }))
    }
    bbx <- range(a3$xx, na.rm = T) * c(0.99, 1.01)
    bby <- range(a3$yy, na.rm = T) * c(0.99, 1.01)
    if (length(boundbox) == 2) {
        if (grepl("value", boundboxtype)) {
            bby = boundbox
        }
        else {
            bbx <- quantile(a3[["xx"]], probs = c(boundbox[1], 
                1 - boundbox[1]), na.rm = T) * c(0.95, 1.05)
            bby <- quantile(a3[["yy"]], probs = c(boundbox[2], 
                1 - boundbox[2]), na.rm = T) * c(0.95, 1.05)
        }
    }
    if (length(boundbox) == 4) {
        if (grepl("value", boundboxtype)) {
            bbx = c(boundbox[1], boundbox[2])
            bby = c(boundbox[3], boundbox[4])
        }
        else {
            bbx <- quantile(a3[["xx"]], probs = c(boundbox[1], 
                boundbox[2]), na.rm = T) * c(0.95, 1.05)
            bby <- quantile(a3[["yy"]], probs = c(boundbox[3], 
                boundbox[4]), na.rm = T) * c(0.95, 1.05)
        }
    }
    if (length(boundbox) > 1 & verbose) {
        message("Bounding Boxes ", boundboxtype, " gives (x,y)=", 
            paste(round(c(bbx, bby), 4), collapse = " "))
    }
    bbx = sort(bbx)
    bby = sort(bby)
    a3[, `:=`(inbox, (between(xx, bbx[1], bbx[2]) & between(yy, 
        bby[1], bby[2])))]
    if (!grepl("graphidentify|none", boundboxtype, ignore.case = T)) {
        a3 <- a3[inbox == TRUE, ]
        if (verbose) {
            message("bbx, bby:", bbx, " : ", bby, " gives ", 
                nrow(a3), " obs out of ", nrow(a2), " orig")
        }
    }
    if (grepl("^(path)", type)) {
        pointform <- ""
        addscales$size <- FALSE
        tmpfunc <- function(x) {
            u1 = mutate(x, islast = (dt > (max(dt) - 6)))
            bind_rows(toseasonaldates(filter(u1, !islast), dtname = "dt", 
                toadd = "yrwk", rtn = "last"), filter(u1, islast) \%>\% 
                mutate(yrwk = 0, colorfac = "dly"))
        }
        a2a = group_by(a3, colfactor) \%>\% do({
            tmpfunc(.)
        }) \%>\% mutate(colorfac = case_when(row_number() < (n() - 
            3) ~ "-", row_number() == n() ~ "lastx", TRUE ~ "last3"))
        a2a$colorfac = as.factor(a2a$colorfac)
        a3$colfactor = make_datecutlabels(a2, c(0))
        a2a = adddoidates(ungroup(a2a) \%>\% mutate(DT_ENTRY = dt))
        colorset = s("blue;red")
        p <- ggplot(a2a[, `:=`(dtback, .I)], aes(x = xx, y = yy)) + 
            geom_path(aes(linewidth = dtback)) + scale_linewidth(range = c(0.5, 
            1.5))
        p <- p + geom_point(aes(x = xx, y = yy, color = colorfac), 
            data = subset(a2a, !(colorfac == "-")), size = 4)
    }
    else {
        p <- ggplot(a3, aes(x = xx, y = yy, show.legend = TRUE))
    }
    pointform = fcoalesce(pointformoverride, pointform)
    if (length(s(annotatecorners)) == 4) {
        xrange = bbx * 0.8
        yrange = bby * 0.8
        annodf = data.frame(x = c(xrange[1], xrange[2], xrange[2], 
            xrange[1]), y = c(yrange[2], yrange[2], yrange[1], 
            yrange[1]), text = s(annotatecorners))
        annodf \%<>\% filter(!(text == "." | text == "null"))
        p <- p + geom_label(aes(x, y, label = text), data = annodf, 
            vjust = "inward", hjust = "inward", color = "darkgreen", 
            fill = "yellow", size = tsize * 0.8, alpha = 0.8, 
            fontface = "bold", family = "sans")
    }
    tsizes = c(6, 3, 2, rep(1, 10))
    if (is.null(colorset)) {
        tcolors = c(s("black;red;magenta;dark green"), rev(hicPalette))
    }
    else {
        tcolors = colorset
    }
    xcolfaclist_1 = a3[, .N, by = .(colfactor)][, `:=`(usehex, 
        (N > 200))] \%>\% separate(colfactor, c("orig_cf", "dt_cf"), 
        sep = ",", fill = "left", remove = F) \%>\% as.data.table
    if (length(unique(xcolfaclist_1$orig_cf)) > 1) {
        xcolfaclist_agg = xcolfaclist_1[, .SD[1][, .(xx = N)], 
            by = .(orig_cf)][, `:=`(sizeno = tsizes[.N - .I + 
            1], colorno = tcolors[.N - .I + 1])][]
        xcolfaclist = xcolfaclist_agg[xcolfaclist_1, on = .(orig_cf)]
    }
    else {
        xcolfaclist_agg = xcolfaclist_1[, .SD[1][, .(xx = N)], 
            by = .(colfactor)][, `:=`(sizeno = tsizes[.N - .I + 
            1], colorno = tcolors[.I])][]
        xcolfaclist = xcolfaclist_agg[xcolfaclist_1, on = .(colfactor)]
    }
    a3 = xcolfaclist[, .(colfactor, colorno, sizeno, orig_cf)][a3, 
        on = .(colfactor)][]
    lastdta = a3[nchar(as.character(colfactor)) > 1, ][, .SD[.N], 
        by = .(colorno)]
    lmcolorcol = "colorno"
    if (pointform == "text") {
        if (!(labelcolor \%in\% colnames(dta))) {
            a3 = a3[, `:=`(labelcolor = colfactor)]
        }
        pjit = position_jitterdodge(jitter.width = jitter[1], 
            dodge.width = jitter[2])
        lmcolorcol = "labelcolor"
        if (outlinetext == "none" & repel == TRUE) {
            p <- p + geom_text_repel(aes(x = xx, y = yy, label = labels, 
                color = labelcolor), size = tsize, fontface = "bold", 
                max.overlaps = 60, data = a3)
        }
        if (outlinetext == "none" & repel == FALSE) {
            p <- p + geom_text(aes(x = xx, y = yy, label = labels, 
                color = labelcolor), size = tsize, fontface = "bold", 
                data = a3, check_overlap = !(repel == "nooverlap"), 
                position = pjit)
        }
        if (outlinetext == "box" & repel == TRUE) {
            p <- p + geom_label_repel(aes(x = xx, y = yy, label = labels, 
                color = labelcolor), size = tsize, fontface = "bold", 
                max.overlaps = 60, data = a3)
        }
        if (outlinetext == "box" & repel == FALSE) {
            p <- p + geom_label(aes(x = xx, y = yy, label = labels, 
                color = labelcolor), size = tsize, fontface = "bold", 
                data = a3, position = pjit)
        }
        if (outlinetext == "hilight" & repel == TRUE) {
            a3 = a3[, `:=`(runhilight = !(is.na(labelhilight) | 
                labelhilight == ""))]
            p <- p + geom_text_repel(aes(x = xx, y = yy, label = labels, 
                color = labelcolor), size = tsize, fontface = "bold", 
                max.overlaps = 60, data = a3[runhilight == FALSE])
            p <- p + geom_label_repel(aes(x = xx, y = yy, label = labels, 
                color = labelcolor), size = tsize, fontface = "bold", 
                max.overlaps = 60, fill = labelhilightcolor, 
                alpha = 0.6, data = a3[runhilight == TRUE])
        }
        if (outlinetext == "hilight" & repel == FALSE) {
            a3 = a3[, `:=`(runhilight = !(is.na(labelhilight) | 
                labelhilight == ""))]
            p <- p + geom_text(aes(x = xx, y = yy, label = labels, 
                color = labelcolor), size = tsize, fontface = "bold", 
                data = a3[runhilight == FALSE])
            p <- p + geom_label(aes(x = xx, y = yy, label = labels, 
                color = labelcolor), size = tsize, fontface = "bold", 
                fill = labelhilightcolor, alpha = 0.6, data = a3[runhilight == 
                  TRUE])
        }
    }
    else {
        if (grepl("join", type)) {
            addscales$shape = TRUE
            p <- p + geom_line(aes(x = xx, y = yy, color = colfactor, 
                linewidth = sizefactor, alpha = alphafactor, 
                shape = symbolfactor), show.legend = TRUE, data = a3)
            pointform <- ""
        }
        if (grepl("dens", type)) {
            if (do_not_fill) {
                p <- p + geom_density_2d(aes(x = xx, y = yy, 
                  color = colfactor), size = 1.2, data = a3)
            }
            else {
                p <- p + geom_density_2d_filled(aes(x = xx, y = yy, 
                  color = colfactor), size = 1.2, data = a3)
            }
            pointform <- ""
        }
        if (pointform == "point") {
            addscales$shape = TRUE
            addscales$color = FALSE
            p <- p + geom_point(aes(x = xx, y = yy, color = colorno, 
                size = sizefactor, alpha = alphafactor, shape = symbolfactor), 
                data = a3)
            cset = xcolfaclist$colorno
            names(cset) = xcolfaclist$colorno
            p <- p + scale_color_manual("XXXX", values = cset, 
                labels = xcolfaclist$colfactor[order(xcolfaclist$colorno)], 
                guide = "legend")
        }
        if (pointform == "tips") {
            p <- p + geom_point_interactive(aes(tooltip = tooltips, 
                data_id = tooltips), size = 2, color = "gray", 
                alpha = 0.01, show.legend = FALSE)
        }
        if (pointform == "hex") {
            fillcolors = c("lightblue", "lightgreen", "pink", 
                "orange")
            for (j in 1:nrow(xcolfaclist)) {
                tdata = a3[colfactor == xcolfaclist[j]$colfactor]
                if (xcolfaclist[j]$usehex) {
                  addscales$alpha <- FALSE
                  if (j == 1) {
                    p <- p + geom_hex(aes(x = xx, y = yy), data = tdata[, 
                      .(xx, yy)], alpha = 0.7) + scale_fill_gradient(low = "gray80", 
                      high = "gray20")
                  }
                  else {
                    p <- p + geom_hex(data = tdata[, .(xx, yy)], 
                      fill = fillcolors[[j - 1]], alpha = 0.3)
                  }
                }
                else {
                  addscales$shape = TRUE
                  p <- p + geom_point(aes(x = xx, y = yy, alpha = alphafactor, 
                    shape = symbolfactor), data = tdata, size = xcolfaclist[[j, 
                    "sizeno"]], color = xcolfaclist[[j, "colorno"]])
                }
            }
        }
    }
    if (grepl("(lm|loess)", type)) {
        if (returnregresults) {
            if (grepl("one", type)) {
                regres = a3 \%>\% do({
                  lma = lm(tformula, data = rename(., y = yy, 
                    x = xx))
                  broom::tidy(lma)
                }) \%>\% mutate(p.value = round(p.value, 5))
            }
            else {
                regres = group_by(a3, colfactor) \%>\% do({
                  lma = lm(tformula, data = rename(., y = yy, 
                    x = xx))
                  broom::tidy(lma)
                }) \%>\% mutate(p.value = round(p.value, 5))
            }
        }
        actmethod = fcoalesce(stringi::stri_extract(regex = "(lm|loess)", 
            type), "lm")
        talpha = ifelse(do_not_fill, 0, 0.2)
        if (verbose & !(tformula == formula("y~x"))) {
            print(summary(lm(tformula, data = a3[, .(x = xx, 
                y = yy)])))
        }
        if (grepl("one", type)) {
            addscales$alpha <- FALSE
            p <- p + geom_smooth(method = actmethod, formula = tformula, 
                colour = "black", alpha = talpha, show.legend = FALSE, 
                data = a3)
        }
        else {
            p <- p + geom_smooth(method = actmethod, formula = tformula, 
                aes(colour = .data[[lmcolorcol]]), alpha = talpha, 
                show.legend = FALSE, data = a3)
        }
        if (actmethod == showregline & !grepl("noeq", type)) {
            legx <- data.frame(xx = quantile(a3[["xx"]], probs = c(0.2), 
                na.rm = T), yy = quantile(a3[["yy"]], probs = c(0.99), 
                na.rm = T), labels = lm_eqn(a3, "xx", "yy", rtnstyle = "simplewitht"))
            p <- p + geom_text(aes(x = xx, y = yy, label = labels, 
                hjust = 0), data = legx, size = 2, show.legend = FALSE)
        }
    }
    if (FALSE) {
        paftsum = p
        cAssign("paftsum;doi;addscales")
    }
    if (ellipse) {
        p <- p + stat_ellipse(aes(color = colfactor))
    }
    if (grepl("last", doi) | dolastanyway) {
        p <- p + geom_point(aes(x = xx, y = yy, color = .data[[lmcolorcol]]), 
            data = lastdta, size = 5)
        p <- p + geom_label_repel(aes(x = xx, y = yy, label = orig_cf, 
            color = .data[[lmcolorcol]]), size = tsize, max.overlaps = 40, 
            data = lastdta)
        legend = paste0(legend, "nocolor")
    }
    if (addscales$size) {
        nsizes = length(unique(a3$sizefac))
        if (nsizes <= 10) {
            p <- p + scale_size_manual(values = psize * seq(nsizes)/nsizes, 
                name = "ms2_size")
        }
        else {
            p <- p + scale_size_continuous(range = c(2, 8), trans = sizetrans)
        }
    }
    if (addscales$alpha) {
        nalphas = length(unique(a3$alphafac))
        p <- p + scale_alpha_manual(values = seq(nalphas)/nalphas, 
            name = "ms2_alp")
    }
    if (grepl("graphidentify", boundboxtype, ignore.case = T)) {
        a3b = filter(a3, is.na(inbox) | !inbox) \%>\% rowwise \%>\% 
            mutate(xx = pmax(pmin(xx, bbx[[2]]), bbx[[1]]), yy = pmax(pmin(yy, 
                bby[[2]]), bby[[1]]))
        if (pointform == "text") {
            pjit = position_jitterdodge(jitter.width = jitter[1], 
                dodge.width = jitter[2])
            caption = paste(caption, ",Beyond borders in italics")
        }
        else {
            p <- p + geom_point(aes(x = xx, y = yy), data = a3b, 
                size = 3)
        }
        p <- p + coord_cartesian(xlim = round(bbx, 0), ylim = round(bby, 
            0))
    }
    else if (grepl("graph", boundboxtype, ignore.case = T)) {
        p <- p + coord_cartesian(xlim = round(bbx, 0), ylim = round(bby, 
            0))
    }
    if (length(s(axislabels)) == 2) {
        xnm <- axislabels[1]
        ynm <- axislabels[2]
    }
    if (length(s(xlabeldecoration)) == 2) {
        xnm <- paste0(s(xlabeldecoration)[1], " <-- ", xnm, " --> ", 
            s(xlabeldecoration)[2])
    }
    if (is.numeric(glinex)) {
        p <- p + gline_x(int = glinex)
    }
    if (is.numeric(gliney)) {
        p <- p + gline_y(int = gliney)
    }
    if (!is.na(glinex == "last" || gliney == "last")) {
        for (irow in 1:nrow(lastdta)) {
            if (glinex == "last") {
                p <- p + gline_y(int = lastdta[[irow, "xx"]])
            }
            if (gliney == "last") {
                p <- p + gline_x(int = lastdta[[irow, "yy"]])
            }
        }
    }
    if (is.numeric(glineidentify)) {
        p <- p + gline_identify(p, glineidentify[1], glineidentify[2])
    }
    if (glineidentify == TRUE) {
        if (nchar(doi) > 0) {
            lastdta = a3[nchar(as.character(colfactor)) > 2, 
                ][, .SD[.N], by = .(colorno)]
        }
        else {
            lastdta = a3[, .SD[.N], by = .(colorno)]
        }
        for (irow in 1:nrow(lastdta)) {
            p <- p + gline_identify(p, lastdta[[irow, "xx"]], 
                lastdta[[irow, "yy"]], color = lastdta[[irow, 
                  "colorno"]])
        }
    }
    title = c(title, "")
    p <- p + labs(title = title[[1]], subtitle = title[[2]], 
        caption = caption, x = xnm, y = ynm)
    if (addscales$shape) {
        p <- p + scale_shape_manual(values = c(16, 1, 17, 2, 
            15, 0, 18, 5), name = "ms2_shape", labels = levels(a3$symbolfactor))
    }
    if (addscales$color) {
        p <- p + scale_color_manual(values = tcolors, name = "ms2_color", 
            labels = levels(a3$colfactor))
    }
    p <- p + BaseTheme(gridstyle = gridstyle, legend = legend)
    p <- p + guides(alpha = "none", fill = ifelse((grepl("nofill", 
        legend) | length(unique(a3$colfactor)) == 1), "none", 
        "legend"), colour = ifelse((grepl("nocolor", legend) | 
        length(unique(a3$colfactor)) == 1), "none", "legend"), 
        size = ifelse((grepl("nosize", legend) | length(unique(a3$sizefactor)) == 
            1), "none", "legend"), shape = ifelse((grepl("noshape", 
            legend) | length(unique(a3$symbolfactor)) == 1), 
            "none", "legend"))
    if (returnregresults) {
        return(list(p, regres))
    }
    else {
        psave(savetitle, p)
    }
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
