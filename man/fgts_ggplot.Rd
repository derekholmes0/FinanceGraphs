\name{fgts_ggplot}
\alias{fgts_ggplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A Capitalized Title (ideally limited to 65 characters)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fgts_ggplot(dta, title = "", ylabel = "", xlabel = "", cutdata = "", cutset = "", cutname = "", cutcolors = "", linetypename = "", ycoord = c(-Inf, +Inf), hlines = c(), panelvar = "", colorvar = "variable", printlastdata = "", boldline = "", cutsuffix = "", savetitle = "", lastoffset = 5, legend = "topleft", alreadymelted = FALSE, fcstpds = 0, cornerannotations = NULL, colorset = c(s("black;red;green;blue;magenta;grey30;purple2;turquoise;pink;orange;dark green;light blue"), cbbPalette))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dta}{
%%     ~~Describe \code{dta} here~~
}
  \item{title}{
%%     ~~Describe \code{title} here~~
}
  \item{ylabel}{
%%     ~~Describe \code{ylabel} here~~
}
  \item{xlabel}{
%%     ~~Describe \code{xlabel} here~~
}
  \item{cutdata}{
%%     ~~Describe \code{cutdata} here~~
}
  \item{cutset}{
%%     ~~Describe \code{cutset} here~~
}
  \item{cutname}{
%%     ~~Describe \code{cutname} here~~
}
  \item{cutcolors}{
%%     ~~Describe \code{cutcolors} here~~
}
  \item{linetypename}{
%%     ~~Describe \code{linetypename} here~~
}
  \item{ycoord}{
%%     ~~Describe \code{ycoord} here~~
}
  \item{hlines}{
%%     ~~Describe \code{hlines} here~~
}
  \item{panelvar}{
%%     ~~Describe \code{panelvar} here~~
}
  \item{colorvar}{
%%     ~~Describe \code{colorvar} here~~
}
  \item{printlastdata}{
%%     ~~Describe \code{printlastdata} here~~
}
  \item{boldline}{
%%     ~~Describe \code{boldline} here~~
}
  \item{cutsuffix}{
%%     ~~Describe \code{cutsuffix} here~~
}
  \item{savetitle}{
%%     ~~Describe \code{savetitle} here~~
}
  \item{lastoffset}{
%%     ~~Describe \code{lastoffset} here~~
}
  \item{legend}{
%%     ~~Describe \code{legend} here~~
}
  \item{alreadymelted}{
%%     ~~Describe \code{alreadymelted} here~~
}
  \item{fcstpds}{
%%     ~~Describe \code{fcstpds} here~~
}
  \item{cornerannotations}{
%%     ~~Describe \code{cornerannotations} here~~
}
  \item{colorset}{
%%     ~~Describe \code{colorset} here~~
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
function (dta, title = "", ylabel = "", xlabel = "", cutdata = "", 
    cutset = "", cutname = "", cutcolors = "", linetypename = "", 
    ycoord = c(-Inf, +Inf), hlines = c(), panelvar = "", colorvar = "variable", 
    printlastdata = "", boldline = "", cutsuffix = "", savetitle = "", 
    lastoffset = 5, legend = "topleft", alreadymelted = FALSE, 
    fcstpds = 0, cornerannotations = NULL, colorset = c(s("black;red;green;blue;magenta;grey30;purple2;turquoise;pink;orange;dark green;light blue"), 
        cbbPalette)) 
{
    mindt <- min(zoo::index(dta))
    dta <- xts2df(dta)
    if (!("DT_ENTRY" \%in\% colnames(dta))) {
        dta$DT_ENTRY <- as.Date(rownames(dta))
    }
    if (alreadymelted) {
        dtx <- dta
    }
    else {
        dtx <- dta \%>\% select(which(sapply(., class) == "numeric" | 
            sapply(., class) == "Date")) \%>\% pivot_longer(-DT_ENTRY, 
            names_to = "variable")
    }
    yrng <- range(dtx$value, na.rm = T)
    drng <- range(dtx$DT_ENTRY, na.rm = T)
    if (nchar(boldline) > 0) {
        dtx$sizfactor <- as.factor(grepl(boldline, dtx[, "variable"], 
            perl = T))
    }
    else {
        dtx$sizfactor <- as.factor(1)
    }
    if (alreadymelted & linetypename \%in\% colnames(dta)) {
        dtx$ltype = dtx[, linetypename]
    }
    else {
        dtx$ltype = 1
    }
    if (alreadymelted & panelvar \%in\% colnames(dta)) {
        dtx$panel = dtx[, panelvar]
    }
    else {
        dtx$panel = NA
    }
    if (nchar(cutname) > 0 & grepl(cutname, paste(colnames(dta), 
        collapse = " "))) {
        cutdata <- dta[, c("DT_ENTRY", cutname)]
        cutdata$dtlag <- as.Date(c(NA, head(cutdata$DT_ENTRY, 
            -1)))
        cutdata$cid <- dta[, c(cutname)]
        cutnm <- cutname
        cids <- unique(cutdata$cid)
        cutdata$variable <- ""
        cutdata$value <- 0
    }
    else if (is.xts(cutdata) | is.data.frame(cutdata)) {
        cutdata <- as.data.frame(cutdata[paste0(mindt, "::"), 
            ])
        cutnm <- ifelse(nchar(cutname) > 1, cutname, colnames(cutdata)[1])
        cutdata$DT_ENTRY <- as.Date(rownames(cutdata))
        cutdata$dtlag <- as.Date(c(NA, head(cutdata$DT_ENTRY, 
            -1)))
        cutset <- c(-Inf, cutset, Inf)
        cids <- paste0("<", cutset[-1], " ", cutsuffix, sep = "")
        cids[length(cids)] <- paste0(">", cutset[length(cutset) - 
            1], " ", cutsuffix)
        cutdata$cid <- cids[as.numeric(cut(cutdata[, 1], cutset))]
        cutdata$variable <- ""
        cutdata$value <- 0
    }
    if (fcstpds > 0) {
        fcstfunc = function(x) {
            fc1 = try(forecast(as.ts(df2xts(x[, s("DT_ENTRY;value")])), 
                h = fcstpds), silent = TRUE)
            if (!inherits(fc1, "try-error")) {
                fc2 = as.data.frame(fc1)
                fc2$DT_ENTRY = bdayseq(max(x$DT_ENTRY), nrow(fc2))
                fcfirst = head(subset(x, select = s("variable;sizfactor;panel")), 
                  1)
                print(paste("FCST>", fcfirst[1, "variable"], 
                  " > ", fc1$model$method, " MAPE:", accuracy(fc1)[4]))
                fc2 = merge(select(fc2, DT_ENTRY, value = contains("Point Forecast")), 
                  fcfirst) \%>\% mutate(ltype = 2)
            }
            else {
                fc1 = data.frame()
            }
        }
        dtxf = group_by(dtx, variable) \%>\% do(fcstfunc(.))
        dtx = rbind(dtx, as.data.frame(dtxf))
    }
    dtx \%<>\% mutate(xcolorvar = !!rlang::sym(colorvar))
    u1 <- ggplot(dtx, aes(x = DT_ENTRY, y = value))
    if (nchar(printlastdata) > 0) {
        ulast = group_by(dtx, variable) \%>\% filter(DT_ENTRY == 
            max(DT_ENTRY)) \%>\% select(variable, yy = value, DT_ENTRY)
        if (grepl("^var", printlastdata, perl = T)) {
            ulast$ss = ulast$variable
        }
        else {
            ulast$ss = sprintf(printlastdata, ulast$yy)
        }
        if (abs(lastoffset) > 0) {
            ulast$DT_ENTRY = ulast$DT_ENTRY + lastoffset
        }
        u1 <- u1 + geom_text(aes(x = DT_ENTRY, y = yy, color = variable, 
            label = ss, group = "Region"), data = ulast, check_overlap = TRUE, 
            position = position_jitter(width = 5, height = 0))
    }
    if (length(unique(dtx$ltype)) > 1) {
        u1 <- u1 + geom_line(aes(size = sizfactor, color = xcolorvar, 
            linetype = ltype)) + scale_linetype_manual(values = c("solid", 
            "dotted", "dotdash"))
    }
    else {
        u1 <- u1 + geom_line(aes(size = sizfactor, color = xcolorvar))
    }
    if (is.data.frame(cutdata)) {
        u1 <- u1 + geom_rect(data = cutdata, aes(xmin = dtlag, 
            xmax = DT_ENTRY, fill = cid), ymin = -Inf, ymax = Inf, 
            alpha = 0.2, linetype = 0)
        if (length(cutcolors) > 1) {
            u1 <- u1 + scale_fill_manual(name = cutnm, values = cutcolors)
        }
        else {
            if (length(cids) <= 2) {
                u1 <- u1 + scale_fill_manual(name = cutnm, values = c("red", 
                  "green"))
            }
            else if (length(cids) == 3) {
                u1 <- u1 + scale_fill_manual(name = cutnm, values = c("red", 
                  "white", "green"))
            }
            else if (length(cids) == 4) {
                u1 <- u1 + scale_fill_manual(name = cutnm, values = c("red", 
                  "white", "green", "blue"))
            }
            else {
                u1 <- u1 + scale_fill_brewer(name = cutnm, type = "seq")
            }
        }
    }
    if (length(ycoord) == 2) {
        u1 <- u1 + scale_y_continuous(limits = ycoord)
    }
    else if (!is.na(dtx[1, "panel"])) {
        u1 = u1 + facet_grid(panel ~ ., scales = "free_y") + 
            theme(strip.text.y = element_text(size = 11, color = "black", 
                face = "bold"), strip.background = element_rect(fill = "wheat"))
    }
    else {
        u1 <- u1 + coord_cartesian(ylim = c(0.95, 1.05) * yrng)
    }
    if (length(levels(dtx$sizfactor)) > 1) {
        u1 <- u1 + scale_size_manual(values = c(1, 1.6))
    }
    else {
        u1 <- u1 + scale_size_manual(values = c(1, 1.6), guide = "none")
    }
    u1 <- u1 + BaseTheme() + scale_color_manual(values = colorset)
    for (ln in hlines) {
        u1 <- u1 + geom_hline(yintercept = ln, color = "blue")
    }
    title = c(title, "")
    u1 = u1 + labs(title = title[[1]], subtitle = title[[2]])
    psave(savetitle, u1)
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
