\name{fgts_biplot}
\alias{fgts_biplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A Capitalized Title (ideally limited to 65 characters)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fgts_biplot(PC, x = "PC1", y = "PC2", rownames = c(), title = "", showpoints = "all", opts = "repelarrows", xcolors = c("blue", "black", "grey80", "grey60", "grey40", "grey20"), vectorcolormap = "red", xsizes = c(2, 1, 0.5, 0.2, 0.1, 0.1), footnote = "", labelsincludehr = FALSE, rtn = "graph")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{PC}{
%%     ~~Describe \code{PC} here~~
}
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{rownames}{
%%     ~~Describe \code{rownames} here~~
}
  \item{title}{
%%     ~~Describe \code{title} here~~
}
  \item{showpoints}{
%%     ~~Describe \code{showpoints} here~~
}
  \item{opts}{
%%     ~~Describe \code{opts} here~~
}
  \item{xcolors}{
%%     ~~Describe \code{xcolors} here~~
}
  \item{vectorcolormap}{
%%     ~~Describe \code{vectorcolormap} here~~
}
  \item{xsizes}{
%%     ~~Describe \code{xsizes} here~~
}
  \item{footnote}{
%%     ~~Describe \code{footnote} here~~
}
  \item{labelsincludehr}{
%%     ~~Describe \code{labelsincludehr} here~~
}
  \item{rtn}{
%%     ~~Describe \code{rtn} here~~
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
function (PC, x = "PC1", y = "PC2", rownames = c(), title = "", 
    showpoints = "all", opts = "repelarrows", xcolors = c("blue", 
        "black", "grey80", "grey60", "grey40", "grey20"), vectorcolormap = "red", 
    xsizes = c(2, 1, 0.5, 0.2, 0.1, 0.1), footnote = "", labelsincludehr = FALSE, 
    rtn = "graph") 
{
    library(ggnewscale)
    rown = row.names(PC$x)
    if (length(rownames) == nrow(PC$x)) {
        obsnames = rownames
    }
    else {
        obsnames = c(rep("x", max(1, nrow(PC$x) - 4)), rep("rec", 
            3), "last")
    }
    data <- data.frame(obsnames = obsnames, PC$x)
    plot <- ggplot(data, aes(x = .data[[x]], y = .data[[y]]))
    if (showpoints == "all" || showpoints == TRUE) {
        plot <- plot + geom_point(aes(color = obsnames, size = obsnames)) + 
            scale_color_manual(values = xcolors) + scale_size_manual(values = xsizes)
    }
    if (showpoints == "last") {
        plot <- plot + geom_point(aes(color = obsnames, size = obsnames), 
            data = tail(data, 1)) + scale_color_manual(values = xcolors) + 
            scale_size_manual(values = xsizes)
    }
    plot <- plot + geom_hline(yintercept = 0, linewidth = 0.2) + 
        geom_vline(xintercept = 0, linewidth = 0.2)
    datapc <- data.frame(variable = rownames(PC$rotation), PC$rotation)
    mult <- min((max(data[, y]) - min(data[, y])/(max(datapc[, 
        y]) - min(datapc[, y]))), (max(data[, x]) - min(data[, 
        x])/(max(datapc[, x]) - min(datapc[, x]))))
    datapc <- transform(datapc, v1 = 0.7 * mult * (get(x)), v2 = 0.7 * 
        mult * (get(y)))
    expl <- summary(PC)$importance[2, ]
    if (rtn == "data") {
        return(datapc)
    }
    if (grepl("repel", opts)) {
        plot <- plot + geom_label_repel(data = mutate(datapc, 
            v1 = 1.05 * v1, v2 = 1.05 * v2), aes(x = v1, y = v2, 
            label = variable), size = 2, , max.overlaps = 40)
    }
    else {
        plot <- plot + geom_text(data = mutate(datapc, v1 = 1.05 * 
            v1, v2 = 1.05 * v2), aes(x = v1, y = v2, label = variable), 
            size = 3, vjust = 1, color = "black")
    }
    if (is.data.frame(vectorcolormap)) {
        datapc \%<>\% left_join(vectorcolormap, by = "variable")
    }
    else {
        datapc \%<>\% mutate(color = vectorcolormap)
    }
    if (grepl("arrows", opts)) {
        plot <- plot + new_scale_color() + geom_segment(data = datapc, 
            aes(x = 0, y = 0, xend = v1, yend = v2, color = color), 
            arrow = arrow(length = unit(0.2, "cm")), alpha = 0.75, 
            linewidth = 1.2) + scale_color_identity()
        plot <- plot + geom_segment(data = datapc, aes(x = 0, 
            y = 0, xend = v1, yend = v2, color = color), arrow = arrow(length = unit(0.2, 
            "cm")), alpha = 0.75, linewidth = 1.2) + scale_color_manual(values = c("red", 
            "blue", "green"))
    }
    plot <- plot + BaseTheme(legend = "insidebottom", title = "bdays back")
    plot <- plot + labs(title = title, x = paste(x, round(expl[1] * 
        100, 1), "pct"), y = paste(y, round(expl[2] * 100, 1), 
        "pct"))
    if (labelsincludehr) {
        plot <- plot + labs(caption = "Labels may include 1st PC hedge ratio")
    }
    else {
        plot <- plot + labs(caption = paste0("N:", length(obsnames), 
            " obs"))
    }
    plot
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
