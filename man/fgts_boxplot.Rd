\name{fgts_boxplot}
\alias{fgts_boxplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A Capitalized Title (ideally limited to 65 characters)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fgts_boxplot(dt, title = "", xlabel = "value", ylabel = "index", lastlabel = "label", flip = F, reverse = F, colorstr = "", violin = F, savetitle = "", outliers = F, legend = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dt}{
%%     ~~Describe \code{dt} here~~
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
  \item{lastlabel}{
%%     ~~Describe \code{lastlabel} here~~
}
  \item{flip}{
%%     ~~Describe \code{flip} here~~
}
  \item{reverse}{
%%     ~~Describe \code{reverse} here~~
}
  \item{colorstr}{
%%     ~~Describe \code{colorstr} here~~
}
  \item{violin}{
%%     ~~Describe \code{violin} here~~
}
  \item{savetitle}{
%%     ~~Describe \code{savetitle} here~~
}
  \item{outliers}{
%%     ~~Describe \code{outliers} here~~
}
  \item{legend}{
%%     ~~Describe \code{legend} here~~
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
function (dt, title = "", xlabel = "value", ylabel = "index", 
    lastlabel = "label", flip = F, reverse = F, colorstr = "", 
    violin = F, savetitle = "", outliers = F, legend = "") 
{
    if (("variable" \%in\% colnames(dt)) & ("value" \%in\% colnames(dt))) {
        dml = group_by(dt, variable) \%>\% slice(n())
        dm = as.data.frame(dt)
    }
    else {
        dt <- dt[, !is.na(apply(dt, 2, sum))]
        dm <- data.table::melt(as.data.table(dt))
        dml <- data.table::melt(as.data.table(tail(dt, 1)))
    }
    if (globaldebug[["grdbg"]]) {
        cAssign("dm;dml;dt")
    }
    if (nchar(colorstr) > 0) {
        dm$col <- grepl(colorstr, dm$variable, perl = TRUE)
    }
    else {
        dm$col <- ""
    }
    if (violin) {
        b1 <- ggplot(dm, aes(variable, value, color = col)) + 
            geom_violin(outlier.shape = ifelse(outliers, 16, 
                NA))
    }
    else {
        b1 <- ggplot(dm, aes(variable, value, color = col)) + 
            geom_boxplot(outlier.shape = ifelse(outliers, 16, 
                NA))
    }
    if (reverse) {
        b1 <- b1 + scale_x_discrete(limits = rev(levels(dm$variable)))
    }
    if (grepl("label", lastlabel)) {
        dgno <- ifelse(min(dml$value, na.rm = T) < 1, 4, 1)
        dml$valtxt <- ifelse(is.na(dml$value), "-", format(dml$value, 
            digits = dgno))
        b2 <- b1 + geom_text(aes(variable, value, label = valtxt), 
            data = dml, color = "red", fontfamily = "Helvetica", 
            check_overlap = TRUE, fontface = "bold", size = 4)
        title <- paste(title, "\nDot/No: Last observation")
    }
    else {
        if (grepl("point", lastlabel)) {
            dgno <- ifelse(min(dml$value) < 1, 4, 1)
            dml$valtxt <- format(dml$value, digits = dgno)
            b2 <- b1 + geom_point(aes(variable, value), data = dml, 
                color = "red", size = 4)
            title <- paste(title, "\nDot/No: Last observation")
        }
    }
    title = c(title, "")
    b2 <- b2 + labs(title = title[[1]], x = xlabel, y = ylabel)
    b2 <- b2 + guides(fill = ifelse(grepl("empty|nofill", legend), 
        "none", "legend"), colour = ifelse(grepl("empty|nocolor", 
        legend), "none", "legend"), size = ifelse(grepl("empty|nosize", 
        legend), "none", "legend"), shape = ifelse(grepl("empty|noshape", 
        legend), "none", "legend"))
    if (flip) {
        b2 <- b2 + coord_flip()
    }
    b2 <- b2 + BaseTheme(gridstyle = "dotted")
    psave(savetitle, b2, w = 5, h = 8)
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
