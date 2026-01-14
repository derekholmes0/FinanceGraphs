\name{fgts_dygraph}
\alias{fgts_dygraph}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A Capitalized Title (ideally limited to 65 characters)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fgts_dygraph(indt, splitfirst = FALSE, title = "", meltvar = "variable", ylab = "", roller = "default", events = "",
eventdataset = NULL, pointers = "hair,horizontal", ylimits = NULL, miscannotation = "", dtstartpct = 0,
dtwindow = "", exportevents = FALSE, hlinedsn = NULL, hideseries = NULL, lownm = "", hinm = "",
colors = hicPaletteDygraph, stroke = NULL, strokewidth = 1, forecast = "",
forecast_periods = 10, dylegend = "always", groupnm = "common",
pvalue = NULL, addextraspace = 0.1, stepPlot = NULL, fillGraph = FALSE, extraoptions = list(), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{indt}{
Normalized dataset with miniumum columns ("DT_ENTRY",{meltvar},"value")
}
  \item{splitfirst}{
Create new scale for first time series in list
}
  \item{title}{
String with title to add to graph
}
  \item{meltvar}{
(Default: "variable") Grouping column in time series dataset `indt`
}
  \item{ylab}{
Label for y axis
}
  \item{roller}{
    (Default: "default") How smooth to make displayed time series,  Options include
    \itemize{
      \item \code{"default"}: chooses a value consistent with the size of time series,
      \item \code{"finest"}: no smoothing
      \item Integer > 0 : How many periods to smooth time series over.
      }
    }

  \item{events}{  Events to highlight on graph. Default is none, and mutiple choices can be semicolon separated.

\tabular{ll}{
    \code{doi,regimename} \tab  Dates of interest in regimeset (See vignette for regimeset creation) \cr
    \code{lmregime}  \tab  Linear model regimes on data as given \cr
    \code{stregime,variable} \tab  Regimes from precomputed structural time series models. \cr
    \code{KNN,knnset} \tab  kNN dates from ... \cr
    \code{roll} \tab   IMM CDS roll dates \cr
    \code{break} \tab  Breakout dates from ... \cr
    \code{tp,n} \tab  n turning points points from ... \cr
    \code{tpa,n} \tab  n turning points points from mean of all data in \code{indt} \cr
    \code{dt,datelist} \tab  Individual dates in a list (see examples) \cr
    \code{sig,variable,level} \tab  Significance levels for \code{variable} in indt at level \code{level}.
            Colors region red if variable is in range \code{(-inf,-level]}, white if between \code{(-level,level)} and green if \code{[level,Inf)} \cr
    \code{rtbubble} \tab  Real time bubble dates from ... \cr
    }
  }

  \item{eventdataset}{
Custom event dataset to put vertical event dates (notations) on the graph.  The dataset can contain the following columns:

\tabular{lll}{
    \code{DT_ENTRY} \tab \tab Date of event \cr
    \code{END_DT_ENTRY} \tab (optional) \tab End date of event for regime or multiperiod events \cr
    \code{color} \tab (optional) \tab Color to use for event line and text \cr
    \code{text} \tab (optional) \tab Text to display at event line \cr
    \code{loc} \tab (optional) \tab Location to place text ("top","bottom","middle") within graph
}

Note that this overrides the `events` parameter if both \code{events} and \code{eventdataset} are specified.
}
  \item{pointers}{  Options to show pointers when mousing over the graph.  Multiple options can be semicolon separated. Default is "hair,horizontal".
\tabular{ll}{
    \code{hair} \tab  Use hairlines \cr
    \code{horizontal}  \tab  Horizontal hairlines only \cr
    \code{hline} \tab  horizlotal lines>?  \cr
    \code{hbar} \tab  bars ? \cr
    }

}
  \item{ylimits}{  Scale limits for y axis.  Can either be a two element numeric vector \code{c(min,max)} or a string of the form \code{"winsor,limit"}. to choose 
  limits based on percentiles of the data.

}
  \item{miscannotation}{ Miscellaneous annotations to add to points in the graph.  Can take several forms:
    \itemize{
      \item \code{"last,last"}: creates label text of last value of each time series next to its last value
      \item \code{"last,<string>"}: creates label text <string> of last value of each time series next to its last value
      \item data.frame with columns ....
      }
}
  \item{dtstartpct}{
What percentage within the date range of the dataset to start the graph on.  Creates a graph which just highlights recent data, but can be backed up to see broader time scales.
}
  \item{dtwindow}{
A string specifying a date window to narrow the input data \code{indta} to.  The string speficies one or both of the endpoints of the desired range, generally formed as
\code{"start::end"}.  Each endpoint can be absolute (e.g. \code{"yyyy-mm-dd"}) or relative th the current date, e.g. \code{"-3m"} or \code{-90d}.  For example, \code{"-3m::-5d"}
specifies a window from 3 months ago to 5 days ago.  If any endpoint is left blank, it is assumed to be the first or last date in the data.
}
  \item{exportevents}{
    (default: FALSE) 
    Copies all event dates used in the graph to a data.table called \code{dyg_dates} in the global environment which contains the events plotted on the graph.
}
  \item{hlinedsn}{
%%     ~~Describe \code{hlinedsn} here~~
}
  \item{hideseries}{
%%     ~~Describe \code{hideseries} here~~
}
  \item{lownm}{
%%     ~~Describe \code{lownm} here~~
}
  \item{hinm}{
%%     ~~Describe \code{hinm} here~~
}
  \item{colors}{
List of colors to use for each time series in the graph
}
  \item{stroke}{
%%     ~~Describe \code{stroke} here~~
}
  \item{strokewidth}{
%%     ~~Describe \code{strokewidth} here~~
}
  \item{forecast}{
%%     ~~Describe \code{forecast} here~~
}
  \item{forecast_periods}{
%%     ~~Describe \code{forecast_periods} here~~
}
  \item{dylegend}{
Option to add legend to dygraph.  See dyLegend
}
  \item{groupnm}{
Name of a group to link multiple dygraphs together for synchronized zooming and panning (useful for dashboards)
}
  \item{pvalue}{
%%     ~~Describe \code{pvalue} here~~
}
  \item{addextraspace}{
%%     ~~Describe \code{addextraspace} here~~
}
  \item{stepPlot}{
Display all time series as step plots rather than line plots
}
  \item{fillGraph}{
Fill area under time series curves
}
  \item{extraoptions}{
%%     ~~Describe \code{extraoptions} here~~
}
  \item{\dots}{
Additional arguments ...
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
library(tidyquant)
dta = tq_get(c("IBM","ORCL","NVDA")) |> select(DT_ENTRY=date, variable=symbol,value=adjusted)
fgts_dygraph(dta, title="Tech Stock Prices", ylab="Adjusted Close Price", roller="default",dtstartpct=0.8)
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
