
<!-- README.md is generated from README.Rmd. Please edit that file -->

# FinanceGraphs

<!-- badges: start -->

<!-- badges: end -->

A flexible wrapper around \[dygraphs\] and \[ggplot2\] to graph and
annototate financial time series data. The package provides several ways
to add additional information to a simple series vs time data, including
horizontal annotations (events highligted by lines and colored bands)
and vertical annotations (key levels or regions). Colors and set of
labels can be customized and persist across invocations of the package,
but sensible dfaults are used. To minimize verbiage to get what you
want, the package emphasizes options in the main function call rather
than multiple functions or pipes to build up a graph.

## Installation

You can install the development version of FinanceGraphs from

``` r
pak::pak("derekholmes0/FinanceGraphs")
# install.packages("FinanceGraphs")  # If from CRAN
```

## Input data

The package is designed to be flexile with input data. The data can
either be in **long** format (e.g. `date`,`series`,`value`) or **wide**
format (date,col1,col2,…).  
The `date` column can be called anything or be anywhere in the input
data.frame, but there must be at least one coercible column with dates
and one numeric column. Each column after the date column is treated as
a separate series to be graphed, but if the column name (series name)
ends in any of ‘`.lo`, `.hi`, `.f` (for forecast),’`.flo`, `.fhi` then
those columns are treated specially as lower/upper bands or forecast
series associated with the prefix of the name.

## Simple Examples

``` r
fgts_dygraph(eqtypx, title="Stock Prices", ylab="Adjusted Close")
```

- All of the data is displayed on the graph by default, but nost of the
  time, we want to focus on recent periods and have the option of
  backing up to all history. The `dtstartfrac` parameter shows just the
  last (1-`dtstartfrac`) percent of the data. Alternatively, a *generic
  date window* of the form “start::end” can be used in the `dtwindow`
  parameter. Each end of the window can be a full date
  (e.g. “2022-01-01”) or a relative date string (e.g. “-6m” for 6 months
  ago.

- One of the most appealing features of \[dygraphs\] is the ability to
  smooth series interactively. The `roller` parameter adds a rolling
  average smoother of the specified width (in data points). A default
  smoothing parameter is chosen depending on the length of the
  underlying data, but can be overridden using the `roller` parameter.

- Individual series can be highlighted with different stroke patterns
  and width using the `hilightcols` and related parameters.

``` r
fgts_dygraph(eqtypx, dtstartfrac=0.8,hilightcols="IBM",hilightwidth=4,roller=3)
```

## Events

### Events in the function call

### Event helpers

## Other annnotations

## Forecasts

``` r

#' fgts_dygraph(eqtypx, title="Stock Prices", ylab="Adjusted Close")
#'
#' # With series Highlights, finer resolution and focused date range
#' fgts_dygraph(eqtypx, dtstartfrac=0.8,hilightcols="IBM",hilightwidth=4,roller=3)
#'
#' # Rebasing to 1/1/2022
#' fgts_dygraph(eqtypx, title="Rebased Prices", ylab="Adjusted Close",rebase="2022-01-01")
#'
#' # Using bands (.lo, .hi)
#' toplot <- reerdta[REGION=="LATAM",.(cop=sum(value*(IMFCC=="COL")),
#'               cop.lo=min(value),cop.hi=max(value)),by=.(date)]
#' fgts_dygraph(toplot,title="COP REER vs Latam peers",roller=3)
#'
#' # Events Examples.  Notice how roller shortens with the series.
#' # See Vignette for more extensive examples
#' require(data.table)
#' smalldta <- narrowbydtstr(eqtypx[,.(date,TLT,EEM)],"-3y::")
#' fgts_dygraph(smalldta,events="doi,regm;doi,fedmoves")
#' fgts_dygraph(smalldta,events="date,FOMO,2025-01-01,2025-06-01;date,xmas,2025-12-25")
#'
#' # Events passed in as data.frames
#' myevents = data.frame(end_date =as.Date(c("2024-03-10","2024-01-10")),
#'             date=as.Date(c("2024-01-10","2024-04-10")),
#'             text=c("range","event"),color=c("green","red"))
#' fgts_dygraph(smalldta,events="doi,fedmoves",event_ds=myevents)
#'
#' # Annotations on y axis
#' fgts_dygraph(eqtypx,annotations="last,linevalue")
#' fgts_dygraph(eqtypx,annotations="hline,100,at100,red;hline,200,at200;range,300,400")
#'
#' # use with helpers
#' require(data.table)
#' smalldta <- narrowbydtstr(eqtypx[,.(date,IBM,QQQ)],"-2y::")
#' fgts_dygraph(smalldta,title="W TurnPts",event_ds=fg_findTurningPoints(smalldta[,.(date,QQQ)]))
#' fgts_dygraph(smalldta,title="W Sentiment",event_ds=fg_cut_to_events(consumer_sent,center="zscore"))
#' fgts_dygraph(smalldta,title="W dividends",event_ds=fg_tq_divs(c("IBM","QQQ")))
#'
#' # Other helpers for use with credit ratings, breakouts, and earnings data are available.
#'
#' # use with forecasts
#'
#' require(forecast)
#' smalldta <- narrowbydtstr(eqtypx[,.(date,IBM,QQQ)],"-2y::")
#' fcst_one <- function(ticker) {
#'   t1_ts <- zoo::zoo(smalldta[[ticker]],smalldta[["date"]])
#'   forecast::ets(t1_ts) |> forecast::forecast(h=36) |>  fg_predict(seriesnm=ticker)
#'   }
#' fpred <- merge(fcst_one("QQQ"),fcst_one("IBM"),by="date")
#' fgts_dygraph(smalldta,title="With Forecasts", dtstartfrac=0.7,forecast_ds=fpred)
#'
```
