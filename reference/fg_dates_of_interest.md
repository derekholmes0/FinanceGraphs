# Maintain Aethestics and Dates of Interest

`fg_get_dates_of_interest()` gets a set of time events for use in fg
time series graphs `fg_update_dates_of_interest()` updates a set of time
events for future use in time series graphs

## Usage

``` r
fg_get_dates_of_interest(
  search_categories = "",
  use_default = TRUE,
  startdt = NULL,
  totoday = FALSE
)

fg_update_dates_of_interest(indta, replace = FALSE)
```

## Arguments

- search_categories:

  Grep string of categories to return.

- use_default:

  (Default TRUE) use dedault dates if none else found.

- startdt:

  Minimum date for events to be returned.

- totoday:

  (Default: FALSE) Ends last date set returned (if applicable) with
  `totoday` if a date,
  [`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html)

- indta:

  `data.table` with columns as shown in details.

- replace:

  (Default: FALSE) If TRUE, replaces existing dates of interest with new
  set provided, otherwise replaces/inserts new rows only.

## Value

[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)
of date or date ranges, or \`NULL“if new dates are added.

## Details

Retrieves default dates of interest given a grepstring of categories.
There are a default set of categories provided which may not be up to
date. New data passed into `fg_update_dates_of_interest()` or
[`fg_update_aes()`](https://derekholmes0.github.io/FinanceGraphs/reference/set_constants.md)
persists across future loads of the package. Any duplicates in the new
file will be taken out.

New doi `data.frames` must have at least three columns:

|  |  |
|----|----|
| Column | Meaning |
| `category` | Grouping name (string) for a given set of dates of interest |
| `eventid` | Character string to be displayed at each event. |
| `DT_ENTRY` | Start Date of event |
| `END_DT_ENTRY` | Optional end of period to define regimes or ranges of events. |

## See also

[`fgts_dygraph()`](https://derekholmes0.github.io/FinanceGraphs/reference/fgts_dygraph.md)

## Examples

``` r
require(utils)
require(data.table)
#> Loading required package: data.table
#> 
#> Attaching package: ‘data.table’
#> The following object is masked from ‘package:base’:
#> 
#>     %notin%
tail(fg_get_dates_of_interest("fedmoves"),2)
#>    category eventid eventid2   DT_ENTRY END_DT_ENTRY  color strokePattern
#>      <char>  <char>   <char>     <Date>       <Date> <char>        <char>
#> 1: fedmoves   F:-25     rt:4 2025-10-29   2025-10-29   <NA>          <NA>
#> 2: fedmoves   F:-25  rt:3.75 2025-12-10   2025-12-10   <NA>          <NA>
#>       loc
#>    <char>
#> 1:   <NA>
#> 2:   <NA>
# To add (for example) a new FOMC cut of 50bps on 6/16/2026:
newdoi <-data.table(category="fedmoves",eventid="F:-50",
            DT_ENTRY=as.Date("6/16/2026",format="%m/%d/%Y"))
fg_update_dates_of_interest(newdoi)
#> Saved dates of interest file to /home/runner/.cache/R/FinanceGraphs/fg_doi.RD
#> NULL
# Since this is in the future, we have to make the future now.
fg_get_dates_of_interest("fedmoves",totoday=as.Date("2026-12-31"))
#> Key: <category, DT_ENTRY>
#>      category eventid eventid2   DT_ENTRY END_DT_ENTRY  color strokePattern
#>        <char>  <char>   <char>     <Date>       <Date> <char>        <char>
#>   1: fedmoves  F:init    rt:15 1980-02-15   1980-02-15   <NA>          <NA>
#>   2: fedmoves  F:+500    rt:20 1980-03-03   1980-03-03   <NA>          <NA>
#>   3: fedmoves  F:-850  rt:11.5 1980-04-01   1980-04-01   <NA>          <NA>
#>   4: fedmoves   F:-75 rt:10.75 1980-05-22   1980-05-22   <NA>          <NA>
#>   5: fedmoves  F:-125   rt:9.5 1980-06-05   1980-06-05   <NA>          <NA>
#>  ---                                                                       
#> 184: fedmoves   F:-25   rt:4.5 2024-12-18   2024-12-18   <NA>          <NA>
#> 185: fedmoves   F:-25  rt:4.25 2025-09-17   2025-09-17   <NA>          <NA>
#> 186: fedmoves   F:-25     rt:4 2025-10-29   2025-10-29   <NA>          <NA>
#> 187: fedmoves   F:-25  rt:3.75 2025-12-10   2025-12-10   <NA>          <NA>
#> 188: fedmoves   F:-50     <NA> 2026-06-16   2026-12-31   <NA>          <NA>
#>         loc
#>      <char>
#>   1:   <NA>
#>   2:   <NA>
#>   3:   <NA>
#>   4:   <NA>
#>   5:   <NA>
#>  ---       
#> 184:   <NA>
#> 185:   <NA>
#> 186:   <NA>
#> 187:   <NA>
#> 188:   <NA>
fg_reset_to_default_state("doi")
#> Removing dates file and reverting to defaults of package
#> fg_reset_to_default_state(doi) completed
fg_reset_to_default_state("all")
#> Removing dates file and reverting to defaults of package
#> Removing Aesthetics file and reverting to defaults of package
#> Removing User-made Themes and reverting to defaults of package
#> Removing cache Directory
#> fg_reset_to_default_state(all) completed
```
