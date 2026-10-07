# Date Utilities

COnverts a generic relative string defining one or two endpoints to
exact dates or datestrings

## Usage

``` r
gendtstr(x, today = Sys.Date(), rtn = "dtstr")

narrowbydtstr(
  xin,
  dtstr = "",
  includetoday = TRUE,
  windowdays = 0,
  invert = FALSE,
  addindicator = FALSE
)

extenddtstr(
  instr,
  begchg = 0,
  endchg = 0,
  mindt = NULL,
  maxdt = NULL,
  rtn = "",
  rtnstyle = "string"
)
```

## Arguments

- x:

  String describing generalized date as of today

- today:

  Default [`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html)

- rtn:

  string describing what to do:
  (`list`,`datelist`,`fromtoday`,`totoday`)

- xin:

  Input `data.frame` or `data.table` with a Date column

- dtstr:

  Generalized Date string of the form `<yyyy-mm-dd>::<yyyy-mm-dd>` or
  e.g. `-3m::`

- includetoday:

  (Default: TRUE) pass either today
  [`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html) or `Sys.Date()-1`
  to `gendtstr`

- windowdays:

  (Default: 0)Number of additional days to add at beginning of series

- invert:

  (Default: FALSE) Return dates not in `dtstr`

- addindicator:

  (Default: FALSE) Returns original dataset with logical variable
  `inrange` if date is in desired range.

- instr:

  Input generalized date string, `data.table` or `xts` dataset

- begchg:

  (Default: 0) Number of calendar days to extend beginning

- endchg:

  (Default: 0) Number of calendar days to extend end

- mindt:

  Minimum date to return

- maxdt:

  Maximum date to return

- rtnstyle:

  REturn datestring or list

## Value

an exact start date `startdt` and an exact end date `enddt`, in the
following forms: If `rtn="list"` returns `c(startdt,enddt)`, if
`rtn="first"` then `startdt`, if `rtn="days` then an integer number of
days from `startdt` to `today` otherwise (by default) `"startdt::enddt"`

Same form as `xin`, i.e. a `data.table` or `data.frame`

`character` string or `list` with new dates

## Examples

``` r
gendtstr("-3m::")
#> [1] "2026-07-07::2026-10-07"
gendtstr("-2y::-3m",today=as.Date("2025-03-15"))
#> [1] "2023-03-15::2024-12-15"
narrowbydtstr(eqtypx,"-2m::-1m")
#> Key: <date>
#>           date   EEM    IBM    QQQ   TLT
#>         <Date> <num>  <num>  <num> <num>
#>  1: 2026-08-07 65.64 235.59 723.03 82.76
#>  2: 2026-08-10 65.17 236.31 720.87 82.06
#>  3: 2026-08-11 65.43 238.42 718.45 82.19
#>  4: 2026-08-12 66.46 235.98 723.70 82.11
#>  5: 2026-08-13 66.68 237.14 732.07 82.59
#>  6: 2026-08-14 66.61 234.32 731.07 82.04
#>  7: 2026-08-17 67.32 228.85 729.87 81.35
#>  8: 2026-08-18 65.34 232.67 717.51 81.66
#>  9: 2026-08-19 66.11 237.16 716.08 83.02
#> 10: 2026-08-20 66.62 233.69 710.93 82.34
#> 11: 2026-08-21 67.12 235.68 713.44 82.05
#> 12: 2026-08-24 66.11 231.04 706.32 82.56
extenddtstr("-2m::-1m")
#> [1] "2026-08-07::2026-09-07"
extenddtstr("-2m::-1m",begchg=-10,endchg=5)
#> [1] "2026-07-28::2026-09-12"
```
