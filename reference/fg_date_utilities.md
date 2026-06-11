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
#> [1] "2026-03-11::2026-06-11"
gendtstr("-2y::-3m",today=as.Date("2025-03-15"))
#> [1] "2023-03-15::2024-12-15"
narrowbydtstr(eqtypx,"-2m::-1m")
#> Key: <date>
#> Empty data.table (0 rows and 5 cols): date,EEM,IBM,QQQ,TLT
extenddtstr("-2m::-1m")
#> [1] "2026-04-11::2026-05-11"
extenddtstr("-2m::-1m",begchg=-10,endchg=5)
#> [1] "2026-04-01::2026-05-16"
```
