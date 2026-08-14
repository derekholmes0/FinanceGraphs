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
#> [1] "2026-05-14::2026-08-14"
gendtstr("-2y::-3m",today=as.Date("2025-03-15"))
#> [1] "2023-03-15::2024-12-15"
narrowbydtstr(eqtypx,"-2m::-1m")
#> Key: <date>
#>           date   EEM      IBM      QQQ      TLT
#>         <Date> <num>    <num>    <num>    <num>
#>  1: 2026-06-15 69.75 266.7961 743.1833 85.06192
#>  2: 2026-06-16 68.64 268.8812 729.0588 85.52831
#>  3: 2026-06-17 68.56 260.4814 721.7169 85.66724
#>  4: 2026-06-18 70.79 247.3258 739.8070 86.08401
#>  5: 2026-06-22 71.21 250.4236 737.9500 85.42907
#>  6: 2026-06-23 67.17 263.0530 713.6500 85.53823
#>  7: 2026-06-24 67.25 261.0871 710.6200 86.70917
#>  8: 2026-06-25 67.96 256.4305 716.3800 86.67941
#>  9: 2026-06-26 67.19 269.6953 706.5200 86.68933
#> 10: 2026-06-29 67.43 276.0200 724.0800 86.77863
#> 11: 2026-06-30 68.41 279.2071 736.4000 85.75654
#> 12: 2026-07-01 66.48 284.2112 725.1700 85.17688
#> 13: 2026-07-02 65.70 287.4579 712.6000 85.16692
#> 14: 2026-07-06 67.57 297.3867 722.8200 85.10715
#> 15: 2026-07-07 65.72 303.9496 709.4300 84.21078
#> 16: 2026-07-08 66.23 299.8987 711.4400 84.02154
#> 17: 2026-07-09 66.78 293.1967 723.2800 84.15101
#> 18: 2026-07-10 66.90 285.5119 725.5100 84.13110
#> 19: 2026-07-13 64.50 288.1629 711.7400 83.63310
#> 20: 2026-07-14 65.67 215.5239 719.6900 83.74266
#>           date   EEM      IBM      QQQ      TLT
#>         <Date> <num>    <num>    <num>    <num>
extenddtstr("-2m::-1m")
#> [1] "2026-06-14::2026-07-14"
extenddtstr("-2m::-1m",begchg=-10,endchg=5)
#> [1] "2026-06-04::2026-07-19"
```
