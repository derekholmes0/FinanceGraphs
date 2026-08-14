# Maintain Colors

`fg_update_aes()` updates or replaces default aesthestics (e.g. colors,
linestyles, etc). `fg_update_line_colors()` replaces line colors only
`fg_reset_to_default_state()` resets colors and/or dates of interest
`fg_replace_theme()` Replaces default theme used in static plots
`fg_verbose()` Toggles printing of aesthetics

## Usage

``` r
fg_update_aes(indta, aestype = NA_character_, persist = TRUE, replace = FALSE)

fg_update_line_colors(colorlist, replace = FALSE, persist = TRUE)

fg_replace_theme(newTheme, persist = TRUE)

fg_verbose(item = "")

fg_reset_to_default_state(reset = "all")
```

## Arguments

- indta:

  `data.table` aesthetic `data.fram` with columns as shown in details.

- aestype:

  (Default: `NA`) character string with type of aesthetic requested. If
  not provided in `[fg_Update_aes()]` the

- persist:

  (Default: TRUE) Keep changes across invocations of the package.

- replace:

  (Default: FALSE) Replaces existing dates of interest with new set
  provided, otherwise replaces/inserts new rows only.

- colorlist:

  List with up to 14 new colors just for line (series) coloring

- newTheme:

  A new ggplot2 theme

- item:

  (Default: "") A grep string for categories desired.

- reset:

  (Default: "all"), options in ("all","colors","doi") to reset to
  defaults with the package.

## Value

No return value, as these are called for the side effects of adding to
or replacing aesthetic sets.

## Details

For colors, New data passed into `fg_update_aes()` persists across
future loads of the package unless `persist=FALSE`. New color datasets
must have at least three columns:

|  |  |
|----|----|
| Column | Meaning |
| `category` | Arbitrary aestehtic category, e.g. `"lines"` for line colors. |
| `variable` | Any string that can be sorted or grepped to map to data. |
| `type` | Aesthetic type, in `c("color","colorrange","linetype","symbol","alpha")` |
| `value` | String with value detired (e.g a color) |

`variable` is used to prioritize colors, so (e.g. `D01` will be the
color of the first series in an input dataset)

If `aestype=="colorrange"` then a sequential scale of size `n_max` will
be returned using details saved from `fg_update_aes()`. See
[scales::brewer_pal](https://scales.r-lib.org/reference/pal_brewer.html)
and
[colorbrewer](https://colorbrewer2.org/#type=sequential&scheme=Greens&n=7)

## See also

[`fgts_dygraph()`](https://derekholmes0.github.io/FinanceGraphs/reference/fgts_dygraph.md),
[`fg_scatplot()`](https://derekholmes0.github.io/FinanceGraphs/reference/fg_scatplot.md),
[`fg_get_aes()`](https://derekholmes0.github.io/FinanceGraphs/reference/get_constants.md)

## Examples

``` r
# Data set, String
head(oldcolors <- fg_get_aes("lines"),3)
#>    category variable   type  value  const   used                     helpstr
#>      <char>   <char> <char> <char> <char> <char>                      <char>
#> 1:    lines      D01  color  black           all Low cardinality line colors
#> 2:    lines      D02  color    red           all Low cardinality line colors
#> 3:    lines      D03  color   blue           all Low cardinality line colors
# then change as needed.  For example, to make the second line blue, and the 4th line red,
oldcolors[c(2,3),"value"] <- c("blue","tomato")
fg_update_aes( oldcolors )
#> Saved aesthetic updates to /home/runner/.cache/R/FinanceGraphs/fg_aes.RD
head( fg_get_aes("lines"),3)
#> Key: <type, category, variable>
#>    category variable   type  value  const   used                     helpstr
#>      <char>   <char> <char> <char> <char> <char>                      <char>
#> 1:    lines      D01  color  black           all Low cardinality line colors
#> 2:    lines      D02  color   blue           all Low cardinality line colors
#> 3:    lines      D03  color tomato           all Low cardinality line colors
# to create a new category, make a similar `data.frame`, as in
newcolors <- data.frame(category=rep("mylines",3),variable=c("D01","D02","D03"),
                value=c("red","black","green"))
fg_update_aes( newcolors, aestype="color")
#> Saved aesthetic updates to /home/runner/.cache/R/FinanceGraphs/fg_aes.RD
fg_get_aesstring("mylines")
#> [1] "red"   "black" "green"
#Theme replacement
require(ggplot2)
#> Loading required package: ggplot2
fg_replace_theme(ggplot2::theme_dark(),persist=FALSE)
fg_reset_to_default_state("all")
#> Removing dates file and reverting to defaults of package
#> Removing Aesthetics file and reverting to defaults of package
#> Removing User-made Themes and reverting to defaults of package
#> Removing cache Directory
#> fg_reset_to_default_state(all) completed
```
