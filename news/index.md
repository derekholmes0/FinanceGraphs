# Changelog

## FinanceGraphs 0.9.21

- Removed `ggtext` dependency
- [`fg_seasonalstudy()`](https://derekholmes0.github.io/FinanceGraphs/reference/fg_seasonalstudy.md)
  ignores events datasets when a data.table with no rows passed.
- [`fg_RegimeChange()`](https://derekholmes0.github.io/FinanceGraphs/reference/fg_RegimeChange.md)
  more robust against undeterminable confidence intervals, has better
  coloring.

## FinanceGraphs 0.9.2

CRAN release: 2026-08-21

### New Functionality

- New graph type
  [`fg_seasonalstudy()`](https://derekholmes0.github.io/FinanceGraphs/reference/fg_seasonalstudy.md)
- [`fgts_dygraph()`](https://derekholmes0.github.io/FinanceGraphs/reference/fgts_dygraph.md)
  ignores `splitcols` argument if only one series is plotted.
- Cache created with .onLoad error
- New function
  [`fg_get_datemap()`](https://derekholmes0.github.io/FinanceGraphs/reference/get_constants.md)
  gets current date map used.
- New function
  [`fg_current_theme()`](https://derekholmes0.github.io/FinanceGraphs/reference/get_constants.md)
  gets current `ggplot2` theme used.

### Bug fixes

- Fixed crash when the date column is part of the plot formula
- Fixed crash when `event_ds` is an empty data.table

## FinanceGraphs 0.9.0

CRAN release: 2026-06-22

- fgts_dygraphs: Fixed Juneteenth option expiration
- fgts_dygraphs: Added fg_RegimeChange event handler
- fgts_dygraphs: Work around for not yet fixed
  <https://github.com/Rdatatable/data.table/pull/7667>
- fg_eventStudy: Holidays are either NYSE or US Bond market holidays,
  not just NYSE
- fg_scatplot: Fixed error with single date breakset
- fg_scatplot: Fixed aesthetic confusion when both `doi` and `lm` are
  used
- Fixed error with integer date classes.
- Updates dates of interest, including VIX events

## FinanceGraphs 0.8.0

CRAN release: 2026-03-29

- fgts_dygraphs: y Scaling corrected to include all data
- fgts_dygraphs: Clarified annotations documentation
- fg_scatplot: Axis titling code can handle multiple formats
- fg_scatplot: User defined viewport expansion percentages added.
- fg_scatplot: Took out rounding of viewport calculations
- fg_scatplot: Added parameter `melted` to override casting process if
  needed
- fg_scatplot: Generalized `doi` parameter to use more sensible
  aesthetics
- Added Iran War to Dates of Interest
- Proper cleanup of cache directories

## FinanceGraphs 0.7.9

- Initial CRAN submission.

## FinanceGraphs 0.7.0

- Documentation completion

## FinanceGraphs 0.6.0

- [`fg_scatplot()`](https://derekholmes0.github.io/FinanceGraphs/reference/fg_scatplot.md)
  new function
