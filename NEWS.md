# FinanceGraphs 0.9.21

## Bug fixes

* `fg_seasonalstudy()` ignores events datasets when a data.table with no rows passed.
* Removed `ggtext` dependency
* `fg_RegimeChange()` more robust against undeterminable confidence intervals, has better coloring.

# FinanceGraphs 0.9.2

## New Functionality

* New graph type `fg_seasonalstudy()`
* `fgts_dygraph()` ignores `splitcols` argument if only one series is plotted.
* Cache created with .onLoad error
* New function `fg_get_datemap()` gets current date map used.
* New function `fg_current_theme()` gets current `ggplot2` theme used.

## Bug fixes

* Fixed crash when the date column is part of the plot formula
* Fixed crash when `event_ds` is an empty data.table

# FinanceGraphs 0.9.0

* fgts_dygraphs: Fixed Juneteenth option expiration
* fgts_dygraphs: Added fg_RegimeChange event handler
* fgts_dygraphs: Work around for not yet fixed https://github.com/Rdatatable/data.table/pull/7667
* fg_eventStudy: Holidays are either NYSE or US Bond market holidays, not just NYSE
* fg_scatplot: Fixed error with single date breakset
* fg_scatplot: Fixed aesthetic confusion when both `doi` and `lm` are used
* Fixed error with integer date classes.
* Updates dates of interest, including VIX events

# FinanceGraphs 0.8.0

* fgts_dygraphs: y Scaling corrected to include all data
* fgts_dygraphs: Clarified annotations documentation
* fg_scatplot: Axis titling code can handle multiple formats
* fg_scatplot: User defined viewport expansion percentages added.
* fg_scatplot: Took out rounding of viewport calculations
* fg_scatplot: Added parameter `melted` to override casting process if needed
* fg_scatplot: Generalized `doi` parameter to use more sensible aesthetics
* Added Iran War to Dates of Interest
* Proper cleanup of cache directories

# FinanceGraphs 0.7.9

* Initial CRAN submission.

# FinanceGraphs 0.7.0

* Documentation completion

# FinanceGraphs 0.6.0

* `fg_scatplot()` new function
