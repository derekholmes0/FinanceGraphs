# Group Synchronization

Sets, gets, or resets a common name to be passed into
[`fgts_dygraph()`](https://derekholmes0.github.io/FinanceGraphs/reference/fgts_dygraph.md)
for synchronization.

## Usage

``` r
fg_sync_group(gpname = "")
```

## Arguments

- gpname:

  A string or NULL

  - `gpname=NULL` turns off dygraphs synchronization.

  - `gpname=<string>` set the common group to `<string>`

  - `gpname=""` (Default), just returns the current common group name.

## Value

current groupname

## Details

Use thie to set a common groupname for time scale synchronization (for
Markdown or shiny apps), Only set it in the beginning, or when needed,
and call with NULL to turn synchronization off.

## See also

[`fgts_dygraph()`](https://derekholmes0.github.io/FinanceGraphs/reference/fgts_dygraph.md)

## Examples

``` r
fg_sync_group()
#> NULL
fg_sync_group("common")
fg_sync_group()
#> [1] "common"
fg_sync_group(NULL)
```
