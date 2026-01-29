#' Forecast Helpers
#'
#' @title Forecast_Helpers
#' @name fg_sweep
#' @description
#' `fg_sweep` Augments a [sweep::sw_sweep()] output into [fgts_dygraph()] `forecastdataset` format.  See
#' [Introduction to Sweep](https://business-science.github.io/sweep/articles/SW00_Introduction_to_sweep.html)
#'
#' @param swept_data Data resulting from a [sweep::sw_sweep()] call
#' @param confidence (Default: 80) Confidence interval (in percent) to display
#'
#' @returns `data.table` suitable for passing into [fgts_dygraph()] via the `forecastdataset` parameter
#'
#' @examples
#' require(timetk)
#' require(forecast)
#' require(sweep)
#' dta <- eqtypx[,.(date,QQQ)]
#' fcst_eqtypx <- tk_ts(dta) |> ets() |> forecast::forecast(h=30) |> sweep::sw_sweep(timetk_idx=TRUE)
#' fcst_in <- fg_sweep(fcst_eqtypx)
#' fgts_dygraph(dta,title="With Forecasts", roller=1,dtstartfrac=0.7,forecast_ds=fcst_in)
#'
#' @import data.table
#' @export
fg_sweep <- function(swept_data,confidence=80)  {
  #swept_data = data.table::copy(data.table::data.table(fcst_eqtypx))
  swept_data <- data.table(swept_data)[key=="forecast",]
  series_fctd <- colnames(swept_data)[3]
  cconf <- as.character(confidence)
  old_colname <- c("index",series_fctd,paste0("lo.",cconf),paste0("hi.",cconf))
  new_colname <- c("date",paste0(series_fctd, c(".f",".flo",".fhi")))
  setnames(swept_data,old_colname,new_colname)
  return(swept_data[,.SD,.SDcols=new_colname])
}

#' @title Forecast_Helpers
#' @name fg_predict
#' @description
#' `fg_predict` Converts a [forecast::forecast()] output into [fgts_dygraph()] `forecastdataset` format.
#' @param p Output from [forecast::forecast()]
#' @param confidence (Default: 80) Confidence interval (in percent) to display
#' @param seriesnm Series name which has been forecast.  Note that [forecast::forecast()] loses the name
#' of what's been forecast, necessitating this.
#'
#' @returns `data.table` suitable for passing into [fgts_dygraph()] via the `forecastdataset` parameter
#'
#' @examples
#' require(forecast)
#' require(zoo)
#' px_small <- narrowbydtstr(eqtypx,"-2y::")
#' fcst_one <- function(ticker) {
#'   t1_ts <- zoo::zoo(px_small[[ticker]],px_small[["date"]])
#'   forecast::ets(t1_ts) |> forecast::forecast(h=36) |>  fg_predict(seriesnm=ticker)
#'   }
#' fpred <- merge(fcst_one("QQQ"),fcst_one("IBM"),by="date")
#' fgts_dygraph(px_small,title="With Forecasts", roller=1,dtstartfrac=0.7,
#'               forecast_ds=fpred,verbose=TRUE)
#'
#' @import data.table
#' @export
fg_predict <- function(p,confidence=80,seriesnm="") {
  predf <- ts2df(p$mean,prefix=seriesnm)
  predu <- ts2df(p$upper,paste0(seriesnm,".hi"),adddate=FALSE)
  predl <- ts2df(p$lower,paste0(seriesnm,".lo"),adddate=FALSE)
  predall <- cbindlist(list(predf,predl,predu))
  rtn <- predall[,.SD,.SDcols=c("DT_ENTRY",grep(paste0("x|",confidence),names(predall),value=TRUE))][]
  newnames <- c("date",paste0(seriesnm,c(".f",".flo",".fhi")))
  setnames(rtn,newnames)
  return(rtn)
}
