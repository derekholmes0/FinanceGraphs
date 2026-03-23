#' Forecast Helpers
#'
#' @title Forecast_Helpers
#' @name fg_sweep
#' @description
#' `fg_sweep` Augments a [sweep::sw_sweep()] output into [fgts_dygraph()] `forecastdataset` format.  See
#' [Introduction to Sweep](https://business-science.github.io/sweep/articles/SW00_Introduction_to_sweep.html)
#' @param swept_data Data resulting from a [sweep::sw_sweep()] call
#' @param confidence (Default: 80) Confidence interval (in percent) to display
#' @returns `data.table` suitable for passing into [fgts_dygraph()] via the `forecastdataset` parameter
#' @examples
#' if (
#'     requireNamespace("forecast", quietly = TRUE) &
#'     requireNamespace("timetk", quietly = TRUE) &
#'     requireNamespace("sweep", quietly = TRUE)
#' ) {
#' fcst_eqtypx <- timetk::tk_ts(eqtypx[,.(date,QQQ)]) |> forecast::ets() |>
#'       forecast::forecast(h=30) |> sweep::sw_sweep(timetk_idx=TRUE)
#' fcst_in <- fg_sweep(fcst_eqtypx)
#' toplot <- eqtypx[,.(date,IBM,QQQ)]
#' fgts_dygraph(toplot,title="With Forecasts", roller=1,dtstartfrac=0.7,forecast_ds=fcst_in)
#' }
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

# TO do: fable.  Left Field
# library("fable","tsibble","tsibbledata")
# fc1 = tsibble(eqtypx[,.(date,QQQ)],"date") |> model(ets=ETS(QQQ)) |> forecast(h='3 months')
# fc2 = data.table(fc1)[,.(date,QQQ.f=.mean,QQQ.flo=hilo(QQQ, 95)$lower, QQQ.flo=hilo(QQQ, 95)$upper)]
# dplr: mtcars |> mutate("{vnm}_f":=.data[[vnm]]*100000)

#' @title Forecast_Helpers
#' @name fg_prophet
#' @description
#' `fg_prophet` Augments a [prophet::predict.prophet()]  output into [fgts_dygraph()] `forecastdataset` format.
#' @param prophet_data Data resulting from a [prophet::predict.prophet()]  call
#' @param seriesname (Default: `"y"`) Series name to attach forecast to
#' @returns `data.table` suitable for passing into [fgts_dygraph()] via the `forecastdataset` parameter
#' @details Note that  [prophet::predict.prophet()] loses the name of the series, the
#' @examples
#' if (requireNamespace("prophet", quietly = TRUE)) {
#' p_model <- eqtypx[,.(ds=date,y=QQQ)] |> narrowbydtstr(dtstr="-1y::") |> prophet::prophet()
#' p_fcst <- predict(p_model,prophet::make_future_dataframe(p_model,periods=60))
#' fgts_dygraph(eqtypx[,.(date,QQQ)],title="With Prophet Forecasts", roller=1,dtstartfrac=0.8,
#'       forecast_ds=fg_prophet(p_fcst,seriesname="QQQ"))
#' }
#' @import data.table
#' @export
fg_prophet <- function(prophet_data,seriesname="y")  {
  `.`=yhat=yhat_lower=yhat_upper=ds=NULL
  tortn <- data.table(prophet_data)[,.(date=as.Date(ds),yhat,yhat_lower,yhat_upper)]
  setnames(tortn,colnames(tortn),c("date",paste0(seriesname, c(".f",".flo",".fhi"))))
  return(tortn)
}
