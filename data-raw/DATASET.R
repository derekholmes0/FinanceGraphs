## code to prepare `DATASET` dataset goes here

#usethis::use_data(DATASET, overwrite = TRUE)

require(imfapi)
require(tidyquant)
require(forecast)

ratings_db <- fread("./inst/extdata/some_ratings_history.csv",na.strings="")[,let(DT_ENTRY=as.Date(DT_ENTRY,format="%m/%d/%Y"))]

eqtypx_melt <- tidyquant::tq_get(c("QQQ","IBM","TLT","EEM"),from="2018-01-01") |> dplyr::select(date, variable=symbol,value=adjusted)
rtndta  <- data.table::data.table(eqtypx_melt)[,let(rtn=c(NA_real_,diff(log(value),1))), by=.(variable)]
eqtypx <- data.table::dcast(rtndta,date ~ variable, value.var="value")

rtndta_tr <- data.table::dcast(rtndta,date ~ variable, value.var="rtn")
regfn <- function(x) { broom::glance(lm(TLT ~ QQQ, data=x))[[1,"p.value"]]}
regpvalue <- data.table::data.table(rtndta_tr)[,.(date,p_TLT_QQQ= data.table::frollapply(.SD, 66,  regfn , by.column=FALSE))]
eqtyrtn <- rtndta_tr[regpvalue,on=.(date)]

nomfxdta = tidyquant::tq_get(c("COP=X","BRL=X","CNY=X","MXN=X","EUR=X"),from="2016-01-01") |> dplyr::transmute(date, variable=gsub("=X","",symbol),value=adjusted)

imf_ctrylist <- data.table::fread("./inst/extdata/imf_ctrylist.csv")
reerdta <-imfapi::imf_get(dataflow_id="EER",dimensions=list(FREQUENCY=c("M"))) |> data.table::data.table(keyby=c("INDICATOR","COUNTRY"))
reerdta <- reerdta[INDICATOR=="REER_IX_RY2010_ACW_RCPI",][imf_ctrylist,on=.(COUNTRY=IMFCC)]
reerdta <- reerdta[!is.na(OBS_VALUE),][,.(date=lubridate::as_date(TIME_PERIOD,format="%Y-M%m"),variable=COUNTRY,REGION,value=OBS_VALUE)]
reerdta <- reerdta[date>=as.Date("2005-01-01")]

consumer_sent <- tq_get("UMCSENT",get="economic.data")


smalldta <- tail(eqtypx[,.(date,IBM,QQQ)],2*262)
fcst_one <- function(ticker) {
  t1_ts <- zoo::zoo(smalldta[[ticker]],smalldta[["date"]])
  forecast::ets(t1_ts) |> forecast::forecast(h=36) |>  fg_predict(seriesnm=ticker)
}
example_fcst_set <- merge(fcst_one("QQQ"),fcst_one("IBM"),by="date")

earnings_ibm = alphavantagepf::av_get_pf("IBM","EARNINGS") |>
  alphavantagepf::av_extract_df("quarterlyEarnings") |> dplyr::select(reportedDate, reportedEPS, symbol)

usethis::use_data(eqtypx,eqtypx_melt,eqtyrtn,nomfxdta,reerdta,consumer_sent,
                  ratings_db, example_fcst_set,earnings_ibm, overwrite = TRUE)
