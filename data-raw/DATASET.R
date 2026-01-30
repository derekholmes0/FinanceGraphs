## code to prepare `DATASET` dataset goes here

require(imfapi)
require(tidyquant)

eqtypx_melt <- tidyquant::tq_get(c("QQQ","IBM","TLT","EEM"),from="2016-01-01") |> dplyr::select(date, variable=symbol,value=adjusted)
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

#save(eqtypx,file="./data/eqtypx.rda")
#save(eqtypx_melt,file="./data/eqtypx_melt.rda")
#save(eqtyrtn,file="./data/eqtyrtn.rda")
#save(nomfxdta,file="./data/nomfxdta.rda")
#save(reerdta,file="./data/reerdta.rda")
#save(consumer_sent,file="./data/cons_sent.rda")

usethis::use_data(eqtypx,eqtypx_melt,eqtyrtn,nomfxdta,reerdta,consumer_sent, overwrite = TRUE)
