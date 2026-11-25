#' @title fg_ts_to_drawdowns
#' @name fg_ts_to_drawdowns
#' @description Convert data.table of prices to drawdowns.
#' @param indta Time series `data.table` with a date as the first column and a VALUE series as the second column.
#' @param stat (default; "index")  What to return:
#' |param|Description|
#' |:---:|:----------------|
#' |index|100*x/cummax(x)|
#' |drawdown|x/cummax(x)-1|
#' |table|List of Drawdowns|
#' |nonol_dd|List of `ntoget` top/bottom nonoverlapping `nwindow` drawdows|
#' |nonol_du|List of `ntoget` top/bottom nonoverlapping `nwindow` rallies/drawups|
#' @param nwindow (default: 5) Business day window for nonoverlapping drawdowns
#' @param ntoget (default: 20) Number of largest drawdowns (e.g.) to return
#' @returns `data.table` suitable for passing into [fgts_dygraph()]
#' @examples
#' eqtypx[,.(date,EEM,IBM,QQQ)] |> fg_ts_to_drawdowns(stat="table")
#' eqtypx[,.(date,EEM,IBM,QQQ)] |> fg_ts_to_drawdowns(stat="nonol_dd")
#' eqtypx[,.(date,EEM,IBM,QQQ)] |> fg_ts_to_drawdowns() |> fgts_dygraph(roller=1)
#' toplot = eqtypx[,.(date,EEM,IBM,QQQ)] |> fg_ts_to_drawdowns(stat="nonol_dd,10,20")
#' toplot_tr = data.table::dcast(toplot[,ii:=.I-min(.I),by=.(symbol)],
#'      ii ~ symbol, value.var=c("rtnwindow","DT_ENTRY"))
#' fg_scatplot(toplot_tr, "rtnwindow_IBM ~ rtnwindow_QQQ + label:DT_ENTRY_QQQ",
#'        "loess", title=" 20 worst 10 day drawdowns comparison")
#' @import data.table
#' @export
fg_ts_to_drawdowns<-function(indta, stat="index",nwindow=5,ntoget=20) {
  ilag=r_i=ddno=istart=iend=iworst=rtn=rtnwindow=isunique=NULL
  stat=tolower(stat)
  odata=data.table::copy(indta)
  datename <- names(indta)[1]
  ddnames <- names(indta)[-1]
  odata[,(ddnames):=lapply(.SD, \(x) nafill(x,type="locf")), .SDcols=ddnames]
  odata[,(ddnames):=lapply(.SD, \(x) x/cummax(x)-1), .SDcols=ddnames]
  if(stat=="index") {
    odata[,(ddnames):=lapply(.SD, \(x) 100*(1+x)), .SDcols=ddnames]
  }
  if(stat=="table") {
    onedd<-function(x, dtvec) {
      dd1 <- rle(fifelse(x<0,1,0))
      dd2 <- data.table(r_i=cumsum(dd1$lengths),ddno=cumsum(dd1$values), r_isdd=dd1$values)[,ilag:=shift(r_i,1)]
      dd3 <- dd2[,.(istart=.SD[1]$ilag+1, iend=.SD[.N]$ilag), by=.(ddno)][!is.na(istart)]
      dd3 <- dd3[,let(iworst=istart+which.min(x[istart:iend])-1), by=.(ddno)]
      dd3 <- dd3[,let(DT_ENTRY=dtvec[istart], END_DT_ENTRY=dtvec[iend], WORST_DT_ENTRY=dtvec[iworst], rtnwindow=x[iworst])]
      dd3 <- dd3[,let(bdaystoworst=iworst-istart,bdaystorecover=iend-iworst)]
      return(dd3[,.SD,.SDcols=!c("istart","iend","iworst")])
    }
    odata=rbindlist(lapply(ddnames, \(nm) onedd(odata[[nm]],odata[[1]])[,symbol:=nm][]))
    setcolorder(odata,"symbol")
  }
  if(grepl("^nonol",stat)) {
    odata=data.table::copy(indta)
    datecol = 1
    vmult = fifelse(grepl("_dd",stat),-1,1)
    dd.nol <- function(x,dtvec) {
      vdt <- data.table(xx=x,END_DT_ENTRY=dtvec)[,rtn:=c(NA_real_,diff(log(xx),1))][]
      vdt <- vdt[,let(rtnwindow=frollsum(rtn,nwindow,na.rm=T), DT_ENTRY=shift(END_DT_ENTRY,nwindow))]
      vdt <- vdt[order(-vmult*rtnwindow)][,.SD[1:(5*ntoget)]]
      vdt$isunique <- dd.nolfilter(vdt)
      nn_toget <- min(ntoget,nrow(vdt[isunique==TRUE,]))
      return(vdt[isunique==TRUE,][,.SD[1:nn_toget], .SDcols=!c("xx","isunique")])
    }
    odata=rbindlist(lapply(ddnames, \(nm) dd.nol(odata[[nm]],odata[[1]])[,symbol:=nm][]))
    setcolorder(odata,"symbol")
  }
  return(odata[])
}

dd.nolfilter<-function(udt) {
  c(TRUE,sapply(2:nrow(udt),
                function(i) {
                  yy=(udt[i,]$END_DT_ENTRY < udt[1:(i-1),]$DT_ENTRY | udt[i,]$DT_ENTRY>udt[1:(i-1),]$END_DT_ENTRY)
                  sum(!yy)<=0 } )) }
