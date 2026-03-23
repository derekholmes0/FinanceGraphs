#' Consumer Sentiment Data
#'
#' University of Michigan Consumer Sentiment Data, FRED code UMCSENT
#'
#' @format ## `consumer_sent`
#' A data frame with 120 rows and 3 columns
#' \describe{
#'   \item{symbol}{FRED identifier}
#'   \item{date}{Date of report}
#'   \item{pricce}{Observation}
#' }
#'
#' @source <https://fred.stlouisfed.org/>
"consumer_sent"

#' Monthly recession indicator, FRED code RECPROUSM156N
#'
#' @format ## `recession_indic `
#' A data frame with 120 rows and 3 columns
#' \describe{
#'   \item{symbol}{FRED identifier}
#'   \item{date}{Date of report}
#'   \item{pricce}{Observation}
#' }
#'
#' @source <https://fred.stlouisfed.org/>
"recession_indic"


#' Equity Prices
#'
#' Closing Equity Prices
#'
#' @format ## `eqtypx`
#' A data table with 2529 obersavatin and 5 variables
#' \describe{
#'  \item{date}{Date Of Observation}
#'  \item{EEM}{EEM Closing Price}
#'  \item{IBM }{IBM  Closing Price}
#'  \item{QQQ}{QQQ Closing Price}
#'  \item{TLT}{TLT Closing Price}
#' }
#'
#' @source <https://finance.yahoo.com>
"eqtypx"

#' Equity Prices
#'
#' Closing Equity Prices (melted)
#'
#' @format ## `eqtypx_melt`
#' A tibble with 10116 observations and 3 columns
#' \describe{
#'  \item{date}{Date Of Observation}
#'  \item{variable}{Ticker}
#'  \item{value}{Closing Price}
#' }
#'
#' @source <https://finance.yahoo.com>
"eqtypx_melt"

#' Equity returns
#'
#' Closing Equity returns and a rolling regression
#'
#' @format ## `eqtyrtn`
#' A tibble with 2529 observations and 6 columns
#' \describe{
#'  \item{date}{Date Of Observation}
#'  \item{EEM}{EEM Log daily return}
#'  \item{IBM }{IBM Log daily return}
#'  \item{QQQ}{QQQ Log daily return}
#'  \item{TLT}{TLT Log daily return}
#'  \item{p_TLT_QQQ}{ROlling 66 business day regression of TLT on QQQ p.value}
#' }
#'
#' @source <https://finance.yahoo.com>
"eqtyrtn"

#' Nominal FX levels
#'
#' Closing Nominal currency levels (local currency per dollar)
#'
#' @format ## `nomfxdta`
#' A tibble with 13134 observations and 3 columns
#' \describe{
#'  \item{date}{Date Of Observation}
#'  \item{variable}{CUrrency}
#'  \item{value}{Currency/USD}
#' }
#'
#' @source <https://finance.yahoo.com>
"nomfxdta"

#' Nominal FX levels
#'
#' Real Effective Exchange Rates
#'
#' @format ## `reerdta`
#' A tibble with 1920 observations and 3 columns
#' \describe{
#'  \item{date}{Date Of Observation}
#'  \item{variable}{ISO Country code}
#'  \item{REGION}{investment region to which each country belongs}
#'  \item{value}{Index of Real Effective Exchange Rates}
#' }
#'
#' @source <https://imf.org>
"reerdta"

#' Ratings Database
#'
#' Ratings changes for a few select sovereigns.
#'
#' @format ## `ratings_db`
#' A tibble with 110 observations and 5 columns
#' \describe{
#'  \item{CREDIT}{Short Credit name for a few countries}
#'  \item{AGENCY}{Ratings Agency}
#'  \item{RATING}{Rating code for each country specific to each agency}
#'  \item{WATCH}{Character indicator if ratings is watch positive or negative}
#'  \item{DT_ENTRY}{Date of ratings annoucement}
#' }
#'
#' @source <https://ratingshistory.info/>
"ratings_db"

#' IBM Earnings
#'
#' IBM Earnings download
#'
#' @format ## `earnings_ibm`
#' A `data.table` with 120 observations and e columns
#' \describe{
#'  \item{reportedDate}{Earnings announcement date}
#'  \item{reportedEPS}{Reported Earnings per Share}
#'  \item{symbol}{Company}
#' }
#'
#' @source <https://finance.yahoo.com>
"earnings_ibm"

#' Example forecasts
#'
#' IBM Stock price forecasts from ets baseline ets model
#'
#' @format ## `example_fcst_set`
#' A data table with forecasts for two equities
#' \describe{
#'  \item{date}{Date}
#'  \item{QQQ.f}{Forecast price for QQQ}
#'  \item{QQQ.flo}{80th percentile in Forecast CI for QQQ}
#'  \item{QQQ.fhi}{20th percentile in Forecast CI for QQQ}
#'  \item{IBM.f}{Forecast price for IBM}
#'  \item{IBM.flo}{80th percentile in Forecast CI for IBM}
#'  \item{IBM.fhi}{20th percentile in Forecast CI for IBM}
#' }
#'
#' @source <https://finance.yahoo.com>
"example_fcst_set"


#' Constant Maturity UST rates
#'
#' FRED calculated constant maturity interest rates
#'
#' @format ## `yc_CMSUST`
#' A long format `data.table` with constant maturity UST with tenors 2 year, 10 year and 30 year
#' \describe{
#'  \item{variable}{Term of UST}
#'  \item{date}{Date of observation}
#'  \item{value}{Annualized Percent interest rae}
#' }
#'
#' @source <https://fred.stlouisfed.org/>
"yc_CMSUST"


#' IMF Economic FOrecasts
#'
#' IMF World Economic Outlook June 2025
#'
#' @format ## `imfdta`
#' A long format `data.table` with both historical economic data and projections.
#' \describe{
#'  \item{CC}{ISO Country code}
#'  \item{SUBJ}{IMF subject classification}
#'  \item{value}{Value of historical data or forecast}
#'  \item{variable}{Short abbreviation for SUBJ}
#'  \item{ctryname}{Full country name correponding to CC}
#'  \item{region}{Investment Region each country is in}
#'  \item{usedccy}{Dominant currency used in Country}
#'  \item{date}{Date of historical data or of forecast}
#' }
#'
#' @source <https://www.imf.org/en/publications/weo>
"imfdta"


