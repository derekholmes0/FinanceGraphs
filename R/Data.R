#' Consumer Sentiment Data
#'
#' University of Michigan Consumer Sentiment Data
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
#' A tibble with 13134 observations and 3 columns
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


