#' Consumer Price Index, annual average, not seasonally adjusted, Quebec
#'
#' Subset of data from the \emph{Institut de la statistique du Quebec} on the
#' consumer price index (CPI). The values are annual averages for all goods and
#' services (‘2002’ fixed at 100).
#'
#' @format
#' \code{Data.table} of two columns:
#' \describe{
#'   \item{year_cpi}{Year}
#'   \item{cpi}{Consumer price index}
#' }
#' @source Statistics Canada, Table 18-10-0005-01  Consumer Price Index, annual
#'  average, not seasonally adjusted.
#' <https://doi.org/10.25318/1810000501-eng>
#' Last update: 2026-01-19
"tbl_cpi"

#' Unit values data for hospital stays, Quebec
#'
#' Contains data from the Quebec Ministry of Health and Social Services on the
#' unit costs of level of relative intensity of resources used (known as NIRRU)
#' per fiscal period.
#'
#' @format
#' \code{Data.table} of two columns:
#' \describe{
#'   \item{year_unit_cost}{Fiscal year}
#'   \item{unit_cost}{Unit value of one unit of NIRRU}
#' }
#' @source .
#' Last update: 2025
"tbl_nirru"

#' Emergency room visit unit value data, Quebec
#'
#' Contains data from Quebec's Ministry of Health and Social Services on unit
#' costs for an emergency room visit (regardless of duration) by fiscal period.
#'
#' @format
#' \code{Data.table} of two columns:
#' \describe{
#'   \item{year_unit_cost}{Fiscal year}
#'   \item{unit_cost}{Unit value of one emergency room visit}
#' }
#' @source MSSS.
#' Last update: 2026
"tbl_urgence"

#' Unit values data for for same-day surgery hospital stays, Quebec
#'
#' Contains data from the Quebec Ministry of Health and Social Services on the
#' unit costs of level of relative intensity of resources used for same-day
#' surgery (known as NIRRUc1j) per fiscal period.
#'
#' @format
#' \code{Data.table} of two columns:
#' \describe{
#'	 \item{year_unit_cost}{Fiscal year}
#'   \item{unit_cost}{Unit value of one unit of NIRRUc1j}
#' }
#' @source .
#' Last update: 2025
"tbl_nirruc1j"
