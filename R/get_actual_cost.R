#' Adjust nominal costs into actual costs
#'
#' This function adjusts the nominal cost of a service according to the
#' consumer price index (CPI) for a given reference year.
#'
#' @param nominal_cost A \code{numeric} vector of nominal cost.
#' @param year A \code{integer} vector of service's calendar year.
#' @param reference_year \code{integer}. Reference year to adjust to.
#' @param cpi A \code{list} or \code{data.frame} of consumer price index
#'  conversion table. Must be comprised of the variables named "year_cpi" and
#'  "cpi".
#'
#' @returns
#'  Vector of actual cost the same length as \code{nominal_cost}.
#'
#' @examples
#' nominal_cost <- c(23.45, 12.99, 21.48, 34.00)
#' year <- c(2009, 2012, 2013, 2013)
#' reference_year <- 2013
#' cpi = data.frame(
#'   year_cpi = c(2009, 2010, 2011, 2012, 2013),
#'   cpi = c(113.4, 114.8, 118.3, 120.8, 121.7)
#' )
#' get_actual_cost(nominal_cost, year, reference_year, cpi)
#'
#' @seealso [tbl_cpi], Consumer price index conversion table.
#'
#' @importFrom collapse fmatch whichNA alloc
#' @export
get_actual_cost <- function(nominal_cost, year, reference_year, cpi) {
	cpi_ref <- cpi[["cpi"]][which(cpi[["year_cpi"]] == reference_year)]
	multplier_cpi <- cpi_ref / cpi[["cpi"]]
	notna <- collapse::whichNA(year, invert = TRUE)
	match_index <- collapse::alloc(NA_real_, length(nominal_cost))
	match_index[notna] <- collapse::fmatch(year[notna], cpi[["year_cpi"]])
	nominal_cost * multplier_cpi[match_index]
}
