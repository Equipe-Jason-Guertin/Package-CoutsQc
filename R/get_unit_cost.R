#' Convert resources usage indicator to cost.
#'
#' This function converts the resource usage value of a service into a nominal
#' cost according to a unit cost conversion table.
#'
#' @param value A \code{numeric} vector of value of the resources used.
#' @param year A \code{integer} vector of service fiscal year.
#' @param unit_cost A \code{list} or \code{data.frame} of unit cost conversion
#'  table for the resource usage indicator. Must be comprised of the variables
#'  named "year_unit_cost" and "unit_cost".
#'
#' @returns
#'  Vector of nominal cost the same length as \code{value}.
#'
#' @examples
#' value = c(23.45, 12.99, 21.48, 34.00)
#' year = c(2009, 2012, 2013, 2013)
#' unit_cost = data.frame(
#'   year_unit_cost = c(2009, 2010, 2011, 2012, 2013),
#'   unit_cost = c(3320.92, 3513.59, 3500.76, 3736.88, 3695.07)
#' )
#' get_unit_cost(value, year, unit_cost)
#'
#' @importFrom collapse fmatch whichNA alloc
#' @export
get_unit_cost <- function(value, year, unit_cost) {
	notna <- collapse::whichNA(year, invert = TRUE)
	match_index <- collapse::alloc(NA_real_, length(value))
	match_index[notna] <- collapse::fmatch(year[notna], unit_cost[["year_unit_cost"]])
	value * unit_cost[["unit_cost"]][match_index]
}
