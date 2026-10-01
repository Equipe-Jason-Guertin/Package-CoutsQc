#' @title CoutQc database
#'
#' @description
#' The \code{coutqc_bd} function allows to instance a \code{CoutQc_bd} object
#' of a Quebec medico-administrative database. Class objects created from this
#' function are used by other procedures of the package, namely the
#' \code{CoutQc} class.
#'
#' @details
#' For the \code{period_duration} argument, the option \code{"fiscal"} allows to
#' identify fiscal years, which, in Quebec, begin on April 1 of a given year and
#' end on March 31 the following year. The options \code{"month"} and
#' \code{"quarter"} identify the month and quarter of each calendar year. The
#' option \code{“follow”} allows the user to define the duration of the period
#' by the time elapsed from an individual's index date. The time is measured by
#' intervals of length specified by the \code{follow_interval} argument.
#'
#' @usage coutqc_bd(data, reference_year, cpi, unit_cost = NULL,
#'  period_duration = "year", follow_interval = NULL, cohort = NULL,
#'  validate = TRUE)
#'
#' @param data A \code{data.frame} of a medico-administrative database.
#'  Its must minimally be composed of an identifier `id`, a date variable for
#'  the date of services named `service_date` and a variable for the value of
#'  those services named `service_cost`. This value can be either a direct cost
#'  or a resource utilization indicator.
#' @param reference_year An \code{integer} of the reference year to which the
#'  cost should be inflated to.
#' @param cpi A \code{data.frame}; the consumer price index table. Its format is
#'  composed of two variables: `year_cpi` corresponding to the calendar years
#'  and `cpi` corresponding to the consumer price index of those years.
#' @param unit_cost A \code{data.frame} of unit cost conversion table. The
#'  format of the table consists of two variables: `year_unit_cost`
#'  corresponding to the year at the end of the fiscal year and `unit_cost`
#'  corresponding to the cost of one unit of resource utilized. \code{NULL}
#'  takes \code{service_cost} as nominal cost.
#' @param period_duration A \code{character} to specifies the duration of the
#'  period that is identified. By default, \code{"year"}, the calendar year is
#'  identify. Available duration are \code{"year"}, \code{"fiscal"},
#'  \code{"month"}, \code{"quarter"}, \code{“follow”}. See details for
#'  additional information on supported duration.
#' @param follow_interval An \code{integer} of the number of days in follow-up
#'  intervals.
#' @param cohort A \code{data.frame} of the cohort database. Used to supply
#'  index dates.
#' @param validate A \code{logical}. If \code{TRUE}, Perform additional
#'  validations of inputs.
#'
#' @seealso
#' vignette("Introduction_macro_CoutsQc", package = "CoutsQc")
#'
#' @export
coutqc_bd <- function(
		data,
		reference_year,
		cpi,
		unit_cost = NULL,
		period_duration = "year",
		follow_interval = NULL,
		cohort = NULL,
		validate = TRUE
) {
	call <- match.call()
	if (!is.null(unit_cost)) {
		call[[1]] <- quote(CoutsQc::CoutQc_bd_iru$new)
		eval(call, parent.frame())
	}
	else {
		call[[1]] <- quote(CoutsQc::CoutQc_bd$new)
		eval(call, parent.frame())
	}
}

