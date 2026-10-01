#' Identify service periods
#'
#' This function identifies the period corresponding to a service date based on
#' a period duration.
#'
#' @usage get_period(date, period_duration = c("year", "fiscal", "month",
#'  "quarter", "follow"), index_date = NULL, follow_interval = NULL)
#'
#' @param date A \code{Date} vector of service date.
#' @param period_duration A \code{character} string of the period duration.
#'  * \code{"year"} identifies the calendar year,
#'  * \code{"fiscal"} identifies the Quebec's fiscal year,
#'  * \code{"month"} identifies the month of the calendar year,
#'  * \code{"quarter"} identifies the quarter of the calendar year,
#'  * \code{"follow"} identifies follow-up period from an index date.
#' @param index_date A \code{Date} vector of index date the same length as
#'  \code{date}.
#' @param follow_interval \code{numeric}. Length of days between follow-up
#'  periods.
#'
#' @returns
#' Vector of period the same length as \code{date}.
#'
#' @examples
#'  # Calendar year as period duration.
#'  service_dates <- as.Date(c("2010-02-03", "2012-05-14", "2013-08-01"))
#'  get_period(service_dates, period_duration = "year")
#'
#'  # 40-day follow-up period.
#'  service_dates <- as.Date(c("2011-02-03", "2010-12-14", "2011-05-01"))
#'  index_dates <- as.Date(c("2011-01-13", "2010-10-14", "2011-05-12"))
#'  get_period(
#'    service_dates,
#'    period_duration = "follow",
#'    index_date = index_dates,
#'    follow_interval = 40
#'  )
#'
#' @importFrom data.table year yearqtr yearmon
#' @export
get_period <- function(
		date,
		period_duration = c("year", "fiscal", "month", "quarter", "follow"),
		index_date = NULL,
		follow_interval = NULL
) {
	period_duration <- match.arg(period_duration)
	switch(
		period_duration,
		year = data.table::year(date),
		fiscal = as.integer(floor(data.table::yearqtr(date) + 0.75)),
		month = data.table::yearmon(date),
		quarter = data.table::yearqtr(date),
		follow = (unclass(date) - unclass(index_date)) %/% follow_interval
	)
}
