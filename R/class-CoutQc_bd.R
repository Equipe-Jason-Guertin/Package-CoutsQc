#' @title Class CoutQc_bd
#'
#' @description
#' The \code{coutqc_bd} function allows to instance a \code{CoutQc_bd} object
#' of a Quebec medico-administrative database. Class objects created from this
#' function are used by other procedures of the package, namely the
#' \code{CoutQc} class. Its role is to validate the database, inflate service
#' costs, and identify service periods.
#'
#' The \code{CoutQc_bd_iru} class is an extension of the \code{CoutQc_bd}
#' structure that allows the conversion of resource utilizations into costs.
#'
#' @seealso
#' [get_actual_cost()], [get_period()], [get_unit_cost()]; functions used by the
#'  class.
#'
#' @importFrom R6 R6Class
#' @importFrom collapse all_identical funique fmatch na_rm whichNA ftransform
#'
#' @rdname class-CoutQc_bd
#'
#' @export
CoutQc_bd <- R6::R6Class(
	"CoutQc_bd",
	cloneable = FALSE,
	public = list(
		#' @description
		#' Creating a new \code{CoutQc_bd} object.
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
		#' @param period_duration A \code{character} to specifies the duration of the
		#'  period that is identified. By default, \code{"year"}, the calendar year is
		#'  identify. Available duration are \code{"year"}, \code{"fiscal"},
		#'  \code{"month"}, \code{"quarter"}, \code{“follow”}. See details for
		#'  additional information on supported duration.
		#' @param follow_interval An \code{integer} of the number of days in follow-up
		#'  intervals.
		#' @param cohort A \code{data.frame} of the cohort database. Used to supply
		#'  the \code{“date_index”} variable. Should be composed of one line by
		#'  identifier `id`.
		#' @param validate A \code{logical}. If \code{TRUE}, Perform additional
		#'  validations of inputs.
		#'
		#' @return A \code{CoutQc_bd} class object.
		initialize = function(
		data,
		reference_year,
		cpi,
		period_duration = "year",
		follow_interval = NULL,
		cohort = NULL,
		validate = TRUE
		) {
			# Data Argument ----
			if (!(is.data.frame(data) |
						(is.list(data) & collapse::all_identical(lengths(data)))))
				stop(paste(
					strwrap("The 'data' argument must be a valid 'data.frame'
									or 'list'.", prefix = " "),
					collapse = "\n\t "), call. = FALSE)

			if (!all(c("id", "service_cost", "service_date") %in% names(data)))
				stop(paste(
					strwrap("The 'data' argument must contain variables:\n\t
									'id', 'service_cost' and 'service_date'.", prefix = " "),
					collapse = "\n\t "), call. = FALSE)

			if (!any(class(data[["service_date"]]) %in% c("IDate", "Date")))
				stop(paste(
					strwrap("The variable 'service_date' from the 'data' argument must be
									of class 'Date'.", prefix = " "),
					collapse = "\n\t "), call. = FALSE)

			# CPI Argument ----
			if (!(is.data.frame(cpi) |
						(is.list(cpi) & collapse::all_identical(lengths(cpi)))))
				stop(paste(
					strwrap("The 'cpi' argument must be a valid 'data.frame' or 'list'.",
									prefix = " "),
					collapse = "\n\t "), call. = FALSE)

			if (!all(c("year_cpi", "cpi") %in% names(cpi)))
				stop(paste(
					strwrap("The 'cpi' table must contain variables:\n\t
									'year_cpi' et 'cpi'.", prefix = " "),
					collapse = "\n\t "), call. = FALSE)

			# Period Argument -----
			private$.validate_reference_year(reference_year, cpi)
			private$.validate_period_duration(period_duration)
			if (!is.null(follow_interval)) {
				private$.validate_follow_interval(follow_interval)
			}
			# Argument index_date ----
			if (!is.null(cohort)) {
				if (!"date_index" %in% names(cohort))
					stop(paste(
						strwrap("The 'cohort' database must contain the variable:\n\t
										'date_index'.", prefix = " "),
						collapse = "\n\t "), call. = FALSE)

				if (!any(class(cohort[["date_index"]]) %in% c("IDate", "Date")))
					stop(paste(
						strwrap("The 'cohort$date_index' variable must be of class 'Date'.",
										prefix = " "),
						collapse = "\n\t "), call. = FALSE)

				data[["date_index"]] <- cohort[["date_index"]][
					collapse::fmatch(data[["id"]], cohort[["id"]])]
			}

			if (period_duration == "follow") {
				if (!"date_index" %in% names(data))
					stop(paste(
						strwrap("The 'data' argument must contain the variable:\n\t
										'date_index'.", prefix = " "),
						collapse = "\n\t "), call. = FALSE)

				if (!any(class(data[["date_index"]]) %in% c("IDate", "Date")))
					stop(paste(
						strwrap("The variable 'date_index' from the 'data' argument must be
						of class 'Date'.", prefix = " "),
						collapse = "\n\t "), call. = FALSE)

				if (is.null(follow_interval))
					stop(paste(
						strwrap("The 'follow_interval' must be supplied when
										'period_duration == \"follow\"'.", prefix = " "),
						collapse = "\n\t "), call. = FALSE)
			}

			# Argument logique ----
			if (!is.logical(validate))
				stop(paste(
					strwrap("The 'validate' argument must be 'logical'.", prefix = " "),
					collapse = "\n\t "), call. = FALSE)

			if (!length(validate) == 1)
				stop(paste(
					strwrap("The 'validate' argument must be of length 1.", prefix = " "),
					collapse = "\n\t "), call. = FALSE)

			# Arguments ----
			private$.data <- data
			private$.service_date <- data[["service_date"]]
			private$.service_cost <- data[["service_cost"]]
			private$.reference_year <- reference_year
			private$.cpi <- cpi
			private$.period_duration <- period_duration
			if (!is.null(follow_interval)){
				private$.follow_interval <- follow_interval
			}
			private$.index_date <- data[["date_index"]]
			private$.validate()
			private$.nominal_cost <- private$.service_cost
			private$.compute_cost()
			private$.compute_period()
			private$.generate_aug_data()
		},

		#' @description
		#' Print information of an \code{CoutQc_bd} object.
		#'
		#' @param ... Further arguments passed to or from other methods.
		print = function(...) {
			cat("CoutQc_bd Database of ", NROW(self$aug_data), " obs.\n")
			cat(paste0(
				"Period duration: ", self$period_duration,
				if (self$period_duration == "follow") {
					paste(", with follow-up intervals of",
								self$follow_interval, "days")
				},"\n"
			))
			cat("Reference year:", self$reference_year, "\n")
			invisible(self)
		}
	),

	private = list(
		.validate_period_duration = function(period_duration) {
			if (!is.character(period_duration))
				stop(paste(
					strwrap("The 'period_duration' argument must be 'character'.",
									prefix = " "),
					collapse = "\n\t "), call. = FALSE)

			if (!length(period_duration) == 1)
				stop(paste(
					strwrap("The 'period_duration' argument must be of length 1.",
									prefix = " "),
					collapse = "\n\t "), call. = FALSE)

			if (
				!period_duration %in% c("year", "fiscal", "month", "quarter", "follow")
			)
				stop(paste(
					strwrap("The 'period_duration' argument must be one of:\n\t
									'year', 'fiscal', 'month', 'quarter', 'follow'.",
									prefix = " "),
					collapse = "\n\t "), call. = FALSE)
			invisible(self)
		},
		.validate_follow_interval = function(follow_interval) {
			if (!is.numeric(follow_interval))
				stop(paste(
					strwrap("The 'follow_interval' argument must be 'numeric'.",
									prefix = " "),
					collapse = "\n\t "), call. = FALSE)

			if (!length(follow_interval) == 1)
				stop(paste(
					strwrap("The 'follow_interval' argument must be of length 1.",
									prefix = " "),
					collapse = "\n\t "), call. = FALSE)

			if (!follow_interval >= 1)
				stop(paste(
					strwrap("The 'follow_interval' argument must be at minimum 1 day.",
									prefix = " "),
					collapse = "\n\t "), call. = FALSE)
			invisible(self)
		},
		.validate_reference_year = function(reference_year, cpi) {
			if (!is.numeric(reference_year))
				stop(paste(
					strwrap("The 'reference_year' argument must be 'numeric'.",
									prefix = " "),
					collapse = "\n\t "), call. = FALSE)

			if (!length(reference_year) == 1)
				stop(paste(
					strwrap("The 'reference_year' argument must be of length 1.",
									prefix = " "),
					collapse = "\n\t "), call. = FALSE)

			if (!reference_year %in% cpi[["year_cpi"]])
				stop(paste(
					strwrap("The 'reference_year' value is missing from cpi conversion
									table.", prefix = " "),
					collapse = "\n\t "), call. = FALSE)
			cpi_ref <- cpi[["cpi"]][cpi[["year_cpi"]] == reference_year]
			if (!length(cpi_ref) == 1)
				stop(paste(
					strwrap("Multiple value of cpi for the reference_year.",
									prefix = " "),
					collapse = "\n\t "), call. = FALSE)

			if (is.na(cpi_ref))
				stop(paste(
					strwrap("cpi value NA for the reference_year.", prefix = " "),
					collapse = "\n\t "), call. = FALSE)

			if (cpi_ref == 0)
				stop(paste(
					strwrap("cpi value 0 for the reference_year.", prefix = " "),
					collapse = "\n\t "), call. = FALSE)
			invisible(self)
		},
		.data = list(),
		.service_date = NA_real_,
		.service_cost = NA_real_,
		.reference_year = NA_integer_,
		.cpi = list(),
		.validate = function() {
			# cpi values missing
			have_NA <- private$.missing_table_value(
				"year",
				private$.cpi[["year_cpi"]],
				private$.cpi[["cpi"]]
			)
			if (!length(have_NA) == 0) {
				warning(paste(
					"\t",
					paste(
						strwrap("Values missing from cpi conversion table for the calender
										year :", prefix = " "),
						collapse = "\n\t "),
					"\n\t",
					paste(have_NA, collapse = ", ")
				), call. = FALSE)
			}
			invisible(self)
		},
		.nominal_cost = NA_real_,
		.actual_cost = NA_real_,
		.missing_table_value = function(period, table_period, table_value) {
			needed_dates <- CoutsQc::get_period(
				date = private$.service_date,
				period_duration = period
			) |> collapse::funique()

			times <- collapse::fmatch(needed_dates, table_period)
			values <- table_value[times]

			collapse::na_rm(
				needed_dates[union(
					collapse::whichNA(times),
					collapse::whichNA(values)
				)]
			)
		},
		.compute_cost = function() {
			private$.actual_cost <- get_actual_cost(
				nominal_cost = private$.nominal_cost,
				year = get_period(private$.service_date, "year"),
				reference_year = private$.reference_year,
				cpi = private$.cpi
			)
			invisible(self)
		},
		.period_duration = NA_character_,
		.follow_interval = NA_integer_,
		.index_date = NA_real_,
		.period = NA_real_,
		.compute_period = function() {
			private$.period <- get_period(
				date = private$.service_date,
				period_duration = private$.period_duration,
				index_date = private$.index_date,
				follow_interval = private$.follow_interval
			)
			invisible(self)
		},
		.aug_data = list(),
		.generate_aug_data = function() {
			private$.aug_data <-
				collapse::add_vars(
					private$.data,
					period = private$.period,
					actual_cost = private$.actual_cost
				) |>
				data.table::as.data.table()
			if (!"date_index" %in% names(private$.aug_data) & !is.null(private$.index_date)) {
				private$.aug_data$date_index <- private$.index_date
			}
			invisible(self)
		},
		finalize = function() {}
	),

	active = list(
		#' @field cpi
		#' Accessor for the Consumer price index conversion table.
		#'
		cpi = function(value) {
			if (missing(value)) {
				private$.cpi
			} else {
				stop("$cpi is read only.", call. = FALSE)
			}
		},

		#' @field reference_year
		#' Accessor for reference_year attribute.
		#'
		reference_year = function(value) {
			if (missing(value)) {
				private$.reference_year
			} else {
				if (value != private$.reference_year) {
					private$.validate_reference_year(value, private$.cpi)
					private$.reference_year <- value
					private$.compute_cost()
					private$.aug_data$actual_cost <- private$.actual_cost
				}
			}
		},

		#' @field period_duration
		#' Accessor for period_duration attribute.
		#'
		period_duration = function(value) {
			if (missing(value)) {
				private$.period_duration
			} else {
				private$.validate_period_duration(value)
				if (value != private$.period_duration) {
					if (value == 'follow') {
						stop("Use the $follow_interval method to change period_duration to
							 'follow'", call. = FALSE)
					}
					private$.period_duration <- value
					private$.compute_period()
					private$.aug_data$period <- private$.period
				}
			}
		},

		#' @field follow_interval
		#' Accessor for follow_interval attribute.
		#'
		follow_interval = function(value) {
			if (missing(value)) {
				private$.follow_interval
			} else {
				private$.validate_follow_interval(value)
				if (!"date_index" %in% names(private$.aug_data))
					stop(paste(
						strwrap("The 'date_index' variable must be available through the
						'data' or 'cohort' argument.", prefix = " "),
						collapse = "\n\t "), call. = FALSE)

				if (!any(class(private$.aug_data[["date_index"]]) %in% c("IDate", "Date")))
					stop(paste(
						strwrap("The variable 'date_index' must be of class 'Date'.",
										prefix = " "),
						collapse = "\n\t "), call. = FALSE)

				if (is.null(value)) {
					stop("New value for $follow_interval can not be NULL", call. = FALSE)
				}
				private$.follow_interval <- value
				private$.period_duration <- 'follow'
				private$.compute_period()
				private$.aug_data$period <- private$.period
			}
		},

		#' @field nominal_cost
		#' Accessor for nominal_cost attribute.
		#'
		nominal_cost = function(value) {
			if (missing(value)) {
				private$.nominal_cost
			} else {
				stop("$nominal_cost is read only", call. = FALSE)
			}
		},

		#' @field actual_cost
		#' Accessor for actual_cost attribute.
		#'
		actual_cost = function(value) {
			if (missing(value)) {
				private$.actual_cost
			} else {
				stop("$actual_cost is read only", call. = FALSE)
			}
		},

		#' @field period
		#' Accessor for period attribute.
		#'
		period = function(value) {
			if (missing(value)) {
				private$.period
			} else {
				stop("$period is read only", call. = FALSE)
			}
		},

		#' @field aug_data
		#' Accessor for augmented database.
		#'
		aug_data = function(value) {
			if (missing(value)) {
				private$.aug_data
			} else {
				stop("$aug_data is read only", call. = FALSE)
			}
		}
	)
)

#' @importFrom R6 R6Class
#' @importFrom collapse whichNA na_rm fmatch funique all_identical
#'
#' @rdname class-CoutQc_bd
#' @export
CoutQc_bd_iru <- R6::R6Class(
	"CoutQc_bd_iru",
	cloneable = FALSE,
	inherit = CoutQc_bd,
	public = list(
		#' @description
		#' Creating a new \code{CoutQc_bd_iru} object.
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
		#' @return A \code{CoutQc_bd} class object.
		initialize = function(
		data,
		reference_year,
		cpi,
		unit_cost,
		period_duration = "year",
		follow_interval = NULL,
		cohort = NULL,
		validate = TRUE
		) {
			# unit_cost Argument ----
			if (!(is.data.frame(unit_cost) |
						(is.list(unit_cost) &
						 collapse::all_identical(lengths(unit_cost)))))
				stop(paste(
					strwrap("The 'unit_cost' argument is invalid.", prefix = " "),
					collapse = "\n\t "), call. = FALSE)

			if (!all(c("year_unit_cost", "unit_cost") %in% names(unit_cost)))
				stop(paste(
					strwrap("The 'unit_cost' conversion table must contain variables:\n\t
									'year_unit_cost' and 'unit_cost'.", prefix = " "),
					collapse = "\n\t "), call. = FALSE)
			# Arguments ----
			private$.unit_cost <- unit_cost
			super$initialize(
				data = data,
				reference_year = reference_year,
				cpi = cpi,
				period_duration = period_duration,
				follow_interval = follow_interval,
				cohort = cohort,
				validate = validate
			)
		}
	),
	private = list(
		.validate = function() {
			super$.validate()
			# all needed unit cost conversion values available
			have_NA <- private$.missing_table_value(
				"fiscal",
				private$.unit_cost[["year_unit_cost"]],
				private$.unit_cost[["unit_cost"]]
			)
			if (!length(have_NA) == 0) {
				warning(paste(
					"\t",
					paste(strwrap("Values missing from unit cost conversion table for the
												fiscal year:", prefix = " "), collapse = "\n\t "),
					"\n\t",
					paste(have_NA, collapse = ", ")
				), call. = FALSE)
			}
			invisible(self)
		},
		.unit_cost = list(),
		.compute_cost = function() {
			private$.nominal_cost <-
				get_unit_cost(
					value = private$.service_cost,
					year = get_period(private$.service_date, "fiscal"),
					unit_cost = private$.unit_cost
				)
			super$.compute_cost()
		}
	),
	active = list(
		#' @field unit_cost
		#' Accessor for the unit cost conversion table.
		#'
		unit_cost = function(value) {
			if (missing(value)) {
				private$.unit_cost
			} else {
				stop("$unit_cost is read only.", call. = FALSE)
			}
		}
	)
)
