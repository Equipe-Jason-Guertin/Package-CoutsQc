#' Summarize actual costs
#'
#' This function produce a small summary of actual cost according to given
#' factors.
#'
#' @param out_cohort A \code{data.frame} of a databases with the actual cost and
#'  factor of interest for each id.
#' @param y A \code{character} to specify the column name of the response
#'  variable (usualy \code{"actual_cost"}).
#' @param by A \code{character} vector to specify the column names of the
#'  factor(s) of interest.
#'
#' @returns
#'  A \code{list} of an overview of actual costs according to \code{by}
#'  variables.
#'
#' @importFrom collapse fmutate collapv fsum fmean group_by_vars fsummarise
#' @importFrom purrr map
#' @importFrom utils globalVariables
#' @export
data_summary <- function(out_cohort, y, by) {
	response <- as.character(y)
	vars <- as.character(by)
	vars <- vars[!vars %in% "id"]

	cost_agg <- function(data, vars_by) {
		agg <- collapse::fmutate(
			data,
			across(
				.cols = response,
				.fns = \(x) as.numeric(x > 0),
				.names = "grzero"
			)
		) |>
			collapse::group_by_vars(vars_by)
		collapse::fsummarise(
			agg,
			across(
				.cols = response,
				.fns = list(
					collapse::fsum,
					collapse::fmean
				),
				.names = c(
					"sum",
					"mean_per_id"
				)
			),
			across(
				.cols = response,
				.fns = collapse::fmean,
				w = agg[["grzero"]],
				.names = "mean_per_id_greater_zero"
			),
			across(
				.cols = "grzero",
				.fns = list(
					collapse::fsum,
					collapse::fmean
				),
				.names = c(
					"n_id_greater_zero",
					"proportion_id_greater_zero"
				)
			)
		)
	}

	overview <- vector(
		mode = "list",
		length = 1 + length(vars) + 1 * (length(vars) >= 2)
	)
	names(overview) <- c(
		"cumulative",
		vars,
		if((length(vars) >= 2)) "cross_table" else NULL
	)

	cumulative <- collapse::collapv(
		X = out_cohort,
		by = "id",
		FUN = collapse::fsum,
		cols = response
	) |>
		collapse::fmutate(
			across(
				.cols = response,
				.fns = \(x) as.numeric(x > 0),
				.names = "grzero"
			)
		)
	overview$cumulative <- collapse::fsummarise(
		cumulative,
		across(
			.cols = response,
			.fns = list(collapse::fsum, collapse::fmean),
			.names = c("sum", "mean_per_id")
		),
		across(
			.cols = response,
			.fns = collapse::fmean,
			w = cumulative[["grzero"]],
			.names = "mean_per_id_greater_zero"
		),
		across(
			.cols = "grzero",
			.fns = list(collapse::fsum, collapse::fmean),
			.names = c("n_id_greater_zero", "proportion_id_greater_zero")
		)
	)

	if (!length(vars) == 0) {
		overview[vars] <- purrr::map(
			vars,
			function(x) {
				cost_agg(
					data = collapse::collapv(
						X = out_cohort,
						by = c(x, "id"),
						FUN = collapse::fsum,
						cols = response
					),
					vars_by = x
				)
			}
		)
	}

	if (length(vars) >= 2) {
		overview$cross_table <- cost_agg(data = out_cohort, vars_by = vars)
	}

	overview
}

if(getRversion() >= "2.15.1")  utils::globalVariables("across")
