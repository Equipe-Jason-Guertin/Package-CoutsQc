#' Aggregate actual costs
#'
#' This function aggregates a actual cost of medical services for each
#' individual by given factors.
#'
#' @param aug_data A \code{list} of \code{data.frame}, the databases augmented
#'  with the actual cost and factor(s) of interest for each medical services.
#' @param y A \code{character} to specify the column name of the response
#'  variable (usualy \code{"actual_cost"}).
#' @param by A \code{character} vector to specify the column names of the
#'  factor(s) of interest.
#' @param expand A \code{logical}; If \code{TRUE}, adds zero costs in output
#'  cohort for individuals who have not incurred costs for the factor(s) of
#'  interest.
#' @param include_id A \code{character} vector of individual's id to be force in
#'  the output. Used to include individual without cost in any
#'  medico-administrative database.
#'
#' @returns
#'	A \code{data.frame} of the aggregated costs for each individual.
#'
#' @importFrom collapse unlist2d funique na_omit collapv fsum join replace_na
#' @importFrom collapse "%iin%" whichNA fmutate
#' @importFrom purrr map
#' @importFrom data.table CJ
#' @importFrom utils globalVariables
#' @export
data_aggregate <- function(aug_data, y, by, expand = TRUE, include_id = NULL) {
	response <- as.character(y)
	vars <- as.character(by)
	if (!"id" %in% vars) vars <- c("id", vars)
	vars_without_database <- vars[!vars %in% "database"]

	compact_cohort <- purrr::map(
		aug_data,
		function(x) {
			collapse::collapv(
				X = collapse::na_omit(x, cols = vars_without_database),
				by = vars_without_database,
				FUN = collapse::fsum,
				cols = response
			)
		}
	) |>
		collapse::unlist2d(
			idcols = if ("database" %in% vars) "database" else FALSE,
			id.factor = TRUE,
			DT = TRUE
		) |>
		collapse::collapv(
			by = vars,
			FUN = collapse::fsum,
			cols = response
		)

	out_cohort <- compact_cohort
	if (isTRUE(expand)) {
		cross_table <- do.call(
			data.table::CJ,
			purrr::map(
				vars,
				function(var) {
					unique_var <- collapse::funique(compact_cohort[[var]]) |>
						collapse::na_omit()
					if (var == "id" & !is.null(include_id)) {
						unique_var <- collapse::funique(include_id) |>
							collapse::na_omit()
					}
					unique_var
				}
			)
		)
		names(cross_table) <- vars

		index_true_na <- collapse::"%iin%"(
			cross_table,
			compact_cohort[
				i = collapse::whichNA(compact_cohort[[response]]),
				j = vars,
				with = FALSE
			]
		)

		out_cohort <- collapse::join(
			cross_table,
			compact_cohort,
			on = vars,
			verbose = FALSE
		) |>
			collapse::fmutate(
				across(
					.cols = response,
					.fns = is.na,
					.names = "added_zero_cost"
				)
			)
		data.table::set(
			out_cohort,
			i = collapse::whichNA(out_cohort[[response]]),
			j = response,
			0
		)
		data.table::set(out_cohort, i = index_true_na, j = response, NA)
	}

	out_cohort
}

if(getRversion() >= "2.15.1")  utils::globalVariables("across")
