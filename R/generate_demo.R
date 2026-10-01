#' Demo Databases
#'
#' Databases used for CoutsQc package demo
#'
#' @returns
#'  list of 6 demo datasets:
#'  - pharma_coutsqc, pharmaceutical services;
#'  - medical_coutsqc, medical services;
#'  - hospital_coutsqc, hospital services;
#'  - emergency_coutsqc, emergency visits;
#'  - insurance_coutsqc, RAMQ medical insurance eligibility;
#'  - cohort_coutsqc, cohort data.
#'
#' @importFrom collapse fcumsum fsum setop setv fnth ss fnrow whichv copyv join
#' @importFrom collapse na_insert
#' @importFrom data.table data.table as.IDate as.data.table
#' @importFrom stats rbinom rexp rlnorm rnbinom rnorm runif pnorm qnorm
#'
#' @export
generate_demo <- function() {
	size_id <- 2386
	level_event <- sample(
		x = c(7, 20, 50, 70, 150),
		size = size_id,
		replace = TRUE,
		prob = c(.04, .12, .34, .33, .15)
	)
	n_event <- pmax(stats::rnbinom(size_id, 2.5, mu = level_event), 1)
	m_days <- 7 +
		8 * (level_event <= 70) +
		15 * (level_event <= 50) +
		50 * (level_event <= 20) +
		420 * (level_event <= 7)

	range_time <- 365 * 10
	range_exit <- 365 * 2
	envent <- list(
		id = rep(seq_len(size_id), times = n_event)
	)

	m_days_r <- rep(m_days, times = n_event)

	envent$time <- trunc(
		pmax(
			stats::rnorm(length(envent$id), mean = m_days_r, sd = m_days_r * 0.4), 1
		) |> collapse::fcumsum(g = envent$id) +
			rep(
				stats::runif(length(n_event), 0, range_exit) *
					stats::rbinom(length(n_event), 1, 0.2),
				times = n_event
			)
	)

	envent$time <- range_time - envent$time
	envent <- collapse::ss(envent, which(envent$time >= 1))
	envent$nservice <- pmax(stats::rbinom(length(envent$time), size = 6, .7), 1)

	m_cost <- sample(
		c(5, 15, 47, 72, 127),
		collapse::fsum(envent$nservice),
		replace = TRUE,
		prob = c(.28, .42, .16, .09, .05)
	)

	data_event <- list(
		id = rep(envent$id, times = envent$nservice),
		time = rep(envent$time, times = envent$nservice),
		cost = stats::rnorm(collapse::fsum(envent$nservice),
												mean = m_cost, sd = 0.2 * m_cost)
	)

	collapse::setop(data_event$time, "+", 15796)

	data_event$type <- sample(
		c("pharma", "medical", "hospital", "emergency"),
		length(data_event$time),
		replace = TRUE,
		prob = c(0.811, 0.160, 0.020, 0.009)
	)

	nb_event_medical <- collapse::fsum(data_event$type == "medical")
	data_event$cost[which(data_event$type == "medical")] <-
		(stats::rlnorm(nb_event_medical, 2.3, 1) +
		 	stats::rbinom(nb_event_medical, 1, 0.4) *
		 	stats::rlnorm(nb_event_medical, 4, 1/2))

	min_day <- min(data_event$time)
	max_day <- max(data_event$time)
	b1 <- 10.8 / (max_day - min_day)
	b0 <- 1 - min_day / 60 * b1

	inflation <- b0 + b1 * data_event$time / 60 +
		exp(stats::rnorm(
			length(data_event$time),
			-3,
			0.00002 * (data_event$time - min_day)^1.2
		))

	collapse::setop(data_event$cost, "*", inflation)

	nb_event_hospital <- collapse::fsum(data_event$type == "hospital")
	data_event$cost[which(data_event$type == "hospital")] <-
		stats::rbinom(nb_event_hospital, 1, .6) *
		stats::runif(nb_event_hospital, min = 0.37,	max = 7.36)

	collapse::setop(
		data_event$cost, "*",
		stats::rbinom(length(data_event$cost), 1, 0.92) +
			stats::rbinom(length(data_event$cost), 1, 0.95)
	)

	date_index <- collapse::fnth(
		data_event$time,
		n = 0.15,
		g = data_event$id,
		ties = 2,
		use.g.names = FALSE
	)

	rnorm_trunc <- function(n, min, max, mu, sd) {
		F_min <- pnorm(min, mean = mu, sd = sd)
		F_max <- pnorm(max, mean = mu, sd = sd)
		u <- F_min + runif(n) * (F_max - F_min)
		qnorm(u, mean = mu, sd = sd)
	}
	date_births <- 	pmin(
		rnorm_trunc(
			size_id, min = -4309, max = 18265, mu = 6027, sd = 9000
		),
		date_index
	)

	date_deaths <-
		stats::rbinom(size_id, 1, 0.09) * (19265 - stats::rexp(size_id, 1 / 1000))
	collapse::setv(date_deaths, 0, NA)

	valid_life <- collapse::whichv(date_births < date_deaths, FALSE, invert = TRUE)

	cohort_coutsqc <- data.table::data.table(
		id = 1:size_id,
		date_birth = data.table::as.IDate(date_births),
		date_death = data.table::as.IDate(date_deaths),
		date_index = data.table::as.IDate(date_index)
	) |>
		collapse::ss(valid_life)

	max_date_subscribe <- collapse::copyv(
		unclass(cohort_coutsqc$date_death),
		is.na(cohort_coutsqc$date_death),
		18993
	)
	date_subscribe <- stats::runif(
		collapse::fnrow(cohort_coutsqc),
		unclass(cohort_coutsqc$date_birth),
		max_date_subscribe
	)
	date_withdraw <-
		date_subscribe + stats::runif(collapse::fnrow(cohort_coutsqc), 0, 4000)

	insurance_coutsqc <- data.table::data.table(
		id = cohort_coutsqc$id,
		date_subscribe = data.table::as.IDate(date_subscribe),
		date_withdraw = data.table::as.IDate(date_withdraw)
	)

	data_event_join <- collapse::join(
		data_event,
		cohort_coutsqc,
		on = "id",
		how = "inner",
		verbose = FALSE
	)

	data_event_join <- collapse::ss(
		data_event_join,
		-c(which(data_event_join$time < data_event_join$date_birth),
			 which(data_event_join$time > data_event_join$date_death))
	)
	data_event_join$date_death <- NULL
	data_event_join$date_birth <- NULL
	data_event_join$service_date <- data.table::as.IDate(data_event_join$time)
	data_event_join$time <- NULL
	data_event_join$service_cost <- data_event_join$cost
	data_event_join$cost <- NULL

	pharma_coutsqc <- collapse::ss(
		data_event_join,
		collapse::whichv(data_event_join$type, "pharma")
	)
	pharma_coutsqc$type <- NULL
	pharma_coutsqc$service_cost <- round(pharma_coutsqc$service_cost, 2)

	medical_coutsqc <- collapse::ss(
		data_event_join,
		collapse::whichv(data_event_join$type, "medical")
	)
	medical_coutsqc$type <- NULL
	medical_coutsqc$service_cost <- round(medical_coutsqc$service_cost, 2)

	emergency_coutsqc <- collapse::ss(
		data_event_join,
		collapse::whichv(data_event_join$type, "emergency")
	)
	emergency_coutsqc$type <- NULL
	emergency_coutsqc$service_cost <- NULL

	hospital_coutsqc <- collapse::ss(
		data_event_join,
		collapse::whichv(data_event_join$type, "hospital")
	)
	hospital_coutsqc$type <- NULL


	nb_hospit <- collapse::fnrow(hospital_coutsqc)
	hospital_coutsqc$nirru <- hospital_coutsqc$service_cost
	hospital_coutsqc$nirru_c1j <-
		hospital_coutsqc$service_cost + stats::rnorm(nb_hospit, 0.7, sd = 0.1)
	hospital_coutsqc$service_cost <- NULL

	sample_c1j <- sample.int(nb_hospit, nb_hospit * 0.65)
	hospital_coutsqc[["nirru_c1j"]][sample_c1j] <- NA
	hospital_coutsqc[["nirru"]][-sample_c1j] <- 0.000

	collapse::na_insert(pharma_coutsqc$service_cost, prop = 0.05, set = TRUE)
	collapse::na_insert(pharma_coutsqc$service_date, prop = 0.05, set = TRUE)
	collapse::na_insert(medical_coutsqc$service_cost, prop = 0.05, set = TRUE)
	collapse::na_insert(medical_coutsqc$service_date, prop = 0.05, set = TRUE)
	collapse::na_insert(hospital_coutsqc$nirru, prop = 0.05, set = TRUE)
	collapse::na_insert(hospital_coutsqc$service_date, prop = 0.05, set = TRUE)

	pharma_coutsqc <- data.table::as.data.table(pharma_coutsqc)
	medical_coutsqc <- data.table::as.data.table(medical_coutsqc)
	hospital_coutsqc <- data.table::as.data.table(hospital_coutsqc)
	emergency_coutsqc <- data.table::as.data.table(emergency_coutsqc)

	list(
		insurance_coutsqc = insurance_coutsqc,
		cohort_coutsqc = cohort_coutsqc,
		pharma_coutsqc = pharma_coutsqc,
		medical_coutsqc = medical_coutsqc,
		hospital_coutsqc = hospital_coutsqc,
		emergency_coutsqc = emergency_coutsqc
	)
}

