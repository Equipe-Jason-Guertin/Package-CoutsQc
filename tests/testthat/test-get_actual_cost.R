# HEADER --------------------------------------------------------------------
#
# Author:       LaRue Simon
# Copyright     Copyright 2026 - LaRue Simon
# Email:        simon.larue@crchudequebec.ulaval.ca
# Affiliation:  CRCHU de Quebec
#
# Date:         2026-03-30
#
# Script Name:
# 	./CoutsQc/tests/testthat/test-get_actual_cost.R
#
# Script Description:
#		Unit test for get_actual_cost
#
# SCRIPT --------------------------------------------------------------------

test_that("Correctly compute", {
	expect_equal(
		get_actual_cost(
			nominal_cost = c(23.45, 12.99, 21.48),
			year = c(2010, 2012, 2013),
			reference_year = 2012,
			cpi = list(
				year_cpi = c(2010, 2012, 2013),
				cpi = c(114.8, 120.8, 121.7)
			)
		),
		c(23.45, 12.99, 21.48) * 120.8 / c(114.8, 120.8, 121.7)
	)
})

test_that("Acount for NA in nominal_cost and year", {
	expect_equal(
		get_actual_cost(
			nominal_cost = c(NA, 12.99, 21.48),
			year = c(2010, 2012, NA),
			reference_year = 2012,
			cpi = list(
				year_cpi = c(2010, 2012, 2013),
				cpi = c(114.8, 120.8, 121.7)
			)
		),
		c(NA_real_, 12.99, NA_real_)
	)
})

test_that("Acount for NA in cpi", {
	expect_equal(
		get_actual_cost(
			nominal_cost = c(23.45, 12.99, 21.48),
			year = c(2010, 2012, 2013),
			reference_year = 2012,
			cpi = list(
				year_cpi = c(NA_real_, 2012, 2013),
				cpi = c(114.8, 120.8, NA_real_)
			)
		),
		c(NA_real_, 12.99, NA_real_)
	)
})

test_that("Acount for NA at reference_year", {
	expect_equal(
		get_actual_cost(
			nominal_cost = c(23.45, 12.99, 21.48),
			year = c(2010, 2012, 2013),
			reference_year = NA,
			cpi = list(
				year_cpi = c(2010, 2012, 2013),
				cpi = c(114.8, 120.8, 121.7)
			)
		),
		c(NA_real_, NA_real_, NA_real_)
	)
})

test_that("Acount for NA at in cpi value", {
	expect_equal(
		get_actual_cost(
			nominal_cost = c(23.45, 12.99, 21.48),
			year = c(2010, 2012, 2013),
			reference_year = 2012,
			cpi = list(
				year_cpi = c(2010, 2012, 2013),
				cpi = c(114.8, NA_real_, 121.7)
			)
		),
		c(NA_real_, NA_real_, NA_real_)
	)
})

test_that("Acount for NA at in year_cpi value", {
	expect_equal(
		get_actual_cost(
			nominal_cost = c(23.45, 12.99, 21.48),
			year = c(2010, 2012, 2013),
			reference_year = 2012,
			cpi = list(
				year_cpi = c(2010, NA_real_, 2013),
				cpi = c(114.8, 120.8, 121.7)
			)
		),
		c(NA_real_, NA_real_, NA_real_)
	)
})

test_that("Acount for NA year and year_cpi", {
	expect_equal(
		get_actual_cost(
			nominal_cost = c(NA_real_, 12.99, 21.48),
			year = c(2010, 2012, NA_real_),
			reference_year = 2012,
			cpi = list(
				year_cpi = c(NA_character_, "2012", "2013"),
				cpi = c(114.8, 120.8, NA_real_)
			)
		),
		c(NA_real_, 12.99, NA_real_)
	)
})

