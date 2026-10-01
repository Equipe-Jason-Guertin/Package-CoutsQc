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
# 	./CoutsQc/tests/testthat/test-get_unit_cost.R
#
# Script Description:
# 	Unit test for get_unit_cost
#
# SCRIPT --------------------------------------------------------------------

test_that("Correctly compute", {
	expect_equal(
		get_unit_cost(
			value = c(23.45, 12.99, 21.48),
			year = c(2010, 2012, 2013),
			unit_cost = list(
				year_unit_cost = c("2010", "2012", "2013"),
				unit_cost = c(114.8, 120.8, 121.7)
			)
		),
		c(23.45, 12.99, 21.48) * c(114.8, 120.8, 121.7)
	)
})

test_that("Acount for NA in value and year", {
	expect_equal(
		get_unit_cost(
			value = c(NA_real_, 12.99, 21.48),
			year = c(2010, 2012, NA_real_),
			unit_cost = list(
				year_unit_cost = c("2010", "2012", "2013"),
				unit_cost = c(114.8, 120.8, 121.7)
			)
		),
		c(NA_real_, 12.99 * 120.8, NA_real_)
	)
})

test_that("Acount for NA in unit_cost", {
	expect_equal(
		get_unit_cost(
			value = c(23.45, 12.99, 21.48),
			year = c(2010, 2012, 2013),
			unit_cost = list(
				year_unit_cost = c(NA_character_, "2012", "2013"),
				unit_cost = c(114.8, 120.8, NA_real_)
			)
		),
		c(NA_real_, 12.99 * 120.8, NA_real_)
	)
})

test_that("Acount for NA year and year_unit_cost", {
	expect_equal(
		get_unit_cost(
			value = c(NA_real_, 12.99, 21.48),
			year = c(2010, 2012, NA_real_),
			unit_cost = list(
				year_unit_cost = c(NA_character_, "2012", "2013"),
				unit_cost = c(114.8, 120.8, NA_real_)
			)
		),
		c(NA_real_, 12.99 * 120.8, NA_real_)
	)
})
