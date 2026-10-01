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
# 	./CoutsQc/tests/testthat/test-get_period.R
#
# Script Description:
# 	Unit test for get_period
#
# SCRIPT --------------------------------------------------------------------

test_that("Correctly compute calendar year", {
	expect_equal(
		get_period(
			date = as.Date(c("2010-02-03", "2012-05-14", "2013-08-01")),
			period_duration = "year"
		),
		c(2010, 2012, 2013)
	)
})

test_that("Correctly compute fiscal year", {
	expect_equal(
		get_period(
			date = as.Date(c("2010-02-03", "2012-05-14", "2013-08-01")),
			period_duration = "fiscal"
		),
		c(2010, 2013, 2014)
	)
})

test_that("Correctly compute year-month", {
	expect_equal(
		get_period(
			date = as.Date(c("2010-02-03", "2012-05-14", "2013-08-01")),
			period_duration = "month"
		),
		c(2010 + 1/12, 2012 + 4/12, 2013 + 7/12)
	)
})

test_that("Correctly compute year-quarter", {
	expect_equal(
		get_period(
			date = as.Date(c("2010-02-03", "2012-05-14", "2013-08-01")),
			period_duration = "quarter"
		),
		c(2010, 2012 + 1/4, 2013 + 2/4)
	)
})

test_that("Correctly compute follow", {
	expect_equal(
		get_period(
			date = as.Date(c("2010-12-14", "2011-02-03", "2011-05-01")),
			period_duration = "follow",
			follow_interval = 30,
			index_date = as.Date(c("2011-01-01", "2011-01-01", "2011-02-27"))
		),
		c(-1, 1, 2)
	)
})

test_that("Acount for NA calendar year", {
	expect_equal(
		get_period(
			date = as.Date(c("2010-02-03", "2012-05-14", NA)),
			period_duration = "year"
		),
		c(2010, 2012, NA)
	)
})

test_that("Acount for NA fiscal year", {
	expect_equal(
		get_period(
			date = as.Date(c("2010-02-03", "2012-05-14", NA)),
			period_duration = "fiscal"
		),
		c(2010, 2013, NA)
	)
})

test_that("Acount for NA year-month", {
	expect_equal(
		get_period(
			date = as.Date(c("2010-02-03", "2012-05-14", NA)),
			period_duration = "month"
		),
		c(2010 + 1/12, 2012 + 4/12, NA)
	)
})

test_that("Acount for NA year-quarter", {
	expect_equal(
		get_period(
			date = as.Date(c("2010-02-03", "2012-05-14", NA)),
			period_duration = "quarter"
		),
		c(2010, 2012 + 1/4, NA)
	)
})

test_that("Acount for NA follow", {
	expect_equal(
		get_period(
			date = as.Date(c("2010-12-14", "2011-02-03", NA)),
			period_duration = "follow",
			follow_interval = 30,
			index_date = as.Date(c(NA, "2011-01-01", "2011-02-27"))
		),
		c(NA, 1, NA)
	)
})
