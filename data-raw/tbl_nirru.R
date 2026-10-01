## Preparation of the unit cost table for the NIRRU dataset.
unit_cost <- data.table::fread("data-raw/cout_nirru.csv")

list_strsplit <- strsplit(unit_cost[[1]], split = "-")
years <- sapply(list_strsplit, \(x) x[length(x)])
tbl_nirru <- data.table::data.table(
	year_unit_cost = as.integer(years),
	unit_cost = as.double(unit_cost[[2]])
)

usethis::use_data(tbl_nirru, overwrite = TRUE)

tbl_nirruc1j <- data.table::data.table(
	year_unit_cost = as.integer(years),
	unit_cost = as.double(unit_cost[[3]])
)

usethis::use_data(tbl_nirruc1j, overwrite = TRUE)


