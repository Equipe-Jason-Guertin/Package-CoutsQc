## Preparation of the unit cost table for the emergency dataset.
unit_cost <- data.table::fread("data-raw/cout_urgence.csv")

list_strsplit <- strsplit(unit_cost[[1]], split = "-")
tbl_urgence <- data.table::data.table(
	year_unit_cost = as.integer(sapply(list_strsplit,\(x) x[length(x)])),
	unit_cost = as.double(unit_cost[[2]])
)

usethis::use_data(tbl_urgence, overwrite = TRUE)
