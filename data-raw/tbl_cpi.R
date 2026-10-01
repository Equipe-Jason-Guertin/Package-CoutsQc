# ## Preparation of the consumer price index table
# # file `Tableau.xlsx` from "https://statistique.quebec.ca/fr/produit/tableau/3878"
# cpi <- readxl::read_xlsx("data-raw/Tableau.xlsx", skip = 5)
# max_index <- which(cpi[[1]] == "2002")
# tbl_cpi <- data.table::data.table(
# 	year_cpi = as.integer(cpi[[1]][1:max_index]),
# 	cpi = as.double(cpi[[4]][1:max_index])
# )

cpi <- data.table::fread("data-raw/ipc.csv")
tbl_cpi <- data.table::data.table(
	year_cpi = as.integer(cpi[[1]]),
	cpi = as.double(cpi[[2]])
)

usethis::use_data(tbl_cpi, overwrite = TRUE)
