DATA <- read_csv("fis.csv",
								col_names = TRUE,
								col_types = "iiccccdccccccciiciiddd")
DATA$cat2[DATA$cat2 == ""] <- NA