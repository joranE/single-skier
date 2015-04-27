# DATA <- read_csv("fis1.csv",
# 								col_names = TRUE,
# 								col_types = "iiccccdccccccciiciiddd")
FIS <- src_sqlite("fis.sqlite3",create = FALSE)
DATA <- tbl(FIS,"main")