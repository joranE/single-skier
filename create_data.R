library(statskier)
library(dplyr)

con <- db_xc()
x <- query(con,"select * from main")

x$cat2[x$cat2 == ""] <- NA

maj_ind <- x$cat1 %in% c('WC','WSC','OWG','TDS')
x <- split(x,maj_ind)

XC_FAC <- load_xc_conv()
x[[2]] <- standardize_mpb_xc(mpb(con,x[[2]],0.5),XC_FAC)
x[[1]]$mpb <- NA

x <- do.call("rbind",x)
x <- dplyr::arrange(x,id)

out <- subset(x,fisid %in% fisid[cat1 %in% c('WC','WSC','OWG','TDS')])

file.remove("fis.sqlite3")
fis_db <- src_sqlite("fis.sqlite3",create = TRUE)
fis_main <- copy_to(fis_db,
                    out,
                    name = "main",
                    temporary = FALSE,
                    indexes = list("raceid","fisid","cat1","type"))