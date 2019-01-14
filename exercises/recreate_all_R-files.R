
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

all_rmd <- list.files(pattern = "Rmd$")

library(stringr)

all_rmd <- all_rmd[!str_detect(all_rmd, pattern = "pdf")]

for (i in seq_along(all_rmd)) knitr::purl(all_rmd[i])
