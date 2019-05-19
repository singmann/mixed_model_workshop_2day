
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

all_rmd <- list.files(pattern = "Rmd$")

library(stringr)

all_rmd <- all_rmd[!str_detect(all_rmd, pattern = "pdf")]

for (i in seq_along(all_rmd)) knitr::purl(all_rmd[i])

### presentations

setwd("../part0-introduction/")
getwd()
knitr::purl("introduction.Rmd")

setwd("../part1-statistical-modeling-in-r/")
getwd()
knitr::purl("statistical_modeling.Rmd")

setwd("../part2-mixed-models-in-r/")
getwd()
knitr::purl("mixed_models.Rmd")

