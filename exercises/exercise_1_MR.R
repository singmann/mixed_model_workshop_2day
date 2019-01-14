## ---- message=FALSE, results='markup'------------------------------------
library("tidyverse")
theme_set(theme_bw(base_size = 17))
data(USArrests)  
glimpse(USArrests)

## ---- fig.width=5, fig.height=5------------------------------------------
GGally::ggscatmat(USArrests)

## ------------------------------------------------------------------------
# simple linear models:

## ------------------------------------------------------------------------
## multiple regression model

