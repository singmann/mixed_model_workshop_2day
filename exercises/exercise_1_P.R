## ---- message=FALSE, warning=FALSE---------------------------------------
library("tidyverse")
library("broom") # not automatically loaded

## ------------------------------------------------------------------------
# Run complete chunk: Ctrl+Shift+Enter

# You might need to set the correct working directory via the menu: 
# Session -> Set Working Directory -> To Source File Location

afex::set_sum_contrasts() # just in case we set orthogonal contrasts

load("ssk16_dat_prepared_ex1.rda") # data preapred in 'prepare_data.R'
glimpse(dat1)


## ------------------------------------------------------------------------
m0 <- lm(if_A_then_B ~ B_given_A, dat1)
summary(m0)

## ---- fig.width=7, fig.height=3------------------------------------------
ggplot(data = dat1) + 
  geom_point(mapping = aes(x = B_given_A, y = if_A_then_B), alpha = 0.2, pch = 16) + 
  coord_fixed()

## ------------------------------------------------------------------------

# go

## ------------------------------------------------------------------------
load("ssk16_dat_prepared_ex2.rda")
str(dat2)

## ------------------------------------------------------------------------
# go

## ------------------------------------------------------------------------
# go

