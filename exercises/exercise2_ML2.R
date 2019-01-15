## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, eval = TRUE)

## ---- message=FALSE------------------------------------------------------
## preparation and loading
library("tidyverse")
library("afex")
library("emmeans")
emm_options(lmer.df = "Satterthwaite")
theme_set(theme_bw(base_size = 15))
d <- read_csv("ml2_alter_selected.csv")
d <- d %>%
  mutate(
    source = factor(source),
    uID = factor(uID),
    syllogism = factor(syllogism),
    fluency = factor(fluency, levels = c("easy", "difficult"))
  )
str(d)

## ------------------------------------------------------------------------
## maximal model

## ------------------------------------------------------------------------
## reduced models

## ------------------------------------------------------------------------
## tests of effects

## ------------------------------------------------------------------------
## follow-up tests

## ---- fig.width=5, fig.height=4, dev='svg'-------------------------------
## plot code goes here ...

## ------------------------------------------------------------------------
# model(s) and follow-up tests

## ---- message=FALSE------------------------------------------------------
# ANOVA and follow-up tests

## ------------------------------------------------------------------------
# final plot

