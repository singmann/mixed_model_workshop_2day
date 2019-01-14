## ----setup, include=FALSE------------------------------------------------
require(psych)
data(sat.act)
sat.act$gender <- factor(sat.act$gender, 1:2, labels = c("male", "female"))
sat.act$education <- factor(sat.act$education)
sat.act <- na.omit(sat.act)

## ---- eval=FALSE---------------------------------------------------------
## lm(ACT ~ SATQ_c + SATV_c, sat.act)   # a
## lm(ACT ~ SATQ_c : SATV_c, sat.act)   # b
## lm(ACT ~ 0 + SATQ_c:SATV_c, sat.act) # c
## lm(ACT ~ SATQ_c*SATV_c, sat.act)     # d
## lm(ACT ~ 0+SATQ_c*SATV_c, sat.act)   # e

## ---- eval=FALSE---------------------------------------------------------
## lm(ACT ~ gender, sat.act)                  # a
## lm(ACT ~ 0+gender, sat.act)                # b
## lm(ACT ~ gender+education, sat.act)        # c
## lm(ACT ~ 0+gender+education, sat.act)      # d
## lm(ACT ~ gender:education, sat.act)        # e
## lm(ACT ~ 0+gender:education, sat.act)      # f
## lm(ACT ~ gender*education, sat.act)        # g
## lm(ACT ~ 0+gender*education, sat.act)      # h
## lm(ACT ~ gender+gender:education, sat.act) # i

## ------------------------------------------------------------------------
levels(sat.act$gender) ## 2
levels(sat.act$education) ## 6

