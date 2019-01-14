## ---- message=FALSE, results='markup'------------------------------------
library("tidyverse")
theme_set(theme_bw(base_size = 17))
data(USArrests)  
glimpse(USArrests)

## ---- fig.width=5, fig.height=5------------------------------------------
GGally::ggscatmat(USArrests)

## ------------------------------------------------------------------------
ma <- lm(Murder ~ Assault, USArrests)
summary(ma)

## ------------------------------------------------------------------------
mu <- lm(Murder ~ UrbanPop, USArrests)
summary(mu)

## ------------------------------------------------------------------------
mr <- lm(Murder ~ Rape, USArrests)
summary(mr)

## ------------------------------------------------------------------------
mall <- lm(Murder ~ Assault + UrbanPop + Rape, data = USArrests)
summary(mall)

## ------------------------------------------------------------------------
summary(ma)$coefficients %>% zapsmall

## ------------------------------------------------------------------------
ma_r <- lm(Assault ~ Rape + UrbanPop, data = USArrests)
USArrests$resid_a <- residuals(ma_r)
summary(lm(Murder ~ 0 + resid_a, USArrests))$coefficients %>% round(4)

## ------------------------------------------------------------------------
summary(mr)$coefficients %>% zapsmall
mr_r <- lm(Rape ~ Assault + UrbanPop, data = USArrests)
USArrests$resid_r <- residuals(mr_r)
summary(lm(Murder ~ 0 + resid_r, USArrests))$coefficients %>% round(4)

