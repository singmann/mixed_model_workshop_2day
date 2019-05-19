## ----setup, include=FALSE------------------------------------------------
options(htmltools.dir.version = FALSE)
# see: https://github.com/yihui/xaringan
# install.packages("xaringan")
# see: 
# https://github.com/yihui/xaringan/wiki
# https://github.com/gnab/remark/wiki/Markdown
options(width=110)
options(digits = 4)


## ---- message=FALSE------------------------------------------------------
library("tidyverse")
data(sat.act, package = "psych")
sat.act <- sat.act %>% 
  mutate(gender = factor(gender, 1:2, labels = c("male", "female")),
         education = factor(education)) %>% 
  filter(!is.na(SATQ))  ## remove 13 NAs in SATQ
summary(sat.act) # alternatively: psych::describe(sat.act)


## ---- fig.height=3.75, fig.width= 7, dev='svg', message=FALSE------------
theme_set(theme_bw(base_size = 17))
sat2 <- sat.act %>% 
  gather(key = "sat_type", value = "sat_value", SATV, SATQ)
ggplot(sat2, aes(x = sat_value, y = ACT)) +
  geom_point(alpha = 0.2) +
  facet_wrap(~ sat_type)


## ------------------------------------------------------------------------
m1 <- lm(ACT ~ SATQ, sat.act)
summary(m1)


## ------------------------------------------------------------------------
coef(m1)


## ---- fig.height=3, fig.width=4, dev='svg'-------------------------------
ggplot(sat.act, 
       aes(x = SATQ, y = ACT)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", 
              se = FALSE)


## ------------------------------------------------------------------------
sat.act <- sat.act %>% 
  mutate(SATQ_c = SATQ - mean(SATQ),
         SATV_c = SATV - mean(SATV))
m2 <- lm(ACT ~ SATQ_c, sat.act)
summary(m2)


## ------------------------------------------------------------------------
coef(m2)


## ---- fig.height=3, fig.width=4, dev='svg'-------------------------------
ggplot(sat.act, 
       aes(x = SATQ_c, y = ACT)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", 
              se = FALSE)


## ------------------------------------------------------------------------
m2b <- lm(ACT ~ scale(SATQ), sat.act)
summary(m2b)


## ------------------------------------------------------------------------
coef(m2b)


## ---- fig.height=3, fig.width=4, dev='svg'-------------------------------
ggplot(sat.act, 
       aes(x = scale(SATQ), 
           y = ACT)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", 
              se = FALSE)


## ---- fig.height=5.5, fig.width=5.5, dev='svg', message=FALSE------------
GGally::ggscatmat(sat.act[, 3:6], alpha = 0.3)


## ------------------------------------------------------------------------
m3 <- lm(ACT ~ SATQ_c + SATV_c, sat.act)
summary(m3)


## ------------------------------------------------------------------------
## Full Model
summary(m3)$coefficients %>% zapsmall(6)


## ------------------------------------------------------------------------
## SATQ
m3_q <- lm(SATQ_c ~ SATV_c, sat.act)
sat.act$resid_q <- residuals(m3_q)
summary(lm(ACT ~ 0 + resid_q, sat.act))$coefficients 


## ------------------------------------------------------------------------
## SATV
m3_v <- lm(SATV_c ~ SATQ_c, sat.act)
sat.act$resid_v <- residuals(m3_v)
summary(lm(ACT ~ 0 + resid_v, sat.act))$coefficients 


## ------------------------------------------------------------------------
data("Prestige", package = "carData")
glimpse(Prestige, width = 50)


## ---- fig.width=5, fig.height=5, dev='svg'-------------------------------
GGally::ggscatmat(Prestige, 
                  columns = c(1, 4, 2))


## ------------------------------------------------------------------------
mp_1 <- lm(income ~ education + prestige, Prestige)
summary(mp_1)


## ------------------------------------------------------------------------
mp_1b <- lm(income ~ scale(education) + scale(prestige), Prestige)
summary(mp_1b)


## ------------------------------------------------------------------------
mp_3 <- lm(income ~ scale(education)*scale(prestige), Prestige)
summary(mp_3)


## ------------------------------------------------------------------------
library("emmeans")
get_z_positions <- function(x, at = c(-1, 0, 1), round = 2) {
  return(round(mean(x) + at*sd(x), round))
}
pres_t <- emtrends(mp_3, specs = "education", var = "prestige", 
                   at = list(education = get_z_positions(Prestige$education))) %>% summary


## ------------------------------------------------------------------------
pres_t
pres_t$prestige.trend * sd(Prestige$prestige)


## ------------------------------------------------------------------------
summary(mp_3)$coefficients %>% zapsmall


## ------------------------------------------------------------------------
edu_t <- emtrends(mp_3, specs = "prestige", var = "education", 
                  at = list(prestige = get_z_positions(Prestige$prestige)))  %>% summary

edu_t


## ------------------------------------------------------------------------
edu_t$education.trend * 
  sd(Prestige$education)


## ------------------------------------------------------------------------
summary(mp_3)$coefficients %>% zapsmall


## ------------------------------------------------------------------------
Prestige <- Prestige %>% 
  mutate(edu_cat = cut(education, 3, labels = c("low", "medium", "high")),
         prestige_cat = cut(prestige, 3, labels = c("low", "medium", "high"))
         )


## ---- include=FALSE, eval=FALSE------------------------------------------
## ## Alternatively, create unequal size groups based on theoretical quantiles.
## Prestige <- Prestige %>%
##   mutate(edu_cat = cut(education,
##                        breaks = c(
##                          0,
##                          get_z_positions(education,
##                                          at = c(qnorm(.333), qnorm(.666))),
##                          Inf),
##                        labels = c("low", "medium", "high")),
##          prestige_cat = cut(prestige,
##                        breaks = c(
##                          0,
##                          get_z_positions(prestige,
##                                          at = c(qnorm(.333), qnorm(.666))),
##                          Inf),
##                        labels = c("low", "medium", "high"))
##          )


## ---- fig.width=4.5, fig.height=4.5, dev='svg', echo=FALSE---------------
ggplot(Prestige, aes(x = prestige, y = income, 
                     color = edu_cat, shape = edu_cat)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme(legend.position = "bottom")


## ---- fig.width=4.5, fig.height=4.5, dev='svg', echo=FALSE---------------
ggplot(Prestige, aes(x = education, y = income, 
                     color = prestige_cat, shape = prestige_cat)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme(legend.position = "bottom")


## ---- eval=FALSE---------------------------------------------------------
## lm(ACT ~ SATQ_c + SATV_c, sat.act)   # a
## lm(ACT ~ SATQ_c : SATV_c, sat.act)   # b
## lm(ACT ~ 0 + SATQ_c:SATV_c, sat.act) # c
## lm(ACT ~ SATQ_c*SATV_c, sat.act)     # d
## lm(ACT ~ 0+SATQ_c*SATV_c, sat.act)   # e


## ------------------------------------------------------------------------
coef(lm(ACT ~ SATQ_c + SATV_c, sat.act))   # a
coef(lm(ACT ~ SATQ_c : SATV_c, sat.act))   # b
coef(lm(ACT ~ 0 + SATQ_c:SATV_c, sat.act)) # c


## ------------------------------------------------------------------------
coef(lm(ACT ~ SATQ_c*SATV_c, sat.act))     # d
coef(lm(ACT ~ 0+SATQ_c*SATV_c, sat.act))   # e


## ---- eval=FALSE, include=FALSE------------------------------------------
## summary(lm(ACT ~ SATQ + SATV, sat.act))   # a
## summary(lm(ACT ~ SATQ : SATV, sat.act))   # b
## summary(lm(ACT ~ 0 + SATQ:SATV, sat.act)) # c
## summary(lm(ACT ~ SATQ*SATV, sat.act))     # d
## summary(lm(ACT ~ 0+SATQ*SATV, sat.act))   # e


## ------------------------------------------------------------------------
str(sat.act) ## alternatively tibble::glimpse(sat.act)


## ------------------------------------------------------------------------
m3 <- lm(ACT ~ gender, sat.act)
summary(m3)


## ---- include=FALSE------------------------------------------------------
op <- options(width = 40)
require(dplyr)


## ------------------------------------------------------------------------
mean(sat.act$ACT)
sat.act %>% group_by(gender) %>%
  summarise(m = mean(ACT))


## ------------------------------------------------------------------------
sat.act %>% group_by(gender) %>%
  summarise(m = mean(ACT)) %>%
  {.$m[2] - .$m[1]}


## ---- include=FALSE------------------------------------------------------
options(op)


## ------------------------------------------------------------------------
model.matrix(ACT ~ gender, sat.act[1:5,])


## ------------------------------------------------------------------------
model.matrix(ACT ~ gender, sat.act[1:5,])


## ------------------------------------------------------------------------
afex::set_sum_contrasts()


## ------------------------------------------------------------------------
model.matrix(ACT ~ gender, sat.act[1:5,])


## ------------------------------------------------------------------------
m4 <- lm(ACT ~ gender, sat.act)
summary(m4)


## ---- include=FALSE------------------------------------------------------
op <- options(width = 40)


## ------------------------------------------------------------------------
mean(sat.act$ACT)
sat.act %>% group_by(gender) %>%
  summarise(m = mean(ACT))
sat.act %>% group_by(gender) %>%
  summarise(m = mean(ACT)) %>% 
  summarise(mean(m))



## ---- include=FALSE------------------------------------------------------
options(op)


## ------------------------------------------------------------------------
afex::set_default_contrasts() # or set_treatment_contrasts()


## ---- include=FALSE------------------------------------------------------
op <- options(width = 70)


## ------------------------------------------------------------------------
m5 <- lm(ACT ~ gender*education, sat.act)
coef(m5)


## ------------------------------------------------------------------------
sat.act %>% 
  group_by(gender,education) %>%
  summarise(mean(ACT))


## ---- include=FALSE------------------------------------------------------
options(op)


## ------------------------------------------------------------------------
afex::set_sum_contrasts() # or set_effects_contrasts() or set_deviation_contrasts()


## ---- include=FALSE------------------------------------------------------
op <- options(width = 70)


## ------------------------------------------------------------------------
m6 <- lm(ACT ~ gender*education, sat.act)
coef(m6)


## ------------------------------------------------------------------------
sat.act %>% 
  group_by(gender,education) %>%
  summarise(m = mean(ACT)) %>% 
  ungroup() %>% 
  summarise(mean(m))


## ---- include=FALSE------------------------------------------------------
options(op)


## ---- eval=FALSE---------------------------------------------------------
## lm(ACT ~ SATQ + SATV, sat.act)   # a: 3
## lm(ACT ~ SATQ : SATV, sat.act)   # b: 2
## lm(ACT ~ 0 + SATQ:SATV, sat.act) # c: 1
## lm(ACT ~ SATQ*SATV, sat.act)     # d: 4
## lm(ACT ~ 0+SATQ*SATV, sat.act)   # e: 3
## 
## lm(ACT ~ SATQ, sat.act)          # f: 2
## lm(ACT ~ 0 + SATQ, sat.act)      # g: 1


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
levels(sat.act$gender)
levels(sat.act$education)


## ------------------------------------------------------------------------
coef(lm(ACT ~ gender, sat.act))                  # a: 2
coef(lm(ACT ~ 0+gender, sat.act))                # b: 2
coef(lm(ACT ~ gender+education, sat.act))        # c: 7
coef(lm(ACT ~ 0+gender+education, sat.act))      # d: 7


## ------------------------------------------------------------------------
coef(lm(ACT ~ gender:education, sat.act))        # e: 13
coef(lm(ACT ~ 0+gender:education, sat.act))      # f: 12


## ---- eval = FALSE-------------------------------------------------------
## coef(lm(ACT ~ gender*education, sat.act))        # g: 12
## coef(lm(ACT ~ 0+gender*education, sat.act))      # h: 12
## coef(lm(ACT ~ gender+gender:education, sat.act)) # i: 12


## ---- include=FALSE------------------------------------------------------
op <- options(width = 70)


## ---- message=FALSE------------------------------------------------------
afex::set_sum_contrasts()
m6 <- lm(ACT ~ gender*education, sat.act)
summary(m6)


## ------------------------------------------------------------------------
sat.act %>% 
  group_by(gender, education) %>%
  summarise(m = mean(ACT)) %>% 
  ungroup() %>% 
  summarise(mean(m))


## ---- include=FALSE------------------------------------------------------
options(op)


## ---- message=FALSE------------------------------------------------------
require(car) # Companion to Applied Regression (Fox & Weisberg, 2011)
Anova(m6, type = 3)


## ---- message=FALSE, warning=FALSE---------------------------------------
library("emmeans")    
(emms <- emmeans(m6, ~education))


## ---- message=FALSE------------------------------------------------------
pairs(emms, adjust='holm')


## ---- message=FALSE, warning=FALSE---------------------------------------
library("emmeans")  
(emms <- emmeans(m6, "education")) 


## ---- message=FALSE------------------------------------------------------
cs <- list(
  "12-45" = c(0, -0.5, -0.5, 0, 0.5, 0.5),
  "0-3" = c(-1, 0, 0, 1, 0, 0),
  "all-last" = c(-rep(0.2, 5), 1)
)
contrast(emms, cs, adjust = "holm")


## ---- message=FALSE, comment='#'-----------------------------------------
library("afex")
sat.act$id <- factor(1:nrow(sat.act))
(a1 <- aov_car(ACT ~ gender+Error(id), sat.act))


## ------------------------------------------------------------------------
(a2 <- aov_car(ACT ~ gender*education+Error(id), 
               sat.act))


## ------------------------------------------------------------------------
(a2 <- aov_car(ACT ~ gender*education+Error(id), 
               sat.act))


## ---- fig.width=4.5, fig.height=4.5, dev='svg'---------------------------
afex_plot(a2, "education", "gender",
          data_geom = geom_violin, 
          data_arg = list(width = 0.4)) +
  theme(legend.position = "bottom")



## ------------------------------------------------------------------------
emmeans(a2, "gender")


## ------------------------------------------------------------------------
emmeans(a2, c("education")) %>% pairs


## ------------------------------------------------------------------------
data("Machines", package = "MEMSS")
str(Machines)


## ---- include=FALSE------------------------------------------------------
library("tidyverse")


## ------------------------------------------------------------------------
library("tidyverse")
Machines %>% group_by(Machine) %>% 
  summarise(m = mean(score), se = sd(score)/sqrt(n()))


## ---- fig.height=4, dev='svg'--------------------------------------------
ggplot(Machines, aes(x = Machine, y = score)) +
  geom_point() + 
  facet_wrap(~ Worker) + 
  theme_light()


## ------------------------------------------------------------------------
mach_agg <- Machines %>% 
  group_by(Worker, Machine) %>% 
  summarise(score = mean(score))


## ---- include=FALSE------------------------------------------------------
ggplot(mach_agg, aes(x = Machine, y = score)) + geom_point()


## ---- message=FALSE------------------------------------------------------
afex::set_sum_contrasts()
mmach <- lm(score ~ Machine, mach_agg)
car::Anova(mmach, type = 3)


## ------------------------------------------------------------------------
library("emmeans")
pairs(emmeans(mmach, "Machine"), 
      adjust = "holm")


## ------------------------------------------------------------------------
dm1 <- Machines %>% 
  filter(Worker == "1")


## ------------------------------------------------------------------------
m1 <- lm(score ~ Machine, dm1)
car::Anova(m1, type = 3)


## ---- warning=FALSE------------------------------------------------------
a1 <- aov_car(score ~ Error(Worker/Machine), Machines)
a1


## ------------------------------------------------------------------------
pairs(emmeans(a1, "Machine"), 
      adjust = "holm")


## ------------------------------------------------------------------------
pairs(emmeans(mmach, "Machine"), 
      adjust = "holm")  ## complete pooling results

