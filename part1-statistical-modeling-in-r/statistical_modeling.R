## ----setup, include=FALSE------------------------------------------------
options(htmltools.dir.version = FALSE)
# see: https://github.com/yihui/xaringan
# install.packages("xaringan")
# see: 
# https://github.com/yihui/xaringan/wiki
# https://github.com/gnab/remark/wiki/Markdown
options(width=110)
options(digits = 4)
options(pillar.sigfig = 4)


## ---- message=FALSE------------------------------------------------------
library("tidyverse")
load("ds_vb_18.rda")  ## or: load(url("http://singmann.org/download/r/ds_vb_18.rda"))
summary(ds_vb_18)     ## or: psych::describe(ds_vb_18)


## ---- fig.height=3.75, fig.width= 7, dev='svg', message=FALSE------------
theme_set(theme_bw(base_size = 17))
wm2 <- ds_vb_18 %>% 
  gather(key = "wm_task", value = "wm_performance", binding, updating)
ggplot(wm2, aes(x = wm_performance, y = reasoning)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~ wm_task, scales = "free_x")


## ------------------------------------------------------------------------
m1 <- lm(reasoning ~ binding, ds_vb_18)
summary(m1)


## ------------------------------------------------------------------------
coef(m1)


## ---- fig.height=3, fig.width=4, dev='svg'-------------------------------
ggplot(ds_vb_18, 
       aes(x = binding, 
           y = reasoning)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", 
              se = FALSE)


## ------------------------------------------------------------------------
ds_vb_18 <- ds_vb_18 %>% 
  mutate(binding_c = binding - mean(binding),
         updating_c = updating - mean(updating))
m2 <- lm(reasoning ~ binding_c, ds_vb_18)
summary(m2)


## ------------------------------------------------------------------------
coef(m2)


## ---- fig.height=3, fig.width=4, dev='svg'-------------------------------
ggplot(ds_vb_18, 
       aes(x = binding_c, 
           y = reasoning)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", 
              se = FALSE)


## ------------------------------------------------------------------------
m2b <- lm(reasoning ~ scale(binding), ds_vb_18)
summary(m2b)


## ------------------------------------------------------------------------
coef(m2b)


## ---- fig.height=3, fig.width=4, dev='svg'-------------------------------
ggplot(ds_vb_18, 
       aes(x = scale(binding), 
           y = reasoning)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", 
              se = FALSE)


## ---- fig.height=5.5, fig.width=5.5, dev='svg', message=FALSE------------
GGally::ggscatmat(ds_vb_18[, 4:6], alpha = 0.3)


## ------------------------------------------------------------------------
m3 <- lm(reasoning ~ binding_c + updating_c, ds_vb_18)
summary(m3)


## ------------------------------------------------------------------------
## Full Model
summary(m3)$coefficients %>% zapsmall(6)


## ------------------------------------------------------------------------
## binding
m3_b <- lm(binding_c ~ updating_c, ds_vb_18)
ds_vb_18$resid_b <- residuals(m3_b)
summary(lm(reasoning ~ 0 + resid_b, ds_vb_18))$coefficients 


## ------------------------------------------------------------------------
## updating
m3_u <- lm(updating_c ~ binding_c, ds_vb_18)
ds_vb_18$resid_u <- residuals(m3_u)
summary(lm(reasoning ~ 0 + resid_u, ds_vb_18))$coefficients 


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
## lm(reasoning ~ binding + updating, ds_vb_18)   # a
## lm(reasoning ~ binding : updating, ds_vb_18)   # b
## lm(reasoning ~ 0 + binding:updating, ds_vb_18) # c
## lm(reasoning ~ binding*updating, ds_vb_18)     # d
## lm(reasoning ~ 0+binding*updating, ds_vb_18)   # e


## ---- include=FALSE------------------------------------------------------
op <- options(width = 40)


## ------------------------------------------------------------------------
coef(lm(reasoning ~ binding + updating, ds_vb_18))
coef(lm(reasoning ~ binding : updating, ds_vb_18))
coef(lm(reasoning ~ 0+binding:updating, ds_vb_18))


## ------------------------------------------------------------------------
coef(lm(reasoning ~ binding*updating, ds_vb_18))
coef(lm(reasoning ~ 0+binding*updating, ds_vb_18))


## ---- include=FALSE------------------------------------------------------
options(op)


## ---- eval=FALSE, include=FALSE------------------------------------------
## summary(lm(reasoning ~ binding + updating, ds_vb_18))   # a
## summary(lm(reasoning ~ binding : updating, ds_vb_18))   # b
## summary(lm(reasoning ~ 0 + binding:updating, ds_vb_18)) # c
## summary(lm(reasoning ~ binding*updating, ds_vb_18))     # d
## summary(lm(reasoning ~ 0+binding*updating, ds_vb_18))   # e


## ------------------------------------------------------------------------
str(ds_vb_18) ## alternatively tibble::glimpse(ds_vb_18)


## ------------------------------------------------------------------------
m3 <- lm(reasoning ~ order, ds_vb_18)
summary(m3)


## ---- include=FALSE------------------------------------------------------
op <- options(width = 40)
require(dplyr)


## ------------------------------------------------------------------------
mean(ds_vb_18$reasoning)
ds_vb_18 %>% group_by(order) %>%
  summarise(m = mean(reasoning))


## ------------------------------------------------------------------------
ds_vb_18 %>% group_by(order) %>%
 summarise(m=mean(reasoning)) %>%
 {.$m[2] - .$m[1]}


## ---- include=FALSE------------------------------------------------------
options(op)


## ------------------------------------------------------------------------
model.matrix(reasoning ~ order, ds_vb_18[1:5,])


## ------------------------------------------------------------------------
model.matrix(reasoning ~ order, ds_vb_18[1:5,])


## ---- warning=FALSE, message=FALSE, echo=FALSE, include=FALSE------------
afex::set_sum_contrasts()


## ------------------------------------------------------------------------
afex::set_sum_contrasts()


## ------------------------------------------------------------------------
model.matrix(reasoning ~ order, ds_vb_18[1:5,])


## ------------------------------------------------------------------------
m4 <- lm(reasoning ~ order, ds_vb_18)
summary(m4)


## ---- include=FALSE------------------------------------------------------
op <- options(width = 40)


## ------------------------------------------------------------------------
mean(ds_vb_18$reasoning)
ds_vb_18 %>% group_by(order) %>%
  summarise(m = mean(reasoning))
ds_vb_18 %>% group_by(order) %>%
 summarise(m=mean(reasoning)) %>% 
 summarise(mean(m))



## ---- include=FALSE------------------------------------------------------
options(op)


## ------------------------------------------------------------------------
afex::set_default_contrasts() # or set_treatment_contrasts()


## ---- include=FALSE------------------------------------------------------
op <- options(width = 70)


## ------------------------------------------------------------------------
m5 <- lm(reasoning ~ order*training, ds_vb_18)
coef(m5)


## ------------------------------------------------------------------------
ds_vb_18 %>% 
  group_by(order,training) %>%
  summarise(mean(reasoning))


## ---- include=FALSE------------------------------------------------------
options(op)


## ------------------------------------------------------------------------
afex::set_sum_contrasts() # or set_effects_contrasts() or set_deviation_contrasts()


## ---- include=FALSE------------------------------------------------------
op <- options(width = 70)


## ------------------------------------------------------------------------
m6 <- lm(reasoning ~ order*training, ds_vb_18)
coef(m6)


## ------------------------------------------------------------------------
ds_vb_18 %>% 
 group_by(order,training) %>%
 summarise(m=mean(reasoning)) %>% 
 ungroup() %>% 
 summarise(mean(m))


## ---- include=FALSE------------------------------------------------------
options(op)


## ---- eval=FALSE---------------------------------------------------------
## reasoning ~ binding + updating   # a: 3
## reasoning ~ binding : updating,  # b: 2
## reasoning ~ 0 + binding:updating # c: 1
## reasoning ~ binding*updating     # d: 4
## reasoning ~ 0+binding*updating   # e: 3
## 
## reasoning ~ binding              # f: 2
## reasoning ~ 0 + binding          # g: 1


## ---- eval=FALSE---------------------------------------------------------
## lm(reasoning ~ order, ds_vb_18)               # a
## lm(reasoning ~ 0+order, ds_vb_18)             # b
## lm(reasoning ~ order+training, ds_vb_18)      # c
## lm(reasoning ~ 0+order+training, ds_vb_18)    # d
## lm(reasoning ~ order:training, ds_vb_18)      # e
## lm(reasoning ~ 0+order:training, ds_vb_18)    # f
## lm(reasoning ~ order*training, ds_vb_18)      # g
## lm(reasoning ~ 0+order*training, ds_vb_18)    # h
## lm(reasoning ~ order+order:training, ds_vb_18)# i


## ------------------------------------------------------------------------
levels(ds_vb_18$order)
levels(ds_vb_18$training)


## ------------------------------------------------------------------------
coef(lm(reasoning ~ order, ds_vb_18))                 # a: 2
coef(lm(reasoning ~ 0+order, ds_vb_18))               # b: 2
coef(lm(reasoning ~ order+training, ds_vb_18))        # c: 4
coef(lm(reasoning ~ 0+order+training, ds_vb_18))      # d: 4


## ------------------------------------------------------------------------
coef(lm(reasoning ~ order:training, ds_vb_18))        # e: 7
coef(lm(reasoning ~ 0+order:training, ds_vb_18))      # f: 6


## ---- eval = FALSE-------------------------------------------------------
## coef(lm(reasoning ~ order*training, ds_vb_18))        # g: 6
## coef(lm(reasoning ~ 0+order*training, ds_vb_18))      # h: 6
## coef(lm(reasoning ~ order+order:training, ds_vb_18))  # i: 6


## ---- include=FALSE------------------------------------------------------
op <- options(width = 70)


## ---- message=FALSE------------------------------------------------------
afex::set_sum_contrasts()
m6 <- lm(reasoning ~ order*training, ds_vb_18)
summary(m6)


## ------------------------------------------------------------------------
ds_vb_18 %>% 
 group_by(order, training) %>%
 summarise(m=mean(reasoning)) %>% 
 ungroup() %>% 
 summarise(mean(m))


## ---- include=FALSE------------------------------------------------------
options(op)


## ---- message=FALSE------------------------------------------------------
require(car) # Companion to Applied Regression (Fox & Weisberg, 2011)
Anova(m6, type = 3)


## ---- message=FALSE, warning=FALSE---------------------------------------
library("emmeans")    
(emms <- emmeans(m6, "training"))


## ---- message=FALSE------------------------------------------------------
pairs(emms, adjust='holm')


## ---- message=FALSE, warning=FALSE---------------------------------------
library("emmeans")  
(emms <- emmeans(m6, "training")) 


## ---- message=FALSE------------------------------------------------------
cs <- list(
  "c-u" = c(1, -1, 0),
  "c-wm" = c(1, -0.5, -0.5),
  "c-uub" = c(1, -0.75, -0.25)
)
contrast(emms, cs, adjust = "holm")


## ---- message=FALSE, comment='#'-----------------------------------------
library("afex")
(a1 <- aov_car(reasoning ~ order + Error(code), 
               ds_vb_18))


## ------------------------------------------------------------------------
(a2 <- aov_car(reasoning ~ order*training + 
                 Error(code), ds_vb_18))


## ------------------------------------------------------------------------
(a2 <- aov_car(reasoning ~ order*training +
                 Error(code), ds_vb_18))


## ---- fig.width=4.5, fig.height=4.5, dev='svg'---------------------------
afex_plot(a2, "training", "order",
          data_geom = geom_violin, 
          data_arg = list(width = 0.4)) +
  theme(legend.position = "bottom")



## ------------------------------------------------------------------------
emmeans(a2, "order")


## ------------------------------------------------------------------------

emmeans(a2, c("training")) %>% pairs


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
  facet_wrap("Worker") + 
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
emmeans(a1, "Machine", adjust = "holm") %>% 
  pairs


## ------------------------------------------------------------------------
emmeans(mmach, "Machine", adjust = "holm") %>% 
  pairs ## complete pooling results

