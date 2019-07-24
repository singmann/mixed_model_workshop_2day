## ---- include=FALSE------------------------------------------------------
options(dplyr.show_progress = FALSE)


## ---- message=FALSE, warning=FALSE---------------------------------------
library("tidyverse")
theme_set(theme_bw())
library("broom") # not automatically loaded


## ---- message=FALSE------------------------------------------------------
# Run complete chunk: Ctrl+Shift+Enter

# You might need to set the correct working directory via the menu: 
# Session -> Set Working Directory -> To Source File Location

afex::set_sum_contrasts() # just in case we set orthogonal contrasts

load("ssk16_dat_prepared_ex1.rda") # data preapred in 'prepare_data.R'
str(dat1)



## ------------------------------------------------------------------------
m0 <- lm(if_A_then_B ~ B_given_A, dat1)
summary(m0)


## ---- fig.width=7, fig.height=3------------------------------------------
ggplot(data = dat1) + 
  geom_point(mapping = aes(x = B_given_A, y = if_A_then_B), alpha = 0.2, pch = 16) + 
  coord_fixed()


## ------------------------------------------------------------------------

ex1_no_pooling_estimates <- dat1 %>% 
  group_by(p_id) %>% 
  do(tidy(lm(if_A_then_B ~ B_given_A, .)))
head(ex1_no_pooling_estimates)



## ------------------------------------------------------------------------
ex1_slopes <- ex1_no_pooling_estimates %>% 
  filter(term == "B_given_A")

ggplot(ex1_slopes, aes(estimate)) +
  geom_histogram(bins = 30) 


## ------------------------------------------------------------------------
summary(lm(estimate ~ 1, ex1_slopes))


## ------------------------------------------------------------------------
load("ssk16_dat_prepared_ex2.rda")
str(dat2)


## ------------------------------------------------------------------------
afex::set_sum_contrasts()
library("emmeans")

ex2_comp_1 <- lm(if_A_then_B ~ B_given_A * rel_cond, dat2)
car::Anova(ex2_comp_1, type = 3)

emtrends(ex2_comp_1, "rel_cond", var = "B_given_A")



## ------------------------------------------------------------------------

ex2_comp_2 <- dat2 %>% 
  group_by(rel_cond, p_id) %>% 
  summarise_if(is.numeric, mean) %>% 
  lm(if_A_then_B ~ B_given_A * rel_cond, .)
car::Anova(ex2_comp_2, type = 3)

emtrends(ex2_comp_2, "rel_cond", var = "B_given_A")



## ------------------------------------------------------------------------

ex2_comp_3 <- dat2 %>% 
  group_by(rel_cond, i_id) %>% 
  summarise_if(is.numeric, mean) %>% 
  lm(if_A_then_B ~ B_given_A * rel_cond, .)
car::Anova(ex2_comp_3, type = 3)

emtrends(ex2_comp_3, "rel_cond", var = "B_given_A")



## ---- message=FALSE------------------------------------------------------
ex2_no_pooling_estimates <- dat2 %>% 
  group_by(p_id, rel_cond) %>% 
  do(tidy(lm(if_A_then_B ~ B_given_A, .)))
head(ex2_no_pooling_estimates)


## ---- message=FALSE------------------------------------------------------
ex2_slopes <- ex2_no_pooling_estimates %>% 
  filter(term == "B_given_A")

ggplot(ex2_slopes, aes(estimate)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ rel_cond)


## ---- message=FALSE------------------------------------------------------
library("afex")

(a1 <- aov_car(estimate ~ rel_cond + Error(p_id/rel_cond), ex2_slopes))



## ------------------------------------------------------------------------
emmeans::emmeans(a1, "rel_cond")

