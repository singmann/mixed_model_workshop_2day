## ------------------------------------------------------------------------
load("ssk16_dat_prepared_ex2.rda")
str(dat2)
library("lme4")

## ------------------------------------------------------------------------
m1 <- lmer(if_A_then_B_c ~ B_given_A_c + (0+B_given_A_c|p_id), dat2)
m2 <- lmer(if_A_then_B_c ~ B_given_A_c + (B_given_A_c|p_id), dat2)
m3 <- lmer(if_A_then_B_c ~ B_given_A_c + (1|p_id), dat2)

## ------------------------------------------------------------------------
str(summary(m2), 1)

## ------------------------------------------------------------------------
summary(m1)$optinfo$conv

summary(m2)$optinfo$conv

summary(m3)$optinfo$conv

## ------------------------------------------------------------------------
summary(m1)$varcor

summary(m2)$varcor

summary(m3)$varcor

## ------------------------------------------------------------------------
summary(m1)$coefficients

summary(m2)$coefficients

summary(m3)$coefficients

## ------------------------------------------------------------------------
m4 <- lmer(if_A_then_B_c ~ B_given_A_c + (B_given_A_c||p_id), dat2)
summary(m4)

