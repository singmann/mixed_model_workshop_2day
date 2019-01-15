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
## 1. full model shows a singular fit:
m_max <-  mixed(correct ~ fluency*source + (1|uID) + (fluency*source|syllogism), d, 
                method = "S", progress = FALSE) ## suppress output
summary(m_max)$varcor
m_max

## ------------------------------------------------------------------------
## r1. full model without correlations also shows singular fit
m_r1 <-  mixed(correct ~ fluency*source + (1|uID) + (fluency*source||syllogism), d, 
               method = "S", expand_re = TRUE, progress = FALSE)
summary(m_r1)$varcor

## r2. model without interaction still shows singular fit 
m_r2 <-  mixed(correct ~ fluency*source + (1|uID) + (fluency+source||syllogism), d, 
               method = "S", expand_re = TRUE, progress = FALSE)
summary(m_r2)$varcor

## r3. model with random slopes only for source still shows singular fit 
m_r3 <-  mixed(correct ~ fluency*source + (1|uID) + (source||syllogism), d, 
               method = "S", expand_re = TRUE, progress = FALSE)
summary(m_r3)$varcor

## r4. model with random slopes only for fluency still shows singular fit 
m_r4 <-  mixed(correct ~ fluency*source + (1|uID) + (fluency||syllogism), d, 
               method = "S", expand_re = TRUE, progress = FALSE)
summary(m_r4)$varcor

## r5. random intercept onloy model does not show singular fit
m_r5 <-  mixed(correct ~ fluency*source + (1|uID) + (1|syllogism), d, 
               method = "S", progress = FALSE)
summary(m_r5)$varcor

## ------------------------------------------------------------------------
## full model shows significant interaction
m_max

# minimal model shows significant interaction
m_r5

# models in between also shows significant interaction
m_r4

## ------------------------------------------------------------------------
emmeans(m_r5, "fluency", by = "source")

pairs(emmeans(m_r5, "fluency", by = "source"))

## results very similar for full model
pairs(emmeans(m_max, "fluency", by = "source"))


## ---- fig.width=6, fig.height=3, dev='svg'-------------------------------
## something is wrong here:
afex_plot(m_r5, "source", "fluency")

## Maybe we can try with aggregating for each participant:
afex_plot(m_r5, "source", "fluency", random = "uID")

## For this plot, reordering the x-axis might be a good idea
#   and we might change a few more settings for data in bg
#   also, error bars look to wide given our knowledge of effect
afex_plot(m_r5, "source", "fluency", random = "uID", error = "mean",
          data_geom = ggbeeswarm::geom_beeswarm, 
          data_alpha = 0.15,
          data_arg = list(cex = 0.2, dodge.width = 0.5, color = "black")) + 
  scale_x_discrete(limits=c("tilburgcaf", "purkyne", "uniporto"))

## ---- fig.width=6, fig.height=3, dev='svg'-------------------------------
## Maybe not different lines, but different panels? 
afex_plot(m_r5, panel = "source", x = "fluency", random = "uID", error = "mean",
          data_alpha = 0.15,
          data_arg = list(cex = 0.8, dodge.width = 0.5, color = "black"))

## ------------------------------------------------------------------------
# m5_0 <-  mixed(correct ~ fluency*source*syllogism + (syllogism|uID), d, 
#                method = "S")
## cannot be estimated, as there are no replicates for participant:syllogism

m5 <-  mixed(correct ~ fluency*source*syllogism + (1|uID), d, 
               method = "S", progress = FALSE)
m5 ## important: no interaction of syllogism with fluency

emmeans(m5, "fluency", by = "source")

pairs(emmeans(m5, "fluency", by = "source"))


## ---- message=FALSE------------------------------------------------------

a6 <- aov_car(correct ~ fluency*source*syllogism + Error(uID/syllogism), d)
a6

## Compared to mixed model, ANOVA cannot handle partially missing data. 
## Those observations have to be excluded from the analysis.

pairs(emmeans(a6, "fluency", by = "source"))

## ---- fig.width=5, fig.height=3, dev='svg'-------------------------------
afex_plot(m5, "source", "fluency", random = "uID", 
          data_geom = ggbeeswarm::geom_beeswarm, 
          data_alpha = 0.15, dodge = 0.6,
          data_arg = list(cex = 0.3, dodge.width = 0.6, color = "black")) + 
  scale_x_discrete(limits=c("tilburgcaf", "purkyne", "uniporto"))


