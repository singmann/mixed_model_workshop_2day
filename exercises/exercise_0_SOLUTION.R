## ---- message=FALSE------------------------------------------------------
library("tidyverse")
data("fhch2010", package = "afex") # load 
fhch <- droplevels(fhch2010[ fhch2010$correct,]) # remove errors
str(fhch) # structure of the data
library("tidyverse")

## ------------------------------------------------------------------------
fhch %>% 
  group_by(task) %>% 
  summarise(m = mean(rt))

## ------------------------------------------------------------------------
fhch %>% 
  group_by(task) %>% 
  summarise(m = mean(rt),
            sd = sd(rt))

## ------------------------------------------------------------------------
fhch %>% 
  filter(rt > 0.25, rt < 2.5) %>% 
  group_by(task) %>% 
  summarise(m = mean(rt),
            sd = sd(rt))


## ------------------------------------------------------------------------
agg1 <- fhch %>% 
  filter(task == "lexdec") %>% 
  group_by(id, length) %>% 
  summarise(mrt = mean(rt))

## ------------------------------------------------------------------------
ggplot(agg1, aes(x = length, y = mrt)) +
  geom_jitter()

## ------------------------------------------------------------------------
ggplot(agg1, aes(x = length, y = mrt)) +
  geom_point(alpha = 0.2) +
  geom_violin(fill = "transparent") +
  stat_summary(color = "red") +
  theme_bw()

## ------------------------------------------------------------------------
agg2 <- fhch %>% 
  filter(task == "lexdec") %>% 
  group_by(id, length, density) %>% 
  summarise(mrt = mean(rt))
ggplot(agg2, aes(x = length, y = mrt, color = density, group = density)) +
  geom_point(position = position_dodge(0.25)) +
  stat_summary(position = position_dodge(0.25))


## ------------------------------------------------------------------------
ggplot(agg2, aes(x = length, y = mrt, color = density, group = density)) +
  geom_point(position = position_dodge(0.25), alpha = 0.5) +
  stat_summary(position = position_dodge(0.25)) +
  theme_light()


