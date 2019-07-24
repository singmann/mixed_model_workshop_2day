## ---- message=FALSE------------------------------------------------------
library("tidyverse")
library("cowplot")
theme_set(theme_bw(base_size = 12) + 
            theme(legend.position="bottom", 
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank()))
w <- read_csv("weather_wb.csv")


## ------------------------------------------------------------------------
w %>% 
  group_by(id) %>% 
  summarise(n = n()) %>% 
  psych::describe()


## ------------------------------------------------------------------------
w %>% 
  group_by(id) %>% 
  summarise(mean_sunh = mean(sunh),
            mean_swb = mean(swb)) %>% 
  select(-id) %>% 
  psych::describe()


## ------------------------------------------------------------------------
# ...


## ------------------------------------------------------------------------
# ...


## ---- message=FALSE------------------------------------------------------
library("lme4")
# ...

