## ---- message=FALSE------------------------------------------------------
library("tidyverse")
library("viridis")
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
  do(psych::describe(.$n))


## ------------------------------------------------------------------------
w %>% 
  group_by(id) %>% 
  summarise(mean_sunh = mean(sunh),
            mean_swb = mean(swb)) %>% 
  select(-id) %>% 
  do(psych::describe(.))


## ---- fig.width=4, fig.height=4, dpi=125---------------------------------
## Rmarkdown settings: fig.width=4, fig.height=4, dpi=125
ggplot(w, aes(x = sunh, y = swb, color = id)) +
  geom_point(alpha = 0.6, position = position_jitter()) +
  scale_color_viridis()


## ---- fig.width=2, fig.height=2.5, dpi=150-------------------------------
no_all <- w %>% 
  group_by(id) %>% 
  do(broom::tidy(lm(swb ~ sunh, .)) ) 

no_slope <- no_all %>% 
  filter(term == "sunh")
ggplot(no_slope, aes(estimate)) +
  geom_histogram(bins = 20) +
  geom_vline(aes(xintercept = mean(estimate)), col='red', size=2)



## ------------------------------------------------------------------------
summary(lm(estimate ~ 1, no_slope))


## ------------------------------------------------------------------------
mean(no_slope$estimate > 0)


## ------------------------------------------------------------------------
psych::describe(no_slope$estimate)


## ---- fig.width=4, fig.height=4, dpi=125---------------------------------
no_plot <- no_all %>% 
  select(id:estimate) %>% 
  spread(term, estimate)

ggplot(w, aes(x = sunh, y = swb, color = id)) +
  geom_point(alpha = 0.8, position = position_jitter()) +
  geom_abline(data = no_plot, aes(slope = sunh, intercept = `(Intercept)`, color = id), alpha = 0.3) +
  scale_color_viridis()


## ---- fig.width=4, fig.height=4, dpi=125---------------------------------
no_plot <- no_all %>% 
  select(id:estimate) %>% 
  spread(term, estimate)

ggplot(w, aes(x = sunh, y = swb, color = id)) +
  geom_abline(data = no_plot, aes(slope = sunh, 
                                  intercept = `(Intercept)`, 
                                  color = id), alpha = 0.8) +
  scale_color_viridis() +
  xlim(0, 12) + ylim(0, 10) +
  theme(legend.position = "none")



## ------------------------------------------------------------------------
cp1 <- lm(swb ~ sunh, w)
summary(cp1)


## ---- fig.width=4, fig.height=3.5, dpi=125-------------------------------
ggplot(w, aes(x = sunh, y = swb)) +
  geom_point(alpha = 0.6, position = position_jitter()) +
  geom_abline(intercept = coef(cp1)[1], slope = coef(cp1)[2])


## ------------------------------------------------------------------------
w_agg <- w %>% 
  group_by(id) %>% 
  summarise(swb = mean(swb),
            sunh = mean(sunh))
cp2 <- lm(swb ~ sunh, w_agg)
summary(cp2)


## ---- fig.width=3, fig.height=3, dpi=125---------------------------------
ggplot(w_agg, aes(x = sunh, y = swb)) +
  geom_point() +
  geom_abline(intercept = coef(cp2)[1], slope = coef(cp2)[2])


## ---- message=FALSE------------------------------------------------------
library("lme4")
mm1 <- lmer(swb ~ sunh + (sunh|id), w)
summary(mm1)



## ---- fig.width=4, fig.height=3.5, dpi=125-------------------------------
rnd_coefs <- as_tibble(coef(mm1)$id)

ggplot(data = w, aes(x = sunh, y = swb)) + 
  geom_point(alpha = 0.2, pch = 16, size = 3) + 
  geom_abline(data = rnd_coefs, aes_string(intercept = "`(Intercept)`", 
                                     slope = "sunh"), 
              color = "lightgrey", size = 1.2) +
  geom_abline(intercept = fixef(mm1)[1], slope = fixef(mm1)[2], size = 1.5)



## ---- eval=FALSE---------------------------------------------------------
## library("tidyverse")
## library("psych")
## 
## set.seed(66)
## 
## total_N <- 60                 ## Number of participants
## 
## N <- 45                       ## number of observations per participant
## R <- 0.2                      ## mean correlation in population
## Rz <- fisherz(R)
## 
## indiv_R <- fisherz2r(rnorm(total_N, Rz, 0.07))
## 
## mus_sunh <- runif(total_N, 1, 9)
## mus_swb <- rnorm(total_N, 7, 1)
## 
## SD <- c(2, 1)              ## population SD
## 
## ## function only works for bivariate data
## gen_correlated_vars <- function(N, R, MU, SD, empirical = FALSE) {
##   cor_mat <- matrix(c(1,R,R,1), ncol = 2)
##   cov_mat <- cor_mat * tcrossprod(SD)
##   ## 'MASS::mvrnorm' uses function mvrnorm from package MASS:
##   d <- MASS::mvrnorm(n = N,
##                      mu = MU,
##                      Sigma = cov_mat,
##                      empirical = empirical) ## TRUE ensures correlation is true in sample
##   colnames(d) <- c("swb", "sunh")
##   d <- as_tibble(d)
##   d$swb <- round(d$swb/0.25)*0.25
##   d$swb <- ifelse(d$swb > 10, 10,
##                   ifelse(d$swb < 0, 0, d$swb))
##   d$sunh <- round(d$sunh, 1)
##   return(d)
## }
## 
## 
## dl <- vector("list", total_N)
## 
## for (i in seq_len(total_N)) {
##   dl[[i]] <- gen_correlated_vars(N = N, R = indiv_R[i],
##                                  MU = c(mus_swb[i], mus_sunh[i]), SD = SD)
##   dl[[i]]$id <- i
## }
## 
## d <- bind_rows(dl)
## d <- d %>%
##   mutate(sunh = ifelse(sunh < 0, 0, sunh))
## 
## cor(d$swb, d$sunh)
## 
## GGally::ggpairs(d, columns = 1:2, diag = list(continuous = "barDiag"))
## 
## ggplot(d, aes(swb)) +
##   geom_bar()
## 
## d %>%
##   select(id, sunh, swb) %>%
##   write_csv("weather_wb.csv")
## 

