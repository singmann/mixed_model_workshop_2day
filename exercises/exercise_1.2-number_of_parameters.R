## ----setup, include=FALSE------------------------------------------------
load(url("http://singmann.org/download/r/ds_vb_18.rda"))


## ---- eval=FALSE---------------------------------------------------------
## lm(reasoning ~ binding + updating, ds_vb_18)   # a
## lm(reasoning ~ binding : updating, ds_vb_18)   # b
## lm(reasoning ~ 0 + binding:updating, ds_vb_18) # c
## lm(reasoning ~ binding*updating, ds_vb_18)     # d
## lm(reasoning ~ 0+binding*updating, ds_vb_18)   # e


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
levels(ds_vb_18$order)    ## 2
levels(ds_vb_18$training) ## 3

