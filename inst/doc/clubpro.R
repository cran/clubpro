## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(clubpro)

## ----set_palette--------------------------------------------------------------
palette(c("#0073C2", "#EFC000", "#868686"))

## ----model_jellyfish----------------------------------------------------------
mod <- club(width ~ location, data = jellyfish)

## ----pcc_jellyfish------------------------------------------------------------
pcc(mod)

## ----cval_jellyfish-----------------------------------------------------------
cval(mod)

## ----summary_jelyfish---------------------------------------------------------
summary(mod)

## ----plot_jellyfish, fig.width=8, fig.height=5--------------------------------
plot(mod)

## ----compute_threshold--------------------------------------------------------
threshold(mod)

## ----plot_theshold, fig.width=8, fig.height=5---------------------------------
plot(threshold(mod))

## ----plot_csi, fig.width=6, fig.height=8--------------------------------------
mod_csi <- csi(mod)
plot(mod_csi)

## ----predict_jellyfish--------------------------------------------------------
predict(mod)

## ----plot_predictions, fig.width=8, fig.height=5------------------------------
plot(predict(mod))

## ----accuracy_jellyfish-------------------------------------------------------
accuracy(mod)

## ----plot_accuracy, fig.width=8, fig.height=5---------------------------------
plot(accuracy(mod))

## ----plot_cval_dist, fig.width=8, fig.height=5--------------------------------
plot(pcc_replicates(mod))

