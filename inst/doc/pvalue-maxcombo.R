## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message=FALSE, warning=FALSE---------------------------------------------
library(simtrial)
library(knitr)
library(dplyr)

## -----------------------------------------------------------------------------
x <- sim_fixed_n(
  n_sim = 1,
  timing_type = 5,
  rho_gamma = data.frame(rho = c(0, 0, 1), gamma = c(0, 1, 1))
)
x |> kable(digits = 2)

## ----warning=FALSE, message=FALSE---------------------------------------------
pvalue_maxcombo(x)

## ----message=FALSE, warning=FALSE, cache=FALSE--------------------------------
s <- sim_pw_surv(n = 100)
head(s) |> kable(digits = 2)

## ----warning=FALSE, message=FALSE---------------------------------------------
x <- s |> cut_data_by_event(75)
head(x) |> kable(digits = 2)

## ----warning=FALSE, message=FALSE---------------------------------------------
z <- s |>
  cut_data_by_event(75) |>
  counting_process(arm = "experimental") |>
  fh_weight(
    rho_gamma = data.frame(rho = c(0, 0, 1, 1), gamma = c(0, 1, 0, 1)),
    return_corr = TRUE
  )
z |> kable(digits = 2)

## ----warning=FALSE, message=FALSE---------------------------------------------
pvalue_maxcombo(z)

## ----warning=FALSE, message=FALSE---------------------------------------------
pvalue_maxcombo(
  z |>
    select(-c(v1, v4)) |>
    filter((rho == 0 & gamma == 1) | (rho == 1 & gamma == 0))
)

## ----warning=FALSE, message=FALSE---------------------------------------------
library(survival)
head(aml) |> kable()

## ----warning=FALSE, message=FALSE---------------------------------------------
x <- aml |> transmute(
  tte = time,
  event = status,
  stratum = "All",
  treatment = as.character(x)
)
head(x) |> kable()

## ----warning=FALSE, message=FALSE---------------------------------------------
x |>
  counting_process(arm = "Maintained") |>
  fh_weight(
    rho_gamma = data.frame(rho = 0, gamma = c(0, 1)),
    return_corr = TRUE
  ) |>
  pvalue_maxcombo()

## ----cache=FALSE, warning=FALSE, message=FALSE--------------------------------
# Only use cut events + min follow-up
xx <- sim_fixed_n(
  n_sim = 100,
  timing_type = 5,
  rho_gamma = data.frame(rho = c(0, 0, 1), gamma = c(0, 1, 1))
)
# MaxCombo power estimate for cutoff at max of targeted events, minimum follow-up
p <- xx |>
  group_by(sim) |>
  group_map(~ pvalue_maxcombo(.x)) |>
  unlist()
mean(p < .001)

## ----cache=FALSE, warning=FALSE, message=FALSE--------------------------------
# Only use cuts for events and events + min follow-up
xx <- sim_fixed_n(
  n_sim = 100,
  timing_type = c(2, 5),
  rho_gamma = data.frame(rho = 0, gamma = c(0, 1))
)
head(xx) |> kable(digits = 2)

## ----warning=FALSE, message=FALSE---------------------------------------------
# Subset to targeted events cutoff tests
p <- xx |>
  filter(cut == "Targeted events") |>
  group_by(sim) |>
  group_map(~ pvalue_maxcombo(.x)) |>
  unlist()
mean(p < .025)

## ----warning=FALSE, message=FALSE---------------------------------------------
# Subset to targeted events cutoff tests
p <- xx |>
  filter(cut != "Targeted events") |>
  group_by(sim) |>
  group_map(~ pvalue_maxcombo(.x)) |>
  unlist()
mean(p < .025)

