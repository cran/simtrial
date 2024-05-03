## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

run <- if (rlang::is_installed(c("dplyr", "gt"))) TRUE else FALSE
knitr::opts_chunk$set(eval = run)

## ----message=FALSE, warning=FALSE---------------------------------------------
library(simtrial)
library(knitr)
library(dplyr)
library(gt)

## -----------------------------------------------------------------------------
set.seed(123)

x <- sim_fixed_n(
  n_sim = 1,
  timing_type = 5,
  rho_gamma = data.frame(rho = c(0, 0, 1), gamma = c(0, 1, 1))
)

x |>
  gt() |>
  fmt_number(columns = c("ln_hr", "z", "duration", "v1", "v2", "v3"), decimals = 2)

## ----message=FALSE, warning=FALSE, cache=FALSE--------------------------------
set.seed(123)

s <- sim_pw_surv(n = 100)

s |>
  head() |>
  gt() |>
  fmt_number(columns = c("enroll_time", "fail_time", "dropout_time", "cte"), decimals = 2)

## ----warning=FALSE, message=FALSE---------------------------------------------
x <- s |> cut_data_by_event(75)

x |>
  head() |>
  gt() |>
  fmt_number(columns = "tte", decimals = 2)

## ----warning=FALSE, message=FALSE---------------------------------------------
z <- s |>
  cut_data_by_event(75) |>
  maxcombo(rho = c(0, 0, 1, 1), gamma = c(0, 1, 0, 1))

z

## ----warning=FALSE, message=FALSE---------------------------------------------
z <- s |>
  cut_data_by_event(75) |>
  maxcombo(rho = c(0, 1), gamma = c(1, 0))

z

## ----warning=FALSE, message=FALSE---------------------------------------------
library(survival)
aml |>
  head() |>
  gt()

## ----warning=FALSE, message=FALSE---------------------------------------------
x <- aml |> transmute(
  tte = time,
  event = status,
  stratum = "All",
  treatment = case_when(
    x == "Maintained" ~ "experimental",
    x == "Nonmaintained" ~ "control"
  )
)

x |>
  head() |>
  gt()

## ----warning=FALSE, message=FALSE---------------------------------------------
x |> maxcombo(rho = c(0, 0), gamma = c(0, 1))

## ----cache=FALSE, warning=FALSE, message=FALSE--------------------------------
set.seed(123)

# Only use cut events + min follow-up
x <- sim_fixed_n(
  n_sim = 100,
  timing_type = 5,
  rho_gamma = data.frame(rho = c(0, 0, 1), gamma = c(0, 1, 1))
)

# MaxCombo power estimate for cutoff at max of targeted events, minimum follow-up
x |>
  group_by(sim) |>
  filter(row_number() == 1) |>
  ungroup() |>
  summarize(power = mean(p_value < .001))

## ----cache=FALSE, warning=FALSE, message=FALSE--------------------------------
# Only use cuts for events and events + min follow-up
set.seed(123)

x <- sim_fixed_n(
  n_sim = 100,
  timing_type = c(2, 5),
  rho_gamma = data.frame(rho = 0, gamma = c(0, 1))
)

## ----warning=FALSE, message=FALSE---------------------------------------------
# Subset to targeted events cutoff tests
# This chunk will be updated after the development of sim_gs_n and sim_fixed_n
x |>
  filter(cut == "Targeted events") |>
  group_by(sim) |>
  filter(row_number() == 1) |>
  ungroup() |>
  summarize(power = mean(p_value < .025))

## ----warning=FALSE, message=FALSE---------------------------------------------
# Subset to targeted events cutoff tests
x |>
  filter(cut != "Targeted events") |>
  group_by(sim) |>
  filter(row_number() == 1) |>
  ungroup() |>
  summarize(power = mean(p_value < .025))

