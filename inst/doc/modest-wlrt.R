## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

run <- if (rlang::is_installed(c("dplyr"))) TRUE else FALSE
knitr::opts_chunk$set(eval = run)

## ----message=FALSE, warning=FALSE---------------------------------------------
library(simtrial)
library(dplyr)
library(survival)

## ----message=FALSE, warning=FALSE---------------------------------------------
study_duration <- 36
sample_size <- 300
enroll_rate <- data.frame(duration = 12, rate = 200 / 12)
fail_rate <- data.frame(
  stratum = c("All", "All"),
  duration = c(6, 36),
  fail_rate = c(log(2) / 15, log(2) / 15),
  hr = c(1, .7),
  dropout_rate = c(0, 0)
)

## ----message=FALSE, warning=FALSE, fig.width=7.5, fig.height=4----------------
set.seed(7789)
xpar <- to_sim_pw_surv(fail_rate)
MBdelay <- sim_pw_surv(
  n = sample_size,
  stratum = data.frame(stratum = "All", p = 1),
  block = c(rep("control", 2), rep("experimental", 2)),
  enroll_rate = enroll_rate,
  fail_rate = xpar$fail_rate,
  dropout_rate = xpar$dropout_rate
) |>
  cut_data_by_date(study_duration)
fit <- survfit(Surv(tte, event) ~ treatment, data = MBdelay)
plot(fit, col = 1:2, mark = "|", xaxt = "n")
axis(1, xaxp = c(0, 36, 6))

## -----------------------------------------------------------------------------
ZMB <- MBdelay |>
  wlr(weight = mb(delay = 6))
# Compute p-value of modestly weighted logrank of Magirr-Burman
pnorm(ZMB$z)

## -----------------------------------------------------------------------------
ZMB <- MBdelay |>
  wlr(weight = mb(delay = Inf, w_max = 2))
# Compute p-value of modestly weighted logrank of Magirr-Burman
pnorm(ZMB$z)

## -----------------------------------------------------------------------------
w_max <- 2
Z_modified_FH <- MBdelay |>
  counting_process(arm = "experimental") |>
  mutate(w = pmin(w_max, 1 / s)) |>
  summarize(
    S = sum(o_minus_e * w),
    V = sum(var_o_minus_e * w^2),
    z = S / sqrt(V)
  )
# Compute p-value of modestly weighted logrank of Magirr-Burman
pnorm(Z_modified_FH$z)

## ----message=FALSE, warning=FALSE---------------------------------------------
study_duration <- 5
sample_size <- 2000
enroll_duration <- .0001
enroll_rate <- data.frame(
  duration = enroll_duration,
  rate = sample_size / enroll_duration
)
fail_rate <- data.frame(
  stratum = "All",
  fail_rate = 0.25,
  dropout_rate = 0,
  hr = c(4 / .25, .19 / .25),
  duration = c(.1, 4.9)
)

## ----message=FALSE, warning=FALSE, fig.width=7.5, fig.height=4----------------
set.seed(7783)
xpar <- to_sim_pw_surv(fail_rate)
FHwn <- sim_pw_surv(
  n = sample_size,
  stratum = data.frame(stratum = "All", p = 1),
  block = c(rep("control", 2), rep("experimental", 2)),
  enroll_rate = enroll_rate,
  fail_rate = xpar$fail_rate,
  dropout_rate = xpar$dropout_rate
) |>
  cut_data_by_date(study_duration)
fit <- survfit(Surv(tte, event) ~ treatment, data = FHwn)
plot(fit, col = 1:2, mark = "|", xaxt = "n")
axis(1, xaxp = c(0, 36, 6))

## -----------------------------------------------------------------------------
xx <- FHwn |>
  maxcombo(rho = c(0, 0, 1), gamma = c(0, 1, 1))
xx

## -----------------------------------------------------------------------------
ZMB <- FHwn |>
  wlr(weight = mb(delay = 6, w_max = 2))

# Compute p-value of modestly weighted logrank of Magirr-Burman
pnorm(ZMB$z)

## -----------------------------------------------------------------------------
xx <- FHwn |>
  maxcombo(rho = c(0, 0, .5), gamma = c(0, .5, .5))
xx

