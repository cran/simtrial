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
randomize_by_fixed_block(n = 10, block = c("A", "Dog", "Cat", "Cat"))

## -----------------------------------------------------------------------------
randomize_by_fixed_block(n = 20)

## -----------------------------------------------------------------------------
rpwexp_enroll(
  n = 20,
  enroll_rate = data.frame(
    duration = c(1, 2),
    rate = c(2, 5)
  )
)

## ----fig.width=6--------------------------------------------------------------
x <- rpwexp(
  10000,
  fail_rate = data.frame(
    rate = c(1, 3, 10),
    duration = c(.5, .5, 1)
  )
)
plot(
  sort(x),
  (10000:1) / 10001,
  log = "y",
  main = "PW Exponential simulated survival curve",
  xlab = "Time", ylab = "P{Survival}"
)

## -----------------------------------------------------------------------------
stratum <- data.frame(stratum = c("Negative", "Positive"), p = c(.5, .5))

block <- c(rep("control", 2), rep("experimental", 2))

enroll_rate <- data.frame(rate = c(3, 6, 9), duration = c(3, 2, 1))

fail_rate <- data.frame(
  stratum = c(rep("Negative", 4), rep("Positive", 4)),
  period = rep(1:2, 4),
  treatment = rep(c(rep("control", 2), rep("experimental", 2)), 2),
  duration = rep(c(3, 1), 4),
  rate = log(2) / c(4, 9, 4.5, 10, 4, 9, 8, 18)
)
dropout_rate <- data.frame(
  stratum = c(rep("Negative", 4), rep("Positive", 4)),
  period = rep(1:2, 4),
  treatment = rep(c(rep("control", 2), rep("experimental", 2)), 2),
  duration = rep(c(3, 1), 4),
  rate = rep(c(.001, .001), 4)
)

## -----------------------------------------------------------------------------
x <- sim_pw_surv(
  n = 400,
  stratum = stratum,
  block = block,
  enroll_rate = enroll_rate,
  fail_rate = fail_rate,
  dropout_rate = dropout_rate
)
head(x) |> kable(digits = 2)

## -----------------------------------------------------------------------------
y <- cut_data_by_date(x, cut_date = 5)

head(y) |> kable(digits = 2)

## -----------------------------------------------------------------------------
cut50Positive <- get_cut_date_by_event(filter(x, stratum == "Positive"), 50)
y50Positive <- cut_data_by_date(x, cut50Positive)

with(y50Positive, table(stratum, event))

## -----------------------------------------------------------------------------
y150 <- cut_data_by_event(x, 150)
table(y150$event, y150$treatment)

## -----------------------------------------------------------------------------
ten150 <- counting_process(y150, arm = "experimental")

head(ten150) |> kable(digits = 2)

## -----------------------------------------------------------------------------
z <- with(ten150, sum(o_minus_e) / sqrt(sum(var_o_minus_e)))
c(z, pnorm(z))

## -----------------------------------------------------------------------------
xx <- mutate(ten150, w = s * (1 - s)^2)
z <- with(xx, sum(o_minus_e * w) / sum(sqrt(var_o_minus_e * w^2)))
c(z, pnorm(z))

## -----------------------------------------------------------------------------
fh_weight(
  x = ten150,
  rho_gamma = data.frame(rho = c(0, 0, 1, 1), gamma = c(0, 1, 0, 1))
) |> kable(digits = 2)

## ----message=FALSE------------------------------------------------------------
x <- ten150 |>
  fh_weight(
    rho_gamma = data.frame(rho = c(0, 0, 1, 1), gamma = c(0, 1, 0, 1)),
    return_corr = TRUE
  )
x |> kable(digits = 2)

## ----message=FALSE------------------------------------------------------------
# Compute p-value for MaxCombo
pvalue_maxcombo(x)

## -----------------------------------------------------------------------------
stratum <- data.frame(stratum = "All", p = 1)
enroll_rate <- data.frame(
  duration = c(2, 2, 10),
  rate = c(3, 6, 9)
)
fail_rate <- data.frame(
  stratum = "All",
  duration = c(3, 100),
  fail_rate = log(2) / c(9, 18),
  hr = c(0.9, 0.6),
  dropout_rate = rep(0.001, 2)
)
block <- rep(c("experimental", "control"), 2)
rho_gamma <- data.frame(rho = 0, gamma = 0)

## -----------------------------------------------------------------------------
sim_fixed_n(
  n_sim = 2, # Number of simulations
  sample_size = 500, # Trial sample size
  target_event = 350, # Targeted events at analysis
  stratum = stratum, # Study stratum
  enroll_rate = enroll_rate, # Enrollment rates
  fail_rate = fail_rate, # Failure rates
  total_duration = 30, # Planned trial duration
  block = block, # Block for treatment
  timing_type = 1:5, # Use all possible data cutoff methods
  rho_gamma = rho_gamma # FH test(s) to use; in this case, logrank
) |> kable(digits = 2)

## -----------------------------------------------------------------------------
enroll_rate |> summarize(
  "Targeted enrollment based on input enrollment rates" = sum(duration * rate)
)

## -----------------------------------------------------------------------------
total_duration <- 30 # From above
total_duration - sum(enroll_rate$duration)

