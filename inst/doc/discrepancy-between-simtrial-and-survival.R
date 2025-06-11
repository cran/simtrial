## ----message=FALSE------------------------------------------------------------
library(gsDesign)
library(gsDesign2)
library(dplyr)
library(gt)
library(simtrial)
library(tidyr)
library(survival)

## -----------------------------------------------------------------------------
survival_at_24_months <- 0.35
hr <- log(.35) / log(.25)
control_median <- 12
control_rate <- c(log(2) / control_median, (log(.25) - log(.2)) / 12)

scenarios <- tribble(
  ~Scenario, ~Name,           ~Period, ~duration, ~Survival,
  0,         "Control",       0,       0,         1,
  0,         "Control",       1,       24,        .25,
  0,         "Control",       2,       12,        .2,
  1,         "PH",            0,       0,         1,
  1,         "PH",            1,       24,        .35,
  1,         "PH",            2,       12,        .2^hr,
  2,         "3-month delay", 0,       0,         1,
  2,         "3-month delay", 1,       3,         exp(-3 * control_rate[1]),
  2,         "3-month delay", 2,       21,        .35,
  2,         "3-month delay", 3,       12,        .2^hr,
  3,         "6-month delay", 0,       0,         1,
  3,         "6-month delay", 1,       6,         exp(-6 * control_rate[1]),
  3,         "6-month delay", 2,       18,        .35,
  3,         "6-month delay", 3,       12,        .2^hr,
  4,         "Crossing",      0,       0,         1,
  4,         "Crossing",      1,       3,         exp(-3 * control_rate[1] * 1.3),
  4,         "Crossing",      2,       21,        .35,
  4,         "Crossing",      3,       12,        .2^hr,
  5,         "Weak null",     0,       0,         1,
  5,         "Weak null",     1,       24,        .25,
  5,         "Weak null",     2,       12,        .2,
  6,         "Strong null",   0,       0,         1,
  6,         "Strong null",   1,       3,         exp(-3 * control_rate[1] * 1.5),
  6,         "Strong null",   2,       3,         exp(-6 * control_rate[1]),
  6,         "Strong null",   3,       18,        .25,
  6,         "Strong null",   4,       12,        .2,
)
# scenarios |> gt()

## -----------------------------------------------------------------------------
fr <- scenarios |>
  group_by(Scenario) |>
  #  filter(Scenario == 2) |>
  mutate(
    Month = cumsum(duration),
    x_rate = -(log(Survival) - log(lag(Survival, default = 1))) /
      duration,
    rate = ifelse(Month > 24, control_rate[2], control_rate[1]),
    hr = x_rate / rate
  ) |>
  select(-x_rate) |>
  filter(Period > 0, Scenario > 0) |>
  ungroup()
# fr |> gt() |> fmt_number(columns = everything(), decimals = 2)

fr <- fr |> mutate(fail_rate = rate, dropout_rate = 0.001, stratum = "All")

# MWLR
mwlr <- fixed_design_mb(
  tau = 12,
  enroll_rate = define_enroll_rate(duration = 12, rate = 1),
  fail_rate = fr |> filter(Scenario == 2),
  alpha = 0.025, power = .85, ratio = 1,
  study_duration = 36
) |> to_integer()

er <- mwlr$enroll_rate

## -----------------------------------------------------------------------------
set.seed(3219)

dgm <- fr[c(14:17), ]

fail_rate <- data.frame(
  stratum = rep("All", 2 * nrow(dgm)),
  period = rep(dgm$Period, 2),
  treatment = c(
    rep("control", nrow(dgm)),
    rep("experimental", nrow(dgm))
  ),
  duration = rep(dgm$duration, 2),
  rate = c(dgm$rate, dgm$rate * dgm$hr)
)

dgm$stratum <- "All"
# Constant dropout rate for both treatment arms and all scenarios
dropout_rate <- data.frame(
  stratum = rep("All", 2),
  period = rep(1, 2),
  treatment = c("control", "experimental"),
  duration = rep(100, 2),
  rate = rep(.001, 2)
)

## -----------------------------------------------------------------------------
ss <- 395

set.seed(8316951 + ss * 1000)

# Generate a dataset
dat <- sim_pw_surv(
  n = 698,
  enroll_rate = er,
  fail_rate = fail_rate,
  dropout_rate = dropout_rate
)

analysis_data <- cut_data_by_date(dat, 36)

dfa <- analysis_data

dfa$treat <- ifelse(dfa$treatment == "experimental", 1, 0)

z1 <- dfa |> wlr(weight = fh(rho = 0, gamma = 0))

check <- survdiff(Surv(tte, event) ~ treat, data = dfa)

# Note, for `coxph()`, use
# cph.score <- summary(coxph(Surv(tte, event) ~ treat, data = dfa, control = coxph.control(timefix = TRUE)))$sctest

cat("Log-rank wlr() vs survdiff()", c(z1$z^2, check$chisq), "\n")

## -----------------------------------------------------------------------------
cph.score <- summary(coxph(
  Surv(tte, event) ~ treat,
  data = dfa,
  control = coxph.control(timefix = FALSE)
))$sctest
cat("Log-rank wlr() vs Cox score z^2", c(z1$z^2, cph.score["test"]), "\n")

## -----------------------------------------------------------------------------
Y <- dfa[, "tte"]
Delta <- dfa[, "event"]

tfixed <- aeqSurv(Surv(Y, Delta))
Y <- tfixed[, "time"]
Delta <- tfixed[, "status"]
# Use aeqSurv version
dfa$tte2 <- Y
dfa$event2 <- Delta

# wlr() after "timefix"
dfa2 <- dfa
dfa2$tte <- dfa2$tte2
dfa2$event <- dfa2$event2
z1new <- dfa2 |> wlr(weight = fh(rho = 0, gamma = 0))
cat("Log-rank wlr() with timefix vs survdiff() z^2", c(z1new$z^2, check$chisq), "\n")

## -----------------------------------------------------------------------------
dfa <- dfa[order(dfa$tte2), ]

id <- seq(1, nrow(dfa))

diff <- exp(dfa$tte) - exp(dfa$tte2)
id_diff <- which(abs(diff) > 0)

tolook <- seq(id_diff - 2, id_diff + 2)

dfcheck <- dfa[tolook, c("tte", "tte2", "event", "event2", "treatment")]
print(dfcheck, digits = 12)

## -----------------------------------------------------------------------------
# Check Cox with ties
cox_breslow <- summary(coxph(Surv(tte, event) ~ treatment, data = dfa, ties = "breslow"))$conf.int
cox_efron <- summary(coxph(Surv(tte, event) ~ treatment, data = dfa, ties = "efron"))$conf.int
cat("Cox Breslow and Efron hr (tte, timefix=TRUE):", c(cox_breslow[1], cox_efron[1]), "\n")

# Here ties do not have impact because in separate arms
cox_breslow <- summary(coxph(Surv(tte2, event2) ~ treatment, data = dfa, ties = "breslow", control = coxph.control(timefix = FALSE)))$conf.int
cox_efron <- summary(coxph(Surv(tte2, event2) ~ treatment, data = dfa, ties = "efron", control = coxph.control(timefix = FALSE)))$conf.int
cat("Cox Breslow and Efron hr (tte2, timefix=FALSE):", c(cox_breslow[1], cox_efron[1]), "\n")

## -----------------------------------------------------------------------------
# Create tie within treatment arm by changing treatment
dfa3 <- dfa
dfa3[19, "treat"] <- 1.0

cox_breslow <- summary(coxph(Surv(tte, event) ~ treat, data = dfa3, ties = "breslow", control = coxph.control(timefix = TRUE)))$conf.int
cox_efron <- summary(coxph(Surv(tte, event) ~ treat, data = dfa3, ties = "efron", control = coxph.control(timefix = TRUE)))$conf.int
cat("Cox Breslow and Efron hr (tte, timefix=TRUE)=", c(cox_breslow[1], cox_efron[1]), "\n")

## -----------------------------------------------------------------------------
cox_breslow <- summary(coxph(Surv(tte2, event2) ~ treat, data = dfa3, ties = "breslow", control = coxph.control(timefix = FALSE)))$conf.int
cox_efron <- summary(coxph(Surv(tte2, event2) ~ treat, data = dfa3, ties = "efron", control = coxph.control(timefix = FALSE)))$conf.int
cat("Cox Breslow and Efron hr (tte2, timefix=FALSE)=", c(cox_breslow[1], cox_efron[1]), "\n")

