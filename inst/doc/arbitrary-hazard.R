## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.height = 4,
  fig.width = 7.5,
  out.width = "100%"
)

run <- requireNamespace("dplyr", quietly = TRUE) &&
  requireNamespace("ggplot2", quietly = TRUE)
knitr::opts_chunk$set(eval = run)

## ----message=FALSE, warning=FALSE---------------------------------------------
library(simtrial)
library(ggplot2)
library(dplyr)
library(survival)

## ----eval=FALSE---------------------------------------------------------------
#  remotes::install_github("cran/bshazard")

## -----------------------------------------------------------------------------
set.seed(123)

dloglogis <- function(x, alpha = 1, beta = 4) {
  1 / (1 + (x / alpha)^beta)
}
times <- (1:150) / 50
xx <- data.frame(
  Times = times,
  Survival = dloglogis(times, alpha = .5, beta = 4)
) |>
  mutate(
    duration = Times - lag(Times, default = 0),
    H = -log(Survival),
    rate = (H - lag(H, default = 0)) / duration / 3
  ) |>
  select(duration, rate)
ggplot(
  data = xx |> mutate(Time = lag(cumsum(duration), default = 0)),
  aes(x = Time, y = rate)
) +
  geom_line()

## -----------------------------------------------------------------------------
tx <- "Log-logistic"
enroll_rate <- data.frame(duration = .5, rate = 500)
dropout_rate <- data.frame(
  treatment = tx,
  duration = 3,
  rate = .05,
  period = 1,
  stratum = "All"
)
block <- rep(tx, 2)
x <- sim_pw_surv(
  n = 250, # Sample size
  block = block,
  enroll_rate = enroll_rate,
  fail_rate = xx |> mutate(
    stratum = "All",
    treatment = tx,
    period = seq_len(n()),
    stratum = "All"
  ),
  dropout_rate = dropout_rate
)

## -----------------------------------------------------------------------------
y <- x |> cut_data_by_date(3)
head(y)

## ----fig.height=4, fig.width=7.5----------------------------------------------
fit <- survfit(Surv(tte, event) ~ 1, data = y)
plot(fit, mark = "|")

## ----echo=FALSE---------------------------------------------------------------
fit <- readRDS("fit-bshazard.rds")

plot.bshazard <- function(
    x, conf.int = TRUE, overall = TRUE, col = 1, lwd = 1, lty = 1,
    xlab = "Time", ylab = "Hazard rate", border = NA, col.fill = "lightgrey",
    ...) {
  plot(
    x$time, x$hazard,
    xlab = xlab, type = "l", ylab = ylab,
    lwd = lwd, lty = lty, col = col, ...
  )
  polygon(
    c(x$time, rev(x$time)), c(x$low, rev(x$up)),
    col = col.fill, border = border, ...
  )
  lines(
    x$time, x$hazard,
    xlab = xlab, type = "l",
    ylab = ylab, lwd = 2, lty = lty, col = col, ...
  )
}

## ----eval=FALSE---------------------------------------------------------------
#  fit <- bshazard::bshazard(Surv(tte, event) ~ 1, data = y, nk = 120)

## -----------------------------------------------------------------------------
plot(fit, conf.int = TRUE, xlab = "Time", xlim = c(0, 3), ylim = c(0, 2.5), lwd = 2)
lines(x = times, y = (xx |> mutate(Time = lag(cumsum(duration), default = 0)))$rate, col = 2)

