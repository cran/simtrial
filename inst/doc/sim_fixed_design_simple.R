## ----message=FALSE, warning=FALSE---------------------------------------------
library(gsDesign2)
library(simtrial)
library(dplyr)
library(gt)

set.seed(2027)

## -----------------------------------------------------------------------------
n_sim <- 100
total_duration <- 36
stratum <- data.frame(stratum = "All", p = 1)
block <- rep(c("experimental", "control"), 2)

enroll_rate <- data.frame(stratum = "All", rate = 12, duration = 500 / 12)
fail_rate <- data.frame(stratum = "All",
                        duration = c(3, Inf), fail_rate = log(2) / 10, 
                        hr = c(1, 0.6), dropout_rate = 0.001)

## -----------------------------------------------------------------------------
x <- fixed_design_ahr(enroll_rate = enroll_rate, fail_rate = fail_rate, 
                      alpha = 0.025, power = 0.85, ratio = 1, 
                      study_duration = total_duration) |> to_integer()
x |> summary() |> gt() |> 
  tab_header(title = "Sample Size and Targeted Events Based on AHR Method", 
             subtitle = "Fixed Design with 85% Power, One-sided 2.5% Type I error") |>
  fmt_number(columns = c(4, 5, 7), decimals = 2)

## -----------------------------------------------------------------------------
sample_size <- x$analysis$n
target_event <- x$analysis$event
enroll_rate <- x$enroll_rate

## ----message=FALSE------------------------------------------------------------
sim_res <- sim_fixed_n(
  n_sim = 2, # only use 2 simulations for initial run
  sample_size = sample_size, 
  block = block, 
  stratum = stratum,
  target_event = target_event, 
  total_duration = total_duration,
  enroll_rate = enroll_rate, 
  fail_rate = fail_rate,
  timing_type = 1:5, 
  rho_gamma = data.frame(rho = 0, gamma = 0))

## -----------------------------------------------------------------------------
sim_res |>
  gt() |>
  tab_header("Tests for Each Simulation Result", subtitle = "Logrank Test for Different Analysis Cutoffs") |>
  fmt_number(columns = c(4, 5, 7), decimals = 2)

## ----message=FALSE------------------------------------------------------------
sim_res <- sim_fixed_n(
  n_sim = n_sim,
  sample_size = sample_size, 
  block = block, stratum = stratum,
  target_event = target_event, 
  total_duration = total_duration,
  enroll_rate = enroll_rate, 
  fail_rate = fail_rate,
  timing_type = 1:5, 
  rho_gamma = data.frame(rho = 0, gamma = 0))

## -----------------------------------------------------------------------------
sim_res |>
  group_by(cut) |>
  summarize(`Simulated Power` = mean(z > qnorm(1 - 0.025)), 
            `Mean events` = mean(event),
            `Mean duration` = mean(duration)) |>
  mutate(`Sample size` = sample_size,
         `Targeted events` = target_event) |>
  gt() |>
  tab_header(title = "Summary of 100 simulations by 5 different analysis cutoff methods",
             subtitle = "Tested by logrank") |>
  fmt_number(columns = c(2:4), decimals = 2)

## ----fig.width = 6------------------------------------------------------------
hist(sim_res$event[sim_res$cut == "Planned duration"], 
     breaks = 10,
     main = "Distribution of Event Counts at Planned Study Duration",
     xlab = "Event Count at Targeted Trial Duration")

## ----fig.width = 6------------------------------------------------------------
plot(density(sim_res$duration[sim_res$cut == "Targeted events"]), 
     main = "Trial Duration Smoothed Density",
     xlab = "Trial duration when Targeted Event Count is Observed")

