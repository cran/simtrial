## ----message=FALSE, warning=FALSE---------------------------------------------
library(gsDesign2)
library(simtrial)
library(dplyr)
library(gt)

set.seed(2025)

## -----------------------------------------------------------------------------
stratum <- data.frame(stratum = "All", p = 1)
block <- rep(c("experimental", "control"), 2)
# enrollment rate will be updated later, 
# multiplied by a constant to get targeted power
enroll_rate <- data.frame(stratum = "All", rate = 1, duration = 12)
fail_rate <- data.frame(stratum = "All",
                        duration = c(3, Inf), fail_rate = log(2) / 10, 
                        hr = c(1, 0.6), dropout_rate = 0.001)
# Derive design using the average hazard ratio method
x <- gs_design_ahr(enroll_rate = enroll_rate, fail_rate = fail_rate,
                   analysis_time = c(12, 24, 36), alpha = 0.025, beta = 0.1,
                   # spending function for upper bound
                   upper = gs_spending_bound, 
                   upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
                   # Fixed lower bound
                   lower = gs_b,
                   lpar = rep(-Inf, 3)) |> to_integer()

sample_size <- x$analysis$n |> max()
event <- x$analysis$event
eff_bound <- x$bound$z[x$bound$bound == "upper"]

## -----------------------------------------------------------------------------
cat(paste("The total sample size is ", sample_size, "\n", sep = ''))
cat("The number of events at IA1, IA2 and FA are:", event, "\n")
cat("The efficacy bounds at IA1, IA2 and FA are:", round(eff_bound, 3), "\n")
cat("Targeted analysis times:", round(x$analysis$time, 1), "\n")

## -----------------------------------------------------------------------------
enroll_rate <- x$enroll_rate
enroll_rate

## -----------------------------------------------------------------------------
# Example for logrank
weight <- fh(rho = 0, gamma = 0)
test <- wlr
# Example for Modestly Weighted Logrank Test (Magirr-Burman)
# weight <- mb(delay = Inf, w_max = 2)
# Example for Fleming-Harrington(0, 0.5)
# weight <- fh(rho = 0, gamma = 0.5)

## -----------------------------------------------------------------------------
ia1_cut <- create_cut(target_event_overall = event[1])
ia2_cut <- create_cut(target_event_overall = event[2])
fa_cut <- create_cut(target_event_overall = event[3])

cut <- list(ia1 = ia1_cut, ia2 = ia2_cut, fa = fa_cut)

## ----eval=FALSE---------------------------------------------------------------
#  ia1_cut <- create_cut(
#    planned_calendar_time = round(x$analysis$time[1]),
#    target_event_overall = x$analysis$event[1],
#    max_extension_for_target_event = 16)
#  
#  ia2_cut <- create_cut(
#    planned_calendar_time = round(x$analysis$time[2]),
#    target_event_overall = x$analysis$event[2],
#    min_time_after_previous_analysis = 10,
#    max_extension_for_target_event = 28)
#  
#  fa_cut <- create_cut(
#    planned_calendar_time = round(x$analysis$time[3]),
#    min_time_after_previous_analysis = 6,
#    target_event_overall = x$analysis$event[3])
#  
#  cut <- list(ia1 = ia1_cut, ia2 = ia2_cut, fa = fa_cut)

## ----message=FALSE------------------------------------------------------------
n_sim <- 100 # Number of simulated trials
sim_res <- sim_gs_n(
  n_sim = n_sim,
  sample_size = sample_size, stratum = stratum, block = block,
  enroll_rate = enroll_rate, fail_rate = fail_rate,
  test = test, weight = weight, cut = cut)

## -----------------------------------------------------------------------------
sim_res |> head(n = 6) |> gt() |> tab_header("Overview Each Simulation results") |>
  fmt_number(columns = c(5, 8:12), decimals = 2)

## ----message=FALSE------------------------------------------------------------
sim_res |>
  left_join(data.frame(analysis = 1:3, eff_bound = eff_bound)) |>
  group_by(analysis) |>
  summarize(`Mean time` = mean(cut_date), `sd(time)` = sd(cut_date), `Simulated power` = mean(z >= eff_bound)) |>
  ungroup() |>
  mutate(`Asymptotic power` = x$bound$probability[x$bound$bound == "upper"]) |>
  gt() |>
  tab_header("Summary of 100 simulations") |> 
  fmt_number(columns = 2, decimals = 1) |>
  fmt_number(columns = 3:5, decimals = 2)

