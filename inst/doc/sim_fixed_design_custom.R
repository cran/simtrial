## ----message=FALSE, warning=FALSE---------------------------------------------
library(gsDesign2)
library(simtrial)
library(dplyr)
library(gt)
library(doFuture)
library(tibble)
set.seed(2025)

## -----------------------------------------------------------------------------
n_sim <- 100
n <- 500
stratum <- data.frame(stratum = "All", p = 1)
block <- rep(c("experimental", "control"), 2)

enroll_rate <- define_enroll_rate(rate = 12, duration = n / 12)

fail_rate <- define_fail_rate(duration = c(6, Inf), fail_rate = log(2) / 10, 
                              hr = c(1, 0.7), dropout_rate = 0.0001)

uncut_data_a <- sim_pw_surv(n = n, stratum = stratum, block = block,
                            enroll_rate = enroll_rate,
                            fail_rate = to_sim_pw_surv(fail_rate)$fail_rate,
                            dropout_rate = to_sim_pw_surv(fail_rate)$dropout_rate)

## -----------------------------------------------------------------------------
uncut_data_a |> head() |> gt() |> tab_header("An Overview of Simulated TTE data")

## -----------------------------------------------------------------------------
differential_dropout_rate <- data.frame(
  stratum = rep("All", 3), 
  period = c(1, 2, 1), 
  treatment = c("control", "control", "experimental"), 
  duration = c(10, Inf, Inf), 
  rate = c(0.002, 0.001, 0.001))

uncut_data_b <- sim_pw_surv(n = n, stratum = stratum, block = block,
                            enroll_rate = enroll_rate,
                            fail_rate = to_sim_pw_surv(fail_rate)$fail_rate,
                            dropout_rate = differential_dropout_rate)

## -----------------------------------------------------------------------------
stratified_enroll_rate <- data.frame(
  stratum = c("Biomarker positive", "Biomarker negative"),
  rate = c(12, 12), 
  duration = c(1, 1))

stratified_fail_rate <- data.frame(
  stratum = c(rep("Biomarker positive", 3), rep("Biomarker negative", 3)), 
  period = c(1, 1, 2, 1, 1, 2), 
  treatment = rep(c("control", "experimental", "experimental"), 2),
  duration = c(Inf, 3, Inf, Inf, 3, Inf), 
  rate = c(# failure rate of biomarker positive subjects: control arm, exp arm period 1, exp arm period 2
           log(2) / 10, log(2) /10, log(2) / 10 * 0.6,
           # failure rate of biomarker negative subjects: control arm, exp arm period 1, exp arm period 2
           log(2) / 8, log(2) /8, log(2) / 8 * 0.8)
  )

stratified_dropout_rate <- data.frame(
  stratum = rep(c("Biomarker positive", "Biomarker negative"), each = 2),
  period = c(1, 1, 1, 1), 
  treatment = c("control", "experimental", "control", "experimental"),
  duration = rep(Inf, 4), 
  rate = rep(0.001, 4)
  )

uncut_data_c <- sim_pw_surv(n = n,
                            stratum = data.frame(stratum = c("Biomarker positive", "Biomarker negative"), 
                                                 p = c(0.5, 0.5)),
                            block = block,
                            enroll_rate = stratified_enroll_rate,
                            fail_rate = stratified_fail_rate,
                            dropout_rate = stratified_dropout_rate
                            )

## -----------------------------------------------------------------------------
enroll_rate <- define_enroll_rate(rate = 12, duration = n / 12)

three_arm_fail_rate <- data.frame(
  stratum = "All",
  period = c(1, 1, 2, 1, 2), 
  treatment = c("control", "low-dose", "low-dose", "high-dose", "high-dose"),
  duration = c(Inf, 3, Inf, 3, Inf), 
  rate = c(# failure rate of control arm: period 1, period 2
           log(2) / 10,
           # failure rate of low-dose arm: period 1, period 2
           log(2) / c(10, 10 / .8),
           # failure rate of high-dose arm: period 1, period 2
           log(2) / c(10, 10 / .6)))

three_arm_dropout_rate <- data.frame(
  stratum = "All",
  period = c(1, 1, 1), 
  treatment = c("control", "low-dose", "high-dose"),
  duration = rep(Inf, 3), 
  rate = rep(0.001, 3))

uncut_data_d <- sim_pw_surv(n = n,
                            stratum = data.frame(stratum = "All"),
                            block = c(rep("control", 3), rep("low-dose", 2), rep("high-dose", 2)),
                            enroll_rate = enroll_rate,
                            fail_rate = three_arm_fail_rate,
                            dropout_rate = three_arm_dropout_rate)

## -----------------------------------------------------------------------------
uncut_data <- uncut_data_b

## -----------------------------------------------------------------------------
cut_date_a <- get_analysis_date(data = uncut_data,
                                planned_calendar_time = 24,
                                target_event_overall = 300)

## -----------------------------------------------------------------------------
cut_date_b <- get_analysis_date(data = uncut_data,
                                min_followup = 12,
                                target_event_overall = 300)

## -----------------------------------------------------------------------------
cut_date_c <- get_analysis_date(data = uncut_data,
                                max_extension_for_target_event = 12,
                                target_event_overall = 300)

## -----------------------------------------------------------------------------
cut_date_d <- get_analysis_date(data = uncut_data,
                                min_n_overall = 100 * 0.8,
                                min_followup = 12)

## -----------------------------------------------------------------------------
cut_date <- cut_date_d
cat("The cutoff date is ", round(cut_date, 2))

cut_data <- uncut_data |> cut_data_by_date(cut_date)
cut_data |> head() |> gt() |> tab_header(paste0("An Overview of TTE data Cut at ", round(cut_date, 2), "Months"))

## -----------------------------------------------------------------------------
# Logrank test
sim_res_lr <- cut_data |> wlr(weight = fh(rho = 0, gamma = 0))

# weighted logrank test by Fleming-Harrington weights
sim_res_fh <- cut_data |> wlr(weight = fh(rho = 0, gamma = 0.5))

# Modestly weighted logrank test
sim_res_mb <- cut_data |> wlr(weight = mb(delay = Inf, w_max = 2))

# Weighted logrank test by Xu 2017's early zero weights
sim_res_xu <- cut_data |> wlr(weight = early_zero(early_period = 3))

# RMST test
sim_res_rmst <- cut_data |> rmst(tau = 10)

# Milestone test
sim_res_ms <- cut_data |> milestone(ms_time = 10)

# Maxcombo tests comboing multiple weighted logrank test with Fleming-Harrington weights
sim_res_mc <- cut_data |> maxcombo(rho = c(0, 0), gamma = c(0, 0.5))

## -----------------------------------------------------------------------------
sim_res <- tribble(
  ~Method, ~Parameter, ~Z, ~Estimate, ~SE, ~`P value`,
  sim_res_lr$method, sim_res_lr$parameter, sim_res_lr$z, sim_res_lr$estimate, sim_res_lr$se, pnorm(-sim_res_lr$z),
  sim_res_fh$method, sim_res_fh$parameter, sim_res_fh$z, sim_res_fh$estimate, sim_res_fh$se, pnorm(-sim_res_fh$z),
  sim_res_mb$method, sim_res_mb$parameter, sim_res_mb$z, sim_res_mb$estimate, sim_res_mb$se, pnorm(-sim_res_mb$z),
  sim_res_xu$method, sim_res_xu$parameter, sim_res_xu$z, sim_res_xu$estimate, sim_res_xu$se, pnorm(-sim_res_xu$z),
  sim_res_rmst$method, sim_res_rmst$parameter|> as.character(), sim_res_rmst$z, sim_res_rmst$estimate, sim_res_rmst$se, pnorm(-sim_res_rmst$z),
  sim_res_ms$method, sim_res_ms$parameter |> as.character(), sim_res_ms$z, sim_res_ms$estimate, sim_res_ms$se, pnorm(-sim_res_ms$z),
  sim_res_mc$method, sim_res_mc$parameter, NA, NA, NA, sim_res_mc$p_value
  ) 

sim_res |> gt() |> tab_header("One Simulation Results")

## -----------------------------------------------------------------------------
one_sim <- function(sim_id = 1, 
                    # arguments from Step 1: design characteristic
                    n, stratum, enroll_rate, fail_rate, dropout_rate, block, 
                    # arguments from Step 2； cutting method
                    min_n_overall, min_followup,
                    # arguments from Step 3； testing method
                    fh, mb, xu, rmst, ms, mc
                    ) {
    # Step 1: simulate a time-to-event data
    uncut_data <- sim_pw_surv(
      n = n,
      stratum = stratum,
      block = block,
      enroll_rate = enroll_rate,
      fail_rate = fail_rate,
      dropout_rate = dropout_rate) 
    
    ## Step 2: Cut data
    cut_date <- get_analysis_date(min_n_overall = min_n_overall, min_followup = min_followup, data = uncut_data)
    cut_data <- uncut_data |> cut_data_by_date(cut_date)
    
    # Step 3: Run tests
    sim_res_lr <- cut_data |> wlr(weight = fh(rho = 0, gamma = 0))
    sim_res_fh <- cut_data |> wlr(weight = fh(rho = fh$rho, gamma = fh$gamma))
    sim_res_mb <- cut_data |> wlr(weight = mb(delay = mb$delay, w_max = mb$w_max))
    sim_res_xu <- cut_data |> wlr(weight = early_zero(early_period = xu$early_period))
    sim_res_rmst <- cut_data |> rmst(tau = rmst$tau)
    sim_res_ms <- cut_data |> milestone(ms_time = ms$ms_time)
    sim_res_mc <- cut_data |> maxcombo(rho = mc$rho, gamma = mc$gamma)
    
    sim_res <- tribble(
      ~`Sim ID`, ~Method, ~Parameter, ~Z, ~Estimate, ~SE, ~`P value`,
      sim_id, sim_res_lr$method, sim_res_lr$parameter, sim_res_lr$z, sim_res_lr$estimate, sim_res_lr$se, pnorm(-sim_res_lr$z),
      sim_id, sim_res_fh$method, sim_res_fh$parameter, sim_res_fh$z, sim_res_fh$estimate, sim_res_fh$se, pnorm(-sim_res_fh$z),
      sim_id, sim_res_mb$method, sim_res_mb$parameter, sim_res_mb$z, sim_res_mb$estimate, sim_res_mb$se, pnorm(-sim_res_mb$z),
      sim_id, sim_res_xu$method, sim_res_xu$parameter, sim_res_xu$z, sim_res_xu$estimate, sim_res_xu$se, pnorm(-sim_res_xu$z),
      sim_id, sim_res_rmst$method, sim_res_rmst$parameter|> as.character(), sim_res_rmst$z, sim_res_rmst$estimate, sim_res_rmst$se, pnorm(-sim_res_rmst$z),
      sim_id, sim_res_ms$method, sim_res_ms$parameter |> as.character(), sim_res_ms$z, sim_res_ms$estimate, sim_res_ms$se, pnorm(-sim_res_ms$z),
      sim_id, sim_res_mc$method, sim_res_mc$parameter, NA, NA, NA, sim_res_mc$p_value
  ) 
      
    return(sim_res)
}

## -----------------------------------------------------------------------------
set.seed(2025)

plan("multisession", workers = 2)
ans <- foreach(
  sim_id = seq_len(n_sim),
  .errorhandling = "stop",
  .options.future = list(seed = TRUE)
  ) %dofuture% {
    ans_new <- one_sim(
      sim_id = sim_id, 
      # arguments from Step 1: design characteristic
      n = n, 
      stratum = stratum, 
      enroll_rate = enroll_rate, 
      fail_rate = to_sim_pw_surv(fail_rate)$fail_rate, 
      dropout_rate = differential_dropout_rate, 
      block = block, 
      # arguments from Step 2； cutting method
      min_n_overall = 500 * 0.8,
      min_followup = 12,
      # arguments from Step 3； testing method
      fh = list(rho = 0, gamma = 0.5), 
      mb = list(delay = Inf, w_max = 2), 
      xu = list(early_period = 3), 
      rmst = list(tau = 10), 
      ms = list(ms_time = 10), 
      mc = list(rho = c(0, 0), gamma = c(0, 0.5))
      )
                              
    ans_new
  }

ans <- data.table::rbindlist(ans)

plan("sequential")

## -----------------------------------------------------------------------------
ans |> head() |> gt() |> tab_header("Overview Each Simulation results")

## ----message=FALSE------------------------------------------------------------
ans_non_mc <- ans |>
  filter(Method != "MaxCombo") |>
  group_by(Method, Parameter) %>% 
  summarise(Power = mean(Z > -qnorm(0.025))) |>
  ungroup()

ans_mc <- ans |>
  filter(Method == "MaxCombo") |>
  summarize(Power = mean(`P value` < 0.025), Method = "MaxCombo", Parameter = "FH(0, 0) + FH(0, 0.5)") 

ans_non_mc |>
  union(ans_mc) |>
  gt() |>
  tab_header("Summary from 100 simulations")

