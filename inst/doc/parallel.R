## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----dependencies, message=FALSE, warning=FALSE-------------------------------
library(simtrial)
library(future)
library(doFuture)

## ----set-plan-sequential, echo=FALSE------------------------------------------
plan("sequential") # Ensure that the backend is sequential

## ----enrollments, fig.height=4, fig.width=6, fig.align="center"---------------
set.seed(1)

n <- 5000
enroll_rate1 <- data.frame(rate = c(5, 20, 10), duration = c(100, 150, 150))
enroll_rate2 <- data.frame(rate = c(10, 15, 30), duration = c(150, 175, 75))
x1 <- rpwexp_enroll(n = n, enroll_rate = enroll_rate1)
x2 <- rpwexp_enroll(n = n, enroll_rate = enroll_rate2)

plot(
  x1, 1:n,
  type = "l",
  col = palette()[4],
  xlim = c(0, max(x1, x2)),
  main = "Piecewise enrollments",
  xlab = "Time",
  ylab = "Enrollment"
)
lines(x2, 1:n, col = palette()[7])
legend(
  250, 1500,
  legend = c("Enrollment 1", "Enrollment 2"),
  col = c(palette()[4], palette()[7]),
  lty = c(1, 1)
)

## ----confirm-sequential-------------------------------------------------------
data.table::setDTthreads(threads = 1)
set.seed(1)

n_sim <- 200

start_sequential <- proc.time()

seq_result1 <- sim_fixed_n(
  n_sim = n_sim,
  sample_size = 3000,
  target_event = 700,
  enroll_rate = enroll_rate1,
  timing_type = 2 # Time until targeted event count achieved
)

seq_result2 <- sim_fixed_n(
  n_sim = n_sim,
  sample_size = 3000,
  target_event = 700,
  enroll_rate = enroll_rate2,
  timing_type = 2 # Time until targeted event count achieved
)

duration_sequential <- proc.time() - start_sequential

## ----sequential-time----------------------------------------------------------
print(duration_sequential)

## ----sequential-display-duration, echo=FALSE, fig.height=4, fig.width=6, fig.align="center"----
duration_min <- min(seq_result1$duration, seq_result2$duration)
duration_max <- max(seq_result1$duration, seq_result2$duration)
hist(
  x = seq_result1$duration,
  col = palette()[4],
  main = "Duration by enrollment strategy",
  xlab = "Duration",
  xlim = c(duration_min, duration_max),
  ylab = "Number of simulations",
  ylim = c(0, n_sim / 3)
)
hist(seq_result2$duration, col = palette()[7], add = TRUE)
abline(v = mean(seq_result1$duration), lwd = 2, lty = "dashed")
abline(v = mean(seq_result2$duration), lwd = 2, lty = "dashed")
legend(
  "top",
  legend = c("Enrollment 1", "Enrollment 2"),
  col = c(palette()[4], palette()[7]),
  lty = c(1, 1)
)

## ----multisession1------------------------------------------------------------
plan(multisession, workers = 2)

## ----confirm-multisession-----------------------------------------------------
set.seed(1)

start_parallel <- proc.time()

par_result1 <- sim_fixed_n(
  n_sim = n_sim,
  sample_size = 3000,
  target_event = 700,
  enroll_rate = enroll_rate1,
  timing_type = 2 # Time until targeted event count achieved
)

par_result2 <- sim_fixed_n(
  n_sim = n_sim,
  sample_size = 3000,
  target_event = 700,
  enroll_rate = enroll_rate2,
  timing_type = 2 # Time until targeted event count achieved
)

duration_parallel <- proc.time() - start_parallel

## ----time-parallel------------------------------------------------------------
print(duration_parallel)

## ----plan-sequential----------------------------------------------------------
plan(sequential)

## ----compare-results----------------------------------------------------------
all.equal(seq_result1, par_result1)
all.equal(seq_result2, par_result2)

## ----schema, echo=FALSE, fig.cap="Available resource schematic.", fig.align="center", out.width="90%"----
knitr::include_graphics("./figures/schema.png")

## ----nested-topology, eval=FALSE----------------------------------------------
#  nodes <- c("n1", "n2")
#  custom_cores <- function() {
#    switch(Sys.info()[["nodename"]],
#      "n1" = 3L, # Modify here for number of cores on node1
#      "n2" = 3L, # Modify here for number of cores on node2
#      ## Default:
#      availableCores()
#    )
#  }
#  plan(list(
#    tweak(cluster, workers = nodes),
#    tweak(multisession, workers = custom_cores)
#  ))

## ----confirm-cluster, eval=FALSE----------------------------------------------
#  set.seed(1)
#  
#  enroll_rates <- list(enroll_rate1, enroll_rate2)
#  
#  nested_result <- foreach::foreach(
#    i = 1:2,
#    .combine = "list",
#    .options.future = list(seed = TRUE)
#  ) %dofuture% {
#    sim_fixed_n(
#      n_sim = n_sim,
#      sample_size = 3000,
#      target_event = 700,
#      enroll_rate = enroll_rates[[i]],
#      timing_type = 2 # Time until targeted event count achieved
#    )
#  }

## ----plan-sequential2, eval=FALSE---------------------------------------------
#  plan(sequential)

