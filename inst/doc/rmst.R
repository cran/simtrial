## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

run <- if (rlang::is_installed(c("gt", "survRM2"))) TRUE else FALSE
knitr::opts_chunk$set(eval = run)

## ----message=FALSE, warning=FALSE---------------------------------------------
# Simulate NPH data from the piecewise model
library(simtrial)
# Table display
library(gt)

## -----------------------------------------------------------------------------
data(ex1_delayed_effect)
data_single_arm <- ex1_delayed_effect[ex1_delayed_effect$trt == 1, ]
simtrial:::rmst_single_arm(
  time_var = data_single_arm$month,
  event_var = data_single_arm$evntd,
  tau = 10
) |> gt()

## -----------------------------------------------------------------------------
tau <- 10

data(ex1_delayed_effect)

ex1_delayed_effect |>
  rmst(
    var_label_tte = "month",
    var_label_event = "evntd",
    var_label_group = "trt",
    tau = 10,
    reference = "0"
  )

## -----------------------------------------------------------------------------
verify <- survRM2::rmst2(
  time = ex1_delayed_effect$month,
  status = ex1_delayed_effect$evntd,
  arm = ex1_delayed_effect$trt,
  tau = tau,
  alpha = 0.05
)

verify$RMST.arm1$rmst[1] - verify$RMST.arm0$rmst[1]

