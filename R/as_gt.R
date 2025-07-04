#  Copyright (c) 2025 Merck & Co., Inc., Rahway, NJ, USA and its affiliates.
#  All rights reserved.
#
#  This file is part of the simtrial program.
#
#  simtrial is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' Convert summary table to a gt object
#'
#' @param x A summary object of a fixed or group sequential design.
#' @param ... Additional arguments (not used).
#'
#' @return A gt table.
#'
#' @export
as_gt <- function(x, ...) {
  UseMethod("as_gt", x)
}


#' @param x A object returned by [summary()].
#' @param title Title of the gt table.
#' @param subtitle Subtitle of the gt table.
#' @param ... Additional parameters (not used).
#'
#' @return A gt table summarizing the simulation results.
#' @export
#' @rdname as_gt
#'
#' @examples
#'
#' # Parameters for enrollment
#' enroll_rampup_duration <- 4 # Duration for enrollment ramp up
#' enroll_duration <- 16 # Total enrollment duration
#' enroll_rate <- gsDesign2::define_enroll_rate(
#'   duration = c(
#'     enroll_rampup_duration, enroll_duration - enroll_rampup_duration),
#'  rate = c(10, 30))
#'
#' # Parameters for treatment effect
#' delay_effect_duration <- 3 # Delay treatment effect in months
#' median_ctrl <- 9 # Survival median of the control arm
#' median_exp <- c(9, 14) # Survival median of the experimental arm
#' dropout_rate <- 0.001
#' fail_rate <- gsDesign2::define_fail_rate(
#'   duration = c(delay_effect_duration, 100),
#'   fail_rate = log(2) / median_ctrl,
#'   hr = median_ctrl / median_exp,
#'   dropout_rate = dropout_rate)
#'
#' # Other related parameters
#' alpha <- 0.025 # Type I error
#' beta <- 0.1 # Type II error
#' ratio <- 1 # Randomization ratio (experimental:control)
#'
#' # Build a one-sided group sequential design
#' design <- gsDesign2::gs_design_ahr(
#'   enroll_rate = enroll_rate, fail_rate = fail_rate,
#'   ratio = ratio, alpha = alpha, beta = beta,
#'   analysis_time = c(12, 24, 36),
#'   upper = gsDesign2::gs_spending_bound,
#'   upar = list(sf = gsDesign::sfLDOF, total_spend = alpha),
#'   lower = gsDesign2::gs_b,
#'   lpar = rep(-Inf, 3))
#'
#' # Define cuttings of 2 IAs and 1 FA
#' ia1_cut <- create_cut(target_event_overall = ceiling(design$analysis$event[1]))
#' ia2_cut <- create_cut(target_event_overall = ceiling(design$analysis$event[2]))
#' fa_cut <- create_cut(target_event_overall = ceiling(design$analysis$event[3]))
#'
#' # Run simulations
#' simulation <- sim_gs_n(
#'   n_sim = 3,
#'   sample_size = ceiling(design$analysis$n[3]),
#'   enroll_rate = design$enroll_rate,
#'   fail_rate = design$fail_rate,
#'   test = wlr,
#'   cut = list(ia1 = ia1_cut, ia2 = ia2_cut, fa = fa_cut),
#'   weight = fh(rho = 0, gamma = 0.5))
#'
#' # Summarize simulations
#' simulation |>
#'  summary(bound = gsDesign::gsDesign(k = 3, test.type = 1, sfu = gsDesign::sfLDOF)$upper$bound) |>
#'  simtrial::as_gt()
#'
#' # Summarize simulations and compare with the planned design
#' simulation |>
#'   summary(design = design) |>
#'   simtrial::as_gt()
as_gt.simtrial_gs_wlr <- function(x,
                                  title = "Summary of simulation results by WLR tests",
                                  subtitle = NULL, ...){
  # get the default subtitle
  if (is.null(subtitle)) {
    subtitle <- paste0("Weighted by ", attributes(x)$method)
  }

  # if it is not compared with the design
  if (attributes(x)$compare_with_design == "no") {
    x |>
      gt::gt() |>
      gt::cols_label(sim_time = "Time", sim_n = "N", sim_event = "Event", sim_upper_prob = "Crossing probability") |>
      gt::cols_move(columns = c(sim_time, sim_n, sim_event), after = analysis) |>
      gt::tab_header(title = title, subtitle = subtitle)
  } else {
    # get the design type, either one-sided or two-sided
    design_type <- attributes(x)$design_type

    # build a gt table as return
    ans <- x |>
      gt::gt() |>
      gt::tab_spanner(label = "Time", columns = gt::ends_with("_time")) |>
      gt::tab_spanner(label = "Events", columns = gt::ends_with("_event")) |>
      gt::tab_spanner(label = "N", columns = gt::ends_with("_n")) |>
      gt::tab_spanner(
        label = "Probability of crossing efficacy bounds under H1",
        columns = gt::ends_with("_upper_prob"))

    if (design_type == "two-sided") {
      ans <- ans |> gt::tab_spanner(
        label = "Probability of crossing futility bounds under H1",
        columns = gt::ends_with("_lower_prob"))
    }

    ans |>
      gt::cols_label(
        starts_with("asy") ~ "Asymptotic",
        starts_with("sim") ~ "Simulated",
        matches("analysis") ~ "Analysis") |>
      gt::cols_move(columns = c(asy_time, sim_time, asy_n, sim_n, asy_event, sim_event), after = analysis) |>
      gt::tab_header(title = title, subtitle = subtitle)
  }
}

# Only purpose of the methods below is to fix S3 redirection when simtrial is
# loaded after gsDesign2, which masks the as_gt() generic from gsDesign2

#' @export
as_gt.fixed_design <- function(x, ...) {
  f <- utils::getFromNamespace("as_gt.fixed_design", "gsDesign2")
  f(x, ...)
}

#' @export
as_gt.gs_design <- function(x, ...) {
  f <- utils::getFromNamespace("as_gt.gs_design", "gsDesign2")
  f(x, ...)
}
