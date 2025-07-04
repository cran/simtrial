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

#' Derive analysis date for interim/final analysis given multiple conditions
#'
#' @param data A simulated data generated by [sim_pw_surv()].
#' @param planned_calendar_time A numerical value specifying the
#'   planned calendar time for the analysis.
#' @param target_event_overall A numerical value specifying the
#'   targeted events for the overall population.
#' @param target_event_per_stratum A numerical vector specifying the
#'   targeted events per stratum.
#' @param max_extension_for_target_event A numerical value specifying the
#'   maximum time extension to reach targeted events.
#' @param previous_analysis_date A numerical value specifying the
#'   previous analysis date.
#' @param min_time_after_previous_analysis A numerical value specifying the
#'   planned minimum time after the previous analysis.
#' @param min_n_overall A numerical value specifying the
#'   minimal overall sample size enrolled to kick off the analysis.
#' @param min_n_per_stratum A numerical value specifying the
#'   minimal sample size enrolled per stratum to kick off the analysis.
#' @param min_followup A numerical value specifying the
#'   minimal follow-up time after specified enrollment fraction in
#'   `min_n_overall` or `min_n_per_stratum`.
#'
#' @details
#' To obtain the analysis date, consider the following multiple conditions:
#'
#' \describe{
#' \item{Condition 1}{The planned calendar time for analysis.}
#' \item{Condition 2}{The targeted events, encompassing both overall population
#' and stratum-specific events.}
#' \item{Condition 3}{The maximum time extension required to achieve the
#' targeted events.}
#' \item{Condition 4}{The planned minimum time interval after the
#' previous analysis.}
#' \item{Condition 5}{The minimum follow-up time needed to reach a
#' certain number of patients in enrollments.}
#' }
#'
#' Users have the flexibility to employ all 5 conditions simultaneously or
#' selectively choose specific conditions to determine the analysis date.
#' Any unused conditions will default to `NA` and not affect the output.
#' Regardless of the number of conditions used, the analysis date is determined
#' by `min(max(date1, date2, date4, date5, na.rm = TRUE), date3, na.rm = TRUE)`,
#' where `date1`, `date2`, `date3`, `date4`, `date5` represent the analysis
#' dates determined solely by Condition 1, Condition 2, Condition 3,
#' Condition 4 and Condition 5, respectively.
#'
#' @return A numerical value of the analysis date.
#'
#' @importFrom data.table as.data.table .N
#'
#' @export
#'
#' @examplesIf requireNamespace("gsDesign2", quietly = TRUE)
#' library(gsDesign2)
#'
#' alpha <- 0.025
#' ratio <- 3
#' n <- 500
#' info_frac <- c(0.7, 1)
#' prevalence_ratio <- c(0.4, 0.6)
#' study_duration <- 48
#'
#' # Two strata
#' stratum <- c("Biomarker-positive", "Biomarker-negative")
#'
#' prevalence_ratio <- c(0.6, 0.4)
#' # enrollment rate
#' enroll_rate <- define_enroll_rate(
#'   stratum = rep(stratum, each = 2),
#'   duration = c(2, 10, 2, 10),
#'   rate = c(c(1, 4) * prevalence_ratio[1], c(1, 4) * prevalence_ratio[2])
#' )
#' enroll_rate$rate <- enroll_rate$rate * n / sum(enroll_rate$duration * enroll_rate$rate)
#'
#' # Failure rate
#' med_pos <- 10 # Median of the biomarker positive population
#' med_neg <- 8 # Median of the biomarker negative population
#' hr_pos <- c(1, 0.7) # Hazard ratio of the biomarker positive population
#' hr_neg <- c(1, 0.8) # Hazard ratio of the biomarker negative population
#' fail_rate <- define_fail_rate(
#'   stratum = rep(stratum, each = 2),
#'   duration = 1000,
#'   fail_rate = c(log(2) / c(med_pos, med_pos, med_neg, med_neg)),
#'   hr = c(hr_pos, hr_neg),
#'   dropout_rate = 0.01
#' )
#'
#' # Simulate data
#' temp <- to_sim_pw_surv(fail_rate) # Convert the failure rate
#' set.seed(2023)
#' simulated_data <- sim_pw_surv(
#'   n = n, # Sample size
#'   # Stratified design with prevalence ratio of 6:4
#'   stratum = data.frame(stratum = stratum, p = prevalence_ratio),
#'   # Randomization ratio
#'   block = c("control", "control", "experimental", "experimental"),
#'   enroll_rate = enroll_rate, # Enrollment rate
#'   fail_rate = temp$fail_rate, # Failure rate
#'   dropout_rate = temp$dropout_rate # Dropout rate
#' )
#'
#' # Example 1: Cut for analysis at the 24th month.
#' # Here, we only utilize the `planned_calendar_time = 24` argument,
#' # while leaving the remaining unused arguments as their default value of `NA`.
#' get_analysis_date(
#'   simulated_data,
#'   planned_calendar_time = 24
#' )
#'
#' # Example 2: Cut for analysis when there are 300 events in the overall population.
#' # Here, we only utilize the `target_event_overall = 300` argument,
#' # while leaving the remaining unused arguments as their default value of `NA`.
#' get_analysis_date(
#'   simulated_data,
#'   target_event_overall = 300
#' )
#'
#' # Example 3: Cut for analysis at the 24th month and there are 300 events
#' # in the overall population, whichever arrives later.
#' # Here, we only utilize the `planned_calendar_time = 24` and
#' # `target_event_overall = 300` argument,
#' # while leaving the remaining unused arguments as their default value of `NA`.
#' get_analysis_date(
#'   simulated_data,
#'   planned_calendar_time = 24,
#'   target_event_overall = 300
#' )
#'
#' # Example 4a: Cut for analysis when there are at least 100 events
#' # in the biomarker-positive population, and at least 200 events
#' # in the biomarker-negative population, whichever arrives later.
#' # Here, we only utilize the `target_event_per_stratum = c(100, 200)`,
#' # which refers to 100 events in the biomarker-positive population,
#' # and 200 events in the biomarker-negative population.
#' # The remaining unused arguments as their default value of `NA`,
#' # so the analysis date is only decided by the number of events
#' # in each stratum.
#' get_analysis_date(
#'   simulated_data,
#'   target_event_per_stratum = c(100, 200)
#' )
#' # Example 4b: Cut for analysis when there are at least 100 events
#' # in the biomarker-positive population, but we don't have a requirement
#' # for the biomarker-negative population. Additionally, we want to cut
#' # the analysis when there are at least 150 events in total.
#' # Here, we only utilize the `target_event_overall = 150` and
#' # `target_event_per_stratum = c(100, NA)`, which refers to 100 events
#' # in the biomarker-positive population, and there is event requirement
#' # for the biomarker-negative population.
#' # The remaining unused arguments as their default value of `NA`,
#' # so the analysis date is only decided by the number of events
#' # in the biomarker-positive population, and the total number of events,
#' # which arrives later.
#' get_analysis_date(
#'   simulated_data,
#'   target_event_overall = 150,
#'   target_event_per_stratum = c(100, NA)
#' )
#' # Example 4c: Cut for analysis when there are at least 100 events
#' # in the biomarker-positive population, but we don't have a requirement
#' # for the biomarker-negative population. Additionally, we want to cut
#' # the analysis when there are at least 150 events in total and after 24 months.
#' # Here, we only utilize the `planned_calendar_time = 24`,
#' # `target_event_overall = 150` and
#' # `target_event_per_stratum = c(100, NA)`, which refers to 100 events
#' # in the biomarker-positive population, and there is event requirement
#' # for the biomarker-negative population.
#' # The remaining unused arguments as their default value of `NA`,
#' # so the analysis date is only decided by the number of events
#' # in the biomarker-positive population, the total number of events, and
#' # planned calendar time, which arrives later.
#' get_analysis_date(
#'   simulated_data,
#'   planned_calendar_time = 24,
#'   target_event_overall = 150,
#'   target_event_per_stratum = c(100, NA)
#' )
#'
#' # Example 5: Cut for analysis when there are at least 100 events
#' # in the biomarker positive population, and at least 200 events
#' # in the biomarker negative population, whichever arrives later.
#' # But will stop at the 30th month if events are fewer than 100/200.
#' # Here, we only utilize the `max_extension_for_target_event = 30`,
#' # and `target_event_per_stratum =  c(100, 200)`, which refers to
#' # 100/200 events in the biomarker-positive/negative population.
#' # The remaining unused arguments as their default value of `NA`,
#' # so the analysis date is only decided by the number of events
#' # in the 2 strata, and the max extension to arrive at the targeted
#' # events, which arrives later.
#' get_analysis_date(
#'   simulated_data,
#'   target_event_per_stratum = c(100, 200),
#'   max_extension_for_target_event = 30
#' )
#'
#' # Example 6a: Cut for analysis after 12 months followup when 80%
#' # of the patients are enrolled in the overall population.
#' # The remaining unused arguments as their default value of `NA`,
#' # so the analysis date is only decided by
#' # 12 months + time when 80% patients enrolled.
#' get_analysis_date(
#'   simulated_data,
#'   min_n_overall = n * 0.8,
#'   min_followup = 12
#' )
#' # Example 6b: Cut for analysis after 12 months followup when 80%
#' # of the patients are enrolled in the overall population. Besides,
#' # the analysis happens when there are at least 150 events in total.
#' # The remaining unused arguments as their default value of `NA`,
#' # so the analysis date is only decided by the total number of events,
#' # and 12 months + time when 80% patients enrolled, which arrives later.
#' get_analysis_date(
#'   simulated_data,
#'   target_event_overall = 150,
#'   min_n_overall = n * 0.8,
#'   min_followup = 12
#' )
#'
#' # Example 7a: Cut for analysis when 12 months after at least 200/160 patients
#' # are enrolled in the biomarker positive/negative population.
#' # The remaining unused arguments as their default value of `NA`,
#' # so the analysis date is only decided by 12 months + time when there are
#' # 200/160 patients enrolled in the biomarker-positive/negative stratum.
#' get_analysis_date(
#'   simulated_data,
#'   min_n_per_stratum = c(200, 160),
#'   min_followup = 12
#' )
#' # Example 7b: Cut for analysis when 12 months after at least 200 patients
#' # are enrolled in the biomarker positive population, but we don't have a
#' # specific requirement for the biomarker negative population.
#' # The remaining unused arguments as their default value of `NA`,
#' # so the analysis date is only decided by 12 months + time when there are
#' # 200 patients enrolled in the biomarker-positive stratum.
#' get_analysis_date(
#'   simulated_data,
#'   min_n_per_stratum = c(200, NA),
#'   min_followup = 12
#' )
#' # Example 7c: Cut for analysis when 12 months after at least 200 patients
#' # are enrolled in the biomarker-positive population, but we don't have a
#' # specific requirement for the biomarker-negative population. We also want
#' # there are at least 80% of the patients enrolled in the overall population.
#' # The remaining unused arguments as their default value of `NA`,
#' # so the analysis date is only decided by 12 months + max(time when there are
#' # 200 patients enrolled in the biomarker-positive stratum, time when there are
#' # 80% patients enrolled).
#' get_analysis_date(
#'   simulated_data,
#'   min_n_overall = n * 0.8,
#'   min_n_per_stratum = c(200, NA),
#'   min_followup = 12
#' )
get_analysis_date <- function(
    data,
    # Option 1: Planned calendar time for the analysis
    planned_calendar_time = NA,
    # Option 2: Reach targeted events
    target_event_overall = NA,
    target_event_per_stratum = NA,
    # Option 3: Max time extension to reach targeted events
    max_extension_for_target_event = NA,
    # Option 4: Planned minimum time after the previous analysis
    previous_analysis_date = 0,
    min_time_after_previous_analysis = NA,
    # Option 5: Minimal follow-up time after specified enrollment fraction
    min_n_overall = NA,
    min_n_per_stratum = NA,
    min_followup = NA) {
  input_check_scalar(planned_calendar_time)
  input_check_scalar(target_event_overall, require_whole_number = TRUE)
  input_check_scalar(max_extension_for_target_event)
  input_check_scalar(min_time_after_previous_analysis)
  input_check_scalar(min_n_overall, require_whole_number = TRUE)
  input_check_scalar(min_followup)
  input_check_vector(target_event_per_stratum, require_whole_number = TRUE)
  input_check_vector(min_n_per_stratum, require_whole_number = TRUE)

  # Check if `min_n_overall` is input by user
  cond1 <- !is.na(min_n_overall)
  # Check if `min_n_per_stratum` is input by user
  cond2 <- !all(is.na(min_n_per_stratum))

  n_max <- nrow(data)
  # if user input either `min_n_overall` or `min_n_per_stratum`, it is required to input `min_followup`.
  if (cond1 || cond2) {
    if (is.na(min_followup)) {
      stop("`min_followup` must be provided.")
    }
  }
  # if user input `min_n_overall` but it > n_max, then output error message
  if (cond1 && min_n_overall > n_max) {
    stop("`min_n_overall` must be a positive number less than or equal to the total sample size.")
  }
  # if user input `min_n_per_stratum` but sum of it > n_max, then output error message
  if (cond2 && sum(min_n_per_stratum, na.rm = TRUE) > n_max) {
    stop("`min_n_per_stratum` must be a sum of positive numbers less than or equal to the total sample size.")
  }

  data <- as.data.table(data)

  # Cutting option 1: Planned calendar time for the analysis
  cut_date1 <- planned_calendar_time

  # Cutting option 2: Reach targeted events
  # 2a: Reach targeted events of the overall population
  if (!is.na(target_event_overall)) {
    cut_date2a <- get_cut_date_by_event(data, event = target_event_overall)
  } else {
    cut_date2a <- NA
  }
  # 2b: Reach targeted events per sub-population
  if (!all(is.na(target_event_per_stratum))) {
    stratum <- unique(data$stratum)
    cut_date2b <- vector(mode = "list", length = length(stratum))
    for (i in seq_along(target_event_per_stratum)) {
      cut_date2b[[i]] <- get_cut_date_by_event(
        x = data[stratum == stratum[i], ],
        event = target_event_per_stratum[i]
      )
    }
    cut_date2b <- max(unlist(cut_date2b))
  } else {
    cut_date2b <- NA
  }
  cut_date2 <- pmax(cut_date2a, cut_date2b, na.rm = TRUE)

  # Cutting option 3: Max time extension to reach targeted events
  cut_date3 <- max_extension_for_target_event

  # Cutting option 4: Planned minimum time after the previous analysis
  cut_date4 <- previous_analysis_date + min_time_after_previous_analysis

  # Cutting option 5: Minimal follow-up time after specified enrollment fraction
  # 5a: At least 10 months after the 80% of the patients are enrolled
  if (!all(is.na(min_n_overall))) {
    data5a <- as.data.table(data)
    data5a <- data5a[order(enroll_time), ]
    data5a <- data5a[seq_len(pmin(min_n_overall, .N)), ]
    cut_date5a <- max(data5a$enroll_time) + min_followup
  } else {
    cut_date5a <- NA
  }
  # 5b: At least 10 months after the 80% biomarker positive patients are
  # enrolled and 70% biomarker negative patients are enrolled
  if (!all(is.na(min_n_per_stratum))) {
    cut_date5b <- vector(mode = "list", length = length(min_n_per_stratum))
    for (i in seq_along(min_n_per_stratum)) {
      if (is.na(min_n_per_stratum[i])) {
        cut_date5b[[i]] <- NA
        next
      }
      data5b <- as.data.table(data)
      data5b <- data5b[stratum == stratum[i], ]
      data5b <- data5b[order(enroll_time), ]
      data5b <- data5b[seq_len(pmin(min_n_per_stratum[i], .N)), ]
      cut_date5b[[i]] <- max(data5b$enroll_time)
    }
    cut_date5b <- max(unlist(cut_date5b), na.rm = TRUE) + min_followup
  } else {
    cut_date5b <- NA
  }
  cut_date5 <- pmax(cut_date5a, cut_date5b, na.rm = TRUE)

  # Combining all 5 cutting options
  cut_date <- pmin(pmax(cut_date1, cut_date2, cut_date4, cut_date5, na.rm = TRUE), cut_date3, na.rm = TRUE)

  cut_date
}
