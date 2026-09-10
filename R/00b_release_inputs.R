# ===============================================
# 00b - Release input tables (amendments A33 to A36, 10 September 2026)
# ===============================================
# Derives the release-candidate analysis inputs from the historical derived
# tables without touching them. The historical layer (data/derived/
# prevalence.csv, trials.csv) reproduces the 2 September fixtures byte for
# byte; everything decided on 10 September 2026 is applied here:
#   1. release-layer ledger corrections (data/review/correction_ledger.csv,
#      layer == "release", C15 onwards, amendment A36);
#   2. population decisions (data/review/population_eligibility.csv,
#      decision_a24_strict and decision_a33, amendments A24 and A33);
#   3. construct, window and baseline-selection decisions
#      (data/review/release_construct_review.csv, amendment A36).
# Membership rules are mechanical: no set is defined by naming studies.
# Outputs: data/derived/release_prevalence.csv, release_trials.csv,
# release_trial_arms.csv and data/derived/release_membership_report.md.
# ===============================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
})
source(if (file.exists("R/utils.R")) "R/utils.R" else "utils.R")

derived_dir <- root_path("data", "derived")
prevalence <- read_csv(file.path(derived_dir, "prevalence.csv"), show_col_types = FALSE)
trials     <- read_csv(file.path(derived_dir, "trials.csv"), show_col_types = FALSE)

# -------------------------------
# 1. Release-layer ledger corrections
# -------------------------------
ledger <- read_csv(root_path("data", "review", "correction_ledger.csv"),
                   col_types = cols(.default = col_character()), na = character(0))
release_corr <- ledger %>% filter(layer == "release")
check(nrow(release_corr) > 0, "no release-layer ledger rows found")
check(all(release_corr$approver != "" & release_corr$decided_date != ""),
      "a release-layer correction lacks an approver or date")

apply_corrections <- function(tab, corr, effect) {
  corr <- corr %>% filter(effect_type == effect)
  for (i in seq_len(nrow(corr))) {
    cr  <- corr[i, ]
    idx <- which(tab$study_id_clean == cr$study_id_clean)
    check(length(idx) == 1, paste("release correction target not unique:", cr$study_id_clean, effect))
    check(cr$field %in% names(tab), paste("release correction field not in table:", cr$field))
    current <- tab[[cr$field]][idx]
    current_chr <- if (is.na(current)) "" else as.character(current)
    check(current_chr == cr$raw,
          paste0("release-layer before-value mismatch for ", cr$study_id_clean, " ", cr$field,
                 ": found '", current_chr, "', expected '", cr$raw, "'"))
    tab[[cr$field]][idx] <- if (is.numeric(tab[[cr$field]])) as.numeric(cr$corrected) else cr$corrected
  }
  tab
}
prevalence <- apply_corrections(prevalence, release_corr, "proportion")
trials     <- apply_corrections(trials, release_corr, "trial_binary")
trials     <- trials %>% mutate(rr_computed = (events_intervention / n_intervention) /
                                  (events_control / n_control))

# -------------------------------
# 2. Population decisions (A24 strict, A33)
# -------------------------------
elig <- read_csv(root_path("data", "review", "population_eligibility.csv"),
                 col_types = cols(.default = col_character()))
check(all(c("decision_a24_strict", "decision_a33") %in% names(elig)),
      "population_eligibility.csv lacks decision_a24_strict or decision_a33")
check(all(elig$checker == "RT" & elig$check_date == "2026-09-10"),
      "population eligibility rows are not all checked by RT on 2026-09-10")
pop <- elig %>%
  transmute(study_id_clean,
            conflict_grade = conflict_with_prospero_v2,
            a24_strict_eligible = str_detect(decision_a24_strict, "^eligible"),
            a33_eligible = str_detect(decision_a33, "^eligible"),
            a33_duration_sens_only = str_detect(decision_a33, "duration sensitivity only"),
            decision_a33)
check(all(prevalence$study_id_clean %in% pop$study_id_clean),
      "a prevalence study has no population decision")
check(all(trials$study_id_clean %in% pop$study_id_clean),
      "a trial study has no population decision")
check(all(pop$a24_strict_eligible == (pop$conflict_grade == "none")),
      "A24 strict decision must equal 'conflict grade none' (mechanical rule)")
check(!any(pop$a33_eligible & pop$a33_duration_sens_only),
      "a report is both A33-eligible and duration-sensitivity-only")

# -------------------------------
# 3. Construct, window and baseline-selection review (A36)
# -------------------------------
cons <- read_csv(root_path("data", "review", "release_construct_review.csv"),
                 col_types = cols(.default = col_character()))
check(setequal(cons$study_id_clean, prevalence$study_id_clean),
      "release_construct_review.csv does not cover exactly the 23 prevalence studies")
check(all(cons$reviewer == "RT" & cons$review_date == "2026-09-10"), "construct review not signed by RT on 2026-09-10")
cons <- cons %>%
  transmute(study_id_clean,
            construct_verified,
            usable_point = usable_point_prevalence == "yes",
            usable_period = usable_period_prevalence == "yes",
            window_status,
            variable_window = str_detect(window_status, "^variable"),
            baseline_selected = baseline_selection != "none",
            construct_reason = reason)

# -------------------------------
# 4. Prevalence membership (mechanical)
# -------------------------------
release_prev <- prevalence %>%
  left_join(pop, by = "study_id_clean") %>%
  left_join(cons, by = "study_id_clean") %>%
  mutate(
    in_window = timepoint_months >= 6 & timepoint_months <= 24 & !variable_window,
    # A25 point-prevalence estimand: paid or competitive employment, point
    # basis, fixed assessment inside 6 to 24 months, not selected on baseline
    # employment, one contribution per cohort.
    point_measure_ok = outcome_basis == "point" & usable_point & in_window &
      !baseline_selected & !overlapping_cohort,
    period_measure_ok = outcome_basis == "period" & usable_period & in_window & !baseline_selected,
    # Sets
    in_a33_point_primary       = point_measure_ok & a33_eligible,
    in_a33_point_duration_sens = point_measure_ok & (a33_eligible | a33_duration_sens_only),
    in_a33_period              = period_measure_ok & (a33_eligible | a33_duration_sens_only),
    in_a33_variable_window     = variable_window & outcome_basis == "period" &
      (a33_eligible | a33_duration_sens_only) & !baseline_selected,
    in_strict_a24_point        = point_measure_ok & a24_strict_eligible,
    # One-line reason for the membership report
    membership_reason = case_when(
      in_a33_point_primary ~ "A33 point-prevalence primary set",
      in_a33_point_duration_sens ~ "A33 duration sensitivity only (population duration criterion exceeds five years)",
      in_a33_period ~ "Period measure: A33 period set only",
      in_a33_variable_window ~ "Variable follow-up: separately labelled analysis only",
      overlapping_cohort ~ "Cohort overlap (same service as a retained report): excluded from every pooled set",
      baseline_selected ~ "Sample selected on baseline employment: not a prevalence sample",
      !usable_point & !usable_period ~ paste0("No usable employment count: ", construct_reason),
      !a33_eligible & !a33_duration_sens_only ~ "Population not eligible under A33",
      TRUE ~ "Not selected (see construct review)"
    )
  )

# -------------------------------
# 5. Trial membership (mechanical)
# -------------------------------
# A25 trial estimand: IPS versus usual care, employment attained during the
# earliest post-randomisation six-month interval with usable per-arm counts.
# A result qualifies when its (corrected) timepoint is 6 months and its
# outcome is a period measure; comparison type must be IPS added to usual care.
release_trials <- trials %>%
  left_join(pop, by = "study_id_clean") %>%
  mutate(
    six_month_interval = timepoint_months == 6 & outcome_basis == "period",
    ips_vs_tau = str_detect(comparison_type, "^IPS added to TAU"),
    in_trial_a33_k3_0to6 = six_month_interval & ips_vs_tau & a33_eligible,
    in_trial_strict = six_month_interval & ips_vs_tau & a24_strict_eligible,
    release_role = case_when(
      in_trial_a33_k3_0to6 & in_trial_strict ~ "A33 pool and strict comparison",
      in_trial_a33_k3_0to6 ~ "A33 pool (registered-ineligible diagnoses reported)",
      analysis_set == "sensitivity_k4" ~ "Descriptive only: no six-month interval; active vocational comparator (A36)",
      TRUE ~ paste0("Not pooled: ", comparison_type)
    )
  )

check(all(release_trials$events_intervention <= release_trials$n_intervention &
            release_trials$events_control <= release_trials$n_control), "trial events exceed denominators")
check(all(release_prev$events <= release_prev$n_assessed), "release prevalence events exceed n_assessed")

release_arms <- bind_rows(
  release_trials %>% filter(in_trial_a33_k3_0to6) %>%
    transmute(study_id_clean, study_label, arm = "intervention", arm_ips = 1L,
              events = events_intervention, n_analysed = n_intervention, n_full = n_intervention_full),
  release_trials %>% filter(in_trial_a33_k3_0to6) %>%
    transmute(study_id_clean, study_label, arm = "control", arm_ips = 0L,
              events = events_control, n_analysed = n_control, n_full = n_control_full)
) %>% arrange(study_id_clean, desc(arm_ips))

# -------------------------------
# 6. Assertions that encode the approved rules (not the pool sizes)
# -------------------------------
# The strict registered rule must leave nothing pooled (recorded consequence,
# A35); this assertion guards against a silent re-inclusion, not a target.
check(sum(release_prev$in_strict_a24_point) == 0,
      "strict A24 point set is not empty; a population grade changed without an amendment")
check(sum(release_trials$in_trial_strict) == 1 &&
        release_trials$study_id_clean[release_trials$in_trial_strict] == "KILLACKEY_2008",
      "strict trial set should be Killackey 2008 alone (A35)")
check(!any(release_prev$in_a33_point_primary & release_prev$definition_class == "retention"),
      "a job-retention row entered the A33 point set")
check(!any(release_prev$in_a33_point_primary & release_prev$overlapping_cohort),
      "an overlapping cohort entered the A33 point set")
check(all(release_trials$timepoint_months[release_trials$in_trial_a33_k3_0to6] == 6),
      "an A33 trial result is not a six-month interval")

# -------------------------------
# 7. Write outputs
# -------------------------------
write_csv(release_prev %>% select(-decision_a33), file.path(derived_dir, "release_prevalence.csv"))
write_csv(release_trials %>% select(-decision_a33), file.path(derived_dir, "release_trials.csv"))
write_csv(release_arms, file.path(derived_dir, "release_trial_arms.csv"))

fmt_set <- function(x) if (length(x) == 0) "(none)" else paste(x, collapse = ", ")
rep <- c(
  "# Release membership report (generated by R/00b_release_inputs.R)",
  "",
  "Rules: A33 population (docs/amendment_2026-09-06.md), A24 strict comparison, A25 measurement rule as approved on 10 September 2026 (A36). Membership is derived mechanically from data/review/population_eligibility.csv, data/review/release_construct_review.csv and the release-layer ledger rows.",
  "",
  "## Prevalence sets",
  "",
  paste0("- A33 point-prevalence primary (k = ", sum(release_prev$in_a33_point_primary), "): ",
         fmt_set(release_prev$study_id_clean[release_prev$in_a33_point_primary])),
  paste0("- A33 duration sensitivity (k = ", sum(release_prev$in_a33_point_duration_sens), "): ",
         fmt_set(release_prev$study_id_clean[release_prev$in_a33_point_duration_sens])),
  paste0("- A33 period set (k = ", sum(release_prev$in_a33_period), "): ",
         fmt_set(release_prev$study_id_clean[release_prev$in_a33_period])),
  paste0("- Variable follow-up (k = ", sum(release_prev$in_a33_variable_window), "): ",
         fmt_set(release_prev$study_id_clean[release_prev$in_a33_variable_window])),
  paste0("- Strict A24 point set (k = ", sum(release_prev$in_strict_a24_point), "): ",
         fmt_set(release_prev$study_id_clean[release_prev$in_strict_a24_point])),
  "",
  "| Study | Basis | Months | Events / n | Conflict grade | A33 decision | Membership |",
  "|---|---|---:|---:|---|---|---|",
  with(release_prev, paste0("| ", study_id_clean, " | ", outcome_basis, " | ", timepoint_months, " | ",
                            events, " / ", n_assessed, " | ", conflict_grade, " | ",
                            ifelse(a33_eligible, "eligible", ifelse(a33_duration_sens_only, "duration sensitivity", "not eligible")),
                            " | ", membership_reason, " |")),
  "",
  "## Trial sets",
  "",
  paste0("- A33 IPS pool, months 0 to 6 (k = ", sum(release_trials$in_trial_a33_k3_0to6), "): ",
         fmt_set(release_trials$study_id_clean[release_trials$in_trial_a33_k3_0to6])),
  paste0("- Strict comparison (k = ", sum(release_trials$in_trial_strict), "): ",
         fmt_set(release_trials$study_id_clean[release_trials$in_trial_strict])),
  "",
  "| Study | Months | Basis | IPS events / n | Control events / n | RR | Conflict grade | Release role |",
  "|---|---:|---|---:|---:|---:|---|---|",
  with(release_trials, paste0("| ", study_id_clean, " | ", timepoint_months, " | ", outcome_basis, " | ",
                              events_intervention, " / ", n_intervention, " | ", events_control, " / ", n_control,
                              " | ", formatC(rr_computed, format = "f", digits = 2), " | ", conflict_grade,
                              " | ", release_role, " |")),
  "",
  "## Release-layer corrections applied",
  "",
  with(release_corr, paste0("- ", correction_id, " ", study_id_clean, " ", effect_type, " `", field, "`: ",
                            raw, " to ", corrected, " (", amendment_ref, ", ", approver, ")"))
)
writeLines(rep, file.path(derived_dir, "release_membership_report.md"))
cat(paste(rep, collapse = "\n"), "\n")
