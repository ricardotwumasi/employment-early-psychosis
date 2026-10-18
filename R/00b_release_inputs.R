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
# Release candidate 2 (amendment A42, 16 October 2026) adds:
#   4. results from reports outside the historical extraction, read from
#      data/review/release_new_results.csv; each row carries its own
#      population, construct and approval fields and passes through the same
#      membership predicates as the historical rows;
#   5. a dated approval rule in place of fixed reviewer dates: every decision
#      row names a reviewer and a date on or after the decision date of the
#      amendment it cites (data/review/amendment_dates.csv);
#   6. one contribution per cohort in every pooled set.
# Release candidate 3 (amendment A46, 18 October 2026) adds:
#   7. the vocational-exposure classification of the point primary results
#      (data/review/release_vocational_exposure.csv) for the pre-specified
#      sensitivity excluding samples with a universal vocational offer.
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
# 0. Dated approval rule (A42)
# -------------------------------
# A decision row is approved when it names a reviewer and carries a date on
# or after the decision date of the amendment it cites. The amendment is the
# leading "A<number>" of the reference, so "A33 (A24 strict recorded in ...)"
# resolves to A33.
amend_dates <- read_csv(root_path("data", "review", "amendment_dates.csv"),
                        col_types = cols(.default = col_character()))
as_day <- function(x) suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
amendment_date <- function(ref) {
  d <- as_day(amend_dates$decision_date[match(str_extract(ref, "^A[0-9]+"), amend_dates$amendment_ref)])
  check(!anyNA(d), paste("amendment without a recorded decision date:", paste(unique(ref[is.na(d)]), collapse = ", ")))
  d
}
dated_approval <- function(who, when, ref) {
  !is.na(who) & nzchar(who) & !is.na(as_day(when)) & as_day(when) >= amendment_date(ref)
}

# -------------------------------
# 1. Release-layer ledger corrections
# -------------------------------
ledger <- read_csv(root_path("data", "review", "correction_ledger.csv"),
                   col_types = cols(.default = col_character()), na = character(0))
release_corr <- ledger %>% filter(layer == "release")
check(nrow(release_corr) > 0, "no release-layer ledger rows found")
check(all(dated_approval(release_corr$approver, release_corr$decided_date, release_corr$amendment_ref)),
      "a release-layer correction lacks an approver or a decision date on or after its amendment")

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
check(all(dated_approval(elig$checker, elig$check_date, elig$amendment_ref)),
      "a population eligibility row lacks a checker or a check date on or after its amendment")
check(!anyDuplicated(elig$study_id_clean) && all(nzchar(elig$cohort_id)),
      "population_eligibility.csv: duplicated study or missing cohort_id")
pop <- elig %>%
  transmute(study_id_clean,
            cohort_id,
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
check(all(dated_approval(cons$reviewer, cons$review_date, cons$amendment_ref)),
      "a construct review row lacks a reviewer or a review date on or after its amendment")
cons <- cons %>%
  transmute(study_id_clean,
            result_uid = candidate_result_uid,
            construct_verified,
            usable_point = usable_point_prevalence == "yes",
            usable_period = usable_period_prevalence == "yes",
            window_status,
            variable_window = str_detect(window_status, "^variable"),
            baseline_selected = baseline_selection != "none",
            construct_reason = reason)

# -------------------------------
# 3b. Results from reports outside the historical extraction (A42)
# -------------------------------
# One row per adopted result, keyed by result_uid. Each row carries the
# population decisions (as in population_eligibility.csv), the construct and
# window decisions (as in release_construct_review.csv) and its approval, and
# is validated before it can enter any set.
newres <- read_csv(root_path("data", "review", "release_new_results.csv"),
                   col_types = cols(.default = col_character()), na = character(0))
is_count <- function(x) grepl("^[0-9]+$", x)
check(nrow(newres) > 0 && all(nzchar(newres$result_uid)) && !anyDuplicated(newres$result_uid),
      "release_new_results.csv: result_uid missing or duplicated")
check(!any(newres$study_id_clean %in% prevalence$study_id_clean),
      "a new result belongs to a study in the historical extraction; correct it through the release-layer ledger instead")
check(all(nzchar(newres$cohort_id) & nzchar(newres$study_id_clean)), "a new result lacks a study or cohort identifier")
check(all(is_count(newres$events) & is_count(newres$n_assessed) & is_count(newres$n_total)),
      "new result counts must be non-negative integers")
check(all(as.integer(newres$n_assessed) > 0 & as.integer(newres$events) <= as.integer(newres$n_assessed) &
            as.integer(newres$n_total) >= as.integer(newres$n_assessed)),
      "a new result has a zero denominator, more events than assessed, or a full-cohort total below the number assessed")
check(all(newres$outcome_basis %in% c("point", "period")), "a new result has an outcome basis other than point or period")
check(all(newres$conflict_grade %in% c("none", "possible", "definite")), "a new result has an unknown conflict grade")
check(all(newres$overlapping_cohort %in% c("TRUE", "FALSE") & newres$baseline_selected %in% c("TRUE", "FALSE")),
      "a new result has a non-logical overlap or baseline-selection flag")
check(all(dated_approval(newres$approver, newres$adoption_date, newres$amendment_ref)),
      "a new result lacks an approver or an adoption date on or after its amendment")
check(all(nzchar(newres$source_check_by) & !is.na(as_day(newres$source_check_date)) &
            as_day(newres$source_check_date) <= as_day(newres$adoption_date)),
      "a new result lacks a source check dated on or before its adoption")
num <- function(x) suppressWarnings(as.numeric(x))
new_prev <- newres %>%
  transmute(study_id_clean, study_label, author, year = num(year), year_online = num(year_online), country, region,
            design, diagnosis, n_total = num(n_total), timepoint_months = num(timepoint_months),
            follow_up_months = num(follow_up_months), outcome_basis, outcome_measure, outcome_definition,
            definition_class, overlapping_cohort = overlapping_cohort == "TRUE",
            events = num(events), n_assessed = num(n_assessed), vocational_exposure,
            quality_rating = na_if(quality_rating, ""), result_id = result_uid,
            cohort_id, conflict_grade,
            a24_strict_eligible = str_detect(decision_a24_strict, "^eligible"),
            a33_eligible = str_detect(decision_a33, "^eligible"),
            a33_duration_sens_only = str_detect(decision_a33, "duration sensitivity only"),
            decision_a33, result_uid, construct_verified = outcome_definition,
            usable_point = outcome_basis == "point", usable_period = outcome_basis == "period",
            window_status, variable_window = str_detect(window_status, "^variable"),
            baseline_selected = baseline_selected == "TRUE", construct_reason = note,
            source_page, approver, adoption_date, amendment_ref)
check(all(new_prev$a24_strict_eligible == (new_prev$conflict_grade == "none")),
      "A24 strict decision of a new result must equal 'conflict grade none' (mechanical rule)")

# -------------------------------
# 4. Prevalence membership (mechanical)
# -------------------------------
release_prev <- prevalence %>%
  left_join(pop, by = "study_id_clean", relationship = "one-to-one") %>%
  left_join(cons, by = "study_id_clean", relationship = "one-to-one") %>%
  bind_rows(new_prev) %>%
  mutate(
    in_window = !is.na(timepoint_months) & timepoint_months >= 6 & timepoint_months <= 24 & !variable_window,
    # A25 point-prevalence estimand: paid or competitive employment, point
    # basis, fixed assessment inside 6 to 24 months, not selected on baseline
    # employment, one contribution per cohort.
    point_measure_ok = outcome_basis == "point" & usable_point & in_window &
      !baseline_selected & !overlapping_cohort,
    # Period estimand (A42): employment at any time during a fixed interval
    # ending inside 6 to 24 months; one contribution per cohort.
    period_measure_ok = outcome_basis == "period" & usable_period & in_window & !baseline_selected &
      !overlapping_cohort,
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
      !usable_point & !usable_period & str_detect(construct_reason, "^Excluded") ~ construct_reason,
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
# Pool sizes are findings, not constraints (RT, 23 September 2026). A silent
# re-inclusion is prevented instead by the dated approval rule above: no
# population or construct decision can change without a dated row citing an
# amendment. The strict sets are defined by the A24 decision column alone,
# and tests/testthat/test-release-inputs.R recomputes every set from the
# decision tables independently of this script.
# One contribution per cohort in every pooled set (A42). The variable-window
# observations are reported one by one and never pooled, so two results from
# the same programme may both appear there.
for (set in c("in_a33_point_primary", "in_a33_point_duration_sens", "in_a33_period", "in_strict_a24_point")) {
  ids <- release_prev$cohort_id[release_prev[[set]]]
  check(!anyNA(ids) && !anyDuplicated(ids), paste0(set, ": a cohort contributes more than once or lacks a cohort_id"))
}
for (set in c("in_trial_a33_k3_0to6", "in_trial_strict")) {
  ids <- release_trials$cohort_id[release_trials[[set]]]
  check(!anyNA(ids) && !anyDuplicated(ids), paste0(set, ": a cohort contributes more than once or lacks a cohort_id"))
}
check(!anyDuplicated(release_prev$result_uid[!is.na(release_prev$result_uid)]), "a result_uid appears twice in the release prevalence table")
check(!any(release_prev$in_a33_point_primary & release_prev$definition_class == "retention"),
      "a job-retention row entered the A33 point set")
check(!any(release_prev$in_a33_point_primary & release_prev$overlapping_cohort),
      "an overlapping cohort entered the A33 point set")
check(all(release_trials$timepoint_months[release_trials$in_trial_a33_k3_0to6] == 6),
      "an A33 trial result is not a six-month interval")

# -------------------------------
# 6b. Vocational-exposure classification (A46)
# -------------------------------
# The pre-specified sensitivity (specification of 1 September 2026) drops
# samples in which every participant was offered a vocational intervention.
# A46 applies it to the point primary set: only a universal offer confirmed in
# the source (all_offered) excludes a cohort. Unknown is not none, so
# no_offer_reported and unclear cohorts are retained, as are some_offered.
# Every point primary result must be classified, so a change to the primary
# set cannot pass without a dated classification. Run after the checks above,
# so the set it restricts has already passed them.
expo <- read_csv(root_path("data", "review", "release_vocational_exposure.csv"),
                 col_types = cols(.default = col_character()), na = character(0))
check(all(expo$exposure_class %in% c("all_offered", "some_offered", "no_offer_reported", "unclear")),
      "release_vocational_exposure.csv: an exposure class outside the controlled vocabulary")
check(!anyDuplicated(expo$result_uid), "release_vocational_exposure.csv: a result_uid is classified twice")
check(all(dated_approval(expo$reviewer, expo$review_date, expo$amendment_ref) & nzchar(expo$source_page)),
      "an exposure classification lacks a reviewer, a source page or a review date on or after its amendment")
primary_uids <- release_prev$result_uid[release_prev$in_a33_point_primary]
check(setequal(expo$result_uid, primary_uids),
      paste("release_vocational_exposure.csv must classify exactly the point primary results; unclassified:",
            paste(setdiff(primary_uids, expo$result_uid), collapse = ", "),
            "; not in the primary set:", paste(setdiff(expo$result_uid, primary_uids), collapse = ", ")))
check(all(expo$cohort_id == release_prev$cohort_id[match(expo$result_uid, release_prev$result_uid)]),
      "release_vocational_exposure.csv: a cohort_id disagrees with the release prevalence table")
release_prev <- release_prev %>%
  left_join(expo %>% select(result_uid, exposure_class), by = "result_uid", relationship = "many-to-one") %>%
  mutate(in_a33_point_excl_all_offered = in_a33_point_primary & exposure_class != "all_offered")

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
  "Rules: A33 population (docs/amendment_2026-09-06.md), A24 strict comparison, A25 measurement rule as approved on 10 September 2026 (A36), period selection rule and new results (A42, 16 October 2026). Membership is derived mechanically from data/review/population_eligibility.csv, data/review/release_construct_review.csv, data/review/release_new_results.csv and the release-layer ledger rows.",
  "",
  "## Prevalence sets",
  "",
  paste0("- A33 point-prevalence primary (k = ", sum(release_prev$in_a33_point_primary), "): ",
         fmt_set(release_prev$study_id_clean[release_prev$in_a33_point_primary])),
  paste0("- A33 duration sensitivity (k = ", sum(release_prev$in_a33_point_duration_sens), "): ",
         fmt_set(release_prev$study_id_clean[release_prev$in_a33_point_duration_sens])),
  paste0("- A33 point primary excluding a confirmed universal vocational offer (A46; k = ",
         sum(release_prev$in_a33_point_excl_all_offered), "): ",
         fmt_set(release_prev$study_id_clean[release_prev$in_a33_point_excl_all_offered])),
  paste0("- A33 period set (k = ", sum(release_prev$in_a33_period), "): ",
         fmt_set(release_prev$study_id_clean[release_prev$in_a33_period])),
  paste0("- Variable follow-up, reported one by one and never pooled (", sum(release_prev$in_a33_variable_window), " results): ",
         fmt_set(paste0(release_prev$study_label, " [", release_prev$result_uid, "]")[release_prev$in_a33_variable_window])),
  paste0("- Strict A24 point set (k = ", sum(release_prev$in_strict_a24_point), "): ",
         fmt_set(release_prev$study_id_clean[release_prev$in_strict_a24_point])),
  "",
  "| Study | Result | Cohort | Basis | Months | Events / n | Conflict grade | A33 decision | Membership |",
  "|---|---|---|---|---:|---:|---|---|---|",
  with(release_prev, paste0("| ", study_id_clean, " | ", result_uid, " | ", cohort_id, " | ", outcome_basis, " | ", timepoint_months, " | ",
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
