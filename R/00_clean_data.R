# ===============================================
# 00 - Data layer: read, correct, derive and assert
# ===============================================
# Reads the raw extraction file (data/yanan_data_280826.csv, never edited),
# applies the source-verified corrections listed in the `corrections` table,
# derives the analysis tables written to data/derived/, and stops with a clear
# message if any pre-specified assertion fails. Everything downstream reads
# the derived tables only.
#
# Identifiers added here:
#   study_id_clean  one per study (author key plus citable print year); resolves
#                   the three raw study_id collisions
#   report_id       row order in the raw file
#   result_id       study_id_clean + effect type + outcome + timepoint
# ===============================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
})
source(if (file.exists("R/utils.R")) "R/utils.R" else "utils.R")

raw_file    <- root_path("data", "yanan_data_280826.csv")
derived_dir <- root_path("data", "derived")
dir.create(derived_dir, showWarnings = FALSE, recursive = TRUE)

# -------------------------------
# 1. Read the raw bytes and repair the only non-ASCII content
# -------------------------------
# The file is ASCII apart from GB2312 full-width parentheses (bytes A3A8 and
# A3A9) around a few parenthetical notes and one non-breaking space (A0). They
# are replaced byte-wise so that the parsed text is plain ASCII.
bytes <- readBin(raw_file, what = "raw", n = file.info(raw_file)$size)
txt   <- rawToChar(bytes)
Encoding(txt) <- "bytes"
txt <- gsub(rawToChar(as.raw(c(0xa3, 0xa8))), " (", txt, fixed = TRUE, useBytes = TRUE)
txt <- gsub(rawToChar(as.raw(c(0xa3, 0xa9))), ") ", txt, fixed = TRUE, useBytes = TRUE)
txt <- gsub(rawToChar(as.raw(0xa0)), " ", txt, fixed = TRUE, useBytes = TRUE)
check(!grepl("[^\\x01-\\x7f]", txt, perl = TRUE, useBytes = TRUE),
      "non-ASCII bytes remain after the three known replacements")
Encoding(txt) <- "UTF-8"

na_tokens <- c("", "NA", "N/A", "/")
raw <- read_csv(I(txt), col_types = cols(.default = col_character()),
                na = character(0), show_col_types = FALSE, progress = FALSE)
raw <- raw %>% mutate(across(everything(), ~ str_squish(.x)))
raw$report_id <- seq_len(nrow(raw))
check(nrow(raw) == 44, "expected 44 raw rows")

# -------------------------------
# 2. Types
# -------------------------------
numeric_columns <- c(
  "year", "n_total", "pct_male", "mean_age", "sd_age", "follow_up_months",
  "timepoint_months", "events", "n_assessed", "events_intervention",
  "n_intervention", "events_control", "n_control", "r", "n_r",
  "mean1", "sd1", "n1", "mean2", "sd2", "n2",
  "reported_es", "reported_ci_low", "reported_ci_high"
)
dat <- raw
for (col in numeric_columns) {
  parsed <- suppressWarnings(parse_number(dat[[col]], na = na_tokens))
  # NA may only arise from the agreed missing-value tokens
  check(all(is.na(parsed) == (dat[[col]] %in% na_tokens)),
        paste("unexpected NA when parsing", col))
  dat[[col]] <- parsed
}

# -------------------------------
# 3. Study-level identifiers
# -------------------------------
# Citable print years where the extraction used the online-first year.
print_year <- c(DUDLEY_2013 = 2014, HEGELSTAD_2018 = 2019, TAPFUMANEYI_2014 = 2015)

dat <- dat %>%
  mutate(
    author_key  = toupper(str_replace_all(author, "[^A-Za-z]", "")),
    year_online = year,
    study_key   = paste0(author_key, "_", year_online),
    year        = if_else(study_key %in% names(print_year),
                          unname(print_year[study_key]), year),
    study_id_clean = paste0(author_key, "_", year),
    study_label    = paste(author, year),
    effect_type    = str_to_lower(effect_type),
    outcome_basis  = str_to_lower(outcome_basis),
    quality_rating = str_to_title(quality_rating),
    quality_tool   = str_to_upper(quality_tool)
  )

# The raw study_id column collides for three pairs of distinct studies. The
# set of collisions is asserted so that any future edit introducing a new one
# is caught.
collisions <- dat %>%
  distinct(study_id, study_id_clean) %>%
  count(study_id) %>%
  filter(n > 1) %>%
  pull(study_id)
check(setequal(collisions, c("KILLACKEY_001", "KILLACKEY_002", "RINALDI_001")),
      "raw study_id collision set is not exactly {KILLACKEY_001, KILLACKEY_002, RINALDI_001}")

dat <- dat %>%
  mutate(result_id = paste(study_id_clean, effect_type,
                           str_replace_all(str_to_lower(outcome_measure), "[^a-z0-9]+", "_"),
                           paste0(timepoint_months, "m"), sep = "__"))
check(!any(duplicated(dat$result_id)), "result_id is not unique")

# -------------------------------
# 4. Source-verified corrections (raw CSV untouched)
# -------------------------------
corrections <- tribble(
  ~study_id_clean, ~effect_type, ~field, ~raw, ~corrected, ~source, ~reason,
  "KILLACKEY_2019", "proportion", "timepoint_months", "12", "6",
  "Killackey et al. 2019, Br J Psychiatry 214(2):76-82, Results",
  "76/126 equals the pooled 0-6 month arm counts (47+29 over 66+60); the paper gives no 12-month per-arm counts and 126 cannot be a 12-month denominator given 20.5% and 28.8% missing at 12 months",
  "KILLACKEY_2019", "proportion", "follow_up_months", "12", "18",
  "Killackey et al. 2019, Methods",
  "Trial follow-up was 18 months; aligns the proportion row with the trial row",
  "KILLACKEY_2019", "proportion", "outcome_definition",
  "Worked at least 1 day in paid employment during 6_12 month follow-up period",
  "Worked at least 1 day in paid employment during 0_6 month follow-up period (arms pooled)",
  "Killackey et al. 2019, Results", "Label corrected to match the counts",
  "CRAIG_2014", "proportion", "n_assessed", "159", "134",
  "Craig et al. 2014, Br J Psychiatry 205(2):145-150, Table 2 (68 + 66 assessed)",
  "Available-case rule: the prevalence denominator is the number assessed, consistent with the trial row",
  "HEGELSTAD_2019", "proportion", "n_assessed", "60", "57",
  "Hegelstad et al. 2019, Early Interv Psychiatry 13(4):859-866, Results (30 completers; 3 controls lacked employment data at 1 year)",
  "Available-case rule: 14/30 intervention plus 2/27 controls",
  "HEGELSTAD_2019", "trial_binary", "n_control", "30", "27",
  "Hegelstad et al. 2019, Results", "Three matched controls lacked employment status at 1 year",
  "EACK_2011", "proportion", "outcome_definition",
  "Paid competitive employment at 12-month follow-up",
  "Paid competitive employment at 24-month follow-up (end of treatment)",
  "Eack et al. 2011, Res Soc Work Pract 21(1):32-42, Table 1 (PMC3718562)",
  "Employment outcomes are reported for the 46 two-year completers; label corrected to the 24-month timepoint",
  "EACK_2011", "proportion", "events", "12", "17",
  "Eack et al. 2011, Table 1 (PMC3718562): competitively employed CET 13 of 24 (54%), EST 4 of 22 (18%)",
  "The extracted 9 and 3 do not appear in any row of Table 1; counts replaced by the published end-of-treatment figures",
  "EACK_2011", "trial_binary", "events_intervention", "9", "13",
  "Eack et al. 2011, Table 1 (PMC3718562)", "As above",
  "EACK_2011", "trial_binary", "events_control", "3", "4",
  "Eack et al. 2011, Table 1 (PMC3718562)", "As above",
  "TURNER_2019", "proportion", "notes", "",
  "Follow-up was a mean of 19 months (range 2 to 44), not a fixed 18 months",
  "Turner et al. 2019, Ir J Occup Ther 47(2):114-123", "Table 1 footnote",
  "ERICKSON_2021", "trial_binary", "notes", "",
  "Denominators 47 and 50 reconstructed from the printed counts and percentages (34, 72.3%; 25, 50.0%) in Table 2; randomised 56 vs 53",
  "Erickson et al. 2021, Early Interv Psychiatry 15(3):662-668, Table 2", "Extraction note",
  "NUECHTERLEIN_2020", "trial_binary", "notes", "",
  "Competitive employment during months 7 to 18, available case (36 of 41 and 15 of 22 with outcome data); randomised 46 vs 23",
  "Nuechterlein et al. 2020, Psychol Med 50(1):20-28, Results", "Extraction note",
  "FOWLER_2019", "trial_binary", "notes", "",
  "Non-affective subgroup of the ISREP 24-month follow-up (77 randomised)",
  "Fowler et al. 2019, Schizophr Res 203:99-104", "Extraction note"
)

# Keep the pre-correction values so that the dissertation's figures can be
# reproduced exactly (01_frequentist_registered.R, "as submitted" model).
dat <- dat %>%
  mutate(events_raw = events, n_assessed_raw = n_assessed,
         timepoint_months_raw = timepoint_months,
         events_intervention_raw = events_intervention,
         events_control_raw = events_control, n_control_raw = n_control)

for (i in seq_len(nrow(corrections))) {
  cr  <- corrections[i, ]
  idx <- which(dat$study_id_clean == cr$study_id_clean & dat$effect_type == cr$effect_type)
  check(length(idx) == 1, paste("correction target not unique:", cr$study_id_clean, cr$effect_type))
  current <- dat[[cr$field]][idx]
  current_chr <- if (is.na(current)) "" else as.character(current)
  check(current_chr == cr$raw,
        paste0("raw value mismatch for ", cr$study_id_clean, " ", cr$field,
               ": found '", current_chr, "', expected '", cr$raw, "'"))
  dat[[cr$field]][idx] <- if (is.numeric(dat[[cr$field]])) as.numeric(cr$corrected) else cr$corrected
}
write_csv(corrections, file.path(derived_dir, "corrections.csv"))

# Full denominators for the missing-as-not-employed sensitivity (randomised,
# or matched group size for the non-randomised Hegelstad comparison).
full_denominators <- tribble(
  ~study_id_clean, ~n_intervention_full, ~n_control_full, ~denominator_source,
  "ERICKSON_2021", 56, 53, "randomised (Erickson 2021, Table 1)",
  "KILLACKEY_2008", 20, 21, "randomised; all 41 followed (Killackey 2008)",
  "KILLACKEY_2019", 73, 73, "randomised 1:1 (Killackey 2019, Methods)",
  "NUECHTERLEIN_2020", 46, 23, "randomised (Nuechterlein 2020)",
  "HEGELSTAD_2019", 30, 30, "matched groups remaining at 1 year (Hegelstad 2019)"
)

# -------------------------------
# 5. Prevalence table
# -------------------------------
prevalence <- dat %>%
  filter(effect_type == "proportion") %>%
  left_join(full_denominators %>% select(study_id_clean), by = "study_id_clean") %>%
  mutate(
    definition_class = case_when(
      study_id_clean == "CHUA_2019"     ~ "composite",   # employment or age-appropriate role
      study_id_clean == "WILLIAMS_2016" ~ "retention",   # retained job among those employed at baseline
      TRUE                              ~ "competitive_or_paid"
    ),
    population_fep = study_id_clean != "LIN_2026",       # early-phase schizophrenia drug trial
    # Provisional coding of whether the sample was offered a vocational
    # intervention; only rows with exposure_verified == "yes" are used by the
    # exposure sensitivity analysis.
    vocational_exposure = case_when(
      study_id_clean %in% c("CRAIG_2014", "NUECHTERLEIN_2020", "VANDUIN_2021",
                            "RINALDI_2004", "RINALDI_2010", "WILLIAMS_2016",
                            "HUMENSKY_2017", "TURNER_2019") ~ "all",
      study_id_clean %in% c("DUDLEY_2014", "HEGELSTAD_2019", "MAJOR_2010",
                            "ERICKSON_2021", "KILLACKEY_2008", "KILLACKEY_2019",
                            "ROSENHECK_2017") ~ "part",
      study_id_clean %in% c("EACK_2011", "FOWLER_2019", "LIN_2026") ~ "none",
      TRUE ~ NA_character_
    ),
    exposure_verified = if_else(
      study_id_clean %in% c("CRAIG_2014", "NUECHTERLEIN_2020", "VANDUIN_2021",
                            "RINALDI_2004", "RINALDI_2010", "DUDLEY_2014",
                            "HEGELSTAD_2019", "MAJOR_2010", "ERICKSON_2021",
                            "KILLACKEY_2008", "KILLACKEY_2019", "EACK_2011",
                            "FOWLER_2019", "LIN_2026"),
      "yes", "no"),
    attrition = 1 - n_assessed / n_total,
    # Rinaldi 2004 (data collected over 12 months, published 2004) and Rinaldi
    # 2010 (data collected November 2001 to July 2006) report the same South
    # West London early intervention service, so the 2010 cohort contains the
    # 2004 participants. The primary set keeps the larger, later cohort only.
    overlapping_cohort = study_id_clean == "RINALDI_2004",
    in_primary_set = outcome_basis == "point" &
      definition_class == "competitive_or_paid" & population_fep & !overlapping_cohort,
    in_point_relaxed_set = outcome_basis == "point",
    in_period_set = outcome_basis == "period",
    in_legacy_set = TRUE
  ) %>%
  select(study_id_clean, study_label, author, year, year_online, country, region,
         design, diagnosis, n_total, pct_male, mean_age, timepoint_months,
         follow_up_months, outcome_basis, outcome_measure, outcome_definition,
         definition_class, population_fep, overlapping_cohort, events, n_assessed, attrition,
         vocational_exposure, exposure_verified, quality_rating,
         in_primary_set, in_point_relaxed_set, in_period_set, in_legacy_set,
         events_raw, n_assessed_raw, timepoint_months_raw,
         reported_es, notes, report_id, result_id)

# -------------------------------
# 6. Trials table and arm-level table
# -------------------------------
trials <- dat %>%
  filter(effect_type == "trial_binary") %>%
  left_join(full_denominators, by = "study_id_clean") %>%
  mutate(
    analysis_set = case_when(
      study_id_clean %in% c("ERICKSON_2021", "KILLACKEY_2008", "KILLACKEY_2019") ~ "primary",
      study_id_clean == "NUECHTERLEIN_2020" ~ "sensitivity_k4",
      study_id_clean %in% c("CRAIG_2014", "VANDUIN_2021") ~ "excluded_addon",
      study_id_clean %in% c("EACK_2011", "FOWLER_2019") ~ "non_vocational_rct",
      study_id_clean %in% c("DUDLEY_2014", "HEGELSTAD_2019", "MAJOR_2010", "ROSENHECK_2017") ~ "other_comparison",
      TRUE ~ NA_character_
    ),
    comparison_type = case_when(
      analysis_set == "primary" ~ "IPS added to TAU vs TAU (RCT)",
      analysis_set == "sensitivity_k4" ~ "IPS plus WFM vs BVR plus SST (RCT, active vocational comparator)",
      analysis_set == "excluded_addon" ~ "IPS plus adjunct vs IPS (RCT; add-on question)",
      analysis_set == "non_vocational_rct" ~ "Non-vocational intervention RCT",
      study_id_clean == "ROSENHECK_2017" ~ "NAVIGATE vs community care (cluster-randomised RAISE-ETP)",
      TRUE ~ "Non-randomised comparison"
    ),
    rr_computed = (events_intervention / n_intervention) / (events_control / n_control)
  ) %>%
  select(study_id_clean, study_label, design, timepoint_months, follow_up_months,
         outcome_basis, outcome_measure, outcome_definition, intervention_name, comparator,
         events_intervention, n_intervention, events_control, n_control,
         n_intervention_full, n_control_full, denominator_source, analysis_set,
         comparison_type, rr_computed, events_intervention_raw, events_control_raw,
         n_control_raw, reported_es_type, reported_es,
         reported_ci_low, reported_ci_high, reported_p, quality_rating, notes,
         report_id, result_id)

# Arm-level long table for the exact binomial model (one row per arm).
trial_arms <- bind_rows(
  trials %>% filter(analysis_set %in% c("primary", "sensitivity_k4")) %>%
    transmute(study_id_clean, study_label, analysis_set, arm = "intervention", arm_ips = 1L,
              events = events_intervention, n_analysed = n_intervention,
              n_full = n_intervention_full),
  trials %>% filter(analysis_set %in% c("primary", "sensitivity_k4")) %>%
    transmute(study_id_clean, study_label, analysis_set, arm = "control", arm_ips = 0L,
              events = events_control, n_analysed = n_control, n_full = n_control_full)
) %>% arrange(study_id_clean, desc(arm_ips))

# -------------------------------
# 7. Recovery outcomes (descriptive only)
# -------------------------------
recovery <- dat %>%
  filter(effect_type %in% c("correlation", "smd")) %>%
  select(study_id_clean, study_label, effect_type, timepoint_months, outcome_domain,
         outcome_measure, outcome_definition, r, n_r, group1_label, mean1, sd1, n1,
         group2_label, mean2, sd2, n2, quality_rating, report_id, result_id)

# -------------------------------
# 8. Reconciliation of computed against reported effect sizes
# -------------------------------
reconciliation <- bind_rows(
  prevalence %>%
    transmute(study_id_clean, effect_type = "proportion",
              computed = events / n_assessed, reported = reported_es,
              tolerance = 0.01),
  trials %>%
    transmute(study_id_clean, effect_type = "trial_binary",
              computed = rr_computed, reported = reported_es, tolerance = 0.02)
) %>%
  mutate(difference = computed - reported,
         flag = !is.na(reported) & abs(difference) > tolerance)
write_csv(reconciliation, file.path(derived_dir, "reconciliation.csv"))

# -------------------------------
# 9. Assertions on the derived tables
# -------------------------------
check(nrow(prevalence) == 23, "expected 23 prevalence rows")
check(!any(duplicated(prevalence$study_id_clean)), "more than one prevalence row per study")
check(sum(prevalence$outcome_basis == "point") == 14 &&
        sum(prevalence$outcome_basis == "period") == 9, "expected 14 point and 9 period rows")
check(identical(as.vector(table(prevalence$quality_rating)[c("Weak", "Moderate", "Strong")]),
                c(8L, 6L, 9L)), "EPHPP counts are not 8 Weak, 6 Moderate, 9 Strong")
check(identical(as.vector(table(prevalence$region)[c("Europe", "North America", "Oceania", "Asia")]),
                c(10L, 7L, 4L, 2L)), "region counts are not 10/7/4/2")
check(identical(as.vector(table(prevalence$design)[c("RCT", "Cohort")]), c(8L, 15L)),
      "design counts are not 8 RCT / 15 cohort (dissertation coding)")
check(all(prevalence$events <= prevalence$n_assessed), "events exceed n_assessed")
check(all(prevalence$n_assessed <= prevalence$n_total), "n_assessed exceeds n_total")
check(all(prevalence$timepoint_months >= 6 & prevalence$timepoint_months <= 24),
      "a prevalence timepoint lies outside the 6 to 24 month window")
check(sum(prevalence$in_primary_set) == 10,
      "strict primary set is not k = 10 (11 competitive point-prevalence FEP studies minus the overlapping Rinaldi 2004 cohort)")
check(!any(prevalence$in_primary_set & prevalence$definition_class == "retention"),
      "a job-retention row entered a competitive-employment set")
check(all(trials$n_intervention + trials$n_control <= dat$n_total[match(trials$study_id_clean, dat$study_id_clean)]),
      "arm totals exceed n_total")
check(!any(is.na(trials$analysis_set)) && nrow(trials) == 12, "every trial row must carry exactly one analysis_set label")
check(sum(trials$analysis_set == "primary") == 3, "primary trial set is not k = 3")
primary_rr <- sort(trials$rr_computed[trials$analysis_set == "primary"])
check(all(abs(primary_rr - c(1.45, 1.47, 6.83)) < 0.006),
      paste("primary RRs do not reproduce 1.45, 1.47, 6.83:", paste(round(primary_rr, 3), collapse = ", ")))
check(all(!is.na(trial_arms$n_full)), "a primary or k4 arm lacks a verified full denominator")

# Within-study consistency: follow-up constant; proportion counts equal the
# arm sums where a trial row exists at the same timepoint and definition.
fu_check <- dat %>% group_by(study_id_clean) %>% summarise(n_fu = n_distinct(follow_up_months), .groups = "drop")
check(all(fu_check$n_fu == 1), paste("follow_up_months varies within:",
                                     paste(fu_check$study_id_clean[fu_check$n_fu > 1], collapse = ", ")))
paired <- prevalence %>%
  inner_join(trials, by = "study_id_clean", suffix = c("_p", "_t")) %>%
  filter(timepoint_months_p == timepoint_months_t)
sum_ok <- with(paired, events == events_intervention + events_control &
                 n_assessed == n_intervention + n_control)
check(all(sum_ok), paste("proportion counts do not equal arm sums for:",
                         paste(paired$study_id_clean[!sum_ok], collapse = ", ")))

# Timepoint stated in the definition text must match timepoint_months.
stated <- as.numeric(str_match(prevalence$outcome_definition, "(\\d+)[- ]?month")[, 2])
tp_ok  <- is.na(stated) | stated == prevalence$timepoint_months
check(all(tp_ok), paste("definition text timepoint disagrees with timepoint_months for:",
                        paste(prevalence$study_id_clean[!tp_ok], collapse = ", ")))

# Categorical fields within allowed sets and free of stray whitespace.
check(all(prevalence$region %in% c("Europe", "North America", "Oceania", "Asia")), "unexpected region")
check(all(prevalence$design %in% c("RCT", "Cohort")), "unexpected design")
check(all(prevalence$quality_rating %in% c("Weak", "Moderate", "Strong")), "unexpected quality rating")
check(all(dat$quality_tool == "EPHPP"), "quality_tool is not EPHPP after trimming")
check(!any(grepl("[^ -~]", c(dat$study_id_clean, dat$region, dat$design, dat$country))),
      "non-ASCII characters in identifier or moderator columns")

# -------------------------------
# 10. Write derived tables
# -------------------------------
write_csv(prevalence, file.path(derived_dir, "prevalence.csv"))
write_csv(trials,     file.path(derived_dir, "trials.csv"))
write_csv(trial_arms, file.path(derived_dir, "trial_arms.csv"))
write_csv(recovery,   file.path(derived_dir, "recovery.csv"))
write_csv(dat %>% select(report_id, study_id, study_id_clean, result_id, everything()),
          file.path(derived_dir, "all_rows_clean.csv"))

cat("\nCorrections applied:\n")
print(as.data.frame(corrections[, c("study_id_clean", "effect_type", "field", "raw", "corrected")]),
      right = FALSE)
cat("\nReconciliation flags (computed vs reported):\n")
print(as.data.frame(reconciliation %>% filter(flag)))
cat("\nPrevalence sets: primary k =", sum(prevalence$in_primary_set),
    "| point relaxed k =", sum(prevalence$in_point_relaxed_set),
    "| period k =", sum(prevalence$in_period_set),
    "| legacy k =", sum(prevalence$in_legacy_set), "\n")
cat("Primary set by region:\n")
print(table(prevalence$region[prevalence$in_primary_set]))
cat("Trial analysis sets:\n")
print(table(trials$analysis_set))
cat("\nDerived tables written to", derived_dir, "\n")
