# ===============================================
# 08 - Compare YL's re-extraction of 11 September 2026 with the pinned raw
#      file and the release inputs
# ===============================================
# Not part of run_all.R. Reads:
#   data/yanan_data_280826.csv                  pinned raw extraction (A6)
#   data/review/yanan_data_110926.csv           YL's re-extraction, supplied 11 Sep 2026
#   data/derived/release_prevalence.csv         release layer (R/00b)
#   data/derived/release_trial_arms.csv         release layer (R/00b)
# Writes three review files, all mechanical (no judgement encoded):
#   data/review/li_reextraction_diff_2026-09-11.csv        cell-level differences on
#                                                          the 49 shared columns
#   data/review/li_reextraction_vs_release_2026-09-11.csv  YL's employment cells
#                                                          beside the release inputs
#   data/review/li_ephpp_components_2026-09-11.csv         YL's five EPHPP components
#                                                          and the global-rating rule
# Neither raw file is modified. Adopting any cell from the re-extraction is a
# ledger entry with a dated amendment, not a change here.
# ===============================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tidyr)
})
source(if (file.exists("R/utils.R")) "R/utils.R" else "utils.R")

read_li <- function(path) {
  txt <- readBin(path, "raw", file.info(path)$size)
  txt <- rawToChar(txt)
  Encoding(txt) <- "bytes"
  # Same three byte replacements as R/00_clean_data.R (cp1252 punctuation)
  txt <- gsub(rawToChar(as.raw(c(0xa3, 0xa8))), " (", txt, fixed = TRUE, useBytes = TRUE)
  txt <- gsub(rawToChar(as.raw(c(0xa3, 0xa9))), ") ", txt, fixed = TRUE, useBytes = TRUE)
  txt <- gsub(rawToChar(as.raw(c(0xa3, 0xbb))), "; ", txt, fixed = TRUE, useBytes = TRUE)
  txt <- gsub(rawToChar(as.raw(0xa0)), " ", txt, fixed = TRUE, useBytes = TRUE)
  txt <- iconv(txt, from = "CP1252", to = "UTF-8", sub = "?")
  read_csv(I(txt), col_types = cols(.default = col_character()), na = character(0),
           show_col_types = FALSE, progress = FALSE) %>%
    mutate(across(everything(), ~ str_squish(.x)))
}

old <- read_li(root_path("data", "yanan_data_280826.csv"))
new <- read_li(root_path("data", "review", "yanan_data_110926.csv"))
check(nrow(old) == 44 && nrow(new) == 43, "unexpected row counts in the two extraction files")

shared <- intersect(names(old), names(new))
only_new <- setdiff(names(new), names(old))
cat("shared columns:", length(shared), "; columns only in the 11 Sep file:", length(only_new), "\n")

# Rows are matched on study_id and year, then on order within that key (both
# files list a study's rows in the same order where the study appears in both).
key_rows <- function(d, label) {
  d %>% mutate(row_in_file = row_number()) %>%
    group_by(study_id, year) %>% mutate(seq = row_number()) %>% ungroup() %>%
    mutate(file = label)
}
old_k <- key_rows(old, "2026-08-28"); new_k <- key_rows(new, "2026-09-11")

long <- function(d) {
  d %>% select(study_id, year, seq, row_in_file, all_of(shared)) %>%
    pivot_longer(all_of(setdiff(shared, c("study_id", "year"))), names_to = "column", values_to = "value")
}
diff <- full_join(long(old_k) %>% rename(value_2026_08_28 = value, row_2026_08_28 = row_in_file),
                  long(new_k) %>% rename(value_2026_09_11 = value, row_2026_09_11 = row_in_file),
                  by = c("study_id", "year", "seq", "column")) %>%
  mutate(status = case_when(
    is.na(row_2026_08_28) ~ "row only in 2026-09-11 file",
    is.na(row_2026_09_11) ~ "row only in 2026-08-28 file",
    coalesce(value_2026_08_28, "") == coalesce(value_2026_09_11, "") ~ "same",
    TRUE ~ "differs")) %>%
  filter(status != "same") %>%
  arrange(coalesce(row_2026_09_11, row_2026_08_28), column) %>%
  select(study_id, year, seq, column, value_2026_08_28, value_2026_09_11, status)
# Collapse the whole-row cases to one line each
row_only <- diff %>% filter(status != "differs") %>% distinct(study_id, year, seq, status) %>%
  mutate(column = "(all)", value_2026_08_28 = NA, value_2026_09_11 = NA)
diff <- bind_rows(diff %>% filter(status == "differs"), row_only) %>%
  arrange(study_id, year, seq, column)
write_csv(diff, root_path("data", "review", "li_reextraction_diff_2026-09-11.csv"), na = "")
cat("cell differences:", sum(diff$status == "differs"), "; rows only in one file:", nrow(row_only), "\n")

# YL's employment cells (proportion rows and trial rows) beside the release inputs
rp  <- read_csv(root_path("data", "derived", "release_prevalence.csv"), show_col_types = FALSE)
arms <- read_csv(root_path("data", "derived", "release_trial_arms.csv"), show_col_types = FALSE)
print_year <- c(DUDLEY_2013 = "2014", HEGELSTAD_2018 = "2019", TAPFUMANEYI_2014 = "2015")
li_key <- function(author, year) {
  k <- paste0(toupper(str_replace_all(author, "[^A-Za-z]", "")), "_", year)
  if_else(k %in% names(print_year), paste0(sub("_.*", "", k), "_", print_year[k]), k)
}
li_emp <- new %>%
  filter(effect_type %in% c("proportion", "trial_binary"), outcome_domain == "employment") %>%
  transmute(study_id_clean = li_key(author, year), li_row = study_id,
            li_effect_type = effect_type, li_timepoint = timepoint_months, li_basis = outcome_basis,
            li_outcome = outcome_definition, li_events = events, li_n_assessed = n_assessed,
            li_events_int = events_intervention, li_n_int = n_intervention,
            li_events_con = events_control, li_n_con = n_control, li_page = page_source)
set_cols <- c("in_a33_point_primary", "in_a33_point_duration_sens", "in_a33_period",
              "in_a33_variable_window", "in_strict_a24_point")
rel_prev <- rp %>%
  mutate(rel_sets = apply(across(all_of(set_cols)), 1, function(z) paste(sub("^in_", "", set_cols[as.logical(z)]), collapse = "; "))) %>%
  transmute(study_id_clean, rel_timepoint = as.character(timepoint_months),
            rel_basis = outcome_basis, rel_events = as.character(events),
            rel_n_assessed = as.character(n_assessed), rel_sets)
rel_arms <- arms %>% group_by(study_id_clean) %>%
  summarise(rel_events_int = as.character(events[arm == "intervention"]),
            rel_n_int = as.character(n_analysed[arm == "intervention"]),
            rel_events_con = as.character(events[arm == "control"]),
            rel_n_con = as.character(n_analysed[arm == "control"]), .groups = "drop")
vs <- li_emp %>%
  left_join(rel_prev, by = "study_id_clean") %>%
  left_join(rel_arms, by = "study_id_clean") %>%
  mutate(counts_agree = case_when(
    li_effect_type == "proportion" & is.na(rel_events) ~ NA,
    li_effect_type == "proportion" ~ li_events == rel_events & li_n_assessed == rel_n_assessed,
    is.na(rel_events_int) ~ NA,
    TRUE ~ li_events_int == rel_events_int & li_n_int == rel_n_int &
      li_events_con == rel_events_con & li_n_con == rel_n_con),
    timepoint_label_same = if_else(li_effect_type == "proportion", li_timepoint == rel_timepoint, NA),
    release_comparison = case_when(
      li_effect_type == "proportion" & is.na(rel_events) ~ "no release prevalence row",
      li_effect_type == "trial_binary" & is.na(rel_events_int) ~ "no release trial row",
      counts_agree ~ "counts agree with release input",
      TRUE ~ "counts differ from release input")) %>%
  arrange(study_id_clean, li_row)
write_csv(vs, root_path("data", "review", "li_reextraction_vs_release_2026-09-11.csv"), na = "")
cat("\nYL 11 Sep employment cells versus release inputs:\n")
print(vs %>% count(li_effect_type, release_comparison) %>% as.data.frame())

# EPHPP components: five rated components (Blinding absent), global rule per the
# EPHPP dictionary: Strong = no Weak component, Moderate = one, Weak = two or more.
comp_cols <- c("quality_rating_Selection Bias", "quality_rating_Study Design", "quality_rating_Confounders",
               "quality_rating_Data Collection Methods", "quality_rating_Withdrawals and Dropouts")
ephpp <- new %>%
  distinct(study_id_clean = li_key(author, year), across(all_of(comp_cols)),
           analyses_column = `quality_rating_Analyses (appropriateness)`, global_as_supplied = quality_rating) %>%
  mutate(n_weak = rowSums(across(all_of(comp_cols), ~ str_to_lower(.x) == "weak")),
         global_by_rule = case_when(n_weak == 0 ~ "Strong", n_weak == 1 ~ "Moderate", TRUE ~ "Weak"),
         rule_matches_supplied = global_by_rule == global_as_supplied)
names(ephpp) <- str_replace_all(names(ephpp), "quality_rating_", "component_") %>% str_replace_all(" ", "_")
write_csv(ephpp, root_path("data", "review", "li_ephpp_components_2026-09-11.csv"), na = "")
cat("\nEPHPP: studies", nrow(ephpp), "; global rating matches the five-component rule in",
    sum(ephpp$rule_matches_supplied), "\n")
