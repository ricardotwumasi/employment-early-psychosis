# EPHPP two-source check (verified 2 September 2026)

**Sources compared**
1. `msc_dissertations/each_chapter/Yanan Quality Appraisal Table 170626.docx`: EPHPP component table, 43 quantitative studies. Columns: Selection Bias, Study Design, Confounders, Data Collection Methods, Withdrawals and Dropouts, Analyses (appropriateness), Global Rating.
2. `manuscript/sr_data/ephpp_items.csv`: 43 rows, transcribed from the Kaplankiran dissertation.

**Result**: both tables hold exactly 43 rows. Matching rows on normalised author/year label (case- and accent-insensitive) gives 41 matched pairs with **zero** component or global-rating differences across all 41. Two labels do not match by exact string:
- Yanan `Miley et al. (2021)` vs local `Miley et al. (2023)`: a year difference only; no `literature/` PDF for Miley exists (searched, none found), and both `manuscript/sr_data/included_studies_45.csv` and `manuscript/sr_data/ephpp_items.csv` independently give 2023 (`study_key` `miley2023`). No PDF-level evidence for either year was found in this repository; the discrepancy is reported here, not resolved.
- Yanan `ten Velden Hegelstad et al. (2017)` vs local `Hegelstad et al. (2017)`: a naming (surname-prefix) difference only, same year; ratings for this row (Moderate/Moderate/Strong/Strong/Moderate/Yes/Strong) are identical between the two sources.

**Blinding**: the EPHPP Blinding component is absent from both sources. The local `ephpp_items.csv` carries a `blinding` column but every value is `not reported`; Yanan's table has no Blinding column at all, instead using an "Analyses (appropriateness)" column, which is not a standard EPHPP-rated component.

**Global-rating rule check**: applying the EPHPP rule (Strong = no weak component; Moderate = one weak; Weak = two or more weak) across the five rated components (Selection Bias, Study Design, Confounders, Data Collection Methods, Withdrawals and Dropouts) to all 43 rows in each source, every row's printed Global Rating is consistent with the rule: zero violations in either source.

**JBI qualitative table**: the same docx contains a JBI checklist with columns for items 1, 2, 3, 4, 5, 8, 9, 10 only (items 6 and 7 absent) for `Jones et al. (2023)` and `Ortenblad et al. (2025)`, both rated Yes on all eight present items with an overall "Include" mark of ✓✓.

**Method**: table cells extracted programmatically from the docx's `word/document.xml` (python `zipfile` + regex over `<w:tr>`/`<w:tc>`/`<w:t>`) and compared row-by-row against `ephpp_items.csv` by author/year label; component values compared case-insensitively per row.
