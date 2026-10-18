# Release candidate 2 to release candidate 3 comparison (18 October 2026)

Candidate 3 was fitted by `Rscript run_all.R --release` from the clean freeze `505b247` (RT's approval of that exact commit, 18 October 2026), 12:15 to 12:32 BST (16.8 minutes): 74 fits, none from cache, every convergence gate, `assert_release_fits_fresh()` and `verify_run_artefacts()` passed. As in candidate 2, the bridge-sampling model `rc_bf_h1_modini2016` failed its first attempt (two divergent transitions) and passed on the pre-specified refit at adapt_delta 0.999. The manifest records both new decision inputs (`release_vocational_exposure.csv`, `review_disposition_2026-10-18.csv`).

The only input change between the freezes `0c337f2` (candidate 2) and `505b247` is A46: the vocational-exposure classification and the review-disposition layer. No existing rule, count or pool changed.

## Method

Every file under `output/` was compared with the committed candidate 2 outputs (`git status` against HEAD `505b247`). For CSV files, rows were matched on their first column and compared cell by cell; for PDF files, the bytes were compared after removing the `/CreationDate` and `/ModDate` entries; LaTeX files were compared by `git diff`.

## Result

- **CSV outputs:** five files changed (`rc_prevalence_summary.csv`, `rc_prevalence_diagnostics.csv`, `rc_table_mcmc_diagnostics.csv`, `rc_table_prevalence.csv`, plus the historical `output/bayesian/gate_attempts.csv`), each by one added row for `rc_a33_point_excl_all_offered_hn1` and no other change. Every other CSV is byte-identical.
- **LaTeX outputs:** `rc_numbers.tex` gained nine macros (`\rcPrevExp*`) and no other line changed; `rc_table_prevalence.tex` and `rc_table_mcmc_diagnostics.tex` each gained one row.
- **PDF outputs:** 49 changed files differ only in their creation and modification dates; one new file, `ppc_rc_a33_point_excl_all_offered_hn1.pdf`.
- **Manifest and session information** changed as expected (new run).

Every candidate 2 number therefore stands unchanged in candidate 3.

## New result

| Analysis | k | Assessed | Median (95 % CrI) | Prediction interval | tau |
|---|---|---|---|---|---|
| Point primary (unchanged) | 8 | 1,591 | 32.7 % (24.3 to 42.3) | 12.5 to 62.0 | 0.49 |
| Excluding a confirmed universal vocational offer (A46) | 5 | 1,224 | 30.0 % (19.7 to 43.7) | 9.2 to 65.0 | 0.53 |

Excluded: Abdel-Baki 2013, Craig 2014, Rinaldi 2010. Retained: Andersen 2024, Dudley 2014, Hegelstad 2019, Drake 2013, Clarke 2023. The medians differ by 2.7 percentage points; the two models use different data, so this is a descriptive difference, not a contrast, an intervention effect or an untreated estimate.
