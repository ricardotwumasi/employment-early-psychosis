# Cache-only replay of 6 September 2026

A staged replay of the full pipeline in cache-only mode (`Rscript run_all.R --cache-only`), run in a disposable copy of the checkout outside the repository, to verify the Phase 1 engineering without fitting any model. Files here are the record of that run; nothing in this directory is an analysis input.

| File | Content |
|---|---|
| `run_manifest_cache_only_2026-09-06.json` | The manifest written by the staged run: status completed, mode cache-only, 42 fits all `from_cache = TRUE`, 42 gate attempts, elapsed 2.47 minutes, input and output hashes of the staged copy |
| `replay_log_cache_only_2026-09-06.txt` | Console log: 42 `[cache-only]` lines, no `[fit]`, no `[gate]` retry; the `Recompiling the model with 'rstan'` lines are bridge-sampling recompilations, not fits |
| `fixture_comparison_2026-09-06.csv` | Every file in `docs/fixtures/historical_outputs_2026-09-02.sha256` classified against the staged output |
| `stochastic_comparison_2026-09-06.csv` | Bridge-sampling Bayes factors, historical (unseeded) versus replay (locally seeded 20260901), with the tolerance fixed before comparison |

## Outcome

- 54 fixture files byte-identical (all posterior summaries, leave-one-out, exact-binomial, meta-regression, Dienes quadrature, frequentist outputs and all tables except the two below).
- Promoted into the repository with a named difference: the two draw exports (new columns `model_id, .chain, .iteration, .draw`; `mu` and `tau` identical), the three diagnostics CSVs (new column `attempt_id`; all other columns identical), `output/bayesian/gate_attempts.csv` (new), and Table 1 (the Lin 2026 set label changes from "Not FEP" to "Outside primary estimand (A5)"; no number changes).
- **Not promoted:** `output/bayesian/trials_bridge_bf.csv` and `output/tables/dienes_bf_quadrature_vs_bridge.csv` (bridge sampling is stochastic; the historical run was unseeded), every PDF figure (PDF bytes differ; the posterior-predictive figures are now seeded and would differ visibly), and `output/session_info.txt`. The committed versions remain the 2 September 2026 fixtures.

## Bridge-sampling comparison (tolerance fixed in advance: relative difference at most 5 % flagged for review; nothing relaxed)

All five seeded bridge estimates differ from the historical unseeded values by 0.9 % to 3.6 %, within tolerance. Against the quadrature values the seeded bridge estimates differ by 3.2 % to 3.5 % for all five scales, so the pre-specified two-significant-figure agreement criterion of amendment A18 is met for none of the five in the replay (historically it was met for two of five). This is Monte Carlo variability of bridge sampling of about 3 %, not a change in the quadrature Bayes factors, which are byte-identical. The A18 record stands as a failed pre-specified check; the release run must either reduce bridge-sampling error (more bridge repetitions or draws) or retain the formal record of failure with the method's limitation stated.
