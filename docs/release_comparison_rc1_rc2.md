# Release candidate 2 against release candidate 1

18 October 2026. Release candidate 2 is a provisional development analysis: Gate 3 (search recovery) is open.

## 1. The two runs

| | Release candidate 1 | Release candidate 2 |
|---|---|---|
| Inputs | 10 September 2026 (A33 to A36) | Freeze commit `0c337f2` (A42, A43), approved by RT on 18 October 2026 |
| Run | Generated at `43f0e87`; outputs committed at `41a7f52` | Started from a clean tree at `0c337f2` (manifest `git_start`) |
| Manifest SHA-256 | `d68c422d871710ff…` | `a3f31c0b56e6980a…` (`output/run_manifest.json` as written by the run) |
| Fits | 71 logged (70 registered plus `rc_bf_h1_modini2016_ad999`) | 73 logged (72 registered fitted models plus the same `_ad999` re-run); none from cache; post-fit artefact check passed |
| Time | 13.3 minutes | 15.2 minutes |

The comparison baseline is the RC1 output tree at `ef779c5` (identical to `41a7f52` for `output/`), extracted with `git archive` before the RC2 run overwrote the working files.

## 2. Outputs whose inputs did not change (tolerances of `docs/release_tolerances_2026-09-10.md`)

The trial inputs (`data/derived/release_trial_arms.csv`) are byte-identical to RC1. Every trial output is byte-identical to RC1: `rc_trials_summary.csv`, `rc_trials_exact_binomial.csv` (OR model), `rc_absolute_risk_scenarios.csv`, `rc_trials_bayes_factors.csv` (quadrature and bridge), `rc_trials_bf_robustness_region.csv`, `rc_trials_leave_one_out.csv`, `rc_trials_primary_draws.csv`, `rc_exact_binomial_a33_k3_joint_draws.csv` and `rc_trials_diagnostics.csv`. The largest difference is therefore zero on every toleranced quantity. The historical layer (`output/bayesian/*.csv` and `output/tables/`) is unchanged except `gate_attempts.csv`, whose release-prevalence rows reflect the new fits; regenerated PDFs differ only in embedded metadata.

The EPHPP sensitivity is also identical (23.2 %, 13.3 to 45.5, k = 3), because its three studies (Craig 2014, Dudley 2014, Hegelstad 2019) are unchanged; the two new primary studies are unrated and excluded under the A42 rule.

## 3. Prevalence models (inputs changed; each change traced, agreement not expected)

Posterior median prevalence, 95 % CrI, 95 % prediction interval and tau median.

| Model | RC1 | RC2 | Source of the change |
|---|---|---|---|
| Point primary | k = 6; 31.8 (20.6 to 44.9); PI 9.5 to 67.7; tau 0.59 | k = 8; 32.7 (24.3 to 42.3); PI 12.5 to 62.0; tau 0.49 | Drake 2013 102/295 (NU23_01) and Clarke 2022 37/100 (NX02_02), `release_new_results.csv` (A42) |
| Tau prior HN(0.5) | 31.6 (22.4 to 43.2) | 32.5 (25.1 to 41.4) | As above |
| Tau prior HN(2) | 31.8 (20.7 to 47.3) | 32.8 (24.4 to 43.1) | As above |
| Duration sensitivity | k = 7; 32.3 (22.8 to 44.5) | k = 9; 33.0 (25.6 to 41.6) | As above, plus Eack 2011 as before |
| Missing as not employed | 24.6 (15.4 to 39.1) | 24.7 (17.2 to 34.6) | As above; `n_total` 385 (Drake, eligible baseline) and 146 (Clarke, randomised) |
| Normal-normal approximation | 32.0 (20.9 to 45.5) | 32.9 (24.9 to 42.7) | As above |
| Period set | k = 8; 38.5 (24.2 to 56.4); PI 7.5 to 83.7; tau 0.90 | k = 9; 39.2 (26.8 to 52.9); PI 10.0 to 79.4; tau 0.77 | Nossel 2018 124/260 added (NU16_01); Fowler 2019 5/44 to 15/66 (C30 to C32, C44); van Duin 2021 25/56 to 15/57, months 0 to 6 under the A42 period rule (C40 to C43) |
| Moderate or Strong only | k = 3; 23.2 (13.3 to 45.5) | identical | No change (Section 2) |

Leave-one-out medians on the primary set range from 31.2 % (without Rinaldi 2010) to 35.7 % (without Dudley 2014); leaving out Drake 2013 gives 32.5 % and leaving out Clarke 2022 32.1 %.

## 4. Standalone results (no model)

Observed proportions with 95 % Jeffreys intervals: Turner 2019 21/34, 61.8 % (45.0 to 76.6), as in RC1; Humensky 2017 34/65, 52.3 % (40.3 to 64.1) (C33 to C37); Porteous 2007 36/100, 36.0 % (27.1 to 45.7), and 58/125, 46.4 % (37.8 to 55.1) (NX18_01, NX18_02). The strict registered point set remains not estimable (k = 0); the strict trial comparison, Killackey 2008 alone, is unchanged.

## 5. Verdict

Every output with unchanged inputs reproduces RC1 exactly; every prevalence change traces to a row of `release_new_results.csv` or the release-layer ledger approved under A42 and A43. No tolerance was breached. GT's independent rebuild of RC2 under the same tolerances remains outstanding (Gate 4).
