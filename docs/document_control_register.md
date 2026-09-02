# Document-control register

**Project:** Employment in First-Episode Psychosis: A Systematic Review and Bayesian Meta-Analysis  
**Opened:** 2 September 2026 by the statistical and release lead (RT)  
**Purpose:** to fix the baseline that the v1.1.0 release programme starts from, and to record gate decisions with their evidence. Hashes are SHA-256. Times are Europe/London.

## 1. Baseline artefacts at adoption of plan revision 1.3

| Artefact | Location | Identity | Notes |
|---|---|---|---|
| Public analysis repository | `employment-early-psychosis/` (GitHub `ricardotwumasi/employment-early-psychosis`) | `main` at `adb39de`, matching `origin/main` | Clean tree verified 2 September 2026 |
| Superseded release tag | same repository | `v1.0.0` at `ffa2890` | Retained; must not be moved or deleted |
| Working branch for Phase 0 archive | same repository | `v1.1-phase0` from `adb39de` | Created 2 September 2026; local, not pushed at creation |
| Raw extraction CSV | `data/yanan_data_280826.csv` | `a167afc2fb70a2e1fbf96eddc3f540c266235cd3507b855adb5be786d757f70f` | Byte-identical to `msc_dissertations/each_chapter/Yanan Data 280826.csv` and to the workspace copy `data/yanan_data_280826.csv` (verified 2 September 2026) |
| Corrections table | `data/derived/corrections.csv` | `4a6b8d628879ed5ce034d116b82c9344a8c3a3be3c19d36ebd5ad8cd7567afea` | Fourteen source-linked rows |
| 1 September specification (archive) | `docs/analysis_specification_2026-09-01.txt` | `d6928e505182cce9135b1ce6ecef3106f7d0550e36c932a275d90edc5e338fee` | Verbatim; see `docs/CHANGELOG.txt` for provenance and limitations |
| Dated amendment | `docs/amendment_2026-09-02.md` | `5d478f80f7aff25829b6f922944cf8f4a052c6b8ac6c033382d4ba15faac361c` | Register of changes A1 to A22, written after the authors approved the decisions it records; circulated for confirmation |
| PROSPERO revision draft as prepared | `docs/registration/prospero_major_revision_draft_as_prepared_2026-09-02.md` | `8c1291417c274567e5bbd142dae118d6441264925f094c4cfe8712e269df06c5` | Draft, not the submitted text |
| Manuscript source (local, not in Git) | `../manuscript/manuscript.tex` | `3b2031158194835747d788204d0a506850c4b31f0aa04978ffe7a1a79483ab1c` | Modified 2 September 2026 09:21 |
| Supplement source (local, not in Git) | `../manuscript/supplement.tex` | `d53de2d0998f6347ce049cfbaa30baf3cf4dc86cd15aaa0f001e74ced5636e50` | Modified 2 September 2026 09:12 |
| Manuscript PDF (local) | `../manuscript/manuscript.pdf` | `9459473b7c5c2c44072bac0cb2a412ea4d49ab8cc1a5cc662d2c9edbe5f9d11a` | Built 2 September 2026 09:21; 39 pages |
| Supplement PDF (local) | `../manuscript/supplement.pdf` | `97194cec072b986712a15142926694b04dc1298b0310caeb7f3d71b01953779c` | Built 2 September 2026 09:21; 16 pages |
| Plan revision 1.2 (superseded) | `../tmp/NEXT_VERSION_PLAN_v1.1_rev1.2_backup_2026-09-02.md` | `059174e6fb020316e392902b6de56845ff77ccad15e768288009f1a38b380611` | Verbatim backup of the text that `../NEXT_VERSION_PLAN_v1.1.md` held before revision 1.3 replaced it in place |
| Quality assessment | `../PLAN_HANDOVER_QUALITY_ASSESSMENT_2026-09-02.md` | `6f93f2c100d93d2f0d92919d4f7736482f5f8fbb05d330a4497bf81f1c53d867` | |
| Publication readiness assessment | `../PUBLICATION_READINESS_ASSESSMENT_2026-09-02.md` | `31a14a031f23d02e257595a19bf57a1b0e1b3961a3562b4860741cbc808e4dea` | |
| Handover (morning revision) | `../HANDOVER_2026-09-02.md` | `06c0ec280f7c35b973aff174f72061908ab34811e5c8c4a41093d2d991a740b1` | Hash taken before the afternoon update |

## 2. Gate record

| Gate | Status | Date | Decision basis | Evidence held | Evidence outstanding |
|---|---|---|---|---|---|
| 0A local scientific freeze | Passed | 2 September 2026 | All five authors approved the package of decisions (estimands, analysis hierarchy, terminology, Lin 2026 treatment); confirmed to RT on 2 September 2026 and submitted to PROSPERO the same day | This register; `docs/amendment_2026-09-02.md` (SHA-256 `5d478f80…`, the written record of those decisions, drafted after the approval); archived specification | Written approval files for each author under `docs/registration/approvals/` with hashes; each author's confirmation that the amendment text matches what they approved |
| 0B registration submission | Passed subject to archive | 2 September 2026 | AG submitted the PROSPERO major revision with review-team approval; the public revised record was expected later on 2 September 2026 | Draft as prepared | Exact submitted text, submission receipt, public version 1.0 export, public revised export, checksum file |
| 1 tested reproducible implementation | Open | | | | |
| 2 approved analysis specification | Open | | | | |
| 3 auditable review record | Open | | | | |
| 4 reproducible scientific run | Open | | | | |
| 5 submission candidate | Open | | | | |
| 6 publication-ready release | Open | | | | |

## 3. Chronology evidence relied upon for the Bayesian specification

| Observation | Value | Source |
|---|---|---|
| Specification last modified | 1 September 2026 23:55:04 BST | filesystem metadata of `~/.claude/plans/abundant-gathering-pancake.md` |
| Earliest cached Bayesian fit | 2 September 2026 00:09:40 BST (`output/bayesian/fits/primary_strict_point_hn1_80c4f35089e3.rds`) | filesystem metadata; fits are git-ignored |
| Analysis commits | 2 September 2026 03:55:22 +0100 (`b51fd6e`, `8645d83`, `ba01b74`, `abb5558`) | `git log` |
| Team confirmation | No Bayesian numerical output before approval of the specification | Authors, 2 September 2026 |

These observations are consistent with the confirmed sequence. They are not proof, and the manuscript must continue to say so.

## 4. Author identity confirmations

| Item | Confirmed value | Date |
|---|---|---|
| Lead author name | Hazal Kaplankiran | 2 September 2026 |
| Author order | Kaplankiran, Li, Trotta, Twumasi, Georgiades (corresponding) | 2 September 2026 |
| Extraction-sheet checker initials | The raw extraction uses "HZ" for the checker; this is Hazal Kaplankiran (HK) | 2 September 2026 |
| PROSPERO team | Kaplankiran, Li, Georgiades, Twumasi; GT not added by decision | 2 September 2026 |
