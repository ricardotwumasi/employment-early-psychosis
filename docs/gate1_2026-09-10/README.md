# Gate 1 closure evidence, 10 September 2026

Two items were outstanding for Gate 1 (docs/document_control_register.md, revision 1.5): restoration of the environment from `renv.lock` into a fresh library, and the release-mode failure path.

## 1. Restoration from renv.lock into a fresh library

- Before restoring, `renv::status(lockfile = "renv.lock")` reported the project as not synchronised for two reasons: the archived external supplement `data/external/frederick2019/pone.0212208.s002.R` was being scanned as project code (it calls ConfoundedMeta and stargazer, which are not project dependencies), and the five recommended packages used by the analysis (codetools, lattice, Matrix, mgcv, nlme) were not recorded. Fix: a `.renvignore` excluding `data/external/` and `tmp/`, and `renv::snapshot(type = "implicit")`, which added only those five packages to the lockfile (no version of any other package changed).
- `renv::restore(lockfile = "renv.lock", library = <fresh empty directory>)` then installed all 102 recorded packages in 0.76 minutes (log: `renv_restore_fresh_library.log`). bayesplot 1.12.0.9000 was restored from the stan-dev r-universe record (RemoteSha d5237925...), cmdstanr 0.9.0 likewise; brms 2.23.0, metafor 4.6.0, posterior 1.6.0, bridgesampling 1.2.1 restored from CRAN records.
- `renv::status(lockfile = "renv.lock", library = c(<fresh library>, .Library))` reports synchronised. The system library is needed only for the five recommended packages, which ship with R 4.4.2; a fresh library alone reports them as missing because renv does not reinstall recommended packages into a project library.
- With `.libPaths()` set to the fresh library plus the system library, brms, cmdstanr, bayesplot and metafor load from the fresh library and `cmdstanr::cmdstan_version()` returns 2.36.0 (CmdStan is installed separately at `~/.cmdstan/cmdstan-2.36.0`, as documented).

## 2. Release-mode failure path

`run_all.R --release` previously forced `fep.refit`, so its post-run assertion that no fit came from a cache could never fire. The guards are now separate functions in `R/utils.R` (`check_release_preconditions()`, `assert_release_fits_fresh()`, and a cache refusal inside `fit_cached()` when `fep.release` is set without `fep.refit`), each exercised by `tests/testthat/test-release-mode.R`: missing lockfile, unsynchronised library, dirty tree, a logged cached fit, an empty fit log, and the cache refusal all stop with the documented message.

## 3. Limits

CI still covers the deterministic layer only (no Stan). The full analysis environment is demonstrated here on the development machine (macOS, R 4.4.2, arm64), not on a second platform; GT's independent rebuild (Gate 4) is the cross-machine check.
