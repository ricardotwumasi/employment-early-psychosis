# ===============================================
# utils.R - shared helpers for the FEP employment meta-analysis
# ===============================================
# Sourced by every numbered script. Provides:
#   - repository paths and a small assertion helper
#   - brms fitting with a content-addressed cache
#   - posterior summaries on transformed scales and prediction draws
#   - MCMC diagnostics against the pre-specified convergence gate
#   - Dienes-style Bayes factors (closed-form half-normal, exact random-effects)
#   - null-interval probabilities and robustness regions
# All functions are deterministic given the seeds set in run_all.R.
# ===============================================

# -------------------------------
# Paths and assertions
# -------------------------------

# The repository root is the directory that holds run_all.R. Scripts are run
# from the root by run_all.R, but this also works when a script is run from R/.
repo_root <- function() {
  if (file.exists("run_all.R")) return(normalizePath("."))
  if (file.exists(file.path("..", "run_all.R"))) return(normalizePath(".."))
  stop("Cannot locate the repository root (run_all.R not found).", call. = FALSE)
}

root_path <- function(...) file.path(repo_root(), ...)

# Stop with a clear message when a pre-specified check fails. Every data
# assertion in 00_clean_data.R goes through this so that a violated
# assumption halts the pipeline rather than propagating silently.
check <- function(condition, message) {
  if (!isTRUE(condition)) stop("CHECK FAILED: ", message, call. = FALSE)
  invisible(TRUE)
}

# -------------------------------
# MCMC settings (frozen in the analysis plan)
# -------------------------------
mcmc_settings <- list(
  chains      = 4,
  warmup      = 2000,
  iter        = 6000,   # brms iter includes warmup: 4000 sampling draws per chain
  adapt_delta = 0.99,
  seed        = 20260901,
  backend     = "cmdstanr"
)

# -------------------------------
# brms fitting with a content-addressed cache
# -------------------------------
# The cache key is a hash of the data, formula, priors, family and sampler
# settings, so a change to any of them forces a refit. Set
# options(fep.refit = TRUE) to ignore the cache, or options(fep.cache_only =
# TRUE) to forbid fitting altogether (stop if a cache entry is missing or
# invalid). The actual brm() call is indirected through fep_brm_call() so
# that tests can replace it with a stub that must never be reached in
# cache-only mode.
fep_brm_call <- function(...) brms::brm(...)

# Appends one row to the in-session fit log (options("fep.fit_log")); fit_log()
# returns the accumulated rows as a data.frame. Provenance only, not used by
# any downstream analysis.
record_fit_log <- function(model_id, cache_file, key, from_cache, cache_mtime) {
  row <- data.frame(model_id = model_id, cache_file = cache_file, key = key,
                    cache_key_version = 1L, from_cache = from_cache,
                    cache_mtime = cache_mtime, stringsAsFactors = FALSE)
  log <- getOption("fep.fit_log", list())
  log[[length(log) + 1]] <- row
  options(fep.fit_log = log)
  invisible(row)
}

fit_log <- function() {
  log <- getOption("fep.fit_log", list())
  if (length(log) == 0) {
    return(data.frame(model_id = character(0), cache_file = character(0), key = character(0),
                      cache_key_version = integer(0), from_cache = logical(0),
                      cache_mtime = as.POSIXct(character(0)), stringsAsFactors = FALSE))
  }
  do.call(rbind, log)
}

fit_cached <- function(formula, data, prior, family, tag,
                       adapt_delta = mcmc_settings$adapt_delta,
                       save_all_pars = FALSE, ...) {
  key <- digest::digest(list(
    formula = deparse(formula), data = data, prior = as.data.frame(prior),
    family = family$family, link = family$link,
    settings = c(mcmc_settings[c("chains", "warmup", "iter", "seed")],
                 adapt_delta = adapt_delta, save_all_pars = save_all_pars)
  ))
  cache_dir <- root_path("output", "bayesian", "fits")
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  cache_file <- file.path(cache_dir, paste0(tag, "_", substr(key, 1, 12), ".rds"))
  refit <- isTRUE(getOption("fep.refit", FALSE))
  cache_only <- isTRUE(getOption("fep.cache_only", FALSE))

  if (cache_only) {
    if (refit) {
      stop("CACHE_ONLY: fep.cache_only and fep.refit cannot both be set (conflict) for ", tag,
           call. = FALSE)
    }
    if (!file.exists(cache_file)) {
      stop("CACHE_ONLY: missing cache for ", tag, ": ", cache_file, call. = FALSE)
    }
    fit <- tryCatch(readRDS(cache_file), error = function(e) {
      stop("CACHE_ONLY: unreadable cache for ", tag, ": ", conditionMessage(e), call. = FALSE)
    })
    check(inherits(fit, "brmsfit"), paste0("CACHE_ONLY: cached object for ", tag, " is not a brmsfit"))
    meta <- attr(fit, "fep_cache_meta")
    if (is.null(meta)) {
      # Fits cached before 6 September 2026 carry no metadata; their identity
      # rests on the 12-character key prefix in the file name, which
      # file.exists(cache_file) has already matched against the computed key.
      message("  [cache-only] ", tag, " (legacy cache without metadata; key prefix matched by file name)")
    } else {
      if (!identical(meta$key, key)) {
        stop("CACHE_ONLY: cached fit for ", tag, " key mismatch (stale cache)", call. = FALSE)
      }
      message("  [cache-only] ", tag)
    }
    record_fit_log(tag, cache_file, key, from_cache = TRUE, cache_mtime = file.mtime(cache_file))
    return(fit)
  }

  # Release mode (options(fep.release = TRUE)) must never read a cache, even
  # if fep.refit were unset by a caller; the assertion after the run
  # (assert_release_fits_fresh) is the second line of defence.
  if (isTRUE(getOption("fep.release", FALSE)) && !refit) {
    stop("RELEASE: fep.release is set but fep.refit is not; refusing to consult the cache for ", tag,
         call. = FALSE)
  }
  if (!refit && file.exists(cache_file)) {
    message("  [cache] ", tag)
    fit <- readRDS(cache_file)
    record_fit_log(tag, cache_file, key, from_cache = TRUE, cache_mtime = file.mtime(cache_file))
    return(fit)
  }
  message("  [fit] ", tag)
  # Each fit writes its CmdStan CSV files to its own directory. Consecutive
  # fast fits (the leave-one-out loops take under two seconds each) otherwise
  # share cmdstanr's timestamped file names in the session temp directory,
  # and the previous fit object's finaliser can remove the new fit's CSV
  # before it is read ("File does not exist" from read_cmdstan_csv; seen twice
  # on 10 September 2026). The directory has no effect on the draws.
  out_dir <- file.path(tempdir(), "cmdstan_output", paste0(tag, "_", substr(key, 1, 12)))
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  fit <- fep_brm_call(
    formula = formula, data = data, prior = prior, family = family,
    chains = mcmc_settings$chains, warmup = mcmc_settings$warmup,
    iter = mcmc_settings$iter, seed = mcmc_settings$seed,
    backend = mcmc_settings$backend,
    control = list(adapt_delta = adapt_delta),
    save_pars = if (save_all_pars) brms::save_pars(all = TRUE) else NULL,
    refresh = 0, silent = 2, output_dir = out_dir, ...
  )
  attr(fit, "fep_cache_meta") <- list(
    key = key, cache_key_version = 1L, tag = tag, fitted_at = Sys.time(),
    brms = packageVersion("brms"), cmdstanr = packageVersion("cmdstanr"),
    cmdstan = cmdstanr::cmdstan_version(), r = R.version.string
  )
  saveRDS(fit, cache_file)
  record_fit_log(tag, cache_file, key, from_cache = FALSE, cache_mtime = file.mtime(cache_file))
  fit
}

# -------------------------------
# Convergence gate
# -------------------------------
# Returns one row per fit with the quantities in the pre-specified gate.
# A model that fails is flagged; 03/02 re-run it at adapt_delta = 0.999 once.
diagnose_fit <- function(fit, tag) {
  np <- brms::nuts_params(fit)
  divergent <- sum(np$Value[np$Parameter == "divergent__"])
  treedepth <- max(np$Value[np$Parameter == "treedepth__"])
  # E-BFMI per chain, as defined by Betancourt (2016)
  energy <- np[np$Parameter == "energy__", ]
  ebfmi <- min(vapply(split(energy$Value, energy$Chain), function(e) {
    var(diff(e)) / var(e)
  }, numeric(1)))
  draws <- posterior::as_draws_array(fit)
  # Restrict to model parameters (drop lp__ and generated quantities)
  keep <- grep("^(b_|sd_|r_|Intercept)", posterior::variables(draws), value = TRUE)
  summ <- posterior::summarise_draws(posterior::subset_draws(draws, variable = keep),
                                     "rhat", "ess_bulk", "ess_tail")
  data.frame(
    model         = tag,
    max_rhat      = max(summ$rhat, na.rm = TRUE),
    min_ess_bulk  = min(summ$ess_bulk, na.rm = TRUE),
    min_ess_tail  = min(summ$ess_tail, na.rm = TRUE),
    divergences   = divergent,
    max_treedepth = treedepth,
    min_ebfmi     = ebfmi,
    passes_gate   = max(summ$rhat, na.rm = TRUE) < 1.01 &
      min(summ$ess_bulk, na.rm = TRUE) > 1000 &
      min(summ$ess_tail, na.rm = TRUE) > 1000 &
      divergent == 0 & treedepth < 10 & ebfmi > 0.2,
    stringsAsFactors = FALSE
  )
}

# Appends one row (with an attempt_id: 1 for the first try, 2 for the
# adapt_delta = 0.999 retry) to the in-session gate-attempt log
# (options("fep.gate_attempts")); write_gate_attempts() writes it to file.
# Every attempt is kept, including a failing first attempt that fit_gated()
# would otherwise silently overwrite.
record_gate_attempt <- function(diag) {
  attempts <- getOption("fep.gate_attempts", list())
  attempts[[length(attempts) + 1]] <- diag
  options(fep.gate_attempts = attempts)
  invisible(diag)
}

write_gate_attempts <- function(path) {
  attempts <- getOption("fep.gate_attempts", list())
  df <- if (length(attempts) == 0) {
    data.frame(model = character(0), max_rhat = numeric(0), min_ess_bulk = numeric(0),
              min_ess_tail = numeric(0), divergences = numeric(0), max_treedepth = numeric(0),
              min_ebfmi = numeric(0), passes_gate = logical(0), attempt_id = integer(0))
  } else {
    do.call(rbind, attempts)
  }
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  write.csv(df, path, row.names = FALSE)
  invisible(df)
}

# Fit, check the gate, and re-run once at adapt_delta = 0.999 if it fails.
# In cache-only mode a failing gate stops the run rather than triggering a
# retry, because a retry would mean fitting.
fit_gated <- function(formula, data, prior, family, tag, ...) {
  cache_only <- isTRUE(getOption("fep.cache_only", FALSE))
  fit  <- fit_cached(formula, data, prior, family, tag, ...)
  diag <- diagnose_fit(fit, tag)
  diag$attempt_id <- 1L
  record_gate_attempt(diag)
  if (!diag$passes_gate) {
    if (cache_only) {
      stop("CACHE_ONLY: gate failed for ", tag, "; refit not permitted", call. = FALSE)
    }
    message("  [gate] ", tag, " failed; re-running at adapt_delta = 0.999")
    fit  <- fit_cached(formula, data, prior, family, paste0(tag, "_ad999"),
                       adapt_delta = 0.999, ...)
    diag <- diagnose_fit(fit, paste0(tag, "_ad999"))
    diag$attempt_id <- 2L
    record_gate_attempt(diag)
  }
  list(fit = fit, diag = diag)
}

# -------------------------------
# Posterior summaries
# -------------------------------
# Equal-tailed summaries of a vector of draws on the link scale and after a
# transformation (plogis for proportions, exp for ratios). Medians and
# quantiles are transformation-invariant; means and SDs are reported on the
# link scale only, as the plan specifies.
summarise_effect <- function(draws_link, transf, label, k = NA_integer_) {
  q <- quantile(transf(draws_link), c(0.025, 0.5, 0.975), names = FALSE)
  data.frame(
    model     = label,
    k         = k,
    estimate  = q[2],
    ci_low    = q[1],
    ci_high   = q[3],
    link_mean = mean(draws_link),
    link_sd   = sd(draws_link),
    stringsAsFactors = FALSE
  )
}

# Draws of the pooled effect (b_Intercept), the between-study SD, and the
# chain/iteration/draw indices (so callers can export a fully indexed table).
pooled_draws <- function(fit) {
  d <- posterior::as_draws_df(fit)
  sd_name <- grep("^sd_", names(d), value = TRUE)[1]
  list(mu = d$b_Intercept, tau = d[[sd_name]],
       chain = d$.chain, iteration = d$.iteration, draw = d$.draw)
}

# Prediction draws for a new study: theta_new_j = mu_j + tau_j * z_j, with
# z drawn once under a fixed seed so results are reproducible.
prediction_draws <- function(mu, tau, seed = mcmc_settings$seed) {
  set.seed(seed)
  mu + tau * rnorm(length(mu))
}

# -------------------------------
# Dienes-style Bayes factors
# -------------------------------

# Closed-form Bayes factor for a directional half-normal model of H1,
# B_HN(0, s), against a point null at zero, given a normal likelihood
# summary (estimate d with standard error se):
#   B = 2 * N(d | 0, se^2 + s^2) * Phi(d * s / (se * sqrt(se^2 + s^2)))
#       / N(d | 0, se^2)
# Unit test (Dienes 2020, footnote 4): bf_hn(1, 2, 5) = 0.5615.
bf_hn <- function(d, se, s) {
  v <- se^2 + s^2
  2 * dnorm(d, 0, sqrt(v)) * pnorm(d * s / (se * sqrt(v))) / dnorm(d, 0, se)
}

# Exact random-effects Bayes factor for the normal-normal model with known
# within-study SEs. With v_i = s_i^2 + tau^2 and precision weights w_i, the
# likelihood factorises so that, conditional on tau, the evidence about mu
# is carried by ybar_tau ~ N(mu, sig_tau^2). Hence
#   B_RE = E_{p(tau | y, H0)}[ B_HN(ybar_tau, sig_tau; s) ],
#   p(tau | y, H0) proportional to L0(tau) * HN(tau | 0, tau_scale).
bf_re <- function(y, sei, s, tau_scale, upper = 5) {
  log_l0 <- function(tau) sum(dnorm(y, 0, sqrt(sei^2 + tau^2), log = TRUE))
  # Normalising constant of the H0 posterior for tau (log-scaled for stability)
  c0 <- optimize(function(t) -log_l0(t), c(0, upper))$objective
  post_unnorm <- function(tau) {
    vapply(tau, function(t) exp(log_l0(t) + c0) * 2 * dnorm(t, 0, tau_scale), numeric(1))
  }
  z <- integrate(post_unnorm, 0, upper, rel.tol = 1e-9)$value
  integrand <- function(tau) {
    vapply(tau, function(t) {
      w    <- 1 / (sei^2 + t^2)
      ybar <- sum(w * y) / sum(w)
      sig  <- sqrt(1 / sum(w))
      bf_hn(ybar, sig, s) * post_unnorm(t) / z
    }, numeric(1))
  }
  integrate(integrand, 0, upper, rel.tol = 1e-9)$value
}

# Robustness region for the H1 scale s: the contiguous set of s values on a
# grid that lead to the same evidential category as the primary s.
# Categories: "H1" (B > 3), "H0" (B < 1/3), "insensitive" (otherwise).
bf_category <- function(b) ifelse(b > 3, "H1", ifelse(b < 1 / 3, "H0", "insensitive"))

robustness_region_s <- function(bf_fun, s_primary, grid = seq(0.01, 2.5, by = 0.01)) {
  b_grid   <- vapply(grid, bf_fun, numeric(1))
  category <- bf_category(b_grid)
  target   <- bf_category(bf_fun(s_primary))
  same     <- category == target
  # Contiguity: the run containing s_primary
  idx <- which.min(abs(grid - s_primary))
  if (!same[idx]) stop("Primary s is not in its own category; check the grid.")
  lo <- idx; while (lo > 1 && same[lo - 1]) lo <- lo - 1
  hi <- idx; while (hi < length(grid) && same[hi + 1]) hi <- hi + 1
  contiguous <- sum(same) == (hi - lo + 1)
  list(category = target, s_min = grid[lo], s_max = grid[hi],
       hits_grid_edge = lo == 1 || hi == length(grid),
       contiguous = contiguous, grid = data.frame(s = grid, bf = b_grid, category = category))
}

# -------------------------------
# Null-interval inference (Kruschke 2018 rule, Dienes 2020 reporting)
# -------------------------------

# Posterior probabilities below, inside and above the null interval [-m, m]
# on the log scale.
null_interval_probs <- function(draws_log, m) {
  c(p_below = mean(draws_log < -m),
    p_inside = mean(draws_log >= -m & draws_log <= m),
    p_above = mean(draws_log > m))
}

# Decision from the equal-tailed 95% CrI [L, U] on the log scale against
# [-m, m], with the robustness region for m: the set of minimally interesting
# effects that would lead to the same decision.
null_interval_decision <- function(L, U, m) {
  if (L > m) {
    list(decision = "reject null interval", m_min = 0, m_max = L)
  } else if (L > -m && U < m) {
    list(decision = "accept null interval", m_min = max(abs(L), abs(U)), m_max = Inf)
  } else if (L > 0) {
    list(decision = "suspend judgement", m_min = L, m_max = U)
  } else if (L < 0 && U > 0) {
    list(decision = "suspend judgement", m_min = 0, m_max = max(abs(L), U))
  } else {
    list(decision = "suspend judgement", m_min = NA, m_max = NA)
  }
}

# -------------------------------
# Reproducibility helpers
# -------------------------------

# Run expr under a temporary seed, restoring the caller's random stream
# (including "no seed set yet") on exit. Used to seed the unseeded historical
# pp_check() and bridge_sampler() calls without affecting anything else that
# draws random numbers in the same session.
with_local_seed <- function(seed, expr) {
  had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = globalenv())
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = globalenv())
    } else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      rm(".Random.seed", envir = globalenv())
    }
  }, add = TRUE)
  set.seed(seed)
  expr
}

# SHA-256 of a file's contents, for provenance and fixture checks.
file_sha256 <- function(path) digest::digest(file = path, algo = "sha256")

# Cross-check the fit registry against output/bayesian/fits/: every
# registered fit_tag must have exactly one readable cache file, matched
# exactly (a tag must not match another tag's file as a substring, e.g.
# "loo_X" must not match "loo_XY", "sens_prior_hn1" must not match
# "sens_prior_hn1x").
preflight_cache <- function(registry_path = root_path("data", "registry", "fit_registry.csv"),
                            fits_dir = root_path("output", "bayesian", "fits"),
                            layers = NULL) {
  registry <- read.csv(registry_path, stringsAsFactors = FALSE)
  # layers: restrict to the historical or release layer of the registry
  # (NULL = every registered fit).
  if (!is.null(layers)) registry <- registry[registry$layer %in% layers, ]
  rows <- lapply(registry$fit_tag, function(tag) {
    pattern  <- paste0("^", tag, "_[0-9a-f]{12}\\.rds$")
    files    <- list.files(fits_dir, pattern = pattern)
    readable <- length(files) == 1 && tryCatch(
      inherits(readRDS(file.path(fits_dir, files[1])), "brmsfit"),
      error = function(e) FALSE
    )
    data.frame(fit_tag = tag, n_files = length(files), readable = readable,
              stringsAsFactors = FALSE)
  })
  result <- do.call(rbind, rows)
  n_ok <- sum(result$n_files == 1 & result$readable)
  cat("preflight:", n_ok, "of", nrow(result), "registered fits have exactly one readable cache file\n")
  if (n_ok < nrow(result)) {
    print(result[!(result$n_files == 1 & result$readable), ], row.names = FALSE)
  }
  invisible(result)
}

# -------------------------------
# Release-mode guards (run_all.R --release)
# -------------------------------
# Preconditions: a lockfile that the library satisfies and a clean git tree.
# Both checks are injectable so that the failure paths can be tested without
# a real renv library or a dirty checkout.
check_release_preconditions <- function(lockfile = "renv.lock",
                                        synced_fun = function(lf) renv::status(lockfile = lf)$synchronized,
                                        porcelain_fun = function() system2("git", c("status", "--porcelain"), stdout = TRUE)) {
  if (!file.exists(lockfile)) stop("--release requires renv.lock to exist.", call. = FALSE)
  synced <- tryCatch(synced_fun(lockfile),
                     error = function(e) stop("--release: renv::status() could not be evaluated: ",
                                              conditionMessage(e), call. = FALSE))
  if (!isTRUE(synced)) {
    stop("--release requires renv::status(lockfile = \"", lockfile, "\")$synchronized to be TRUE.", call. = FALSE)
  }
  porcelain <- porcelain_fun()
  if (length(porcelain) > 0) stop("--release requires a clean git tree; uncommitted changes present.", call. = FALSE)
  invisible(TRUE)
}

# After the run: every logged fit must have been fitted in this session.
assert_release_fits_fresh <- function(fits) {
  if (nrow(fits) == 0) stop("--release: no fits were logged; nothing was fitted.", call. = FALSE)
  if (any(fits$from_cache)) {
    stop("--release requires every model to be fitted, not loaded from cache: ",
         paste(fits$model_id[fits$from_cache], collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}
