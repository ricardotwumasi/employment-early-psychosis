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
# options(fep.refit = TRUE) to ignore the cache.
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
  if (!refit && file.exists(cache_file)) {
    message("  [cache] ", tag)
    return(readRDS(cache_file))
  }
  message("  [fit] ", tag)
  fit <- brms::brm(
    formula = formula, data = data, prior = prior, family = family,
    chains = mcmc_settings$chains, warmup = mcmc_settings$warmup,
    iter = mcmc_settings$iter, seed = mcmc_settings$seed,
    backend = mcmc_settings$backend,
    control = list(adapt_delta = adapt_delta),
    save_pars = if (save_all_pars) brms::save_pars(all = TRUE) else NULL,
    refresh = 0, silent = 2, ...
  )
  saveRDS(fit, cache_file)
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

# Fit, check the gate, and re-run once at adapt_delta = 0.999 if it fails.
fit_gated <- function(formula, data, prior, family, tag, ...) {
  fit  <- fit_cached(formula, data, prior, family, tag, ...)
  diag <- diagnose_fit(fit, tag)
  if (!diag$passes_gate) {
    message("  [gate] ", tag, " failed; re-running at adapt_delta = 0.999")
    fit  <- fit_cached(formula, data, prior, family, paste0(tag, "_ad999"),
                       adapt_delta = 0.999, ...)
    diag <- diagnose_fit(fit, paste0(tag, "_ad999"))
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

# Draws of the pooled effect (b_Intercept) and the between-study SD.
pooled_draws <- function(fit) {
  d <- posterior::as_draws_df(fit)
  sd_name <- grep("^sd_", names(d), value = TRUE)[1]
  list(mu = d$b_Intercept, tau = d[[sd_name]])
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
