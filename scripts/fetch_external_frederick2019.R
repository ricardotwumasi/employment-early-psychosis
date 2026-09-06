# ===============================================
# fetch_external_frederick2019.R - download the Frederick and VanderWeele
# (2019) supplementary data, once, with network access
# ===============================================
# The committed copy at data/external/frederick2019/competitive_employment_any.rds
# is what R/07_external_prior_frederick2019.R reads; that script never
# downloads anything itself. Run this script only if the committed .rds is
# missing (it should not be, in a checkout of this repository).
#
# Usage, from the repository root:
#   Rscript scripts/fetch_external_frederick2019.R
#
# The download logic below (moved verbatim from R/07, where it used to run
# automatically) fetches PLOS ONE's S3 File and extracts the one .rds this
# project uses, then verifies it against the SHA-256 of the committed copy so
# that a change upstream is caught rather than silently adopted.
# ===============================================

source(if (file.exists("R/utils.R")) "R/utils.R" else "utils.R")

committed_sha256 <- "6bf5b2f78888e11b2b76d48af508ff2374036c431170ec61172e7e22c61ca9dd"

ext_dir  <- root_path("data", "external", "frederick2019")
rds_path <- file.path(ext_dir, "competitive_employment_any.rds")
zip_path <- file.path(ext_dir, "pone.0212208.s003.zip")
plos_url <- paste0("https://journals.plos.org/plosone/article/file?",
                   "type=supplementary&id=10.1371/journal.pone.0212208.")

if (!file.exists(rds_path)) {
  dir.create(ext_dir, recursive = TRUE, showWarnings = FALSE)
  if (!file.exists(zip_path)) {
    download.file(paste0(plos_url, "s003"), zip_path, mode = "wb")
  }
  unzip(zip_path, files = "competitive_employment_any.rds", exdir = ext_dir)
} else {
  message("Already present: ", rds_path, " (delete it to re-download)")
}

downloaded_sha256 <- file_sha256(rds_path)
cat("SHA-256 of", rds_path, ":", downloaded_sha256, "\n")
if (!identical(downloaded_sha256, committed_sha256)) {
  stop("Downloaded competitive_employment_any.rds does not match the committed copy's SHA-256.\n",
       "  Expected: ", committed_sha256, "\n",
       "  Got:      ", downloaded_sha256, "\n",
       "This means PLOS ONE's supplementary file changed since 2026-09-02; do not overwrite the",
       " committed .rds without a dated entry in docs/amendment_2026-09-02.md.")
}
cat("Matches the committed copy's SHA-256; no action needed.\n")
