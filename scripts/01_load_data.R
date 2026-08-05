# =================================================================
# Stage 1 of 5: load observed HFD data
# -----------------------------------------------------------------
# Input:  HFD "ESP" birthsVV series, fetched live via HMDHFDplus
#         (cached locally to data-raw/ESP_birthsVV.rds on first run,
#         gitignored -- never committed/redistributed, per the HFD
#         User Agreement: https://www.humanfertility.org/Data/UserAgreement)
# Output: data/01_observed.rds
#
# Data provenance: HFD's Spain series (birthsVV/asfrRR/exposRR) is
# compiled by Daniel Devolder from INE (Instituto Nacional de
# Estadistica) Vital Statistics -- birth bulletins filed by Civil
# Registries with INE's provincial delegations. Confirmed via HFD's
# own Spain documentation/data-source notes (humanfertility.org,
# Country=ESP). So the whole series here traces back to INE, just via
# HFD's harmonized/reconstructed cohort-period format rather than
# INE's raw tables directly.
#
# Requires an HFD account (free, register at humanfertility.org) and
# HFD_USER / HFD_PASS set as environment variables (e.g. in .Renviron).
#
# Run from anywhere, e.g.: Rscript scripts/01_load_data.R
# =================================================================
script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
archive_root <- dirname(dirname(normalizePath(sub("^--file=", "", script_arg))))
setwd(archive_root)

# y-axis scale cap, needed again downstream (grid_amp in stage 3, the
# fixed +/-700k axis frame in stage 5) -- defined once here so every
# later stage reads the same constant rather than re-declaring it.
axisMax <- 700000

dir.create("data-raw", showWarnings = FALSE, recursive = TRUE)
raw_file <- "data-raw/ESP_birthsVV.rds"
if (!file.exists(raw_file)) {
  library(HMDHFDplus)
  hfd_user <- Sys.getenv("HFD_USER")
  hfd_pass <- Sys.getenv("HFD_PASS")
  if (hfd_user == "" || hfd_pass == "") {
    stop("Set HFD_USER and HFD_PASS environment variables with your ",
         "Human Fertility Database credentials (free registration at ",
         "https://www.humanfertility.org).")
  }
  ESP_birthsVV <- readHFDweb("ESP", "birthsVV", hfd_user, hfd_pass)
  saveRDS(ESP_birthsVV, raw_file)
}
ESP_obs <- readRDS(raw_file)[, c("Year", "ARDY", "Cohort", "Total")]

out_dir <- "data"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
saveRDS(list(ESP_obs = ESP_obs, axisMax = axisMax),
        file.path(out_dir, "01_observed.rds"))

cat("Stage 1 done: observed", min(ESP_obs$Year), "-", max(ESP_obs$Year),
    "(", nrow(ESP_obs), "rows ) ->", file.path(out_dir, "01_observed.rds"), "\n")
