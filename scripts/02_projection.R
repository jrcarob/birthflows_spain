# =================================================================
# Stage 2 of 5: fertility-rate projection, capped at the last-
# observed mother cohort (2023, not the calendar year) -- extended
# from 2010 to 2023 so the bottom mirror's cohort axis reaches as far
# as the original poster's own (which runs its cohorts through its
# last observed year too). Cohorts up to maxConfidentCohort (2010,
# set later in stage 5) are shown in full real color; 2011-2023 are
# real cohorts but too recently born to trust a projected complete
# trajectory for, so they're marked gray/incomplete instead of
# colored.
#
# Projection method: de Beer (1985/1989) CARIMA extrapolation of
# observed age-specific fertility rates (the same method + code
# already used for the Swedish forecast in this repo,
# R/Method13_deBeer1985and1989-swe.R), applied to Spain's HFD ASFR
# series and combined with the female population held at its 2023
# age structure (no separate population forecast is available in
# this project, so this is a rates-only "what if fertility follows
# its recent trend and the population stays the size it is today"
# illustration, not an official population/birth forecast).
# -----------------------------------------------------------------
# Input:  data/01_observed.rds
#         HFD "ESP" asfrRR/exposRR series, fetched live via HMDHFDplus
#         and cached to data-raw/ (gitignored -- see stage 1's header)
#         (only fetched if the projection cache doesn't exist yet)
# Output: data/02_combined.rds
#
# Requires HFD_USER / HFD_PASS environment variables (see stage 1).
#
# Run from anywhere, after stage 1:
#   Rscript scripts/02_projection.R
# =================================================================
script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
archive_root <- dirname(dirname(normalizePath(sub("^--file=", "", script_arg))))
setwd(archive_root)

library(reshape2)
library(data.table)
source("R/Method13_deBeer1985and1989-swe.R")    # de Beer ASFR projection
source("R/Functions.R")    # RR2VV()

data_dir <- "data"
stage1 <- readRDS(file.path(data_dir, "01_observed.rds"))
ESP_obs <- stage1$ESP_obs
axisMax <- stage1$axisMax

maxCohortCap <- 2023
proj_file <- "data/ESP_births_proj.rds"
if (!file.exists(proj_file)) {
  dir.create("data-raw", showWarnings = FALSE, recursive = TRUE)
  asfr_raw_file  <- "data-raw/ESP_asfrRR.rds"
  expo_raw_file  <- "data-raw/ESP_exposRR.rds"
  if (!file.exists(asfr_raw_file) || !file.exists(expo_raw_file)) {
    library(HMDHFDplus)
    hfd_user <- Sys.getenv("HFD_USER")
    hfd_pass <- Sys.getenv("HFD_PASS")
    if (hfd_user == "" || hfd_pass == "") {
      stop("Set HFD_USER and HFD_PASS environment variables with your ",
           "Human Fertility Database credentials (free registration at ",
           "https://www.humanfertility.org).")
    }
    saveRDS(readHFDweb("ESP", "asfrRR", hfd_user, hfd_pass), asfr_raw_file)
    saveRDS(readHFDweb("ESP", "exposRR", hfd_user, hfd_pass), expo_raw_file)
  }
  asfr_df <- readRDS(asfr_raw_file)
  ASFR    <- reshape2::acast(asfr_df, Age ~ Year, value.var = "ASFR")

  deBeer <- Method13_deBeer1985and1989.R(ASFR, joy = 2023, obs = 30, age1 = 12, age2 = 55,
                                          parameter = c(1, 0, 0, 1, 0, 0), len = 67, pop = "ESP")

  expo    <- readRDS(expo_raw_file)
  Pop2023 <- expo$Exposure[expo$Year == 2023]
  names(Pop2023) <- expo$Age[expo$Year == 2023]

  projYears <- as.character(2024:2090)
  predASFR  <- as.matrix(deBeer$predASFR[, projYears])
  storage.mode(predASFR) <- "numeric"

  ages  <- rownames(predASFR)
  Bproj <- predASFR * Pop2023[ages]  # rates x population held at 2023 age structure

  Bproj_long <- reshape2::melt(Bproj, varnames = c("Age", "Year"), value.name = "Births")
  Bproj_long$Age  <- as.integer(as.character(Bproj_long$Age))
  Bproj_long$Year <- as.integer(as.character(Bproj_long$Year))
  Bproj_long <- data.table(Bproj_long)
  Bproj_long <- Bproj_long[, RR2VV(.SD), by = list(Year)]
  Bproj_long$Cohort <- Bproj_long$Year - Bproj_long$Age
  Bproj_long <- Bproj_long[Bproj_long$Cohort <= maxCohortCap, ]
  setnames(Bproj_long, c("Age", "Births"), c("ARDY", "Total"))
  Bproj_long <- as.data.frame(Bproj_long)[, c("Year", "ARDY", "Cohort", "Total")]
  saveRDS(Bproj_long, proj_file)
}
ESP_proj <- readRDS(proj_file)

ESP_obs$Projected  <- FALSE
ESP_proj$Projected <- TRUE
ESP <- rbind(ESP_obs, ESP_proj)

# The maxCohortCap filter above only ever applied to the projected half
# (Bproj_long) -- HFD's own observed series includes a handful of births
# to very young (age ~12) mothers, which puts a stray 2011 cohort into
# the data even though the design caps at 2010. That single-record
# "cohort" gave wsd() a degenerate near-zero SD, which slipped past the
# sd_lo clamp used elsewhere (that clamp only guards the main flow's
# color bands, not the raw Coh_SD line in the bottom-right inset) and
# showed up as a plunge-to-0 spike at the inset's right edge.
ESP <- ESP[ESP$Cohort <= maxCohortCap, ]

lastObsYear <- max(ESP_obs$Year)  # 2023: the observed/projected split point

saveRDS(list(ESP = ESP, lastObsYear = lastObsYear, maxCohortCap = maxCohortCap,
             axisMax = axisMax),
        file.path(data_dir, "02_combined.rds"))

cat("Stage 2 done: observed", min(ESP_obs$Year), "-", lastObsYear,
    ", projected to", max(ESP$Year), "->", file.path(data_dir, "02_combined.rds"), "\n")
