# =================================================================
# Stage 3 of 5: period-cohort matrix and every derived series the
# figure needs -- stream geometry, the meandering centerline, guide
# gridlines, and the standard-deviation color bands.
# -----------------------------------------------------------------
# Input:  data/02_combined.rds
# Output: data/03_series.rds
#
# Run from anywhere, after stage 2:
#   Rscript scripts/03_series.R
# =================================================================
script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
archive_root <- dirname(dirname(normalizePath(sub("^--file=", "", script_arg))))
setwd(archive_root)

library(viridis)
library(reshape2)
source("R/Functions.R")    # wsd(), groupN()

data_dir <- "data"
stage2 <- readRDS(file.path(data_dir, "02_combined.rds"))
ESP         <- stage2$ESP
lastObsYear <- stage2$lastObsYear
axisMax     <- stage2$axisMax

# =================================================================
# Period-cohort matrix and derived series
# =================================================================
PC <- acast(ESP, Year ~ Cohort, value.var = "Total", fill = 0)

Bt <- rowSums(PC)
Bc <- colSums(PC)

Yrs  <- sort(unique(ESP$Year))
Cohs <- sort(unique(ESP$Cohort))

# Band width for both the stream geometry and its per-band color.
# Originally 5-year bands: each band got ONE flat color from its own
# 5-year mean SD, so wherever the underlying SD moved fast (e.g.
# cohort SD climbing .6 units from 1980 to 1985 alone) a single band
# boundary jumped many steps of the color ramp at once -- reads as an
# abrupt seam no matter how fine the palette itself is. The original
# poster's texture is much finer-grained (near single-year stripes),
# which is also why its color changes look continuous. Single-year
# bands here match that and bound each step to one year's worth of
# real change instead of five.
bandN <- 1
# single calendar year, single-year cohort bins
PC5 <- apply(PC, 1, function(x, Cohs) groupN(x, Cohs, bandN), Cohs = Cohs)
# single cohort bin, single-year calendar bins
P5C <- apply(PC, 2, function(x, Yrs) groupN(x, Yrs, bandN), Yrs = Yrs)

PC5cs <- apply(PC5, 2, cumsum)
P5Ccs <- apply(P5C, 2, cumsum)
PC5cs <- rbind(0, PC5cs)
P5Ccs <- rbind(0, P5Ccs)

BT <- colSums(PC5)
BC <- colSums(P5C)

# -----------------------------------------------------------------
# meandering centerline (ratio of offspring cohort size to same-year
# period size). Cohorts born after maxCompleteCohort (= max(Yrs) - 49)
# have not finished their fertile years, so their BC totals are still
# incomplete -- we build the ratio only from *complete* cohorts and
# hold it flat from there on, exactly as the left tail before 1922 is
# padded. Also capped at max(Cohs) itself: the projection is now
# capped at the 2010 mother cohort, so max(Yrs) - 49 (which reaches
# past 2010 since calendar years run out to ~2065 to close out that
# cohort's fertile span) would otherwise look past the last cohort
# that actually exists in the data.
# -----------------------------------------------------------------
maxCompleteCohort <- min(max(Yrs) - 49, max(Cohs))
yrs_complete       <- min(Yrs):maxCompleteCohort
yrs_completec      <- as.character(yrs_complete)

meander <- BC[yrs_completec] / BT[yrs_completec]

end <- mean(tail(meander, 5), na.rm = TRUE)

Nstart <- min(Yrs) - min(Cohs)
Nend   <- max(Yrs) - maxCompleteCohort

# There's no real HFD data before 1922 to compute an actual pre-1922
# trend from, so this stretch (1867-1921, the bottom mirror's earliest
# cohorts) has always been synthetic padding. It was previously a flat
# hold at the mean of the first 10 real ratios -- checked against the
# original again, whose own left tail visibly curves upward into
# "growth" territory right from its very first cohort, not flat.
# Approximate that same sense (population growth was the norm through
# this era) with a gentle ramp *ascending into* the real first
# observed value, instead of a dead-flat pad. (First attempt started
# high and descended into 1922's real value, which -- since the real
# data then immediately continues rising through the 1920s-30s growth
# phase -- produced a down-then-up kink right at the 1922 seam rather
# than one continuous upward arc; starting lower and ascending into
# the real data continues the same direction through the seam.)
pad_start_val    <- meander[1] / 1.12
pad_ramp         <- seq(pad_start_val, meander[1], length.out = Nstart)
meander_extended <- c(pad_ramp, meander, rep(end, Nend))
yrs_smooth        <- min(Cohs):max(Yrs)

meander_smoothed        <- smooth.spline(x = yrs_smooth, y = log(meander_extended), lambda = 1 / 1e5)$y
names(meander_smoothed) <- yrs_smooth

# 4e4 (an earlier pass, aiming to match how flat the original reads)
# went too far the other way -- the line should visibly curve: up
# through "crude generation growth" (positive baseline, meander > 1,
# more children than same-year births) and down through "crude
# generation contraction" (negative baseline, meander < 1), rather
# than reading as nearly straight.
multiply <- 1.6e5
baseline <- meander_smoothed * multiply

PC5cs2 <- t(t(PC5cs) + baseline[colnames(PC5cs)])
P5Ccs2 <- t(t(P5Ccs) - baseline[colnames(P5Ccs)])

# -----------------------------------------------------------------
# reference gridlines: rather than mirroring the same meander
# baseline above and below the centerline, give the upper grid its
# own gentle trend that follows the top mirror's real shape (Bt,
# births per year) and the lower grid one that follows the bottom
# mirror's real shape (Bc, total offspring) -- heavily smoothed so
# they read as calm guides, not another wavy line competing with the
# centerline.
# -----------------------------------------------------------------
# Checked against the original: its horizontal gridlines clearly dip
# at the Civil War trough, sag through the fertility collapse, and
# lift again with the immigration rebound. Light smoothing (1/20)
# follows those real features instead of smoothing them away.
#
# Anchored so the residual wave is forced to exactly zero at both the
# left and right edges (subtract the straight line between the
# trend's own first/last values, not just the mean) -- this is what
# makes the gridlines land exactly on the tick marks at both ends.
smooth_trend <- function(vals, target_amp) {
  x    <- as.numeric(names(vals))
  logv <- log(vals)
  sm   <- smooth.spline(x, logv, lambda = 1 / 20)$y
  lin  <- sm[1] + (sm[length(sm)] - sm[1]) * (x - x[1]) / (x[length(x)] - x[1])
  resid <- sm - lin
  names(resid) <- names(vals)
  resid / max(abs(resid), na.rm = TRUE) * target_amp
}
extend_flat <- function(trend, full_range) {
  out        <- setNames(rep(NA_real_, length(full_range)), full_range)
  out[names(trend)] <- trend
  first_val  <- trend[1]
  last_val   <- trend[length(trend)]
  out[as.character(full_range[full_range < min(as.numeric(names(trend)))])]  <- first_val
  out[as.character(full_range[full_range > max(as.numeric(names(trend)))])]  <- last_val
  out
}

# Built directly on the real "baseline" (not a separately re-smoothed
# copy) so the gridlines are exactly consistent with where the y-axis
# tick marks themselves are drawn (both anchored off the same baseline
# values at min(Cohs)/max(Yrs)) -- with smooth_trend's own values
# forced to zero at both ends (above), grid_top/grid_bottom therefore
# equal baseline itself, exactly, right at the two tick-mark edges.
# Reduced from an earlier .05 (35k) so the wavy gridlines can't swing
# far enough to put a labeled data point on the visually "wrong" side
# of a round-number line (e.g. the verified 2008 peak of 518k briefly
# rendered below the wavy "500k" line at an earlier, wider amplitude).
grid_amp  <- axisMax * .015
top_trend <- extend_flat(smooth_trend(Bt, grid_amp), yrs_smooth)
bot_trend <- extend_flat(smooth_trend(Bc, grid_amp), yrs_smooth)
grid_top    <- baseline + top_trend
grid_bottom <- baseline + bot_trend

# coloring by SD of age at childbearing
Coh_SD <- apply(PC, 2, wsd, x = Yrs + .5)
Per_SD <- apply(PC, 1, wsd, x = Cohs + .5)

Per_SD5 <- groupN(Per_SD, y = Yrs, n = bandN, fun = mean)
Coh_SD5 <- groupN(Coh_SD, y = Cohs, n = bandN, fun = mean)

# The original poster's legend runs about 4.8-6.8 (its own data's
# real min/max -- black is their lowest observed SD, not an arbitrary
# 0), rendered as a near-continuous ramp of many thin swatches. An
# auto-scaled 0-6.8 range wasted most of the ramp on an artifact tail
# from sparse youngest cohorts. Match the original's range (its own
# real min/max, not a round-number 4-7): set both ends manually to
# bracket where the bulk of the real distribution actually sits
# (median ~5.7-5.8), close to the ~2-unit span the original uses.
sd_lo  <- 4.5
sd_hi  <- 6.8
# Back to the original's own 46-bin legend precision -- with bands
# down to single years, neighboring bands differ by one year's worth
# of SD change instead of five, which is what removes the abrupt
# red-to-beige jump a coarser legend/wider band produced.
breaks     <- seq(sd_lo, sd_hi, length.out = 46)
fillcolors <- viridis(length(breaks) - 1, option = "A")

# Values *below* sd_lo (a handful of far-future projected years/
# cohorts with almost no births left to compute a spread from) are
# clamped back UP into sd_lo's own bin (the darkest end of the
# palette) rather than routed to gray, so the color genuinely
# continues and tapers to a point at the cohort cap (2010) rather
# than stopping short.
Per_SD5_clamped <- pmin(pmax(Per_SD5, sd_lo), sd_hi)
Coh_SD5_clamped <- pmin(pmax(Coh_SD5, sd_lo), sd_hi)

ColsC5 <- as.character(cut(Per_SD5_clamped, breaks = breaks, labels = fillcolors, include.lowest = TRUE))
ColsP5 <- as.character(cut(Coh_SD5_clamped, breaks = breaks, labels = fillcolors, include.lowest = TRUE))

series <- list(
  axisMax = axisMax, lastObsYear = lastObsYear,
  PC = PC, Bt = Bt, Bc = Bc, Yrs = Yrs, Cohs = Cohs,
  PC5 = PC5, P5C = P5C, PC5cs = PC5cs, P5Ccs = P5Ccs,
  PC5cs2 = PC5cs2, P5Ccs2 = P5Ccs2,
  baseline = baseline, yrs_smooth = yrs_smooth,
  grid_top = grid_top, grid_bottom = grid_bottom,
  Per_SD5 = Per_SD5, Coh_SD5 = Coh_SD5,
  sd_lo = sd_lo, sd_hi = sd_hi, breaks = breaks, fillcolors = fillcolors,
  ColsC5 = ColsC5, ColsP5 = ColsP5
)
saveRDS(series, file.path(data_dir, "03_series.rds"))

cat("Stage 3 done: series built for", min(ESP$Year), "-", lastObsYear,
    ", projected to", max(Yrs), "->", file.path(data_dir, "03_series.rds"), "\n")
