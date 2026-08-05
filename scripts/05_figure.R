# =================================================================
# Stage 5 of 5: draw and export the figure (PDF, PNG or SVG,
# controlled by the OUT_FORMAT env var, default "pdf").
# -----------------------------------------------------------------
# Input:  data/03_series.rds
#         data/04_annotations.rds
# Output: figures/SpainBirthFlows_paper.<pdf|png|svg>
#
# Run from anywhere, after stages 1-4:
#   Rscript scripts/05_figure.R
#   OUT_FORMAT=png Rscript scripts/05_figure.R
#   OUT_FORMAT=svg Rscript scripts/05_figure.R
# =================================================================
script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
archive_root <- dirname(dirname(normalizePath(sub("^--file=", "", script_arg))))
setwd(archive_root)
source("R/Functions.R")    # draw.fork()

data_dir <- "data"
series      <- readRDS(file.path(data_dir, "03_series.rds"))
annotations <- readRDS(file.path(data_dir, "04_annotations.rds"))
list2env(series, envir = environment())
list2env(annotations, envir = environment())

# =================================================================
# Plotting
# =================================================================
# y-axis limits fixed at +/-700 (thousand), ticks at 100/300/500/700 --
# axisMax anchors the visible axis frame itself (left/right axis bars,
# outer guide envelope), separate from "amp" below (the flow's own
# natural scale, still used for annotation heights etc. so those
# aren't thrown off by this fixed cap).
yticks  <- c(100000, 300000, 500000, 700000)
yticks  <- c(-yticks, yticks)

# The bottom mirror plots by Cohort, which reaches back to 1867 --
# well before Yrs starts (1922). xlim uses the full combined range
# (matching yrs_smooth, which already spans min(Cohs):max(Yrs)), with
# a small, roughly symmetric pad on both sides -- checked against the
# original poster's own right edge, which runs almost flush to the
# axis frame with only a sliver of blank space.
topDisplayMaxYr <- max(Yrs)
yrs_smooth_disp <- yrs_smooth[yrs_smooth <= topDisplayMaxYr]
Yrs_disp        <- Yrs[Yrs <= topDisplayMaxYr]

xpad <- diff(range(yrs_smooth_disp)) * .025
xlim <- c(min(yrs_smooth_disp) - xpad, max(yrs_smooth_disp) + xpad)

xticks    <- pretty(range(yrs_smooth_disp), 24)
xticksc   <- as.character(xticks[as.character(xticks) %in% names(baseline)])
xticks20  <- pretty(range(yrs_smooth_disp), 13)
xticks20c <- as.character(xticks20[as.character(xticks20) %in% names(baseline)])
xticks    <- as.integer(xticksc)
xticks20  <- as.integer(xticks20c)

graphics.off()

# Sized to fill its own A4 landscape page completely -- canvas is
# true A4 landscape (11.69 x 8.27in, 1.41:1), not a wider 2.4-2.7:1
# ratio meant for a narrower column. The taller canvas naturally
# stretches the flow less panoramically (taller relative to its width)
# since the horizontal
# scale (inches per year) is close to what it was before while the
# vertical scale grows a lot -- exactly the "fill the page, not just
# sit on it" redesign asked for here.
#
# pointsize bumped from 7 to 11 to match -- text size is set in points
# (physical size), not auto-scaled by canvas size, so without this the
# much bigger canvas would make every label read relatively smaller.
fig_width  <- 11.69
fig_height <- 7.4
pt_scale   <- fig_width / 24
# Output format picked via OUT_FORMAT env var (pdf/png/svg, default
# pdf) -- same drawing code below works unchanged on any R graphics
# device, so this reruns the whole script per format rather than
# rasterizing/converting after the fact (keeps png and svg genuinely
# native raster/vector output instead of a PDF screenshot).
out_format <- Sys.getenv("OUT_FORMAT", "pdf")
dir.create("figures", showWarnings = FALSE, recursive = TRUE)
out_pdf <- sprintf("figures/SpainBirthFlows_paper.%s", out_format)
# png()'s default macOS backend (bitmapType "quartz") renders the
# plain hyphen noticeably shorter than the pdf() device's own built-in
# Helvetica -- type="cairo" is a verified fix (checked via pixel diff
# against the quartz default): still not byte-identical to the PDF's
# dash (cairo resolves a generic system sans, not the exact same
# Helvetica metrics pdf() uses), but visibly closer and a stable
# one-line change.
if (out_format == "pdf") {
  pdf(out_pdf, height = fig_height, width = fig_width, pointsize = 11)
} else if (out_format == "png") {
  png(out_pdf, height = fig_height, width = fig_width, units = "in",
      res = 300, pointsize = 11, type = "cairo")
} else if (out_format == "svg") {
  svg(out_pdf, height = fig_height, width = fig_width, pointsize = 11)
} else {
  stop("Unknown OUT_FORMAT: ", out_format)
}
# Margins enlarged per request, along with the canvas -- the previous
# vector (0.8, 1.1, .3, .7) was tuned tight for a wide, short canvas
# squeezed into a body-text column; this version has a whole A4 page
# to itself, so there's room to give the axis labels and titles more
# breathing space.
par(mai = c(1.0, 1.3, .4, .8) * pt_scale,
    xpd = TRUE,
    xaxs = "i",
    yaxs = "i")

# "amp" is the actual half-range of the rendered flow (PC5cs2/P5Ccs2
# already include the baseline shift pointwise, so this already
# accounts for the meander's contribution everywhere the flow exists).
amp <- max(abs(range(c(unlist(PC5cs2), unlist(-P5Ccs2)))))
yr  <- amp

# the visible axis frame, guide envelope, and annotation heights are
# all keyed off the fixed axisMax (700k) instead of the data-driven
# amp/yr, so the tail cohorts' real taper isn't visually crushed by a
# vertical scale inflated well past what the axis itself shows.
topY   <- axisMax * 1.25
# ylim still needs max(amp, axisMax) as a safety net in case the flow
# itself ever exceeds the fixed 700k cap, so nothing gets clipped.
# widened further for the 3-tier event/callout stagger above (was
# 2.15/1.85), which now reaches one tier higher/lower than before.
# bumped further (was 2.35/2.0) to clear the top title (moved to 1.95)
# and the bottom title/callouts (bottom_y and its tiers moved further
# out) above.
# tightened (was 2.5) -- the previous lower bound left a large dead
# strip below the bottom title/legend row, since the extra buffer
# there was well past what that content actually needs.
ylim <- c(-max(amp * 1.15, axisMax * 2.2), axisMax * 2.15)

plot(NULL,
     type = "n",
     xlim = xlim,
     ylim = ylim,
     axes = FALSE,
     xlab = "",
     ylab = "")

# ------------------------------------------------------------------------
# historical event annotations. Leader lines start at the top SURFACE
# of the flow (baseline + Bt, where the colored area actually ends),
# not at the baseline itself -- otherwise the line cuts all the way
# down through the polygon to the centerline, which reads as messy.
# ------------------------------------------------------------------------
events_all <- rbind(
  data.frame(year_start = events_point$year, year_end = events_point$year,
             label = events_point$label, is_range = FALSE,
             stringsAsFactors = FALSE),
  data.frame(year_start = events_range$year_start, year_end = events_range$year_end,
             label = events_range$label, is_range = TRUE,
             stringsAsFactors = FALSE)
)
events_all$year_mid <- (events_all$year_start + events_all$year_end) / 2
events_all <- events_all[order(events_all$year_mid), ]

event_y    <- axisMax * 1.42
event_drop <- axisMax * .10
# the range/fork event (civil war) drops its branch bar much closer to
# the flow than the point events' stem, keeping the box small while
# staying clear of the flow tops in that era (max ~670k there).
event_drop_range <- axisMax * .30

# A run of tightly-spaced labels (civil war/famine, recession/covid-19,
# ~3-6 years apart) would collide at this pointsize. Alternate close
# labels onto a slightly taller second tier -- a gap flip rather than
# a fixed tier per label, so a run of several tight labels in a row
# (not just pairs) still alternates correctly instead of stacking the
# same tier repeatedly.
# Thresholds widened for the bigger pointsize (11, up from 7 -- labels
# are ~57% wider, so a gap that used to be safe now collides). A
# 2-state flip only ever compares a label to its immediate
# predecessor, so a chain of 3+ mutually tight labels (e.g. 1930/1939/
# 1948-style spacing) can land the two *ends* of the chain back on the
# same tier despite still being too close for that tier's height --
# cycling through 3 tiers instead of 2 fixes that without needing to
# track every previously-placed label's own text extent.
event_gap_thresh <- 16
event_tier_offset <- axisMax * .16
prev_mid <- -Inf
tier <- 0
for (i in seq_len(nrow(events_all))) {
  gap <- events_all$year_mid[i] - prev_mid
  tier <- if (gap < event_gap_thresh) (tier + 1) %% 3 else 0
  prev_mid <- events_all$year_mid[i]
  ty <- event_y + tier * event_tier_offset
  y1 <- events_all$year_start[i]
  y2 <- events_all$year_end[i]
  if (events_all$is_range[i]) {
    xmid <- mean(c(y1, y2))
    draw.fork(xmid, y1, y2,
              y1 = ty, y2 = ty - event_drop_range,
              y3 = baseline[as.character(y1)] + Bt[as.character(y1)],
              y4 = baseline[as.character(y2)] + Bt[as.character(y2)],
              lwd = .8, col = "#2a70e0", lty = 1)
    text(xmid, ty, events_all$label[i], pos = 3, cex = .68, col = "#2a70e0")
  } else {
    yc <- as.character(y1)
    flow_top <- baseline[yc] + Bt[yc]
    segments(y1, flow_top, y1, ty, lwd = .8, col = "#2a70e0", lty = 1)
    text(y1, ty, events_all$label[i], pos = 3, cex = .68, col = "#2a70e0")
  }
}
# -------- end historical-event annotations ----------- #

# y-guides following the meandering baseline (upper/lower boundary
# curves that track the same non-linear trend as the centerline) plus
# vertical decade gridlines, both anchored to baseline at their own x
# so they shift together with the meander rather than sitting on a
# rigid, unrelated straight axis -- matching the original figure.
segments(xticks20,
         baseline[xticks20c] + axisMax,
         xticks20,
         baseline[xticks20c] - axisMax,
         col = gray(.7), lwd = .4)
for (yy in yticks) {
  ref <- if (yy > 0) grid_top else grid_bottom
  lines(yrs_smooth_disp,
        yy + ref[as.character(yrs_smooth_disp)],
        col = gray(.8),
        lwd = .6)
}

# shaded polygons: top = births in year, bottom = children those cohorts
# had. Every single-year band gets its own precise color (bandN <- 1
# upstream); borders are never drawn (NA) -- with single-year bands
# this fine-grained, adjacent colors are already close enough that no
# visible seam reappears.
white_border_every <- 5
pc5_cohs_all <- as.integer(rownames(PC5))
p5c_years_all <- as.integer(rownames(P5C))
# top mirror's own polygons are drawn only through topDisplayMaxYr
# (PC5cs2's columns are in Yrs order, so a positional subset works).
disp_yr_idx <- which(Yrs <= topDisplayMaxYr)
for (i in 1:(nrow(PC5cs) - 1)) {
  polygon(c(Yrs_disp, rev(Yrs_disp)),
          c(PC5cs2[i, disp_yr_idx], rev(PC5cs2[i + 1, disp_yr_idx])),
          col = ColsP5[i],
          border = NA,
          lwd = 1.0)
}
# Every-5th "bold" band divider, drawn as a separate pass, one line
# per bold boundary, matching the reference figure's own dense white
# cohort/year separators. Drawn as each divider's true, unmasked path:
# wherever a cohort/year hasn't started contributing yet, its
# cumulative sum (PC5cs[i+1, ]) is exactly 0, so that stretch of the
# divider sits exactly ON the centerline (PC5cs2 = PC5cs + baseline) --
# mathematically identical, not just close. The centerline itself is
# drawn later (after every polygon and divider here), fully opaque and
# thicker (lwd 2.2 vs the divider's 1.0), so it cleanly overpaints
# those coincident stretches -- the divider then only becomes visible
# once it actually lifts off the centerline for real, reading as one
# line touching and splitting away rather than two parallel ones.
for (i in 1:(nrow(PC5cs) - 1)) {
  if (is.na(pc5_cohs_all[i]) || pc5_cohs_all[i] %% white_border_every != 0) next
  y_line <- PC5cs2[i + 1, disp_yr_idx]
  lines(Yrs_disp, y_line, col = "#FFFFFFB5", lwd = 1.0)
}
# Cohorts beyond maxConfidentCohort (2010) are real but too recently
# born to trust a projected complete trajectory for -- shown gray/
# incomplete instead of their real computed color, matching the
# original poster's own bottom-mirror treatment. Mirror issue at the
# *other* end: HFD only observes ages 12-55 in years >= 1922, so any
# cohort born before 1910 has its earliest fertile years missing
# entirely -- verified numerically (cohort 1867's "completed
# fertility" is a single data point, ~0.3 births; SD/width both climb
# monotonically with the count of observed ages and only stabilize at
# cohort ~1910). Same "not enough real data to trust a complete
# trajectory" problem as the >= 2010 tail, just left- instead of
# right-truncated -- gets the same gray treatment.
maxConfidentCohort <- 2010
minConfidentCohort <- 1910
complete_idx   <- which(Cohs >= minConfidentCohort & Cohs <= maxConfidentCohort)
incomplete_idx <- which(Cohs >= maxConfidentCohort)
left_incomplete_idx <- which(Cohs <= minConfidentCohort)
for (i in 1:(nrow(P5Ccs) - 1)) {
  # Late-period bands (years deep in the projection) only have a thin
  # sliver of cohorts <= 2010 old enough to still be contributing --
  # an even sparser/more degenerate real SD than the ordinary sd_lo
  # cutoff was built to catch. If the *whole* band's real SD is this
  # degenerate, gray it out entirely (both cohort pieces) rather than
  # only the >=2010 side. Widened specifically for projected years
  # (never for real observed ones, which never come this close to
  # sd_lo to begin with).
  band_yr <- p5c_years_all[i]
  band_sd <- Per_SD5[as.character(band_yr)]
  band_is_sparse <- band_sd < sd_lo || (band_yr > lastObsYear && band_sd < 5.0)
  complete_col <- if (band_is_sparse) gray(.8) else ColsC5[i]
  polygon(c(Cohs[complete_idx], rev(Cohs[complete_idx])),
          -c(P5Ccs2[i, complete_idx], rev(P5Ccs2[i + 1, complete_idx])),
          col = complete_col, border = NA, lwd = 1.0)
  polygon(c(Cohs[incomplete_idx], rev(Cohs[incomplete_idx])),
          -c(P5Ccs2[i, incomplete_idx], rev(P5Ccs2[i + 1, incomplete_idx])),
          col = gray(.8), border = NA, lwd = 1.0)
  polygon(c(Cohs[left_incomplete_idx], rev(Cohs[left_incomplete_idx])),
          -c(P5Ccs2[i, left_incomplete_idx], rev(P5Ccs2[i + 1, left_incomplete_idx])),
          col = gray(.8), border = NA, lwd = 1.0)
}
# Same unmasked-divider pass as the top mirror above, mirrored.
for (i in 1:(nrow(P5Ccs) - 1)) {
  if (is.na(p5c_years_all[i]) || p5c_years_all[i] %% white_border_every != 0) next
  y_line <- -P5Ccs2[i + 1, ]
  lines(Cohs, y_line, col = "#FFFFFFB5", lwd = 1.0)
}

# small year markers every 10 years (between the 20-year gridlines),
# matching the in-band year labels on the original Swedish foldout:
# parenthesized, smaller than the axis labels, light-medium gray, and
# tilted to follow the local slope of the band they sit in.
pin_wh <- par("pin")
usr_xy <- par("usr")
slope_to_angle <- function(dx, dy) {
  dx_in <- dx / diff(usr_xy[1:2]) * pin_wh[1]
  dy_in <- dy / diff(usr_xy[3:4]) * pin_wh[2]
  atan2(dy_in, dx_in) * 180 / pi
}
labelGrayCol <- "#5B5B5BB0"

# Pick white over dark bands and keep the original medium gray over
# light/pale bands, based on the real fill color at that exact band
# (relative luminance, standard perceptual weights) -- a single fixed
# medium-gray was illegible over the dark purple/near-black bands.
rel_luminance <- function(hexcol) {
  rgbv <- grDevices::col2rgb(hexcol) / 255
  sum(c(0.299, 0.587, 0.114) * rgbv[, 1])
}
contrastLabelCol <- function(hexcol) {
  if (is.na(hexcol)) return(labelGrayCol)
  if (rel_luminance(hexcol) < 0.5) "#FFFFFFC0" else labelGrayCol
}

# The >1000/>500 thresholds below find nowhere to put a label at all
# inside the gray "unreliable" zones (thin, sparse bands by
# construction), so fall back to the band's own widest point
# (which.max) whenever the threshold search comes up empty, so every
# decade still gets a year marker, gray zone or not.
p5c_years <- as.integer(rownames(P5C))
label_years_bottom <- p5c_years[p5c_years %% 10 == 0]
for (yy in label_years_bottom) {
  ci <- match(yy, p5c_years)  # color/band index (no leading-0 offset)
  i <- ci + 1                 # +1 for the leading 0 row in P5Ccs
  row_vals <- P5C[as.character(yy), ]
  ind <- which(row_vals > 1000)[1]
  if (is.na(ind) && any(row_vals > 0)) ind <- which.max(row_vals)
  if (!is.na(ind)) {
    lo <- max(ind - 3, 1)
    hi <- min(ind + 3, ncol(P5Ccs))
    ang <- slope_to_angle(Cohs[hi] - Cohs[lo], -P5Ccs2[i, hi] - (-P5Ccs2[i, lo]))
    # midpoint of the band's own two boundaries, not its outer edge --
    # reads more clearly than sitting right on the flow's border.
    mid_y <- -(P5Ccs2[ci, ind] + P5Ccs2[i, ind]) / 2
    # Match the polygon loop's own color logic (real color vs. gray
    # override) at this specific cohort position -- a label contrast
    # color picked from the band's nominal ColsC5 alone ignores that
    # the same band can render gray here (truncated cohort, or a
    # wholly sparse projected band) even where its "official" color
    # would be dark.
    band_sd <- Per_SD5[as.character(yy)]
    band_is_sparse <- band_sd < sd_lo || (yy > lastObsYear && band_sd < 5.0)
    label_is_gray <- band_is_sparse ||
      Cohs[ind] <= minConfidentCohort || Cohs[ind] >= maxConfidentCohort
    label_col <- if (label_is_gray) gray(.8) else ColsC5[ci]
    text(Cohs[ind], mid_y, sprintf("(%d)", yy), cex = .42,
         col = contrastLabelCol(label_col), srt = ang)
  }
}
pc5_cohs <- as.integer(rownames(PC5))
label_cohs_top <- pc5_cohs[pc5_cohs %% 10 == 0]
for (yy in label_cohs_top) {
  ci <- match(yy, pc5_cohs)  # color/band index (no leading-0 offset)
  i <- ci + 1                # +1 for the leading 0 row in PC5cs
  row_vals <- PC5[as.character(yy), ]
  ind <- which(row_vals > 500)[1]
  if (is.na(ind) && any(row_vals > 0)) ind <- which.max(row_vals)
  if (!is.na(ind)) {
    lo <- max(ind - 3, 1)
    hi <- min(ind + 3, ncol(PC5cs))
    ang <- slope_to_angle(Yrs[hi] - Yrs[lo], PC5cs2[i, hi] - PC5cs2[i, lo])
    mid_y <- (PC5cs2[ci, ind] + PC5cs2[i, ind]) / 2
    text(Yrs[ind], mid_y, sprintf("(%d)", yy), cex = .42,
         col = contrastLabelCol(ColsP5[ci]), srt = ang)
  }
}

# observed/projected divider: the top mirror's own visible extent
# (baseline to baseline+Bt), in white, no text annotation.
proj_start <- lastObsYear + 1
psc <- as.character(proj_start)
proj_bottom <- baseline[psc]
proj_top    <- baseline[psc] + Bt[psc]
segments(proj_start, proj_bottom, proj_start, proj_top,
         col = "#FFFFFFB5", lwd = .9, lty = "42")

# -----------------------------------------------------------------
# "how to read this chart" example annotation, matching the original
# poster's own worked example ("mother born in year 1720"): a label
# placed inside that cohort's own stripe, left in its real data
# color, positioned at the cohort's *peak* childbearing year (where
# her stripe is widest).
# -----------------------------------------------------------------
ec_i <- match(example_cohort, pc5_cohs_all)
if (!is.na(ec_i)) {
  ind <- which.max(PC5[as.character(example_cohort), ])
  lo <- max(ind - 3, 1)
  hi <- min(ind + 3, ncol(PC5cs))
  ang <- slope_to_angle(Yrs[hi] - Yrs[lo], PC5cs2[ec_i, hi] - PC5cs2[ec_i, lo])
  mid_y <- (PC5cs2[ec_i, ind] + PC5cs2[ec_i + 1, ind]) / 2
  # this cohort's peak year sits in one of the flow's dark purple
  # bands, where a fixed dark-gray color would be nearly invisible.
  text(Yrs[ind], mid_y, sprintf("mothers born in year %d", example_cohort),
       cex = .55, col = "#FFFFFFE0", srt = ang)
}

# centerline (the meandering crude-replacement ratio).
lines(yrs_smooth_disp, baseline[as.character(yrs_smooth_disp)], col = "white", lwd = 2.2)
segments(xticks,
         baseline[xticksc] - max(yticks) * .05,
         xticks,
         baseline[xticksc] + max(yticks) * .05,
         col = "white", lwd = 1.3)

# Separate straight reference line at the 100k mark, bottom mirror
# only, in white, spanning exactly the observed-data window (1922 to
# the year the projection starts) -- distinct from the wavy
# 100/300/500/700 gridlines elsewhere and from the meandering
# centerline right above it.
straight100_y <- grid_bottom[as.character(min(Yrs))] - 100000
segments(min(Yrs), straight100_y, proj_start, straight100_y,
         col = "white", lwd = .8)

# "crude generation contraction" label, placed directly above the
# straight 100k reference line. Verified against the actual data:
# baseline (the log-smoothed BC/BT ratio * multiply) at year 1995 is
# -3,288 -- negative, i.e. genuinely on the contraction side of zero.
# It's a small value because 1995 sits right at a local turning point
# (the ratio rises out of the 1960s-70s trough through the mid-1990s,
# very nearly recrosses 1.0, then reverses hard into the fertility-
# collapse/2008-crisis plunge visible immediately to its right) -- but
# the sign right at the label's own anchor, and the unambiguous multi-
# decade trend it sits in front of, are both contraction.
label_offset <- yr * .025
text(1995, straight100_y + label_offset, "crude generation contraction",
     col = "white", cex = 1.15, font = 3)

# the single remaining black/explanatory top annotation -- the actual
# 2008 birth peak, drawn at its own fixed height just under the blue row.
peak_y <- axisMax * 1.02
yc <- as.character(peak_callout$year)
flow_top <- baseline[yc] + Bt[yc]
segments(peak_callout$year, flow_top, peak_callout$year, peak_y,
         lwd = .8, col = "black", lty = 1)
text(peak_callout$year, peak_y, peak_callout$label, pos = 3, cex = .66, col = "black")

# bottom-mirror annotations: identical text-and-solid-line structure,
# mirrored downward from the "children they had" flow's lower surface
# (baseline - Bc, the cohort-side equivalent of baseline + Bt above).
# pushed further out (was -1.12) -- tier-0 callouts (e.g. "baby-boom
# cohort at its peak", "fertility collapse + 2008 crisis") sat right
# on top of the x-axis year labels (1960, 1980, 2000...) just above
# them.
bottom_y <- -axisMax * 1.32
# Same 3-tier cycling as the top blue annotations above -- the tight
# 1930/1939/1948 run (9 years apart each) is exactly the 3-in-a-row
# case a 2-state flip can't resolve (1930 and 1948, both landing back
# on tier 0, are still only 18 years apart). Offset pushes further
# *down* (more negative) since these sit below the flow, and the
# threshold is wider than the top's (these labels run up to 3 lines).
callout_gap_thresh <- 20
callout_tier_offset <- axisMax * .22
prev_callout_yr <- -Inf
callout_tier <- 0
for (i in seq_len(nrow(callouts_bottom))) {
  yy <- callouts_bottom$year[i]
  gap <- yy - prev_callout_yr
  callout_tier <- if (gap < callout_gap_thresh) (callout_tier + 1) %% 3 else 0
  prev_callout_yr <- yy
  by <- bottom_y - callout_tier * callout_tier_offset
  yc <- as.character(yy)
  flow_bottom <- baseline[yc] - Bc[yc]
  segments(yy, flow_bottom, yy, by, lwd = .8, col = "black", lty = 1)
  text(yy, by, callouts_bottom$label[i], pos = 1, cex = .66, col = "black")
}

# outer x axis labels
text(xticks20, baseline[xticks20c] - axisMax, xticks20, pos = 1, cex = .95, xpd = TRUE)
text(xticks20, baseline[xticks20c] + axisMax, xticks20, pos = 3, cex = .95, xpd = TRUE)

# grid lines over polygons
for (yy in yticks) {
  ref <- if (yy > 0) grid_top else grid_bottom
  lines(yrs_smooth_disp,
        yy + ref[as.character(yrs_smooth_disp)],
        col = "#B3B3B3A0",
        lwd = .5)
}

# left y axis -- anchored at min(Cohs), the true left edge of the data
# (the bottom mirror's earliest cohorts), matching how the original
# Swedish foldout anchors its left axis at min(Cohs) rather than min(Yrs)
push <- baseline[as.character(min(Cohs))]
segments(min(Cohs), -axisMax + push, min(Cohs), axisMax + push)
segments(min(Cohs), yticks + push, min(Cohs) + .5, yticks + push)
text(min(Cohs), yticks + push, abs(yticks) / 1000, pos = 2, cex = 1.05)

# right y axis -- anchored at topDisplayMaxYr (== max(Yrs), 2078)
push2 <- baseline[as.character(topDisplayMaxYr)]
segments(topDisplayMaxYr, -axisMax + push2, topDisplayMaxYr, axisMax + push2)
segments(topDisplayMaxYr, yticks + push2, topDisplayMaxYr - .5, yticks + push2)
text(topDisplayMaxYr, yticks + push2, abs(yticks) / 1000, pos = 4, cex = 1.05)

# titles -- placed clearly outside the grid (beyond the outermost
# gridline at +/-axisMax), anchored at xlim[1] (the true left edge of
# the plot region). No main title/subtitle row: this version doesn't
# carry its own headline.
# moved well above the blue event annotations (was 1.18, which sat
# right in the middle of the vertical connector lines/labels for
# events near the left edge, e.g. "Primo de Rivera dictatorship" --
# the title text and that line's own vertical stem occupied the same
# y at x~1923). Placed above the highest annotation tier instead, so
# no connector line reaches this high at any x.
text(xlim[1], axisMax * 1.95, "Births per year (thousands)", pos = 4, cex = 2.0, font = 2)
# pushed further down (was -1.25) to clear the taller callout block
# below (bigger pointsize + wider callout_tier_offset now reach
# further down before their own 3-line text).
# pushed further down again (was -1.75) to clear the 3rd callout tier.
# pushed a little further (was -2.0) to clear the tier-2 callout
# block, which moved down along with bottom_y above.
text(xlim[1], -axisMax * 2.1, "The children they had", pos = 4, cex = 2.0, font = 2)

# =================================================================
# bottom-margin legend, small and tucked into the corner -- a compact
# horizontal color strip that shouldn't compete with the main chart
# for attention.
# =================================================================
usr             <- par("usr")
plot_bottom_ndc <- grconvertY(usr[3], from = "user", to = "ndc")

# compact color legend, bottom-right corner, flush-ish against the
# right edge of the graph.
#
# Pulled in from the true right edge -- the legend title text
# overhangs the narrow color bar on both sides, and flush-right left
# no buffer for that overhang before running off the actual page.
# widened (.075 -> .12) so the bigger tick labels (5.0/5.5/6.0/6.5 at
# the larger pointsize) don't run into each other.
leg_x1 <- xlim[2] - diff(xlim) * .06
leg_x0 <- leg_x1 - diff(xlim) * .12
# Aligned to the same height as the "The children they had" title
# (drawn below at the same -axisMax*2.1 anchor) so the bottom of the
# figure reads as one aligned row, rather than picking an NDC offset
# independent of where that title actually landed.
bottom_title_y_ndc <- grconvertY(-axisMax * 2.1, from = "user", to = "ndc")
bar_y1_ndc <- bottom_title_y_ndc + .012
bar_y0_ndc <- bottom_title_y_ndc - .008
bar_y1 <- grconvertY(bar_y1_ndc, from = "ndc", to = "user")
bar_y0 <- grconvertY(bar_y0_ndc, from = "ndc", to = "user")

n  <- length(breaks)
xt <- seq(leg_x0, leg_x1, length = n)
nn <- length(xt)
rect(xt[-nn], bar_y0, xt[-1], bar_y1, border = "white", col = fillcolors, lwd = .4)
# ticks at round .5 steps within [sd_lo, sd_hi], positioned by linear
# interpolation into the bar's pixel span rather than requiring an
# exact break match -- matching the original poster's legend precision
# (5.00, 5.50, 6.00, 6.50). Skip ticks flush against either edge
# (matching the original, whose labels start at 5.00 despite the bar
# itself starting a bit lower).
tick_vals <- seq(ceiling(sd_lo * 2) / 2, floor(sd_hi * 2) / 2, by = .5)
tick_vals <- tick_vals[tick_vals > sd_lo + .15 & tick_vals < sd_hi - .15]
tick_x    <- leg_x0 + (tick_vals - sd_lo) / (sd_hi - sd_lo) * (leg_x1 - leg_x0)
text(tick_x, bar_y0, sprintf("%.1f", tick_vals), pos = 1, cex = .8, xpd = TRUE)
text((leg_x0 + leg_x1) / 2, bar_y1,
     "standard deviation of\nage at childbearing (years)",
     pos = 3, cex = .88, font = 2, xpd = TRUE)

# No in-figure caption -- the caption is meant to live in the
# accompanying manuscript text, not drawn onto the image itself.

dev.off()
cat("Stage 5 done ->", out_pdf, "\n")
