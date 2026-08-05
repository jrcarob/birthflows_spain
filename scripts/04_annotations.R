# =================================================================
# Stage 4 of 5: annotation content -- the historical/political event
# markers, the peak-birth callout, and the bottom-mirror cohort
# callouts. Pure data (no dependency on the built series beyond the
# cited figures already having been verified against it), kept
# separate so the figure's text content can be reviewed/edited
# without touching any plotting code.
# -----------------------------------------------------------------
# Output: data/04_annotations.rds
#
# Run from anywhere:
#   Rscript scripts/04_annotations.R
# =================================================================
script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
archive_root <- dirname(dirname(normalizePath(sub("^--file=", "", script_arg))))
setwd(archive_root)

data_dir <- "data"

# -----------------------------------------------------------------
# historical/demographic event annotations, all one blue row -- only
# the "peak: 518k births" note and the two bottom-mirror notes stay
# in the separate black/explanatory style; every other top annotation
# reads as one consistent timeline of blue markers.
# -----------------------------------------------------------------
events_point <- data.frame(
  year  = c(1923, 1941, 1950, 1975, 1987, 2002, 2014, 2020),
  label = c("Primo de Rivera\ndictatorship",
            "famine",
            "mass labor\nemigration",
            "start of the\ntransition",
            "fertility collapse\n(1978-1998)",
            "immigration-driven\nrebound",
            "recession",
            "covid-19"),
  stringsAsFactors = FALSE
)
events_range <- data.frame(
  year_start = c(1936),
  year_end   = c(1939),
  label      = c("civil\nwar"),
  stringsAsFactors = FALSE
)

# the one top annotation that stays black/explanatory rather than
# joining the blue timeline -- the actual computed 2008 peak (518.5k
# births, verified against both the pipeline's own Bt and Spain's
# official INE figure of ~519.8k).
peak_callout <- data.frame(
  year = 2008, label = "peak:\n518k births\n(immigration\nboom)",
  stringsAsFactors = FALSE
)

# -----------------------------------------------------------------
# bottom-mirror annotations: same text-and-solid-line structure as
# the top annotations, but anchored to the "children they had" side
# and telling the cohort-side half of the story -- which generations
# were unusually small or large. Each is tied to a real computed
# extremum rather than a round/approximate year:
#  - 1930: the true peak cohort by completed fertility (714.3k
#    children, the most of any generation in the series).
#  - 1939: the smallest post-war cohort (the actual Bt/Bc minimum).
#  - 1948: the cohort with the most tightly-concentrated childbearing
#    ages (lowest SD in the series, 4.79 years).
#  - 1964: the true local max in the baby-boom neighborhood (536.7k
#    children), matching this series' own Bt maximum year.
#  - 1994: the cohort with the fewest completed children of any
#    modern generation (366.4k), merged with the fertility-collapse/
#    2008-crisis note since the two years collided once both got
#    their own multi-line label.
# -----------------------------------------------------------------
callouts_bottom <- data.frame(
  year  = c(1930, 1939, 1948, 1964, 1994),
  label = c("cohort of 1930:\nmost children of any\ngeneration (714k)",
            "smallest\npost-war\ncohort",
            "cohort of 1948:\nmost concentrated\nchildbearing ages",
            "baby-boom cohort\nat its peak",
            "fertility collapse + 2008 crisis:\nfewest children of any\nmodern generation (366k)"),
  stringsAsFactors = FALSE
)

# worked "how to read this chart" example: mothers born in year 1935,
# labeled inside that cohort's own stripe at its peak childbearing year.
example_cohort <- 1935

annotations <- list(
  events_point = events_point, events_range = events_range,
  peak_callout = peak_callout, callouts_bottom = callouts_bottom,
  example_cohort = example_cohort
)
saveRDS(annotations, file.path(data_dir, "04_annotations.rds"))

cat("Stage 4 done: annotation content ->", file.path(data_dir, "04_annotations.rds"), "\n")
