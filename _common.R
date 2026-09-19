# _common.R — shared setup for the MLE Fall 2026 course notes.
#
# Sourced from each topic .qmd's setup chunk so decks don't re-declare the
# Binghamton palette or the plotting helpers. Keep this lean: colors + a few
# ggplot conveniences, nothing that pulls in packages a deck might not want.

# Binghamton University palette --------------------------------------------
# Dave's standing course colors: dark green, bright green, teal, grey, black.
#
# ACCESSIBILITY (2026-09-19): every color used to DRAW something — a line, a
# point, a bar boundary, a reference segment — must clear 3:1 against the white
# panel (WCAG 2.2 SC 1.4.11, non-text contrast). The original bright/pale/grey
# were at 2.22:1, 1.60:1 and 1.86:1, i.e. below the floor, so a student with low
# vision or a projector with the lamp going lost those series entirely. The three
# were darkened in place; the names and their positions in `bucolors` did not
# change, so no deck's call sites need editing.
#
#   bu_green       #005A43   8.26:1   unchanged
#   bu_lightgreen  #4E9B2F   3.47:1   was #6CC24A (2.22:1)
#   bu_teal        #00877A   4.42:1   new slot-3 line color (see bu_palegreen)
#   bu_grey        #767676   4.54:1   was #BDBEBD (1.86:1)
#   bu_black       #000000  21.00:1   unchanged
bu_green      <- "#005A43"   # primary — dark BU green
bu_lightgreen <- "#4E9B2F"   # secondary — bright green, darkened to clear 3:1
bu_teal       <- "#00877A"   # tertiary — separates from the greens by HUE, not
                             # lightness, which is the only way to get a third
                             # drawable color inside the 3:1 budget (see below)
bu_grey       <- "#767676"   # neutral, darkened to clear 3:1
bu_black      <- "#000000"

# FILL-ONLY COLORS. These stay light on purpose: they are for confidence ribbons
# and area fills, where the information is carried by the line drawn over the
# band, not by the band. Never use them for a line, a point, a bar that is the
# only mark, or text — below 3:1 on white they are not reliably visible.
#
# THE TRAP THIS PAIR EXISTS TO CLOSE (found 2026-09-19 doing the hazards deck):
# a single constant cannot serve both jobs, because strokes and fills have
# OPPOSITE requirements — a stroke must be dark enough to see, a fill must be
# light enough to see a line drawn on top of it. `bu_grey` was doing both, so
# darkening it to 4.54:1 for the stroke case turned every `fill = bu_grey,
# alpha = .7` ribbon into a near-black slab that swallowed its own fit line.
# Use bu_grey/bu_lightgreen/bu_teal to DRAW, bu_palegreen/bu_greyfill to FILL.
bu_palegreen  <- "#A7DA92"   # light green fill
bu_greyfill   <- "#808080"   # grey fill — the old bu_grey's fill role
#
# WHY bu_greyfill LOOKS TOO DARK IN THE SOURCE AND IS NOT. These fills are drawn
# at alpha, so the hex is not what lands on the page. Over white at alpha 0.7,
# #808080 renders as #A6A6A6 — 2.43:1, visible but well behind an 8.26:1 line.
# The first try at this constant was #D9D9D9, which rendered as #E4E4E4 at
# 1.27:1: fainter than the #BDBEBD it replaced, and invisible on a projector
# (Dave, 2026-09-19: "the interval is invisible"). Judge a fill by its BLENDED
# value, not its hex: bu_contrast(<blend>) is the check, not bu_contrast(<hex>).

# Ordered discrete scale. Position 3 is now bu_teal rather than bu_palegreen:
# a pale fill color has no business being the third *line* color. Length and
# ordering of the first two are unchanged, so existing scale_color_bu() calls
# keep their series colors.
bucolors <- list(bu_green, bu_lightgreen, bu_teal, bu_grey, bu_black)

# The two-line default (logit vs probit, y=1 vs y=0, etc.).
bu_two <- c(bu_green, bu_lightgreen)

# WHY THE SERIES LOOK CLOSER TOGETHER NOW. Clearing 3:1 against white caps every
# color's luminance, which necessarily compresses the gaps *between* them:
# bu_two went from 3.72:1 apart to 2.38:1. That trade is unavoidable inside one
# hue family and it is the right way round — a series you cannot see against the
# panel is worse than two series you must tell apart by their dash pattern. So:
#
#   COLOR makes a mark visible.  LINETYPE (or shape, or a direct label) makes it
#   identifiable.  Any plot with two or more series needs both.
#
# That is SC 1.4.1 (use of color) as well as 1.4.11 — color must never be the
# only channel carrying which-series-is-which.

# Convention (Dave, 2026-08; reaffirmed 2026-09-19): TEXT annotations inside
# plots use a DARK color (bu_black, or bu_green where it labels a dark-green
# series) — never a light color. Light colors are now fine for FILLS ONLY;
# the earlier note said "lines, fills, and reference segments," and the lines
# and segments part of that is what 1.4.11 rules out.

# ggplot conveniences ------------------------------------------------------
# Discrete BU palette for colored/filled series.
scale_color_bu <- function(...) ggplot2::scale_color_manual(values = unlist(bucolors), ...)
scale_fill_bu  <- function(...) ggplot2::scale_fill_manual(values = unlist(bucolors), ...)

# Redundant encoding for line series. Pair with scale_color_bu() so a reader who
# cannot separate the hues — color vision deficiency, greyscale printout, a
# washed-out projector — still gets the series from the dash pattern.
bu_lty  <- c("solid", "22", "4212", "11", "1343")
bu_two_lty <- bu_lty[1:2]
scale_linetype_bu <- function(...) ggplot2::scale_linetype_manual(values = bu_lty, ...)

# theme_bu(): the course's default look. Currently theme_minimal() so it's a
# drop-in for existing decks; kept as a named hook so we can evolve one shared
# course theme later without touching every file.
theme_bu <- function(base_size = 12, ...) ggplot2::theme_minimal(base_size = base_size, ...)

# bu_contrast(): the check behind the numbers in the header comment. Kept here so
# the rule is runnable rather than a claim in a comment — if a color gets added
# or changed, bu_contrast(new) has to clear 3 before it is allowed to draw.
bu_contrast <- function(hex, bg = "#FFFFFF") {
  relL <- function(x) {
    ch <- grDevices::col2rgb(x)[, 1] / 255
    ch <- ifelse(ch <= 0.03928, ch / 12.92, ((ch + 0.055) / 1.055)^2.4)
    sum(c(0.2126, 0.7152, 0.0722) * ch)
  }
  l <- sort(c(relL(hex), relL(bg)), decreasing = TRUE)
  (l[1] + 0.05) / (l[2] + 0.05)
}
