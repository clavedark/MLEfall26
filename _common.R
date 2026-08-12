# _common.R — shared setup for the MLE Fall 2026 course notes.
#
# Sourced from each topic .qmd's setup chunk so decks don't re-declare the
# Binghamton palette or the plotting helpers. Keep this lean: colors + a few
# ggplot conveniences, nothing that pulls in packages a deck might not want.

# Binghamton University palette --------------------------------------------
# Dave's standing course colors: dark green, bright green, pale green, grey, black.
bucolors <- list("#005A43", "#6CC24A", "#A7DA92", "#BDBEBD", "#000000")

# Named aliases so plot code reads in words, not hex.
bu_green      <- "#005A43"   # primary — dark BU green
bu_lightgreen <- "#6CC24A"   # secondary — bright green
bu_palegreen  <- "#A7DA92"   # tertiary — pale green
bu_grey       <- "#BDBEBD"
bu_black       <- "#000000"

# The two-line default (logit vs probit, y=1 vs y=0, etc.).
bu_two <- c(bu_green, bu_lightgreen)

# Convention (Dave, 2026-08): TEXT annotations inside plots use a DARK color
# (bu_black, or bu_green where it labels a dark-green series) -- never a light
# color (bu_lightgreen / bu_palegreen / bu_grey), which is hard to read on a
# white panel. Light colors are fine for lines, fills, and reference segments.

# ggplot conveniences ------------------------------------------------------
# Discrete BU palette for colored/filled series.
scale_color_bu <- function(...) ggplot2::scale_color_manual(values = unlist(bucolors), ...)
scale_fill_bu  <- function(...) ggplot2::scale_fill_manual(values = unlist(bucolors), ...)

# theme_bu(): the course's default look. Currently theme_minimal() so it's a
# drop-in for existing decks; kept as a named hook so we can evolve one shared
# course theme later without touching every file.
theme_bu <- function(base_size = 12, ...) ggplot2::theme_minimal(base_size = base_size, ...)
