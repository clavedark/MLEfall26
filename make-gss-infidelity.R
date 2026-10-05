# make-gss-infidelity.R --------------------------------------------------------
#
# Builds data/gss-infidelity.csv, the GSS extract behind causalinfidelity26.qmd.
#
# Source: General Social Survey cumulative file 1972-2024, Release 3a (NORC),
# Stata version, downloaded from gss.norc.org. Citation:
#   Davern, Michael; Bautista, Rene; Freese, Jeremy; Herd, Pamela; and Morgan,
#   Stephen L.; General Social Survey 1972-2024. [Machine-readable data file].
#   NORC ed. Chicago, 2026. 1 datafile (Release 3a) and 1 codebook.
#
# Rows: every respondent in a wave that asked EVSTRAY (17 waves, 1991-2022).
# The file keeps the never-married (evstray == 3) so the page can show who
# selects into the ever-married sample. It keeps more variables than the causal
# page uses -- partner counts, own divorce, age at first marriage -- because
# the same extract is meant to serve the count and duration weeks.
#
# GSS missing-value codes (NA(d), NA(i), ...) are converted to plain NA, and
# value labels are dropped; the codes are documented in the GSS codebook.
#
# Run from anywhere: Rscript make-gss-infidelity.R

library(here)
library(tidyverse)

gss_dta <- here::here("data", "gss7224_r3a.dta")

# download and unzip the cumulative file if it is not already in data/
if (!file.exists(gss_dta)) {
  zip <- tempfile(fileext = ".zip")
  download.file("https://gss.norc.org/content/dam/gss/get-the-data/documents/stata/GSS_stata.zip",
                zip, mode = "wb")
  unzip(zip, files = "GSS_stata/gss7224_r3a.dta", exdir = tempdir())
  gss_dta <- file.path(tempdir(), "GSS_stata", "gss7224_r3a.dta")
}

keep <- c("year", "id", "wtssps",
          # outcome
          "evstray",
          # treatment: family structure at 16, and why not with both parents
          "family16", "famdif16",
          # pre-treatment background
          "cohort", "age", "sex", "race", "reg16", "res16", "relig16", "fund16",
          "maeduc", "paeduc", "incom16",
          # respondent's own adult characteristics (post-treatment)
          "degree", "educ", "attend", "marital", "divorce", "agewed", "childs",
          # partner counts (count models)
          "numwomen", "nummen", "partnrs5")

gss <- haven::read_dta(gss_dta, col_select = all_of(keep)) %>%
  haven::zap_missing() %>%
  haven::zap_labels() %>%
  filter(evstray %in% 1:3)

stopifnot(nrow(gss) == 34737)

write_csv(gss, here::here("data", "gss-infidelity.csv"))
