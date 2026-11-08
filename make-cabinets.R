# make-cabinets.R --------------------------------------------------------------
#
# Builds data/cabinets.csv: one row per non-caretaker cabinet in the ParlGov
# parliamentary democracies, 1945 to June 2023, with how long it lasted, how
# it ended, and the cabinet and parliament characteristics the cabinet-duration
# literature uses. Running data for the continuous-time hazards deck.
#
# Source: ParlGov 2024 release (Döring, Huber & Manow), Harvard Dataverse,
# doi:10.7910/DVN/2VZ5ZC, CC0 1.0. "All elections and cabinets until June 2023."
# Raw files are cached in data/parlgov-raw/; delete that folder to re-fetch.
# (parlgov.org itself failed its TLS handshake on 2026-11-01, hence Dataverse.)
#
# CODING RULES -----------------------------------------------------------------
#
# Start  ParlGov's start_date. ParlGov starts a new cabinet at every election,
#        change of prime minister, or change of party composition.
# End    the start_date of the cabinet that succeeds it (its successor names it
#        as previous_cabinet_id). ParlGov stores no end date in the stable
#        release, and its experimental termination_date is coded for only some
#        cabinets, so the successor's start is the one rule that covers all.
# How it ended, from the next parliamentary election after the start date:
#   regular election  that election falls on or before the end date and is not
#                     flagged early -- the cabinet ran out the clock. CENSORED:
#                     we never see how long it would have lasted.
#   early election    the election falls on or before the end date and ParlGov
#                     flags it early -- parliament was dissolved. FAILURE.
#   replaced          no election before the end date -- the cabinet fell and
#                     another formed from the same parliament. FAILURE.
#   in office         no successor by June 2023. CENSORED at 2023-06-30.
# Caretaker cabinets are dropped as units but still count as successors, so a
# cabinet that resigns into a caretaker ends the day the caretaker starts.
# Dropped because cabinets there do not need parliament's confidence:
# Switzerland (collegial Federal Council), Cyprus (presidential), and Turkey
# from Erdogan IV (2018-07-09), under the presidential constitution.
#
# One correction to ParlGov: Orban V (HUN 2022) lists the Fidesz-KDNP alliance
# as a cabinet party with 0 seats and Fidesz itself (117 seats) as outside the
# cabinet. Fidesz is recoded as a cabinet party.
#
# Covariates
#   parties      number of parties in the cabinet
#   seatshare    cabinet parties' share of seats in parliament
#   majority     seatshare > 0.5
#   range        left-right distance between the most extreme cabinet parties
#                (ParlGov 0-10 scale; 0 for single-party cabinets)
#   enpp         effective number of parliamentary parties, 1 / sum(s^2)
#   postelection first non-caretaker cabinet formed after its election
#   cee          Central and Eastern European post-communist democracy
#
# Packages: tidyverse, here -- both in the project library. The cabinet and
# election tables are read from ParlGov's SQLite file with the sqlite3 command
# line tool (ships with macOS) rather than RSQLite, which is not installed; the
# early-election flag exists only in the database, not in the CSV views.

library(tidyverse)
library(here)

raw_dir <- here::here("data", "parlgov-raw")
dir.create(raw_dir, showWarnings = FALSE)

# Dataverse file ids for the 2024 release
files <- c("view_cabinet.csv"        = 10437088,
           "parlgov-experimental.db" = 10437084,
           "codebook.md"             = 10437085)
for (f in names(files)) {
  dest <- file.path(raw_dir, f)
  if (!file.exists(dest)) {
    download.file(paste0("https://dataverse.harvard.edu/api/access/datafile/",
                         files[[f]], "?format=original"),
                  dest, mode = "wb")
  }
}

# run one query against the ParlGov database and return a data frame
sql <- function(query) {
  out <- system2("sqlite3", c("-header", "-csv",
                              shQuote(file.path(raw_dir, "parlgov-experimental.db")),
                              shQuote(query)),
                 stdout = TRUE)
  read_csv(I(out), show_col_types = FALSE)
}

cab <- sql("
  SELECT c.id AS cabinet_id, c.previous_cabinet_id, c.name AS cabinet_name,
         c.start_date, c.caretaker, k.name_short AS country
  FROM cabinet c JOIN country k ON k.id = c.country_id") %>%
  mutate(start_date = as.Date(start_date))

elec <- sql("
  SELECT k.name_short AS country, e.date AS edate, e.early
  FROM election e
  JOIN country k ON k.id = e.country_id
  JOIN info_id i ON i.id = e.type_id
  WHERE i.short = 'parliament'") %>%
  mutate(edate = as.Date(edate))

data_end <- as.Date("2023-06-30")

# successor's start date = this cabinet's end date
ends <- cab %>%
  filter(!is.na(previous_cabinet_id)) %>%
  group_by(previous_cabinet_id) %>%
  summarise(end_date = min(start_date), .groups = "drop") %>%
  rename(cabinet_id = previous_cabinet_id)

# next parliamentary election strictly after each cabinet's start
next_elec <- cab %>%
  select(cabinet_id, country, start_date) %>%
  inner_join(elec, by = "country", relationship = "many-to-many") %>%
  filter(edate > start_date) %>%
  group_by(cabinet_id) %>%
  slice_min(edate, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(cabinet_id, next_election = edate, early)

# party-level rows: every party in parliament, flagged if in cabinet
pv <- read_csv(file.path(raw_dir, "view_cabinet.csv"), show_col_types = FALSE) %>%
  mutate(cabinet_party = if_else(cabinet_name == "Orban V" &
                                   party_name_short == "Fi-MPSz", 1, cabinet_party))

covs <- pv %>%
  group_by(cabinet_id) %>%
  summarise(election_id = first(election_id),
            parties     = sum(cabinet_party == 1),
            seatshare   = sum(seats[cabinet_party == 1], na.rm = TRUE) /
                          first(election_seats_total),
            range       = if (sum(cabinet_party == 1) > 1)
                            diff(range(left_right[cabinet_party == 1], na.rm = TRUE))
                          else 0,
            enpp        = 1 / sum((seats / sum(seats, na.rm = TRUE))^2, na.rm = TRUE),
            .groups = "drop")

cee_codes <- c("BGR", "CZE", "EST", "HRV", "HUN", "LTU", "LVA", "POL",
               "ROU", "SVK", "SVN")

cabinets <- cab %>%
  left_join(ends, by = "cabinet_id") %>%
  left_join(next_elec, by = "cabinet_id") %>%
  left_join(covs, by = "cabinet_id") %>%
  filter(start_date >= as.Date("1945-01-01"),
         !country %in% c("CHE", "CYP"),
         !(country == "TUR" & start_date >= as.Date("2018-07-09"))) %>%
  arrange(country, start_date) %>%
  # first non-caretaker cabinet of each election
  group_by(country, election_id) %>%
  mutate(postelection = as.integer(cumsum(caretaker == 0) == 1 & caretaker == 0)) %>%
  ungroup() %>%
  filter(caretaker == 0) %>%
  mutate(
    end_date = coalesce(end_date, data_end),
    ended_by = case_when(
      start_date == end_date & is.na(next_election) ~ "in office",
      end_date == data_end & !(cabinet_id %in% ends$cabinet_id) ~ "in office",
      !is.na(next_election) & next_election <= end_date & early == 0 ~ "regular election",
      !is.na(next_election) & next_election <= end_date & early == 1 ~ "early election",
      TRUE ~ "replaced"),
    fail     = as.integer(ended_by %in% c("early election", "replaced")),
    days     = as.numeric(end_date - start_date),
    months   = days / (365.25 / 12),
    majority = as.integer(seatshare > 0.5),
    cee      = as.integer(country %in% cee_codes)) %>%
  filter(days > 0) %>%
  select(cabinet_id, country, cabinet_name, start_date, end_date, days, months,
         fail, ended_by, parties, seatshare, majority, range, enpp,
         postelection, cee)

write_csv(cabinets, here::here("data", "cabinets.csv"))

cat(nrow(cabinets), "cabinets;", sum(cabinets$fail), "failures\n")
print(count(cabinets, ended_by))
