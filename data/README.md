# MLEfall26/data/

This folder holds the data files used in the course `.qmd` topic files. **Contents are intentionally not tracked in git** — see `MLEfall26/.gitignore`.

**One deliberate exception (2026-08-12): `ITT.csv` IS tracked and published**, and linked from the [code page](../code.qmd) with a source credit. The ITT country-year data (Conrad, Haglund, and Moore 2013) is openly distributed, and its merged covariates are World Bank WDI and Polity — both open — so redistributing this derived file with attribution is fine. It is the only data file in the `.gitignore` allowlist (`!data/ITT.csv`).

Two reasons the *rest* stay untracked:

1. **License**. Most datasets used in the course (V-Dem, ANES, ACLED, replication archives, etc.) have terms that disallow redistribution. Pushing them to a public repo is a license violation even if the audience is small.
2. **Repo hygiene**. Data files are bigger than the rest of the course material combined, change between releases, and bloat clone times.

## How to populate this folder

Run the bootstrap script from the course root:

```bash
cd ..       # MLEfall26/
bash populate-data.sh
```

The script copies known data files from their canonical homes on Dave's machine into this folder. To add a new data file, edit the `COPIES` array in `populate-data.sh` and re-run.

## How students get the data

Students download data files from Brightspace, not this repo. The `.qmd` files reference data via `here::here("data", "filename")`, so students who download into a `data/` subfolder of the project root can render the documents themselves.

## What's expected to live here

Whatever the current set of topic files needs. The script in `populate-data.sh` is the authoritative list; if a topic file references a data file not in that list, add it.
