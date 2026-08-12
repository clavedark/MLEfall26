#!/usr/bin/env bash
# populate-data.sh
# One-shot: copy course data files from their canonical homes into MLEfall26/data/.
# Safe to re-run — uses `cp -n` so existing files in data/ are not overwritten.
#
# To add a new data file: add a line to the COPIES array below.
# To force-refresh a file: delete it from data/ first, then re-run.

set -euo pipefail

# Always work relative to this script's directory so the script can be run from anywhere.
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
DATA_DIR="${SCRIPT_DIR}/data"
mkdir -p "$DATA_DIR"

# Each entry: "<source-path>|<dest-filename>|<note>"
COPIES=(
  # --- Democratic peace (binary models, hazards) ---
  "${HOME}/Documents/teaching/501/2023/slides/L7_limiteddv/code/dp.dta|dp.dta|Oneal & Russett DP, dyad-year"
  "${HOME}/Documents/teaching/606J-mle/2022/exercises/ex1/dp.csv|dp.csv|DP exercise version (CSV)"
  "${HOME}/Documents/teaching/606J-mle/2024/MLEfall24/dptime.csv|dptime.csv|DP with time variable, for hazards"

  # --- Linear-model prediction preview (OLS; predictionOLS26) ---
  "${HOME}/Documents/teaching/501/2023/exercises/ex4/ITT/data/ITT.csv|ITT.csv|Ill-Treatment & Torture, country-year (OLS prediction preview)"

  # --- ANES / CCES (binary extensions, ordered, choice) ---
  "${HOME}/Documents/teaching/606J-mle/2020/slides/L3_binaryextensions/code/anes_pilot_2016.csv|anes_pilot_2016.csv|ANES 2016 pilot"
  "${HOME}/Documents/teaching/606J-mle/2024/MLEfall24/ces.csv|ces.csv|CCES (large; consider git-lfs or .gitignore)"

  # --- Count models ---
  "${HOME}/Documents/teaching/606J-mle/2022/slides/L10_count1/code/H_S_JPR_491_Replication_Revised.dta|H_S_JPR_491.dta|Hill & Sigelman count data"
  "${HOME}/Documents/teaching/606J-mle/2022/slides/L11_count2/code/fordham98jcr.dta|fordham98jcr.dta|Fordham 1998 JCR replication"

  # --- Choice models ---
  "${HOME}/Documents/teaching/606J-mle/2024/topics/choicemodels/nominaldata.dta|nominaldata.dta|Nominal/multinomial example"
  "${HOME}/Documents/teaching/606J-mle/2024/MLEfall24/NAFTA.csv|NAFTA.csv|NAFTA vote data"

  # --- Ordered ---
  "${HOME}/Documents/teaching/606J-mle/2024/topics/ordered-variance/beer.dta|beer.dta|Ordered probit example"
  "${HOME}/Documents/teaching/606J-mle/2024/MLEfall24/pride_index.csv|pride_index.csv|Pride index, ordered"

  # --- Duration / hazards ---
  "${HOME}/Documents/teaching/606J-mle/2022/slides/L12_hazards2/code/one_per_war.csv|one_per_war.csv|War duration, one row per war"

  # --- Selection ---
  "${HOME}/Documents/teaching/606J-mle/2022/slides/L13_selection/code/clarkreed.dta|clarkreed.dta|Clark & Reed selection"

  # --- Misc ---
  "${HOME}/Documents/teaching/606J-mle/2024/MLEfall24/confusion.csv|confusion.csv|Confusion-matrix demo"
)

copied=0
skipped=0
missing=0

for entry in "${COPIES[@]}"; do
  IFS='|' read -r src dest note <<< "$entry"
  dest_path="${DATA_DIR}/${dest}"

  if [ ! -f "$src" ]; then
    echo "  MISS  $dest  (source not found: $src)"
    missing=$((missing+1))
    continue
  fi

  if [ -f "$dest_path" ]; then
    echo "  SKIP  $dest  (already present)"
    skipped=$((skipped+1))
    continue
  fi

  cp "$src" "$dest_path"
  echo "  COPY  $dest  ($note)"
  copied=$((copied+1))
done

echo
echo "Done. copied=$copied  skipped=$skipped  missing=$missing"
echo "data/ now contains:"
ls -la "$DATA_DIR"
