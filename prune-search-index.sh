#!/usr/bin/env bash
# prune-search-index.sh — trim docs/search.json to the published pages.
#
# Quarto builds the index from every page it renders into docs/, so the tracked
# index needs trimming to match what the site actually serves. The allowlist comes
# from the `!docs/<page>.html` lines in .gitignore, which keeps publishing a
# one-place edit.
#
# Run after every `quarto render`, before committing.

set -euo pipefail
cd "$(dirname "$0")"

python3 - <<'PY'
import json, re, pathlib

allow = {
    m.group(1)
    for line in pathlib.Path(".gitignore").read_text().splitlines()
    if (m := re.fullmatch(r"!docs/([A-Za-z0-9._-]+\.html)", line.strip()))
}

index = pathlib.Path("docs/search.json")
entries = json.loads(index.read_text())
kept = [e for e in entries if e.get("href", "").split("#")[0] in allow]

dropped = sorted({e.get("href", "").split("#")[0] for e in entries} - allow)
index.write_text(json.dumps(kept, indent=2) + "\n")

print(f"search index: kept {len(kept)}/{len(entries)} entries across {len(allow)} published pages")
if dropped:
    print("held out: " + ", ".join(dropped))
PY
