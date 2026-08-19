#!/usr/bin/env bash
# prune-search-index.sh — hold unreleased material out of the public search index.
#
# Quarto builds docs/search.json from EVERY page it renders into docs/, including
# decks, exercise answer keys, and side projects that .gitignore deliberately keeps
# out of the public repo. The index is tracked (the site search needs it), so the
# full text of unreleased material rides along with it. This script rewrites
# docs/search.json down to the pages that are actually published.
#
# The allowlist is derived from .gitignore's `!docs/<page>.html` un-ignore lines,
# so releasing a deck stays a one-place edit: un-ignore it, render, run this, commit.
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
