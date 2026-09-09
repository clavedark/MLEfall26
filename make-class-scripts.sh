#!/usr/bin/env bash
# make-class-scripts.sh — regenerate the .R live-demo scripts from their .qmd.
#
# Each script is every code chunk of its deck, in document order, with the Quarto
# chunk options stripped. Section and block headers are written in the form
# RStudio's outline pane reads: label first, four-or-more dashes last, depth set
# by the number of leading `#`. Getting that backwards is what produced a pane
# full of "untitled" entries in September 2026 — see the header rule() below.
#
# Run after editing any of the source .qmd files. The scripts are NOT rendered
# output; nothing regenerates them automatically.
#
# NOTE: buildtemplate26answers.R is the answer key in plain-script form and is
# gitignored on purpose. This script writes it; never commit it.

set -euo pipefail
cd "$(dirname "$0")"

python3 - <<'PY'
import re, pathlib

DROP = (re.compile(r'^\s*options\(htmltools\.dir\.version'),
        re.compile(r'^\s*knitr::opts_chunk\$set'))
W = 78

def clean(s):
    if not s: return ""
    s = re.sub(r'\$[^$]*\$', lambda m: m.group(0).strip('$')
               .replace('\\widehat','').replace('\\','').replace('{','').replace('}',''), s)
    s = re.sub(r'[*`]', '', s)
    for a, b in (("—","--"), ("–","-"), ("’","'"),
                 ("“",'"'), ("”",'"'), ("−","-")):
        s = s.replace(a, b)
    return re.sub(r'\s+', ' ', s).strip()

def rule(text, level):
    # RStudio makes an outline entry from a comment ending in 4+ dashes; the
    # label is what precedes them, and the count of leading #'s sets the depth
    pre = ("#" * level) + " " + text + " "
    return pre + "-" * max(4, W - len(pre))

def parse(qmd_path):
    lines = pathlib.Path(qmd_path).read_text().split("\n")
    blocks, sec, sub, i, in_yaml = [], None, None, 0, False
    while i < len(lines):
        l = lines[i]
        if i == 0 and l.strip() == "---": in_yaml = True; i += 1; continue
        if in_yaml:
            if l.strip() == "---": in_yaml = False
            i += 1; continue
        m = re.match(r'^# (?!=)(.+)$', l)
        if m and not l.startswith("#|"): sec, sub = m.group(1).strip(), None; i += 1; continue
        m = re.match(r'^## (.+)$', l)
        if m: sub = m.group(1).strip(); i += 1; continue
        m = re.match(r'^```\{r\s*(.*?)\}\s*$', l)
        if m:
            hdr = m.group(1).strip()
            label = hdr.split(",")[0].strip() if hdr else ""
            body, i = [], i + 1
            while i < len(lines) and not lines[i].startswith("```"):
                body.append(lines[i]); i += 1
            i += 1
            keep = []
            for b in body:
                if b.startswith("#|"):
                    lm = re.match(r'^#\|\s*label:\s*(\S+)', b)
                    if lm and not label: label = lm.group(1)
                    continue
                if any(d.match(b) for d in DROP): continue
                if b.strip().startswith(("warning =", "fig.retina")): continue
                # Dave's own in-code banners (# ---- LABEL ----) are real
                # signposts, but as written RStudio reads them as top-level
                # sections named "---- LABEL". Re-cut them as level-3 entries
                # so they nest under their block, wording untouched.
                bm = re.match(r'^\s*#\s*-{2,}\s*(.+?)\s*-{2,}\s*$', b)
                if bm:
                    keep.append(rule(bm.group(1), 3)); continue
                keep.append(b)
            while keep and not keep[0].strip(): keep.pop(0)
            while keep and not keep[-1].strip(): keep.pop()
            if keep:
                blocks.append(dict(label=label or "unnamed", sec=sec, sub=sub, code="\n".join(keep)))
            continue
        i += 1
    return blocks

def build(qmd_path, out_path, title, howto=None):
    blocks = parse(qmd_path)
    out = [rule(out_path, 1),
           '# Code from %s -- "%s" (606J-MLE, Fall 2026).' % (pathlib.Path(qmd_path).name, title),
           "#",
           "# Every code block from the source, in document order, with the Quarto chunk",
           "# options (#| echo, #| label, #| fig-height, ...) stripped out.",
           "#",
           "# Sections and blocks are RStudio outline entries -- open the outline pane",
           "# (Cmd-Shift-O) and jump. Blocks are numbered in document order and named for",
           "# their chunk label, which is what the .qmd calls them too.",
           "#"]
    out += (howto or
            ["# HOW TO RUN: open MLEfall26.Rproj FIRST, then this script, so here::here()",
             "# resolves. Run block [01] first -- packages and data -- then any block you",
             "# like; later blocks reuse objects made by earlier ones."])
    out += ["#",
            "# Dropped as render-only: options(htmltools.dir.version), knitr::opts_chunk$set().",
            "#", "# CONTENTS"]
    last = None
    for n, b in enumerate(blocks, 1):
        s = clean(b["sec"]).upper() or "SETUP"
        if s != last:
            out += ["#", "#   " + s]; last = s
        out.append("#     [%02d] %-22s %s" % (n, b["label"], clean(b["sub"])))
    out += ["#", ""]

    last = None
    for n, b in enumerate(blocks, 1):
        s = clean(b["sec"]).upper() or "SETUP"
        if s != last:
            out += ["", rule(s, 1), ""]; last = s
        sub = clean(b["sub"])
        tail = ("%s -- %s" % (b["label"], sub)) if sub else b["label"]
        out += [rule("[%02d] %s" % (n, tail), 2), "", b["code"], ""]
    pathlib.Path(out_path).write_text("\n".join(out).rstrip() + "\n")
    print("%-28s %2d blocks" % (out_path, len(blocks)))

build("binaryextensions126.qmd", "binaryextensions126.R", "Diagnostics, Classification, and Fit")
build("prediction26.qmd", "prediction26.R", "Prediction Methods for MLE Models")
build("buildtemplate26answers.qmd", "buildtemplate26answers.R", "Build Your Own Estimator -- ANSWER KEY",
      howto=["# HOW TO RUN: open MLEfall26.Rproj FIRST, then this script. The ITT data are",
             "# read over the web, so no local data/ folder is needed -- but block [01]",
             "# sources _common.R through here::here(), which does need the project."])
PY
