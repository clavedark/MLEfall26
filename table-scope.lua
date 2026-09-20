--[[
table-scope.lua — mark table header cells so a screen reader can say which
column (or row) a value belongs to.

WHY THIS IS A FILTER AND NOT AN EDIT IN EVERY DECK. Quarto renders in three
stages: the .qmd goes to Pandoc, Pandoc builds a structured model of the
document, and HTML comes out the other end. A Lua filter runs in the middle,
while the document is still a structure rather than text, so it can say "every
header cell in every table gets marked" once and have that hold for every page,
every render, and every deck written from here on. As of 2026-09-19 the site had
227 unmarked header cells across 88 tables; none of them needed touching by hand.

WHAT IT FIXES. WCAG 2.2 SC 1.3.1 (Info and Relationships, Level A). Without
`scope`, a screen reader working across a wide regression table reads the
numbers but cannot reliably say which model column a coefficient sits in. On a
two-column table this hardly matters; on the model-comparison tables it does.

WHAT IT CANNOT REACH. Some tables arrive at Pandoc as a lump of raw HTML that
its table parser rejects — you see "Unable to parse table from raw html block:
skipping" go past during a render. Those never become a Table in the document
model, so nothing here touches them. On discretehazards26 that is 5 tables of
16. Verified 2026-09-19 that the filter DOES reach ordinary markdown tables,
kableExtra `kbl()` output, and modelsummary output (0 of 9 header cells marked
before, 9 of 9 after).

Also verified: a project-level filter still applies to a deck that declares its
own `filters:` block (every topic deck declares `parse-latex`), so this does not
need repeating per file.
]]

function Table(tbl)
  -- Column headers: every cell in the table's head row(s).
  for _, row in ipairs(tbl.head.rows) do
    for _, cell in ipairs(row.cells) do
      cell.attr.attributes['scope'] = 'col'
    end
  end

  -- Row headers: only where the table actually declares leading header columns
  -- (`row_head_columns`). Those cells render as <th>; the rest are <td>, and
  -- putting scope on a <td> is invalid, so this stays conditional rather than
  -- blanket-marking every first cell.
  for _, body in ipairs(tbl.bodies) do
    local n = body.row_head_columns or 0
    if n > 0 then
      for _, row in ipairs(body.body) do
        for i = 1, math.min(n, #row.cells) do
          row.cells[i].attr.attributes['scope'] = 'row'
        end
      end
    end
  end

  return tbl
end
