# Local SAFE introduction builder

The builder edits BSAI and GOA SAFE introductions by FMP, stock, regional component
and subsection, then renders HTML for QA/QC, editable Word, and PDF. It is a local
Shiny app using the installed R/Quarto tools. It preserves the earlier CSV-based
catch-report workflow and all existing package functions.

## Start

Double-click **Start Builder.command** in this checkout. It starts the app when
needed, waits for a valid HTTP response, and opens <http://127.0.0.1:3847> in your
normal browser. Starting it again reuses the running instance. You can close the
launcher Terminal window after startup; the app is owned by macOS rather than the
Codex task terminal. Save now before double-clicking **Stop Builder.command**.

From any terminal, the equivalent service commands are:

```sh
cd ~/_mymods/Council/planteam
/usr/bin/python3 scripts/manage-builder.py start
/usr/bin/python3 scripts/manage-builder.py status
# Save in the app first, then stop it when finished:
/usr/bin/python3 scripts/manage-builder.py stop
```

This is an on-demand `launchd` job for the current logged-in Mac session. It is
loaded only by Start and survives terminal/task completion. It stops at logout
and never starts automatically at boot/login. Its plist is stored in ignored
`appdata/builder-launch.plist`, not in a LaunchAgents/LaunchDaemons startup folder.
Its log is `appdata/builder-service.log`. An occupied port from an unmanaged
process is reported without killing that process. A mismatched service command
is likewise refused. Stop and start the service after changing app code or the
`PLANTEAM_BUILDER_DATA` environment setting.

For first-time setup, install R >= 4.1, Python 3, Quarto CLI and the app packages:

```sh
Rscript --vanilla -e 'install.packages(c("shiny", "jsonlite", "yaml"), repos="https://cloud.r-project.org")'
```

The original foreground debugging command remains available:
`Rscript --vanilla scripts/run-builder.R`. That command requires its terminal
to stay open and should not be used to keep the delivered app available after a
task finishes. Stop a foreground instance with Ctrl+C before using Start.

The server binds only to `127.0.0.1`; there is no public hosting, external sharing,
Google login or automatic data fetch. PDF additionally needs a working XeLaTeX
installation and Times New Roman; both were available on this Mac. Automatic TeX
package installs are disabled, so a missing package/font produces a retained log.

## Help in the app

The prominent **Help & hints** link opens a separate local page at
<http://127.0.0.1:3847/help.html>. **Return to editor** brings the original tab
forward without reloading it, preserving selections, clipboard and undo. If help
was opened directly or its original tab has closed, the return link opens an
editor; a second session may conflict with an existing one. Help includes topic
links, saving/recovery, export paths, common errors and release limitations.

Hover over a control or focus it with Tab for a short description. The same text
is associated with its control using `aria-describedby`; Escape dismisses the
hint. Hints stay available while the pointer is over them or the control has
focus. They do not change input values. Full screen-reader/conformance testing is
still required before claiming accessibility compliance.

Help/JavaScript/CSS updates need a browser refresh, rather than a service restart.
Save first and finish any internal clipboard/undo operation before refreshing;
those items and browser output links belong to the session. Rendered bundles
remain on disk. Application R code changes still require a saved, controlled
stop/start.

## Local versus hosted

Start the app on this Mac with `Start Builder.command`, then open
<http://127.0.0.1:3847/>. Pushing the source to GitHub does not start or host the
R/Shiny backend. GitHub Pages can serve static outputs but cannot run this app.
A server deployment would need R, Quarto, render dependencies, persistent
storage, access controls and separate user drafts. The current single-working-
file design is not ready for shared multiuser hosting; deployment is a separate
project.

## Edit a working document

1. Select BSAI or GOA, a document part/stock, regional component and subsection.
   Both FMPs remain in the same working file. Each has its own report metadata.
2. A new document supplies editable TODO boilerplate and the 20-stock hierarchy
   observed in the examples. BSAI pollock has EBS, AI and Bogoslof components;
   BSAI Pacific cod has EBS and AI. Shared tables remain at Whole stock.
3. Paste narrative with Cmd+V. Paragraphs and line breaks are retained. Text is
   plain: pasted Markdown, HTML, TeX, code chunks and Quarto shortcodes print
   literally. Rich formatting, executable chunks, embedded images and automatic
   citation processing are deliberately outside this first editor.
4. Copy/cut whole subsection uses the app's internal clipboard, including its
   table/source fields. Paste explicitly replaces the target and resets author
   review; Undo restores the preceding cut/paste. Native text copy/paste also
   works in every field. Content edits reset the author-review checkbox.
5. Include/exclude individual subsections, or a whole stock, without deleting its
   content. Excluded content remains in JSON. Empty and unreviewed/stale material
   is visible in draft outputs and in Review and render.
6. Replace TODO prose with current scientific content, supply the accepted
   source/run/approval reference and the report year for which it was reviewed,
   then mark author review. No historical stock-status claims, tiers, ABC/OFL
   values, biomass numbers or apportionments are supplied as current defaults.

Front-matter, appendices and tables are edited in their own subsections. The
renderer orders introductory material before stocks and references/appendices/
summary tables afterward. Historical headings are a starting structure; the
editor does not enforce an adopted management decision or import a PDF's prose.

## Paste tables

Expand Table, caption and units. Paste tab-separated values with one header row
from a spreadsheet. Tables require 2–30 unique, nonempty headers, equal-width rows
and at most 1000 data rows. Cells are literal text: no formulas, conversions,
totals, projections or inferred units are calculated. Preserve zeros, NA and
empty cells explicitly. Embedded cell tabs/newlines are unsupported; put multiline
explanations in the narrative. Supply a caption and units.

For readable portrait output, tables over seven columns are split into labelled
panels; the first two identifying columns repeat in each panel. All remaining
columns and values are retained exactly once. Put stable identifiers such as
stock/area and year first. This differs from historical merged-cell tables and
requires author review for long labels, unusually wide values and complex footnotes.

## Save, reopen and recover

`appdata/working.json` is the authoritative editable working file. It contains
both FMPs, all content, inclusion flags, source fields and review flags. Autosave
runs after an 800 ms pause. Save now before closing or switching machines. Reopen
saved file reloads the disk copy. Download working copy exports the complete JSON;
Working files lets you open that JSON again after schema validation.

Writes use a temporary validated file and atomic rename, with the latest 20 prior
versions retained in `appdata/backups/`. New document and importing a working copy
first preserve the preceding working file. Another browser/session changing the
same disk file causes a save conflict instead of overwriting it. Download your
session's JSON before reopening. A failed save at session end writes an additive
`appdata/recovery-*.json` copy. Save-lock failures are reported explicitly; a
stale `.lock` directory should be removed only after confirming no app process is
writing. Never delete the working file to resolve an error.

The JSON schema is version 2. Early version-1 files migrate by adding one excluded sculpin appendix; all existing content is retained, and the next save backs up the original file. Unknown versions, altered section keys, malformed
JSON and files over 20 MB are rejected without replacing the saved document.
Changing the template structure in code requires a deliberate schema migration.
For a separate test workspace, set `PLANTEAM_BUILDER_DATA` to an absolute local
directory before starting the app. Working data is separate from source code.

## Render and find outputs

Choose Draft or For review, then HTML, Word and/or PDF, and Save and render selected
FMP. Drafts visibly label TODO, missing, stale and unreviewed content. For review
requires completed document metadata and every included subsection's content,
source, current source-review year, review flag and valid table/caption. These
checks do not replace scientific or release approval.

Every render creates a unique bundle under
`_output/builder/<document-id>-<FMP>-<unique-suffix>/`:

- `working-document.json`: the exact editable document snapshot used.
- `document.qmd` and `_quarto.yml`: standalone, nonexecuting Quarto source/config.
- `rendered/document.html`, `rendered/document.docx`, `rendered/document.pdf`: the
  selected formats that succeeded. HTML embeds resources for offline QA/QC.
- `render-html.log`, `render-docx.log`, `render-pdf.log`: logs for requested formats.
- `manifest.yml`: format success/failure, source/input/output MD5 identifiers,
  source revision, software versions, FMP, draft/review mode and open-item count.
- `document.tex` and compiler intermediates may remain for PDF diagnostics.

Runs preserve prior bundles. A failed format remains explicitly failed; other
requested formats can succeed. Links are offered only for outputs that exist and
returned success. A source/validation failure leaves the saved working file
intact. Read the retained log, fix the source or dependency, and render again.

HTML/PDF contents are generated from the document hierarchy. Word uses the
existing `inst/template.docx`; update its table of contents field in Word before
release. Native Word pagination and final accessibility still require target-
editor review. The PDF is currently untagged and is not a Section 508-certified
release artifact. The examples guide structure, rather than promise page-for-page
reproduction, fixed headers, merged cells or historical table numbering.

## Validate the implementation

```sh
Rscript --vanilla app/tests/test-document.R
Rscript --vanilla app/tests/render-smoke.R
```

The first checks keyed edits across FMP/stock navigation, multiline persistence,
JSON round trips, backups, locks/conflicts, internal clipboard/undo, table checks,
markup escaping, stale flags, renderer failure logs and saved-file preservation.
The second renders explicit **synthetic** BSAI and GOA fixtures to all three
formats; it never reads or replaces `appdata/working.json`. Its index is
`_output/builder/smoke-index.json`. See VALIDATION.md for actual run results and
remaining manual checks, and PDF-EXAMPLES.md for the four source examples.

`appdata/` and `_output/` are Git-ignored and excluded from the wiki catalog and
package build. They may contain restricted scientific material: retain them on
approved storage and review their contents before sharing. No Library upload or
publication is part of this workflow.

Implementation references: [Shiny local runApp](https://shiny.posit.co/r/reference/shiny/latest/runapp.html),
[Shiny browser/server messages](https://shiny.posit.co/r/articles/build/js-send-message/),
[Quarto PDF](https://quarto.org/docs/output-formats/pdf-basics.html), and
[Quarto Word](https://quarto.org/docs/output-formats/ms-word.html).
