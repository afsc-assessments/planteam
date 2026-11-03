# Validation record

Validated locally on 21 October 2026 in
`/Users/jim/_mymods/Council/planteam`, branch `codex/quarto-workflow`, based on
upstream `19cf1d4`. This record covers checks before the first feature-branch
push. Validation performed no PR creation, deployment, Google Sheet write or
production-data upload.

## Commands and outcomes

| Check | Outcome | Evidence |
|---|---|---|
| `quarto --version`; R session | Pass | Quarto 1.10.18; R 4.6.1; knitr 1.51, rmarkdown 2.31, yaml 2.3.12. Runner captures versions per bundle. |
| `quarto check knitr` | Pass | R detection and a knitr engine render both succeeded. |
| `Rscript --vanilla scripts/test-report.R` | Pass | Offline fixture verifies unchanged values/row order/NA/zero, stock/species aliases, GOA/BSAI/subarea selection, and failures for missing file/columns, conflicting aliases, duplicate keys, missing year, malformed numbers, invalid date/year/schedule, no match, and production use of bundled examples. |
| `Rscript --vanilla scripts/render-report.R --example` | Pass | Actually rendered both `_output/example/catch-report.html` and `.docx`, then wrote selected CSV, provenance YAML and session information. No Google authentication. |
| `quarto render catch-report.qmd --to html` without params | Pass (expected failure) | Exit 1 with `Supply a nonempty doc_title in the parameter YAML.` No automatic data fallback. |
| `Rscript --vanilla -e 'rmarkdown::render("README.Rmd", quiet=TRUE)'` | Pass | Regenerated README.md from the maintained Rmd source. Temporary README.html preview removed. |
| HTML content/structure | Pass | `en-US`, main title and headings, explicit EXAMPLE ONLY label, 6 table column headers and 4 data rows, expected years/zero/NA, Council SAFE-library URL and table caption present. |
| DOCX content/structure | Pass | Valid OOXML ZIP, synthetic label and authors, expected data table (4 data rows, 6 columns) and external Council relationship. Quarto wraps the table caption in an additional layout table. |
| Output/provenance consistency | Pass | Input and selected-data values compared; manifest checksum entries and output files checked. Source and parameter checksums recorded. |
| `R CMD build --no-build-vignettes --no-manual /Users/jim/_mymods/Council/planteam` | Pass | Built `planteam_0.1.0.tar.gz` in the task workspace. Quarto workflow/inputs/outputs/scripts excluded from the package tarball. |
| `R CMD check --no-manual --no-build-vignettes --ignore-vignettes planteam_0.1.0.tar.gz` | Failed at dependency gate | `Package required but not available: openxlsx2`; remote CRAN/Bioconductor indexes also inaccessible in the sandbox. Later checks did not run. Log: `/Users/jim/Documents/Codex/2026-10-21/task-6/planteam.Rcheck/00check.log`. |
| `git diff --check` | Pass | No whitespace errors. Existing R functions/templates/vignettes have no changes. |
| `air format scripts` | Not run | `air` is absent. No formatter or legacy dependencies installed globally. |
| Production/legacy live workflows | Not tested | No approved production snapshot or Sheet authorization established; live functions and unfinished spreadsheet/export code were not executed. |

In a vanilla R session, legacy Imports `bookdown`, `flextable`, `googlesheets4`,
`here`, and `openxlsx2` are unavailable. Jim's normal R startup adds libraries
that supply the first four; `openxlsx2` remains unavailable and blocks package
checking. The smaller Quarto path uses only the three explicitly documented
report dependencies and does not require the full package. The R `quarto`
package is absent and unnecessary because the runner calls the installed CLI.

## Layout and accessibility limits

| Criterion | Outcome | Evidence or remaining check |
|---|---|---|
| Language, semantic source, table caption and headers | Pass | Quarto YAML/Markdown and generated HTML inspected offline. |
| Meaningful title, example label, units and missingness | Pass | Present in both formats; synthetic values are explicitly unsuitable for advice. |
| Link targets | Pass for static presence | Council URL retained in HTML and DOCX; external availability was not fetched during the offline artifact check. Local wiki links checked separately by `wiki/build.py`. |
| Visual HTML layout in browser | Not tested | File preview was explicitly rejected by browser security policy: local `file:` protocol is unsupported. No workaround attempted. |
| Visual Word pagination/layout | Not tested | Microsoft Word app inspection timed out; the workspace dependency loader reported that bundled document tools are unsupported for this thread. No bundled renderer runtime was available through that loader. |
| Screen-reader navigation, reading order and table associations | Not tested | Requires manual review in the target browser/editor and agency workflow. |
| PDF checks | Not applicable | PDF is not an output of this workflow. |

Successful rendering and structural checks do not establish Section 508
conformance. Before release, an analyst must visually inspect the complete
production HTML and Word documents, confirm table widths/page breaks, and perform
manual accessibility and scientific review.

## Assessment-workflow gates

Track: Pass (incremental catch-report migration). Provenance: Pass for this local
example bundle; production source approval remains Not Tested. Scientific:
Not Tested against production; example selection regression passes and existing
scientific code is unchanged. Consistency: Pass for the synthetic fixture.
Render: Pass for HTML/Word generation. Accessibility: partially tested as above;
release review remains open. Full-summary migration and specification totals
are intentionally outside the certified example scope.

## Council wiki

The existing source is `/Users/jim/_mymods/Council/wiki/planteam.qmd`, linked from
the overview and Catch & management sidebar. `build.py` catalogs a narrow set of
planteam source/documentation/example paths and excludes `inputs/`, `_output/`,
Git internals and historical generated package documentation. The manually linked
example report is clearly labelled synthetic. Run `python3 wiki/build.py` from
Council to render the existing wiki and check local references. The actual build passed: **11 pages and 745 local references, zero errors**.
Catalog exclusion tests also passed for private inputs, production outputs and
Git/generated docs; the synthetic fixture is indexed. All changes and
rendered wiki pages are local only. Pre-edit wiki sources are retained under
`/Users/jim/Documents/Codex/2026-10-21/task-6/council-wiki-before/` for comparison.


## Document-builder expansion validation

The later app request is implemented in `app/`, with launcher
`scripts/run-builder.R`, usage guide `BUILDER.md`, and `PDF-EXAMPLES.md`. The four
Library PDFs were transferred successfully and inspected; the four subsequently
copied root PDFs were verified separately. Three match previously inspected bytes;
`GOAintro 2.pdf` adds the November 2023 GOA draft. Five unique references were
inspected overall. The root PDFs and unrelated `assessment_2026.html` are preserved.

- **Passed:** `Rscript --vanilla app/tests/test-document.R`: both FMP/20-stock maps,
  multiline keyed edits after navigation, JSON save/reopen, atomic backups,
  stale-session conflicts and locks, schema-1-to-2 additive migration, rejected
  malformed JSON/section maps/tables, copy/cut/paste/undo, and renderer-failure
  logs with the input snapshot preserved. Shiny server tests exercise actual
  observers, rather than only pure serialization helpers.
- **Passed:** `Rscript --vanilla app/tests/render-smoke.R`: actual HTML, DOCX and
  PDF for synthetic BSAI and GOA documents. The latest exact paths are recorded in
  `_output/builder/smoke-index.json`. Every render bundle keeps its source, input
  JSON, format logs and manifest. Earlier failed attempts are retained as failed
  bundles; only the latest index denotes the successful smoke run.
- **Passed:** all four pages of each synthetic PDF were rasterized with Poppler
  and visually inspected: readable headings, literal pasted code, captions,
  seven-column stock table, two-panel wide table, and no clipping or overlap.
  PDF output is Letter, Times New Roman, produced with installed XeTeX/TeX Live
  2025. The initial unavailable TeX Gyre font was replaced by the Mac's installed
  Times New Roman; TeX automatic installation remains disabled.
- **Passed:** rendered HTML browser inspection and semantic/text checks. Pasted
  script tags, R chunks, Quarto shortcodes, math markers and TeX commands remain
  literal; computation is disabled. Tables retain NA and zero values and have
  generated captions. Native app navigation and saved multiline BSAI text were
  checked in the live localhost interface.
- **Passed with limits:** actual DOCX ZIP/OOXML content and tables checked; macOS
  Quick Look generated an HTML preview and thumbnail that were visually checked.
  Quick Look omits the cached contents field, so update that field in Word.
  Exact Microsoft Word pagination and a full Word accessibility audit remain
  **Not Tested** because direct Word automation timed out earlier. No desktop
  LibreOffice fallback was used.
- **Local only:** app at `http://127.0.0.1:3847`; working data in ignored
  `appdata/`; unique rendered bundles in ignored `_output/builder/`. The wiki
  catalogs app sources and documentation, while excluding appdata, production
  outputs, root reference PDFs and unrelated HTML.

Synthetic fixtures contain invented values and are labelled unsuitable for advice.
Production inputs, author decisions and management outcomes remain unvalidated.
PDFs are untagged; no Section 508 conformance claim is made. Full legacy package
checking remains blocked by missing `openxlsx2`, independently of this app.


## App lifecycle repair

After the delivered task ended, port 3847 had no listener and HTTP requests
returned connection refused. The previous managed foreground process/session no
longer existed. The delivered URL depended on a task-owned terminal rather than
a durable user-invoked process; no browser-routing problem was found.

`Start Builder.command` and `scripts/manage-builder.py start` now explicitly load
an on-demand GUI-session `launchd` job, with its plist/log under ignored appdata.
No startup registration is written to LaunchAgents/LaunchDaemons. The app survives
task/terminal exit and stops at logout or explicit Stop. The launcher refuses
unmanaged port conflicts and verifies its own service command before stopping it.

Verified a live process with parent PID 1 and its own process group, listening
only at 127.0.0.1:3847; HTTP 200 with the expected app marker. Repeated starts reused
the same PID. The initiating command had exited before separate status, port and
HTTP checks were run. A fresh browser reload connected its Shiny session, loaded
the saved working file and switched to GOA with 21 navigation choices and the
expected saved metadata. The saved working JSON checksum was unchanged
before and after startup/browser verification. User documents/backups were not
rewritten. Service diagnostics are in `appdata/builder-service.log`.

Launcher guard checks also passed:
`PYTHONDONTWRITEBYTECODE=1 /usr/bin/python3 app/tests/test-launcher.py` (four tests)
verifies unmanaged ports are left untouched, repeated starts do not replace the
process, mismatched commands are never stopped, and absent jobs trigger no signal.
Shell launcher syntax and Python parsing passed. Later independent checks still
reported the same process, HTTP readiness and the identical working-file checksum.
The Council wiki was rebuilt with the Start/Stop links and checked locally.


## Follow-up: contextual help

Implemented mouse-hover and keyboard-focus hints in the existing static assets,
with `aria-describedby`, visible focus rings, Escape dismissal and a hoverable
hint surface. A prominent Help & hints link opens `app/www/help.html` separately;
Return to editor focuses the originating tab without reloading its session.
A direct visit has an ordinary editor-link fallback. The page describes the
actual selection/edit/save/recovery/export behavior, output paths, common errors,
local versus hosted operation, and scientific/accessibility release limitations.
No R document/render calculations, schema, dependencies or launcher behavior
changed.

Passed in Chrome on this Mac on 21 October 2026:

- Mouse-only hover on the render-mode label showed its hint while focus stayed
  outside the control; moving onto the hint kept it visible.
- Tab focus showed the stock-selector hint, with its `aria-describedby` target;
  Escape dismissed it. Focus outlines and hint placement were inspected visually.
- Initial QA caught a narrative hint obscuring clipboard buttons. Placement was
  corrected to prefer above the control; Copy worked in the revised browser UI.
- Mouse and Enter activation opened Help; mouse and keyboard Return closed Help
  and restored the same editor. Nondefault GOA/Pacific cod/trends selection,
  multiline narrative and the BSAI subsection clipboard remained intact.
- Help topic anchors and the direct-visit return fallback worked. Desktop Help
  layout and the live production Help URL were inspected visually.
- Synthetic BSAI summary, EBS pollock and GOA cod edits saved in a separate
  task-workspace storage directory; refresh/reopen retained multiline and literal
  markup text. The temporary port-3848 test server was stopped after QA.
- `Rscript --vanilla app/tests/test-document.R` passed model and Shiny observer
  checks. JavaScript syntax checks (`node --check` for both scripts), Help HTML
  language/headings/unique IDs/topic anchors/local assets, and `git diff --check`
  passed. No browser console warnings/errors were observed in the QA session.
- `quarto render planteam.qmd` in the existing Council wiki succeeded; its
  validator checked 11 pages and 761 local references without errors.

The live Chrome editor was saved and refreshed to load the changed static assets.
Its original BSAI/front-matter/summary selection, Review and render tab, draft mode
and Word-only format selection were restored. Its internal clipboard was empty
before refreshing. The saved production working JSON retained the same checksum
as at the start of the help task. The independent port-3847 app service was not
restarted. As documented, a refresh clears session render links; existing bundles
remain on disk.

Not rerun for this static-help change: synthetic HTML/DOCX/PDF rendering and the R
package build, which passed before the preceding push. Full screen-reader,
accessibility conformance and native Word pagination reviews remain outstanding.
Before the authorized help push on 22 October 2026, the model/Shiny checks,
JavaScript syntax, Help structure/links/assets and diff whitespace checks were
rerun successfully. This record describes the pre-push validation; hosted
deployment remains outside this follow-up.
