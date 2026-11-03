# Workflow evaluation and migration notes

Inspected upstream `main` at `19cf1d4e070526da60ac552d42513ab335c2e954`
(17 November 2024), including R sources, templates, vignettes, bundled files and
catch-report history (`5b97181`, `7a7956c`). This is an R package and a collection
of report templates, not a copy of the separate pollock SAFE project. There were
no AGENTS.md files, local skills, Quarto sources/configuration, test suite,
lockfile or automated build workflow in the checkout.

## Existing execution flow

| Product | Entry and inputs | Actual outputs and caveats |
|---|---|---|
| GOA/BSAI summaries | `inst/rmarkdown/templates/{goa,bsai}_pt_summary/skeleton/skeleton.Rmd`; create with RStudio From Template after installing `planteam`. Both initialize year/meeting/catch dates to 2023 and authenticate with `googlesheets4::gs4_auth()` before reading tab `goa` or `bsai`. | `bookdown::word_document2`, using the skeleton's `template.docx`. Output basename is the chosen Rmd filename. Authors fill assessment narrative placeholders and verify stock-specific statements. |
| Catch report | `catch_report()` calls installed `inst/catch_report.Rmd`, reads the live regional Sheet and selects the four-year window and stock/area. | `paste(toupper(area), stock, ".docx", sep="_")`, e.g. `BOGOSLOF_pollock_.docx`, in supplied `output_dir`. This differs from the vignette's documented name. |
| Specification table | `spec_table(year, month="sep"/"nov", area="goa"/"bsai", catch_date=...)`, reading the same Sheet. | Saves `test.xlsx` in the current directory with `overwrite=TRUE`, including hardcoded GOA sheet/formula locations. Treat as unfinished and verify totals/formulas before use. |
| Workbook reader | `read_specs(path, year, range="A5:L91", ...)` reads an analyst-supplied `.xlsx`. | Returns a tibble, fills species downward; no file output. Twelve columns default to `species,area,0ofl,0abc,0tac,0catch,1ofl,1abc,1tac,1catch,2ofl,2abc`; first two text, remaining ten numeric. `year` is currently unused; the meaning of relative-year prefixes must be confirmed from the actual workbook. |
| Catch upload | `catch_to_sheet(year,data,area)` supports GOA only, but overwrites `data` from `here::here("dev",2024,"catch_table_nov_pt.csv")`. Reads a Sheet and calls `sheet_write()`. | Modifies the live `goa` worksheet; no local output contract. Not run: it has a missing pipe after `drop_na()`, an undeclared `vroom` dependency, hardcoded year/path and unsafe write assumptions. An independently reviewed repair is needed before use. |

All live references identify Google spreadsheet
`1uHmCuY3GXfSBCbsP61nAQeATBfoslXeS3PQNLlioWIk` and regional tabs `goa` / `bsai`.
Access, current contents, approval status and confidentiality were not established.
No production Sheet was read or written.

## Legacy input contracts and bundled material

The summary/specification sources expect `stock,year,area,biomass,rec_age,ofl,abc,tac,catch`
plus workflow columns. Summary setup drops `completed,checked`; other GOA code
instead drops `completed,check,check_2`. Types inferred from usage: stock/area are
text, year is integer, biomass/specifications/catch numeric; `rec_age` is an age
in years used to label age-plus biomass. Narrative establishes biomass and catch
specifications in tonnes, with stock-specific biomass definitions. Confirm the
current schema and units with the data steward; the code is internally inconsistent.

Summary windows are generally year − 1 through year + 2; area apportionments use
year + 1 and year + 2. Expected shorthand and area levels are embedded in each
stock section and in `R/spec_table.R`. There is no universal safe aggregation rule:
`Total`, `Subtotal`, `GOA-wide`, `AK Total` and regional rows can overlap. The new
workflow preserves selected rows and never adds them together.

The BSAI skeleton ships `data/bsai_specs.xlsx`, `data/bio.csv` and
`data/bsai_team.csv`. Current skeletons do not read them: both read Google Sheets.
`bio.csv` has `stock,year,area,biomass,type` and 2020–2023 records, including
comma-formatted biomass and a `type` code without an established current mapping.
The team CSV has `member,chair`. These historical assets are not validated
production snapshots and are not used by the new example. Existing `styles.css`
and Word references are presentation assets; the catch workflow reuses
`inst/template.docx`. No model executable, accepted model run or assessment result
objects are present, and no model is run by the reports.

## What changed and what remains compatible

This is an incremental migration of the catch-report presentation path.
`_quarto.yml` explicitly scopes rendering to one root QMD and fixes execution to
the project root. A checkout-only runner validates local inputs, renders HTML and
Word, exports selected data and records provenance. It has no live Sheet client
and no implicit data fetch. The README separates example and production modes.

The adapter retains the legacy stock/year/area selection: GOA selects `Total`
and `GOA-wide`, BSAI selects every area for that stock, and a subarea selects an
exact match. It preserves numeric values, missing values and row order. It adds
schema/duplicate/year-window validation and accepts `species` as a shorthand
alias because commit `5b97181` changed the catch template from `stock` to
`species` while summary code retained `stock`. It initializes the year before
inline use and displays the supplied authors (the legacy wrapper comments them
out). These fixes apply only to the new path. The new narrative avoids asserting
that an assessment was skipped or that specifications were approved based only
on a filename or template default.

Package R functions, man pages, vignettes, installed templates and scientific
calculations remain unchanged. The Quarto files are excluded from R package
builds. Existing `rmarkdown::render()` calls still use their original templates;
root `quarto render` does not migrate or execute them. Native Markdown tables
replace the legacy flextable layout in the new path; Word pagination and merging
are not promised to be identical.

A full summary migration should first reconcile current Sheet schema/stock-area
coverage, obtain an approved complete snapshot, and compare every table,
apportionment and scientific statement with an accepted baseline. The example
catch test cannot establish those agreements. Specification export and Sheet
upload repairs require separate review because they affect totals or shared data.

## Provenance and review decisions

The staged contract is approved external source → local immutable snapshot →
validated selection → selected CSV → Quarto documents → reviewed release bundle.
The runner records source revision/changes and content checksums, parameters,
software versions and output checksums. This provides lightweight TAF-style
traceability; it does not claim a full TAF execution or an environment lock.
No `asar` dependency is added: this package supplies short templates/tables and
has no standardized model output contract. The existing focused catch report
fits a small incremental Quarto migration better than a new SAFE report framework.

Outstanding decisions: current authoritative schema and stock/area labels;
approved snapshot location/access rules; accepted assessment schedule and source
citation; full-summary migration priority; authoritative totals/formulas for
`spec_table()`; target Word editor and accessibility release review. These remain
analyst/data-steward decisions rather than assumptions embedded in this change.

Implementation references: [Quarto parameters](https://quarto.org/docs/computations/parameters.html)
and [project execution](https://quarto.org/docs/projects/code-execution.html).
