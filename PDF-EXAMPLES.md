# PDF reference inspection

All four user attachments were materialized through their authorized Library
transfer URLs, checked for PDF signatures and expected byte sizes, and given the
returned Library version xattr. Original filenames and bytes were preserved in
`/Users/jim/Documents/Codex/2026-10-21/task-6/reference-pdfs/`. These are three
vintages of one BSAI product and one GOA product, not four report types. PDF body
content identifies the year; embedded metadata is occasionally inconsistent.

| File | Product identified from content | PDF pages | SHA-256 |
|---|---|---:|---|
| BSAIintro.pdf | 2024 BSAI introduction | 50 | c6fc2b23431076ed1fb6817f2d5c8b2a053cc10bf46c68e652057fc5207d7361 |
| BSAIintro(1).pdf | 2023 BSAI Council draft introduction | 50 | 2cad4cd4cb7d64d62231c8f80957172e483814539acf99e1c14d815b49a34458 |
| BSAIintro(2).pdf | 2020 BSAI introduction | 54 | ea41a97aeb43332358c8c5c9544e607c1d057fa296128e1417992bcdede3f161 |
| GOAintro.pdf | 2024 GOA introduction | 42 | 7a8f279a0c7d561992486fbce4da686e1491ddd85c3f6f288c7ea6c639896249 |

`pdftotext -layout` extracted the documents for section comparisons. Page images
were produced with `pdftoppm` and visually inspected for stock summaries, regional
components and end tables in every file (BSAI 2024 pages 19/50, BSAI 2023 pages
18/50, BSAI 2020 pages 19/54, GOA 2024 pages 19/42); cover pages were also rendered.
The 2020 file allows copying/printing but restricts modification; its bytes were
not edited. All originals remain outside tracked app sources; any user-added
copies in the repository are preserved and are not staged or committed.

## Shared structure and differences

All are Letter-sized SAFE introductions with title/contributors, contents,
summary/background, assessment and harvest-rule context, economic/ecosystem
summaries, numbered stock status sections, and final tables. Each FMP has 20 stock
headings. Recurring stock subsections cover changes from the previous assessment,
spawning biomass and stock trends, tier/Plan Team recommendations, status and area
apportionment, with status/catch specifications before the prose.

BSAI separates EBS/AI/Bogoslof pollock narratives and EBS/AI Pacific cod narratives,
while sharing their stock-level tables. GOA includes its distinct flatfish,
rockfish and skate groups, with area tables in the stock sections. The 2024 BSAI
version contains a multi-species section; final summary, discard-mortality and
reduction tables vary by year. The longer 2020 BSAI version also contains extensive
historical catch tables. GOA includes forage/grenadier appendices and final tables.
The 2024 BSAI contents contain broken bookmark text; generated Quarto contents
avoid inheriting those links. The Word contents field needs updating in Word.

The references use serif body text, hierarchical stock headings, repeated running
headers/footers, grouped rows and some wide/merged specification tables. The first
app keeps the editable hierarchy, serif print text and native tables. It uses
readable table panels for wide data and does not promise identical pagination,
merged cells or running-header design. No historical numerical advice, status
claim or stock interpretation is inserted as current boilerplate. New documents
contain explicit TODO prompts and require source/year/author review.

## Additional local copies supplied later

The four root-level copies were checked separately after Jim placed them in the
repository: `BSAIintro.pdf`, `BSAIintro 2.pdf`, `GOAintro.pdf`, and `GOAintro 2.pdf`.
The first three match inspected attachment bytes (BSAI 2024, BSAI 2023 and GOA
2024). `GOAintro 2.pdf` is a different 39-page November 2023 Council draft, not the
2020 BSAI attachment. Its text, contents, stock page 18 and final-table page 39
were inspected. It retains the same 20-stock GOA hierarchy and includes a sculpin
appendix, so the app also offers an excluded-by-default historical sculpin
appendix. There are five unique inspected documents in total. The app treats all
historical appendices as optional source material, never current status advice.
All four root PDFs remain unmodified and Git-ignored; none is staged/committed.
