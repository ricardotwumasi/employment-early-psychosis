# Review-record files

Working evidence for Phase 3 of `NEXT_VERSION_PLAN_v1.1.md`. These files record what the source documents say; they do not repair or reconcile anything. Values were extracted programmatically on 2 September 2026 (PDF first pages with PyMuPDF; Word tables with python-docx and pandoc) and checked by the statistical and release lead.

| File | Content | Status |
|---|---|---|
| `source_registry.csv` | One row per PDF in the workspace `literature/` folder (29 rows): print year, journal, DOI, role in the analysis, and online-first versus print-year notes. `pmid_or_pmcid` is `not_found` throughout because no PubMed identifier appears in the PDF text | To be extended to one row per included report (45 studies) as PDFs or DOIs are confirmed, and to one row per extracted result with page or table sources |
| `prisma_flow_comparison.csv` | The two dissertations' PRISMA flows side by side with an arithmetic check at each stage | Awaiting the authoritative screening export from HK and YL; no count has been repaired |
| `ephpp_two_source_check.md` | Programmatic comparison of the EPHPP component tables in the two dissertations (43 quantitative studies, zero differences), the global-rating rule check, and the absence of the Blinding component and JBI items 6 and 7 from both | Blinding and JBI items 6 and 7 requested from HK |

Conventions: `study_key` is the lower-case first-author surname plus the print year, matching `manuscript/sr_data/included_studies_45.csv`; the print year with the DOI is the citable identity, so Dudley is 2014, Hegelstad (Job- and schoolprescription) is 2019, Tapfumaneyi is 2015, Erickson is 2021 and Bond (IPS for young adults) is 2016, whatever year the file name carries. The derived analysis tables in `data/derived/` use upper-case identifiers with the same print years.
