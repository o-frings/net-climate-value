# Literature Acquisition Plan — Rate Provenance Gaps

Goal: Ensure every row in `ALL_PROJECTS` has a traceable, PDF-anchored primary source for its rate value.

## Inventory summary (34 citations checked)

- **29 primary sources already in `reference_articles/`** (85% coverage)
- **5 primary sources to download** (15% gap)
- **3 "PRELIMINARY" rows need a scope decision** (Reforestation)
- **1 misattribution to fix** (Species diversification Mediterranean)

Tracking CSV: [output/tables/literature_gaps.csv](../output/tables/literature_gaps.csv)

---

## Stage 1 — Fixes requiring no downloads

These can be done immediately with files already in `reference_articles/`.

### 1.1 Species diversification Mediterranean (MISATTRIBUTED, currently 14.0)

**Problem:** Current code cites "Coletta 2016 10.6-17.3" — but Coletta 2016 is in Chiti Table 1 under *Harvest Intensity* (Pseudotsuga thinning), not species diversification.

**Fix:** Revise to one of:
- **Option A:** 3.5 MgCO2/ha/yr (harmonised with Boreal/Temp Species diversification values) — conservative
- **Option B:** source from a primary Mediterranean mixed-species study (none obvious in current folder)

Recommendation: Option A plus code comment citing harmonisation rationale.

### 1.2 Reforestation (3 rows, PRELIMINARY)

**Problem:** Values (0.5 / 0.8 / 0.5) derived from "~10% of afforestation rate" heuristic with no primary source.

**Decision needed:**
- **Option A (scope narrow):** drop Reforestation from ALL_PROJECTS; LBC Reconstitution credits post-disturbance restocking, but this is arguably covered by Productive afforestation with a discount
- **Option B (keep):** find primary post-disturbance restocking carbon studies (Gauthier et al. on Boreal salvage; Spanish post-fire regeneration studies)
- **Option C (retain heuristic):** acknowledge in manuscript that Reforestation uses a fraction of the afforestation rate, document the fraction, flag as sensitivity variable

### 1.3 Validate extractable values from in-folder PDFs (8 rows)

For each row below, open the PDF, find the specific table/figure with the rate, and log:

| Row | PDF | What to find |
|---|---|---|
| Reduced harvest intensity Boreal/Temp | Pretzsch & Hilmers 2023 | Temperate Picea thinning intensity × AGB differential (verify 1.0 MgCO2 range) |
| Set-aside Temperate | Chiti et al. 2026 | "Management vs no-intervention" Temperate (n=5) supplementary aggregate |
| Continuous stock management Boreal | Eyvindson et al. 2021 | Finnish CCF vs BAU differential over rotation |
| Continuous stock management Temperate | Hilmers et al. 2020 | Net additional C from Norway-spruce-to-mixed transformation |
| Structural diversification Boreal | Mohr et al. 2024 | Austrian CCF productivity vs even-aged |
| Structural diversification Temperate | Hilmers et al. 2020 | Bavarian mountain transformation scenarios |
| Structural diversification Mediterranean | Pretzsch & Hilmers 2023 | Structural diversity × C stock trade-off |
| Fuel management Temperate/Med | Davis 2024 + Fernandes 2013/2015 | Expected avoided-severity × expected fire loss computation |
| Coppice conversion Temp/Med | Campani 2022 + Lee 2018 | AGB+SOC accretion rates post-coppice-abandonment |
| Agroforestry Temperate | Kay et al. 2019 | Temperate subset (UK/DE/FR) silvoarable rates |
| Peatland rewetting Temp | Tiemeyer 2020 + Ojanen & Minkkinen 2020 | Forestry-drained Temperate peatland net GHG post-rewetting |

---

## Stage 2 — Downloads needed (5 papers)

| Citation | Why needed | Search strategy |
|---|---|---|
| **Mander et al. (2024)** | Cited as source for Peatland rewetting global mean (3.4 MgCO2/ha/yr). Currently a fabricated citation — needs verification. | Google Scholar: "Mander 2024 peatland rewetting greenhouse gas global mean". Alternative: Mander Ü, Maddison M, Soosaar K et al., likely in Mitigation and Adaptation Strategies or similar. **Critical.** |
| **Hynynen et al. (2005)** — Silva Fennica 39(3) | Boreal thinning intensity × AGB differential (supports Reduced harvest intensity Boreal = 1.0) | DOI search: 10.14214/sf.413 or similar; Silva Fennica is open access |
| **Pretzsch et al. (2020)** — *Trees* 34:957-970 | Mixed-species AGB uplift meta-analysis; supports Species diversification Boreal/Temp | DOI: 10.1007/s00468-020-01964-1 |
| **Cardinael et al. (2017)** | French silvoarable walnut C sequestration; Agroforestry Temperate | Cardinael R, Chevallier T, Cambou A et al., Geoderma or Plant Soil |
| **Palma et al. (2014)** — *Agroforestry Systems* | Cork oak stand C fluxes; Set-aside Mediterranean + Agroforestry validation | DOI search: Palma 2014 cork oak carbon |

Lower-priority (for additional cross-checks):
- Nilsen & Strand (2008) — Boreal thinning
- Specific studies from Chiti's Table 1 references for validation (Wellock 2014, Tupek 2021, Cukor 2022, etc.) — these are already aggregated in our CSV, individual PDFs only needed for deep QA

---

## Stage 3 — PDF annotation procedure

Since I cannot annotate PDFs programmatically, here is the **manual procedure**:

### For each extraction:

1. **Open the PDF** in Preview (macOS) or Adobe Acrobat
2. **Navigate to the relevant table/figure** (page numbers logged in `literature_gaps.csv`)
3. **Highlight the specific value** you extracted (cell in table, data point in figure)
4. **Add a sticky note** with the text:
   ```
   Carbon farming MS: ALL_PROJECTS row <practice> × <biome>
   Extracted value: <X.X> MgCO2/ha/yr
   Used for: <row in rate_provenance.csv>
   Date: YYYY-MM-DD
   ```
5. **Save the PDF** with `_annotated` suffix in filename OR save into `reference_articles/annotated/`

### Tracking the extraction

For each annotated PDF, append a row to `output/tables/literature_gaps.csv`:
- Set `status = "extracted"`
- Add `pdf_page` column with the specific page number
- Add `extracted_value` column with the numeric value found
- Add `annotated_pdf_path` if saved separately

### Directory convention

```
reference_articles/
├── [original PDFs]              ← untouched
└── annotated/                   ← NEW: create if useful
    ├── Bravo-Oviedo_2015_annotated.pdf
    ├── Campani_2022_annotated.pdf
    └── ...
```

---

## Stage 4 — Integration back to code

After extractions are verified:

1. Update inline comments in `R/03_parameters.R` → `ALL_PROJECTS` tribble with specific values + page references
2. Update `output/tables/rate_provenance.csv` `derivation` column with verified page/table references
3. Remove `flag` entries for resolved rows
4. Update the manuscript supplementary table with the provenance CSV

---

## Suggested order of work

1. **Immediate (no downloads):** fix Species diversification Med misattribution + decide on Reforestation scope (Stage 1.1 and 1.2)
2. **Short (in-folder extractions):** open the 8 PDFs listed in Stage 1.3 and annotate the specific values — ~2-3 hours for someone familiar with the papers
3. **Medium (downloads):** acquire the 5 gap papers (Stage 2), especially Mander 2024 which is currently a load-bearing but unverified citation
4. **Final:** integrate extracted values back into code comments and provenance CSV (Stage 4)
