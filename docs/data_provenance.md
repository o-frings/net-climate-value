# Data provenance & availability

Draft for the Nature Climate Change Data Availability Statement and the
reproducibility package. One row per input the pipeline consumes. **Author must
verify the `Version` and `Accessed` columns against the actual downloads before
submission** (marked `TODO` where not recorded in the repo).

Redistributability key: **C** = committed in this repo (redistributable);
**D** = manual download required (public, not redistributed here);
**R** = restricted / not redistributable (cite source, do not deposit).

| Input (file) | Provides | Source | Version | Accessed | DOI / URL | Redist. |
|---|---|---|---|---|---|---|
| `data/processed/efda_country_summary.rds`, `efda_country_rates/*.rds`, `efda_biome_timeseries.rds` | Per-country & biome natural-disturbance rate λ (harvest excluded), severity, forest area | European Forest Disturbance Atlas (EFDA), Viana-Soto & Senf 2025 | v2.1.1 `TODO confirm` | `TODO` | `TODO Zenodo DOI` | C (derived) / D (raw rasters) |
| `data/processed/gruenig_uplift_factors.rds`, `gruenig_country_uplift_factors.rds` | RCP4.5/8.5 climate uplift U by biome/country | Grünig et al. 2026, *Science* 391:eadx6329 | `TODO` | `TODO` | Dryad 10.5061/dryad.tb2rbp0dv | C (derived) / D (raw) |
| `data/processed/senf_biome_rates.rds` | Disturbance severity by biome | Senf & Seidl 2021, *Nat. Sustain.* 4:63–70 | `TODO` | `TODO` | Zenodo 10.5281/zenodo.3924381 | C (derived) / D (raw) |
| `data/processed/sref_biome.rds` | Reference above-ground stock S_ref by biome | UNECE / Forest Europe, SDG 15.2.1(a); FAO FRA 2020 | 2020 | `TODO` | unece.org; fao.org/faostat (#data/FO) | C (derived) / D (raw) |
| `data/Chiti_et_al_2026_Table1.csv` | Sequestration rates Q by practice (Q-invariant for NCV) | Chiti et al. 2026, *J. Environ. Manage.* 398:128391 | Table 1 (transcribed) | n/a | journal DOI `TODO` | C |
| `data/JRC-risk-model/crcf_risk_bp_maps.gpkg` | JRC per-hexagon buffer benchmark; France/Italy bioregion split | Marinelli et al. 2026 (JRC CRCF risk model) | `TODO` | `TODO` | JRC `TODO` | **R** (deposit at this path or set `JRC_GPKG`; see extract_country_split.R) |
| `data/VROD-v2025-12.xlsx` | California USFP issuance/reversal stats (comparator only) | Voluntary Registry Offsets Database, Haya 2025 | v2025-12 | `TODO` | berkeleycarbontradingproject.org | D |
| `data/raw/wcc_registry_projects.csv`, `wcc_forest_research_2025.ods` | WCC scheme portfolio mix | Woodland Carbon Code registry / Forest Research 2025 | 2025 | `TODO` | woodlandcarboncode.org.uk | D |
| `data/external/scheme_parameters.csv` | Scheme buffer/leakage/liability params (audit trail) | Official scheme documentation (LBC, WCC, PLC, WKS, KSF, CARB) | as cited per row | per row | per `source` column | C |
| `data/external/literature_parameters.csv` | **Reference only — NOT loaded by the pipeline** (model params are in `R/03_parameters.R`) | mixed literature | n/a | n/a | n/a | C (reference) |
| `data/country_biome_map.csv` | country → biome + filename/alias mapping | this study (compiled) | n/a | n/a | n/a | C |

## Notes
- The pipeline runs end-to-end from the **committed `data/processed/*.rds`** with
  `REEXTRACT_DATA = FALSE` (the canonical setting): a third party reproduces every
  number without the raw rasters, the JRC gpkg, or `sf`/`rnaturalearth`.
- Re-extraction (`REEXTRACT_DATA = TRUE`, plus the offline `scripts/efda_offline/`)
  additionally requires the **D**/**R** raw inputs above and is needed only to
  regenerate the cached extracts.
- Environment: pin with `analysis/renv_bootstrap.R` → committed `renv.lock`;
  per-run versions are written to `output/sessionInfo.txt`. Current snapshot in
  `analysis/r_environment_snapshot.txt` (R 4.3.1).
- Establishment-floor citations (`bib.bib`, keys `Banin_2023`,
  `MedDrylandReforestation_2021`, `FinnishAfforestation_2008`,
  `ScotsPineRegen_2021`, `DeerBrowsing_2020`) are flagged `VERIFY` — confirm
  author/title against the full PDFs (DOIs/PIIs are the verified anchors).
