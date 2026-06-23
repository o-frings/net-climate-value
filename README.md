# The Net Climate Value of Carbon Credits — R analysis

Replication code for:
**"The Net Climate Value of Carbon Credits: Pricing Leakage, Time, and Reversal"**
Frings et al. (2026)

The analysis runs in two stages from the cached inputs in `data/processed/`:

1. **`engine/`** computes every number in the manuscript. `engine/R/run_engine.R`
   sources 14 modules in order (`01_data` → `14_href_sensitivity`) and writes all
   results to `engine/output/`. It reads only `engine/params/`, `engine/output/`,
   and `data/processed/` — it does not depend on the scripts in `prep/` at runtime.
2. **`figures/`** renders the figures and tables from `engine/output/`.
   `figures/run_figures.R` writes PDFs/PNGs to `figures/output/` and `.tex` tables
   to `figures/output/tables/`; `figures/sync_to_manuscript.sh` copies them into
   `../manuscript_overleaf_clone/`.

## The model

Net Climate Value is a multiplicative discount on a verified tonne, conditionally
independent across three channels and invariant to the gross quantity `Q`:

```
NCV_share = (1 - L) * (1 - T) * (1 - b)
```

- **L — leakage.** Linearised partial-equilibrium market leakage from an Armington
  representative-elasticity ratio, scaled by the harvest-displacement fraction `x`
  and the leakage-intensity scalar `kappa` (MC-integrated over kappa = 0.33–1.00).
  Negative for supply-positive practices, capped at -0.20.
- **T — temporality.** Deduction relative to true permanence (`H_ref = Inf`):
  `T = 1 - exp(-k0 * tau2)` with `k0 = r - g = 0.01` at the central discount rate
  (a 100-yr horizon gives `T = 1 - e^-1 ≈ 0.37`, so 63% is retained).
- **b — buffer (reversal).** The TVaR99 (expected shortfall) of an empirical
  bootstrap of the 1986–2023 EFDA per-country natural-disturbance series, evaluated
  at the pool's **correlation-limited** effective scale (`N_eff = round(1/c)`
  decorrelated cells, mean-preserving Beta cell draws, tail saturation). The buffer
  prices mature-stand disturbances (fire, wind, insect) only; establishment risk is
  handled through ex-post verification of realised `Q`, not the buffer. See
  `docs/buffer_correlation_note.md`.

Monte Carlo runs at `n = 10,000`; hazard-parameter uncertainty (the disturbance-rate
multiplier `lambda_mult` and climate uplift `U_mult`) propagates into the buffer, so
each channel has one dominant driver. The manuscript reports MC medians.

## Quick start

```bash
cd analysis

# Restore the pinned package environment (first run only)
Rscript renv_bootstrap.R

# 1. Compute results (~10,000 MC iterations)
Rscript engine/R/run_engine.R

# 2. Render figures + tables, then sync into the manuscript
Rscript figures/run_figures.R
bash figures/sync_to_manuscript.sh
```

Re-running the engine yields byte-identical `engine/output/` (every module is
seeded). Set `ENGINE_MC_ITER=1000` for a quick smoke run.

## Layout

| Path | Contents |
|------|----------|
| `engine/R/` | The 14 analysis modules + `run_engine.R` (the order is authoritative). |
| `engine/params/` | The hand-audited parameter CSVs the engine reads (source of truth). Sourcing in `PARAM_SOURCING_AUDIT.md`. |
| `engine/output/` | All computed results (CSV/RDS). |
| `engine/build_scheme_csvs.R` | Rebuilds the scheme parameter CSVs (`schemes`, `scheme_coverage`, `href_values`) from `prep/03_parameters.R`. |
| `figures/` | `theme.R`, `fig3.R`, `fig_main.R` (fig4/5), `fig_ed.R`, `tables.R`, `run_figures.R`, `sync_to_manuscript.sh`. |
| `prep/` | Upstream layer kept for reproducibility — raw-data extraction and the legacy parameter/function source the build step reuses: `01_setup`, `02_data_extraction`, `02b_efda_biome_timeseries`, `03_parameters`, `04_functions`, `20_haya_redd_leakage`. (The engine's own modules live in `engine/R/`.) |
| `data/`, `data-raw/` | Cached processed inputs and raw extraction sources. |
| `docs/` | Provenance and calibration notes. |
| `scripts/` | EFDA offline extraction toolchain. |
| *(superseded pipeline)* | Not included — the clean-room reimplementation the engine was promoted from; available on request. |

## Reproducibility

- **Environment** — packages pinned in `renv.lock` (R 4.3.1); `renv_bootstrap.R`
  hydrates the library from the lockfile.
- **Data re-extraction** — `prep/02_data_extraction.R` (and `02b`) regenerate
  `data/processed/` from the raw sources below; needs `sf`, `rnaturalearth`,
  `rnaturalearthdata`. The committed extracts let the engine run without them.
- **Parameter derivation** — `engine/build_scheme_csvs.R` rebuilds the scheme parameter
  CSVs from `prep/03_parameters.R`. The committed `engine/params/*.csv` are the audited
  final values.
- **Clean room** — the engine was promoted from an independent reimplementation
  (`R/99_compare.R` diffed it against the old pipeline); the superseded pipeline is
  not included in this repository.
- **Provenance** — input sources and redistribution status in `docs/data_provenance.md`.

## Data sources

**Included** (`data/processed/`, `data/external/`): the cached extracts needed to run
the engine.

**Required for re-extraction or raw-data audit** (see `docs/data_provenance.md`):

| Source | Use |
|--------|-----|
| European Forest Disturbance Atlas (Viana-Soto & Senf 2025) | Per-country natural-disturbance rates λ, severity |
| Grünig et al. (2026), *Science* — Dryad `10.5061/dryad.tb2rbp0dv` | RCP4.5/8.5 climate uplift factors `U_50` |
| Senf & Seidl (2021), *Nat. Sustain.* — Zenodo `10.5281/zenodo.3924381` | Disturbance severity by biome |
| UNECE / Forest Europe (FRA 2020) | Reference above-ground stock `S_ref` |
| Marinelli et al. (2026) JRC CRCF risk model (`crcf_risk_bp_maps.gpkg`) | JRC per-country buffer cross-check |
| Chiti et al. (2026), *J. Environ. Manage.* | Sequestration rates `Q` (Q-invariant for NCV) |

## Citation

```bibtex
@article{frings2026ncv,
  title   = {The Net Climate Value of Carbon Credits: Pricing Leakage, Time, and Reversal},
  author  = {Frings, Oliver and Abildtrup, Jens and Delacote, Philippe and
             Kontoleon, Andreas and B{\"o}ttcher, Hannes and Chiti, Tommaso and
             Rey, Ana and Diaci, Jurij and Lehtonen, Aleksi and Puelzl, Helga and
             Schindlbacher, Andreas and Zavala, Miguel A.},
  year    = {2026},
  note    = {Working paper. Submitted for peer review.}
}
```
