# Empirical Calibration Summary

## Overview

The R analysis framework has been calibrated using empirical data from peer-reviewed literature, specifically:

1. **Senf & Seidl (2021)** Nature Sustainability - European disturbance regimes
2. **Kallio & Solberg (2018)** Scand J For Research - Leakage from Norway
3. **Murray et al. (2004)** Land Economics - Leakage estimation framework
4. **Chiti et al. (2026)** J Env Management - Carbon sequestration rates
5. **Groom & Venmans (2023)** Nature - Social value of offsets

---

## Key Empirical Values

### 1. Disturbance Parameters (Senf & Seidl 2021)

| Parameter | Value | Notes |
|-----------|-------|-------|
| Total area disturbed 1986-2016 | 17% | 39M ha of 210M ha European forest |
| Mean patch size | 1.09 ha | Median 0.45 ha, 78% <1 ha |
| Mean disturbance frequency | 0.52 patches/km²/yr | Range: 0.02–31 patches/km² |
| Mean severity | 0.77 | Probability of stand-replacing |
| Frequency increasing (% of area) | 74% | Most forests show increasing trends |
| Severity decreasing (% of area) | 88% | Counter to expectation |

**Regional rates (λ_obs):** the figures above are Senf & Seidl (2021) TOTAL
canopy-mortality rates (harvest-inclusive). The model's `lambda_obs` is the
NATURAL-only disturbance rate (harvest excluded, contractually managed) and is
now calibrated from the European Forest Disturbance Atlas v2.1.1 (Viana-Soto &
Senf 2025), forest-area-weighted per biome (`02b_efda_biome_timeseries.R`):
- Boreal: 0.00096 annual (harvest-dominated; natural disturbance very low)
- Temperate: 0.00239 annual
- Temperate_UK: 0.00310 annual
- Mediterranean: 0.00443 annual

### 2. Leakage Parameters

#### Murray et al. (2004) - US Natural Experiment

| Metric | Value | Context |
|--------|-------|---------|
| PNW federal restrictions | **84%** | N American continental scale |
| PNW Western US only | 43% | Regional scale |
| US total | 58% | Including South response |
| Formula prediction | 87% | Using ε_s=0.46, ε_d=-0.06, f=0.045 |

**Afforestation leakage:**
- Pure afforestation: 7–17%
- Avoided deforestation: Lower (penalises conversions)

#### Kallio & Solberg (2018) - Norway

| Product | Scenario Low | Scenario High |
|---------|-------------|---------------|
| Sawlogs | 60–70% | 85–95% |
| Pulpwood | 80–90% | 70–100% |
| **Total roundwood** | **73–84%** | **75–84%** |

**Key insight:** "60–100% of harvest change offset by opposite change elsewhere"

#### Chiti et al. (2026) - Literature Synthesis

> "Leakage issues...60–100% range across contexts and product categories" (citing Murray et al. 2004; Gan & McCarl 2007; Kallio & Solberg 2018; Meyfroidt et al. 2020)

### 3. Carbon Sequestration Rates (Chiti et al. 2026)

#### Afforestation (Mg CO₂ ha⁻¹ yr⁻¹)

| Region | AGB (Young) | AGB (Mature) | SOC |
|--------|------------|--------------|-----|
| Boreal | 5.0 | 15.0 | 3.0 |
| Temperate | 17.0 | 20.0 | 2.0 |
| Mediterranean (slow) | 8.0 | — | 1.0 |
| Mediterranean (fast) | 30.0 | — | 1.0 |

*Note: "Fast" = non-native species (Eucalyptus, P. radiata) - not recommended for CF due to fire risk and biodiversity concerns*

#### Other Practices (Mediterranean, Fig 2)

| Practice | AGB (Mg CO₂/ha/yr) | Notes |
|----------|-------------------|-------|
| Agroforestry | 12–14 | Highest in Mediterranean |
| Coppice conversion | 8 | Turkey oak systems |
| Longer rotation | 5–8 | Extension by 20–40 years |
| Species selection | 3.5 | Climate-adapted mix |

### 4. Offset Valuation (Groom & Venmans 2023)

| Parameter | Value | Notes |
|-----------|-------|-------|
| Discount rate (r) | 3.2% | Central case |
| SCC growth rate (x) | <r always | Shown analytically |
| 50-yr equivalence (low risk) | 44% | RCP2.6, φ=0 |
| 50-yr equivalence (med risk) | 39% | RCP2.6, φ=0.005/yr |
| 50-yr equivalence (high risk) | 34% | RCP2.6, φ=0.01/yr |

**Rule of thumb:** 2–3 temporary offsets (50 yr) ≈ 1 permanent ton

---

## Model Implementation

### Leakage Formula

Following Murray et al. (2004) Equation 7:

```
L = (ε_s × γ × C_N/C_R) / (ε_s - ε_d × (1 + γ×f))
```

Where:
- ε_s = supply elasticity (0.40–0.50 domestic, 1.0 imports)
- ε_d = demand elasticity (-0.25, inelastic)
- γ = product substitutability (0.85–0.95)
- f = preservation parameter (share of market affected)
- C_N/C_R = carbon density ratio

### SVO Weight (ω)

Following Groom & Venmans (2023):

```
ω = [1 - exp(-(k₀ + φ_add)(τ₂ - τ₁))] / [1 - exp(-k₀ × H_ref)]
```

Where:
- k₀ = r - g (discount rate minus SCC growth)
- φ_add = additionality risk hazard rate
- τ₁, τ₂ = project start/end
- H_ref = reference horizon (100 years)

### Buffer Calibration

Using Senf & Seidl (2021) disturbance rates:

```
b = 1 - exp(-φ × H)
φ = γ × (1 + c) × λ_H × R^α × (S̄/S_ref)^β
```

Where λ_H = λ_obs × (1 + U_H) incorporates climate uncertainty

---

## Validation

The model was validated against:

1. **Murray et al. (2004):** Model predicts 70–85% leakage for temperate roundwood, consistent with their 84% PNW estimate

2. **Kallio & Solberg (2018):** Model ρ_rep ≈ 0.70 aligns with their 73–84% range for Norway

3. **Chiti et al. (2026):** Carbon sequestration rates used directly from their Figures 1–2

---

## Data Files

- `data/external/literature_parameters.csv` - All extracted values with sources
- `R/03_parameters.R` - Calibrated parameter values
- `R/04_functions.R` - Empirical formulas (Murray, Kallio-Solberg, Chiti, Groom-Venmans)

---

## Scheme Comparison Methodology

### How Existing Schemes Calculate Credits

Existing certification schemes use simple fixed deductions from their official documentation:

```
I_net = Q × (1 - leakage_rate) × (1 - buffer_rate)
```

| Scheme | Buffer | Leakage | Notes |
|--------|--------|---------|-------|
| Label Bas-Carbone (FR) | 10% | 5% | Risk discount approach |
| Woodland Carbon Code (UK) | 20% | 0% | Pooled buffer, disclosure only |
| Wald-Klima-Standard (DE) | 15% | 0% | Acknowledged but not quantified |
| Klimaskovfonden (DK) | 15% | 0% | National boundary assumption |
| California USFP (US) | 13.4% | 20% | Risk-rated pool |

### The Integrity Gap

The "integrity gap" = I_net(existing) - I_net(proposed), showing how much existing schemes **over-credit** compared to the proposed framework. Key differences:

1. **Temporality (ω)**: Existing schemes don't discount for temporary storage. The proposed framework applies the Groom & Venmans (2023) SVO weight (~40% for 50-year storage).
2. **Leakage (L)**: Existing schemes use 0–20% fixed rates vs empirical 60–100% for harvest reduction.
3. **Buffer (b)**: Existing schemes use fixed 10–20% vs risk-adjusted buffers from Senf & Seidl (2021).

See `R/09_scheme_comparison.R` for implementation.

---

## References

Chiti, T., et al. (2026). A review of forest management practices potentially suitable for carbon farming in European forests. *Journal of Environmental Management*, 398, 128391.

Groom, B. & Venmans, F. (2023). The social value of offsets. *Nature*, 619, 768–773.

Kallio, A.M.I. & Solberg, B. (2018). Leakage of forest harvest changes in a small open economy: case Norway. *Scandinavian Journal of Forest Research*, 33(5), 502–510.

Murray, B.C., McCarl, B.A. & Lee, H.-C. (2004). Estimating leakage from forest carbon sequestration programs. *Land Economics*, 80(1), 109–124.

Senf, C. & Seidl, R. (2021). Mapping the forest disturbance regimes of Europe. *Nature Sustainability*, 4, 63–70.
