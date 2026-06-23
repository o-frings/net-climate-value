# Q Value Calibration from Chiti et al. (2026)

Source: Chiti et al. (2026) "A review of forest management practices potentially
suitable for carbon farming in European forests" J Environmental Management
398:128391, Table 1 and Figures 1-2.

## Units

**Chiti et al. Table 1 reports rates in Mg CO2 ha^-1 yr^-1** — confirmed
from the column headers ("AGBc rate: Mg CO2 ha−1yr−1", "SOCd rate: Mg CO2
ha−1yr−1"). No C-to-CO2 conversion (factor 3.67) is needed. Our code
uses the same units throughout.

## Formula

```
Q = net_additional_rate (MgCO2/ha/yr) × area (ha) × duration (yr)
```

Three quantities are multiplied:
1. **Rate**: from Chiti et al. Table 1 or derived from its source papers (net additional, not total)
2. **Area**: registry-grounded project area (11–500 ha), based on actual European scheme data
3. **Duration**: crediting/commitment period (10–50 yr), practice-specific

## Critical distinction: Total rate vs Net additional rate

Chiti et al. Table 1 reports **total** sequestration rates for each practice.
For carbon crediting, Q must represent **net additional** carbon relative to
a counterfactual baseline (what would happen without the practice):

| Practice type | Chiti reports | Our Q uses | Relationship |
|---|---|---|---|
| Afforestation | Total AGB+SOC rate | Total rate | Equal (baseline = no forest) |
| Extended rotation | Total AGB in extended stands | Difference vs standard rotation | Q rate << Chiti total rate |
| Reduced harvest | Total AGB in unthinned stands | Difference: control - thinned | Q rate << Chiti total rate |
| Set-aside | Total AGB in unmanaged stands | Difference: unmanaged - managed | Q rate << Chiti total rate |
| Site fertilisation | Additional AGB from fertilisation | Same (baseline = unfertilised) | Equal |
| Peatland rewetting | Avoided SOC emissions | Same (already net) | Equal |
| Species/structure | Total AGB of alternative species | Marginal gain over default | Q rate << Chiti total rate |
| Fuel management | Not in Table 1 | Probability-weighted avoided loss | Derived independently |

## Q invariance of NCV percentage results

Since `net_share = I_net / Q = (1-L) × omega × (1-b)`, **Q cancels completely
from the percentage NCV**. The Q values only affect:
- Absolute I_net (total credits issued, in tCO2)
- Cross-practice absolute comparisons

The paper's main results (43–92% overcrediting) are based on `net_share`
percentages and are invariant to Q.

## Project area grounding from scheme registries

Previous versions used arbitrary round-number areas (500–2000 ha). These are
now grounded in actual European scheme registry data:

| Registry | Country | Projects | Total area (ha) | Mean (ha) | Source |
|---|---|---|---|---|---|
| Woodland Carbon Code (WCC) | UK | 2,214 | 103,995 | ~47 | woodlandcarboncode.org.uk/statistics (Mar 2025) |
| Label Bas-Carbone (LBC) | France | ~1,015 forest | — | ~11 | I4CE (2024) "Six years of carbon certification in France" |
| Peatland Code | UK | 200 | 27,071 | ~135 | IUCN UK Peatland Programme (2025) |
| Wald-Klima-Standard (WKS) | Germany | early-stage | — | min 50–150 | waldklimastandard.de |
| Klimaskovfonden (KSF) | Denmark | small portfolio | — | ~20–50 | klimaskovfonden.dk |
| California USFP (CARB) | USA | ~74 IFM | ~1,600,000 | ~22,000 | CARB compliance database; Food & Water Watch (2024) |

**Key finding**: European forest carbon projects are typically 10–150 ha,
orders of magnitude smaller than US compliance forestry (~22,000 ha).

## Derivation by practice (current values)

### Harvest-reducing practices

| Practice | Chiti Table 1 values | Rate | Derivation | Area (ha) | Source | Duration | Q |
|---|---|---|---|---|---|---|---|
| Extended rotation | Peichl 2023: 4.21, Stokland 2021: 4.58 (total AGB, Boreal) | 3.5 | ~75% of total in deferral years (net vs harvesting now) | 100 | WKS-scale | 20 | 7,000 |
| Reduced harvest intensity | Unthinned 6–13, thinned 3–10 (Bravo-Oviedo 2015, Temperate) | 3.0 | Difference: control minus thinned | 100 | WKS/LBC-scale | 10 | 3,000 |
| Set-aside | Managed 6–13 total AGB; "unmanaged forests accumulate more" (Chiti text) | 4.5 | Synthesised unmanaged minus managed difference | 100 | Biodiversity set-aside | 10 | 4,500 |
| Continuous stock mgmt | Not in Chiti; LBC GFSC methodology | 0.45 | 15% of ~3.0 full growth rate (only 15% harvest reduction) | 11 | LBC mean forest project | 20 | 99 |
| Forested peatland rewet | Mander 2024: SOC 3.40 | 2.5 | Mander gross minus tree C loss minus CH4 offset | 135 | Peatland Code mean | 30 | 10,125 |

### Harvest-neutral practices

| Practice | Chiti Table 1 / Fig 2 values | Rate | Derivation | Area (ha) | Source | Duration | Q |
|---|---|---|---|---|---|---|---|
| Species diversification | Fig 2: total AGB ~3.5 | 1.0 | Marginal gain from species switch (most growth is baseline) | 150 | WKS conversion min | 50 | 7,500 |
| Structural diversification | CCF section: Hilmers 2020 | 1.5 | Net from multi-layered vs even-aged stocking | 100 | WKS-scale | 30 | 4,500 |
| Fuel management | Section 1.2.9 (fire management) | 4.0 | Probability-weighted avoided fire loss (risk x stock x severity) | 500 | Large-scale fuel break | 20 | 40,000 |
| Peatland rewetting | Mander 2024: 3.40; Wilson 2016: 0.26–5.38 | 3.4 | Mander global mean (already avoided emissions) | 135 | Peatland Code mean | 50 | 22,950 |

### Harvest-increasing practices

| Practice | Chiti Table 1 values | Rate | Derivation | Area (ha) | Source | Duration | Q |
|---|---|---|---|---|---|---|---|
| Woodland creation | Cukor 2022: 16–19, Petaja 2023: 6.2–6.4, Vacek 2022: 8.6–9.2 | 8.0 | Lifecycle average (young stands grow faster, mature slower) | 47 | WCC mean project | 30 | 11,280 |
| Site fertilisation | Ojanen 2019: AGB 4.76, SOC -0.48; Moilanen 3.4–5.5; Hanssen 2.5 | 4.0 | Ojanen net (4.76−0.48=4.28), rounded down | 100 | Finnish forestry unit | 10 | 4,000 |
| Agroforestry | Kay 2019 Q. suber: 3.00; Palma 2014: 3.00; SOC: 1.06–1.65 | 3.0 | Native cork oak silvopastoral (conservative) | 15 | LBC agroforestry | 30 | 1,350 |

## Site fertilisation: Ojanen vs Hanssen

Two values appear for site fertilisation in Chiti et al.:
- **Ojanen et al. (2019)**: AGB 4.76 − SOC 0.48 = **4.28 net MgCO2/ha/yr** (peatland ash fertilisation, Finland)
- **Hanssen et al. (2020)**: text (p.9) cites **2.5 MgCO2/ha/yr** additional C sink (Norway upland spruce)

We use 4.0 (rounded Ojanen), which is in the upper-mid range. Since NCV percentages
are Q-invariant, this choice only affects absolute I_net. Using Hanssen's 2.5 would give
Q = 2,500 instead of 4,000 — identical NCV%.

## Woodland creation rate: lifecycle average

Chiti Table 1 reports high rates from young stands (Cukor 2022: 16–19 MgCO2/ha/yr
at age 14; Petaja 2023: 6.2–6.4 at age 15). However, lifecycle-averaged rates
are substantially lower because:
- Early growth (years 0–5) is very slow
- Peak growth occurs age 10–20, then declines
- Vacek 2022 reports 8.6–9.2 at age 52 (cumulative, not peak)

We use 8.0 MgCO2/ha/yr as a 30-year lifecycle average for native broadleaves,
which is conservative relative to peak but accounts for slow establishment.

## CRCF EU-wide deployment scenarios

The CRCF_EU_SCENARIOS table in 03_parameters.R scales from individual project
NCV to aggregate EU-wide climate impact under the Carbon Removals Certification
Framework (Regulation EU 2024/3012).

### EU context
- **LULUCF target**: 310 MtCO2e net removals by 2030 (current: ~198 MtCO2e)
- **Shortfall**: ~100 MtCO2e that carbon farming is designed to help close
- **EU forest area**: ~160M ha (Forest Europe 2020)
- **3 Billion Trees pledge**: ~2M ha additional afforestation by 2030
- **Biodiversity Strategy**: 10% strict protection → ~16M ha forest
- **EU drained peatlands**: ~25M ha; rewetting target 30% by 2030 (~7.5M ha)
- **EU managed forest**: ~100M ha eligible for IFM practices

### EU annual credit potential by practice

| Practice | Rate | EU area (ha) | EU annual (MtCO2/yr) | Source |
|---|---|---|---|---|
| Extended rotation | 3.5 | 1,500,000 | 5.3 | 1.5% of ~100M ha managed forest |
| Reduced harvest intensity | 3.0 | 1,000,000 | 3.0 | 1% of managed forest |
| Set-aside | 4.5 | 16,000,000 | 72.0 | EU Biodiversity Strategy 10% strict protection |
| Continuous stock mgmt | 0.45 | 500,000 | 0.2 | LBC GFSC + similar EU schemes |
| Forested peatland rewetting | 2.5 | 2,000,000 | 5.0 | Subset of 7.5M ha EU rewetting target |
| Species diversification | 1.0 | 5,000,000 | 5.0 | 5% of managed forest (Waldumbau-type) |
| Structural diversification | 1.5 | 3,000,000 | 4.5 | 3% of managed forest (CCF conversion) |
| Fuel management | 4.0 | 5,000,000 | 20.0 | ~25% of Mediterranean forest (~20M ha) |
| Peatland rewetting | 3.4 | 5,000,000 | 17.0 | 30% of ~25M ha EU drained peatland |
| Woodland creation | 8.0 | 2,000,000 | 16.0 | EU 3 Billion Trees pledge: ~2M ha |
| Site fertilisation | 4.0 | 500,000 | 2.0 | Nordic drained peatland forestry |
| Agroforestry | 3.0 | 1,000,000 | 3.0 | EU agroforestry expansion target |
| **Total** | | | **~153** | |

### Policy application

The phantom credit volume at EU scale is:
```
phantom_MtCO2/yr = EU_annual × (1 - net_share)
```

For example, if set-aside credits have NCV = 9.1%, then 90.9% of 72.0 MtCO2/yr
= **65.4 MtCO2/yr** of phantom credits — enough to materially undermine the
LULUCF shortfall target.

## Key changes from previous calibration

| Practice | Previous Q | Current Q | Change | Main driver |
|---|---|---|---|---|
| Extended rotation | 70,000 | 7,000 | -90% | Area: 1000→100 ha (registry-grounded) |
| Reduced harvest intensity | 30,000 | 3,000 | -90% | Area: 1000→100 ha |
| Set-aside | 45,000 | 4,500 | -90% | Area: 1000→100 ha |
| Continuous stock mgmt | 9,000 | 99 | -99% | Area: 1000→11 ha (LBC mean) |
| Forested peatland rewet | 38,000 | 10,125 | -73% | Area: 500→135 ha (Peatland Code) |
| Species diversification | 25,000 | 7,500 | -70% | Area: 500→150 ha |
| Structural diversification | 22,000 | 4,500 | -80% | Area: 500→100 ha |
| Fuel management | 60,000 | 40,000 | -33% | Area: 2000→500 ha |
| Peatland rewetting | 170,000 | 22,950 | -86% | Area: 1000→135 ha |
| Woodland creation | 120,000 | 11,280 | -91% | Area: 500→47 ha (WCC mean) + rate 12→8 |
| Site fertilisation | 20,000 | 4,000 | -80% | Area: 500→100 ha |
| Agroforestry | 45,000 | 1,350 | -97% | Area: 500→15 ha (LBC agroforestry) |

All Q values decreased substantially because European projects are much smaller
than previously assumed. Since **net_share is Q-invariant**, the paper's main
NCV percentage results are completely unaffected by these changes. Only absolute
I_net values (total credits per project) changed.
