# Buffer calibration: the diversification assumption, and reconciliation with JRC

*Diagnostic note, 2026-06-08. Prototype only — no headline outputs or manuscript
text changed yet. Decision requested: whether to re-anchor the buffer methodology.*

## TL;DR

Two surprises — fig4 sitting entirely under the flat 20% benchmark, and our
per-country buffer rates running ~2× below the JRC/Marinelli (2026) model — trace
to **one assumption**: the headline empirical buffer is bootstrapped from the
*country-aggregate* disturbance series, i.e. it prices a buffer pool with **infinite
spatial diversification** (effective pool size N_eff → ∞). Spatial correlation
physically caps diversification at N_eff ≈ 1/c. Pricing that (using the c the model
already carries, Anderegg et al. 2020) reconciles us with JRC:

| | mean rate | median \|b − JRC mean\| | in JRC P10–P90 |
|---|---|---|---|
| current headline | 17.0% | 17.0 pp | 47% |
| correlation-aware | 33.7% | 5.8 pp | 89% |
| JRC | 33.3% | — | — |

The change is **not** a bug fix — the current code computes what it intends. It is a
modelling choice: credit infinite diversification, or only the diversification that
correlation permits. JRC takes the latter; we currently take the former.

## Why the buffer is low (mechanism)

The headline (`R/26_empirical_buffer.R`) bootstraps the country annual *natural*
disturbance series and sums it: `cum_loss = Σ λ_t · E_Z · R`. Because λ_t is a
country average, every project's idiosyncratic "did my stand burn" variance is
already averaged away; only the smoothed temporal variance remains. Re-running the
*same* bootstrap as a pool of N stands (each hit Bernoulli-style with a severity
draw) shows the buffer rise smoothly as diversification falls, passing through JRC at
N ≈ 5 ≈ 1/c (c ≈ 0.2). N = ∞ reproduces the current headline exactly (sanity
check: correlation 0.997).

Separately, **fig4 uses the *parametric* premium** `calc_buffer_rate`
(`b = (1+θ)·λ·E_Z/τ`, θ ≈ 0.02), which prices the **mean** loss plus a tiny loading.
This is structurally ~half the empirical TVaR₉₉ headline (the paper already notes
"parametric runs 30–50% below empirical"). The June λ-recalibration to EFDA-natural
(harvest-excluded) rates then lowered it further, pushing every scenario under 20%.

## Reconciliation (prototype, behind flags in R/26)

`BUFFER_SPATIAL_CORR` (default FALSE; FALSE reproduces the headline exactly) replaces
the summed aggregate with a pool over N_eff = round(1/c) cells, each year's cell rate
~ Beta(mean = λ_t, intra-cluster correlation c), mean-preserving and bounded. Two
further fixes address the residuals:

- **High-λ saturation** — surviving stock compounds (`1 − Π(1 − loss_t)`) instead of
  summing, so Mediterranean conifer no longer runs to 100%+.
- **Establishment-risk floor** `BUFFER_EST_FLOOR` (~0.10, compound) for
  afforestation/reforestation only — young-stand mortality (frost, drought, browsing)
  is independent of mature-forest λ and invisible to EFDA. **Gate to afforestation in
  the full pipeline; it must NOT apply to existing-forest management practices.**

Effect on JRC agreement (62 country × forest-type cells):

| variant | mean | median gap | in band | below JRC | r(residual, λ) |
|---|---|---|---|---|---|
| current headline | 17.0% | 17.0 pp | 47% | 97% | +0.66 |
| + correlation + saturation | 28.9% | 6.8 pp | 92% | 71% | +0.47 |
| + establishment floor (0.10) | 36.0% | 6.6 pp | 90% | 35% | +0.36 |

Headline biome broadleaf means move: Boreal 4.9→21%, Temperate 10.5→31%,
Mediterranean 21→44%. A floor of ~0.07–0.08 centres the mean on JRC (0.10 is mildly
conservative).

## Residuals (what pooling does NOT fix)

The residual correlates with **λ level (r = +0.84)**, not with volatility (−0.04) or
the 2018–23 beetle regime (−0.06 — beetle hypothesis rejected). Our buffer is too
steep in λ; JRC is compressed at both ends. After the floor + saturation the λ-bias
halves (r → 0.36) but two country-level residuals remain, driven by our single
national λ vs JRC's 35 km hexagons:

- **Iberia over** (Spain conifer 56% vs JRC 44%, Portugal 85% vs 61%) — genuinely
  fire-extreme; c = 0.25 may be too high there.
- **Continental/beetle-belt under** (Czechia 43% vs 59%, UK 32% vs 65%) — UK driven
  by c = 0.12 → N_eff = 8 over-crediting diversification.

These roughly cancel in the mean and are the within-country heterogeneity the
manuscript already flags as the source of residual JRC gaps.

## fig4 under corr-aware pricing

`N_pool = 1/c` in `calc_buffer_rate` is a **no-op** (<1% — it only scales the 5%
loading). To make fig4 consistent with the headline it must adopt the tail-based
corr-aware pricing. Portfolio-weighted, all six scenarios then land at **~30–37%, all
above the flat 20% benchmark** (vs 7–12% now), and the scenario spread nearly
vanishes (the floor + saturation compress biome differences). This reverses the
current fig4 *and* the original "reducing-only stays below 20%, others exceed under
RCP 8.5" narrative — under corr-aware pricing essentially everything exceeds 20%.

## Does this threaten the headline result? No.

Integrity gaps are dominated by **temporality** (ΔT = 83–121% of each scheme's gap);
the buffer channel is small (Δb = 0.00–0.15, negative for Peatland Code). Raising the
buffer lowers framework net climate value, so the "schemes over-credit" finding is
**unchanged or slightly strengthened**. The buffer recalibration mainly affects the
buffer-pool-design story (fig4 + the solvency section), not the central thesis.

## Recommended actions

1. **Decide** whether to re-anchor the headline buffer to N_eff = 1/c (+ saturation,
   + afforestation-only floor). Defensible: an independently calibrated parameter (c)
   set on an independent benchmark (JRC).
2. **Stale text to correct regardless of (1)** — these are wrong against the *current*
   pipeline already:
   - JRC comparison caption "median absolute difference from JRC mean is 3.0 pp" →
     actually 17 pp now (5.8 pp if corr-aware adopted).
   - fig4 narrative "reducing-only 14–16%, neutral/supply 21–27%, peaks 25–30%" →
     current pipeline gives 7–13% (all <20%); corr-aware gives ~30–37% (all >20%).
   Defer the exact rewrite until (1) is decided, since adoption changes the numbers.
3. If adopted: full pipeline re-run (10k MC) to refresh abstract rates, fig4,
   integrity gaps, EMPIRICAL_BUFFER_LOOKUP, with the floor gated to afforestation.

Prototype scripts: `/tmp/jrc_finiteN.R`, `/tmp/jrc_refined.R`, `/tmp/residuals.R`,
`/tmp/proto_driver2.R`, `/tmp/fig4_corr.R`. Flags live in `R/26_empirical_buffer.R`
(default off; committed outputs unchanged).
