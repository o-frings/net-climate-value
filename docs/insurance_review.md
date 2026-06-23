# Insurance-modeller's review of the buffer pool calculation

*Internal review — not for the manuscript without further discussion. Notes
shortcomings to address before claiming the buffer pool is a Solvency-II-grade
prudential reserve.*

## Critical (changes the buffer materially)

1. **Multiplicative loading on the mean, not the tail.** Formula is `b = (1+θ) × E[L]`.
   Disturbance has heavy tails (mega-fires, beetle outbreaks) where VaR/E[L] is
   3-5×, not 1.17×. Standard actuarial practice: VaR_{99.5} or ES_{99.5}, not
   μ + θ·μ.
2. **Constant `c` ignores correlation breakdown under stress.** Cross-region
   correlation rises in stress years (2018-2020 European drought triggered
   simultaneous fire spikes; 2003 heatwave depressed GPP across Europe in
   lockstep). `c = 0.15-0.25` from Anderegg captures baseline; under-prices
   cluster-year risk.
3. **No explicit finite-N pool.** MC implicitly assumes asymptotic-N pool with
   biome-mean λ describing pool experience exactly. For a CRCF pool of N=100
   with c=0.2, σ_pool ≈ 45% of σ_per-project — implies θ_eff should be ~0.45,
   not 0.166.
4. **Single-layer buffer.** Real cat insurance has primary / excess / cat layers
   with parametric triggers. A flat buffer rate covers routine reversals but
   structurally cannot absorb a 1-in-50-year continental event.

## Important (worth flagging in limitations)

5. **Severity bounded; no fat tails.** `Z ~ p × 1 + (1-p) × Beta(2, 3.7)` is
   bounded [0,1]. Mechanically correct per project; for the pool, joint extremes
   are absent.
6. **Severity ⊥ frequency.** Z and λ_path are iid. In reality positively
   correlated (hot dry summers: more ignitions AND bigger fires).
7. **Solvency target 95% not 99.5%.** Solvency II uses 99.5% (1-in-200-yr).
   Our 95% means 1-in-20-yr. Pool would be uninsurable as a regulated entity
   at this level.
8. **P(ruin) only — no Expected Shortfall.** MC checks `all(balance >= 0)` but
   not how negative balance goes when it fails.
9. **No adverse selection / moral hazard.** Uniform buffer rate per
   practice/biome; no project-level risk modifiers (slope, fuel load,
   operator history).
10. **State-dependent vulnerability missing.** Post-disturbance stands are more
    flammable / beetle-vulnerable. AR(1) on λ doesn't capture state-dependent
    feedback.

## Minor / by-design

11. Discount rate not in solvency cash flow. Real for H=100 peatland.
12. Climate scenario committed, not integrated over uncertainty.
13. No reinsurance market dynamics.
14. Pool replenishment not modelled (probably correct — want to know if
    self-sufficient).

## Honest manuscript framing

The buffer pool is calibrated as a long-run actuarial premium under stationary
stochastic disturbance, with correlation and tails treated as parameters rather
than empirically estimated. It is **not** a Solvency-II-grade insurance reserve.
For prudential adequacy at policy scale, complementary mechanisms (sovereign
backstop, parametric reinsurance, risk-based pricing) are warranted above the
routine buffer layer. The model bounds the routine reversal cost; the residual
cat-risk requires policy infrastructure outside the buffer pool.

## Top-priority fixes if revisiting

A. **Empirical bootstrap from EFDA worst years** instead of parametric AR(1) +
   Beta severity. Captures observed joint extremes directly.
B. **Augmented MC with finite N + country-sampled disturbance** (option 2 from
   prior conversation). Re-bisects θ_base for realistic pool size.
C. **State-dependent λ** via Markov chain on disturbance regime
   (low/medium/high) following Anderegg-style transitions.
D. **Re-target solvency at 99% with ES reporting** alongside P(ruin).
