# =============================================================================
# engine/R/figdata/ed_pool_buildup.R — plot-ready data for ed_pool_buildup
# =============================================================================
# Pool buffer diversification vs pool size. ALL numeric preparation for the
# figure lives here; figures/ed_pool_buildup.R only reads these tables and draws.
# Runs inside 15_figure_data.R (mc_results, eng(), wfd(), dplyr/tidyr, label
# helpers in scope). Source: engine/output/pool_buildup.csv.
# =============================================================================
local({
  # ─── Engine data: K, div_ratio, ordering (31 K-values x 2 orderings) ───
  d <- eng("pool_buildup.csv")

  # Ordering factor → integer 'ord' column the plot uses for level order.
  ord_levels <- c("Random enrolment", "Largest forest nations first")
  d$ord <- match(d$ordering, ord_levels)
  d <- d[order(d$ord, d$K), c("K", "div_ratio", "ordering", "ord")]

  # Full-pool asymptote = random-enrolment div_ratio at the largest pool.
  asym <- d$div_ratio[d$ordering == "Random enrolment" & d$K == max(d$K)]
  nC   <- max(d$K)

  # Meta table: scalars, the asymptote label string, x-axis annotation y, and
  # the x-axis break positions — everything numeric/derived the plot needs.
  meta <- data.frame(
    asym       = asym,
    nC         = nC,
    asym_y     = asym - 0.03,
    asym_label = sprintf("Full EU pool = %.2f (panel b)", asym),
    stringsAsFactors = FALSE
  )

  breaks <- data.frame(x_break = seq(0, nC, 5))

  wfd(d,      "fd_ed_pool_buildup_main")
  wfd(meta,   "fd_ed_pool_buildup_meta")
  wfd(breaks, "fd_ed_pool_buildup_breaks")
})
