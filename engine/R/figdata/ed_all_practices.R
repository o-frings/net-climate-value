# =============================================================================
# engine/R/figdata/ed_all_practices.R  —  Extended Data Fig 2 data fragment
# =============================================================================
# All numeric prep for the all-practices cross-biome decomposition (ED2) lives
# here; figures/ed_all_practices.R only reads these tables and draws. Logic is a
# verbatim port of the old in-figure computation: median across the 10k MC draws
# per practice×biome×species variant, clamp leakage at 0, recompute net_share as
# 1 - time - leakage - buffer, build the FT·biome variant label, and order
# practices by mean net_share (desc) and variants by mean net_share (asc, so the
# highest sits at the top after coord_flip). Emits an integer 'ord' column for
# each factor so the plot never computes an ordering. Runs inside 15_figure_data.R
# (mc_results, eng, wfd, dplyr/tidyr, _labels.R helpers all in scope).
# =============================================================================

# ─── Per-variant decomposition from mc_results: ALL 44 cross-biome variants ───
# House style: median across draws. Keep every variant (no is_anchor filter, no
# secondary-variant drop) — this ED figure is the full cross-biome set.
peatland_species <- c("Paludiculture", "Drained peatland", "Drained peatland forest")

xb <- mc_results %>%
  group_by(practice, biome, species) %>%
  summarise(net_share = median(net_share), delta_leak = median(delta_leak),
            delta_temp = median(delta_temp), delta_buf = median(delta_buf),
            .groups = "drop") %>%
  mutate(
    delta_leak = pmax(delta_leak, 0),
    net_share  = 1 - delta_temp - delta_leak - delta_buf,
    ft_code    = unname(FOREST_TYPE_ABBREV[sapply(species, forest_type_from_species)]),
    ft_code    = ifelse(is.na(ft_code) | species %in% peatland_species, "—", ft_code),
    biome_code = unname(BIOME_ABBREV[biome]),
    variant    = paste0(ft_code, " · ", biome_code)
  )

# Practices ordered by mean NCV desc; variants ascending so highest sits at the
# top after coord_flip (global factor, exactly as legacy). Emit integer 'ord'
# columns so the figure builds factors from these without any computation.
practice_order <- xb %>% group_by(practice) %>%
  summarise(mean_nv = mean(net_share), .groups = "drop") %>%
  arrange(desc(mean_nv)) %>% pull(practice)
variant_order <- xb %>% group_by(variant) %>%
  summarise(mean_nv = mean(net_share), .groups = "drop") %>%
  arrange(mean_nv) %>% pull(variant)

practice_ord_lkp <- setNames(seq_along(practice_order), practice_order)
variant_ord_lkp  <- setNames(seq_along(variant_order), variant_order)

xb <- xb %>% mutate(
  practice_ord = unname(practice_ord_lkp[as.character(practice)]),
  variant_ord  = unname(variant_ord_lkp[as.character(variant)]))

# ─── Two plot-ready frames: stacked deductions (left) and net-issuance bar ───
# Emit plot-ready y positions too (neg_share = stacked to the left of zero;
# label_y = mid-bar text anchor) so the figure does no arithmetic in aes().
ded_long <- xb %>%
  select(practice, practice_ord, variant, variant_ord,
         Leakage = delta_leak, Time = delta_temp, Buffer = delta_buf) %>%
  pivot_longer(c(Leakage, Time, Buffer), names_to = "component", values_to = "share") %>%
  mutate(component_ord = match(component, c("Buffer", "Time", "Leakage")),
         neg_share = -share)

net_bar <- xb %>% transmute(practice, practice_ord, variant, variant_ord, net_share,
                            label_y = net_share / 2,
                            label = sprintf("%.0f%%", net_share * 100))

wfd(ded_long, "fd_ed_all_practices_ded")
wfd(net_bar,  "fd_ed_all_practices_net")
