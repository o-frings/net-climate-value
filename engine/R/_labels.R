# =============================================================================
# engine/R/_labels.R  —  presentation label helpers (engine-side, self-contained)
# =============================================================================
# Ported from prep/04_functions.R so the engine can emit plot-ready labels
# (practice [FT · biome]) without depending on the prep layer. Used by
# 15_figure_data.R. No analysis here — string/label construction only.
# =============================================================================

CONIFER_SPECIES <- c("Maritime pine", "Mixed conifers", "Norway spruce",
                     "Scots pine", "Sitka spruce", "Native pinewood")
BROADLEAF_SPECIES <- c("Chestnut/oak", "Climate-adapted mix", "Cork oak",
                       "Eucalyptus", "Holm oak", "Mixed broadleaves",
                       "Mixed species", "Native broadleaves", "Old growth",
                       "Beech/oak", "Productive oak/beech")
BIOME_ABBREV <- c(Boreal = "Bor", Temperate = "Tem",
                  Temperate_UK = "Tem-UK", Mediterranean = "Med")
FOREST_TYPE_ABBREV <- c(broadleaf = "BL", conifer = "CF")

# Secondary BL/CF variants dropped from the compact headline (fig3a); kept in
# the full cross-biome decomposition (ED2).
SECONDARY_BLCF_VARIANTS <- list(
  c("Extended rotation",           "Beech/oak"),
  c("Reduced harvest intensity",   "Mixed broadleaves"),
  c("Set-aside",                   "Norway spruce"),
  c("Continuous stock management", "Mixed conifers"),
  c("Reforestation",               "Mixed conifers"))

forest_type_from_species <- function(species) {
  if (length(species) > 1)
    return(vapply(species, forest_type_from_species, character(1), USE.NAMES = FALSE))
  if (is.na(species))                          return("broadleaf")
  if (species %in% c("broadleaf", "conifer"))  return(species)
  if (species %in% CONIFER_SPECIES)            return("conifer")
  if (species %in% BROADLEAF_SPECIES)          return("broadleaf")
  "broadleaf"                                  # non-tree (peatland) default
}

is_secondary_variant <- function(practice, species) {
  sec <- vapply(SECONDARY_BLCF_VARIANTS, function(x) paste(x[1], x[2], sep = "|"),
                character(1))
  paste(practice, species, sep = "|") %in% sec
}

practice_full_label <- function(practice, species, biome) {
  ft <- forest_type_from_species(species)
  ft_code <- unname(FOREST_TYPE_ABBREV[ft]); ft_code[is.na(ft_code)] <- "—"
  ft_code[species %in% c("Paludiculture", "Drained peatland",
                         "Drained peatland forest")] <- "—"
  biome_code <- unname(BIOME_ABBREV[biome])
  biome_code[is.na(biome_code)] <- biome[is.na(biome_code)]
  paste0(practice, " [", ft_code, " · ", biome_code, "]")
}
