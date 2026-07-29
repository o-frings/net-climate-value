# =============================================================================
# engine/R/_labels.R  —  presentation label helpers (engine-side, self-contained)
# =============================================================================
# Ported from prep/04_functions.R so the engine can emit plot-ready labels
# (practice [FT · biome]) without depending on the prep layer. Used by
# 15_figure_data.R. No analysis here — string/label construction only.
# =============================================================================

# Species -> forest type is read from practices.csv, the parameter source of truth that
# already carries a forest_type column. The previous hardcoded CONIFER_SPECIES /
# BROADLEAF_SPECIES lists duplicated it and defaulted anything unlisted to broadleaf, so a
# newly added conifer or a misspelled species was silently mislabelled in every bar label.
# Verified identical to those lists for every species in practices.csv.
.SPECIES_FT <- local({
  p <- read.csv("engine/params/practices.csv", stringsAsFactors = FALSE)
  p <- p[!duplicated(p$species), ]
  setNames(p$forest_type, p$species)
})
BIOME_ABBREV <- c(Boreal = "Bor", Temperate = "Tem",
                  Temperate_UK = "Tem-UK", Mediterranean = "Med")
FOREST_TYPE_ABBREV <- c(broadleaf = "BL", conifer = "CF")

# Secondary BL/CF variants dropped from the compact headline (fig3a); kept in
# the full cross-biome decomposition (ED2).
SECONDARY_BLCF_VARIANTS <- list(
  c("Extended rotation",           "Beech/oak"),
  c("Reduced harvest intensity",   "Mixed broadleaves"),
  c("Set-aside",                   "Norway spruce"),
  c("Continuous cover forestry", "Mixed conifers"),
  c("Reforestation",               "Mixed conifers"))

forest_type_from_species <- function(species) {
  if (length(species) > 1)
    return(vapply(species, forest_type_from_species, character(1), USE.NAMES = FALSE))
  if (is.na(species)) stop("forest_type_from_species: NA species")
  if (species %in% c("broadleaf", "conifer")) return(species)
  # %in% names(), not [[ ]] with an is.null() guard: [[ on a named character vector
  # errors on a missing name rather than returning NULL, so such a guard never fires.
  if (!species %in% names(.SPECIES_FT))
    stop("forest_type_from_species: species not in practices.csv: '", species, "'")
  .SPECIES_FT[[species]]
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
