# =============================================================================
# figures/palette.R — the manuscript's single colour palette
# =============================================================================
# Sourced by _setup_legacy.R AFTER prep/04_functions.R, deliberately overriding
# the legacy constants there: every figure gets its colours from this file.
# Design contract: two accents in the manuscript's original hues — colour
# identifies a quantity or category (blue = net issuance/NCV/EU-pool,
# red = gap/risk/country-level); magnitude is encoded by position. Value ramps
# (blue-beige-red) are reserved for maps/heatmaps, where position cannot carry
# the value. Assumes cwd = analysis/ (as all figure-layer scripts do).
# =============================================================================

NATURE_BLUE <- "#2C7FB8"; NATURE_RED <- "#C0392B"
NATURE_BLUE_LIGHT <- "#BDD7EE"; NATURE_BEIGE <- "#E8D5C4"
NATURE_GREY <- "#5A5A5A"; NATURE_GREY_LIGHT <- "#F5F5F5"

BIOME_COLOURS <- c(Boreal = "#4A90D9", Temperate = "#009E73",
                   Mediterranean = "#E69F00", Temperate_UK = "#56B4E9")

PRACTICE_TYPE_COLOURS <- c("Harvest-reducing" = NATURE_RED,
                           "Harvest-neutral" = "#8C8C8C",
                           "Harvest-increasing" = NATURE_BLUE)

# Shade order documented in the Fig. 3 caption: leakage (dark), buffer
# (medium), temporality (light).
DEDUCTION_COLOURS <- c(Leakage = "#808080", Buffer = "#A8A8A8",
                       Temporality = "#C8C8C8", `Net issuance` = NATURE_BLUE)

# CRCF deployment scenarios, shared by fig4 and fig5 (same scenario = same
# colour in both): pure scenarios carry the full class colour, mixed scenarios
# a 45%-white tint of their dominant class. Matches on label text, so both
# figures' label variants ("Reducing\nonly", "Mixed: reducing") resolve.
scenario_colours <- function(labels) {
  cls <- ifelse(grepl("educing", labels), "reducing",
                ifelse(grepl("eutral", labels), "neutral", "increasing"))
  pure  <- c(reducing = NATURE_RED, neutral = "#8C8C8C", increasing = NATURE_BLUE)
  mixed <- c(reducing = "#DC928A", neutral = "#C0C0C0", increasing = "#8BB9D8")
  setNames(ifelse(grepl("Mixed", labels), mixed[cls], pure[cls]), labels)
}
