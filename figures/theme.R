# =============================================================================
# figures/theme.R  —  shared palette, Nature-style ggplot theme, save helper
# =============================================================================
# Foundation for the figure-rendering layer (P4). Reads engine/output/*.csv and
# writes PDF+PNG to figures/output/. Self-contained: no dependency on analysis/R.
# Ported from the validated theme_nature() / save_figure() (04_functions, 17).
# =============================================================================
suppressPackageStartupMessages({ library(ggplot2) })

FIG_OUT <- "figures/output"
dir.create(FIG_OUT, showWarnings = FALSE, recursive = TRUE)

# --- palette -----------------------------------------------------------------
NATURE_BLUE <- "#4A90D9"; NATURE_RED <- "#D55E00"
NATURE_GREY <- "#5A5A5A"; NATURE_GREY_LIGHT <- "#F5F5F5"
BIOME_COLOURS <- c(Boreal = "#4A90D9", Temperate = "#009E73",
                   Mediterranean = "#E69F00", Temperate_UK = "#56B4E9")
PRACTICE_TYPE_COLOURS <- c("Harvest-reducing" = "#D55E00",
                           "Harvest-neutral" = "#009E73",
                           "Harvest-increasing" = "#4A90D9")
DEDUCTION_COLOURS <- c(Leakage = "#8C8C8C", Temporality = "#C8C8C8",
                       Buffer = "#5C5C5C", `Net issuance` = "#4A90D9")

# --- theme -------------------------------------------------------------------
theme_nature <- function(base_size = 10, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      plot.title = element_text(face = "bold", size = rel(1.1), hjust = 0, margin = margin(b = 8)),
      plot.subtitle = element_text(size = rel(0.9), colour = NATURE_GREY, hjust = 0, margin = margin(b = 12)),
      axis.title = element_text(size = rel(0.9), colour = NATURE_GREY),
      axis.title.x = element_text(margin = margin(t = 8)),
      axis.title.y = element_text(margin = margin(r = 8), angle = 90),
      axis.text = element_text(size = rel(0.85), colour = NATURE_GREY),
      axis.line = element_line(colour = "#CCCCCC", linewidth = 0.4),
      axis.ticks = element_line(colour = "#CCCCCC", linewidth = 0.3),
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = rel(0.85)),
      legend.text = element_text(size = rel(0.8)),
      legend.key = element_rect(fill = NA, colour = NA),
      panel.grid.major.y = element_line(colour = "#EEEEEE", linewidth = 0.3),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold", size = rel(0.9)),
      strip.background = element_rect(fill = NATURE_GREY_LIGHT, colour = NA),
      plot.margin = margin(10, 10, 10, 10)
    )
}
theme_set(theme_nature())

# cairo_pdf if functional, else plain pdf (better font embedding when available)
.has_cairo <- tryCatch({ tf <- tempfile(fileext = ".pdf")
  suppressWarnings(cairo_pdf(tf, width = 1, height = 1)); dev.off(); unlink(tf); TRUE },
  error = function(e) FALSE, warning = function(w) FALSE)

# save a plot as PDF + PNG (mm; 180mm = full page, 85mm = single column)
save_fig <- function(plot, name, width = 180, height = 120) {
  pdf_path <- file.path(FIG_OUT, paste0(name, ".pdf"))
  png_path <- file.path(FIG_OUT, paste0(name, ".png"))
  tryCatch(suppressWarnings(ggsave(pdf_path, plot, width = width, height = height,
           units = "mm", device = if (.has_cairo) cairo_pdf else "pdf")),
           error = function(e) cat(sprintf("  PDF save failed (%s): %s\n", name, conditionMessage(e))))
  ok <- tryCatch({ ggsave(png_path, plot, width = width, height = height, units = "mm", dpi = 300); TRUE },
           error = function(e) { cat(sprintf("  PNG save FAILED (%s): %s\n", name, conditionMessage(e))); FALSE })
  if (ok) cat(sprintf("  saved %s (%.0fx%.0f mm)\n", name, width, height))
}

# read an engine output CSV (fail loud)
eng <- function(f) { p <- file.path("engine/output", f)
  if (!file.exists(p)) stop("engine output missing: ", p, " (run engine/R/run_engine.R first)")
  read.csv(p, stringsAsFactors = FALSE) }
