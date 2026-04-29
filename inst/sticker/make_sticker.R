# =============================================================================
# ViewR — make_sticker.R
# Regenerate the hex sticker using the hexSticker package.
# Run from the package root:
#   source("inst/sticker/make_sticker.R")
# =============================================================================

# Install hexSticker if needed
if (!requireNamespace("hexSticker", quietly = TRUE)) {
  install.packages("hexSticker")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
if (!requireNamespace("showtext", quietly = TRUE)) {
  install.packages("showtext")
}

library(hexSticker)
library(ggplot2)
library(showtext)

# ── Load a Google Font for the sticker ──────────────────────────────────────
font_add_google("Fira Sans", "fira")
showtext_auto()

# ── Build the inner ggplot: a mini data-frame heatmap ───────────────────────
set.seed(42)
inner_df <- expand.grid(col = factor(paste0("V", 1:4)),
                        row = factor(paste0("R", 5:1)))
inner_df$value <- c(
  # Coloured to mimic a typical data table
  0.9, 0.3, 0.7, 0.5,
  0.2, 0.8, 0.4, 0.9,
  0.6, 0.1, 0.9, 0.3,
  0.8, 0.5, 0.2, 0.7,
  0.4, 0.9, 0.6, 0.1
)
inner_df$selected <- inner_df$row == "R3"   # highlight one row

p <- ggplot(inner_df, aes(x = col, y = row, fill = value)) +
  geom_tile(aes(colour = selected), size = 0.4, width = 0.92, height = 0.92) +
  scale_fill_gradient(low = "#1a4a6b", high = "#3498db", guide = "none") +
  scale_colour_manual(values = c("FALSE" = "transparent",
                                  "TRUE"  = "#ffffff"),
                      guide = "none") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "transparent", colour = NA),
    panel.background = element_rect(fill = "transparent", colour = NA)
  )

# ── Create the sticker ───────────────────────────────────────────────────────
sticker(
  subplot    = p,
  package    = "ViewR",
  p_size     = 24,
  p_family   = "fira",
  p_color    = "#ffffff",
  p_y        = 1.55,
  s_x        = 1,
  s_y        = 0.85,
  s_width    = 1.2,
  s_height   = 0.9,
  h_fill     = "#1a252f",
  h_color    = "#3498db",
  h_size     = 1.5,
  url        = "github.com/imaheshdivakaran/ViewR",
  u_size     = 4.5,
  u_color    = "rgba(255,255,255,0.55)",
  filename   = "inst/sticker/ViewR-hex.png",
  dpi        = 300
)

message("Hex sticker saved to inst/sticker/ViewR-hex.png")
