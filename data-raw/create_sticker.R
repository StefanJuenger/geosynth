# install.packages(c("hexSticker", "ggplot2", "showtext"))
library(hexSticker)
library(ggplot2)
library(showtext)

# Use a monospace font (matching the SVG design)
font_add_google("Share Tech Mono", "mono")
showtext_auto()

# ── Data ──────────────────────────────────────────────────────────────────────

set.seed(42)

orig <- data.frame(
  x = c(0.18, 0.30, 0.42, 0.58, 0.68, 0.22, 0.50, 0.62, 0.72, 0.38, 0.12),
  y = c(0.62, 0.54, 0.66, 0.58, 0.64, 0.38, 0.32, 0.38, 0.32, 0.26, 0.46)
)

synth <- data.frame(
  x = orig$x + runif(nrow(orig), -0.07, 0.07),
  y = orig$y + runif(nrow(orig), -0.07, 0.07)
)

# ── Plot ──────────────────────────────────────────────────────────────────────

p <- ggplot() +
  # Grid lines for geographic feel
  geom_hline(yintercept = c(0.25, 0.45, 0.65), color = "#2e2e2e", linewidth = 0.3) +
  geom_vline(xintercept = c(0.25, 0.50, 0.75), color = "#2e2e2e", linewidth = 0.3) +
  # Leader lines from original → synthetic
  geom_segment(
    data = data.frame(x = orig$x, y = orig$y, xend = synth$x, yend = synth$y),
    aes(x = x, y = y, xend = xend, yend = yend),
    color = "#B4B2A9", linewidth = 0.4, linetype = "dashed", alpha = 0.4
  ) +
  # Original points (grey, larger, semi-transparent)
  geom_point(
    data = orig, aes(x = x, y = y),
    color = "#B4B2A9", size = 2.2, alpha = 0.55
  ) +
  # Synthetic points (teal, smaller, crisp)
  geom_point(
    data = synth, aes(x = x, y = y),
    color = "#5DCAA5", size = 1.6, alpha = 0.92
  ) +
  xlim(0, 1) + ylim(0.1, 0.85) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#1a1a1a", color = NA),
    panel.background = element_rect(fill = "#1a1a1a", color = NA)
  )

# ── Sticker ───────────────────────────────────────────────────────────────────

sticker(
  subplot     = p,
  package     = "geosynth",
  p_size      = 30,         # package name font size
  p_color     = "#E1F5EE",   # package name color
  p_family    = "mono",
  p_fontface  = "bold",
  p_y         = 1.48,        # vertical position of package name
  s_x         = 1,           # subplot x center
  s_y         = 0.85,        # subplot y center
  s_width     = 1.4,         # subplot width (relative)
  s_height    = 0.9,         # subplot height (relative)
  h_fill      = "#1a1a1a",   # hex background
  h_color     = "#5DCAA5",   # hex border
  h_size      = 1.2,
  spotlight   = FALSE,
  url         = "synthetic geodata for surveys",
  u_size      = 8,
  u_color     = "#9FE1CB",
  u_family    = "mono",
  filename    = "man/figures/logo.png",
  dpi         = 600
)
