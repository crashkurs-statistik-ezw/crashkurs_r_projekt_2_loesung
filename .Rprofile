.First <- function() {
  # Load packages
  library(tidyverse)
  
  # Set theme
  apa_theme <- theme(
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 22, face = "bold",
                              hjust = 0.5,
                              margin = margin(b = 15)),
    axis.line = element_line(color = "black", size = .5),
    axis.title = element_text(size = 18, color = "black",
                              face = "bold"),
    axis.text = element_text(size = 15, color = "black"),
    axis.text.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.ticks = element_line(size = .5),
    panel.grid = element_blank(),
    legend.position = c(0.20, 0.8),
    legend.background = element_rect(color = "black", size = 0.5),
    legend.text = element_text(size = 15),
    legend.margin = margin(t = 5, l = 5, r = 5, b = 5),
    legend.key = element_rect(color = NA, fill = NA)
  )
  
  theme_set(theme_minimal(base_size = 18) +
              apa_theme)
}
