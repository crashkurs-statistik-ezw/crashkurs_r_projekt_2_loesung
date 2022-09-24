.First <- function() {
  # Load packages
  library(tidyverse)
  
  # Set theme
  apa_theme <- theme(
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 18, face = "bold",
                              hjust = 0,
                              margin = margin(b = 15)),
    axis.line = element_line(color = "black", size = .5),
    axis.title = element_text(size = 14, color = "black",
                              face = "bold"),
    axis.text = element_text(size = 12, color = "black"),
    axis.text.x = element_text(margin = margin(t = 5)),
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15)),
    axis.ticks = element_line(size = .5),
    panel.grid = element_line(color = "grey80", size = 0.5),
    panel.grid.major.x= element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "bottom",
    legend.background = element_rect(color = "gray80", size = 0.5),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.margin = margin(t = 5, l = 5, r = 5, b = 5),
    legend.key = element_rect(color = NA, fill = NA)
  )
  
  theme_set(theme_minimal(base_size = 18) +
              apa_theme)
}
