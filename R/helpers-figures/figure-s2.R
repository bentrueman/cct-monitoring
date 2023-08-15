
# run this script from "07-make-figures.R"

# --------------------- figure S2 ---------------------

figure_s2 <- pmap(
  list(
    c("H1", "H2", "H3"), # site codes
    c(3, 4, 3), # panels
    c("Chloropyromorphite,Hydrocerussite,Cerussite,Lead",
      "Chloropyromorphite,Hydrocerussite,Cerussite,Lead", 
      "Chloropyromorphite,Hydrocerussite,Litharge,Lead")
  ),
  .f = function(x, y, z) {
    gg_xrd(
      pipe = x, 
      layers = y,
      phases = str_split(z, ",")[[1]],
      data = xrd, 
      standards = stds
    )
  }
) %>% 
  wrap_plots() + 
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = "bold"))

ggsave("figures/figure-s2.png", figure_s2, dev = "png", dpi = 600, width = 6.5, height = 6)
