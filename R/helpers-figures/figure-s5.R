
# run this script from "07-make-figures.R"

# --------------------- figure S5 ---------------------

striplabs <- as_labeller(c("1" = "Zone 1 (Rack 1)", "2" = "Zone 1 (Rack 2)"))

figure_s5 <- smooths[[3]] %>%
  mutate(
    date = date_numeric + p_inc_z1,
    rack = as.numeric(str_extract(series, "(?<=r)\\d")),
    pipe = str_extract(series, "(?<=p)\\d$")
  ) %>%
  ggplot(aes(date, estimate__, col = pipe)) +
  facet_wrap(vars(rack), labeller = striplabs) +
  scale_y_continuous(expand = expansion(add = .1)) +
  scale_color_manual(values = palette[c(1,3,4,6)]) +
  geom_line() +
  add_rug(date) +
  labs(
    x = NULL,
    y = "Partial effect",
    col = "Pipe"
  )

ggsave("figures/figure-s5.png", figure_s5, dev = "png", dpi = 600, width = 6.5, height = 3)
