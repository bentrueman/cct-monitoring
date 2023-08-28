
# run this script from "07-make-figures.R"

# --------------------- figure S4 ---------------------

preds_series <- conditional_smooths(model_gam, smooths = "s(date_numeric,by=series,m=1)")[[1]] %>% 
  mutate(
    pipe_number = str_extract(series, "(?<=p)\\d"),
    location = str_extract(series, ".+(?=\\.p)")
  ) %>% 
  rename_all(~ str_remove_all(.x, "__$"))

p1 <- preds_series %>% 
  left_join(limits, by = "location") %>% 
  filter(date_numeric >= min, date_numeric <= max) %>% 
  ggplot(aes(date_numeric, estimate, col = pipe_number)) +
  facet_wrap(vars(location), labeller = striplabs, ncol = 3) +
  geom_line() + 
  geom_rug(
    data = data,
    sides = "b",
    inherit.aes = FALSE,
    aes(x = date_numeric)
  ) +
  scale_color_manual(values = pal_short) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    x = "Time (days)",
    y = "Partial effect",
    col = "Pipe"
  )

ph_increase_date <- ph_increase + p_inc_pw

annot_1$x <- ph_increase_date
annot_1$y <- 5.6
annot_1$lab <- "pH = 6.3"

p2 <- pidat %>% 
  filter(date > "2021-01-01") %>% 
  ggplot(aes(date, ph)) + 
  geom_line() + 
  add_label(labels = annot_1, nudge_x = 55, col = palette[5]) +
  geom_vline(xintercept = ph_increase_date, col = palette[5]) + 
  labs(x = NULL, y = "Coagulation pH")

figure_s4 <- wrap_plots(p1, p2, widths = c(3,1)) + 
  plot_annotation(tag_levels = "a")

ggsave("figures/figure-s4.png", figure_s4, dev = "png", dpi = 600, width = 6.5, height = 2.5)
