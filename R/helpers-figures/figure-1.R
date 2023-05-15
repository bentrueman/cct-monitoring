
# run this script from "07-make-figures.R"

# --------------------- figure 1 ---------------------

# dates and labels:

data_full <- mutate(data_full, zone = paste("Zone", zone))

p_increase <- 0

# range of data for each group:

limits <- data_full %>% 
  group_by(location) %>% 
  summarize(min = min(date_numeric), max = max(date_numeric)) %>% 
  ungroup()

# generate predictions

preds_global <- local_slope(
  data_full, model_gam, "date_numeric", epsilon = .1, 
  smooth = "s(date_numeric,k=6)", 
  pts = 400
) %>% 
  summarize_slopes(width = .95)

preds_loc <- local_slope(
  data_full, model_gam, "date_numeric", epsilon = 1, 
  smooth = "s(date_numeric,by=location,m=1)",
  g_var = "location",
  pts = 400,
  # adding next line because brms::posterior_smooths() requires that all variables used in 
  # constructing AR term also included in newdata arg... even if they are not used in the smooth:
  add_vars = list(series = rep(unique(data_full$series)[1:3], each = 400))
) %>% 
  summarize_slopes(location, width = .95) %>% 
  mutate(
    zone = str_extract(location, "(?<=z)\\d"),
    zone = paste("Zone", zone),
    rack = str_extract(location, "(?<=r)\\d")
  )

preds <- data_full %>% 
  bind_cols(fitted(model_gam, incl_autocor = FALSE)) %>% 
  mutate(
    Estimate = exp(Estimate),
    pipe = factor(pipe_number)
  )

preds_series <- conditional_smooths(model_gam, smooths = "s(date_numeric,by=series,m=1)")[[1]] %>% 
  mutate(
    zone = str_extract(series, "(?<=z)\\d"),
    pipe_number = str_extract(series, "(?<=p)\\d"),
    location = str_extract(series, ".+(?=\\.p)")
  ) %>% 
  rename_all(~ str_remove_all(.x, "__$"))

# generate plot:

annot_1 <- tibble(
  x = 0, 
  y = -Inf,
  lab = "Dose increase<br>(1&ndash;2 mg PO<sub>4</sub> L<sup>-1</sup>)",
  location = "global"
)

annot_2 <- tibble(
  x = ph_increase, 
  y = -Inf,
  lab = "Coagulation pH increase",
  location = "z2.r1"
)

fig_1a <- preds_loc %>%
  left_join(limits, by = "location") %>% 
  filter(date_numeric >= min, date_numeric <= max) %>%
  bind_rows(mutate(preds_global, location = "global")) %>% 
  ggplot(aes(date_numeric, smooth_estimate)) +
  facet_wrap(vars(location), labeller = striplabs, ncol = 1) + 
  # scale_color_manual(values = palette[6], labels = label_parse()) +
  scale_color_manual(values = palette[6]) +
  geom_vline(
    data = tibble(location = c("global", "z2.r1"), date_numeric = c(0, ph_increase)),
    aes(xintercept = date_numeric), linetype = 3
  ) + 
  add_label(labels = annot_1, nudge_x = -200) +
  add_label(labels = annot_2, nudge_x = -200) +
  geom_ribbon(aes(ymin = smooth_lower, ymax = smooth_upper), alpha = alpha, col = NA) +
  geom_line() + 
  geom_line(
    data = function(x) {
      x %>% 
        mutate(
          smooth_estimate = if_else(
            sign(slope_lower) == sign(slope_upper),
            smooth_estimate,
            NA_real_
          )
        )
    },
    # aes(col = "frac(d*'['*Pb*']',dt)<0")
    aes(col = "d[Pb]/dt < 0")
  ) +
  geom_rug(
    data = distinct(data_full, location, date_numeric) %>% 
      bind_rows(tibble(location = "global", date_numeric = unique(data_full$date_numeric))),
    sides = "b",
    inherit.aes = FALSE,
    aes(x = date_numeric)
  ) +
  labs(
    x = "Time (days)",
    y = "Partial effect",
    col = NULL
  )

fig_1b <- finished_ortho %>%
  filter(
    date >= min(data_full$date),
    date <= max(data_full$date)
  ) %>% 
  ggplot(aes(date_numeric, value, col = as.factor(zone))) + 
  scale_color_manual(values = palette[c(1,4)]) +
  theme(
    axis.title.y = element_markdown(),
    legend.position = "right"
  ) +
  geom_rug(
    data = \(x) filter(x, value > 3),
    aes(date),
    sides = "t",
    inherit.aes = FALSE
  ) +
  geom_line(
    data = \(x) filter(x, value <= 3),
    linewidth = .2
  ) +
  labs(
    x = "Time (days)",
    y = "Orthophosphate<br>(mg PO<sub>4</sub> L<sup>-1</sup>)",
    col = "Zone"
  )

annot_3 <- annot_1
annot_3$lab <- "Dose increase (1&ndash;2 mg PO<sub>4</sub> L<sup>-1</sup>)"
annot_3$location <- "z1.r1"
annot_3$y <- Inf

fig_1c <- preds %>%
  ggplot(aes(date_numeric, col = pipe)) + 
  theme(legend.position = "right") +
  facet_wrap(vars(location), labeller = striplabs, ncol = 1) + 
  geom_vline(xintercept = p_increase, linetype = 3) +
  add_label(labels = annot_3) +
  geom_rug(data = \(x) filter(x, value < 5)) +
  geom_line(
    data = \(x) filter(x, value >= 5),
    aes(y = value), linewidth = .2, show.legend = FALSE
  ) +
  geom_line(aes(y = Estimate)) +
  scale_y_log10() + 
  scale_color_manual(values = pal_short) +
  labs(
    x = "Time (days)",
    y = expression("[Pb] (µg L"^-1*")"),
    col = "Pipe"
  )

figure_1 <- wrap_plots(
  fig_1a, fig_1b, fig_1c, 
  design = "AB\nAC\nAC\nAC"
) + 
  plot_annotation(tag_levels = "a")

ggsave("figures/figure-1.png", figure_1, dev = "png", dpi = 600, width = 6.5, height = 6)
