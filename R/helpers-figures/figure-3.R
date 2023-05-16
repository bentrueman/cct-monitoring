
# run this script from "07-make-figures.R"

# --------------------- figure 3 ---------------------

p_inc_z1 <- as.Date("2020-11-30")
p_dec_z1 <- as.Date("2022-05-16")

# generate predictions

epreds <- data_z1 %>%
  with(crossing(
    series = unique(series),
    date = seq(min(date), max(date), by = "1 week")
  )) %>% 
  mutate(
    date_numeric = as.numeric(difftime(date, p_inc_z1, units = "days")), # date of P dose increase
    zone = str_extract(series, "(?<=z)\\d"),
    rack = paste("Rack", str_extract(series, "(?<=r)\\d")),
    pipe_number = str_extract(series, "(?<=p)\\d")
  ) %>% 
  verify(min(date_numeric) == min(data_z1$date_numeric)) %>% 
  # verify that max date_numeric is equal to max date_numeric in data_z1 that is divisible by 7:
  verify(max(date_numeric) == 7 * floor(max(data_z1$date_numeric) / 7)) %>% 
  make_model_in() %>% 
  mutate(
    epreds = fitted(model_gam_z1, newdata = ., incl_autocor = FALSE)[, "Estimate"],
    epreds = exp(epreds)
  )

smooths <- conditional_smooths(model_gam_z1, prob = 0.95)

preds_global <- local_slope(
  data_z1, model_gam_z1, "date_numeric", epsilon = .1, 
  smooth = "s(date_numeric,k=15)", 
  pts = 400
) %>% 
  summarize_slopes(width = .95)

# generate plot:

fig_3a <- preds_global %>%
  mutate(date = date_numeric + p_inc_z1) %>% 
  ggplot(aes(date, smooth_estimate)) + 
  scale_color_manual(values = palette[6], labels = label_parse()) +
  geom_vline(xintercept = p_inc_z1, linetype = 3) +
  geom_vline(xintercept = p_dec_z1, linetype = 3) +
  geom_ribbon(aes(ymin = smooth_lower, ymax = smooth_upper), alpha = alpha) +
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
    aes(col = "d*'['*Pb*']'*'/'*dt!=0")
  ) +
  add_rug(date) +
  labs(
    x = NULL,
    y = "Partial effect",
    col = NULL
  )

fig_3b <- finished_ortho %>% 
  filter(
    zone == 1,
    date >= min(data_z1$date),
    date <= max(data_z1$date)
  ) %>% 
  ggplot(aes(date, value)) + 
  theme(axis.title.y = element_markdown()) +
  geom_rug(
    data = \(x) x %>% 
      filter(value > 3),
    aes(date),
    sides = "t",
    inherit.aes = FALSE
  ) +
  geom_line(
    data = \(x) x %>% 
      filter(value <= 3),
    linewidth = .2
  ) +
  labs(
    x = NULL, 
    y = "Orthophosphate<br>(mg PO<sub>4</sub> L<sup>-1</sup>)"
  )

fig_3c <- smooths[[2]] %>%
  ggplot(aes(as.Date("2021-01-01") + 365 * date_yday, estimate__)) + 
  scale_x_date(date_labels = "%b") +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = alpha) +
  geom_line() + 
  add_rug(as.Date("2021-01-01") + 365 * date_yday) +
  labs(
    x = NULL,
    y = "Partial effect"
  )

fig_3d <- data_z1 %>%
  mutate(
    pipe_number = as.factor(pipe_number),
    rack = paste("Rack", rack)
  ) %>%
  ggplot(aes(date, value, col = pipe_number)) + 
  theme(legend.position = "right") +
  facet_wrap(vars(rack), ncol = 1) +
  scale_color_manual(values = palette[c(6,4,3,1)]) +
  scale_y_log10() +
  geom_line(
    data = \(x) x %>% 
      filter(value >= 5, value <= 500),
    linewidth = .2
  ) + 
  geom_rug(
    data = \(x) x %>% 
      filter(value < 5),
    aes(x = date, col = pipe_number),
    inherit.aes = FALSE,
    linewidth = .3,
    sides = "b"
  ) + 
  geom_rug(
    data = \(x) x %>% 
      filter(value > 500),
    aes(x = date, col = pipe_number),
    inherit.aes = FALSE,
    linewidth = .3,
    sides = "t"
  ) + 
  geom_line(
    data = epreds,
    aes(x = date, y = epreds)
  ) + 
  labs(
    x = NULL, 
    y = expression("[Pb] (µg L"^-1*")"),
    col = "Pipe"
  )

fig_3e <- wq %>% 
  filter(param == "temp", zone == 1) %>% 
  group_by(
    date_yday_n = yday(date) / if_else(leap_year(date), 366, 365),
    date_yday = as.Date("2021-12-31") + 365 * date_yday_n
  ) %>% 
  summarize(
    min = min(value),
    max = max(value),
    value = median(value)
  ) %>% 
  ungroup() %>% 
  ggplot(aes(date_yday, value)) + 
  theme(
    axis.title.y = element_markdown(),
    legend.position = "right"
  ) +
  scale_x_date(date_labels = "%b") +
  scale_color_manual(values = palette[1]) +
  geom_errorbar(aes(ymin = min, ymax = max), width = 0, linewidth = .3) +
  geom_point(shape = 16, alpha = .5) +
  geom_line(
    data = \(x) x %>% 
      mutate(
        cc = predict(
          mgcv::gam(
            value ~ s(date_yday_n, bs = "cc"),
            method = "REML",
            knots = list(date_yday_n = c(0, 1))
          )
        )
      ),
    aes(y = cc, col = "Cubic\nspline"),
    linewidth = .8
  ) +
  labs(
    x = NULL,
    y = "Water<br>temperature (&deg;C)",
    col = NULL
  )

figure_3 <- wrap_plots(fig_3a, fig_3b, fig_3c, fig_3d, fig_3e, design = "AD\nBD\nCE") + 
  plot_annotation(tag_levels = "a")

ggsave("figures/figure-3.png", figure_3, dev = "png", dpi = 600, width = 6.5, height = 5)

