
# run this script from "07-make-figures.R"

# --------------------- toc art ---------------------

figure_toc <- preds_global %>%
  mutate(
    date = date_numeric + p_inc_z1,
    smooth_estimate_nzd = if_else(
      sign(slope_lower) == sign(slope_upper),
      smooth_estimate,
      NA_real_
    ),
    # create a grouping vector for the derivative label colours:
    # start and end of regions of change:
    start = !is.na(smooth_estimate_nzd) & is.na(lag(smooth_estimate_nzd)),
    end = !is.na(smooth_estimate_nzd) & is.na(lead(smooth_estimate_nzd)),
    smgrp = cumsum(start) + cumsum(end),
    smgrp = if_else(smgrp %in% c(2, 4), 0, smgrp),
    smgrp = fct_recode(
      as.factor(smgrp),
      "no trend" = "0",
      "negative" = "1",
      "positive" = "3"
    )
  ) %>% 
  ggplot(aes(date, smooth_estimate)) + 
  scale_color_manual(values = c("black", palette[c(1, 6)])) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  geom_vline(xintercept = p_inc_z1, linetype = 3) +
  geom_vline(xintercept = p_dec_z1, linetype = 3) +
  geom_label(
    data = . %>% 
      distinct(smgrp) %>% 
      mutate(
        x = c(as.Date("2019-11-01"), as.Date("2021-05-01"), as.Date("2022-05-05")),
        y = c(1.7, -0.7, -0.05),
        label = c(
          # two calls to atop() decreases spacing between lines:
          "atop(NA, atop(textstyle(Orthophosphate),textstyle(dose)))", 
          "frac(d*'['*Pb*']',dt)<0", "frac(d*'['*Pb*']',dt)>0"
        )
      ),
    aes(x, y, label = label, col = smgrp),
    label.r = unit(0, "lines"),
    label.size = 0,
    alpha = .8,
    size = 2.5,
    parse = TRUE,
    show.legend = FALSE
  ) +
  geom_ribbon(aes(ymin = smooth_lower, ymax = smooth_upper), alpha = alpha) +
  geom_line() +
  geom_line(
    data = finished_ortho %>% 
      filter(
        zone == 1,
        date >= min(data_z1$date),
        date <= max(data_z1$date)
      ),
    aes(date, value),
    linewidth = .2
  ) +
  geom_line(
    aes(y = smooth_estimate_nzd, col = smgrp),
    show.legend = FALSE
  ) +
  add_rug(date) +
  labs(
    x = NULL,
    y = NULL,
    col = NULL
  )

ggsave("figures/figure-toc.png", figure_toc, dev = "png", dpi = 600, width = 3.33, height = 1.875)
