
# run this script from "07-make-figures.R"

# --------------------- figure 2 ---------------------

# part 1:

d_psim <- as_tibble(as_draws_df(model_prior, "b_round2_after")) # prior simulation
d_p <- as_tibble(as_draws_df(model, "b_round2_after"))
d_np <- as_tibble(as_draws_df(model_noprior, "b_round2_after"))

ylabs <- c( "Flat", "N(-0.8, 0.3)", "N(-0.8, 0.3)\n(prior simulation)")

p1 <- tibble(
  a = d_np$b_round2_after,
  b = d_p$b_round2_after, 
  c = d_psim$b_round2_after
) %>% 
  pivot_longer(everything(), names_to = "model") %>% 
  ggplot(aes(-100 * (1 - exp(value)), model, col = model, fill = model)) + 
  ggdist::stat_halfeye(slab_alpha = .3) + 
  geom_text(
    data = tibble(
      x = 85,
      y = letters[1:3],
      labels = ylabs
    ),
    aes(x, y, label = labels),
    size = 2.5, hjust = 1,
    inherit.aes = FALSE
  ) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  scale_colour_manual(values = c("black", palette[6], "black")) +
  scale_fill_manual(values = c("black", palette[6], "black")) +
  guides(colour = "none", fill = "none") +
  # scale_y_discrete(expand  = expansion(add = c(0.3, 0))) +
  labs(
    x = "% change in [Pb]",
    y = "Density"
  )

# part 2:

d <- as_draws_df(model, "b_round2_after") %>% 
  summarize_draws(~ quantile(.x, probs = c(0.025, .1, .5, .9, 0.975)))

p2 <- model_in %>%
  pivot_wider(
    id_cols = c(location, litre, sample), 
    names_from = round, 
    values_from = c(value, censored)
  ) %>% 
  mutate(
    litre = str_replace(litre, "Flushed", "5"),
    litre = as.numeric(str_extract(litre, "\\d"))
  ) %>% 
  clean_names() %>% 
  # ggplot(aes(value_1_before, value_2_after, col = litre)) +
  ggplot(aes(value_1_before, value_2_after)) +
  annotate("label", 5, 30, label = "Litre 1", label.size = 0, alpha = .8, size = 2.5) +
  geom_point(
    data = \(x) filter(x, if_all(starts_with("cens"), ~ .x == "none")),
    alpha = .7, shape = 16, size = 2
  ) +
  # round 1 censored:
  geom_segment(
    data = \(x) filter(x, censored_1_before == "left", censored_2_after == "none"),
    aes(x = 0, xend = value_1_before, y = value_2_after, yend = value_2_after)
  ) +
  # round 4 censored:
  geom_segment(
    data = \(x) filter(x, censored_1_before == "none", censored_2_after == "left"),
    aes(x = value_1_before, xend = value_1_before, y = 0, yend = value_2_after)
  ) +
  geom_rect(
    data = \(x) filter(x, censored_1_before == "left", censored_2_after == "left") %>% 
      slice(1),
    aes(xmin = 0, xmax = value_1_before, ymin = 0, ymax = value_2_after),
    alpha = .3, fill = "black", col = NA
  ) +
  geom_abline() +
  # difference estimate:
  add_ribbon(d, "2.5%", "97.5%", x = value_1_before[censored_1_before == "none"]) +
  add_segment(d, x = value_1_before[censored_1_before == "none"]) +
  geom_text(
    data = tibble(
      value_1_before = 3,
      value_2_after = 4,
      lab = "y=x"
    ),
    aes(value_1_before, value_2_after, label = lab),
    inherit.aes = FALSE, size = 2.5
  ) +
  geom_text(
    data = tibble(
      value_1_before = 3,
      value_2_after = 1.2,
      lab = glue::glue("y={round(exp(d$`50%`), 1)}x")
    ),
    aes(value_1_before, value_2_after, label = lab),
    inherit.aes = FALSE, col = palette[6], size = 2.5
  ) +
  scale_x_log10() + 
  scale_y_log10() + 
  # scale_color_gradient(high = "black", low = "grey", labels = c(1:4, "Flushed"), trans = "reverse") +
  # scale_color_gradientn(colours = palette[-6], labels = c(1:4, "Flushed"), trans = "reverse") +
  theme(
    axis.title.x = ggtext::element_markdown(),
    axis.title.y = ggtext::element_markdown()
  ) +
  labs(
    x = "[Pb] (µg L<sup>-1</sup><br>at 1 mg PO<sub>4</sub> L<sup>-1</sup>\\)",
    y = "[Pb] (µg L<sup>-1</sup><br>at 2 mg PO<sub>4</sub> L<sup>-1</sup>\\)",
    col = "Profile litre"
  )

figure_2 <- wrap_plots(p1, p2, ncol = 2) +
  plot_annotation(tag_levels = "a") & 
  theme(legend.position = "right")

ggsave("figures/figure-2.png", figure_2, dev = "png", dpi = 600, width = 6, height = 2.75)
