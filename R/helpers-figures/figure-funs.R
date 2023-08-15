
library("testthat")

# --------------------- functions ---------------------

summarize_slopes <- function(x, width = .95, ...) {
  lwr <- (1 - width) / 2
  upr <- 1 - (1 - width) / 2
  x %>% 
    group_by(date_numeric, ...) %>% 
    summarize(
      across(
        c(slope, smooth),
        .fns = list(
          estimate = median,
          lower = ~ quantile(.x, lwr),
          upper = ~ quantile(.x, upr)
        )
      )
    ) %>% 
    ungroup()
}

test_that("summarize_slopes returns expected quantiles", {
  x <- 1:100
  out <- tibble(
    date_numeric = 1,
    slope = x,
    smooth = x - 1,
  ) %>% 
    summarize_slopes()
  expect_equal(out$slope_estimate, median(x))
  expect_equal(out$slope_lower, quantile(x, .025))
  expect_equal(out$slope_upper, quantile(x, .975))
  expect_equal(out$smooth_estimate, median(x - 1))
  expect_equal(out$smooth_lower, quantile(x - 1, .025))
  expect_equal(out$smooth_upper, quantile(x - 1, .975))
})

add_label <- function(labels, ...) {
  geom_richtext(
    data = labels,
    aes(x = x, y = y, label = lab),
    size = 2.5,
    label.r = unit(0, "cm"),
    label.size = 0, 
    alpha = .9,
    inherit.aes = FALSE,
    vjust = "inward",
    ...
  )
}

add_ribbon <- function(d, l, u, ...) {
  summ_args <- enquos(...)
  geom_ribbon(
    data = . %>%
      reframe(x = range(!!!summ_args)) %>%
      mutate(
        ymin = exp(as.numeric(d[, l])) * x,
        ymax = exp(as.numeric(d[, u])) * x
      ),
    aes(x = x, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE,
    fill = palette[6], col = NA, alpha = .3
  )
}

add_segment <- function(d, ...) {
  summ_args <- enquos(...)
  geom_segment(
    data = . %>% 
      summarize(
        x = min(!!!summ_args),
        xend = max(!!!summ_args)
      ) %>% 
      mutate(
        y = exp(d$`50%`) * x,
        yend = exp(d$`50%`) * xend
      ),
    aes(x = x, xend = xend, y = y, yend = yend),
    col = palette[6], inherit.aes = FALSE
  )
}

add_rug <- function(...) {
  geom_rug(
    data = data_z1,
    aes(...),
    inherit.aes = FALSE
  )
}

# plot XRD data:

gg_xrd <- function(pipe, layers = 3, phases, data, standards) {
  
  panels <- layers + length(phases)
  
  layer_names <- paste0("L", seq_len(layers)) %>% 
    # pad layer names with NAs to reach length of 4
    c(rep(NA_character_, 4 - layers))
  
  phase_names <- c(phases, rep(NA_character_, 6 - length(phases)))
  
  xrd_list <- list(
    filter(data, code == pipe, layer == layer_names[1]),
    filter(data, code == pipe, layer == layer_names[2]),
    filter(data, code == pipe, layer == layer_names[3]),
    filter(data, code == pipe, layer == layer_names[4]),
    filter(standards, phase == phases[1]),
    filter(standards, phase == phases[2]),
    filter(standards, phase == phases[3]),
    filter(standards, phase == phases[4]),
    filter(standards, phase == phases[5]),
    filter(standards, phase == phases[6])
  )
  
  xrd_list <- xrd_list[map_lgl(xrd_list, ~ nrow(.x) > 0)]
  
  xrd_list %>% 
    set_names(letters[1:panels]) %>% 
    bind_rows(.id = "facet") %>% 
    ggplot(aes(two_theta, intensity)) + 
    facet_wrap(~ facet, ncol = 1, scales = "free_y") + 
    geom_line(data = function(data) data %>% filter(is.na(phase)), linewidth = .2) + 
    geom_segment(
      data = function(data) data %>% filter(!is.na(phase)),
      aes(x = two_theta, xend = two_theta, y = 0, yend = intensity), 
      col = "deepskyblue4", linewidth = .25
    ) +
    geom_text(
      data = function(data) data %>% 
        filter(!is.na(phase)) %>% 
        distinct(phase, facet),
      aes(x = 55, y = .75, label = phase), 
      col = "deepskyblue4", size = 3
    ) +
    geom_text(
      data = function(data) data %>% 
        filter(is.na(phase)) %>% 
        group_by(phase, facet, layer) %>% 
        summarize(y = 1.1 * intensity[which.max(two_theta)]),
      aes(x = 73, y = y, label = layer), size = 2.5
    ) +
    labs(
      x = expression("2"*theta~"(Cu K"*alpha*")"),
      y = NULL,
      col = NULL
    ) + 
    theme(
      strip.text = element_blank(),
      axis.text.y = element_blank(),
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "none"
    ) +
    geom_rug(
      data = function(data) data %>% 
        with(tibble(two_theta = seq(10, 70, by = 5))) %>% 
        mutate(facet = letters[panels]), 
      aes(x = two_theta),
      length = unit(.04, "cm"),
      sides = "b", outside = TRUE, inherit.aes = FALSE
    ) + 
    coord_cartesian(clip = "off")
}
