
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