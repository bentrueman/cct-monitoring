
source("R/04-model-ml.R")
library("janitor")
library("bayesplot")
library("ggdist")
library("posterior")
library("testthat")

# -------------------- prior simulation --------------------

post_prior <- withr::with_seed(1538, {posterior_predict(model_prior, ndraws = 50)})

post_prior_cens <- post_prior %>% 
  apply(2, function(x) if_else(x < log(.5), log(.5), x))

post_prior_cens %>% 
  t() %>% 
  data.frame() %>% 
  pivot_longer(everything()) %>% 
  ggplot(aes(value, group = name)) + 
  geom_density() + 
  coord_cartesian(xlim = c(-1, 10))

# -------------------- prior sensitivity --------------------

list(
  "prior (sim)" = model_prior,
  "posterior" = model,
  "flat prior" = model_noprior
) %>% 
  map_dfr(
    ~ as_draws_df(.x, "b_round2_after") %>% 
      as_tibble(),
    .id = "type"
  ) %>% 
  ggplot(aes(exp(b_round2_after), col = type, fill = type)) + 
  stat_halfeye(slab_alpha = .5, position = position_dodge(width = .2)) + 
  scale_y_continuous(expand = expansion(add = c(0, .2)))

# -------------------- autocorrelation --------------------

resid <- bind_cols(model_in, residuals(model)) %>% 
  clean_names()

resid %>% 
  filter(censored != "left") %>% 
  with(acf(estimate))

# -------------------- plot residuals --------------------

resid %>% 
  filter(censored != "left") %>% 
  rowid_to_column() %>% 
  ggplot(aes(rowid, estimate, col = location, shape = censored)) +
  geom_point()

resid %>% 
  filter(censored != "left") %>% 
  rowid_to_column() %>% 
  ggplot(aes(sample = estimate)) +
  geom_qq() +
  geom_qq_line()

resid %>% 
  filter(censored != "left") %>% 
  rowid_to_column() %>% 
  ggplot(aes(estimate)) +
  geom_histogram()

# -------------------- loo --------------------

model_loo <- loo(model)

# -------------------- pp check --------------------

post <- withr::with_seed(158, {posterior_predict(model, ndraws = 500)})
post_cens <- apply(post, 2, function(x) if_else(x < log(.5), log(.5), x))

ppc_dens_overlay(log(model_in$value), post_cens) +
  coord_cartesian(xlim = c(0, 5))

# does the proportion censored in the data match the proportion censored in the posterior draws?

ppc_stat(1 * (model_in$censored == "left"), 1 * (post < log(.5)), binwidth = .025)

# -------------------- diagnostic stats --------------------

draws <- as_draws_df(model) %>% 
  summarize_draws()

test_that("rhats are all less than 1.05", {
  expect_equal(mean(draws$rhat < 1.05), 1)
})

test_that("ess_bulk are all greater than 400", {
  expect_equal(mean(draws$ess_bulk > 400), 1)
})

test_that("ess_tail are all greater than 400", {
  expect_equal(mean(draws$ess_tail > 400), 1)
})
