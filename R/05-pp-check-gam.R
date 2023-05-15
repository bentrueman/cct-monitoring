
source("R/02-model-gam.R")
library("janitor")
library("posterior")
library("testthat")

# -------------------- pp-check --------------------

# full:

pp_check(model_gam, "ecdf_overlay", ndraws = 100) + 
  coord_cartesian(xlim = c(-5, 10))

# zone 1:

pp_check(model_gam_z1, "ecdf_overlay", ndraws = 100) + 
  coord_cartesian(xlim = c(-5, 10))

# -------------------- autocorrelation --------------------

# full:

resid <- bind_cols(data_full, residuals(model_gam, incl_autocor = FALSE)) %>% 
  clean_names()

resid %>% 
  with(acf(estimate))

# zone 1:

resid_z1 <- bind_cols(data_z1, residuals(model_gam_z1, incl_autocor = FALSE)) %>% 
  clean_names()

resid_z1 %>% 
  with(acf(estimate))

# -------------------- plot residuals --------------------

# full:

resid %>% 
  rowid_to_column() %>% 
  ggplot(aes(rowid, estimate)) +
  geom_point()

params <- c("df" = median(as_draws_df(model_gam, "nu")$nu))

resid %>% 
  rowid_to_column() %>% 
  ggplot(aes(sample = estimate)) +
  stat_qq(distribution = brms::qstudent_t, dparams = params) +
  stat_qq_line(distribution = brms::qstudent_t, dparams = params)

resid %>% 
  rowid_to_column() %>% 
  ggplot(aes(estimate)) +
  geom_histogram()

# zone 1:

resid_z1 %>% 
  rowid_to_column() %>% 
  ggplot(aes(rowid, estimate)) +
  geom_point()

params_z1 <- c("df" = median(as_draws_df(model_gam_z1, "nu")$nu))

resid_z1 %>% 
  rowid_to_column() %>% 
  ggplot(aes(sample = estimate)) +
  stat_qq(distribution = brms::qstudent_t, dparams = params_z1) +
  stat_qq_line(distribution = brms::qstudent_t, dparams = params_z1)

resid_z1 %>% 
  rowid_to_column() %>% 
  ggplot(aes(estimate)) +
  geom_histogram()

# -------------------- loo --------------------

model_loo <- loo_cv(data_full, model_gam, censoring = FALSE) # full
model_loo_z1 <- loo_cv(data_z1, model_gam_z1, censoring = FALSE) # zone 1

# -------------------- diagnostic stats --------------------

# full:

draws <- as_draws_df(model_gam) %>% 
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

# zone 1:

draws_z1 <- as_draws_df(model_gam_z1) %>% 
  summarize_draws()

test_that("rhats are all less than 1.05", {
  expect_equal(mean(draws_z1$rhat < 1.05), 1)
})

test_that("ess_bulk are all greater than 400", {
  expect_equal(mean(draws_z1$ess_bulk > 400), 1)
})

test_that("ess_tail are all greater than 400", {
  expect_equal(mean(draws_z1$ess_tail > 400), 1)
})
