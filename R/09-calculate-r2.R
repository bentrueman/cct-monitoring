
# -------------------- setup --------------------

source("R/04-model-ml.R")
library("ggdist")
library("testthat")

# -------------------- function --------------------

# see https://avehtari.github.io/bayes_R2/bayes_R2.html

# this definition of Bayesian R2 is applicable to censored models:

bayes_r2 <- function(fit, ...) {
  mupred <- posterior_epred(fit, ...)
  var_mupred <- apply(mupred, 1, var)
  sigma <- as_draws_df(fit, "sigma")$sigma
  mean_qi(var_mupred / (var_mupred + sigma ^ 2))
}

# -------------------- ml --------------------

r2_ml <- bayes_r2(model)

# -------------------- gam --------------------

r2_gam <- bayes_r2(model_gam, incl_autocor = FALSE)

# -------------------- write --------------------

# write summary to file:

# bind_rows("gam" = r2_gam, "ml" = r2_ml, .id = "model") %>% 
#   mutate(
#     across(starts_with("y"), ~ round(.x, 3)),
#     ci = paste(ymin, ymax, sep = "--")
#   ) %>% 
#   pivot_wider(
#     id_cols = starts_with("."), 
#     names_from = model, values_from = c(y, ci)
#   ) %>% 
#   write_csv("data-clean/r2-compare.csv")
