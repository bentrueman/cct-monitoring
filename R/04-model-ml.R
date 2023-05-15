
# -------------------- setup --------------------

source(here::here("R/03-inform-prior-ml.R"))
library("glue")

model_in <- read_csv(here::here("data-clean/model-input.csv"))

# -------------------- inputs --------------------

seed <- 421508

bform <- bf(log(value) | cens(censored) ~ round + (1 | location) + (1 | location:litre))

# -------------------- prior --------------------

priors <- c(
  set_prior(glue("normal({avg}, {stdev})"), "b"), # see inform-prior-ml.R
  prior(normal(0, 1), Intercept),
  prior(cauchy(0, 1), sd),
  prior(normal(0, 1), sigma)
)

# -------------------- functions --------------------

fit_brm <- function(path, ...) {
  brm(
    bform,
    data = model_in,
    family = "student",
    cores = 4,
    seed = seed,
    iter = 4000,
    file = here::here(path),
    file_refit = "on_change",
    ...
  )
}

# -------------------- prior simulation --------------------

model_prior <- fit_brm(
  path = "models/prior",
  sample_prior = "only",
  prior = priors
)

# -------------------- models --------------------

model <- fit_brm(
  path = "models/model",
  prior = priors
)

model_noprior <- fit_brm(
  path = "models/model-noprior"
)

