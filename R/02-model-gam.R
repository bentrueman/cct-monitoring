
# --------------------- setup ---------------------

library("brms")
library("bgamcar1")
library("tidyverse")
library("lubridate")

options(mc.cores = parallel::detectCores())

# -------------------- functions --------------------

make_model_in <- function(x, ...) {
  x %>% 
    group_by(zone, rack, pipe_number) %>% 
    mutate(
      date_yday = yday(date) / if_else(leap_year(date), 366, 365),
      d_x = date_numeric - lag(date_numeric),
      d_x = replace_na(d_x, 0),
      d_x = d_x / 27, # normalize to most common time difference
      ...
    ) %>% 
    ungroup() 
}

# --------------------- read ---------------------

data <- read_csv(here::here("data-clean/model-in-gam.csv")) %>% 
  # add d_x variable
  make_model_in(log_value = log(value))

data_full <- filter(data, date <= "2022-05-16") # for Zone 1 and 2 model
data_z1 <- filter(data, zone == 1) # for Zone 1 model

# --------------------- inputs ---------------------

priors <- c(
  prior(student_t(3, 0, 2.5), b),
  prior(normal(.5, .25), ar, lb = 0, ub = 1)
)

# --------------------- fit ---------------------

model_gam <- fit_stan_model(
  file = here::here("models/gam-brms"),
  seed = 2124536,
  bform = bf(
    log_value ~ 
      1 + (1 | series) +
      s(date_numeric, k = 6) +
      s(date_numeric, by = location, m = 1) +
      s(date_numeric, by = series, m = 1) + 
      ar(time = date_numeric, gr = series)
  ),
  bdata = data_full,
  bpriors = priors,
  control = list(adapt_delta = .9),
  save_warmup = FALSE
)

model_gam_z1 <- fit_stan_model(
  file = here::here("models/gam-brms-z1"),
  seed = 2124536,
  bform = bf(
    log_value ~ 
      s(date_numeric, k = 15) +
      s(date_yday, bs = "cc") +
      s(date_numeric, series, bs = "fs", m = 1) +
      ar(time = date_numeric, gr = series)
  ),
  bdata = data_z1,
  bpriors = priors,
  knots = list(date_yday = c(0, 1)),
  control = list(adapt_delta = .9),
  save_warmup = FALSE
)
