
# inform the prior for the sentinel site model, 
# and generate difference estimates for paper.Rmd

# --------------------- setup ---------------------

source(here::here("R/02-model-gam.R"))
source(here::here("R/helpers-figures/analysis-ph.R"))
library("ggdist")
library("testthat")

tol <- 30 # window in days in which to consider data for tests on difference estimates

# --------------------- predict ---------------------

###################################################################################
# how much is lead predicted to decrease in the first year after the dose increase?

newdata <- tibble(date_numeric = c(0, 365))

# predict from global term of GAM:

post <- posterior_smooths(model_gam, "s(date_numeric,k=6)", newdata = newdata)

d <- post[,2] - post[,1]

# for ml prior:

avg <- round(mean(d), 1)
stdev <- round(sd(d), 1)

# (for paper.Rmd)

pcent_decrease <- median_qi(d) %>% 
  mutate(across(starts_with("y"), ~ 100 * (exp(.x) - 1)))

test_that("percent decrease is reasonable", {
  
  before <- data_full %>%
    filter(
      date_numeric > -tol,
      date_numeric < 0
    ) %>%
    with(mean(value))
  
  after <- data_full %>%
    filter(abs(date_numeric - 365) < tol) %>%
    with(mean(value))
  
  d <- 100 * (after - before) / before
  
  expect_true(d > pcent_decrease$ymin) 
  expect_true(d < pcent_decrease$ymax)
  
})

############################################################################
# how much is lead predicted to decrease due to the coagulation pH increase?

# for paper.Rmd:

newdata2 <- tibble(
  date_numeric = c(ph_increase, 0),
  location = "z2.r1",
  series = NA
)

post2 <- posterior_smooths(
  model_gam, 
  "s(date_numeric,by=location,m=1)", 
  newdata = newdata2[, c("date_numeric", "location")]
)

d2 <- post2[,2] - post2[,1]

pcent_decrease2 <- ggdist::median_qi(d2) %>% 
  mutate(across(starts_with("y"), ~ 100 * (exp(.x) - 1)))

test_that("percent decrease is reasonable", {
  
  before <- data_full %>%
    filter(
      date_numeric - ph_increase > -tol, 
      date_numeric < ph_increase, 
      location == "z2.r1"
    ) %>%
    with(mean(value))
  
  after <- data %>%
    filter(
      date_numeric > 0, 
      date_numeric < tol, 
      location == "z2.r1"
    ) %>%
    with(mean(value))
  
  d <- 100 * (after - before) / before
  
  expect_true(d > pcent_decrease2$ymin) 
  expect_true(d < pcent_decrease2$ymax)
  
})

#############################################################################################
# how much is lead predicted to increase due to orthophosphate dose decrease to 1.5 mg PO4/L?

p_inc_z1 <- as.Date("2020-11-30") # Zone 1 increase
p_dec_z1 <- as.Date("2022-05-16") # Zone 1 P decrease, 2--1.5 mg PO4/L 
# maximum of global trend after P decrease:
final_z1 <- data_z1 %>% 
  mutate(pp = apply(posterior_smooths(model_gam_z1, "s(date_numeric,k=15)"), 2, median)) %>% 
  filter(date > p_dec_z1) %>% 
  with(date[which.max(pp)])

# how much is lead predicted to decrease in the first year after the dose increase?

newdata3 <- tibble(date_numeric = c(
  # convert to numeric dates:
  as.numeric(p_dec_z1 - p_inc_z1), 
  as.numeric(final_z1 - p_inc_z1)
))

# predict from global term of GAM:

post3 <- posterior_smooths(model_gam_z1, "s(date_numeric,k=15)", newdata = newdata3)

d3 <- post3[,2] - post3[,1]

pcent_increase <- ggdist::median_qi(d3) %>% 
  mutate(across(starts_with("y"), ~ 100 * (exp(.x) - 1)))

test_that("percent increase is reasonable", {
  
  before <- data_z1 %>% 
    filter(as.numeric(date - p_dec_z1) > -30) %>%
    with(mean(value))
  
  after <- data_z1 %>%
    filter(as.numeric(date - final_z1) < 30) %>%
    with(mean(value))
  
  d <- 100 * (after - before) / before
  
  expect_true(d > pcent_increase$ymin) 
  expect_true(d < pcent_increase$ymax)
  
})
