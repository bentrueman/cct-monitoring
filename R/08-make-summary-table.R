
# make water quality summary tables for paper.Rmd:

# --------------------- setup ---------------------

library("tidyverse")
library("assertr")

wq <- read_csv("data-clean/data-wq.csv") %>% 
  mutate(
    # phase 1 == 1 ppm PO4, phase 2 == 2 ppm PO4:
    param = case_when(
      param == "orthophosphate" & date_numeric < 0 ~ "orthophosphate (phase 1)", 
      param == "orthophosphate" & date_numeric >= 0 ~ "orthophosphate (phase 2)",
      TRUE ~ param
    )
  )

input <- read_csv("data-clean/summary-table-input.csv")

params_together <- c("dissolved inorganic carbon", "ph", "free cl", 
                     "organic carbon", "dissolved chloride", "turb")

remove_these <- c("collected volume", "do %")

# --------------------- functions ---------------------

summarize_wq <- function(x, ...) {
  x %>% 
    group_by(param, unit, ...) %>% 
    summarize(
      med = median(value),
      lq = quantile(value, 0.25),
      uq = quantile(value, 0.75),
      pcens = mean(censored != "none")
    ) %>% 
    ungroup() %>% 
    verify(pcens < .25)
}

# --------------------- summary table for paper ---------------------

summary <- wq %>% 
  filter(param %in% params_together) %>%
  summarize_wq() %>% 
  left_join(input) %>% 
  select(param = param_table, med, lq, uq, unit = unit_table)

# --------------------- summary table for supplement (separate zones) ---------------------

summary_si <- wq %>% 
  summarize_wq(zone) %>% 
  filter(!param %in% remove_these) %>% 
  left_join(input) %>% 
  select(param = param_table, zone, med, lq, uq, unit = unit_table)

# --------------------- write ---------------------

write_csv(summary, "data-clean/summary-table.csv")
write_csv(summary_si, "data-clean/summary-table-si.csv")
  
  
