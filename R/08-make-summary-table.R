
# make water quality summary tables for paper.Rmd:

# --------------------- setup ---------------------

library("tidyverse")
library("assertr")

wq <- read_csv("data-clean/data-wq.csv") %>% 
  mutate(
    param = case_when(
      param == "orthophosphate" & date_numeric >= 532 & zone == 1 ~ "orthophosphate (phase 3)", # 1.5 mg PO4/L
      param == "orthophosphate" & date_numeric >= 0 ~ "orthophosphate (phase 2)", # 2 mg PO4/L
      param == "orthophosphate" & date_numeric < 0 ~ "orthophosphate (phase 1)", # 1 mg PO4/L
      TRUE ~ param
    )
  )

hardness <- read_csv("data-clean/hardness-z1-z2.csv") %>%
  transmute(
    zone, date,
    value,
    unit = paste(units, str_extract(param, "(?<=\\().+(?=\\))")),
    param = str_extract(param, ".*?(?=\\s)"),
    censored = "none"
  )

input <- read_csv("data-clean/summary-table-input.csv") # get units formatted for .Rmd 

params_together <- c("dissolved inorganic carbon", "ph", "free cl", 
                     "organic carbon", "dissolved chloride", "dissolved sulphate",
                     "total alkalinity", "orthophosphate (phase 1)", "Hardness",
                     "orthophosphate (phase 2)", "orthophosphate (phase 3)")

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
  bind_rows(hardness) %>% 
  filter(param %in% params_together) %>%
  summarize_wq(zone) %>% 
  left_join(input) %>% 
  arrange(str_to_lower(param_table), zone) %>% 
  select(param = param_table, zone, med, lq, uq, unit = unit_table)

# --------------------- summary table for supplement (separate zones) ---------------------

summary_si <- wq %>% 
  summarize_wq(zone) %>% 
  filter(!param %in% c(remove_these, params_together)) %>% 
  left_join(input) %>% 
  select(param = param_table, zone, med, lq, uq, unit = unit_table)

# --------------------- write ---------------------

write_csv(summary, "data-clean/summary-table.csv")
write_csv(summary_si, "data-clean/summary-table-si.csv")
