
#-------- dose increases ----------

p_inc_pw <- as.Date("2021-07-29") # Zone 2

#-------- identify pH increase ----------

pidat <- read_csv(here::here("data-clean/pi-process-data.csv"))

ph_increase <- pidat %>% 
  mutate(ph = stats::filter(ph, rep(1/3, 3), method = "convolution")) %>% 
  filter(ph >= 6.3, date > "2021-01-01") %>% 
  summarize(date = difftime(min(date), p_inc_pw)) %>% 
  pull(date) %>% 
  as.numeric()
