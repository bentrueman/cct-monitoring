
# run this script from "07-make-figures.R"

# --------------------- figure S6 ---------------------

figure_s6 <- wq %>%
  filter(param == "total aluminum", zone == 2, date < "2022-01-01") %>% 
  mutate(
    year = as.factor(year(date)),
    yday = yday(date) / if_else(leap_year(date), 366, 365),
    yday = 365 * yday + as.Date("2021-12-31")
  ) %>% 
  ggplot(aes(yday, value, col = year, group = interaction(series, year))) + 
  scale_x_date(date_labels = "%b") +
  scale_color_manual(values = palette[c(3,1,4,6)]) +
  geom_point() +
  geom_line() + 
  ylim(c(0, 210)) +
  labs(
    x = NULL,
    y = expression("[Al] (µg L"^-1*")"),
    col = NULL
  )

ggsave("figures/figure-s6.png", figure_s6, dev = "png", dpi = 600, width = 6.5, height = 3)
