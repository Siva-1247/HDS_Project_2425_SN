library(ggplot2)
library(dplyr)
vaccination_rate = read.csv("CDC47_Stats.csv")
vaccination_rate <- vaccination_rate %>%
  filter(Age.Group=='12 years and over')
vaccine_rate <- vaccination_rate %>% mutate(area_type = case_when(grepl('city|dublin', Local.Electoral.Area, ignore.case = TRUE) ~ 'City', TRUE ~ 'Rural'))
library(lubridate)
library(scales)
vaccine_rate$Month <- parse_date_time(vaccine_rate$Month, orders = "Y B")
library(nlme)
library(mgcv)
head(vaccine_rate)
vaccine_rate %>%
  filter(!is.na(Primary.Course.Completed....)) %>%
  ggplot(aes(x = Month, y = Primary.Course.Completed...., group = Local.Electoral.Area)) +
  geom_point(size = 2, alpha = 0.6, color = "#96CDCD") +
  geom_smooth(method = "gam", formula = y ~ s(x), se = FALSE, color = "#CD96CD", size = 0.8, alpha=0.2) +
  facet_wrap(~ area_type, scales = "free_y") +
  scale_x_datetime(date_breaks = "2 months", date_labels = "%b %Y") + 
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14)
  )+
  labs(
    title = "Primary Vaccination Course Completion Over Time",
    subtitle = "Grouped by Area Type (Urban/Rural)",
    x = "Month",
    y = "Primary Vaccination Completion Rate (%)"
  )
##Overall Primary course vaccination distributions
library(ggExtra)
plot_1<- ggplot(data = vaccine_rate, aes(x = Month, y = Primary.Course.Completed....)) +
  geom_point(size = 2, alpha = 0.6, color = "#CD96CD") +
  geom_smooth(aes(group = Local.Electoral.Area),method = "gam", formula = y ~ s(x), se = FALSE, color = "#CD96CD", size = 0.8, alpha=0.2)+
  scale_x_datetime(date_breaks = "2 months", date_labels = "%b %Y") +
  xlab("Month") + 
  ylab("Primary Vaccination Completion Rate (%)")
ggExtra::ggMarginal(plot_1, fill = "#EEB4B4",type="densigram",margins = "y")

