#have to load fonts before ggplot
extrafont::loadfonts(device="win")

library(readxl)
library(tidyverse)
library(ggalluvial)
#devtools::install_github("gadenbuie/ggpomological")
library(ggpomological)

#read in data, fix names
data <- read_xlsx("data/field_parents_education.xlsx") 

data_clean <-
  data %>%
  gather(key = education, value = percent, -field, -parent) %>%
  filter(!is.na(percent)) %>%
  group_by(field, education) %>%
  summarize(mean = mean(percent)) %>%
  ungroup() %>%
  mutate(field = case_when(
    field == "Mathematics and computer sciences" ~ "Math and computer sciences",
    field == "Physical sciences and earth sciences" ~ "Physical and earth sciences",
    TRUE ~ field
  )) %>%
  mutate(field = as.factor(field), education = as.factor(education))

data_clean$education <- fct_collapse(data_clean$education, "Some college, high school, or less" = c("Some college", "High school or less"))

data_clean <- 
  data_clean%>%
  mutate(field = str_wrap(data_clean$field, 18), education = str_wrap(data_clean$education, 18))

ggplot(as.data.frame(data_clean),
       aes(y = mean, axis1 = field, axis2 = education)) +
  geom_alluvium(aes(fill = education), width = 1/12, alpha = 0.7) +
  geom_stratum(width = 1/7, fill = "#efe1c6", color = "white", size = 1) +
  geom_text(stat = "stratum", label.strata = TRUE, family = "Lato", hjust = "left", nudge_x = -0.065) +
  scale_x_discrete(limits = c(str_wrap("PhD field of study", 18), str_wrap("Parent's educational attainment", 18)), expand = c(.05, .05)) +
  scale_fill_manual(values = c("#c03728", "#f5c04a", "#0C1E43"), guide = FALSE) +
  theme_pomological(base_family="Lato", base_size = 14) +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.text.x = element_text(size = 14, hjust = 0.5)) +
  theme(plot.subtitle = element_text(face = "italic", size = 14), 
        plot.title = element_text(size = 18),
        plot.caption = element_text(face = "italic", size = 8.5, vjust = 0.5, hjust = 1)) +
  labs(x = "", y = "", title = "Parent's educational attainment of PhD graduates by field of study", subtitle = "Humanities students are most likely to have a parent with an advanced degree", caption = "Graphic: @W_R_Chase\nData: NCSES")

ggsave("field_parents_education.png", device = "png", type = "cairo", height = 8, width = 14)
