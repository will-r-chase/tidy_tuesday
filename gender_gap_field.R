#have to load fonts before ggplot
extrafont::loadfonts(device="win")

library(readxl)
library(tidyverse)
#devtools::install_github("gadenbuie/ggpomological")
library(ggpomological)
library(ggalt)

#read in data
data <- read_xlsx("data/sex_by_field.xlsx", skip = 3) %>%
  filter(field != "All fields")

data_change <- 
  data %>%
  gather(key = year, value = percent, -field, -sex) %>%
  spread(key = sex, value = percent) %>%
  mutate(field = case_when(
    field == "Mathematics and computer sciences" ~ "Math and computer sciences",
    field == "Physical sciences and earth sciences" ~ "Physical and earth sciences",
    TRUE ~ field
  )) %>%
  group_by(field, year) %>%
  mutate(change = round(Male - Female, 2), label = ifelse(change > 0, paste0("+", change, "%"), paste0(change, "%")))

data_change$field <- fct_relevel(data_change$field, "Education", "Psychology and social sciences", "Life sciences",
                                 "Other", "Humanities and arts", "Physical and earth sciences", 
                                 "Math and computer sciences", "Engineering")
                                
ggplot(data_change, aes(x = year, y = field, fill = change))+
  geom_tile(color = "#FFFEEA") +
  scale_fill_gradient2(low = "#919c4c", high = "#f5c04a", mid = "white", 
                      space = "Lab", 
                       name="Gender imbalance (%)", guide = FALSE) +
  theme_pomological(base_family="Lato", base_size = 14) +
  geom_text(aes(x = year, y = field, label = label), color = "black", size = 4) +
  theme(plot.subtitle = element_text(face = "italic", size = 14), 
        plot.title = element_text(size = 18),
        plot.caption = element_text(face = "italic", size = 8, vjust = 0.5, hjust = 1)) +
  labs(x = "Year", y = "Field", title = str_wrap("Gender imbalance of PhDs awarded to men compared to women", 35), caption = "Graphic: @W_R_Chase\nData: NCSES") +
  coord_fixed()

ggsave("gender_gap_by_field.png", device = "png", type = "cairo", width = 8, height = 7)
