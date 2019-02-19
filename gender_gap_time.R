#have to load fonts before ggplot
extrafont::loadfonts(device="win")

library(readxl)
library(tidyverse)
#devtools::install_github("gadenbuie/ggpomological")
library(ggpomological)
library(ggalt)

#read in data
data <- read_xlsx("data/sex_by_field.xlsx", skip = 3)

#get only overall data
data_all_fields <- 
  data %>%
  gather(key = year, value = percent, -field, -sex) %>%
  filter(field == "All fields") %>%
  spread(key = sex, value = percent)

#relevel for plot order
data_all_fields$year <- fct_relevel(data_all_fields$year, "2017", "2012", "2007", "2002", "1997", "1992", "1987")

#plot w/ ggalt
ggplot(data_all_fields) +
  #dumbbells
  geom_dumbbell(aes(y = year, x = Female, xend = Male), colour_x = "#919c4c", colour_xend = "#f5c04a", size = 1, size_x = 5, size_xend = 5) +
  xlim(0, 100) +
  #labels
  geom_text(aes(x = 35, y = "1987", label = "Female"), color = "#919c4c", size = 5, family = "Lato", vjust = -1) +
  geom_text(aes(x = 66, y = "1987", label = "Male"), color = "#f5c04a", size = 5, family = "Lato", vjust = -1) +
  #design stuff
  theme_pomological(base_family="Lato", base_size = 14) +
  theme(plot.subtitle = element_text(face = "italic", size = 14), 
        plot.title = element_text(size = 18),
        plot.caption = element_text(face = "italic", size = 8, vjust = 0.5, hjust = 1)) +
  labs(x = "Percent", y = "Year", title = "PhDs awarded by sex", subtitle = "The gender gap is closing", caption = "Graphic: @W_R_Chase\nData: NCSES")

ggsave("gender_gap_time.png", device = "png", type = "cairo", width = 5, height = 5)
