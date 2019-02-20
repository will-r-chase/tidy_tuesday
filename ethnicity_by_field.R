#have to load fonts before ggplot
extrafont::loadfonts(device="win")

library(readxl)
library(tidyverse)
#devtools::install_github("gadenbuie/ggpomological")
library(ggpomological)

data <- read_xlsx("data/ethnicity_by_field.xlsx", skip = 3) %>%
  select(field, ethnicity, phds = "2017") %>%
  filter(ethnicity != "Not Hispanic or Latino")

totals <-
  data %>%
  filter(ethnicity == "total") %>%
  select(field, total = phds)

data_percent <- 
  data %>%
  right_join(., totals, by = "field") %>%
  mutate(percent = round(phds/total*100, 2)) %>%
  filter(ethnicity != "total") %>%
  mutate(ethnicity = ifelse(ethnicity == "Other race or race not reported", "Other or not reported", ethnicity),
         field = case_when(
           field == "Mathematics and computer sciences" ~ "Math and computer sciences",
           field == "Physical sciences and earth sciences" ~ "Physical and earth sciences",
           TRUE ~ field
         ))

ggplot(data_percent, aes(x = field, y = percent, fill = ethnicity)) +
  #stacked bar
  geom_bar(stat = "identity") +
  #design stuff
  scale_fill_manual("Ethnicity", values = c("#828585", "#4f5157", "#fd8f24", "#c03728", "#f5c04a", "#e68c7c", "#6f5438", "#919c4c")) +
  coord_flip() +
  theme_pomological(base_family="Lato", base_size = 14) +
  theme(plot.subtitle = element_text(face = "italic", size = 14), 
        plot.title = element_text(size = 18),
        plot.caption = element_text(face = "italic", size = 8, vjust = 0.5, hjust = 1)) +
  labs(x = "", y = "Percent", title = "PhDs awarded by ethnicity in 2017", subtitle = "That's pretty depressing... please do better", caption = "Graphic: @W_R_Chase\nData: NCSES")

ggsave("ethnicity_by_field.png", device = "png", type = "cairo", height = 5, width = 10)
