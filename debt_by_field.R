#have to load fonts before ggplot
extrafont::loadfonts(device="win")

library(readxl)
library(tidyverse)
#devtools::install_github("gadenbuie/ggpomological")
library(ggpomological)

#read in data, fix names
data <- read_xlsx("data/debt_by_field_2.xlsx") %>%
  select(-contains("Number"))
colnames(data)[1] <- "debt"
amounts <- c(0, 5000, 15000, 25000, 35000, 45000, 55000, 65000, 75000, 85000, 95000)

data_long <- 
  data %>%
  mutate(debt = amounts) %>%
  gather(key = field, value = percent, -debt) %>%
  mutate(field = case_when(
           field == "Mathematics and computer sciences" ~ "Math and computer sciences",
           field == "Physical sciences and earth sciences" ~ "Physical and earth sciences",
           TRUE ~ field
         ), amount = percent*debt) %>%
  group_by(field) %>%
  summarize(average = sum(amount)/100)

paste0("Sorry non STEM people ", emo::ji("shrug"))

ggplot(data_long, aes(x = reorder(field, average), y = average, fill = field)) +
  geom_bar(stat = "identity") +
  geom_text(aes(x = field, y = average, label = scales::dollar(average)), color = "#141414", nudge_y = 2350) +
  scale_fill_manual(values = c("#828585", "#4f5157", "#fd8f24", "#c03728", "#f5c04a", "#e68c7c", "#919c4c", "#6f5438"), guide = FALSE) +
  coord_flip() +
  ylim(0, 35000) +
  theme_pomological(base_family="Lato", base_size = 14) +
  theme(plot.subtitle = element_text(face = "italic", size = 14), 
        plot.title = element_text(size = 18),
        plot.caption = element_text(face = "italic", size = 8, vjust = 0.5, hjust = 1)) +
  labs(x = "Field", y = "Debt incurred (dollars)", title = "Average debt incurred from PhD expenses", subtitle = "Sorry non STEM people...", caption = "Graphic: @W_R_Chase\nData: NCSES")

ggsave("debt_by_field.png", device = "png", type = "cairo", height = 6, width = 8)


data2 <- read_xlsx("data/debt_by_ethnicity.xlsx", skip = 3)
colnames(data2)[1] <- "type"

data2_long <- 
  data2 %>%
  filter(type == "Graduate debt") %>%
  select(-type) %>%
  gather(key = eth, value = debt)

ggplot(data2_long, aes(x = reorder(eth, debt), y = debt, fill = eth)) +
  geom_bar(stat = "identity") +
  geom_text(aes(x = eth, y = debt, label = scales::dollar(debt)), color = "#141414", nudge_y = 5000) +
  scale_fill_manual(values = c("#828585", "#4f5157", "#fd8f24", "#c03728", "#f5c04a", "#e68c7c", "#919c4c", "#6f5438", "#c3c377", "#BFB8CE"), guide = FALSE) +
  coord_flip() +
  ylim(0, 75000) +
  theme_pomological(base_family="Lato", base_size = 14) +
  theme(plot.subtitle = element_text(face = "italic", size = 14), 
        plot.title = element_text(size = 18),
        plot.caption = element_text(face = "italic", size = 8, vjust = 0.5, hjust = 1)) +
  labs(x = "Demographic", y = "Debt incurred (dollars)", title = "Average debt incurred from PhD expenses", subtitle = "The effect of minorities receiving fewer assistantships", caption = "Graphic: @W_R_Chase\nData: NCSES")

ggsave("debt_by_ethnicity.png", device = "png", type = "cairo", height = 6, width = 8)

