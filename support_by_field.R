#have to load fonts before ggplot
extrafont::loadfonts(device="win")

library(readxl)
library(tidyverse)
#devtools::install_github("gadenbuie/ggpomological")
library(ggpomological)

#read in data, fix names
data <- read_xlsx("data/support_by_field.xlsx", skip = 3) 

#first analyze sex-field-support relationship
data_sex <- 
  data %>%
  filter(field == "All fields") %>%
  select(field, support, male = Male, female = Female) %>%
  mutate(label_male = paste0(male, "%"), 
         label_female = paste0(female, "%"), 
         pos = male - female,
         male_nudge = ifelse(pos >= 0, 2.2, -2.2), 
         female_nudge = ifelse(pos >= 0, -2.2, 2.2)) 

data_sex$support <- fct_relevel(data_sex$support, "Other", "Employer", "Own resources", "Fellowships, scholarships, or dissertation grants", "Teaching assistantships", "Research assistantships or traineeships")

ggplot(data_sex) +
  #dumbbells
  geom_segment(aes(y = support, yend = support, x = male, xend = female), color = "black", size = 1) +
  geom_point(aes(y = support, x = female), color = "#919c4c", size = 10, shape = "|") +
  geom_point(aes(y = support, x = male), color = "#f5c04a", size = 10, shape = "|") +
  #percent labels
  geom_text(aes(y = support, x = female, label = label_female), color = "#919c4c", nudge_x = data_sex$female_nudge, family = "Lato") +
  geom_text(aes(y = support, x = male, label = label_male), color = "#f5c04a", nudge_x = data_sex$male_nudge, family = "Lato") +
  scale_x_continuous(limits = c(-2, 50), breaks = c(0, 10, 20, 30, 40, 50)) +
  scale_y_discrete(expand = expand_scale(mult = c(0.1, 0.2))) +
  #labels
  geom_text(aes(x = 24.3, y = "Research assistantships or traineeships", label = "Female"), color = "#919c4c", size = 4.3, family = "Lato", vjust = -2) +
  geom_text(aes(x = 40.6, y = "Research assistantships or traineeships", label = "Male"), color = "#f5c04a", size = 4.3, family = "Lato", vjust = -2) +
  #design stuff
  theme_pomological(base_family="Lato", base_size = 14) +
  theme(plot.subtitle = element_text(face = "italic", size = 14), 
        plot.title = element_text(size = 18),
        plot.caption = element_text(face = "italic", size = 8, vjust = 0.5, hjust = 1)) +
  labs(x = "Percent", y = "Primary Source of Support", title = "Gender gap in primary funding source for PhDs", subtitle = str_wrap("Compared to men, women receive fewer RA's and use more of their own resources to fund their PhDs", 50), caption = "Graphic: @W_R_Chase\nData: NCSES")

ggsave("gender_gap_funding.png", device = "png", type = "cairo", width = 10, height = 5)


#ethnicity by funding
data_eth <- 
  data %>%
  filter(field == "All fields") %>%
  select(field, support, International = "Temporary visa holder", 8:14) %>%
  gather(key = eth, value = percent, -field, -support) %>%
  select(-field) %>%
  mutate(eth = ifelse(eth == "Other race or race not reported", "Other or not reported", eth),
         percent = round(as.numeric(percent), 1),
         label = paste0(percent, "%"), 
         label = case_when(
           percent < 6 ~ "",
           TRUE ~ label
         ))

#plot
ggplot(data_eth, aes(x = eth, y = percent, fill = support)) +
  #stacked bar
  geom_bar(stat = "identity") +
  geom_text(aes(x = eth, y = percent, label = label), position = position_stack(vjust = 0.5), family = "Lato") +
  coord_flip() +
  #design stuff
  scale_fill_manual("Primary Support", values = c("#828585", "#4f5157", "#fd8f24", "#c03728", "#919c4c", "#f5c04a", "#e68c7c", "#919c4c", "#6f5438")) +
  theme_pomological(base_family="Lato", base_size = 14) +
  theme(plot.subtitle = element_text(face = "italic", size = 14), 
        plot.title = element_text(size = 18),
        plot.caption = element_text(face = "italic", size = 8, vjust = 0.5, hjust = 1)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "Ethnicity", y = "Percent", title = "Primary source of PhD funding by ethnicity", subtitle = str_wrap("International students rely almost entirely on assistantships and grants, while black students are passed over for assistantships and fill the gap with their own resources", 55), caption = "Graphic: @W_R_Chase\nData: NCSES")

ggsave("ethnicity_funding_support.png", device = "png", type = "cairo", height = 6, width = 12)

