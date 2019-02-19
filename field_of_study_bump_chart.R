#have to load fonts before ggplot
extrafont::loadfonts(device="win")

library(readxl)
library(tidyverse)
library(ggalluvial)
#devtools::install_github("gadenbuie/ggpomological")
library(ggpomological)

#read in data, fix names
data1 <- read_xlsx("data/field_of_study_1.xlsx", skip = 3)
data2 <- read_xlsx("data/field_of_study_2.xlsx", skip = 3) 
colnames(data1)[1] <- "field_of_study"
colnames(data2)[1] <- "field_of_study"

#get only subset of data from major categories
#clean up some entries
cats <- c("All fields", "Life sciences", "Physical sciences and earth sciences", "Mathematics and computer sciences",
  "Psychology and social sciences", "Engineering", "Education", "Humanities and arts", "Othero", "Othera")

data2_filter <- 
  data2 %>%
  filter(field_of_study %in% cats)
data2_filter[9, 1] <- "Other"

data1_filter <- 
  data1 %>%
  filter(field_of_study %in% cats) %>%
  select(-ends_with("__1"), -"2012", -"2017")
data1_filter[9, 1] <- "Other"

#join data from old and new sets
data_comb <- 
  inner_join(data1_filter, data2_filter, by = "field_of_study")

#get the overall vals for percent calc
all_fields <- 
  data_comb %>% 
  slice(1) %>% 
  gather(key = year, value = all_fields, -field_of_study) %>% 
  select(2:3)

#switch to long format, calculate percent
data_long <-   
  data_comb %>%
  gather(key = year, value = phds, -field_of_study) %>%
  right_join(., all_fields, by = "year") %>%
  mutate(phds = as.numeric(phds), all_fields = as.numeric(all_fields), percent = round((phds/all_fields)*100, 2)) %>%
  filter(field_of_study != "All fields")

#make labels for y axis
data_rank <- 
  data_long %>%
  arrange(year, percent) %>%
  mutate(rank = rep(8:1, 15))

data_labels_before <- 
  data_rank %>%
  filter(year == "1987") %>%
  mutate(field_of_study = case_when(
    field_of_study == "Mathematics and computer sciences" ~ "Math and computer sciences",
    field_of_study == "Physical sciences and earth sciences" ~ "Physical and earth sciences",
    TRUE ~ field_of_study
  )) %>%
  mutate(pos = cumsum(percent)-(percent/2), 
         field_of_study = str_wrap(data_labels_before$field_of_study, 18),
         color = c("#e68c7c", "#828585", "#fd8f24", "#919c4c", "#4f5157", "#f5c04a", "#6f5438", "#c03728")
        ) 
data_labels_before[1,7] <- 0

data_labels_after <- 
  data_rank %>%
  filter(year == "2017") %>%
  mutate(field_of_study = case_when(
    field_of_study == "Mathematics and computer sciences" ~ "Math and computer sciences",
    field_of_study == "Physical sciences and earth sciences" ~ "Physical and earth sciences",
    TRUE ~ field_of_study
  )) %>%
  arrange(phds) %>%
  mutate(pos = cumsum(percent)-(percent/2), 
         field_of_study = str_wrap(data_labels_after$field_of_study, 18),
         color = c("#828585", "#e68c7c", "#c03728", "#fd8f24", "#4f5157", "#6f5438", "#919c4c", "#f5c04a")
         )

#plot w/ ggalluvial
ggplot(data_long, aes(x = year, y = percent, alluvium = field_of_study)) +
  #area bump chart
  geom_alluvium(aes(fill = field_of_study, color = field_of_study), width = 0.25, alpha = 0.75, decreasing = FALSE) +
  #y axis labels
  geom_text(data = data_labels_before, aes(x = year, y = pos, label = field_of_study), hjust = "right", nudge_x = -0.2, size = 4, family = "Lato", fontface = "bold", color = data_labels_before$color) +
  geom_text(data = data_labels_after, aes(x = year, y = pos, label = field_of_study), hjust = "left", nudge_x = 0.2, size = 4, family = "Lato", fontface = "bold", color = data_labels_after$color) +
  #design stuff
  scale_fill_manual(values = c("#c03728", "#919c4c", "#fd8f24", "#f5c04a", "#e68c7c", "#828585", "#4f5157", "#6f5438"), guide = FALSE) +
  scale_color_manual(values = c("#c03728", "#919c4c", "#fd8f24", "#f5c04a", "#e68c7c", "#828585", "#4f5157", "#6f5438"), guide = FALSE) +
  scale_x_discrete(expand = c(0.185, 0)) +
  theme_pomological(base_family="Lato", base_size = 14) +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
  theme(plot.subtitle = element_text(face = "italic", size = 14), 
        plot.title = element_text(size = 18),
        plot.caption = element_text(face = "italic", size = 8.5, vjust = 0.5, hjust = 1)) +
  labs(x = "Year", y = "Bar order: rank\nBar width: Percent of PhDs awarded", title = "PhDs awarded by field of study", subtitle = "Engineering climbs; education tumbles; biology wins", caption = "Graphic: @W_R_Chase\nData: NCSES")

ggsave("field_of_study_bump.png", device = "png", type = "cairo", height = 7, width = 12)




