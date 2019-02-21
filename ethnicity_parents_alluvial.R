#have to load fonts before ggplot
extrafont::loadfonts(device="win")

library(readxl)
library(tidyverse)
library(ggalluvial)
#devtools::install_github("gadenbuie/ggpomological")
library(ggpomological)

#read in data, fix names
data <- read_xlsx("data/ethnicity_parents_education.xlsx", skip = 3) 

data_clean <-
  data %>%
  gather(key = education, value = percent, -ethnicity, -parent) %>%
  filter(!is.na(percent)) %>%
  group_by(ethnicity, education) %>%
  summarize(mean = mean(percent)) %>%
  ungroup() %>%
  mutate(ethnicity = ifelse(ethnicity == "Temporary visa holder", "International student", ethnicity)) %>%
  mutate(ethnicity = as.factor(ethnicity), education = as.factor(education))

data_clean$education <- fct_collapse(data_clean$education, "Some college, high school, or less" = c("Some college", "High school or less"))
data_clean$ethnicity <- fct_collapse(data_clean$ethnicity, "Other or not reported" = c("Other or not reported", "Ethnicity not reported"))
data_clean$mean[data_clean$ethnicity=="Other or not reported"] <- data_clean$mean[data_clean$ethnicity=="Other or not reported"]/2

data_clean <- 
  data_clean%>%
  mutate(ethnicity = str_wrap(data_clean$ethnicity, 18), education = str_wrap(data_clean$education, 18))

ggplot(as.data.frame(data_clean),
       aes(y = mean, axis1 = ethnicity, axis2 = education)) +
  geom_alluvium(aes(fill = education), width = 1/12, alpha = 0.7) +
  geom_stratum(width = 1/7, fill = "#efe1c6", color = "white", size = 1) +
  geom_text(stat = "stratum", label.strata = TRUE, family = "Lato", hjust = "left", nudge_x = -0.065) +
  scale_x_discrete(limits = c(str_wrap("PhD awardee ethnicity", 18), str_wrap("Parent's educational attainment", 18)), expand = c(.05, .05)) +
  scale_fill_manual(values = c("#c03728", "#f5c04a", "#0C1E43"), guide = FALSE) +
  theme_pomological(base_family="Lato", base_size = 14) +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.text.x = element_text(size = 14, hjust = 0.5)) +
  theme(plot.subtitle = element_text(face = "italic", size = 14), 
        plot.title = element_text(size = 18),
        plot.caption = element_text(face = "italic", size = 8.5, vjust = 0.5, hjust = 1)) +
  labs(x = "", y = "", title = "Parent's educational attainment of PhD graduates by ethnicity", subtitle = "Minority students are less likely to have parents with advanced degrees", caption = "Graphic: @W_R_Chase\nData: NCSES")

ggsave("ethnicity_parents_education.png", device = "png", type = "cairo", height = 8, width = 14)

