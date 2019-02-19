#have to load fonts before ggplot
extrafont::loadfonts(device="win")

library(readxl)
library(tidyverse)
#devtools::install_github("gadenbuie/ggpomological")
library(ggpomological)

#read in data, get subset
data <- read_xlsx("data/ethnicity_1.xlsx", skip = 3) %>%
  slice(1:9)

#do some manual cleaning
data_clean <-
  data %>%
  slice(-3)

colnames(data_clean)[1] <- "ethnicity"  
data_clean[4,1] <- "Asian"
data_clean[8,1] <- "Other or not reported"
data_clean[7,2] <- 0

#get overall vals for percent calc
all_fields <-
  data_clean %>%
  gather(key = year, value = all_fields, -ethnicity) %>%
  filter(ethnicity == "All fields") %>%
  select(all_fields, year) %>%
  mutate(all_fields = as.numeric(all_fields))

#switch to long format, calc percent
data_long <- 
  data_clean %>%
  filter(ethnicity != "All fields") %>%
  gather(key = year, value = phds, -ethnicity) %>%
  right_join(., all_fields, by = "year") %>%
  mutate(phds = as.numeric(phds), percent = (phds/all_fields)*100)

#plot
ggplot(data_long, aes(x = year, y = percent, fill = ethnicity)) +
  #stacked bar
  geom_bar(stat = "identity") +
  #design stuff
  scale_fill_manual("Ethnicity", values = c("#828585", "#4f5157", "#fd8f24", "#c03728", "#f5c04a", "#e68c7c", "#919c4c", "#6f5438")) +
  theme_pomological(base_family="Lato", base_size = 14) +
  theme(plot.subtitle = element_text(face = "italic", size = 14), 
        plot.title = element_text(size = 18),
        plot.caption = element_text(face = "italic", size = 8, vjust = 0.5, hjust = 1)) +
  labs(x = "Year", y = "Percent", title = "PhDs awarded by ethnicity", subtitle = "Grad school is very white, but changing slowly", caption = "Graphic: @W_R_Chase\nData: NCSES")
ggsave("ethnicity_stacked_bar.png", device = "png", type = "cairo", height = 6, width = 6.5)
