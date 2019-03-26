extrafont::loadfonts(device="win")
library(tidyverse)
library(ghibli)

board_games <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-12/board_games.csv")
glimpse(board_games)

my_theme <- theme(
  panel.background = element_blank(),
  plot.background = element_rect(fill = "#FCF5E3", size = 0),
  legend.background = element_blank(),
  legend.key = element_blank(),
  panel.border = element_blank(),
  plot.subtitle = element_text(face = "italic", size = 14, vjust = 5), 
  plot.title = element_text(size = 18, vjust = 5),
  plot.caption = element_text(face = "italic", size = 8, vjust = 0.5, hjust = 1),
  panel.grid.major = element_line(size = 0.12, color = "#E1DAC8"),
  panel.grid.minor = element_blank(), 
  axis.ticks = element_line(size = 0.03, color = "#E1DAC8"),
  axis.title.x = element_text(vjust = -5, margin = margin(15, unit = "pt")),
  plot.margin = margin(t = 25, l = 20, r = 20, b=20, unit = "pt") 
)

designers <- 
  board_games %>%
  group_by(designer) %>%
  summarize(num = n(), rating = round(mean(average_rating), 2), time = mean(max_playtime)) %>%
  filter(!is.na(designer) & designer != "(Uncredited)") %>%
  arrange(desc(num))

artists <- 
  board_games %>%
  group_by(artist) %>%
  summarize(num = n(), rating = round(mean(average_rating), 2), time = mean(max_playtime)) %>%
  filter(!is.na(artist) & artist != "(Uncredited)") %>%
  arrange(desc(num))

publishers <- 
  board_games %>%
  group_by(publisher) %>%
  summarize(num = n(), rating = round(mean(average_rating), 2), time = mean(max_playtime)) %>%
  filter(!is.na(publisher) & publisher != "(Uncredited)") %>%
  arrange(desc(num))

top_artists <- artists[1:10, ]

art_cat <- 
  board_games %>%
  filter(artist %in% top_artists$artist) %>%
  select(name, category, artist) %>%
  group_by(artist, category) %>%
  summarize(count = n()) %>%
  filter(!is.na(category)) %>%
  arrange(artist, desc(count)) %>%
  ungroup() %>%
  separate(category, into = c(paste0("V", 1:10)), sep = ",") %>%
  gather(key = dummy, value = category, -artist, -count) %>%
  select(-dummy) %>%
  filter(!is.na(category)) %>%
  group_by(artist, category) %>%
  summarize(num = sum(count)) %>%
  arrange(artist, desc(num)) 

art_mech <- 
  board_games %>%
  filter(artist %in% top_artists$artist) %>%
  select(name, mechanic, artist) %>%
  group_by(artist, mechanic) %>%
  summarize(count = n()) %>%
  filter(!is.na(mechanic)) %>%
  arrange(artist, desc(count)) %>%
  ungroup() %>%
  separate(mechanic, into = c(paste0("V", 1:10)), sep = ",") %>%
  gather(key = dummy, value = mechanic, -artist, -count) %>%
  select(-dummy) %>%
  filter(!is.na(mechanic)) %>%
  group_by(artist, mechanic) %>%
  summarize(num = sum(count)) %>%
  arrange(artist, desc(num)) 

top_designers <- designers[1:10, ]

designers_cat <- 
  board_games %>%
  filter(designer %in% top_designers$designer) %>%
  select(name, category, designer) %>%
  group_by(designer, category) %>%
  summarize(count = n()) %>%
  filter(!is.na(category)) %>%
  arrange(designer, desc(count)) %>%
  ungroup() %>%
  separate(category, into = c(paste0("V", 1:10)), sep = ",") %>%
  gather(key = dummy, value = category, -designer, -count) %>%
  select(-dummy) %>%
  filter(!is.na(category)) %>%
  group_by(designer, category) %>%
  summarize(num = sum(count)) %>%
  arrange(designer, desc(num)) 

designers_mech <- 
  board_games %>%
  filter(designer %in% top_designers$designer) %>%
  select(name, mechanic, designer) %>%
  group_by(designer, mechanic) %>%
  summarize(count = n()) %>%
  filter(!is.na(mechanic)) %>%
  arrange(designer, desc(count)) %>%
  ungroup() %>%
  separate(mechanic, into = c(paste0("V", 1:10)), sep = ",") %>%
  gather(key = dummy, value = mechanic, -designer, -count) %>%
  select(-dummy) %>%
  filter(!is.na(mechanic)) %>%
  group_by(designer, mechanic) %>%
  summarize(num = sum(count)) %>%
  arrange(designer, desc(num)) 

top_publishers <- publishers[1:10, ]

publishers_cat <- 
  board_games %>%
  filter(publisher %in% top_publishers$publisher) %>%
  select(name, category, publisher) %>%
  group_by(publisher, category) %>%
  summarize(count = n()) %>%
  filter(!is.na(category)) %>%
  arrange(publisher, desc(count)) %>%
  ungroup() %>%
  separate(category, into = c(paste0("V", 1:10)), sep = ",") %>%
  gather(key = dummy, value = category, -publisher, -count) %>%
  select(-dummy) %>%
  filter(!is.na(category)) %>%
  group_by(publisher, category) %>%
  summarize(num = sum(count)) %>%
  arrange(publisher, desc(num)) 

publishers_mech <- 
  board_games %>%
  filter(publisher %in% top_publishers$publisher) %>%
  select(name, mechanic, publisher) %>%
  group_by(publisher, mechanic) %>%
  summarize(count = n()) %>%
  filter(!is.na(mechanic)) %>%
  arrange(publisher, desc(count)) %>%
  ungroup() %>%
  separate(mechanic, into = c(paste0("V", 1:10)), sep = ",") %>%
  gather(key = dummy, value = mechanic, -publisher, -count) %>%
  select(-dummy) %>%
  filter(!is.na(mechanic)) %>%
  group_by(publisher, mechanic) %>%
  summarize(num = sum(count)) %>%
  arrange(publisher, desc(num)) 

fire <- colorRampPalette(c("#ffc12e", "#b32424"))

ggplot(artists[1:10, ]) +
  geom_segment(aes(x = reorder(artist, num), xend = reorder(artist, num), y = 0, yend = num), color = "#1E1E1E") +
  geom_point(aes(x = reorder(artist, num), y = num, size = rating, color = rating)) +
  scale_size_continuous(breaks = c(6, 6.5, 7, 7.5, 8), labels = c("6", "6.5", "7", "7.5", "8"), limits = c(6, 7.5), range = c(8, 12)) +
  scale_color_gradient(breaks = c(6, 6.5, 7, 7.5, 8), labels = c("6", "6.5", "7", "7.5", "8"), limits = c(6, 7.5), low = "#ffc12e", high = "#B51212") +
  guides(color = guide_legend(), size = guide_legend()) +
  coord_flip() +
  labs(title = "Most prolific board game artists", 
       subtitle = "Point size represents the average rating of the artists games", 
       caption = "Graphic: @W_R_Chase\nData: boardgamegeek.com",
       y = "Number of games illustrated",
       x = "") +
  theme_light(base_family = "Lato") +
  my_theme +
  theme(panel.grid.major.y = element_blank())

ggsave("artists.svg", device = "svg", height = 8, width = 10)


ggplot(designers[1:10, ]) +
  geom_segment(aes(x = reorder(designer, num), xend = reorder(designer, num), y = 0, yend = num), color = "#1E1E1E") +
  geom_point(aes(x = reorder(designer, num), y = num, size = rating, color = rating)) +
  scale_size_continuous(breaks = c(6, 6.5, 7), labels = c("6", "6.5", "7"), limits = c(5.8, 7), range = c(8, 12)) +
  scale_color_gradient(breaks = c(6, 6.5, 7), labels = c("6", "6.5", "7"), limits = c(5.8, 7), low = "#179CE7", high = "#882BB9") +
  guides(color = guide_legend(), size = guide_legend()) +
  coord_flip() +
  labs(title = "Most prolific board game designers", 
       subtitle = "Point size represents the average rating of the designer's games", 
       caption = "Graphic: @W_R_Chase\nData: boardgamegeek.com",
       y = "Number of games designed",
       x = "") +
  theme_light(base_family = "Lato") +
  my_theme +
  theme(panel.grid.major.y = element_blank())

ggsave("designers.svg", device = "svg", height = 8, width = 10)
  
  #trying a network thing
  edge_list <- 
    board_games %>%
    select(game_id, name, year_published, category, average_rating, artist, designer, publisher) %>%
    group_by(artist, designer, publisher) %>%
    summarize(weight = n()) %>%
    filter(!is.na(artist) & !is.na(publisher) & !is.na(designer)) %>%
    filter(artist != "(Uncredited)" & publisher != "(Uncredited)" & designer != "(Uncredited)") %>%
    filter(weight > 1)
  