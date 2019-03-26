#have to load fonts before ggplot
extrafont::loadfonts(device="win")
#extrafont::loadfonts(device="postscript")
extrafont::loadfonts()
library(tidyverse)
library(sp)
library(rworldmap)
library(ggforce)
library(relayer)

data <- read_csv("https://raw.githubusercontent.com/emeeks/datavizsociety/master/challenge_data/dvs_challenge_1_membership_time_space.csv")

day <- as.numeric(as.factor(data$date))

data2 <- 
  data %>%
  filter(!is.na(lat)) %>%
  mutate(day = as.numeric(as.factor(.$date))) %>%
  group_by(day) %>%
  mutate(hour_floor = hour - min(hour))

data2 %>% group_by(date) %>% count()
days <- data2 %>% group_by(day) %>% count()
hours <- data2 %>% group_by(hour) %>% count() %>% mutate(percent = (n/3460)*100)
working_hours <- hours[9:23, ]
sum(working_hours$percent)
ggplot(hours) + geom_bar(aes(x = hour, y = n), stat = "identity")

#combining two imperfect geocoding methods because google maps api is a pain in the ass now
dat_coords <- data.frame(lon = data2$long, lat = data2$lat)
coords2continent = function(points)
{  
  countriesSP <- getMap(resolution='low')
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  indices = over(pointsSP, countriesSP)
  indices$REGION   # returns the continent (7 continent model)
}

region <- coords2continent(dat_coords)
data2$region <- region

NAm=data.frame(lat=c(90,       90,  78.13,   57.5,  15,  15,  1.25,  1.25,  51,  60,    60, 90),
               lon=c(-168.75 ,-10 ,-10     ,-37.5 ,-30 ,-75 ,-82.5 ,-105  ,-180 ,-180 ,-168.75, -168.75))
NAm2 = data.frame(lat=c(51,    51,  60, 51),
                  lon=c(166.6, 180, 180, 166.6))
SAm = data.frame(lat=c(1.25,   1.25,  15,  15, -60, -60, 1.25),
                 lon=c(-105, -82.5,  -75, -30, -30, -105, -105))
europe=data.frame(lat=c(90,   90,  42.5, 42.5, 40.79, 41, 40.55, 40.40, 40.05, 39.17, 35.46, 
                        33,   38,  35.42, 28.25, 15,  57.5,  78.13, 90),
                  lon=c(-10, 77.5, 48.8, 30,   28.81, 29, 27.31, 26.75, 26.36, 25.19, 27.91,
                        27.5, 10, -10,  -13,   -30, -37.5, -10, -10))
africa=data.frame(lat=c(15,  28.25 ,35.42 ,38 ,33   ,31.74 ,29.54 ,27.78 ,11.3 ,12.5 ,-60 ,-60, 15),
                  lon=c(-30 ,-13   ,-10 ,10 ,27.5 ,34.58 ,34.92 ,34.46 ,44.3 ,52    ,75 ,-30, -30))
australia=data.frame(lat=c(-11.88, -10.27, -10 ,-30    ,-52.5 ,-31.88, -11.88),
                     lon=c(110,      140  ,145 ,161.25 ,142.5  ,110, 110))
asia=data.frame(lat=c(90   ,42.5 ,42.5 ,40.79 ,41 ,40.55 ,40.4  ,40.05 ,39.17 ,35.46 ,33   ,
                      31.74 ,29.54 ,27.78 ,11.3 ,12.5 ,-60 ,-60 ,-31.88 ,-11.88 ,-10.27 ,33.13 ,51    ,60  ,90, 90),
                lon=c(77.5 ,48.8 ,30   ,28.81 ,29 ,27.31 ,26.75 ,26.36 ,25.19 ,27.91 ,27.5 ,
                      34.58 ,34.92 ,34.46 ,44.3 ,52   ,75  ,110  ,110   ,110    ,140    ,140   ,166.6 ,180 ,180, 77.5))
asia2=data.frame(lat=c(90    ,90      ,60      ,60, 90),
                 lon=c(-180 ,-168.75 ,-168.75 ,-180, -180))
antarctica=data.frame(lat=c(-60, -60, -90, -90, -60),
                      lon=c(-180, 180, 180, -180, -180))

continents=list(
  y=c(NAm$lat, NA, NAm2$lat, NA, SAm$lat, NA, europe$lat,NA,africa$lat,NA,
      australia$lat,NA,asia$lat,NA,asia2$lat,NA,antarctica$lat),
  x=c(NAm$lon, NA, NAm2$lon, NA, SAm$lon, NA,europe$lon,NA,africa$lon,NA,
      australia$lon,NA,asia$lon,NA,asia2$lon,NA,antarctica$lon),
  names=c("North America", "North America:2", "South America", "Europe",
          "Africa","Australia","Asia","Asia:2","Antarctica"))
class(continents) <- "map"
data2$continent <- maps::map.where(continents, x=data2$long, y=data2$lat)

data3 <- 
  data2 %>%
  mutate(region = as.character(region), region = ifelse(is.na(region), continent, region),
         region = ifelse(is.na(region), "Australia", region), region = ifelse(region == "Australia", "Oceania", region)) %>%
  select(-continent)

# ggplot(data3) +
#   geom_point(aes(x = day, y = hour, color = region, size = visualization))
# ggsave("first_plot.png", device = "png", type = "cairo")

data_spacing <- 
  data3 %>%
  group_by(day, hour) %>%
  mutate(num = row_number()) %>%
  group_split() %>%
  map_dfr( ~mutate(., num_scale = scales::rescale(num, to = c(0, 0.99)))) %>%
  mutate(interval = hour + num_scale, score_chg = (data - society)/2) 

day_lag <- 
  data_spacing %>%
  select(day) %>%
  distinct() %>%
  ungroup() %>%
  mutate(day_lag = lag(day), day_lag = ifelse(is.na(day_lag), 0, day_lag))
  
data_spacing2 <- 
  data_spacing %>%
  left_join(., day_lag, by = "day") %>%
  mutate(spacer = 7, x_spaced = day + spacer * day_lag, x_new = x_spaced + score_chg)

 # ggplot(data_spacing2) +
 #   geom_diagonal(aes(x = x_new, y = interval, xend = lead(x_new), yend = lead(interval))) +
 #   geom_point(aes(x = x_new, y = interval, size = visualization, color = region), alpha = 1) +
 #   theme_minimal()
# 
# ggsave("pearls_test.png", device = "png", type = "cairo", height = 15, width = 15)

#reverse every other day so they read end to end like flowing
data_rev <- 
  data_spacing2 %>%
  group_by(day) %>%
  mutate(y_rev = ifelse(day %% 2 == 0, rev(interval), interval)) %>%
  ungroup()

#calculate endpoints for ligatures
bots <- 
  data_rev %>%
  group_by(day) %>%
  filter(y_rev == min(y_rev)) %>%
  select(day, x_bot = x_new, y_bot = y_rev)

tops <- 
  data_rev %>%
  group_by(day) %>%
  filter(y_rev == max(y_rev)) %>%
  select(day, x_top = x_new, y_top = y_rev)

endpoints <- inner_join(bots, tops, by = "day") %>%
  ungroup()

#for equal spaced links
perfect_endpoints <- 
  data_rev %>%
  group_by(day) %>%
  distinct(day, x_spaced) %>%
  ungroup() %>%
  mutate(x_bot = x_spaced, x_top = x_spaced, y_bot = rep(0, 18), y_top = rep(24, 18))
  
#calculate bezier control pts for ligatures

# top_endpoints <- 
#   endpoints %>%
#   mutate(group = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9)) %>%
#   select(-x_bot, -y_bot) %>%
#   group_by(group) %>%
#   mutate(order = c(1, 4)) %>%
#   ungroup()

top_perfect_endpoints <- 
  perfect_endpoints %>%
  mutate(group = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9)) %>%
  select(-x_bot, -y_bot) %>%
  group_by(group) %>%
  mutate(order = c(1, 4)) %>%
  ungroup()

# top_controls <- 
#   top_endpoints %>%
#   mutate(y_top = y_top + 2.5) %>%
#   group_by(group) %>%
#   mutate(order = c(2, 3)) %>%
#   ungroup()

top_perfect_controls <- 
  top_perfect_endpoints %>%
  mutate(y_top = y_top + 2.5) %>%
  group_by(group) %>%
  mutate(order = c(2, 3)) %>%
  ungroup()

# top_links <- 
#   rbind(top_endpoints, top_controls) %>%
#   arrange(group, order)

top_perfect_links <- 
  rbind(top_perfect_endpoints, top_perfect_controls) %>%
  arrange(group, order)

# bot_endpoints <- 
#   endpoints %>%
#   filter(day != 1 & day != 18) %>%
#   mutate(group = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8)) %>%
#   select(-x_top, -y_top) %>%
#   group_by(group) %>%
#   mutate(order = c(1, 4)) %>%
#   ungroup()

bot_perfect_endpoints <- 
  perfect_endpoints %>%
  filter(day != 1 & day != 18) %>%
  mutate(group = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8)) %>%
  select(-x_top, -y_top) %>%
  group_by(group) %>%
  mutate(order = c(1, 4)) %>%
  ungroup()

# bot_controls <- 
#   bot_endpoints %>%
#   mutate(y_bot = y_bot + -2.5) %>%
#   group_by(group) %>%
#   mutate(order = c(2, 3)) %>%
#   ungroup()

bot_perfect_controls <- 
  bot_perfect_endpoints %>%
  mutate(y_bot = y_bot + -2.5) %>%
  group_by(group) %>%
  mutate(order = c(2, 3)) %>%
  ungroup()

# bot_links <- 
#   rbind(bot_endpoints, bot_controls) %>%
#   arrange(group, order)

bot_perfect_links <- 
  rbind(bot_perfect_endpoints, bot_perfect_controls) %>%
  arrange(group, order)

###optional for quadratic instead of cubic bezier
# top_endpoints <- 
#   endpoints %>%
#   mutate(group = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9)) %>%
#   select(-x_bot, -y_bot) %>%
#   group_by(group) %>%
#   mutate(order = c(1, 3)) %>%
#   ungroup()
# 
# top_controls <- 
#   top_endpoints %>%
#   mutate(y_top = y_top + 2.5) %>%
#   group_by(group) %>%
#   mutate(order = c(2, 3), x_top = mean(x_top)) %>%
#   ungroup() %>%
#   filter(order != 3) %>%
#   mutate(y_top = max(y_top))
# 
# top_links <- 
#   rbind(top_endpoints, top_controls) %>%
#   arrange(group, order)
# 
# bot_endpoints <- 
#   endpoints %>%
#   filter(day != 1 & day != 18) %>%
#   mutate(group = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8)) %>%
#   select(-x_top, -y_top) %>%
#   group_by(group) %>%
#   mutate(order = c(1, 3)) %>%
#   ungroup()
# 
# bot_controls <- 
#   bot_endpoints %>%
#   mutate(y_bot = y_bot - 2.5) %>%
#   group_by(group) %>%
#   mutate(order = c(2, 3), x_bot = mean(x_bot)) %>%
#   ungroup() %>%
#   filter(order != 3) %>%
#   mutate(y_bot = min(y_bot))
# 
# bot_links <- 
#   rbind(bot_endpoints, bot_controls) %>%
#   arrange(group, order)
# 
# #perfect cubic ligatures
# top_perfect_endpoints <- 
#   perfect_endpoints %>%
#   mutate(group = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9)) %>%
#   select(-x_bot, -y_bot) %>%
#   group_by(group) %>%
#   mutate(order = c(1, 3)) %>%
#   ungroup()
# 
# top_perfect_controls <- 
#   top_perfect_endpoints %>%
#   mutate(y_top = y_top + 2.5) %>%
#   group_by(group) %>%
#   mutate(order = c(2, 3), x_top = mean(x_top)) %>%
#   ungroup() %>%
#   filter(order != 3) %>%
#   mutate(y_top = max(y_top))
# 
# top_perfect_links <- 
#   rbind(top_perfect_endpoints, top_perfect_controls) %>%
#   arrange(group, order)
# 
# bot_perfect_endpoints <- 
#   perfect_endpoints %>%
#   filter(day != 1 & day != 18) %>%
#   mutate(group = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8)) %>%
#   select(-x_top, -y_top) %>%
#   group_by(group) %>%
#   mutate(order = c(1, 3)) %>%
#   ungroup()
# 
# bot_perfect_controls <- 
#   bot_perfect_endpoints %>%
#   mutate(y_bot = y_bot - 2.5) %>%
#   group_by(group) %>%
#   mutate(order = c(2, 3), x_bot = mean(x_bot)) %>%
#   ungroup() %>%
#   filter(order != 3) %>%
#   mutate(y_bot = min(y_bot))
# 
# bot_perfect_links <- 
#   rbind(bot_perfect_endpoints, bot_perfect_controls) %>%
#   arrange(group, order)
# 
# #get rid of geom_diagonals between endpoints
# data_rev2 <- 
#   data_rev %>%
#   group_by(day) %>%
#   mutate(color = case_when(
#     y_rev == max(y_rev) ~ NA_character_,
#     y_rev == min(y_rev) ~ NA_character_,
#     TRUE ~ "black"
#   ))
# 
# #now adding back in some diagonals that got cut off
# fix_top <- 
#   data_rev2 %>%
#   filter(day %in% c(2, 4, 6, 8, 10, 12, 14, 16, 18)) %>%
#   slice(1:2) %>%
#   ungroup() %>%
#   mutate(num = ifelse(!is.na(color), 2, 1)) %>%
#   select(num, x_new, y_rev, day) %>%
#   gather(key = "var", value = "value", -num, -day) %>%
#   unite("new_var", var, num, sep = "_") %>%
#   spread(key = new_var, value = value)
# 
# fix_bot <- 
#   data_rev2 %>%
#   filter(day %in% c(3, 5, 7, 9, 11, 13, 15, 17)) %>%
#   slice(1:2) %>%
#   ungroup() %>%
#   mutate(num = ifelse(!is.na(color), 2, 1)) %>%
#   select(num, x_new, y_rev, day) %>%
#   gather(key = "var", value = "value", -num, -day) %>%
#   unite("new_var", var, num, sep = "_") %>%
#   spread(key = new_var, value = value)
# 
# fix_all <- rbind(fix_top, fix_bot)
colors_new <-  c("#01E5A8", "#FDEF0B", "#F53E91", "#FE783E", "#B9E128", "#910599")

#using relayer to have diff color scales for diagonals and points
ggplot(data_rev2) +
  geom_diagonal(aes(x = x_new, y = y_rev, xend = lead(x_new), yend = lead(y_rev), colour = color)) +
  geom_diagonal(data = fix_all, aes(x = x_new_1, y = y_rev_1, xend = x_new_2, yend = y_rev_2), color = "white") +
  geom_bezier(data = top_links, aes(x = x_top, y = y_top, group = group), colour = "white") +
  geom_bezier(data = bot_links, aes(x = x_bot, y = y_bot, group = group), colour = "white") +
  geom_point(aes(x = x_new, y = y_rev, size = visualization, colour2 = region), alpha = 1) %>% rename_geom_aes(new_aes = c("colour" = "colour2")) +
  scale_color_identity(aesthetics = "colour") +
  scale_color_manual(aesthetics = "colour2", values = colors_new) +
  coord_flip() +
  scale_x_reverse() +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(),
        panel.background = element_rect(fill = "#14183D", size = 0), plot.background = element_rect(fill = "#14183D", size = 0), 
        panel.border = element_blank(), legend.text = element_text(color = "white"), legend.title = element_text(color = "white"))


#ggsave("colors_test.png", device = "png", type = "cairo", height = 15, width = 15)


pearl_colors <- c("#B5B9A3", "#736C94", "#DFCA87", "#CBC4BA", "#7B3E45", "#9CAFB6")
ggplot(data_rev2) +
  geom_diagonal(aes(x = x_new, y = y_rev, xend = lead(x_new), yend = lead(y_rev), colour = color)) +
  geom_diagonal(data = fix_all, aes(x = x_new_1, y = y_rev_1, xend = x_new_2, yend = y_rev_2), color = "black") +
  geom_bezier(data = top_links, aes(x = x_top, y = y_top, group = group), colour = "black") +
  geom_bezier(data = bot_links, aes(x = x_bot, y = y_bot, group = group), colour = "black") +
  geom_point(aes(x = x_new, y = y_rev, size = visualization, colour2 = region), alpha = 1) %>% rename_geom_aes(new_aes = c("colour" = "colour2")) +
  scale_color_identity(aesthetics = "colour") +
  scale_color_manual(aesthetics = "colour2", values = pearl_colors) +
  coord_flip() +
  scale_x_reverse() +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "NA"), plot.background = element_rect(fill = "white", color = "NA"), 
        panel.border = element_blank(), legend.text = element_text(color = "black"), legend.title = element_text(color = "black"))

ggsave("pearl_ggplot.svg", device = "svg", height = 15, width = 15)

#colors purple, pink, green, blue, yellow, grey
watercolors <- c("#9F5697", "#FF7C7C", "#5ABD5E", "#327CBC", "#EED121", "#8b8285")
ggplot(data_rev2) +
  geom_diagonal(aes(x = x_new, y = y_rev, xend = lead(x_new), yend = lead(y_rev), colour = color)) +
  geom_diagonal(data = fix_all, aes(x = x_new_1, y = y_rev_1, xend = x_new_2, yend = y_rev_2), color = "black") +
  geom_bezier(data = top_links, aes(x = x_top, y = y_top, group = group), colour = "black") +
  geom_bezier(data = bot_links, aes(x = x_bot, y = y_bot, group = group), colour = "black") +
  geom_point(aes(x = x_new, y = y_rev, size = visualization, colour2 = region), alpha = 1) %>% rename_geom_aes(new_aes = c("colour" = "colour2")) +
  scale_color_identity(aesthetics = "colour") +
  scale_color_manual(aesthetics = "colour2", values = watercolors, guide = "none") +
  scale_size(guide = "none") +
  coord_flip() +
  scale_x_reverse() +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "NA"), plot.background = element_rect(fill = "white", color = "NA"), 
        panel.border = element_blank(), legend.text = element_text(color = "black"), legend.title = element_text(color = "black"))

ggsave("watercolor_ggplot.svg", device = "svg", height = 15, width = 15)


#colors purple, pink, blue, yellow, red
#using straight guide lines and perfect ligatures
watercolors <- c("#AC44B5", "#FB5573", "#EC1D14", "#156EDB", "#EED121", "#5ABD5E")
ggplot(data_rev) +
  geom_segment(data = perfect_endpoints, aes(x = x_bot, xend = x_top, y = y_bot, yend = y_top, group = x_spaced), size = 0.8, color = "black", alpha = 0.7) +
  geom_bezier(data = top_perfect_links, aes(x = x_top, y = y_top, group = group), colour = "black", alpha = 0.7, size = 0.8) +
  geom_bezier(data = bot_perfect_links, aes(x = x_bot, y = y_bot, group = group), colour = "black", alpha = 0.7, size = 0.8) +
  geom_point(aes(x = x_new, y = y_rev, size = (visualization + 0.5)*5.2, colour = region), alpha = 0.8) +
  scale_color_manual("Region", values = watercolors) +
  scale_size_identity("Visualization score") +
  coord_flip() +
  scale_x_reverse() +
  theme_minimal() +
  labs(title = "The Data Visualization Society",
       subtitle = "Membership Timeline") +
  guides(colour = guide_legend(override.aes = list(size=20)), size = guide_legend())+
  theme(axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(),
        panel.background = element_blank(), plot.background = element_rect(fill = "transparent", color = "NA"), 
        panel.border = element_blank(), legend.text = element_text(color = "black", size = 45), legend.title = element_text(color = "black", size = 70),
        plot.margin = margin(t = 150, r = 0, b = 100, l = 0, unit = "mm"), legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 120),
        plot.subtitle = element_text(hjust = 0.5, size = 100))

ggsave("watercolor-test2.svg", device = "svg", height = 1189, width = 841, units = "mm", limitsize = FALSE)
