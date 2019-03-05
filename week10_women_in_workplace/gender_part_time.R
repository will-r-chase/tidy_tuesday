extrafont::loadfonts(device="win")
library(tidyverse)
library(ggpomological)

#data
jobs_gender <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-05/gender_earnings.csv")
earnings_female <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-05/earnings_female.csv") 
employed_gender <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-05/employed_gender.csv") 

#base theme
theme_du_bois <- function() {
  theme_gray(base_family = "Inconsolata") %+replace%
    theme(
      plot.background = element_rect(
        fill = "antiquewhite2",
        color = "antiquewhite2"
      ),
      panel.background = element_rect(
        fill = "antiquewhite2",
        color = "antiquewhite2"
      ),
      plot.title = element_text(
        face = "bold",
        hjust = 0
      )
    )
}

#get our data ready
part_time <- 
  employed_gender %>%
  select(year, part_time_female, part_time_male, full_time_female, full_time_male) %>%
  gather(key = var, value = percent, -year) %>%
  separate(var, into = c("type", "time", "sex")) %>%
  select(-time) %>%
  mutate(type = ifelse(type == "full", "Full Time", "Part Time")) %>%
  mutate(half_decade = (year %/% 5) * 5) %>% #this'll make it more blocky like the original
  group_by(half_decade, sex, type) %>%
  mutate(average = round(mean(percent), 0))

#x labels
labs <- c("100", "90", "80", "70", "60", "50", "40", "30", "20", "10", "0", "10",  "20",  "30",  "40",  "50",  "60",  "70",  "80",  "90",  "100")

plot <- 
ggplot(part_time, aes(x = rev(year), y = ifelse(sex=="male", -average, average), fill = type)) +
  geom_bar(stat = "identity", width = 1) +
  geom_hline(yintercept = 0, linetype = "solid", size = 2) +
  annotate("text", x = c(2000, 2000, 2000, 2000), 
           y = c(-50, -6, 13, 50), label = c("FULL-TIME", "PART-TIME", "PART-TIME", "FULL-TIME"),
           family = "Inconsolata", fontface = "bold", 
           angle = c(45, 60, -45, -45), size = 6) +
  coord_flip(clip = "off") +
  theme_du_bois() +
  scale_fill_manual(
    values = c("royalblue3", "#C1032A"),
    labels = c("PART-TIME", "FULL-TIME")
  )  +
  scale_x_continuous(
    breaks = c(1969.5, 1974.5, 1979.5, 1984.5, 1989.5, 1994.5, 1999.5, 2004.5, 2009.5, 2014.5),
    labels = rev(c("1970", "1975", "1980", "1985", "1990", "1995", "2000", "2005", "2010", "2015")),
    expand = c(0, 0),
    sec.axis = dup_axis()
    ) +# dual age axis
  scale_y_continuous(
    breaks = seq(-100, 100, by = 10),
    labels = paste0(labs, "%"),
    expand = c(0, 0),
    # lines on original plot are by 2s
    minor_breaks = seq(-100, 100, by = 2)
  ) +
  labs(title = "Percent of part-time and full-time workers by sex", 
       subtitle = "Women have remained nearly constant over the last 48 years, while the proportion of men working part time is slowly increasing",
       x = "YEAR", 
       y = "PERCENTS",
       caption = "Graphic: @W_R_Chase\nData: Census Bureau") +
  annotate(
    "text",
    label = c("MALES", "FEMALES"),
    y = c(-50, 50),
    x = Inf, # is this a thing? will it just put it outside the panel with
    # clip = "off"?
    vjust = -0.4,
    size = 6,
    family = "Inconsolata",
    fontface = "bold"
  ) +
  theme(
    text = element_text(face = "bold"),
    panel.background = element_blank(),
    plot.title = element_text(
      family = "Cormorant Garamond",
      face = "plain",
      size = 24,
      vjust = 8,
      margin = margin(t = 12, b = 5, unit = "pt")
    ),
    plot.subtitle = element_text(
      family = "Cormorant Garamond",
      face = "plain",
      size = 18,
      vjust = 8,
      margin = margin(b = 10, unit = "pt")
    ),
    axis.title = element_text(size = 14),
    axis.ticks = element_blank(),
    panel.grid.major = element_line(
      color = "black",
      size = 0.1
    ),
    panel.grid.minor.x = element_line(
      color = "black",
      size = 0.06
    ),
    panel.grid.minor.y = element_blank(),
    legend.background = element_blank(),
    legend.position = "none",
    legend.key = element_blank(),
    # put grid lines on top so not covered by plot
    panel.ontop = TRUE,
    panel.border = element_rect(
      fill = NA,
      color = "black"
    ),
    axis.text.x = element_text(size = 12),
    # both axes titles for age hortizontal instead of vertical, and put them at
    # the top, just above the values
    axis.title.y = element_text(
      angle = 0,
      vjust = 1
    ),
    axis.title.y.right = element_text(
      angle = 0,
      vjust = 1
    ),
    # age group labels need to be slightly below grid line
    axis.text.y = element_text(
      vjust = 0.5,
      size = 12
    ),
    #something really weird with margins
    #when you change it it changes the weight of the grid lines
    #played with it to try to fix it... but gave up
    plot.margin = margin(t = 25, l = 10, r = 10, unit = "pt") 
  )


paint_pomological(plot, outfile = "second_plot.png", height = 650, width = 1100, pointsize = 18)

paint_pomological <- function(
  pomo_gg, 
  width = 800, 
  height = 500, 
  pointsize = 16, 
  outfile = NULL,
  pomological_background = "week10_women_in_workplace/background.jpg", 
  pomological_overlay = pomological_images("overlay"),
  ...
) {
  if (!requireNamespace("magick", quietly = TRUE)) {
    stop("The package magick is required for `paint_pomological()`. ",
         "Please install with `install.packages('magick')`")
  }
  if (!file.exists(pomological_background)) {
    warning(paste0("Cannot find file \"", pomological_background, "\""), call. = FALSE)
  }
  
  # Paint figure
  pomo_gg <- pomo_gg + ggplot2::theme(plot.background = ggplot2::element_rect(fill = 'transparent', colour = NA))
  gg_fig <- magick::image_graph(width, height, bg = "transparent", pointsize = pointsize, ...)
  print(pomo_gg)
  dev.off()
  
  if (!is.null(pomological_overlay) && file.exists(pomological_overlay)) {
    pomo_over <- magick::image_read(pomological_overlay)
    pomo_over <- magick::image_resize(pomo_over, paste0(width, "x", height, "!"))
    gg_fig <- magick::image_composite(gg_fig, pomo_over, "blend", compose_args = "15")
  }
  
  # Paint background
  if (file.exists(pomological_background)) {
    pomo_bg <- magick::image_read(pomological_background)
    pomo_bg <- magick::image_resize(pomo_bg, paste0(width, "x", height, "!"))
    pomo_bg <- magick::image_crop(pomo_bg, paste0(width, "x", height))
    
    # Paint figure onto background
    pomo_img <- magick::image_composite(pomo_bg, gg_fig)
  } else pomo_img <- gg_fig
  
  if (!is.null(outfile)) {
    # Do you want your picture framed?
    magick::image_write(pomo_img, outfile)
  }
  pomo_img
}

pomological_images <- function(which = c("background", "overlay")) {
  which <- match.arg(which)
  exts <- c("background" = ".png", "overlay" = ".jpg")
  system.file("images", paste0("pomological_", which, exts[which]),
              package = "ggpomological")
}

test <- paint_pomological(plot, outfile = "test_plot.png", height = 650, width = 1200)

part_time$row
rev(part_time$row)
extrafont::font_import()
