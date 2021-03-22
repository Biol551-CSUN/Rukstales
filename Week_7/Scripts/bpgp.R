#Bad plot Good plot


library(tidyverse)
library(here)
library(tidytuesdayR)
library(jpeg)
library(ggpubr)
library(tvthemes)

tuesdata <- tt_load('2020-08-11')
avatar <- tuesdata$avatar %>%
  select("book", "book_num", "chapter", "chapter_num", "character", "imdb_rating")


##### BAD PLOT ###########

cabbages_image <- readJPEG(here("Week_7", "Data", "cabbages_edited.jpg"))

avatar_summary <- avatar %>%
  group_by(book, book_num, chapter, chapter_num) %>%
  summarize(imdb_mean = mean(imdb_rating))

badplot <- avatar_summary %>%
  ggplot(aes(x = chapter_num,
             y = imdb_mean,
             size = book)) +
  background_image(cabbages_image) +
  geom_point(color = "green") +
  geom_text(aes(label = chapter),
            hjust = 0,
            size = 3.5,
            color = "red") +
  theme(plot.background = element_rect(fill = "purple"),
        legend.background = element_blank(),
        legend.key = element_rect(colour = "transparent", fill = "transparent")) +
  scale_x_continuous(breaks = seq(1, 22, 0.5)) +
  labs(y = "averaeg",
       x = "Chapter",
       size = "legend",
       title = "avatar episode ratings :-)") +
  ggsave(here("Week_7", "Outputs", "atla_bad.png"),
         width = 10.2, height = 6.26)

badplot



##### GOOD PLOT ###############
#one plot, no facets
#book on x-axis
#imdb rating on y-axis
#color by book
#use symbols for points??

goodplot <- avatar_summary %>%
  ggplot(aes(x = book,
             y = imdb_mean,
             color = book)) +
  geom_dotplot(aes(fill = book),
               binaxis = "y",
               stackdir = "center",
               dotsize = 0.9) +
  geom_violin(aes(fill = book),
              alpha = 0.4) +
  theme_bw() +
  theme(panel.grid.major.y = element_line(color = "#dbd2c1"),
        axis.text.x = element_text(size = 12),
        legend.position = "none",
        panel.background = element_blank(),
        plot.background = element_rect(fill = "#ede6d8"),
        plot.title = element_text(size = 16),
        plot.margin = margin(20, 30, 20, 20)) +
  scale_y_continuous(breaks = seq(7, 10, 0.5)) +
  scale_x_discrete(limits = c("Water", "Earth", "Fire")) +
  scale_color_manual(values = c("Water" = "#41799e", #choose colors for each theme
                               "Earth" = "#809c52",
                               "Fire" = "#ab2d24"),
                    breaks = c("Water", "Earth", "Fire")) +
  scale_fill_manual(values = c("Water" = "#41799e", #choose colors for each theme
                               "Earth" = "#809c52",
                               "Fire" = "#ab2d24"),
                    breaks = c("Water", "Earth", "Fire")) +
  annotate("text", x = 0.75, y = 9.6, size = 3, lineheight = .9,
           label = "The Siege of the North, Part 1") +
  geom_curve(aes(x = 0.8, y = 9.55, xend = 0.97, yend = 9.4),
             arrow = arrow(length = unit(0.07, "inch")), size = 0.5, curvature = 0.35, color = "black") +
  annotate("text", x = 0.76, y = 7.3, size = 3, lineheight = .9,
           label = "The Great Divide") +
  geom_curve(aes(x = 0.8, y = 7.25, xend = 0.97, yend = 7.1),
             arrow = arrow(length = unit(0.07, "inch")), size = 0.5, curvature = 0.35, color = "black") +
  annotate("text", x = 1.55, y = 9.5, size = 3, lineheight = .9,
           label = "The Crossroads of Destiny") +
  geom_curve(aes(x = 1.6, y = 9.55, xend = 1.97, yend = 9.62),
             arrow = arrow(length = unit(0.07, "inch")), size = 0.5, curvature = -0.4, color = "black") +
  annotate("text", x = 1.75, y = 7.3, size = 3, lineheight = .9,
           label = "Avatar Day") +
  geom_curve(aes(x = 1.86, y = 7.29, xend = 2, yend = 7.45),
             arrow = arrow(length = unit(0.07, "inch")), size = 0.5, curvature = 0.35, color = "black") +
  annotate("text", x = 2.58, y = 7.5, size = 3, lineheight = .9,
           label = "Nightmares and Daydreams") +
  geom_curve(aes(x = 2.85, y = 7.49, xend = 3, yend = 7.75),
             arrow = arrow(length = unit(0.07, "inch")), size = 0.5, curvature = 0.35, color = "black") +
  annotate("text", x = 2.53, y = 9.55, size = 3, lineheight = .9,
           label = "Sozin's Comet, Parts 3 & 4 \n(series finale)") +
  geom_curve(aes(x = 2.55, y = 9.64, xend = 2.95, yend = 9.82),
             arrow = arrow(length = unit(0.07, "inch")), size = 0.5, curvature = -0.3, color = "black") +
  geom_curve(aes(x = 2.55, y = 9.64, xend = 3.01, yend = 9.85),
             arrow = arrow(length = unit(0.07, "inch")), size = 0.5, curvature = -0.45, color = "black") +
  labs(x = "",
       y = "IMDb Rating \n",
       title = "Avatar: The Last Airbender",
       subtitle = "IMDb ratings of all chapters by book; best- and worst-rated chapters are labeled",
       fill = "Book",
       color = "Book") +
  ggsave(here("Week_7", "Outputs", "atla_good.png"),
         width = 10.2,
         height = 6.26)

goodplot








