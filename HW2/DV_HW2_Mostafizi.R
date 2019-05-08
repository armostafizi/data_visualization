# Uncomment the following line if you don't have these packages installed:
#       tidyverse
#       ggplot2
#       ggplot2movies
#       dplyr
#       viridis
#       ggrepel

#install.packages("ggplot2movies", "ggplot2", "tidyverse", "dplyr", "viridis", "ggrepel")
library(ggplot2movies)
library(ggplot2)
library(dplyr)
library(viridis)
library(tidyverse)
library(ggrepel)

data(movies)
?movies

####### ORIGINAL PLOT ########
# This code generates the original plot which intends to answer the following questions:
# 1. Is there any trend in the number of movies made over the year?
# 2. Which genres are made more and which less?

ggplot(movies, aes(x = year)) +  
  geom_bar(fill = "deepskyblue1", color = "deepskyblue1") +
  geom_line(data = subset(movies, Action == 1), stat = "count", aes(color = "brown"), size = 1) +
  geom_line(data = subset(movies, Drama == 1), stat = "count", aes(color = "green"), size = 1) +
  geom_line(data = subset(movies, Romance == 1), stat = "count", aes(color = "orange"), size = 1) +
  geom_line(data = subset(movies, Comedy == 1), stat = "count", aes(color = "pink"), size = 1) +
  geom_line(data = subset(movies, Documentary == 1), stat = "count", aes(color = "purple"), size = 1) +
  geom_line(data = subset(movies, Short == 1), stat = "count", aes(color = "yellow"), size = 1) +
  scale_color_manual("Genre", values=c(red="red",purple="purple",green="green",yellow="yellow",brown="brown",pink="pink",orange="orange",blue="blue"),
                              labels=c("Action", "Drama", "Romance", "Comedy", "Documentry", "Short", "Animation")) +
  theme_minimal()

ggsave("movie_counts.pdf", height = 6, width = 15)


##### POLISHING ########

# To polish the data, we need to modify and tidy up the data a little
# The following lines include my experiments with dplyr pakcages on how to tidy up the data:

movies %>% head()
movies %>% count(year, Action, Animation, Comedy, Drama, Documentary, Romance, Short)

a <- movies %>% 
  count(year, Action_gnr, Animation, Comedy, Drama, Documentary, Romance, Short)

b <- movies %>% 
  count(year)

movies %>% 
  count(year, Action, Animation, Comedy, Drama, Documentary, Romance, Short) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(total_year = sum(n))

movies %>% 
  count(year, Action, Animation, Comedy, Drama, Documentary, Romance, Short) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(total_year = sum(n)) %>%
  mutate(percentage = n / total_year * 100.)

movies %>% 
  count(year, Action, Animation, Comedy, Drama, Documentary, Romance, Short) %>%
  mutate_at(funs(.*n), .vars =  c("Action", "Animation", "Comedy", "Drama", "Documentary", "Romance", "Short"))


movies %>% 
  count(year, Action, Animation, Comedy, Drama, Documentary, Romance, Short) %>%
  mutate_at(funs(.*n), .vars =  c("Action", "Animation", "Comedy", "Drama", "Documentary", "Romance", "Short")) %>%
  group_by(year) %>%
  summarise_all(funs(sum))

movies %>% 
  count(year, Action, Animation, Comedy, Drama, Documentary, Romance, Short) %>%
  mutate_at(funs(.*n), .vars =  c("Action", "Animation", "Comedy", "Drama", "Documentary", "Romance", "Short")) %>%
  group_by(year) %>%
  summarise_all(funs(sum))

movies %>% 
  count(year, Action, Animation, Comedy, Drama, Documentary, Romance, Short) %>%
  mutate_at(funs(.*n), .vars =  c("Action", "Animation", "Comedy", "Drama", "Documentary", "Romance", "Short")) %>%
  group_by(year) %>%
  summarise_all(funs(sum)) %>%
  mutate_at(funs(./n*100.), .vars =  c("Action", "Animation", "Comedy", "Drama", "Documentary", "Romance", "Short"))


# Now the data is in a better format. Let's make some plots with it.
new_movies <- movies %>% 
  count(year, Action, Animation, Comedy, Drama, Documentary, Romance, Short) %>%
  mutate_at(funs(.*n), .vars =  c("Action", "Animation", "Comedy", "Drama", "Documentary", "Romance", "Short")) %>%
  group_by(year) %>%
  summarise_all(funs(sum)) %>%
  mutate_at(funs(./n*100.), .vars =  c("Action", "Animation", "Comedy", "Drama", "Documentary", "Romance", "Short"))

# to show the total number of movies made in each year
ggplot(data=new_movies) +
  geom_line(mapping = aes(x = year, y = n))

# we still need to gather the genre variable
new_movies %>%
  gather(Action:Short, key = "Genre", value = "Percentage")

new_new_movies <- new_movies %>%
  gather(Action:Short, key = "Genre", value = "Percentage")

# Now lets plot the proportions for every year  
ggplot(data = new_new_movies) +
  geom_line(mapping = aes(x = year, y = Percentage, color = Genre))
# This plot is too noisy and cluttered. We can plot for each 10 years.

## Plotting for every 10 years (just the year)
new_new_movies <- new_movies %>%
  gather(Action:Short, key = "Genre", value = "Percentage") %>%
  filter(year %% 10 == 0)

ggplot(data = new_new_movies) +
  geom_line(mapping = aes(x = year, y = Percentage, color = Genre))

## Instread of plotting every 10 years, we can plot the proportion of the movies made in the entire decade.
## percentage over a decade (10 years)
new_movies <-movies %>% 
  count(year, Action, Animation, Comedy, Drama, Documentary, Romance, Short) %>%
  mutate_at(funs(.*n), .vars =  c("Action", "Animation", "Comedy", "Drama", "Documentary", "Romance", "Short")) %>%
  group_by(year) %>%
  summarise_all(funs(sum)) %>%
  mutate(decade = year - year %% 10) %>%
  group_by(decade) %>%
  summarise_all(funs(sum)) %>%
  select(-one_of("year")) %>%
  mutate_at(funs(./n*100.), .vars =  c("Action", "Animation", "Comedy", "Drama", "Documentary", "Romance", "Short"))

new_movies %>%
  gather(Action:Short, key = "Genre", value = "Percentage")

new_new_movies <- new_movies %>%
  gather(Action:Short, key = "Genre", value = "Percentage")

# Plotting the entire decade
ggplot(data = new_new_movies) +
  geom_line(mapping = aes(x = decade, y = Percentage, color = Genre)) +
  xlim(1930, 1990) +
  ylim(0, 50)
# This tells a better story


# Exploring with the points, bars, and lines to figure out which represents the data better
ggplot(data = new_new_movies) +
  geom_point(mapping = aes(x = decade, y = Percentage, color = Genre)) +
  xlim(1930, 1990) +
  ylim(0, 50)

ggplot(data = new_new_movies) +
  geom_col(mapping = aes(x = decade, y = Percentage, fill = Genre), position = position_dodge()) +
  xlim(1930, 1990) +
  ylim(0, 50)

ggplot(data = new_new_movies) +
  geom_line(mapping = aes(x = decade, y = Percentage, color = Genre)) +
  geom_point(mapping = aes(x = decade, y = Percentage, color = Genre)) +
  xlim(1930, 1990) +
  ylim(0, 50)

# I decided that the lines are a better represantative. Now, real polishing.

###### REAL POLISHING

new_movies <-movies %>% 
  count(year, Action, Animation, Comedy, Drama, Documentary, Romance, Short) %>%
  mutate_at(funs(.*n), .vars =  c("Action", "Animation", "Comedy", "Drama", "Documentary", "Romance", "Short")) %>%
  group_by(year) %>%
  summarise_all(funs(sum)) %>%
  mutate(decade = year - year %% 10) %>%
  group_by(decade) %>%
  summarise_all(funs(sum)) %>%
  select(-one_of("year")) %>%
  mutate_at(funs(./n*100.), .vars =  c("Action", "Animation", "Comedy", "Drama", "Documentary", "Romance", "Short")) %>%
  select(-one_of("Romance")) %>%
  filter(decade >= 1940) %>%
  filter(decade <= 2000)

new_movies %>%
  gather(Action:Short, key = "Genre", value = "Percentage")

new_new_movies <- new_movies %>%
  gather(Action:Short, key = "Genre", value = "Percentage")

# To pinpoint the placement of the legends (annotations)
max_point <- new_new_movies %>%
  slice(c(7, 14, 21, 28, 35, 42))

########################################## THE POLISHED PLOT ###############################333
ggplot(data = new_new_movies) +
  geom_path(mapping = aes(x = decade, y = Percentage, color = Genre), size = 0.8) +
  geom_point(mapping = aes(x = decade, y = Percentage, color = Genre), size = 1.5) +
  ggrepel::geom_label_repel(data = max_point,
                            mapping = aes(x = decade, y = Percentage, label = Genre, color = Genre),
                            force = 0.01) +
  scale_color_viridis(discrete = TRUE) +
  labs(
    title = "Proportion of the movies labled with each genre from 1940s to 2000s",
    subtitle = "Less Comedies and Animations, and More Dramas and Documentaries!\nShort films are back, and Action movies are saying goodbye!",
    caption = "Data from IMDB (ggplot2movies library)",
    x = "Year",
    y = "Percentage (%)",
    colour = "Genre"
  ) +
  scale_x_continuous(breaks = seq(1940, 2000, by = 20), limits = c(1940, 2005),
                     sec.axis = sec_axis(~., name = "Total number of Movies per year",  labels = c("503", "478", "681", "2048"))) +
  scale_y_continuous(breaks = seq(0, 50, by = 10), limits = c(0, 42)) +
  guides(color = "none") + 
  theme_classic() +
  theme(axis.line.y = element_blank(),
        axis.text.x = element_text(face = "bold", size = 11),
        axis.text.y = element_text(face = "bold", size = 11),
        axis.title = element_text(face = "bold", size = 11)
        #title = element_text(face = "bold", size = 14),
        ) +
  coord_equal()

ggsave("movies_new.pdf", height = 6)
