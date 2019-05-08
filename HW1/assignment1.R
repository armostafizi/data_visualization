# Uncomment the following line if you don't have these packages installed:
#       tidyverse
#       ggplot2
#       ggplot2movies
#       hexbin

#install.packages("ggplot2movies", "hexbin", "ggplot2", "tidyverse")
library(ggplot2movies)
library(ggplot2)
data(movies)
?movies

                        ####### First Question: Movie counts ########
## Is there any trend in the number of movies made over the year?
## Which genres are made more and which less?

# The first plot is to just look at the number of movies made over the years,
ggplot(data = movies) +
  geom_bar(mapping = aes(x = year))
# This plot shows an increase in the number of movies made over the years,
# especially in the 30s, and then 80s and 90s.

# Now we have to look into the percentages of the total movies that belong to each genre.
# However, since a movie can belong to multiple categories, reading the next plot is almost impossible
# This plot shows the proprition of the movies that belong to a specific combination of genres.
ggplot(data = movies) +
  geom_bar(mapping = aes(x = year, fill = paste0(Action,
                                                 Drama,
                                                 Romance,
                                                 Comedy,
                                                 Documentary,
                                                 Short))) +
  xlim(1900, 2010)
# As you can see, there are too many different combinations, and thus,colors.
# The interpreting this plot is very hard.

# Same problem when another positioning is used.
ggplot(data = movies) +
  geom_bar(mapping = aes(x = year, fill = paste0(Action,
                                                 Drama,
                                                 Romance,
                                                 Comedy,
                                                 Documentary,
                                                 Short)), position = "dodge") +
  xlim(1900, 2010)

# We can compare the proportion of Action and Non-action or Drama and Non-drama movies
ggplot(movies, aes(x = year)) +  
  geom_bar(aes(color = factor(Action), fill = factor(Action)))
ggplot(movies, aes(x = year)) +  
  geom_bar(aes(color = factor(Drama), fill = factor(Drama)))
# But we cannot compare all genres together at the same plot with this type of plot



# Therefore, I decided to combine a barplot showing the number of movies in total,
# with the number of movies that are labeled with each genre, showed with geom_line.
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



                    ####### Second Question: Rating vs. Budget #######
## Is there a relationship between the movie budget and its rating?
## Are highly budgeted movies rated higher?

ggplot(data = movies) +
  geom_point(mapping = aes(x = budget, y = rating))

# This graph is really hard to interpret. There seems to be a
# positive relationship, but it's hard to say with this graph.
# Let's see how geom_smooth works

ggplot(data = movies) +
  geom_point(mapping = aes(x = budget, y = rating)) +
  geom_smooth(mapping = aes(x = budget, y = rating))


# Still hard to say. There seems to be a negative relationship, and then a very slight positive.
# A logarithmic scale might help.

ggplot(data = movies) +
  geom_point(mapping = aes(x = budget, y = rating)) +
  scale_x_log10()

# Even with the logarithmic scale, there seems to be no
# solid relationship. Let's add a geom smooth layer

ggplot(data = movies) +
  geom_point(mapping = aes(x = budget, y = rating)) +
  geom_smooth(mapping = aes(x = budget, y = rating)) +
  scale_x_log10()

ggsave("budget_rating.pdf", height = 6, width = 15)


# Another way to answer this question is the following plot. It shows the high nunber of low
# budgeted, but highly rated movies.
ggplot(data = subset(movies, budget < 2.5e+7), aes(budget, rating))+
  geom_bin2d()
# or this:
ggplot(data = subset(movies, budget < 2.5e+7), aes(budget, rating))+
  geom_count(position = "jitter")

# But it looks like that there are more data points for high budgets,
# than low budget. Let's look at the distribution of the data over
# different budget levels

ggplot(movies, aes(budget)) +
  geom_histogram() +
  scale_x_log10()

# It looks that most of the movies that exist in the dataset, have high budgets.



                        ####### Third question: Good old movies? #######
## Do viewers like older movies better?
## Are the older movies rated higher or lower?

# This figure shows a boxplot for each year, showing the average and range of the ratings for,
# movies of that year
ggplot(data = movies) +
  geom_boxplot(mapping = aes(x = factor(year), y = rating))
# However, it's hard to read as the boxplots are narrow

# Another representation of for the same question,
# This plot shows the high number of ratings around 7 for older movies,
ggplot(data = movies, aes(cut_number(year,25), rating))+
  geom_hex()
# But its hard to quantify the direct answer to the question from these results

# Going back to the boxplots, we can group the years, in two different ways
ggplot(data = movies) +
  geom_boxplot(mapping = aes(x = cut_number(year, 10), y = rating))

ggplot(data = movies, mapping = aes(x = cut_interval(year, 8), y = rating)) +
  geom_boxplot()

# Adding a geom_smooth would help see the trend in a better way
ggplot(data = movies, mapping = aes(x = cut_number(year, 10), y = rating)) +
  geom_boxplot() +
  geom_smooth(aes(group=1))

ggsave("old_movies.pdf", height = 6, width = 15)



                                  ###########################
## MISC
## These are just miscelaneous plots to explore interesting insights from the data

# This plot shows that the animations are rated much higher than regular movies
ggplot(data = movies) +
  geom_boxplot(mapping = aes(x = factor(Animation), y = rating))

# This plot shows that on average Documentary movies are way cheaper to make than non-documentaries
# and they have much lower budgets.
ggplot(movies, aes(budget, fill = factor(Documentary), colour = factor(Documentary))) +
  geom_density(alpha = 0.1) +
  scale_x_log10()

# This plot shows the same statement for Short films
ggplot(movies, aes(budget, fill = factor(Short), colour = factor(Short))) +
  geom_density(alpha = 0.1) +
  scale_x_log10()