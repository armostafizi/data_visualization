#### Animation/Interaction Self Directed Study

# For this assignment, I plan to study the house price distribution, varried by the year
# that the house was built. To do this, I'm going to use the house price dataset that I
# previously used for Assignment 3 and I possibly use it again for Assignment 4.

# The data and its description can be found here:
# https://www.kaggle.com/c/house-prices-advanced-regression-techniques

# Libraries ----------
library(tidyverse)
library(ggplot2)
library(viridis)
library(scales)

# Working Dir ------- 
# Setting the working dir to the current dir
setwd(dirname(parent.frame(2)$ofile))

# Data import -----------------------------
# Downloaded date: 5/17/2018
# Data Sescription,
# https://www.kaggle.com/c/5407/download/data_description.txt
data <- read_csv('train.csv')
# Replace NA values with 0
data <- data %>%
  replace(., is.na(.), 0)

# Data Manipulation -------
# Add Total Area as defined in Assignemnt 3
data <- data %>%
  mutate(TotalArea = `1stFlrSF` + `2ndFlrSF` + `3SsnPorch` +
           TotalBsmtSF + EnclosedPorch +
           MasVnrArea + OpenPorchSF)
# select the features of interest
data <- data %>%
          select(TotalArea, SalePrice, OverallQual, BedroomAbvGr)

# Creating Plots --------
# This is one of the original plots that I had in Assignemnt 3 that I'm trying to improve
# The plot shows the relationship between total area, sale price, adn the overal quality
ggplot(data) +
  geom_point(aes(TotalArea, SalePrice, color = OverallQual), alpha = 0.7) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = dollar) +
  coord_cartesian(xlim = c(0, 7500), ylim = c(0, 500000)) +
  scale_color_viridis() +
  scale_fill_viridis() +
  theme_minimal(18)

# I'm going to use the tweener pakcage to animate the relationship between total area
# and sale price as the overal quality increases.

# For this, I plan to use GGANIMATE and TWEENR packages.
# Installing gganimate
#install.packages("animation")
#install.packages("devtools")
#devtools::install_github("dgrtwo/gganimate")

library(animation)
library(gganimate)

# Let's create the ggplots and then animate the plots using gganimate
p <- ggplot(data, aes(frame = OverallQual)) +
      #geom_point(aes(TotalArea, SalePrice, color = OverallQual, size = YearBuilt), alpha = 7 / 10) +
      geom_point(aes(TotalArea, SalePrice, color = OverallQual, size = BedroomAbvGr), alpha = 7 / 10) +
      scale_x_continuous(labels = comma) +
      scale_y_continuous(labels = dollar) +
      coord_cartesian(xlim = c(0, 7500), ylim = c(0, 500000)) +
      scale_color_viridis() +
      scale_fill_viridis() +
      theme_minimal(18)

# This needs ImageMagick to be installed
#magickPath <- shortPathName("C:\\Program Files\\ImageMagick-7.0.7-Q16\\magick.exe")
gganimate(p = p, filename = "gganimate.gif")

# Now let's try tweenr for smoother animation
# installing tweenr
#install.packages("tweenr")
library(tweenr)

# Using tweenr here is a little tricky, as not all the points are transitioned in each frame
# so we have to have as many points as needed and set their alphas to zero for the frames that
# they are not needed. Thus we have to manipulate the data as following

# add ids to each group
data_tweenr <- data %>%
  arrange(OverallQual, SalePrice, TotalArea) %>%
  group_by(OverallQual) %>%
  mutate(init_id = row_number())

# max of id
max_id <- max(data_tweenr$init_id)

# add the number of each row has to be repeated,
# to later on use to assign the observation ids
data_tweenr <- data_tweenr %>%
                mutate(repet = ifelse(init_id == n(), max_id - init_id + 1, 1))

# repeat the rows function
row_rep <- function(df) {
  df[rep(1:nrow(df), times = df$repet),]
}

# repeat the rows
replicated_data_tweenr <- row_rep(data_tweenr)

# assign alpha of zero for the observations that have to be hidden
replicated_data_tweenr <- replicated_data_tweenr %>%
  group_by(OverallQual) %>%
  mutate(sec_id = row_number()) %>%
  mutate(alpha = ifelse((sec_id > init_id), 0, 0.7))

# tweenr formatting
data_tweenr <- replicated_data_tweenr %>%
  rename(x=TotalArea, y=SalePrice, time=OverallQual, id=sec_id) %>%
  mutate(ease="linear")

# tweenr elements
data_tweenr <- tween_elements(data_tweenr, "time", "id", "ease", nframes = 300) %>%
                mutate(OverallQual = time)

# make animated chart
data_tween_chart <- ggplot(data_tweenr, aes(x = x, y = y, alpha = alpha, frame = .frame, color = OverallQual, size = BedroomAbvGr)) +
  geom_point() +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = dollar) +
  coord_cartesian(xlim = c(0, 7500), ylim = c(0, 500000)) +
  scale_color_viridis() +
  scale_fill_viridis() +
  theme_minimal(18) +
  xlab("Total Area") +
  ylab("Sale Price") +
  scale_alpha(guide = 'none')
  
# make the animation
gganimate(p = data_tween_chart, filename = "gganimate_smooth.gif", interval = 0.05)
