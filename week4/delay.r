# install.packages("viridis")
library(viridis)
library(ggplot2)
iah <- read.csv("http://vis.cwick.co.nz/data/iah-summary.csv")

# make sure days of week are displayed in the right order
iah$DayOfWeek <- factor(iah$DayOfWeek, 
                        levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))


ggplot(iah, aes(DepHour, DayOfWeek)) +
  geom_tile(aes(fill = prop_over_15)) +
  scale_fill_viridis("Proportion", option = "A") +
  theme_dark()