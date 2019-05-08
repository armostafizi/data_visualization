library(ggplot2)
iah <- read.csv("http://vis.cwick.co.nz/data/iah-summary.csv")

# make sure days of week are displayed in the right order
iah$DayOfWeek <- factor(iah$DayOfWeek, 
                        levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

ggplot(iah, aes(DepHour, DayOfWeek)) +
  geom_tile(aes(fill = prop_over_15)) +
  scale_fill_gradient("Proportion", high = "white", low = "springgreen4")

ggplot(iah, aes(DepHour, DayOfWeek)) +
  geom_tile(aes(fill = prop_over_15)) +
  scale_fill_gradient("Proportion", high = "red", low = "green")


RColorBrewer::display.brewer.all()

ggplot(iah, aes(DepHour, DayOfWeek)) +
  geom_tile(aes(fill = prop_over_15)) +
  scale_fill_distiller(palette = "YlOrRd")

?scale_fill_gradient2

ggplot(iah, aes(DepHour, DayOfWeek)) +
  geom_tile(aes(fill = prop_over_15)) +
  scale_fill_gradient2("Proportion", low = "green", midpoint = 0.5, mid = "white", high = "red")

# discretize average delay
iah$avg_delay_cut <- with(iah,
                          cut(avg_delay, breaks = c(-5, 0, 15, 30, 60, 1000)))

?scale_fill_grey

ggplot(iah, aes(DepHour, DayOfWeek)) +
  geom_tile(aes(fill = avg_delay_cut)) +
  scale_fill_grey()


# install.packages("viridis")
library(viridis)

ggplot(iah, aes(DepHour, DayOfWeek)) +
  geom_tile(aes(fill = prop_over_15)) +
  scale_fill_viridis("Proportion", option = "A") +
  theme_dark()


ggplot(iah, aes(DepHour, DayOfWeek)) +
  geom_tile(aes(fill = avg_delay_delayed)) +
  scale_fill_gradient()

ggplot(iah, aes(DepHour, DayOfWeek)) +
  geom_tile(aes(fill = avg_delay_delayed)) +
  scale_fill_gradient(limits = c(0, 120))

ggplot(iah, aes(DepHour, DayOfWeek)) +
  geom_tile(aes(fill = avg_delay_delayed)) +
  scale_fill_gradient(trans = "log")

ggplot(iah, aes(DepHour, DayOfWeek)) +
  geom_tile(aes(fill = log(avg_delay_delayed))) +
  scale_fill_gradient()


##### POLISHED

library(ggplot2)
iah <- read.csv("http://vis.cwick.co.nz/data/iah-summary.csv")

# reorder days of week - Monday at top
iah$DayOfWeek <- factor(iah$DayOfWeek, 
                        levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

ggplot(iah, aes(DepHour, DayOfWeek)) +
  geom_tile(aes(fill = prop_over_15), colour = "grey50") + 
  scale_x_continuous("Departure time", 
                     breaks = c(0, 6, 12, 18, 24),
                     labels = c("midnight", "6am", "noon", "6pm", "midnight"),
                     expand = c(0, 0),
                     limits = c(0, 23)) +
  scale_y_discrete("Departure day") +
  scale_fill_distiller("Flights delayed \nmore than 15 mins", 
                       palette = "YlOrRd", direction = 1,
                       breaks = c(0, .25, 0.5, .75, 1), 
                       labels = c("0%", "25%", "50%", "75%", "100%"), 
                       expand = c(0, 0),
                       guide = "colorbar") +
  labs(title = "Take an early flight from IAH to avoid delays",
       subtitle = "Based on all departing flights from George Bush Intercontinental Airport (IAH) in 2011") +
  guides(color = "none") + 
  theme_classic() +
  theme(axis.ticks.y = element_blank(), axis.line.y = element_blank()) + coord_equal()
ggsave("08-polished.png", height = 3.5, width = 10)


### OUR POLISHED

library(ggplot2)
library(dplyr)
delays <- read.csv("http://vis.cwick.co.nz/data/all-summary.csv")
# see: 04-get-delay-summary.R for details of summarization

delays$DayOfWeek <- factor(delays$DayOfWeek, 
                           levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))


delays <- na.omit(delays)
delays$Timeofday<-cut(delays$DepHour, c(0,3,6,9,12,15,18,21,24), right=FALSE, 
                      labels=c("Midnight-3:00 am", "3:00 am-6:00 am", "6:00 am-9:00 am", 
                               "9:00 am-12:00 pm", "12:00 pm-3:00 pm", "3:00 pm-6:00 pm",
                               "6:00 pm-9:00 pm","9:00 pm-Midnight"))


c <- delays %>%
  group_by(Timeofday, Origin)%>%
  summarise(avedelay = mean(avg_delay), na.rm=TRUE)


ggplot(c, aes(Timeofday, Origin)) +
  geom_tile(aes(fill = log(avedelay- min(avedelay) + 1))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  xlab("Time of Day")+ylab("Average Delay Time")+ ggtitle("Title")+
  scale_fill_gradient(high = "blue", low = "yellow")


