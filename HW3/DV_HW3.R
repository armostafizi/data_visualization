library(tidyverse)
library(ggplot2)
library(scales)
library(reshape2)
library(GGally)
library(viridis)

# Setting the working dir to the current dir
setwd(dirname(parent.frame(2)$ofile))

# Data import -----------------------------
  
# importing the data, downloaded from
# https://www.kaggle.com/c/house-prices-advanced-regression-techniques/download/train.csv
# Downloaded date: 5/17/2018
# You can find the data description here:
# https://www.kaggle.com/c/5407/download/data_description.txt

data <- read_csv('train.csv')
#data <- read_csv(url('https://www.kaggle.com/c/5407/download/train.csv'))

# I checked the data, and there are many NA value. Mostly not due to data collection,
# but because the variable is not applicable to a specific observation. For instance,
# pool size is not applicable to a house that does not have a pool. So, I decided to 
# set the NAs to zero.

# Replace NA values with 0
data <- data %>%
          replace(., is.na(.), 0)

# This dataset aims to study the correlation between the price of a house,
# and its characteristics

# Initial Explorations -------------------

# The distribution of the house price

ggplot(data) +
  geom_histogram(aes(SalePrice))
# This shows the concentration of the prices around 150k
# To see better, we can change the lables and decrease the bin sizes


ggplot(data) +
  geom_histogram(aes(SalePrice), bins = 200) +
  scale_x_continuous(labels = comma)
# Notice the outliers beyond 500k

# As an example, we can see the correlation between each two features with this
ggpairs(data, columns = 1:10, title = "",  
        axisLabels = "show", columnLabels = colnames(data[, 1:10]))
# but it's hard to read, so I decide to plot all the features veruse
# only the target value which is the sales price


## This will take some time! UNCOMMENTS IF NEEDED ...
# data %>% 
#   gather(variable, value, -SalePrice) %>%
#   ggplot(aes(value, SalePrice)) + 
#   geom_point() + 
#   facet_wrap(~variable, ncol = 9, nrow = 9)


# This clears if there is any correlation between any features and the price,
# but to see it better, lets remove the axis labels
# Also the x axis should not be fixed!

## This will take some time! UNCOMMENTS IF NEEDED ...
# data %>%
#   gather(variable, value, -SalePrice) %>%
#   ggplot(aes(value, SalePrice)) + 
#   geom_point() + 
#   theme(axis.title.x=element_blank(),
#         axis.text.x=element_blank(),
#         axis.ticks.x=element_blank(),
#         axis.title.y=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank()) +
#   facet_wrap(~variable, ncol = 9, nrow = 9, scales = "free_x")


# From this plot, we choose a subset of features that clearly are correlated
# with the price, and also are interesting!

# The variables of interest:
#     Total Area      Building Type     Central Air     Condition1 and 2
#     Total Bath       GrLivArea         Heating         House Style
#     Lot Area        Overall Condition Overall Quality Pool Quality
#     Sale Condition  Sale Type         Total Rooms     YearBuilt

# Let's keep just these variables and drop the rest.
# Also, I assume that the Total Area of the house equals to the summation
# of the area of all the stories including the basement, porch, etc.

# making the total area variable
data <- data %>%
          mutate(TotalArea = `1stFlrSF` + `2ndFlrSF` + `3SsnPorch` +
                             TotalBsmtSF + EnclosedPorch +
                             MasVnrArea + OpenPorchSF)

# making total bathroom variable
data <- data %>%
          mutate(TotalBath = FullBath + (0.5 * HalfBath))

# making "years since remodeled" variable
data <- data %>%
          mutate(YearSinceRemodeled = YrSold - YearRemodAdd)

# selecting the interesting features
data <- data %>%
          select(TotalArea, BldgType, CentralAir, Condition1, Condition2,
                 TotalBath, GrLivArea, Heating, HouseStyle, LotArea,
                 OverallCond, OverallQual, PoolQC, SaleCondition, SaleType,
                 TotRmsAbvGrd, YearBuilt, YearSinceRemodeled, YrSold, SalePrice)

# Now let's plot the same correlation plots
data %>% ## This will take some time!
  gather(variable, value, -SalePrice) %>%
  ggplot(aes(value, SalePrice)) + 
  geom_point() + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  facet_wrap(~variable, ncol = 5, nrow = 4, scales = "free_x")

# We can also see the correlation between all these 20 features together
fact_data <- data %>% mutate_if(is.character, as.factor)
num_data <- fact_data %>% mutate_if(is.factor, as.numeric)
cormat <- round(cor(num_data),2)
melted_cormat <- melt(cormat)

ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# I don't see any correlation that I didn't expect!


# Exploration ---------------------------

# The first guess is that there is a positive correlation between
# House price and the lot area, so

### FIRST QUESTION
# Is there any relationship between house price and its size?
# Either Lot Area or Total Area?

# I expect a strong positive relationship.
ggplot(data) +
  geom_point(aes(LotArea, SalePrice), color = 'blue') +
  geom_point(aes(TotalArea, SalePrice), color = 'red') +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma)
# Notice the positive correlation with both lot and total area
# Also, the outliers
# Let's remove the outliers, and check again


ggplot(data) +
  geom_point(aes(LotArea, SalePrice), color = 'blue') +
#  geom_smooth(aes(LotArea, SalePrice), color = 'blue') +
  geom_point(aes(TotalArea, SalePrice), color = 'red') +
#  geom_smooth(aes(TotalArea, SalePrice), color = 'red') +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  coord_cartesian(xlim = c(0, 50000), ylim = c(0, 500000)) +
  # polish a little to add to the repot
  theme_minimal(18) + xlab("Area")
# Notice the stronger relationship between the total size,
# as opposed to lot area
# Lets use color as representation of the overal quality of the house
# I expect a positive correlation between overal quality and the price

ggplot(data) +
  geom_point(aes(TotalArea, SalePrice, color = OverallQual)) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  coord_cartesian(xlim = c(0, 7500), ylim = c(0, 500000))
# It looks like houses with different quality levels have
# different realtionships.
# Also, it looks like that the bigger houses have higher overal quality.
# Let's briefly check this.

ggplot(data) +
  geom_boxplot(aes(factor(OverallQual), TotalArea))
# This plot confirms the above hypothesis

# We can also see different relationships if we use facet_wrap on the quality
ggplot(data) +
  geom_point(aes(TotalArea, SalePrice, color = OverallQual)) +
#  geom_smooth(aes(TotalArea, SalePrice)) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  coord_cartesian(xlim = c(0, 10000), ylim = c(0, 500000)) +
  facet_wrap(~OverallQual) +
  scale_color_viridis()
# This is very interesting! It seems that the impact of the total size of the
# house is high higher for mid-level quality houses. And for very low quality
# and very high quality houses, the effect of the size on price is lower.

#let's see the same plots for Overal Condition,
ggplot(data) +
  geom_point(aes(TotalArea, SalePrice)) +
  geom_smooth(aes(TotalArea, SalePrice)) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  coord_cartesian(xlim = c(0, 10000), ylim = c(0, 500000)) +
  facet_wrap(~OverallCond)
# These plots also show somewhat similar results. Higher impact of size on price
# for mid-level quality houses.

# Let's make the colors more visible
ggplot(data) +
  geom_point(aes(TotalArea, SalePrice, color = OverallQual), alpha = 7 / 10) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  coord_cartesian(xlim = c(0, 7500), ylim = c(0, 500000)) +
  scale_color_viridis() +
  scale_fill_viridis() +
  theme_minimal(18)

# Also, we can see the same plot for different building types.
ggplot(data) +
  geom_point(aes(TotalArea, SalePrice, color = BldgType)) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  coord_cartesian(xlim = c(0, 10000), ylim = c(0, 500000)) +
  facet_wrap(~BldgType)
# The positive relationship persists, but at different levels. For instance,
# The size of duplexes matters less than the size of 1 family houses.

# How about different house styles?
ggplot(data) +
  geom_point(aes(TotalArea, SalePrice, color = HouseStyle)) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  coord_cartesian(xlim = c(0, 10000), ylim = c(0, 500000)) +
  facet_wrap(~HouseStyle)



### SECOND QUESTION
# Are the houses sold/made/remodeled earlier more expensive?

# I think the later the house is sold, the more expensive it should be,
# due to inflmation and the price increase in housing market.
ggplot(data) +
  geom_boxplot(aes(factor(YrSold), SalePrice))
# It does not look like that there is any apparetn correlation
# between the year of the purchase and the price

# How about year built?
# Add the average saleprice for each year to later associate to the color of the boxplot
data$mean <- ave(data$SalePrice, as.factor(data$YearBuilt), FUN=mean)
ggplot(data) +
  geom_boxplot(aes(factor(YearBuilt), SalePrice, fill = mean), lwd = 0, outlier.size = 0.5) +
  scale_fill_viridis() +
  scale_y_continuous(labels = comma) +
  scale_x_discrete(breaks = seq(1900, 2000, by = 20)) +
  theme_minimal(18) +
  coord_cartesian(ylim = c(0, 600000)) +
  xlab("Year Built") +
  theme(legend.position = "none")
# There is a general positive trend

# How about years since the house is remodeled?
data$mean <- ave(data$SalePrice, as.factor(data$YearSinceRemodeled), FUN=mean)
ggplot(data) +
  geom_boxplot(aes(factor(YearSinceRemodeled), SalePrice, fill = mean), lwd = 0, outlier.size = 0.5) +
  scale_fill_viridis() +
  scale_y_continuous(labels = comma) +
  scale_x_discrete(breaks = seq(0, 100, by = 10)) +
  theme_minimal(18) +
  coord_cartesian(ylim = c(0, 600000)) +
  xlab("Years Since Remodeled") +
  theme(legend.position = "none")
# Obvious negative relationship. The more recent the house was remodeled, the
# more expensive. Also, recently remodeled houses have higher variation in price.
# Let's see the number of examples. I suspect the there are not many houses in
# our data that were remodeled long time ago. To do this, we have to manipulate
# the data a bit.

data_summary <- data %>%
                  select(YearSinceRemodeled, SalePrice) %>%
                    group_by(YearSinceRemodeled) %>%
                      summarise(mean = mean(SalePrice), n = n(), `25p` = quantile(SalePrice, probs=0.25),
                                                                 `50p` = quantile(SalePrice, probs=0.50),
                                                                 `75p` = quantile(SalePrice, probs=0.75)) %>%
                        ungroup()

ggplot(data_summary, aes(alpha = n)) +
  geom_point(aes(YearSinceRemodeled, mean)) +
  geom_errorbar(aes(YearSinceRemodeled, ymin = `25p`, ymax = `75p`))
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# Now we can see that there are many recently remodeled houses in our data
  

### THIRD QUESTION
# What conditions makes a house expensive?

# These two plots give an idea on how the conditions affect the price
  ggplot(data) +
    geom_boxplot(aes(reorder(Condition1, SalePrice, FUN=median), SalePrice))
  
  ggplot(data) +
    geom_boxplot(aes(reorder(Condition2, SalePrice, FUN=median), SalePrice))

# But to thoroughly investiage this, we need to combine them together.
# To do this, we have to manipulate the data. The conditions of a house are
# included into two variables of Condition1 and Condition2.

# Extracting the summary of different conditions
data_condition <- data %>%
  select(Condition1, Condition2, SalePrice) %>%
  mutate(Cond = if_else(Condition1 == Condition2,
                        Condition1,
                        trimws(paste(if_else(Condition1 != "Norm", Condition1, ""),
                                     if_else(Condition2 != "Norm", Condition2, ""))))) %>%
    group_by(Cond) %>%
      summarise(mean = mean(SalePrice), n = n(), `25p` = quantile(SalePrice, probs=0.25),
                                                 `50p` = quantile(SalePrice, probs=0.50),
                                                 `75p` = quantile(SalePrice, probs=0.75)) %>%
        ungroup()

# Droppin duplicated conditions
data_condition <- data_condition %>%
                    filter(Cond != "RRNn Feedr") %>%
                      filter(Cond != "Feedr RRAn")

# plotting the average house price with 25 and 75 percentile error bar
# for each condition
ggplot(data_condition, aes(alpha = n)) +
  geom_point(aes(reorder(Cond, mean), mean)) +
  geom_errorbar(aes(Cond, ymin = `25p`, ymax = `75p`)) +
  #theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme_minimal(18) +
  coord_flip() +
  xlab("") + ylab("SalePrice") +
  scale_y_continuous(labels = comma, breaks = c(min(data_condition$mean), max(data_condition$mean))) +
  scale_x_discrete(labels=c("Artery" = "Adjacent to arterial street",
                            "Feedr" = "Adjacent to feeder street"	,
                            "Norm" = "Normal",	
                            "RRNn" =	"Within 200' of North-South Railroad",
                            "RRAn" =	"Adjacent to North-South Railroad",
                            "PosN" =	"Near positive off-site feature--park, greenbelt, etc.",
                            "PosA"	= "Adjacent to postive off-site feature",
                            "RRNe" =	"Within 200' of East-West Railroad",
                            "RRAe" =	"Adjacent to East-West Railroad",
                            "Feedr RRNn" = "Within 200' of EW Railroad and a adjcent to a Feeder st",
                            "RRAn Feedr" = "Adjacent to North-South Railroad and a Feeder st",
                            "Feedr RRAe" = "Adjacent to East-West Railroad and a Feeder st",
                            "Artery PosA"	= "Adjacent to postive off-site feature and an arterial st"))

#### MISC
# Misc plots to explore other features

ggplot(data) +
  geom_point(aes(GrLivArea, SalePrice))

ggplot(data) +
  geom_point(aes(LotArea, SalePrice))

ggplot(data) +
  geom_boxplot(aes(factor(TotalBath), SalePrice))

ggplot(data) +
  geom_boxplot(aes(factor(TotRmsAbvGrd), SalePrice))

# house style - bldngtype - central air - heating - overal cond/qual - sale cond/type

ggplot(data) +
  geom_freqpoly(aes(x = SalePrice, y = ..density.., color = BldgType))

ggplot(data) +
  geom_freqpoly(aes(x = SalePrice, y = ..density.., color = HouseStyle))

ggplot(data) +
  geom_density(aes(x = SalePrice, y = ..density.., fill = CentralAir, color = CentralAir), size = 1, alpha = 0.5) +
  theme_minimal(18) +
  scale_x_continuous(labels = comma)

ggplot(data) +
  geom_freqpoly(aes(x = SalePrice, y = ..density.., color = Heating))

ggplot(data) +
  geom_freqpoly(aes(x = SalePrice, y = ..density.., color = factor(OverallQual)))

ggplot(data) +
  geom_freqpoly(aes(x = SalePrice, y = ..density.., color = factor(OverallCond)))

ggplot(data) +
  geom_freqpoly(aes(x = SalePrice, y = ..density.., color = SaleCondition))

ggplot(data) +
  geom_freqpoly(aes(x = SalePrice, y = ..density.., color = SaleType))

ggplot(data) +
  geom_freqpoly(aes(x = SalePrice, y = ..density.., color = PoolQC))
