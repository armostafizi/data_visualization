library(tidyverse, ggplot2)

commute_nw_bike <- structure(list(JWTR = c(9L, 9L, 9L, 9L, 9L, 9L), transport_type = c("Bicycle", 
"Bicycle", "Bicycle", "Bicycle", "Bicycle", "Bicycle"), avg_time = c(16.5704697986577, 
20.0443760420675, 15.801775147929, 22.6850393700787, 21.4092872570194, 
24.1366459627329), med_time = c(15, 15, 12, 16.5, 20, 20), min_time = c(1L, 
1L, 1L, 1L, 1L, 1L), max_time = c(60L, 178L, 80L, 155L, 183L, 
154L), q25_time = c(5, 10, 5.25, 10, 10, 10), q75_time = c(20, 
25, 20, 30, 30, 30), n = c(149L, 7797L, 338L, 254L, 1852L, 1288L
), n_missing = c(0L, 0L, 0L, 0L, 0L, 0L), state = c("ak", "ca", 
"id", "nv", "or", "wa"), state_n = c(14570L, 769646L, 33210L, 
58313L, 83168L, 152527L), prop = c(0.0102264927934111, 0.0101306314851243, 
0.0101776573321289, 0.00435580402311663, 0.0222681800692574, 
0.00844440656408374), state_name = c("Alaska", "California", 
"Idaho", "Nevada", "Oregon", "Washington")), class = c("grouped_df", 
"tbl_df", "tbl", "data.frame"), row.names = c(NA, -6L), vars = "state", .Names = c("JWTR", 
"transport_type", "avg_time", "med_time", "min_time", "max_time", 
"q25_time", "q75_time", "n", "n_missing", "state", "state_n", 
"prop", "state_name"), indices = list(0L, 1L, 2L, 3L, 4L, 5L), group_sizes = c(1L, 
1L, 1L, 1L, 1L, 1L), biggest_group_size = 1L, labels = structure(list(
state = c("ak", "ca", "id", "nv", "or", "wa")), class = "data.frame", row.names = c(NA, 
-6L), vars = "state", .Names = "state"))


ggplot(data = commute_nw_bike) +
  geom_bar(mapping = aes(x = reorder(state_name, -prop), y = prop, fill = (state_name == "Oregon")), stat = "identity") +
  xlab("State") +
  ylab("Proportion") +
  guides(fill = FALSE) +
  theme_minimal()
