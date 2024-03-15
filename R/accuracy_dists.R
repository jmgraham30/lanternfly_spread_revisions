library(tidyverse)
library(patchwork)

theme_set(theme_minimal())

source("R/accuracy_function.R")

data_to_map_166_inf <- read_csv("data/data_to_map_166_inf.csv")

glimpse(data_to_map_166_inf)

accuracy_14 <- compute_accuracy(data_to_map_166_inf$x2014,data_to_map_166_inf$i14)
accuracy_15 <- compute_accuracy(data_to_map_166_inf$x2015,data_to_map_166_inf$i15)
accuracy_16 <- compute_accuracy(data_to_map_166_inf$x2016,data_to_map_166_inf$i16)
accuracy_17 <- compute_accuracy(data_to_map_166_inf$x2017,data_to_map_166_inf$i17)
accuracy_18 <- compute_accuracy(data_to_map_166_inf$x2018,data_to_map_166_inf$i18)
accuracy_19 <- compute_accuracy(data_to_map_166_inf$x2019,data_to_map_166_inf$i19)
accuracy_20 <- compute_accuracy(data_to_map_166_inf$x2020,data_to_map_166_inf$i20)
accuracy_21 <- compute_accuracy(data_to_map_166_inf$x2021,data_to_map_166_inf$i21)

data_to_map_166_inf <- data_to_map_166_inf %>% 
  mutate(accuracy_14 = accuracy_14,
         accuracy_15 = accuracy_15,
         accuracy_16 = accuracy_16,
         accuracy_17 = accuracy_17,
         accuracy_18 = accuracy_18,
         accuracy_19 = accuracy_19,
         accuracy_20 = accuracy_20,
         accuracy_21 = accuracy_21)


glimpse(data_to_map_166_inf)


accuracy_15_plot <- data_to_map_166_inf |>
  group_by(fips) |>
  summarise(accuracy_15 = mean(accuracy_15, na.rm = TRUE)) |>
  ggplot(aes(x = accuracy_15)) +
  geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
  ylim(c(0,166)) + 
  labs(title = "2015",
       x = "Accuracy",
       y = "Count") 

accuracy_16_plot <- data_to_map_166_inf |>
  group_by(fips) |>
  summarise(accuracy_16 = mean(accuracy_16, na.rm = TRUE)) |>
  ggplot(aes(x = accuracy_16)) +
  geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
  ylim(c(0,166)) +
  labs(title = "2016",
       x = "Accuracy",
       y = "Count")

accuracy_17_plot <- data_to_map_166_inf |>
  group_by(fips) |>
  summarise(accuracy_17 = mean(accuracy_17, na.rm = TRUE)) |>
  ggplot(aes(x = accuracy_17)) +
  geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
  ylim(c(0,166)) +
  labs(title = "2017",
       x = "Accuracy",
       y = "Count") 

accuracy_18_plot <- data_to_map_166_inf |>
  group_by(fips) |>
  summarise(accuracy_18 = mean(accuracy_18, na.rm = TRUE)) |>
  ggplot(aes(x = accuracy_18)) +
  geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
  ylim(c(0,166)) +
  labs(title = "2018",
       x = "Accuracy",
       y = "Count") 

accuracy_19_plot <- data_to_map_166_inf |>
  group_by(fips) |>
  summarise(accuracy_19 = mean(accuracy_19, na.rm = TRUE)) |>
  ggplot(aes(x = accuracy_19)) +
  geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
  ylim(c(0,166)) +
  labs(title = "2019",
       x = "Accuracy",
       y = "Count") 

accuracy_20_plot <- data_to_map_166_inf |>
  group_by(fips) |>
  summarise(accuracy_20 = mean(accuracy_20, na.rm = TRUE)) |>
  ggplot(aes(x = accuracy_20)) +
  geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
  ylim(c(0,166)) +
  labs(title = "2020",
       x = "Accuracy",
       y = "Count")

accuracy_21_plot <- data_to_map_166_inf |>
  group_by(fips) |>
  summarise(accuracy_21 = mean(accuracy_21, na.rm = TRUE)) |>
  ggplot(aes(x = accuracy_21)) +
  geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
  ylim(c(0,166)) +
  labs(title = "2021",
       x = "Accuracy",
       y = "Count") 

accuracy_15_plot
accuracy_16_plot
accuracy_17_plot
accuracy_18_plot
accuracy_19_plot
accuracy_20_plot
accuracy_21_plot

ggsave(paste0("accuracy_dists/accuracy_","2015",".jpg"),plot=accuracy_15_plot,width=7,height=7)
ggsave(paste0("accuracy_dists/accuracy_","2016",".jpg"),plot=accuracy_16_plot,width=7,height=7)
ggsave(paste0("accuracy_dists/accuracy_","2017",".jpg"),plot=accuracy_17_plot,width=7,height=7)
ggsave(paste0("accuracy_dists/accuracy_","2018",".jpg"),plot=accuracy_18_plot,width=7,height=7)
ggsave(paste0("accuracy_dists/accuracy_","2019",".jpg"),plot=accuracy_19_plot,width=7,height=7)
ggsave(paste0("accuracy_dists/accuracy_","2020",".jpg"),plot=accuracy_20_plot,width=7,height=7)
ggsave(paste0("accuracy_dists/accuracy_","2021",".jpg"),plot=accuracy_21_plot,width=7,height=7)

