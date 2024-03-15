library(tidyverse)
library(patchwork)

theme_set(theme_minimal())

state_vals <- c("connecticut","district of columbia",
                "delaware","maryland","new jersey",
                "new york","ohio","pennsylvania",
                "virginia","west virginia")

states <- map_data("state") %>%
  filter(region %in% state_vals)

glimpse(states)

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

obtain_plot_pairs <- function(y_val,y_val_chr,
                              yr_val,yr_val_chr,
                              acc_val,acc_val_chr,
                              data_to_map_val = data_to_map_166_inf){
  
  y_val <- enquo(y_val)
  t_val <- paste0("20",str_sub(y_val_chr,2,-1))
  
  yr_val <- enquo(yr_val)
  
  
  p_infested <- ggplot() + 
    geom_polygon(data=states,
                 aes(long,lat,group=group),
                 fill="white",color="black") + 
    geom_polygon(data=data_to_map_val,
                 aes(long, lat, group = group, fill = as.character(!!yr_val)),
                 color="black",linewidth=0.2) +
    scale_fill_discrete(name="Infested",type=c("white","darkgreen")) + 
    labs(title = t_val, x = "", y= "") + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          legend.position = "none",
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          plot.title = element_text(hjust = 0.5))
  
  p_pred <-  ggplot() +
    geom_polygon(data=states,
                 aes(long,lat,group=group),
                 fill="white",color="black") + 
    geom_polygon(data=data_to_map_val,
                 mapping = aes(long, lat, group = group, fill = !!y_val),
                 color="black",linewidth=0.2) +
    scale_fill_continuous(name="Proportion", 
                          low = "white", 
                          high = "darkgreen",
                          limits = c(0,1)) + 
    labs(title = "", x = "", y= "") + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          legend.position = "none",
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          plot.title = element_text(hjust = 0.5))
  
  
  p <- (p_infested / p_pred)
  
  #ggsave(paste0("./Maps/map_",t_val,".jpg"),plot=p)
  
  return(p)
  
}


obtain_plot_accuracy <- function(y_val,y_val_chr,
                              yr_val,yr_val_chr,
                              acc_val,acc_val_chr,
                              data_to_map_val = data_to_map_166_inf){
  
  y_val <- enquo(y_val)
  t_val <- paste0("20",str_sub(y_val_chr,2,-1))
  
  yr_val <- enquo(yr_val)
  
  acc_val <- enquo(acc_val)
  
  
  p_accuracy <-  ggplot() +
    geom_polygon(data=states,
                 aes(long,lat,group=group),
                 fill="white",color="black") + 
    geom_polygon(data=data_to_map_val,
                 mapping = aes(long, lat, group = group, fill = !!acc_val),
                 color="black",linewidth=0.2) +
    scale_fill_continuous(name="Accuracy", 
                          low = "white", 
                          high = "darkblue",
                          limits = c(0,100)) + 
    labs(title = t_val, x = "", y= "") + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          legend.position = "none",
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          plot.title = element_text(hjust = 0.5))
  
  
  #ggsave(paste0("./Maps/map_",t_val,".jpg"),plot=p)
  
  return(p_accuracy)
  
}


obtain_plot_cols <- function(y_val,y_val_chr,
                              yr_val,yr_val_chr,
                              acc_val,acc_val_chr,
                              data_to_map_val = data_to_map_166_inf){
  
  y_val <- enquo(y_val)
  t_val <- paste0("20",str_sub(y_val_chr,2,-1))
  
  yr_val <- enquo(yr_val)
  
  acc_val <- enquo(acc_val)
  
  p_infested <- ggplot() + 
    geom_polygon(data=states,
                 aes(long,lat,group=group),
                 fill="white",color="black") + 
    geom_polygon(data=data_to_map_val,
                 aes(long, lat, group = group, fill = as.character(!!yr_val)),
                 color="black",linewidth=0.2) +
    scale_fill_discrete(name="Infested",type=c("white","darkgreen")) + 
    labs(title = t_val, x = "", y= "") + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          legend.position = "none",
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          plot.title = element_text(hjust = 0.5))
  
  p_pred <-  ggplot() +
    geom_polygon(data=states,
                 aes(long,lat,group=group),
                 fill="white",color="black") + 
    geom_polygon(data=data_to_map_val,
                 mapping = aes(long, lat, group = group, fill = !!y_val),
                 color="black",linewidth=0.2) +
    scale_fill_continuous(name="Proportion", 
                          low = "white", 
                          high = "darkgreen",
                          limits = c(0,1)) + 
    labs(title = "", x = "", y= "") + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          legend.position = "none",
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          plot.title = element_text(hjust = 0.5))
  
  p_accuracy <-  ggplot() +
    geom_polygon(data=states,
                 aes(long,lat,group=group),
                 fill="white",color="black") + 
    geom_polygon(data=data_to_map_val,
                 mapping = aes(long, lat, group = group, fill = !!acc_val),
                 color="black",linewidth=0.2) +
    scale_fill_continuous(name="Accuracy", 
                          low = "white", 
                          high = "darkblue",
                          limits = c(0,100)) + 
    labs(title = "", x = "", y= "") + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          legend.position = "none",
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          plot.title = element_text(hjust = 0.5))
  
  
  p <- (p_infested / p_pred / p_accuracy)
  
  #ggsave(paste0("./Maps/map_",t_val,".jpg"),plot=p)
  
  return(p)
  
}

