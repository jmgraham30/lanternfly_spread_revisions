library(tidyverse)
library(readxl)

state_vals <- c("connecticut","district of columbia",
                "delaware","maryland","new jersey",
                "new york","ohio","pennsylvania",
                "virginia","west virginia")

states <- map_data("state") |>
  filter(region %in% state_vals)

glimpse(states)

data_to_map <- read_csv("data/data_to_map.csv")

data_to_map <- data_to_map |>
  select(-c(i14,i15,i16,i17,i18,i19,i20,i21,i22,i23,i24,i25))

glimpse(data_to_map)

data_temporal <- read_csv("data/data_2_temporal.csv")

glimpse(data_temporal)

corrected_581 <- read_xlsx("data/S1Datav2.xlsx",
                           sheet="581 region model",
                           range="A1:P582") |>
  janitor::clean_names()

glimpse(corrected_581)

data_for_infested <- data_temporal |>
  select(year,infested,fips)

data_for_infested <- data_for_infested |>
  pivot_wider(values_from=infested,names_from = year) |>
  arrange(fips)


glimpse(data_for_infested)

data_to_map_581_inf <- corrected_581 |>
  select(-county,-state,-country) |>
  right_join(data_to_map,by="fips") 

glimpse(data_to_map_581_inf)

write_csv(data_to_map_581_inf,"data/data_to_map_581_inf.csv")
