data_to_map_166_inf |>
  filter(accuracy_21 < 20) |>
  select(county,state,x2014,i14,accuracy_14,
         x2015,i15,accuracy_15,
         x2016,i16,accuracy_16,
         x2017,i17,accuracy_17,
         x2018,i18,accuracy_18,
         x2019,i19,accuracy_19,
         x2020,i20,accuracy_20,
         x2021,i21,accuracy_21) |>
  View()
