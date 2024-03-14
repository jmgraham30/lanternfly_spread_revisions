source("R/create_14_21_figures.R")

data_to_map_166_inf |>
  select(x2014,i14,accuracy_14,
         x2015,i15,accuracy_15,
         x2016,i16,accuracy_16,
         x2017,i17,accuracy_17,
         x2018,i18,accuracy_18,
         x2019,i19,accuracy_19,
         x2020,i20,accuracy_20,
         x2021,i21,accuracy_21) |>
  View()

map_14 <- obtain_plot_accuracy(yr_val = x2014,
                         yr_val_chr = "x2014",
                         y_val = i14,
                         y_val_chr = "i14",
                         acc_val = accuracy_14,
                         acc_val_chr = "accuracy_14")

map_15 <- obtain_plot_accuracy(yr_val = x2015,
                            yr_val_chr = "x2015",
                            y_val = i15,
                            y_val_chr = "i15",
                            acc_val = accuracy_15,
                            acc_val_chr = "accuracy_15")

map_16 <- obtain_plot_accuracy(yr_val = x2016,
                            yr_val_chr = "x2016",
                            y_val = i16,
                            y_val_chr = "i16",
                            acc_val = accuracy_16,
                            acc_val_chr = "accuracy_16")

map_17 <- obtain_plot_accuracy(yr_val = x2017,
                            yr_val_chr = "x2017",
                            y_val = i17,
                            y_val_chr = "i17",
                            acc_val = accuracy_17,
                            acc_val_chr = "accuracy_17")

map_18 <- obtain_plot_accuracy(yr_val = x2018,
                            yr_val_chr = "x2018",
                            y_val = i18,
                            y_val_chr = "i18",
                            acc_val = accuracy_18,
                            acc_val_chr = "accuracy_18")

map_19 <- obtain_plot_accuracy(yr_val = x2019,
                            yr_val_chr = "x2019",
                            y_val = i19,
                            y_val_chr = "i19",
                            acc_val = accuracy_19,
                            acc_val_chr = "accuracy_19")

map_20 <- obtain_plot_accuracy(yr_val = x2020,
                            yr_val_chr = "x2020",
                            y_val = i20,
                            y_val_chr = "i20",
                            acc_val = accuracy_20,
                            acc_val_chr = "accuracy_20")

map_21 <- obtain_plot_accuracy(yr_val = x2021,
                            yr_val_chr = "x2021",
                            y_val = i21,
                            y_val_chr = "i21",
                            acc_val = accuracy_21,
                            acc_val_chr = "accuracy_21")
map_14

map_15

map_16

map_17

map_18

map_19

map_20

map_21

