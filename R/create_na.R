if(!"pacman" %in% installed.packages()) install.packages("pacman")
pacman::p_load(tidyverse,  readxl, mice)
set.seed(123)

data_raw <- read_csv("data/raw/data_ewba6_raw.csv")

amputed_data <- data_raw |>
   select(matches("K\\d{3}_\\d{2}")) |>
   ampute(prop = .05, mech = "MCAR", bycases = TRUE) |>
   pluck("amp") |>
   mutate(across(everything(), ~replace_na(.x, -9)))


data_raw_na <- data_raw |>
   select(-matches("K\\d{3}_\\d{2}")) |>
   bind_cols(amputed_data) |>
   relocate(matches("K\\d{3}_\\d{2}"), .after = EI02_01) 


# person did not finish survey at all
data_raw_na[which(data_raw_na$CASE == 42), ] <- data_raw_na |>
   filter(CASE == 42) |>
   mutate(EI05 = 2) |>
   mutate(across(-c(CASE:EI05, LASTDATA), ~-9)) |>
   mutate(FINISHED = 0, Q_VIEWER = 0, LASTPAGE = 2, MISSING = 100, MISSREL = 100) 

lapply(data_raw_na, typeof)


write_csv(data_raw_na, "data/proc/data_ewba6.csv") 
