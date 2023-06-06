## Ryan Elmore
## Two sources of fish data
## 07 June 2023

df_obs <- read.csv("data/full_data_sf.csv") |> 
  dplyr::select(Age, Length) |> 
  janitor::clean_names()

df_mis <- read.csv("data/length_sf.csv") |> 
  janitor::clean_names() |> 
  dplyr::mutate(age = NA)

df <- rbind(df_obs, df_mis)

saveRDS(df, "data/full-sf-data.rds")

df <- read.csv("data/SWdata.csv") |> 
  janitor::clean_names()

saveRDS(df, "data/full-sw-data.rds")
