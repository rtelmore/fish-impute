## Ryan Elmore
## Two sources of fish data
## 07 June 2023

fd_obs <- read.csv("data/full_data_sf.csv") |> 
  dplyr::select(Age, Length) |> 
  janitor::clean_names()

fd_mis <- read.csv("data/length_sf.csv") |> 
  janitor::clean_names() |> 
  dplyr::mutate(age = NA)

fd <- rbind(df_obs, df_mis)

saveRDS(fd, "data/full-sf-data.rds")

p <- ggplot(data = fd |> na.omit(),
            aes(x = age))
p + geom_histogram(aes(y = after_stat(density)), 
                   fill = "grey", 
                   col = "black",
                   alpha = .65) +
  geom_density() +
  scale_x_continuous(breaks = seq(0, 25, by = 2)) +
  labs(x = "Age of Sand Flathead Fish",
       y = "Kernel Density Estimate") +
  theme_minimal()
ggsave("fig/sf-fish-kde-complete-case.pdf", height = 6, width = 8)

## Impute Missing Values

m <- 10
imputed <- mice(fd, 
                print = FALSE, 
                m = m, 
                method = "pmm")

fd_imputed <- complete(imputed, action = "long")

## Just showing all 10
p <- ggplot(data = fd |> na.omit(),
            aes(x = age))
p + geom_histogram(aes(y = after_stat(density)), 
                   fill = "grey80", 
                   col = "black",
                   alpha = .65) +
  geom_density(data = fd_imputed,
               aes(x = age, group = .imp), linetype = "dashed") +
  geom_density() +
  scale_x_continuous(breaks = seq(0, 24, by = 2),
                     limits = c(0, 24)) +
  labs(x = "Age of Sand Flathead Fish",
       y = "Kernel Density Estimate") +
  theme_minimal()
ggsave("fig/sf-kde-imputed.pdf", height = 6, width = 8)

rm(result)
for (i in 1:m){
  fd_tmp <- fd_imputed |> 
    filter(.imp == i)
  dens_est <- density(fd_tmp$age, from = 0, to = 24, bw = "SJ")
  if(exists("result")){
    result <- rbind(result, data.frame(x = dens_est$x,
                                       y = dens_est$y))
  } else{
    result <- data.frame(x = dens_est$x,
                         y = dens_est$y)
  }
}

## Average KDE
result <- result |> 
  group_by(x) |> 
  summarize(y = mean(y)) |> 
  ungroup()

p + geom_histogram(aes(y = after_stat(density)), 
                   fill = "grey80", 
                   col = "black",
                   alpha = .65) +
  geom_line(data = result,
            aes(x = x, y = y), 
            linetype = "dashed") +
  geom_density() +
  scale_x_continuous(breaks = seq(0, 24, by = 2),
                     limits = c(0, 24)) +
  labs(x = "Age of Sand Flathead Fish",
       y = "Kernel Density Estimate") +
  theme_minimal()
theme_minimal()
ggsave("fig/aids-kdes-average.pdf", height = 6, width = 8)

# df_sw <- read.csv("data/SWdata.csv") |> 
#   janitor::clean_names()
# 
# saveRDS(df, "data/full-sw-data.rds")
