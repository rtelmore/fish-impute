## Ryan Elmore
## Data Analysis for Paper
## 23 June 2025

library(mice)
library(dplyr)
library(ggplot2)
library(speff2trial)

fd <- ACTG175 |> 
  select(age, wtkg, treat, cd40, cd420, treat, cd496) |> 
  mutate(diff = cd496 - cd40)

## Incomplete case KDE

p <- ggplot(data = fd |> na.omit(),
            aes(x = diff))
p + geom_histogram(aes(y = after_stat(density)), fill = "grey", col = "black") +
  geom_density() +
  scale_x_continuous(breaks = seq(-600, 800, by = 100)) +
  labs(x = "CD4 at 96 weeks minus CD4 at Baseline",
       y = "Kernel Density Estimate") +
  theme_bw()

## Impute Missing Values

m <- 10
fd_sub <- fd |> 
  select(age, wtkg, diff)
imputed <- mice(fd_sub, 
                print = FALSE, 
                m = m, 
                method = "norm")

fd_imputed <- complete(imputed, action = "long")

## Just showing all 10
p <- ggplot(data = fd |> na.omit(),
            aes(x = diff))
p + geom_histogram(aes(y = after_stat(density)), fill = "grey", col = "black") +
  geom_density(data = fd_imputed,
               aes(x = diff, group = .imp), col = "red", linetype = "dashed") +
  geom_density() +
  scale_x_continuous(breaks = seq(-600, 800, by = 100)) +
  labs(x = "CD4 at 96 weeks minus CD4 at Baseline",
       y = "Kernel Density Estimate") +
  theme_bw()
?density

rm(result)
for (i in 1:m){
  fd_tmp <- fd_imputed |> 
    filter(.imp == i)
  dens_est <- density(fd_tmp$diff, from = -600, to = 700, bw = "SJ")
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
  summarize(y = mean(y))

p + geom_histogram(aes(y = after_stat(density)), fill = "grey80", col = "black") +
  geom_line(data = result,
            aes(x = x, y = y), 
            col = "red", 
            linetype = "dashed") +
  geom_density() +
  scale_x_continuous(breaks = seq(-600, 800, by = 200)) +
  scale_y_continuous(breaks = seq(0, 0.003, by = .0005)) +
  labs(x = "CD4 at 96 weeks minus CD4 at Baseline",
       y = "Kernel Density Estimate") +
  theme_bw()
ggsave("fig/aids-kdes.pdf", height = 6, width = 8)
