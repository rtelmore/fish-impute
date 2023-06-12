## Greg
## Initial imputation
## 12 June 2023

library(dplyr)
library(ggplot2)
library(mice)

sf <- readRDS("data/full-sf-data.rds")

sf |>  
  mutate(missing = is.na(age)) |> 
  group_by(missing) |> 
  summarise(mean(length))
sf |>  
  mutate(missing = is.na(age)) |> 
  ggplot(aes(x = missing, y = length)) + 
  geom_boxplot()

ggplot(aes(x = age, y = length),data = sf) + geom_point() + geom_smooth()

set.seed(2014)
mids <- mice(sf, m  = 100, method = "pmm")
comp <- list()
for (i in 1:100){
  comp[[i]]<-complete(mids,i)
  comp[[i]]$m <- i
}

df <- do.call(rbind,comp)

ggplot()  + geom_density(aes(x = age, group = factor(m)),data = df,bw = .75, col = "gray") + geom_density(aes(x = age,),data = df,bw = .75, lwd = 1) + geom_density(aes(x = age),data = sf,bw = .75, col = "red", lwd = 1)

p <- ggplot()
p + geom_line(data = df, aes(x = age, group = as.factor(m)),
              stat = "density", bw = .75, col = "gray", alpha = .25) +
  geom_density(data = df, aes(x = age), bw = .75, lwd = 1) + 
  geom_density(data = sf, aes(x = age), bw = .75, col = "red", lwd = 1) +
  geom_vline(xintercept = mean(df$age), linetype = "dashed") +
  geom_vline(xintercept = mean(sf$age, na.rm = T), linetype = "dashed", col = "red") +
  scale_x_continuous(breaks = seq(0, 24, by = 2), limits = c(0, 24)) +
  theme_bw()

mean(df$age)
mean(sf$age, na.rm = TRUE)
