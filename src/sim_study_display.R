
results_df %>% mutate(name = substring(name,1,4)) %>% group_by(name, M, n, method, missing_type) %>% summarize(-log(mean(value))) %>% View()
results_df %>% mutate(name = substring(name,1,4)) %>% ggplot(aes(x = as.factor(name), y = -log(value), color = as.factor(n))) + geom_boxplot() + facet_grid(M~method,scales="free_y") 

rezzies  <- as.data.frame(do.call(rbind,results_list)) %>% pivot_longer(cols = everything())
ggplot(aes(x = (value), color = name), data = rezzies) + geom_density() + ggtitle(paste0("M = ", M)) + theme_bw()

rezzies %>% group_by(name) %>% summarize(mean = mean(value), 
                                         median = median(value), 
                                         Q1 = quantile(value, 0.25),
                                         Q3 = quantile(value, 0.75))

#Plot true cdf and then the 500 cdf estimates on top.  

rezzies  <- do.call(rbind,results_list)
ggplot(aes(x = Grouped, y = Grouped - `Avg BW`), data = rezzies) + geom_point()



####################################
#Build Confidence intervals
####################################
imp_data_stacked 
qandb <- imp_dens_stacked %>% group_by(x) %>% summarize(q = mean(y), b = var(y))

#Now get U. 
imp_data_stacked 
nboots <- 10
boot_dens_stacked <- impu_stacked <- data.frame()
for (m in 1:M){print(m)
  for (i in 1:nboots){
    temp <- imp_data_stacked %>% filter(M == m)
    boot <- temp[sample(1:nrow(temp),nrow(temp), replace = TRUE),]
    boot_dens <- data.frame(x=density(boot$V1, from=-4, to=4, n=512)$x, y=density(boot$V1, from=-4, to=4, n=512)$y)
    boot_dens$i <- i
    boot_dens_stacked <- bind_rows(boot_dens_stacked, boot_dens)
  }
  impu <- boot_dens_stacked %>% group_by(x) %>% summarize(u = var(y))
  impu_stacked <- bind_rows(impu_stacked, impu)
}
u <- impu_stacked %>% group_by(x) %>% summarize(ubar = mean(u))

vars <- qandb %>% left_join(u) %>% mutate(Tm = ubar + (1+1/M)*b)

plot(vars$x, vars$q, type = "l", ylim = c(-.5, 1.2))
polygon(c(vars$x,rev(vars$x)), c(vars$q - 1.96*sqrt(vars$Tm),rev(vars$q) + 1.96*rev(sqrt(vars$Tm))), col = "blue", border = "blue")
points(vars$x, vars$q, type = "l", lwd = 2, col = "lightblue")
points(dat$V1,rep(0, length(dat$V1)), pch = 16, cex = 0.5)
abline(v = 0, lty = 3)
abline(h = 0, lty = 3)


plot(vars$x, sqrt(vars$Tm), type = "l")
abline(v = 0, lty = 3)

ggplot(aes(x = x, y = y), data = test) + geom_path()
