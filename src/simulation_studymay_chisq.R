library(mvtnorm)
library(tidyverse)
library(mice)
library(plotly)
library(philentropy)

start <- Sys.time()

set.seed(12)
n_interval <- 2048
M <- 20
n <- 100
rho <- 0.5
missing_type <- 'MAR'
nsim <- 10
method <- "norm"
combined_mean_density_list <- list()
results_df <- data.frame()
#Mice: method pmm, norm, mean
#Add in true density: normal, skewed, mixture of normals.  (Fully obs variable is always normal.  Change the distribution of the one with missingness.  )
#Correlation; 0.25, 0.5, 0.75 (rho)
#MCAR (different rates of missingness in MCAR 0.25, .5), MAR (different rates of missingness in MAR) 
#n: 30, 100, 200, 300, 400, 500
#M: 5, 10, 25, 50, 100
#COmbining methods Imp1-Imp4, CCA

mvec <- c(5, 10, 25,50,100)

for (n in c(30, 100, 200, 300, 400, 500)){print(paste0("n=",n))
for (j in 1:nsim){print(j)
# create dataset
#multivariate normal setting
#Mean 0,0
mu <- c(0, 0)
#correlation 0.5
sigma <- matrix(c(1, rho, rho, 1), nrow = 2)
dat <- as.data.frame(mvtnorm::rmvnorm(n=n, mean=mu, sigma = sigma))

#Skewed data 
dat$V1 <- qchisq(pnorm(dat$V1), 5)

#Skewed data 
#dat$V1 <- qchisq(pnorm(dat$V1), 5)


# duplicate dataset and add missingness
# complete data set is dat
#dat missing has missingness on it.  
dat_missing <- dat

#apply(dat, 2, mean)
#apply(dat_missing, 2, mean, na.rm = TRUE)


#Using the ampute function
#patterns <- rbind(c(1,1),c(0,1))
#dat_missing <- ampute(dat, patterns = patterns, prop = c(0.4), freq = c(0.1, 0.9), mech = missing_type)

if (missing_type == "MAR"){
med <- median(dat$V2)
n1 <- sum(dat$V2 < med)
dat_missing$V1[dat_missing$V2 < med & runif(n1) < .8] <- NA
n2 <- sum(dat_missing$V2 >= med)
dat_missing$V1[dat_missing$V2 >= med & runif(n2) < .2] <- NA
}


if (missing_type == "MCAR"){
   missing_amt <-  nrow(dat) * .5
   dat_missing$V1[1:missing_amt] <- NA
}


results_list <- list()

for (method in c("norm","pmm")){

#impute data
imputed <- mice(dat_missing, print=FALSE, m=100, method = method)  
imp_data_stacked <- complete(imputed, "long")
imp_data_stacked$M <- imp_data_stacked$.imp
  
  for (M in mvec){print(paste0("m=",M))
    
#Complete the M imputed data sets
imp_dens_stacked  <- data.frame()
for (i in 1:M){

#Density function uses method "nrd0" by default for computing bandwidth
imp_dens <- data.frame(x=density(imp_data_stacked$V1[imp_data_stacked$M == i], from=-3, to=18, n=n_interval)$x,
                       y=density(imp_data_stacked$V1[imp_data_stacked$M == i], from=-3, to=18, n=n_interval)$y)
imp_dens$M <- i
imp_dens_stacked <- bind_rows(imp_dens_stacked, imp_dens)
}



#ggplot(aes(x = x, y= y, group = as.factor(M)), data = imp_dens_stacked) + geom_path(color = rgb(0,0,0,0.25)) + theme_bw()
########################
#Combining method 1
########################
#Could also use Sheather Jones for computing bandwidth
#Each data set gets a different nrd0
#We combing across densities
#Combining density estimators
combined_dens1 <- imp_dens_stacked %>% filter(M <= M) %>% group_by(x) %>% summarize(y = mean(y))

########################
#Combining method 2
########################
#Could also use Sheather Jones for computing bandwidth
#Here we average nrd0 across imputations
#Computing nrd0 bandwidth estimate across the different imputations
mean_nrd0 <- imp_data_stacked %>% filter(M <= M) %>% 
  group_by(M) %>%
  summarize(nrd0fn = bw.nrd0(V1),
            sd = sd(V1),
            iqr = IQR(V1),
            min = min(sd, iqr/1.34), 
            nrd0 = (0.9 * min) * (n^-0.2)) %>% 
  pull(nrd0) %>% 
  mean()

#Computing density estimator with all the data and using the mean bandwidth across imputations
combined_dens2 <- data.frame(x=density(imp_data_stacked$V1[imp_data_stacked$M <= M], from=-3, to=18, n=n_interval, bw = mean_nrd0)$x, y=density(imp_data_stacked$V1[imp_data_stacked$M <= M], from=-3, to=18, n=n_interval, bw = mean_nrd0)$y)

########################
#Combining method 3
########################
#Here we average sd and iqr across imputations and then compute a single nrd0
min_for_nrd0 <- imp_data_stacked %>% filter(M <= M) %>% 
  group_by(M) %>%
  summarize(nrd0fn = bw.nrd0(V1),
            sd = sd(V1),
            iqr = IQR(V1)) %>% 
  ungroup() %>% 
  summarize(sd = mean(sd),
            iqr = mean(iqr), 
            min = min(sd, iqr/1.34)) %>% pull(min)

stacked_bw <- 0.9 * min_for_nrd0 * n^-0.2
            
#Computing density estimator with all the data and using the mean bandwidth across imputations
combined_dens3 <- data.frame(x=density(imp_data_stacked$V1[imp_data_stacked$M <= M], from=-3, to=18, n=n_interval, bw = stacked_bw)$x, y=density(imp_data_stacked$V1[imp_data_stacked$M <= M], from=-3, to=18, n=n_interval, bw = stacked_bw)$y)


########################
#Combining method 4
########################
#Compute band width using all the stacked data and then find density estimator on full stacked data 
mean_nrd0 <- imp_data_stacked %>% filter(M <= M) %>%
  summarize(nrd0fn = bw.nrd0(V1),
            sd = sd(V1),
            iqr = IQR(V1),
            min = min(sd, iqr/1.34), 
            nrd0 = (0.9 * min) * (n^-0.2)) %>% 
  pull(nrd0) %>% 
  mean()

#Computing density estimator with all the data and using the mean bandwidth across imputations
combined_dens4 <- data.frame(x=density(imp_data_stacked$V1[imp_data_stacked$M <= M], from=-3, to=18, n=n_interval, bw = mean_nrd0)$x, y=density(imp_data_stacked$V1[imp_data_stacked$M <= M], from=-3, to=18, n=n_interval, bw = mean_nrd0)$y)

########################
#Combining method 5
########################
#Stack and just treat the entire stack as a single data set.  
#combined_dens5 <- data.frame(x=density(imp_data_stacked$V1, from=-3, to=3, n=n_interval)$x, y=density(imp_data_stacked$V1, from=-3, to=3, n=n_interval)$y)


ggplot() +
  geom_density(aes(x = V1, color = "Original"), data = dat) +
  geom_density(aes(x = V1, color = "Missing"), data = dat_missing) +
  geom_path(aes(x = x, y= y, group = as.factor(M), color = "Individual Imputation"), data = imp_dens_stacked) +
  theme_bw() +
  geom_path(aes(x = x, y = y, color = "Imputed Combined: 1"),
            data = combined_dens1) +
  geom_path(aes(x = x, y = y, color = "Imputed Combined: 2"),
            data = combined_dens2) +
  geom_path(aes(x = x, y = y, color = "Imputed Combined: 3"),
            data = combined_dens3) +
  geom_path(aes(x = x, y = y, color = "Imputed Combined: 4"),
            data = combined_dens4) +
  stat_function(aes(color = "Truth"), fun = dnorm, args = list(mean = 0, sd = 1)) +
  scale_color_manual(
    values = c(
      'Truth' = 'gold',
      'Original' = 'black',
      'Missing' = 'red',
      'Individual Imputation' = 'lightgray',
      'Imputed Combined: 1' = 'blue',
      'Imputed Combined: 2' = 'orange',
      'Imputed Combined: 3' = 'darkgreen',
      'Imputed Combined: 4' = 'purple'))


  
orig_y <- density(dat$V1, from=-3, to=18, n=n_interval)$y # black
missing_y <- density(na.omit(dat_missing$V1), from=-3, to=18, n=n_interval)$y # red

orig_x <- density(dat$V1, from=-3, to=18, n=n_interval)$x
truth_y <- dchisq(orig_x, 5) # gold

#The area between the cdfs
# orig <- sum(abs((cumsum(truth_y*diff(orig_x)[1]) - cumsum(orig_y*diff(orig_x)[1]))))
# missing <- sum(abs((cumsum(truth_y*diff(orig_x)[1]) - cumsum(missing_y*diff(orig_x)[1]))))
# imp1 <- sum(abs((cumsum(truth_y*diff(orig_x)[1]) - cumsum(combined_dens1$y*diff(orig_x)[1]))))
# imp2 <- sum(abs((cumsum(truth_y*diff(orig_x)[1]) - cumsum(combined_dens2$y*diff(orig_x)[1]))))
# imp3 <- sum(abs((cumsum(truth_y*diff(orig_x)[1]) - cumsum(combined_dens3$y*diff(orig_x)[1]))))
# imp4 <- sum(abs((cumsum(truth_y*diff(orig_x)[1]) - cumsum(combined_dens4$y*diff(orig_x)[1]))))
# imp5 <- sum(abs((cumsum(truth_y*diff(orig_x)[1]) - cumsum(combined_dens5$y*diff(orig_x)[1]))))

#The area between the pdfs
orig <- sum(abs(((truth_y*diff(orig_x)[1]) - (orig_y*diff(orig_x)[1])))^2) #black
missing <- sum(abs(((truth_y*diff(orig_x)[1]) - (missing_y*diff(orig_x)[1])))^2)
imp1 <- sum(abs(((truth_y*diff(orig_x)[1]) - (combined_dens1$y*diff(orig_x)[1])))^2)
imp2 <- sum(abs(((truth_y*diff(orig_x)[1]) - (combined_dens2$y*diff(orig_x)[1])))^2)
imp3 <- sum(abs(((truth_y*diff(orig_x)[1]) - (combined_dens3$y*diff(orig_x)[1])))^2)
imp4 <- sum(abs(((truth_y*diff(orig_x)[1]) - (combined_dens4$y*diff(orig_x)[1])))^2)
#imp5 <- sum(abs(((truth_y*diff(orig_x)[1]) - (combined_dens5$y*diff(orig_x)[1])))^2)

#Kullback Liebler
# orig <- KL(rbind(orig_y/sum(orig_y), truth_y/sum(truth_y)))
# missing <- KL(rbind(missing_y/sum(missing_y), truth_y/sum(truth_y)))
# imp1 <- KL(rbind(combined_dens1$y/sum(combined_dens1$y), truth_y/sum(truth_y)))
# imp2 <- KL(rbind(combined_dens2$y/sum(combined_dens2$y), truth_y/sum(truth_y)))
# imp3 <- KL(rbind(combined_dens3$y/sum(combined_dens3$y), truth_y/sum(truth_y)))
# imp4 <- KL(rbind(combined_dens4$y/sum(combined_dens4$y), truth_y/sum(truth_y)))
# imp5 <- KL(rbind(combined_dens5$y/sum(combined_dens5$y), truth_y/sum(truth_y)))


# order resulting MSEs and display
results_list[[M]] <- c('Original'=orig, 'Missing'=missing, 'Imp1'= imp1,
                       'Imp2'= imp2,
                       'Imp3'= imp3,
                       'Imp4'= imp4)
#results
#combined_mean_density_list[[j]] <- data.frame(isim = j, combined_dens_mean_dens)

  }  

results_df  <- bind_rows(results_df,as.data.frame(do.call(rbind,results_list))  %>% pivot_longer(cols = everything()) %>% mutate(M=rep(mvec,each = 6), n = n, missing_type= missing_type, method = method, id = j))




}
}
}

end <- Sys.time()
save(results_df, file = "/Users/maynorman/Desktop/fish-impute-git/results_20250922_mar_norm.RData")

time <- end-start
print(time)

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
  boot_dens <- data.frame(x=density(boot$V1, from=-4, to=4, n=n_interval)$x, y=density(boot$V1, from=-4, to=4, n=n_interval)$y)
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

