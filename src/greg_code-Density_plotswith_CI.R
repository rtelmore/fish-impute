library(mvtnorm)
library(tidyverse)
library(mice)
library(plotly)

M <- 100
n <- 50
rho <- 0.5
missing_type <- 'MAR'
nsim <- 300
combined_mean_density_list <- results_list <- list()

for (j in 1:nsim){print(j)
  # create dataset
  #multivariate normal setting
  #Mean 0,0
  mu <- c(0, 0)
  #correlation 0.5
  sigma <- matrix(c(1, rho, rho, 1), nrow = 2)
  dat <- as.data.frame(mvtnorm::rmvnorm(n=n, mean=mu, sigma = sigma))
  
  #Skewed data 
  #dat$V1 <- qchisq(pnorm(dat$V1), 5)
  
  
  # duplicate dataset and add missingness
  # complete data set is dat
  #dat missing has missingness on it.  
  dat_missing <- dat
  
  apply(dat, 2, mean)
  apply(dat_missing, 2, mean, na.rm = TRUE)
  
  
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
  
  
  # impute data
  imputed <- mice(dat_missing, print=FALSE, m=M)
  
  #Complete the M imputed data sets
  imp_dens_stacked <- imp_data_stacked <- data.frame()
  for (i in 1:M){
    imp_data <- complete(imputed, i)
    imp_data$M <- i
    imp_data_stacked <- bind_rows(imp_data_stacked, imp_data)
    
    #Density function uses method "nrd0" by default for computing bandwidth
    imp_dens <- data.frame(x=density(imp_data$V1, from=-4, to=4, n=512)$x, y=density(imp_data$V1, from=-4, to=4, n=512)$y)
    imp_dens$M <- i
    imp_dens_stacked <- bind_rows(imp_dens_stacked, imp_dens)
  }
  
  
  
  
  ggplot(aes(x = x, y= y, group = as.factor(M)), data = imp_dens_stacked) + geom_path(color = rgb(0,0,0,0.25)) + theme_bw()
  ########################
  #Combining method 1
  ########################
  #Each data set gets a different nrd0
  #We combing across densities
  #Combining density estimators
  combined_dens_mean_dens <- imp_dens_stacked %>% group_by(x) %>% summarize(y = mean(y))
  ggplot(aes(x = x, y= y, group = as.factor(M)), data = imp_dens_stacked) + geom_path(color = rgb(0,0,0,0.25)) + theme_bw() + 
    geom_path(aes(x = x, y = y), data= combined_dens_mean_dens, color = "red")
  ########################
  #Combining method 2
  ########################
  #Here we average nrd0 across imputations
  #Computing nrd0 bandwidth estimate across the different imputations
  mean_nrd0 <- imp_data_stacked %>% 
    group_by(M) %>%
    summarize(nrd0fn = bw.nrd0(V1),
              sd = sd(V1),
              iqr = IQR(V1),
              min = min(sd, iqr/1.34), 
              nrd0 = (0.9 * min) * (n^-0.2)) %>% 
    pull(nrd0) %>% 
    mean()
  

  
  #Computing density estimator with all the data and using the mean bandwidth across imputations
  combined_dens_mean_separate_nrd0 <- data.frame(x=density(imp_data_stacked$V1, from=-4, to=4, n=512, bw = mean_nrd0)$x, y=density(imp_data_stacked$V1, from=-4, to=4, n=512, bw = mean_nrd0)$y)
  
  ########################
  #Combining method 3
  ########################
  #Here we average sd and iqr across imputations and then compute a single nrd0
  min_for_nrd0 <- imp_data_stacked %>% 
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
  combined_dens_mean_sdiqr <- data.frame(x=density(imp_data_stacked$V1, from=-4, to=4, n=512, bw = stacked_bw)$x, y=density(imp_data_stacked$V1, from=-4, to=4, n=512, bw = stacked_bw)$y)
  
  
  ########################
  #Combining method 4
  ########################
  #Compute band width using all the stacked data and then find density estimator on full stacked data 
  mean_nrd0 <- imp_data_stacked %>% 
    summarize(nrd0fn = bw.nrd0(V1),
              sd = sd(V1),
              iqr = IQR(V1),
              min = min(sd, iqr/1.34), 
              nrd0 = (0.9 * min) * (n^-0.2)) %>% 
    pull(nrd0) %>% 
    mean()
  
  #Computing density estimator with all the data and using the mean bandwidth across imputations
  combined_dens_mean_all_nrd0 <- data.frame(x=density(imp_data_stacked$V1, from=-4, to=4, n=512, bw = mean_nrd0)$x, y=density(imp_data_stacked$V1, from=-4, to=4, n=512, bw = mean_nrd0)$y)
  
  ########################
  #Combining method 5
  ########################
  #Stack and just treat the entire stack as a single data set.  
  combined_dens_stacked_nrd0 <- data.frame(x=density(imp_data_stacked$V1, from=-4, to=4, n=512)$x, y=density(imp_data_stacked$V1, from=-4, to=4, n=512)$y)
  
  ggplot(aes(x = x, y= y, group = as.factor(M)), data = imp_dens_stacked) + geom_path(color = rgb(0,0,0,0.25)) + theme_bw() + 
    geom_path(aes(x = x, y = y), data= combined_dens_mean_dens, color = "red") + 
    geom_path(aes(x = x, y = y), data= combined_dens_stacked_nrd0, color = "blue") + 
    geom_path(aes(x = x, y = y), data= combined_dens_mean_separate_nrd0, color = "forestgreen")  
    
  
  
  
  ggplot() +
    geom_density(aes(x = V1, color = "Original"), data = dat) +
    geom_density(aes(x = V1, color = "Missing"), data = dat_missing) +
    geom_path(aes(x = x, y= y, group = as.factor(M), color = "Individual Imputation"), data = imp_dens_stacked) +
    theme_bw() +
    geom_path(aes(x = x, y = y, color = "Imputed Combined: Stacked"),
              data = combined_dens_mean_dens) +
    geom_path(aes(x = x, y = y, color = "Imputed Combined: Rounded+Grouped"),
              data = combined_dens_mean_nrd0) +
    geom_path(aes(x = x, y = y, color = "Imputed Combined: Averaged BW"),
              data = combined_dens_mean_sdiqr) +
    
    stat_function(aes(color = "Truth"), fun = dnorm, args = list(mean = 0, sd = 1)) +
    scale_color_manual(
      values = c(
        'Truth' = 'blue',
        'Original' = 'red',
        'Missing' = 'gold',
        'Individual Imputation' = 'lightgray',
        'Imputed Combined: Stacked' = 'darkgray',
        'Imputed Combined: Rounded+Grouped' = 'black',
        'Imputed Combined: Averaged BW' = 'purple'))
  
  
  
  orig_y <- density(dat$V1, from=-4, to=4, n=512)$y # red
  missing_y <- density(na.omit(dat_missing$V1), from=-4, to=4, n=512)$y # gold
  imp_stacked_y <- density(imp_data_stacked$V1, from=-4, to=4, n=512, bw=stacked_bw)$y # gray
  imp_avg_bw_y <- combined_dens_mean_sdiqr$y # purple
  imp_grouped_y <- combined_dens_mean_dens$y #black
  
  orig_x <- density(dat$V1, from=-4, to=4, n=512)$x
  truth_y <- dnorm(orig_x, 0, 1) # BLUE
  
  #The area between the cdfs
  orig <- sum(abs((cumsum(truth_y*diff(orig_x)[1]) - cumsum(orig_y*diff(orig_x)[1]))))
  missing <- sum(abs((cumsum(truth_y*diff(orig_x)[1]) - cumsum(missing_y*diff(orig_x)[1]))))
  stacked <- sum(abs((cumsum(truth_y*diff(orig_x)[1]) - cumsum(imp_stacked_y*diff(orig_x)[1]))))
  avg_bw <- sum(abs((cumsum(truth_y*diff(orig_x)[1]) - cumsum(imp_avg_bw_y*diff(orig_x)[1]))))
  grouped <- sum(abs((cumsum(truth_y*diff(orig_x)[1]) - cumsum(imp_grouped_y*diff(orig_x)[1]))))
  
  # order resulting MSEs and display
  results_list[[j]] <- c('Original'=orig, 'Missing'=missing, 'Stacked'=stacked, 'Avg BW'=avg_bw, 'Grouped'=grouped)
  #results
  
  
  
}


rezzies  <- as.data.frame(do.call(rbind,results_list)) %>% pivot_longer(cols = everything())
ggplot(aes(x = value, color = name), data = rezzies) + geom_density() + ggtitle(paste0("M = ", M))

rezzies %>% group_by(name) %>% summarize(mean = mean(value))

#Plot true cdf and then the 500 cdf estimates on top.  

rezzies  <- do.call(rbind,results_list)
ggplot(aes(x = Grouped, y = Grouped - `Avg BW`), data = rezzies) + geom_point()





