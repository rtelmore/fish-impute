#Missing data
## Missingnes mechanisms

#Male/female Age
n <- 100
set.seed(1234)
dat <- data.frame(sex = c(rep(0,50), rep(1,50)), age = c(rnorm(n/2,40,10),rnorm(n/2,60,10)))
mean(dat$age[dat$sex == 1])
mean(dat$age[dat$sex == 0])
#MCAR: Missing completely at random
#dat$age[runif(100,0,1)>0.5] <- NA

#MAR: Missing at random
dat$age[dat$sex == 1 & runif(100) > 0.1] <- NA
dat$age[dat$sex == 0 & runif(100) > 0.9] <- NA

mean(dat$age, na.rm = T)

#MNAR: Missing not at random
#dat$age[dat$age > 40 & runif(100) > 0.5] <- NA

# Multiple imputation
library(mice)

mids <- mice(dat, m = 5)
comp <- list()
for (i in 1:5){
  comp[[i]] <- complete(mids, i) 
}
mod <- list()
for (i in 1:5){
mod[[i]] <- summary(lm(age ~ sex, data = comp[[i]]))$coef[,1:2]
}

results <- data.frame(do.call(rbind,mod))
results$m <- rep(1:5, each = 2)
results$var <- rep(c("int","sex"),5)

#Q: combined point estimate
Q <- results %>% filter(var == "int") %>% summarize(mean(Estimate))

#Tm: combined variance estimate
Tm = (1+1/M)*Bm + vbarM
M <- 5
vbarM <- results %>% filter(var == "int") %>% summarize(mean(Std..Error^2))
Bm <- results %>% filter(var == "int") %>% summarize(var(Estimate))
Tm <- (1+1/M)*Bm + vbarM

Q
sqrt(Tm)

#density estimation 
hist(dat$age)
plot(density(dat$age)$x, density(dat$age)$y, type = "l")
points(dat$age, runif(100,0,0.005), pct = 16, cex = 0.25)
          
          