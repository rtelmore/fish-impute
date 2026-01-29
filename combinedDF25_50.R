library(plyr)
library(tab)


load("../fish_impute_git/src/results_20250903_mar_norm_cor25.RData") 
resultsdf25 = results_df %>%
  mutate(corr = .25)
load("../fish_impute_git/src/results_20250829_mar_norm.RData")
resultsdf50 = results_df %>%
  mutate(corr = .50)

combin_df = bind_rows(resultsdf25, resultsdf50)
#plot normal results
#ggplot(combin_df,facet_grid(rows = corr, cols = M))
combin_df %>%filter(name %in% c("Original","Missing","Imp1"))%>% mutate(name = substring(name,1,4)) %>% ggplot(aes(x = as.factor(name), y = -log(value), color = as.factor(M))) + geom_boxplot() + facet_grid(corr~n+method)

medians_tbl <- combin_df %>%
  mutate(name = substring(name, 1, 4)) %>%
  group_by(corr, M, method, name) %>%
  summarise(median_val = median(-log(value), na.rm = TRUE), .groups = "drop")

medians_tbl

medians <- ddply(combin_df,.(TYPE),summarise,med = median(combin_df))
tabmedians()


library(plyr)
library(tab)



load("../fish_impute_git/src/results_mar_chisq_0.25.RData") 
resultsdf25chi = results_df %>%
  mutate(corr = .25)
load("../fish_impute_git/src/results_mar_chisq_0.5.RData")
resultsdf50chi = results_df %>%
  mutate(corr = .50)

combin_df = bind_rows(resultsdf25chi, resultsdf50chi)
#plot chi sq results
#ggplot(combin_df,facet_grid(rows = corr, cols = M))
combin_df %>%filter(name %in% c("Original","Missing","Imp1")) %>% mutate(name = substring(name,1,4)) %>% ggplot(aes(x = as.factor(name), y = -log(value), color = as.factor(M))) + geom_boxplot() + facet_grid(corr~n+method)


#table data type, separate data type and distribution
#pivot longer. P m n cca 1 2 
#do mixture. 

load("../fish_impute_git/src/results_mar_mixture_0.25.RData") 
resultsdf25mix = results_df %>%
  mutate(corr = .25)
load("../fish_impute_git/src/results_mar_mixture_0.5.RData")
resultsdf50mix = results_df %>%
  mutate(corr = .50)

combin_df = bind_rows(resultsdf25mix, resultsdf50mix)
#plot chi sq results
#ggplot(combin_df,facet_grid(rows = corr, cols = M))
combin_df %>%filter(name %in% c("Original","Missing","Imp1")) %>% mutate(name = substring(name,1,4)) %>% ggplot(aes(x = as.factor(name), y = -log(value), color = as.factor(M))) + geom_boxplot() + facet_grid(corr~n+method)

