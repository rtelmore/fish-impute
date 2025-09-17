library(plyr)
library(tab)


load("/Users/maynorman/Desktop/fish_impute_git/results_20250903_mar_norm_cor25.RData") 
resultsdf25 = results_df %>%
  mutate(corr = .25)
load("/Users/maynorman/Desktop/fish_impute_git/results_20250829_mar_norm.RData")
resultsdf50 = results_df %>%
  mutate(corr = .50)

combin_df = bind_rows(resultsdf25, resultsdf50)

ggplot(combin_df,facet_grid(rows = corr, cols = M))
combin_df %>% mutate(name = substring(name,1,4)) %>% ggplot(aes(x = as.factor(name), y = -log(value), color = as.factor(M))) + geom_boxplot() + facet_grid(corr~M+method)

medians_tbl <- combin_df %>%
  mutate(name = substring(name, 1, 4)) %>%
  group_by(corr, M, method, name) %>%
  summarise(median_val = median(-log(value), na.rm = TRUE), .groups = "drop")

medians_tbl

medians_tbl %>% ggplot(aes(x = M, y = median_val, color = factor(n))) + geom_point() + geom_line() + facet_grid(name~corr + method)

medians <- ddply(combin_df,.(TYPE),summarise,med = median(combin_df))
tabmedians()