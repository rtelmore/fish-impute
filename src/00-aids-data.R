## Ryan Elmore
## AIDS data
## 14 June 2023

library(speff2trial)
library(dplyr)
library(mgcv)

df <- ACTG175 |> 
  select(cd496, wtkg, symptom, karnof, cd80, cd40, cd820, cd420, oprior, offtrt,
         treat)

apply(df, 2, function(x) sum(is.na(x)))

## propensity score model
df_gam <- df |> 
  mutate(y = ifelse(is.na(cd496), 0, 1)) |> 
  select(-cd496)
ps_mod <- gam(y ~ s(wtkg) + symptom + karnof + s(cd80) + s(cd40) + s(cd820) + 
                s(cd420) + oprior + offtrt + treat, 
              data = df_gam, family = "binomial")
summary(ps_mod)

predict(ps_mod, type = "response")
