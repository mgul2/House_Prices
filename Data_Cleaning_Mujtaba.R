library(ggplot2)
library(dplyr)


setwd("/Users/mujtabagul/House_Prices/")
dat = fread("train.csv")

### FEATURE ENGINEERING
dat$Remodelling = ifelse((dat$YearRemodAdd - dat$YearBuilt) > 0, "YES", "NO")
dat$BsmtUnf_prop = dat$BsmtUnfSF/dat$TotalBsmtSF %>% is.finite()
dat$age_house = dat$YrSold - dat$YearBuilt

### PLOTS
    dat %>% group_by(age_house) %>%
             summarize(SalePrice = mean(SalePrice)) %>%
             ggplot(aes(age_house,SalePrice)) +
             geom_point() +
             geom_smooth() +
             ggtitle("Age of House vs Sale Price")
