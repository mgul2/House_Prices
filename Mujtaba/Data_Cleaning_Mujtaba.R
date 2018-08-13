library(ggplot2)
library(dplyr)


setwd("/Users/mujtabagul/House_Prices/")
dat = fread("train.csv")

### FEATURE ENGINEERING
dat$Remodelling = ifelse((dat$YearRemodAdd - dat$YearBuilt) > 0, "YES", "NO")
dat$BsmtUnf_prop = (dat$BsmtUnfSF/dat$TotalBsmtSF) %>% is.finite()
dat$age_house = dat$YrSold - dat$YearBuilt

all_cols = dat %>% colnames()

dat %>% group_by(Id) %>% summarise_if(is.numeric,mean) 

### PLOTS
    dat %>% group_by(age_house) %>%
             summarize(SalePrice = mean(SalePrice)) %>%
             ggplot(aes(age_house,SalePrice)) +
             geom_point() +
             geom_smooth() +
             ggtitle("Age of House vs Sale Price")
    
    dat %>% group_by(YrSold) %>%
      summarize(SalePrice = mean(SalePrice)) %>%
      ggplot(aes(YrSold,SalePrice)) +
      geom_point() +
      geom_smooth() +
      ggtitle("Year Sold vs Sale Price")
