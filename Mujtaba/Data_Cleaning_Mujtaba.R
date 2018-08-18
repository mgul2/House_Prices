library(ggplot2)
library(dplyr)
library(tidyr)
library(data.table)


setwd("/Users/mujtabagul/House_Prices/Datasets/")
dat_house = fread("train.csv")

### FEATURE ENGINEERING
dat_house$Remodelling = ifelse((dat_house$YearRemodAdd - dat_house$YearBuilt) > 0, "YES", "NO")
dat_house$BsmtUnf_prop = (dat_house$BsmtUnfSF/dat_house$TotalBsmtSF) %>% is.finite()
dat_house$age_house = dat_house$YrSold - dat_house$YearBuilt
dat_house$cond_qual = dat_house$OverallCond + dat_house$OverallQual

all_cols = dat_house %>% colnames()

dat_house %>% group_by(Id) %>% summarise_if(is.numeric,mean) 

### PLOTS
    dat_house %>% group_by(age_house) %>%
             summarize(SalePrice = mean(SalePrice)) %>%
             ggplot(aes(age_house,SalePrice)) +
             geom_point() +
             geom_smooth() +
             ggtitle("Age of House vs Sale Price")
    
    dat_house %>% group_by(YrSold) %>%
      summarize(SalePrice = mean(SalePrice)) %>%
      ggplot(aes(YrSold,SalePrice)) +
      geom_point() +
      geom_smooth() +
      ggtitle("Year Sold vs Sale Price")
    
    dat_house %>% group_by(OverallQual) %>%
      summarise( SalePrice = mean(SalePrice)) %>%
      ggplot(aes(OverallQual,SalePrice)) +
      geom_point() +
      geom_smooth() +
      ggtitle("Overall Quality vs Sales Price")

    dat_house %>% group_by(OverallCond) %>%
      summarise( SalePrice = mean(SalePrice)) %>%
      ggplot(aes(OverallCond,SalePrice)) +
      geom_point() +
      geom_smooth() +
      ggtitle("Overall Condition vs Sales Price")
    
    dat_house %>% group_by(Neighborhood) %>%
      summarise( SalePrice = mean(SalePrice)) %>%
      ggplot(aes(Neighborhood)) +
      geom_bar(aes(y=SalePrice), stat='identity',colour='green',fill='paleturquoise4') +
      ggtitle("Neighbourhood vs Sales Price")
  
    dat_house %>% group_by(cond_qual) %>%
      summarise( SalePrice = mean(SalePrice)) %>%
      ggplot(aes(cond_qual, SalePrice)) +
      geom_point() +
      geom_smooth()

    
    
#### WORKING ON TRAIN_MUJTABA (Pre-processing)
    train_mujtaba = train_mujtaba %>% mutate_at(c("BsmtQual","BsmtCond", "BsmtExposure",
                                                  "BsmtFinType1", "BsmtFinType2","Alley",
                                                  "MasVnrType","Electrical"),
                                                as.character)
    train_mujtaba$SalePrice = train_madhup$SalePrice
    train_mujtaba$Alley = train_mujtaba$Alley %>% replace_na("No Access")
    train_mujtaba$BsmtQual = train_mujtaba$BsmtQual %>% replace_na("No Basement")
    train_mujtaba$BsmtExposure = train_mujtaba$BsmtExposure %>% replace_na("No Basement")
    train_mujtaba$BsmtCond = train_mujtaba$BsmtCond %>% replace_na("No Basement")
    train_mujtaba$BsmtFinType1 = train_mujtaba$BsmtFinType1 %>% replace_na("No Basement")
    train_mujtaba$BsmtFinType2 = train_mujtaba$BsmtFinType2 %>% replace_na("No Basement")
    train_mujtaba$MasVnrType = train_mujtaba$MasVnrType %>% replace_na("None")
    
    train_mujtaba$LotFrontage = train_mujtaba$LotFrontage %>% 
                                replace_na(train_mujtaba$LotFrontage %>% mean(na.rm=T))
    train_mujtaba$MasVnrArea = train_mujtaba$MasVnrArea %>% replace_na(0)
    train_mujtaba$Electrical = train_mujtaba$Electrical %>% replace_na("SBrkr")
    
    
    
    
    
    #train_mujtaba$Alley[which(is.na(train_mujtaba$Alley))]="No Access"    Another way to replace NAs 
    