library(ggplot2)
library(dplyr)
library(tidyr)
library(data.table)


setwd("/Users/mujtabagul/House_Prices/Datasets/")
dat_house = fread("train.csv")
dat_test = fread("test.csv")
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
      ggtitle("Neighbourhood vs Sales Price") +
      
  
    dat_house %>% group_by(cond_qual) %>%
      summarise( SalePrice = mean(SalePrice)) %>%
      ggplot(aes(cond_qual, SalePrice)) +
      geom_point() +
      geom_smooth()
    

    
    
#### WORKING ON TRAIN_MUJTABA (Pre-processing)
    train_mujtaba$SalePrice = train_madhup$SalePrice
    test_mujtaba = dat_test[,1:43]
    test_mujtaba$SalePrice = 0 
    dat_complete = rbind(train_mujtaba,test_mujtaba)
    
    Mode <- function(x) {
      ux <- unique(x)
      ux[which.max(tabulate(match(x, ux)))]
    }
    
    
    dat_complete = dat_complete %>% mutate_at(c("BsmtQual","BsmtCond", "BsmtExposure",
                                                "BsmtFinType1", "BsmtFinType2","Alley",
                                                "MasVnrType","Electrical","MSZoning",
                                                "Utilities","Exterior1st","Exterior2nd",
                                                "BsmtFinSF1","BsmtFinSF2","BsmtUnfSF",
                                                "TotalBsmtSF"),
                                                as.character)
    
    dat_complete$Alley = dat_complete$Alley %>% replace_na("No Access")
    dat_complete$BsmtQual = dat_complete$BsmtQual %>% replace_na("No Basement")
    dat_complete$BsmtExposure = dat_complete$BsmtExposure %>% replace_na("No Basement")
    dat_complete$BsmtCond = dat_complete$BsmtCond %>% replace_na("No Basement")
    dat_complete$BsmtFinType1 = dat_complete$BsmtFinType1 %>% replace_na("No Basement")
    dat_complete$BsmtFinType2 = dat_complete$BsmtFinType2 %>% replace_na("No Basement")
    dat_complete$MasVnrType = dat_complete$MasVnrType %>% replace_na("None")
    dat_complete$LotFrontage = dat_complete$LotFrontage %>% 
                                replace_na(dat_complete$LotFrontage %>% mean(na.rm=T))
    dat_complete$MasVnrArea = dat_complete$MasVnrArea %>% replace_na(0)
    dat_complete$Electrical = dat_complete$Electrical %>% replace_na("SBrkr")
    dat_complete$MSZoning = ifelse(dat_complete$MSSubClass==20,
                                   replace_na("RL"),replace_na("RM"))
    
    
  