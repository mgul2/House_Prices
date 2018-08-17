getwd()
library(dplyr)
attach(train)
library(forcats) 
setwd("/Users/madhupgarg123/House_Prices/Datasets")

dim(train); 
train %>% str()
summary(train[41:81])
train_copy = train
train = train_copy
# Feature engg by Mujtaba:

train$Remodelling = ifelse((train$YearRemodAdd - train$YearBuilt) > 0, "YES", "NO")
train$BsmtUnf_prop_perc = (train$BsmtUnfSF/train$TotalBsmtSF)
train$age_house = train$YrSold - train$YearBuilt
train$cond_qual = train$OverallCond + train$OverallQual

# Convert into either Factor or Numeric

# Select column number to be converted to factors:

col_list = colnames(train)

to_factor = c(2, 3, 18, 19, 20, 21, 48:53, 55, 57, 62, 72, 77, 78, 82)

train[to_factor] = lapply(train[to_factor], factor)

# Divide dataset into 2 halves, Madhup will process columns 44-86

train_mujtaba = train[, c(1:43)]
train_madhup = train[, c(44:85)]
dim(train_madhup)
dim(train_mujtaba)

# Handle Missing Values

# Create a dataframe which shows all columns with number of missing values:
missing_count <-sapply(train_madhup, function(y) sum(length(which(is.na(y)))))
missing_count <- data.frame(missing_count)
View(missing_count)

#Process FireplaceQu, NA means No Fireplace

train_madhup$FireplaceQu = fct_explicit_na(train_madhup$FireplaceQu, na_level = "No Fireplace")

View(train_madhup)
