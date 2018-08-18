#################################################### Import Libraries ####################################################
library(dplyr)
attach(train)
library(forcats) 
library(tidyr)
install.packages("imputeTS")
library(imputeTS)
library(plyr)
install.packages("regclass")
library(regclass)
library(rockchalk)

#################################################### View dataset ####################################################
dim(train); 
train %>% str()

#################################################### Create backups ####################################################
train_copy = train
train = train_copy


#################################################### Feature engg. by Mujtaba:####################################################

train$Remodelling = ifelse((train$YearRemodAdd - train$YearBuilt) > 0, "YES", "NO")
train$BsmtUnf_prop_perc = (train$BsmtUnfSF/train$TotalBsmtSF)
train$age_house = train$YrSold - train$YearBuilt
train$cond_qual = train$OverallCond + train$OverallQual

############################################# Convert categorical variables into Factor #######################################

# Select column number to be converted to factors:

col_list = colnames(train)

to_factor = c(2, 3, 18, 19, 20, 21, 48:53, 55, 57, 62, 72, 77, 78, 82)

train[to_factor] = lapply(train[to_factor], factor)

################################# Divide dataset into 2 halves, Madhup will process columns 44-85#############################

train_mujtaba = train[, c(1:43)]
train_madhup = train[, c(44:85)]
dim(train_madhup)
dim(train_mujtaba)
str(train_madhup)

#################################################### Handle Missing Values ####################################################

# Create a fucntion which shows all columns with number of missing values:

find_missing_values = function(){
                                  missing_count <-sapply(train_madhup, function(y) sum(length(which(is.na(y)))))
                                  missing_count <- data.frame(missing_count)
                                  View(missing_count)
}
#Process FireplaceQu, NA means No Fireplace
train_madhup$FireplaceQu = fct_explicit_na(train_madhup$FireplaceQu, na_level = "No Fireplace")

#Process columns with Garage Information
train_madhup$GarageType = fct_explicit_na(train_madhup$GarageType, na_level = "No Garage")
train_madhup$GarageYrBlt[which(is.na(train_madhup$GarageYrBlt))]= 0
train_madhup$GarageFinish = fct_explicit_na(train_madhup$GarageFinish, na_level = "No Garage")
train_madhup$GarageQual = fct_explicit_na(train_madhup$GarageQual, na_level = "No Garage")
train_madhup$GarageCond = fct_explicit_na(train_madhup$GarageCond, na_level = "No Garage")
find_missing_values()

#Process columns with Pool Information
train_madhup$PoolQC = fct_explicit_na(train_madhup$PoolQC, na_level = "No Pool")

#Process columns with Fence Information
train_madhup$Fence = fct_explicit_na(train_madhup$Fence, na_level = "No Fence")

#Process Basement unfinished proportion percentage
train_madhup$BsmtUnf_prop_perc[which(is.na(train_madhup$BsmtUnf_prop_perc))]= 0

####################################################### Combine Levels ########################################################

# We will combine levels if the 1 class is dominating over other in a categorical variable

# BsmtFullBath
count(train_madhup, "BsmtFullBath")
aggregate(SalePrice ~ BsmtFullBath, data = train_madhup, mean)
suggest_levels(SalePrice ~ BsmtFullBath, data = train_madhup)

train_madhup$BsmtFullBath = combineLevels(train_madhup$BsmtFullBath,levs = c('1','2','3' ), newLabel = c("Not 0") )
levels(train_madhup$BsmtFullBath)

# BsmtHalfBath

count(train_madhup, "BsmtHalfBath")
aggregate(SalePrice ~ BsmtHalfBath, data = train_madhup, mean)
suggest_levels(SalePrice ~ BsmtHalfBath, data = train_madhup)

#Best BIC value is obtained after collapsing all levels into 1. We will drop this column later on.
train_madhup$BsmtHalfBath = combineLevels(train_madhup$BsmtHalfBath,levs = c('1','2' ), newLabel = c("Not 0") )
levels(train_madhup$BsmtHalfBath)