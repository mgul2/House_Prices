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
train %>% dim()
train %>% str()
test %>% dim()
test %>% str()
#################################################### Concatenate Train and Test dataset ####################################################

# Since Test set doesn't have SalePrice column, add a dummy column with repeated value of 0
test$SalePrice = rep(0, nrow(test)) 

# Check if all column names are same or not
colnames(train) == colnames(test)

# Create train_test
train_test = rbind(train, test)
train_test %>% dim()


#################################################### Create backups ####################################################
train_test_copy = train_test
train_test = train_test_copy

#################################################### Feature engg. by Mujtaba:####################################################

train_test$Remodelling = ifelse((train_test$YearRemodAdd - train_test$YearBuilt) > 0, "YES", "NO")
train_test$BsmtUnf_prop_perc = (train_test$BsmtUnfSF/train_test$TotalBsmtSF)
train_test$age_house = train_test$YrSold - train_test$YearBuilt
train_test$cond_qual = train_test$OverallCond + train_test$OverallQual

############################################# Convert categorical variables into Factor #######################################

# Select column number to be converted to factors:

#col_list = colnames(train_test)

to_factor = c(2, 3, 18, 19, 20, 21, 48:53, 55, 57, 62, 72, 77, 78, 82)

train_test[to_factor] = lapply(train_test[to_factor], factor)

################################# Divide dataset into 2 halves, Madhup will process columns 44-85#############################

train_test_mujtaba = train_test[, c(1:43)]
train_test_madhup = train_test[, c(44:85)]
dim(train_test_madhup)
dim(train_test_mujtaba)
str(train_test_madhup)
attach(train_test_madhup)

#################################################### Handle Missing Values ####################################################

# Create a fucntion which shows all columns with number of missing values:

find_missing_values = function(){
                                  missing_count <-sapply(train_test_madhup, function(y) sum(length(which(is.na(y)))))
                                  missing_count <- data.frame(missing_count)
                                  View(missing_count)
}

find_missing_values()

summary(train_test_madhup)

#Process FireplaceQu, NA means No Fireplace
train_test_madhup$FireplaceQu = fct_explicit_na(train_test_madhup$FireplaceQu, na_level = "No Fireplace")

#Process columns with Garage Information
train_test_madhup$GarageType = fct_explicit_na(train_test_madhup$GarageType, na_level = "No Garage")
train_test_madhup$GarageYrBlt[which(is.na(train_test_madhup$GarageYrBlt))]= 0
train_test_madhup$GarageFinish = fct_explicit_na(train_test_madhup$GarageFinish, na_level = "No Garage")
train_test_madhup$GarageQual = fct_explicit_na(train_test_madhup$GarageQual, na_level = "No Garage")
train_test_madhup$GarageCond = fct_explicit_na(train_test_madhup$GarageCond, na_level = "No Garage")
find_missing_values()

#Process columns with Pool Information
train_test_madhup$PoolQC = fct_explicit_na(train_test_madhup$PoolQC, na_level = "No Pool")

#Process columns with Fence Information
train_test_madhup$Fence = fct_explicit_na(train_test_madhup$Fence, na_level = "No Fence")

#Process Basement unfinished proportion percentage
train_test_madhup$BsmtUnf_prop_perc[which(is.na(train_test_madhup$BsmtUnf_prop_perc))]= 0

# Function to get mode of the factor variable
find_mode = function(var_name) names(table(var_name)[which.max(table(var_name))])

train_test_madhup$BsmtFullBath[is.na(train_test_madhup$BsmtFullBath), ] = 

####################################################### Combine Levels ########################################################

# We will combine levels if the 1 class is dominating over other in a categorical variable

# BsmtFullBath
count(train_test_madhup, "BsmtFullBath")
aggregate(SalePrice ~ BsmtFullBath, data = train_test_madhup, mean)
suggest_levels(SalePrice ~ BsmtFullBath, data = train_test_madhup)

train_test_madhup$BsmtFullBath = combineLevels(train_test_madhup$BsmtFullBath,levs = c('1','2','3' ), newLabel = c("1") )
levels(train_test_madhup$BsmtFullBath)
class(train_test_madhup$BsmtFullBath)

train_test_madhup$BsmtFullBath = revalue(train_test_madhup$BsmtFullBath, c('0' = '0', 'Not 0' = '1'))

# BsmtHalfBath

count(train_test_madhup, "BsmtHalfBath")
aggregate(SalePrice ~ BsmtHalfBath, data = train_test_madhup, mean)
suggest_levels(SalePrice ~ BsmtHalfBath, data = train_test_madhup)

#Best BIC value is obtained after collapsing all levels into 1. We will drop this column later on.
train_test_madhup$BsmtHalfBath = combineLevels(train_test_madhup$BsmtHalfBath,levs = c('1','2' ), newLabel = c("1") )
levels(train_test_madhup$BsmtHalfBath)

# FullBath
count(train_test_madhup, "FullBath")
aggregate(SalePrice ~ FullBath, data = train_test_madhup, mean)
suggest_levels(SalePrice ~ FullBath, data = train_test_madhup)
# No need to combine the levels

# HalfBath
count(train_test_madhup, "HalfBath")
aggregate(SalePrice ~ HalfBath, data = train_test_madhup, median)
suggest_levels(SalePrice ~ HalfBath, data = train_test_madhup)
# No need to combine the levels

# BedroomAbvGr
count(train_test_madhup, "BedroomAbvGr")
aggregate(SalePrice ~ BedroomAbvGr, data = train_test_madhup, median)
suggest_levels(SalePrice ~ BedroomAbvGr, data = train_test_madhup)

