#################################################### Import Libraries ####################################################
install.packages("imputeTS")
install.packages("regclass")
install.packages("polycor")

library(dplyr)
attach(train)
library(forcats) 
library(tidyr)
library(imputeTS)
library(plyr)
library(regclass)
library(rockchalk)
library(polycor)

#################################################### View dataset ####################################################

#Import Datasets
train = read.csv(file.choose(), header = TRUE)
test = read.csv(file.choose(), header = TRUE)
# EDA
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
train_test$RemodelAge = train_test$YrSold - train_test$YearRemodAdd

train_test %>% dim
train_test %>% str()
############################################# Convert categorical variables into Factor #######################################

# Select column number to be converted to factors:

#col_list = colnames(train_test)

to_factor = c(2, 3, 18, 19, 20, 21, 48:53, 55, 57, 62, 72, 77, 78, 82)

train_test[to_factor] = lapply(train_test[to_factor], factor)

################################# Divide dataset into 2 halves, Madhup will process columns 44-85#############################

train_test_mujtaba = train_test[, c(1:43)]
train_test_madhup = train_test[, c(44:86)]
dim(train_test_madhup)
dim(train_test_mujtaba)
str(train_test_madhup)
attach(train_test_madhup)

#################################################### Handle Missing Values ####################################################

# Create a fucntion which shows all columns with number of missing values:

find_missing_values = function(x){
                                  missing_count <-sapply(x, function(y) sum(length(which(is.na(y)))))
                                  missing_count <- data.frame(missing_count)
                                  View(missing_count)
}

find_missing_values(train)

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

# Impute rest of the columns with Mode
train_test_madhup$BsmtFullBath[is.na(train_test_madhup$BsmtFullBath)] = find_mode(BsmtFullBath)
train_test_madhup$BsmtHalfBath[is.na(train_test_madhup$BsmtHalfBath)] = find_mode(BsmtHalfBath)
train_test_madhup$KitchenQual[is.na(train_test_madhup$KitchenQual)] = find_mode(KitchenQual)
train_test_madhup$Functional[is.na(train_test_madhup$Functional)] = find_mode(Functional)
train_test_madhup$GarageCars[is.na(train_test_madhup$GarageCars)] = '0'
train_test_madhup$GarageArea[is.na(train_test_madhup$GarageArea)] = 0
train_test_madhup$SaleType[is.na(train_test_madhup$SaleType)] = find_mode(SaleType)
tra

# ###################################################### Combine Levels ###############################################
# 
# # We will combine levels if the 1 class is dominating over other in a categorical variable
# 
# # BsmtFullBath
# count(train_test_madhup, "BsmtFullBath")
# aggregate(SalePrice ~ BsmtFullBath, data = train_test_madhup, mean)
# suggest_levels(SalePrice ~ BsmtFullBath, data = train_test_madhup)
# 
# train_test_madhup$BsmtFullBath = combineLevels(train_test_madhup$BsmtFullBath,levs = c('1','2','3' ), newLabel = c("1") )
# levels(train_test_madhup$BsmtFullBath)
# class(train_test_madhup$BsmtFullBath)
# 
# train_test_madhup$BsmtFullBath = revalue(train_test_madhup$BsmtFullBath, c('0' = '0', 'Not 0' = '1'))
# 
# # BsmtHalfBath
# 
# count(train_test_madhup, "BsmtHalfBath")
# aggregate(SalePrice ~ BsmtHalfBath, data = train_test_madhup, mean)
# suggest_levels(SalePrice ~ BsmtHalfBath, data = train_test_madhup)
# 
# #Best BIC value is obtained after collapsing all levels into 1. We will drop this column later on.
# train_test_madhup$BsmtHalfBath = combineLevels(train_test_madhup$BsmtHalfBath,levs = c('1','2' ), newLabel = c("1") )
# levels(train_test_madhup$BsmtHalfBath)
# 
# # FullBath
# count(train_test_madhup, "FullBath")
# aggregate(SalePrice ~ FullBath, data = train_test_madhup, mean)
# suggest_levels(SalePrice ~ FullBath, data = train_test_madhup)
# # No need to combine the levels
# 
# # HalfBath
# count(train_test_madhup, "HalfBath")
# aggregate(SalePrice ~ HalfBath, data = train_test_madhup, median)
# suggest_levels(SalePrice ~ HalfBath, data = train_test_madhup)
# # No need to combine the levels
# 
# # BedroomAbvGr
# count(train_test_madhup, "BedroomAbvGr")
# aggregate(SalePrice ~ BedroomAbvGr, data = train_test_madhup, median)
# suggest_levels(SalePrice ~ BedroomAbvGr, data = train_test_madhup)

train_test_madhup %>% View()
train_test_madhup %>% dim()
train_test_madhup %>% str() 

####################################################### Drop Columns ###############################################

train_test_madhup$MiscFeature = NULL
# 
# plot(train_test_madhup$GarageArea ~ train_test_madhup$GarageCars) # GarageCars and GarageArea are correlated
# 
# table(GarageQual, GarageCond)
# options(scipen = 999)
# summary(train_test_madhup[1:1460,])
# 
# plot(density(log(train_test$GarageYrBlt), na.rm = TRUE))

#################################### Merge train_test_madhup dataset with Mujtaba's dataset ################################
dim(dat_complete)
train_test_mujtaba = dat_complete
train_test = cbind(train_test_mujtaba, train_test_madhup)
train_test %>% dim
train_test %>% str
train_test$SalePrice1 <- train_test$SalePrice
train_test$SalePrice = NULL
train_test$SalePrice = train_test$SalePrice1
train_test$SalePrice1 = NULL
# Convert char to factor:

train_test$Alley = as.factor(train_test$Alley)
to_factor = c('MasVnrType', 'BsmtQual', 'BsmtCond', 'BsmtExposure', 'BsmtFinType1', 'BsmtFinType2', 'Electrical')
train_test[to_factor] = lapply(train_test[to_factor], factor)

####################################################### Train Test Split ################################################

train <- train_test[1:1460,]
test <- train_test[1461:2919,]
train %>% dim
test %>% dim
train %>% str()

####################################################### Bivariate Analysis ###################################################

class(train$YearBuilt)
table(train$YearBuilt, train$SalePrice)
aggregate(train_test$SalePrice, by = list(train_test$Alley), FUN = mean)
table(train_test$Neighborhood, train_test$Alley)
aggregate(train_test$SalePrice, by = list(train_test$Neighborhood), FUN = mean)
aggregate(train_test$SalePrice, by = list(train_test$LotConfig), FUN = mean)
aggregate(train_test$SalePrice, by = list(train_test$YearBuilt), FUN = sum)
table(YearRemodAdd)
hist(train$RemodelAge, breaks = 10)
quantile(train$RemodelAge)
train$RemodelAge[train$RemodelAge < 0] = 0
fit_age = lm(SalePrice ~ age_house, data = train)
fit_age_rem = lm(SalePrice ~ RemodelAge, data = train)
fit_age %>% summary()
fit_age_rem %>% summary()
# Drop street, alley, utilities, Condition2, YearBuilt (will use age), yearRemodel, 
####################################################### Feature Extraction ###################################################

# Run a linear model to get significant variables

X = train[,-c(1, 80)]
y = train[,80]
Z = as.data.frame(cbind(X, y))
fit = lm(y ~ ., data = Z)
options(max.print = 5000)
summary(fit)
train %>% View()
train %>% summary()
train %>% str()
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fit)
train$SalePrice %>% mean()
train[826,]
train[524,]
train[1325,]
VIF(fit)
alias(fit) # ElectricalMix, HeatingOthW, 
####################################################### Misc. work ###################################################

medians = as.data.frame(aggregate(train$LotFrontage, by=list(train$Neighborhood), FUN=mean))
xx = list(colnames(train))
