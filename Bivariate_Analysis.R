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
test[c(1916,2121,2152,2217,2251,2905),]  %>% View()
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
View(train)
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
