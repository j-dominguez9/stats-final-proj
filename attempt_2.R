train <- read.csv(file.choose(), stringsAsFactors = FALSE)
test <- read.csv(file.choose(), stringsAsFactors = FALSE)
train$Alley[is.na(train$Alley)] = "No alley access"
test$Alley[is.na(test$Alley)] = "No alley access"
train$BsmtQual[is.na(train$BsmtQual)] = "No basement"
test$BsmtQual[is.na(test$BsmtQual)] = "No basement"
train$BsmtCond[is.na(train$BsmtCond)] = "No basement"
test$BsmtCond[is.na(test$BsmtCond)] = "No basement"
train$BsmtExposure[is.na(train$BsmtExposure)] = "No basement"
test$BsmtExposure[is.na(test$BsmtExposure)] = "No basement"
train$BsmtFinType1[is.na(train$BsmtFinType1)] = "No basement"
test$BsmtFinType1[is.na(test$BsmtFinType1)] = "No basement"
train$BsmtFinType2[is.na(train$BsmtFinType2)] = "No basement"
test$BsmtFinType2[is.na(test$BsmtFinType2)] = "No basement"
train$FireplaceQu[is.na(train$FireplaceQu)] = "No fireplace"
test$FireplaceQu[is.na(test$FireplaceQu)] = "No fireplace"
train$GarageType[is.na(train$GarageType)] = "No garage"
test$GarageType[is.na(test$GarageType)] = "No garage"
train$GarageFinish[is.na(train$GarageFinish)] = "No garage"
test$GarageFinish[is.na(test$GarageFinish)] = "No garage"
train$GarageQual[is.na(train$GarageQual)] = "No garage"
test$GarageQual[is.na(test$GarageQual)] = "No garage"
train$GarageCond[is.na(train$GarageCond)] = "No garage"
test$GarageCond[is.na(test$GarageCond)] = "No garage"
train$PoolQC[is.na(train$PoolQC)] = "No pool"
test$PoolQC[is.na(test$PoolQC)] = "No pool"
train$Fence[is.na(train$Fence)] = "No fence"
test$Fence[is.na(test$Fence)] = "No fence"
train$MiscFeature[is.na(train$MiscFeature)] = "None"
test$MiscFeature[is.na(test$MiscFeature)] = "None"


#replace missing values in MasVnrType column in training and test sets with "None"
#, which is most commonly occuring type
train$MasVnrType[is.na(train$MasVnrType)] = "None"
test$MasVnrType[is.na(test$MasVnrType)] = "None"
#replace missing values in Electrical column in training set with _
#"SBrkr, which is most frequently occuring electrical set-up
train$Electrical[is.na(train$Electrical)] = "SBrkr"
#replace missing values in MSZoning column in test set with RL (most frequently occuring)
test$MSZoning[is.na(test$MSZoning)] = "RL"
#replace missing values in Utilities column in test set with AllPub (most frequently occuring)
test$Utilities[is.na(test$Utilities)] = "AllPub"
#replace missing values in Exterior1st column in test set with VinylSd (most frequently occuring)
test$Exterior1st[is.na(test$Exterior1st)] = "VinylSd"
#replace missing values in Exterior2nd column in test set with VinylSd (most frequently occuring)
test$Exterior2nd[is.na(test$Exterior2nd)] = "VinylSd"
#replace missing value in KitchenQual column in test set with TA (most common)
test$KitchenQual[is.na(test$KitchenQual)] = "TA"
#replace missing values in Functional column in test set with Min2 (most common)
test$Functional[is.na(test$Functional)] = "Min2"
#replace missing value in SaleType column in test set with WD (most common)
test$SaleType[is.na(test$SaleType)] = "WD"

#replace missing value in BsmtFinSF1 column in test set with 0 (has no basement)
test$BsmtFinSF1[is.na(test$BsmtFinSF1)] = 0
#replace missing value in BsmtFinSF2 column in test set with 0 (has no basement)
test$BsmtFinSF2[is.na(test$BsmtFinSF2)] = 0
#replace missing value in BsmtUnfSF column in test set with 0 (has no basement)
test$BsmtUnfSF[is.na(test$BsmtUnfSF)] = 0
#replace missing value in TotalBsmtSF column in test set with 0 (has no basement)
test$TotalBsmtSF[is.na(test$TotalBsmtSF)] = 0
#replace missing values in BsmtFullBath column in test set with 0 (has no basement)
test$BsmtFullBath[is.na(test$BsmtFullBath)] = 0
#replace missing values in BsmtHalfBath column in test set with 0 (has no basement)
test$BsmtHalfBath[is.na(test$BsmtHalfBath)] = 0

#replace missing value in GarageCars column in test set with 0 (no garage)
test$GarageCars[is.na(test$GarageCars)] = 0
#replace missing value in GarageArea column in test set with 0 (no garage)
test$GarageArea[is.na(test$GarageArea)] = 0


#replace missing values in LotFrontage column in training and test sets with median
train$LotFrontage[is.na(train$LotFrontage)] = median(train$LotFrontage, na.rm = TRUE)
test$LotFrontage[is.na(test$LotFrontage)] = median(test$LotFrontage, na.rm = TRUE)

#replace missing values in GarageYrBlt column in training and test sets with -9999
train$GarageYrBlt[is.na(train$GarageYrBlt)] = -9999
test$GarageYrBlt[is.na(test$GarageYrBlt)] = -9999
#replace missing values in MasVnrArea column in training and test sets with -9999
train$MasVnrArea[is.na(train$MasVnrArea)] = -9999
test$MasVnrArea[is.na(test$MasVnrArea)] = -9999

set.seed(42)


model_lm = train(SalePrice ~ ., 
                 data = train,
                 method = "lm",
                 trControl = myControl)
model_lm <- lm(SalePrice~., data = train)
model_lm
resamples = resamples(model_list)
summary(resamples)

ggplot(train ,aes(y = SalePrice, x = GrLivArea)) + geom_point()

train_trim = filter(train, GrLivArea <= 4000)

ggplot(train, aes(y = SalePrice, x = OverallQual)) + geom_point()
ols_plot_cooksd_chart(model_lm)
ols_plot_dfbetas(model_lm)
head(varImp(model_lm) %>% arrange(desc(Overall)), 30)
?varImp
Top20Variables = c("OverallQual", "GrLivArea", "TotalBsmtSF", "GarageArea", "GarageCars", 
                   "X1stFlrSF", "YearBuilt", "ExterQual", "BsmtFinSF1", "FullBath",
                   "KitchenQual", "LotArea", "Fireplaces",
                   "FireplaceQu", "YearRemodAdd", "GarageYrBlt", "X2ndFlrSF", 
                   "TotRmsAbvGrd", "MasVnrArea", "LotFrontage")
?one_of()
train_2 <- train %>% select(all_of(Top20Variables), SalePrice)
model_lm2 <- lm(SalePrice~., data = train_2)
ols_plot_cooksd_chart(model_lm2)
ols_plot_dfbetas(model_lm2)

train_2 %>% ggplot(aes(x = LotFrontage, y = SalePrice)) + geom_point()
max(train_2$SalePrice)
train_2 %>% filter(OverallQual == 10) %>% arrange(desc(SalePrice))
train_2 <- train_2 %>% filter(!SalePrice >= 745000) %>% filter(GrLivArea <= 4000) %>% filter(TotalBsmtSF <= 3000) %>% 
  filter(GarageArea <= 1250) %>% filter(X1stFlrSF <= 3000) %>% filter(BsmtFinSF1 <= 2000) %>% 
  filter(LotArea <= 57000) %>% filter(X2ndFlrSF <= 1750) %>% filter(TotRmsAbvGrd <= 13) %>% filter(LotFrontage <= 200)
  
summary(model_lm2)
ols_press(model_lm2)  
ols_regress(model_lm2)
ols_step_forward_p(model_lm2, penter = 0.05, details = TRUE)

library(e1071)
column_classes = sapply(names(train_2, function(x){class(train_2[[x]])}))
numeric_columns = names(column_classes[column_classes == "integer"])
skew = sapply(numeric_columns, function(x){skewness(train_2[[x]], na.rm = T)})
skew = skew[skew > 0.75]
hist(train_2$SalePrice)
hist(train_2$GrLivArea)
train_2$SalePrice <- log(train_2$SalePrice)
train_2$GrLivArea <- log(train_2$GrLivArea)
train_2$X1stFlrSF <- log(train_2$X1stFlrSF)
test$GrLivArea <- log(test$GrLivArea)
test$X1stFlrSF <- log(test$X1stFlrSF)

colnames(train_2)

hist(train_2$TotalBsmtSF)
train_2 %>% ggplot(aes(x = X1stFlrSF, y = SalePrice)) + geom_point()
forward_model <- lm(SalePrice~. -GarageCars -FullBath -TotRmsAbvGrd -MasVnrArea, data = train_2)
ols_press(forward_model)
predictions_forward <- predict(forward_model, newdata = test, type = 'response')
predictions_forward
predictions_forward <- exp(predictions_forward)
predictions_forward <- as.data.frame(predictions_forward)
sum(is.na(predictions_forward$predictions_forward))
predictions_forward %>% filter(!is.na(.)) %>% summarize(median(.))
predictions_forward <- predictions_forward %>% mutate_at(vars(predictions_forward), ~replace_na(., "156916.6"))
library(here)
head(predictions_forward)
exp(predictions_forward$.)
exp(11.96347)
here()
forward_sub <- data.frame(Id = test$Id, SalePrice = predictions_forward$predictions_forward)
forward_sub %>% mutate(SalePrice = exp(SalePrice))
exp(forward_sub$SalePrice)
head(forward_sub)
write_csv(forward_sub, "/Users/joaquindominguez/Dropbox/SMU/Statistical Foundations/Final_project/stats-final-=proj/forward_sub.csv")

