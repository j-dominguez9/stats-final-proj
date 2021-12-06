houses <- read_csv(file.choose())
?read_csv()
houses %>% mutate()
head(houses)
is.na(houses)
library(tidyverse)
for (i in 1:nrow(houses)) {
  is.na(houses[i, ])
}
output <- vector("double", ncol(houses))
for (i in seq_along(houses)) {
  output[[i]] <- count(houses[[i]])
}
output
houses[[1]]
head(houses)
head(houses[2])
!is.na(houses[[1]])
names(houses)
results <- vector("list", length(houses))
names(results) <- names(houses)
for(i in seq_along(houses)) {
  name <- names(houses)[[i]]
  value <- houses[[i]]
}
name
value
colnames(houses)[apply(houses, 2, anyNA)]
houses %>% dplyr::mutate(if_any(.cols = c("Alley", "BsmtQual", "BsmtCond", 'BsmtExposure', 'BsmtFinType1', 'BsmtFinType2', 'FireplaceQu', 'GarageType', 'GarageFinish', 'GarageQual', 'GarageCond', 'PoolQC', 'Fence', 'MiscFeature'), replace_na()))

houses <- houses %>% mutate_at(vars(Alley, BsmtQual, BsmtCond, BsmtExposure, BsmtFinType1, BsmtFinType2, FireplaceQu, GarageType, GarageFinish, GarageQual, GarageCond, PoolQC, Fence, MiscFeature), ~replace_na(.,"None"))
?replace_na()
houses %>% replace_na()

houses <- houses %>% janitor::clean_names()
head(houses)
fit <- lm(log(sale_price)~overall_qual + log(gr_liv_area) + garage_cars + total_bsmt_sf + bsmt_fin_sf1 + year_built + year_remod_add + kitchen_qual + lot_area + garage_area, data = houses)
?lm()
summary(fit)

colnames(houses)

library(leaps)
leaps::regsubsets(sale_price~., nbest = 3, data = houses)
?regsubsets()
library(olsrr)
ols_step_forward_p()
lm(sale_price ~ ., data = houses)
head(houses$sale_price)
is.numeric(houses$sale_price)
lapply(houses$sale_price, unique)
sum(is.na(houses$sale_price))
attributes(houses)
str(houses)
summary(houses)
houses2 <- houses %>% select(everything(), -c(id, pool_qc, fence, misc_feature, alley, utilities))
house_fit <- lm(log(sale_price)~.-exterior1st -bsmt_fin_sf1 -low_qual_fin_sf, data = houses2)
forward <- ols_step_forward_p(house_fit, penter = 0.01, details = TRUE)
forward
ols_press(forward.mod)
f2 <- lm(log(sale_price)~ overall_qual + log(gr_liv_area) + bsmt_qual + total_bsmt_sf + bsmt_unf_sf + overall_cond + bldg_type + kitchen_qual + year_built + lot_area + sale_condition + garage_cars + pool_area + bedroom_abv_gr + mas_vnr_area + land_slope + condition1 + bsmt_fin_sf2 + lot_config, data = houses2)
ols_press(f2)
ols_coll_diag(f2)
ols_regress(f2)
forward_mod <- lm(log(sale_price)~overall_qual+log(gr_liv_area)+ms_sub_class+kitchen_qual+overall_cond+year_built+lot_area+sale_condition+total_bsmt_sf+pool_area+garage_cars+bldg_type+functional+bedroom_abv_gr+condition1+mas_vnr_area+land_slope+land_contour+lot_config+street+year_remod_add+kitchen_abv_gr, data = houses2)
summary(forward_mod)
forward2
prac <- lm(sale_price~overall_qual, data = houses2)
#c(overall_qual, gr_liv_area, neighborhood, bsmt_qual, roof_matl, bsmt_fin_sf1, ms_sub_class, bsmt_exposure, kitchen_qual, condition2, overall_cond, year_built, lot_area, sale_condition, total_bsmt_sf, pool_area, garage_cars, exter_qual, bldg_type, functional, bedroom_abv_gr, condition1, exterior1st, mas_vnr_area, land_slope"     "ms_zoning"       "land_contour"    "lot_config"      "low_qual_fin_sf" "garage_qual, garage_cond, street, year_remod_add, mas_vnr_type, kitchen_abv_gr)
?ols_press()
ols_coll_diag(forward.mod)
ols_regress(forward.mod)
forward$predictors
### "overall_qual"    "gr_liv_area"     "neighborhood"    "bsmt_qual"       "roof_matl"      
##[6] "bsmt_fin_sf1"    "ms_sub_class"    "bsmt_exposure"   "kitchen_qual"    "condition2"     
###[11] "overall_cond"    "year_built"      "lot_area"        "sale_condition"  "total_bsmt_sf"  
###[16] "pool_area"       "garage_cars"     "exter_qual"      "bldg_type"       "functional"     
###[21] "bedroom_abv_gr"  "condition1"      "exterior1st"     "mas_vnr_area"    "land_slope"     
###[26] "ms_zoning"       "land_contour"    "lot_config"      "low_qual_fin_sf" "garage_qual"    
###[31] "garage_cond"     "street"          "year_remod_add"  "mas_vnr_type"    "kitchen_abv_gr" 

###mass_vnr = 39, 16; remove exterior1st, bsmt_fin_sf1, low_qual_fin_sf, garage_qual, garage_cond

ols_step_forward

test <- read_csv(file.choose())
test2 <- test %>% janitor::clean_names()
colnames(test2)
test <- test %>% mutate_at(vars(Alley, BsmtQual, BsmtCond, BsmtExposure, BsmtFinType1, BsmtFinType2, FireplaceQu, GarageType, GarageFinish, GarageQual, GarageCond, PoolQC, Fence, MiscFeature), ~replace_na(.,"None"))
head(test)

pred <- predict.lm(f2, newdata = test2, type = 'response')
mean(pred)
is.numeric(pred)
mean_pred <- pred %>% filter(!is.na(pred))
mean(mean_pred$pred)
pred <- as.data.frame(pred)
mean(pred$pred)
pred <- pred %>% replace_na(., 12.00939)
install.packages("mltools")
library(mltools)
mltools::rmsle(preds = pred, actuals = test2$s)
range(pred)
head(pred)
head(test2)
id <- test2$id
pred %>% as.vector()
id
forward_submission <- data.frame(Id = test2$id, SalePrice = pred$pred)
forward_submission$SalePrice <- exp(forward_submission$SalePrice)
write_csv(forward_submission, "/Users/joaquindominguez/Dropbox/SMU/Statistical Foundations/Final_project/stats-final-=proj/forward_submission.csv")
library(here)
here()

ols_regress(f2)

backwards <- ols_step_backward_p(house_fit, prem = 0.005, details = TRUE)
backwards$removed
back_var_removed <- as.vector(backwards$removed)
back_var_removed
backward_model <- lm(log(sale_price)~.-neighborhood -exterior1st -heating -roof_matl -total_bsmt_sf -ms_zoning -functional -kitchen_qual -bsmt_unf_sf -condition2, data = houses3)
houses3 <- houses2 %>% select(everything(), -c(electrical, yr_sold, ms_sub_class, paved_drive, bsmt_full_bath, open_porch_sf, bsmt_cond, gr_liv_area, heating, bsmt_fin_type2, garage_yr_blt, enclosed_porch, exter_cond, garage_type, sale_type, bsmt_half_bath, lot_shape, misc_val, central_air, heating_qc, fireplace_qu, tot_rms_abv_grd, roof_style, screen_porch, wood_deck_sf, foundation, lot_frontage, half_bath, full_bath, x3ssn_porch, exterior2nd, garage_finish, garage_area, mo_sold, bsmt_fin_type1, house_style, fireplaces, year_remod_add, ms_zoning, kitchen_abv_gr, mas_vnr_type, mas_vnr_area))
houses3 <- houses2 %>% select(everything(), -c(bsmt_half_bath, exter_qual, gr_liv_area, bsmt_fin_sf2, garage_yr_blt, mas_vnr_area, fireplace_qu, mo_sold, yr_sold, open_porch_sf, ms_sub_class, exterior2nd, bsmt_cond, lot_shape, garage_type, electrical, garage_finish, mas_vnr_type, paved_drive, 
                                               bedroom_abv_gr, pool_area, exter_cond, bsmt_exposure, roof_style, house_style, misc_val, land_contour, tot_rms_abv_grd, bsmt_fin_type2, street, 
                                               garage_cond, garage_qual, kitchen_abv_gr, bsmt_qual, x3ssn_porch, garage_cars, bsmt_fin_type1, enclosed_porch, sale_type, wood_deck_sf, lot_frontage, full_bath, 
                                               half_bath, heating_qc, lot_config))
lm(log(sale_price)~back_var_removed, data = houses2)
c(bsmt_half_bath, exter_qual, gr_liv_area, bsmt_fin_sf2, garage_yr_blt, mas_vnr_area, fireplace_qu, mo_sold, yr_sold, open_porch_sf, ms_sub_class, exterior2nd, bsmt_cond, lot_shape, garage_type, electrical, garage_finish, mas_vnr_type, paved_drive, 
bedroom_abv_gr, pool_area, exter_cond, bsmt_exposure, roof_style, house_style, misc_val, land_contour, tot_rms_abv_grd, bsmt_fin_type2, street, 
garage_cond, garage_qual, kitchen_abv_gr, bsmt_qual, x3ssn_porch, garage_cars, bsmt_fin_type1, enclosed_porch, sale_type, wood_deck_sf, lot_frontage, full_bath, 
half_bath, heating_qc, lot_config)  
ols_press(backward_model)
ols_aic(backward_model)
ols_step_best_subset(backward_model)
?ols_best_subset
ols_coll_diag(backward_model)
ols_regress(backward_model)

test <- read_csv(file.choose())
test2 <- test %>% janitor::clean_names()
colnames(test2)
test <- test %>% mutate_at(vars(Alley, BsmtQual, BsmtCond, BsmtExposure, BsmtFinType1, BsmtFinType2, FireplaceQu, GarageType, GarageFinish, GarageQual, GarageCond, PoolQC, Fence, MiscFeature), ~replace_na(.,"None"))
head(test)

pred <- predict.lm(backward_model, newdata = test2, type = 'response')
mean(pred)
is.numeric(pred)
median_pred <- pred %>% filter(!is.na(pred))
median(median_pred$pred)
pred <- as.data.frame(pred)
median(pred$pred)
pred <- pred$pred %>% replace_na(., 11.97423)
pred %>% filter(is.na(pred))
install.packages("mltools")
library(mltools)
mltools::rmsle(preds = pred, actuals = test2$s)
range(pred)
head(pred)
head(test2)
id <- test2$id
pred %>% as.vector()
id
forward_submission <- data.frame(Id = test2$id, SalePrice = pred$pred)
forward_submission$SalePrice <- exp(forward_submission$SalePrice)
write_csv(forward_submission, "/Users/joaquindominguez/Dropbox/SMU/Statistical Foundations/Final_project/stats-final-=proj/forward_submission.csv")
library(here)
here()

backward_submission <- data.frame(Id = test2$id, SalePrice = pred$pred)
head(backward_submission)
backward_submission$SalePrice <- exp(backward_submission$SalePrice)
write_csv(backward_submission, "/Users/joaquindominguez/Dropbox/SMU/Statistical Foundations/Final_project/stats-final-=proj/back_sub.csv")



####step-wise

step_mod <- ols_step_both_p(house_fit, pent = 0.05, prem = 0.005, details = TRUE )
step_mod$predictors
summary(step_mod)
houses4 <- houses2 %>% select(c(overall_qual, neighborhood, gr_liv_area, garage_cars, overall_cond, roof_matl, total_bsmt_sf, 
                                               year_built, ms_zoning, condition2, bsmt_unf_sf, sale_condition, functional, bldg_type, 
                                               lot_area, central_air, kitchen_qual, fireplaces, condition1, year_remod_add, screen_porch, heating, 
                                               bsmt_full_bath, bsmt_exposure, land_slope, foundation, garage_area, sale_price))
stepwise_model <- lm(log(sale_price)~.-ms_zoning -heating -roof_matl -neighborhood -condition2 -kitchen_qual -functional, data = houses4)
ols_press(stepwise_model)
ols_coll_diag(stepwise_model)
ols_regress(stepwise_model)

pred <- predict.lm(stepwise_model, newdata = test2, type = 'response') %>% as.data.frame()
sum(is.na(pred$.))
median_pred <- pred %>% filter(!is.na(.))
median(median_pred$.)
pred$. <- pred$. %>% replace_na(., 11.97542)
stepwise_sub <- data.frame(Id = test$Id, SalePrice = pred$.)
stepwise_sub$SalePrice <- exp(stepwise_sub$SalePrice)

write_csv(stepwise_sub, "/Users/joaquindominguez/Dropbox/SMU/Statistical Foundations/Final_project/stats-final-=proj/step_sub.csv")


### custom
colnames(houses2)
custom_fit <- lm(log(sale_price)~ overall_qual + gr_liv_area + total_bsmt_sf + bsmt_fin_sf1 + year_built + garage_cars + year_remod_add + kitchen_qual + lot_area + garage_area + fireplace_qu + x1st_flr_sf + garage_type + overall_cond + bsmt_qual + ms_zoning + x2nd_flr_sf + bsmt_fin_type1 + bsmt_full_bath, data = houses2)
ols_press(custom_fit)
ols_regress(custom_fit)
pred <- predict.lm(custom_fit, newdata = test2, type = 'response')
pred <- pred %>% as.data.frame()
median_pred <- pred %>% filter(!is.na(.))
median(median_pred$.)
pred$. <- pred$. %>% replace_na(., 11.98766)
custom_sub <- data.frame(Id = test$Id, SalePrice = pred$.)
custom_sub$SalePrice <- exp(custom_sub$SalePrice)
write_csv(custom_sub, "/Users/joaquindominguez/Dropbox/SMU/Statistical Foundations/Final_project/stats-final-=proj/custom_sub.csv")
here()
