
# (1) input data -------------------------------------------------------------------------

# Select your own directory
setwd("D:\\G02_2\\data_sci\\FINAL\\Cali_housing\\FINAL")

housing_raw = read.table("California_Housing_raw.csv", sep=",", quote = "\"",header = T)
dim(housing_raw) # [1] 14448    10

# (train, test) = ( 0.6 , 0.4)
index01 = sample(1:14448, size = (14448*0.4), replace = FALSE, prob = NULL)
index02 = sample(index01, size = (14448*0.4*0.3), replace = FALSE, prob = NULL)
index03 = setdiff(index01, index02)

# Public score = housing_test_30
# Private score = housing_test_70
housing_train = housing_all[-index01,]
housing_test_30 = housing_all[index02,]
housing_test_70 = housing_all[index03,]

dim(housing_train) # [1] 8669   10
dim(housing_test_30) # [1] 1733   10
dim(housing_test_70) # [1] 4046   10
# 8669 / 14448 # [1] 0.6000138
# 1733 / (1733+4046) # [1] 0.2998789
# 1733+4046  # 5779

require(mice)
mice.data_train <- mice(housing_train[-10],
                        m = 1,           #
                        maxit = 1,      # max iteration
                        method = "cart",
                        seed = 188,
                        print= T )

df_train <- complete(mice.data_train, 1)
housing_train_mice = cbind(df_train, housing_train[10])
sum(is.na(housing_train_mice))


dim(housing_test_30) # [1] 1733   10
dim(housing_test_70) # [1] 4046   10


mice.data_test <- mice(rbind(housing_test_30[-10], housing_test_70[-10]),
                       m = 1,           #
                       maxit = 1,      # max iteration
                       method = "cart",
                       seed = 188,
                       print= T )

df_test <- complete(mice.data_test, 1)
sum(is.na(df_test))
dim(df_test) # [1] 5779    9
housing_test_30_mice = df_test[1:1733,]
housing_test_70_mice = df_test[1734:5779,]
str(housing_test_30_mice) # 1733 x 9
str(housing_test_70_mice) # 4046 x 9


write.csv(housing_train_mice, "housing_train_mice.csv",  row.names = F)
write.csv(housing_test_30_mice, "housing_test_30_mice.csv",  row.names = F)
write.csv(housing_test_70_mice, "housing_test_70_mice.csv",  row.names = F)
write.csv(housing_test_30[10], "housing_test_30_answer.csv",  row.names = F)
write.csv(housing_test_70[10], "housing_test_70_answer.csv",  row.names = F)


# ___________-------------------------------------------------------------------------
# (2) EDA -------------------------------------------------------------------------

housing_train = read.table("housing_train_mice.csv", sep=",", quote = "\"",header = T)

require(lattice)
require(ggplot2)

#### 1. histagram ####

hist(housing_train$longitude, main= "longitude")
boxplot(housing_train$longitude, main= "longitude")
median(housing_train$longitude); mean(housing_train$longitude)
# [1] -118.5 [1] -119.5679 # median > mean  # negatively skewed 

hist(housing_train$latitude, main= "latitude")
boxplot(housing_train$latitude, main= "latitude")
median(housing_train$latitude) ; mean(housing_train$latitude)
# [1] 34.26 [1] 35.63572  # median < mean # positively skewed 

hist(housing_train$housing_median_age, main= "housing_median_age")
boxplot(housing_train$housing_median_age, main= "housing_median_age")
median(housing_train$housing_median_age); mean(housing_train$housing_median_age)
# [1] 29 [1] 28.53789 # median > mean # negatively skewed 

hist(housing_train$total_rooms, main= "total_rooms")
boxplot(housing_train$total_rooms, main= "total_rooms")
median(housing_train$total_rooms); mean(housing_train$total_rooms)
# [1] 2136 [1] 2651.125 # median < mean # positively skewed 

hist(housing_train$total_bedrooms, main= "total_bedrooms")
boxplot(housing_train$total_bedrooms, main= "total_bedrooms")
median(housing_train$total_bedrooms); mean(housing_train$total_bedrooms)
# [1] 438 [1] 539.209 # median < mean # positively skewed 

hist(housing_train$population, main= "population")
boxplot(housing_train$population, main= "population")
median(housing_train$population); mean(housing_train$population)
# [1] 1167 [1] 1426.289 # median < mean # positively skewed 

hist(housing_train$households, main= "households")
boxplot(housing_train$households, main= "households")
median(housing_train$households); mean(housing_train$households)
# [1] 412 [1] 501.2356 # median < mean # positively skewed 

hist(housing_train$median_income, main= "median_income")
boxplot(housing_train$median_income, main= "median_income")
median(housing_train$median_income); mean(housing_train$median_income)
# [1] 3.5357 [1] 3.884573 # median < mean # positively skewed 


#### 2. "ocean_proximity" ####

table(housing_train$ocean_proximity)
# <1H OCEAN     INLAND     ISLAND   NEAR BAY NEAR OCEAN 
# 3817       2781          1        952       1118 
barplot(table(housing_train$ocean_proximity), main= "ocean_proximity")

ISLAND = subset(housing_train, housing_train$ocean_proximity == "ISLAND")
apply(ISLAND[-9], 2,mean)
# longitude           latitude housing_median_age        total_rooms     total_bedrooms         population 
# -118.3300            33.3400            52.0000          2359.0000           591.0000          1100.0000 
# households      median_income median_house_value 
# 431.0000             2.8333        414700.0000 

OCEAN_1H = subset(housing_train, housing_train$ocean_proximity == "<1H OCEAN")
apply(OCEAN_1H[-9], 2,mean)
# longitude           latitude housing_median_age        total_rooms     total_bedrooms         population 
# -118.866563          34.578614          29.181556        2651.596018         546.573749        1513.362064 
# households      median_income median_house_value 
# 518.070736           4.283197      242833.358397 

INLAND = subset(housing_train, housing_train$ocean_proximity == "INLAND")
apply(INLAND[-9], 2,mean)
# longitude           latitude housing_median_age        total_rooms     total_bedrooms         population 
# -119.731320          36.722776          24.208198        2744.342682         542.087738        1413.700108 
# households      median_income median_house_value 
# 486.459547           3.179896      125315.287307 

NEAR_BAY = subset(housing_train, housing_train$ocean_proximity == "NEAR BAY")
apply(NEAR_BAY[-9], 2,mean)
# longitude           latitude housing_median_age        total_rooms     total_bedrooms         population 
# -122.259275          37.797941          37.332983        2459.193277         509.141807        1232.047269 
# households      median_income median_house_value 
# 482.036765           4.156755      257287.792017 

OCEAN_NEAR = subset(housing_train, housing_train$ocean_proximity == "NEAR OCEAN")
apply(OCEAN_NEAR[-9], 2,mean)
# longitude           latitude housing_median_age        total_rooms     total_bedrooms         population 
# -119.265134          34.701646          29.600179        2581.337209         532.460644        1326.017889 
# households      median_income median_house_value 
# 496.923971           4.045653      249041.224508 

hist(housing_train$median_house_value, main= "median_house_value")
boxplot(housing_train$median_house_value, main= "median_house_value")
median(housing_train$median_income); mean(housing_train$median_income)
# [1] 3.5357 [1] 3.884573 # mean < median # positively skewed 


# ___________-------------------------------------------------------------------------
# (3) Feature engineering -------------------------------------------------------------------------

#### 1. ocean_proximit / ISLAND => NEAR BAY ####

housing_train$ocean_proximity[housing_train$ocean_proximity == "ISLAND" ] = "NEAR BAY"
table(housing_train$ocean_proximity)
housing_test_30$ocean_proximity[housing_test_30$ocean_proximity == "ISLAND" ] = "NEAR BAY"
housing_test_70$ocean_proximity[housing_test_70$ocean_proximity == "ISLAND" ] = "NEAR BAY"

#### 2. No need to split numeric variable into categorical variable ####

#### 3. population_per_household ####
housing_train$population_per_household = housing_train$population / housing_train$households
pph = subset(housing_train, population_per_household < 30); #str(pph)
hist(pph$population_per_household)

hist(housing_train$population_per_household, main= "population_per_household")
boxplot(housing_train$population_per_household, main= "population_per_household")
median(housing_train$population_per_household); mean(housing_train$population_per_household)
# [1] 2.827303 [1] 3.100197 # median < mean # positively skewed 

#### 4. bedrooms_per_room ####
housing_train$bedrooms_per_room = housing_train$total_bedrooms / housing_train$total_rooms
hist(housing_train$bedrooms_per_room)
bpr = subset(housing_train, bedrooms_per_room < 0.6)# ; str(bpr)
hist(bpr$bedrooms_per_room)

hist(housing_train$bedrooms_per_room, main= "bedrooms_per_room")
boxplot(housing_train$bedrooms_per_room, main= "bedrooms_per_room")
median(housing_train$bedrooms_per_room); mean(housing_train$bedrooms_per_room)
# [1] 0.2031624 [1] 0.2128846 # median < mean # positively skewed 

#### 5. rooms_per_household ####
housing_train$rooms_per_household = housing_train$total_rooms / housing_train$households
hist(housing_train$rooms_per_household)
rph = subset(housing_train, rooms_per_household < 25) # ; str(rph)
hist(rph$rooms_per_household)

hist(housing_train$rooms_per_household, main= "rooms_per_household")
boxplot(housing_train$rooms_per_household, main= "rooms_per_household")
median(housing_train$rooms_per_household); mean(housing_train$rooms_per_household)
# [1] 5.237077 [1] 5.419356 # median < mean # positively skewed 



#### 6. bubble plot ####
# Libraries
library(ggplot2)
library(dplyr)
library(gapminder)
library(scales)
library(hrbrthemes)
library(viridis)

ggplot(housing_train, aes(x=longitude, y=latitude, size = population)) +
  geom_point(alpha=0.7)

# housing_train %>%
#   arrange(desc(population)) %>%
#   # mutate(country = factor(country, country)) %>%
#   ggplot(aes(x=longitude, y=latitude, size=population, fill=ocean_proximity)) +
#   geom_point(alpha=0.5, shape=21, color="black") +
#   scale_size(range = c(.1, 24), name="Population (M)") +
#   scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A") +
#   theme_ipsum() +
#   theme(legend.position="bottom") +
#   ylab("Life Expectancy") +
#   xlab("Gdp per Capita") +
#   theme(legend.position = "none")

housing_train %>%
  arrange(desc(population)) %>%
  ggplot(aes(x=median_income, y=median_house_value, size = population)) +
  geom_point(alpha=0.1) +
  scale_size(range = c(.1, 15), name="Population")

housing_train %>%
  arrange(desc(population)) %>%
  # mutate(country = factor(country, country)) %>%
  ggplot(aes(x=longitude, y=latitude, size=population, color=ocean_proximity)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 24), name="Population (M)")

# housing_train %>%
#   arrange(desc(population)) %>%
#   # mutate(country = factor(country, country)) %>%
#   ggplot(aes(x=longitude, y=latitude, size=population, color=ocean_proximity)) +
#   geom_point(alpha=0.1) +
#   scale_size(range = c(.1, 20), name="median_income")
# 
# housing_train %>%
#   arrange(desc(median_income)) %>%
#   # mutate(country = factor(country, country)) %>%
#   ggplot(aes(x=longitude, y=latitude, size=median_income, color=ocean_proximity)) +
#   geom_point(alpha=0.1) +
#   scale_size(range = c(.1, 13), name="median_income")



#### 7. correlation ####
library(ggcorrplot)
# names(housing_train)
cor01 = cor(housing_train[-9]) #; cor01
ggcorrplot(cor01, 
           hc.order = TRUE, 
           type = "full", # "lower"
           lab = TRUE)
# cor01[,9]
# cor01
barplot(cor01[,9], 
        names.arg = c("longitude","latitude","median_age","rooms","bedrooms","pop",
                      "hh","median_income","median_value",
                      "pop/hh", "bed/room ", "rooms/hh"))


cor02 = cor(housing_train[-c(4,5,6,9)]) #; cor02
ggcorrplot(cor02, 
           hc.order = F, 
           type = "full", # "lower"
           lab = T)

# cor02[,6]
barplot(cor02[-6,6],
        names.arg = c("longitude","latitude","median_age",
                      "hh","median_income", # "median_value"
                      "pop/hh", "bed/room ", "rooms/hh"),
        main = "Correlation to median_value")



#### 8. scale ####

# (1) Need to scale( )
# 1. KNN
# 2. K-means
# 3. PCA, LDA
# 4. Logistic / Linear regression
# 5. SVM
# 6. NN
# 
# (2) No need to scale( )
# 1. treelike methods

# names(housing_train)
# housing_train02 = housing_train[-c(9,10)]
# temp = housing_train[c(9,10)]
# str(housing_train02)
# housing_train02$longitude = housing_train02$longitude + 150
# log(housing_train02)
# 
# housing_train03 = scale(log(housing_train02))
# head(housing_train03)
# str(housing_train03)
# str(housing_train03[[2]])
# 
# housing_train = cbind(housing_train03, temp)



#### 9. log ####
# housing_train$value_log = log(housing_train$median_house_value)

# ___________-------------------------------------------------------------------------
# (4) Building models -------------------------------------------------------------------------

library(rBayesianOptimization)
library(randomForest, quietly = T)
library(rpart)
library(rpart.plot)

library(tidyverse)
library(caret)
library(dplyr)
library(pROC)
library(regclass)
library(car)
library(e1071)
library(adabag)

# k = 5
kfold01 = KFold(1:8669, nfolds = 5, stratified = F, seed = 6) 

acc_train = rep(NA, 5)
acc_valid = rep(NA, 5)
acc_test = rep(NA, 5)


# 3-way validation & k-fold
# for (i in 1:5){

i = 1
Data_test <- housing_train[unlist(kfold01[i]),]

if( i == 5 ) { t = 1 }else{ t = i+1 } 

# Data_valid <- Data[unlist(kfold01[t]),]

index = unlist(c(kfold01[i]))
# index = unlist(c(kfold01[i],kfold01[t])); # index; str(index)

Data_train <- housing_train[(1:8346)[-index], ]; #Titanic_train

# DAAG::vif(model)
# plot(model) # ntree = 150
# names(Data_train)
# tuneRF(Data_train[,1:9], Data_train[,10]) # mtry = 6

# head(Data_train)
# mtry = 4 is the best
# model = randomForest(x = Data_train[3:23], y = Data_train[2][[1]],
#                       ntree = 350, importance = T, replace = T)

# summary(model)
# head(Data_train)
# x11()
# plot(model)
# importance(model,type = 1)
# importance(model,type = 2)
# # cor(model)
# summary(model)


library(alr4); library(DMwR)


#### 1. No regressor (Null Model)####

# model = randomForest( formula = median_house_value ~ .,
#                       data = Data_train,  # Data
#                       ntree = 150, mtry = 6, importance = F, replace = T)
lmMod <- lm(median_house_value ~ 1,Data_train)  
dist_train <- predict(lmMod, Data_train)
dist_test <- predict(lmMod, Data_test) 

# MAPE & RMSE 
regr.eval(Data_train$median_house_value, dist_train)
# mae          mse         rmse         mape 
# 9.213218e+04 1.354364e+10 1.163771e+05 6.217385e-01 
regr.eval(Data_test$median_house_value, dist_test)
# mae          mse         rmse         mape 
# 9.330438e+04 1.354752e+10 1.163938e+05 6.363672e-01 


#### 2. First-order ####
lmMod <- lm(median_house_value ~  median_income  + total_bedrooms + 
              latitude  + population + longitude + housing_median_age +
              households + total_rooms + ocean_proximity, data = Data_train)  

dist_train <- predict(lmMod, Data_train)
dist_test <- predict(lmMod, Data_test)  

summary(lmMod) # total_rooms ; households 
# ocean_proximity cor. total_rooms ? 
# households cor. total_bedrooms ?
# 
car::vif(lmMod)
DAAG::vif(lmMod) # households ; total_bedrooms ; 

# MAPE & RMSE 
regr.eval(Data_train$median_house_value, dist_train)
# mae          mse         rmse         mape 
# 5.014067e+04 4.693704e+09 6.851061e+04 2.913840e-01 
regr.eval(Data_test$median_house_value, dist_test)
# mae          mse         rmse         mape 
# 5.001414e+04 4.613932e+09 6.792593e+04 2.904215e-01 


library(tidyverse)
library(caret)

#### 3. Stepwise ####

nullmodel <- lm(median_house_value ~ 1 , data = Data_train) 
fullmodel <- lm(median_house_value ~ . , data = Data_train) 
housestep <- step(nullmodel, 
                  scope=list(lower=nullmodel, upper=fullmodel),
                  direction="both")
housestep
# median_house_value ~ median_income + ocean_proximity + 
#   latitude + total_bedrooms + population + longitude + housing_median_age
# NO 'households' & 'total_rooms' 
summary(housestep)
names(Data_train)

step01 <- predict(housestep , Data_train)
step02 <- predict(housestep , Data_test)

# MAPE & RMSE
regr.eval(Data_train$median_house_value, step01)
# mae          mse         rmse         mape 
# 4.937236e+04 4.600638e+09 6.782800e+04 2.894012e-01 
regr.eval(Data_test$median_house_value, step02)
# mae          mse         rmse         mape 
# 4.913165e+04 4.519027e+09 6.722371e+04 2.863699e-01 


#### 4. Stepwise with poly & interaction####



#### 5. Lasso  ####
# alpha = 1 is Lasso；alpha = 0 is Ridge； 0 < alpha <1 is Elastic net
require(glmnet)
# 1. Use glmnet() to build basic Lasso model
x1 = model.matrix(median_house_value ~ . , data = Data_train)
x2 = model.matrix(median_house_value ~ . , data = Data_test)
y1 = Data_train$median_house_value
lasso = glmnet(x1,y1,alpha=1,family = "gaussian") 
#cross-validation to get the best lambda, and take it into the model 

# 2. Use cv.glmnet() to find the best panlty value of lambda ---- best.lambda
cv.lasso = cv.glmnet(x1,y1,alpha=1,family = "gaussian")
best.lasso.lambda = cv.lasso$lambda.min
best.lasso.lambda # [1] 356.8872

# 3. Use predict() to predict the result of Lasso
lasso.train = predict(lasso,s = best.lasso.lambda, newx = x1)
lasso.test = predict(lasso,s = best.lasso.lambda, newx = x2)
regr.eval(Data_train$median_house_value, lasso.train)
# mae          mse         rmse         mape 
# 4.931621e+04 4.614774e+09 6.793213e+04 2.855915e-01 
regr.eval(Data_test$median_house_value, lasso.test)
# mae          mse         rmse         mape 
# 4.910622e+04 4.535826e+09 6.734854e+04 2.834369e-01 


#### 6. Elastic net  ####
# Find the best alpha, then find the best lambda
# maintain the same folds across all models
x1 = model.matrix(median_house_value ~ . , data = Data_train )
x2 = model.matrix(median_house_value ~ . , data = Data_test)
y1 = Data_train$median_house_value
fold_id <- sample(x = 1:10, size = length(y1), replace = TRUE)

# search across a range of alphas
tuning_grid <- tibble::tibble(
  alpha      = seq(0, 1, by = .1),
  mse_min    = NA,
  mse_1se    = NA,
  lambda_min = NA,
  lambda_1se = NA
)

for(i in seq_along(tuning_grid$alpha)){
  # fit CV model for each alpha value
  fit <- cv.glmnet(x = x1, y = y1, alpha = tuning_grid$alpha[i],foldid = fold_id)
  # extract MSE and lambda values
  tuning_grid$mse_min[i]    <- fit$cvm[fit$lambda == fit$lambda.min]
  tuning_grid$mse_1se[i]    <- fit$cvm[fit$lambda == fit$lambda.1se]
  tuning_grid$lambda_min[i] <- fit$lambda.min
  tuning_grid$lambda_1se[i] <- fit$lambda.1se
}

library(dplyr); library(ggplot2)
# observe the graph to decide the best alpha. We tried two situation ---- alpha = 0 & alpha = 1.
tuning_grid %>%
  mutate(se = mse_1se - mse_min) %>% # calculate the distance of a SE
  ggplot(aes(alpha, mse_min)) + # Plot the min MSE of CV under different alpha
  geom_line(size = 2) +
  geom_ribbon(aes(ymax = mse_min + se, ymin = mse_min - se), alpha = .25) +
  ggtitle("MSE ± one standard error")


# create a Ridge model ,find the best lambda ,then get RMSE
cv_lasso <- cv.glmnet(x = x1, y = y1, alpha = 0)
pred_train <- predict(cv_lasso,newx = x1, s = cv_lasso$lambda.min)
pred_test <- predict(cv_lasso,newx = x2, s = cv_lasso$lambda.min)
regr.eval(Data_train$median_house_value, pred_train)
# mae          mse         rmse         mape 
# 5.042093e+04 4.819133e+09 6.941998e+04 2.891660e-01 
regr.eval(Data_test$median_house_value, pred_test)
# mae          mse         rmse         mape 
# 5.028658e+04 4.741019e+09 6.885506e+04 2.908131e-01 


# create a Lasso model ,find the best lambda ,then get RMSE
cv_lasso <- cv.glmnet(x = x1, y = y1, alpha = 1)
pred_train <- predict(cv_lasso,newx = x1, s = cv_lasso$lambda.min)
pred_test <- predict(cv_lasso,newx = x2, s = cv_lasso$lambda.min)
regr.eval(Data_train$median_house_value, pred_train)
# mae          mse         rmse         mape 
# 4.931465e+04 4.612416e+09 6.791477e+04 2.858544e-01 
regr.eval(Data_test$median_house_value, pred_test)
# mae          mse         rmse         mape 
# 4.910387e+04 4.533662e+09 6.733247e+04 2.836285e-01 


# create a Elastic model ,find the best lambda ,then get RMSE
cv_lasso <- cv.glmnet(x = x1, y = y1, alpha = 0.5)
pred_train <- predict(cv_lasso,newx = x1, s = cv_lasso$lambda.min)
pred_test <- predict(cv_lasso,newx = x2, s = cv_lasso$lambda.min)
regr.eval(Data_train$median_house_value, pred_train)
# mae          mse         rmse         mape 
# 4.931511e+04 4.616502e+09 6.794485e+04 2.855094e-01 
regr.eval(Data_test$median_house_value, pred_test)
# mae          mse         rmse         mape 
# 4.911146e+04 4.538234e+09 6.736642e+04 2.833860e-01 



#### 7. SVM ####

library('kernlab')
library(alr4); library(DMwR)

# spamFormulaV <- as.formula(paste('spam',
#                                  paste(spamVars,collapse=' + '),sep=' ~ '))
FormulaV = as.formula(median_house_value ~ median_income + ocean_proximity +
                        latitude  + longitude + housing_median_age + # total_bedrooms + population
                        households + total_rooms +
                        population_per_household + bedrooms_per_room + rooms_per_household)

# may want to switch to library('e1071') svm() as had some state holding problems in some examles
svmM <- ksvm(FormulaV,data=Data_train, 	# Note: 1  # spamTrain
             kernel='rbfdot', 	# Note: 2 
             C=15, 	# Note: 3  # C=10
             prob.model=T,cross=5, 	# Note: 4 
             # class.weights=c('spam'=1,'non-spam'=10) 	# Note: 5 
)
print(svmM)

# 10-fold. Pick up the best parameter C
# fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3,returnResamp = "all")
# model <- train(FormulaV, data=Data_train,method='svmRadialCost',trControl = fitControl)
# print(model)
# The final value used for the model was C = 1.

rf_train <- predict(svmM, Data_train)
rf_test <- predict(svmM, Data_test)

# MAPE & RMSE
regr.eval(Data_train$median_house_value, rf_train)
# mae          mse         rmse         mape 
# 3.009128e+04 2.298531e+09 4.794300e+04 1.555334e-01 
regr.eval(Data_test$median_house_value, rf_test)
# mae          mse         rmse         mape 
# 3.733928e+04 3.205611e+09 5.661811e+04 1.978888e-01 



#### 8. RF tuning ####
library(randomForest)
rf01 <- randomForest(median_house_value ~ . , 
                     data = Data_train , ntree = 150, mtry = 6, importance = T, replace = T)
rf_train <- predict(rf01, Data_train)
rf_test <- predict(rf01, Data_test)

# MAPE & RMSE
regr.eval(Data_train$median_house_value, rf_train)
# mae          mse         rmse         mape 
# 1.459442e+04 4.960962e+08 2.227322e+04 8.138840e-02 
regr.eval(Data_test$median_house_value, rf_test)
# mae          mse         rmse         mape 
# 3.404012e+04 2.622380e+09 5.120918e+04 1.868728e-01 

importance(rf01,1)
#                          %IncMSE
# longitude                25.69620
# latitude                 30.17928
# housing_median_age       43.77487
# total_rooms              13.99986
# total_bedrooms           17.31159
# population               11.77426
# households               13.73750
# median_income            61.48248
# ocean_proximity          61.04503
# population_per_household 63.91720
# bedrooms_per_room        17.43240
# rooms_per_household      16.25610


#### 9. RF best ####
# names(Data_train)
# median_house_value ~ median_income + ocean_proximity + 
#   latitude + total_bedrooms + population + longitude + housing_median_age 
# NO 'households' & 'total_rooms' is the best
# names(Data_train)
rf01 <- randomForest(median_house_value ~ median_income + ocean_proximity +
                       latitude  + longitude + housing_median_age + # total_bedrooms + population
                       households + total_rooms +
                       population_per_household + bedrooms_per_room + rooms_per_household ,  
                     data = Data_train , ntree = 300, mtry = 6, importance = T, replace = T)
plot(rf01)
tuneRF(Data_train[,-10], Data_train[,10]) # mtry = 6
# mtry   OOBError
# 2    2 3080490877
# 4    4 2804299684
# 8    8 2865183670

importance(rf01, 1)
#                            %IncMSE
# median_income            105.69750
# ocean_proximity           87.70960
# latitude                  38.55366
# longitude                 49.67933
# housing_median_age        61.37681
# households                18.60663
# total_rooms               21.33286
# population_per_household 125.71593
# bedrooms_per_room         28.13585
# rooms_per_household       27.52476

rf_train <- predict(rf01, Data_train)
rf_test <- predict(rf01, Data_test)

# MAPE & RMSE
regr.eval(Data_train$median_house_value, rf_train)
# mae          mse         rmse         mape 
# 1.442719e+04 4.902147e+08 2.214079e+04 8.014738e-02 
regr.eval(Data_test$median_house_value, rf_test)
# mae          mse         rmse         mape 
# 3.382165e+04 2.607849e+09 5.106711e+04 1.849024e-01 



#### 10. GBT  ####

require(xgboost)
require(nnet)
# str(Data_train) 
# head(Data_train)
# str(Data_train[,-c(5,6,9,10)])
data_train_nn = cbind(Data_train[,-c(5,6,9,10)] , class.ind(Data_train$ocean_proximity))
data_test_nn = cbind(Data_test[,-c(5,6,9,10)] , class.ind(Data_test$ocean_proximity))

dtrain = xgb.DMatrix(data = as.matrix(data_train_nn),
                     label = Data_train$median_house_value)
dtest = xgb.DMatrix(data = as.matrix(data_test_nn),
                    label = Data_test$median_house_value)

# 2. set xgb.params

xgb.params = list(
  # col sampling proportion. Higher -> complexity up
  colsample_bytree = 0.5,
  # row sampling proportion. Higher -> complexity up
  subsample = 0.5,
  booster = "gbtree",
  # max depth of a tree. Higher -> complexity up
  max_depth = 4,
  # boosting would increase the weight of wrong classification. Higher -> complexity down
  eta = 0.03, # 0.03
  # 'mae' is ok
  eval_metric = "rmse",  # rmse or mae
  objective = "reg:linear",
  # Higher -> complexity down
  gamma = 0)            

# 3. 使用xgb.cv()，tune 出最佳的決策樹數量
cv.model = xgb.cv(
  params = xgb.params, 
  data = dtrain,
  nfold = 5,     # 5-fold cv
  nrounds= 130,   # 測試1-100，各個樹總數下的模型
  # 如果當nrounds < 30 時，就已經有overfitting情況發生，那表示不用繼續tune下去了，可以提早停止                
  early_stopping_rounds = 100, 
  print_every_n = 100 # 每20個單位才顯示一次結果，
) 

tmp = cv.model$evaluation_log

plot(x=1:nrow(tmp), y= tmp$train_rmse_mean, col='red', xlab="nround", ylab="rmse", main="Avg.Performance in CV") 
points(x=1:nrow(tmp), y= tmp$test_rmse_mean, col='blue') 
legend("topright", pch=1, col = c("red", "blue"), 
       legend = c("Train", "Validation") )

# 獲得 best nround
best.nrounds = cv.model$best_iteration ; best.nrounds

# 4. 用xgb.train()建立模型
xgb.model = xgb.train(paras = xgb.params, 
                      data = dtrain,
                      nrounds = best.nrounds) 

# 如果要畫出 xgb 內的所有決策樹，可以用以下函式(但因為會很多，這裡就不畫了)
# xgb.plot.tree(model = xgb.model) 

# prediction
xgb_train = predict(xgb.model, dtrain)
regr.eval(Data_train$median_house_value, xgb_train)
# mae          mse         rmse         mape 
# 1.158997e+04 2.620055e+08 1.618658e+04 6.790063e-02 

xgb_test = predict(xgb.model, dtest)
regr.eval(Data_test$median_house_value, xgb_test)
# mae          mse         rmse         mape 
# 3.238790e+04 2.352802e+09 4.850569e+04 1.762927e-01 







# ___________-------------------------------------------------------------------------
# (5) Train_K-fold -------------------------------------------------------------------------

library(rBayesianOptimization)
library(randomForest, quietly = T)
library(rpart)
library(rpart.plot)

library(tidyverse)
library(caret)
library(dplyr)
library(pROC)
library(regclass)
library(car)
library(e1071)
library(adabag)
library(nnet)

# fold.filename = 10
# fold.filename == "5"

# k = 5
kfold01 = KFold(1:8669, nfolds = 5, stratified = F, seed = 6) 

acc_train = rep(NA, 5)
acc_test = rep(NA, 5)


# 3-way validation & k-fold

for (i in 1:5){
  
  # i = 1
  Data_test <- housing_train[unlist(kfold01[i]),]
  if( i == 5 ) { t = 1 }else{ t = i+1 } 
  # Data_valid <- Data[unlist(kfold01[t]),]
  index = unlist(c(kfold01[i]))
  # index = unlist(c(kfold01[i],kfold01[t])); # index; str(index)
  
  Data_train <- housing_train[(1:8346)[-index], ]; #Titanic_train
  
  
  #### 1. SVM ####
  # FormulaV = as.formula(median_house_value ~ median_income + ocean_proximity +
  #                         latitude  + longitude + housing_median_age + # total_bedrooms + population
  #                         households + total_rooms +
  #                         population_per_household + bedrooms_per_room + rooms_per_household)
  # 
  # # may want to switch to library('e1071') svm() as had some state holding problems in some examles
  # svmM <- ksvm(FormulaV,data=Data_train, 	# Note: 1  # spamTrain
  #              kernel='rbfdot', 	# Note: 2
  #              C=10 , 	# Note: 3  # C=10
  #              prob.model=T,cross=5, 	# Note: 4
  #              # class.weights=c('spam'=1,'non-spam'=10) 	# Note: 5
  # )
  # 
  # # Prediction
  # model_train <- predict(svmM, Data_train)
  # model_test <- predict(svmM, Data_test)
  # 
  # # root-mean-square error
  # acc_train[i] = regr.eval(Data_train$median_house_value, model_train)[4]
  # acc_test[i] = regr.eval(Data_test$median_house_value, model_test)[4]
  
  
  #### 2. RF ####
  rf01 <- randomForest(median_house_value ~ median_income + ocean_proximity +
                         latitude  + longitude + housing_median_age + # total_bedrooms + population
                         households + total_rooms +
                         population_per_household + bedrooms_per_room + rooms_per_household ,
                       data = Data_train , ntree = 300, mtry = 6, importance = T, replace = T)

  # Prediction
  model_train <- predict(rf01, Data_train)
  model_test <- predict(rf01, Data_test)

  # root-mean-square error
  acc_train[i] = regr.eval(Data_train$median_house_value, model_train)[4]
  acc_test[i] = regr.eval(Data_test$median_house_value, model_test)[4]
  
  
  #### 3. GBT ####
  # str(Data_train)
  # head(Data_train)
  # str(Data_train[,-c(5,6,9,10)])
  # data_train_nn = cbind(Data_train[,-c(5,6,9,10)] , class.ind(Data_train$ocean_proximity))
  # data_test_nn = cbind(Data_test[,-c(5,6,9,10)] , class.ind(Data_test$ocean_proximity))
  # 
  # dtrain = xgb.DMatrix(data = as.matrix(data_train_nn),
  #                      label = Data_train$median_house_value)
  # dtest = xgb.DMatrix(data = as.matrix(data_test_nn),
  #                     label = Data_test$median_house_value)
  # 
  # # 2. set xgb.params
  # 
  # xgb.params = list(
  #   # col sampling proportion. Higher -> complexity up
  #   colsample_bytree = 0.5,
  #   # row sampling proportion. Higher -> complexity up
  #   subsample = 0.5,
  #   booster = "gbtree",
  #   # max depth of a tree. Higher -> complexity up
  #   max_depth = 4,
  #   # boosting would increase the weight of wrong classification. Higher -> complexity down
  #   eta = 0.03, # 0.03
  #   # 'mae' is ok
  #   eval_metric = "rmse",  # rmse or mae
  #   objective = "reg:linear",
  #   # Higher -> complexity down
  #   gamma = 0)
  # 
  # # 3. 使用xgb.cv()，tune 出最佳的決策樹數量
  # cv.model = xgb.cv(
  #   params = xgb.params,
  #   data = dtrain,
  #   nfold = 5,     # 5-fold cv
  #   nrounds= 130,   # 測試1-100，各個樹總數下的模型
  #   # 如果當nrounds < 30 時，就已經有overfitting情況發生，那表示不用繼續tune下去了，可以提早停止
  #   early_stopping_rounds = 100,
  #   print_every_n = 100 # 每20個單位才顯示一次結果，
  # )
  # 
  # tmp = cv.model$evaluation_log
  # 
  # plot(x=1:nrow(tmp), y= tmp$train_rmse_mean, col='red', xlab="nround", ylab="rmse", main="Avg.Performance in CV")
  # points(x=1:nrow(tmp), y= tmp$test_rmse_mean, col='blue')
  # legend("topright", pch=1, col = c("red", "blue"),
  #        legend = c("Train", "Validation") )
  # 
  # # 獲得 best nround
  # best.nrounds = cv.model$best_iteration
  # # best.nrounds
  # 
  # # 4. 用xgb.train()建立模型
  # xgb.model = xgb.train(paras = xgb.params,
  #                       data = dtrain,
  #                       nrounds = best.nrounds)
  # 
  # # 如果要畫出 xgb 內的所有決策樹，可以用以下函式(但因為會很多，這裡就不畫了)
  # # xgb.plot.tree(model = xgb.model)
  # 
  # # prediction
  # xgb_train = predict(xgb.model, dtrain)
  # xgb_test = predict(xgb.model, dtest)
  # 
  # acc_train[i] = regr.eval(Data_train$median_house_value, xgb_train)[4]
  # acc_test[i] = regr.eval(Data_test$median_house_value, xgb_test)[4]
  
  print(i)
}

acc_train02 = c(acc_train, mean(acc_train))
acc_test02 = c(acc_test, mean(acc_test))
acc_train02; acc_test02
mean(acc_train); mean(acc_test)


#### 4. SVM result ####
# > acc_train02; acc_test02
# [1] 0.1604364 0.1610779 0.1599301 0.1501555 0.1602352 0.1583670
# [1] 0.2040427 0.2027319 0.1933996 0.1865918 0.2084793 0.1990491
# > mean(acc_train); mean(acc_test)
# [1] 0.158367
# [1] 0.1990491


#### 5. RF result ####
# > acc_train02; acc_test02
# [1] 0.07995930 0.07898698 0.07808058 0.07861884 0.07754830 0.07863880
# [1] 0.1850657 0.1784582 0.1918219 0.1933260 0.2111343 0.1919612
# > mean(acc_train); mean(acc_test)
# [1] 0.0786388
# [1] 0.1919612

#### 6. GBT result ####
# > acc_train02; acc_test02
# [1] 0.06790063 0.07037835 0.06743188 0.07165489 0.06633658 0.06874047
# [1] 0.1762927 0.1689866 0.1811226 0.1916840 0.2064967 0.1849165
# > mean(acc_train); mean(acc_test)
# [1] 0.06874047
# [1] 0.1849165


#### 7. Comparison ####
# SVM : 0.1990491
# RF  : 0.1919612
# GBT : 0.1849165


# ___________-------------------------------------------------------------------------
# (6) test_30 (Public score) -------------------------------------------------------------------------

housing_train = read.table("housing_train_mice.csv", sep=",", quote = "\"",header = T)

housing_test_30 = read.table("housing_test_30_mice.csv", sep=",", quote = "\"",header = T)
housing_test_30_ans = read.table("housing_test_30_answer.csv", sep=",", quote = "\"",header = T)
housing_test_70 = read.table("housing_test_70_mice.csv", sep=",", quote = "\"",header = T)
housing_test_70_ans = read.table("housing_test_70_answer.csv", sep=",", quote = "\"",header = T)


#### feature engineering ####

## train
## population_per_household 
housing_train$ocean_proximity[housing_train$ocean_proximity == "ISLAND" ] = "NEAR BAY"
housing_train$population_per_household = housing_train$population / housing_train$households
## bedrooms_per_room 
housing_train$bedrooms_per_room = housing_train$total_bedrooms / housing_train$total_rooms
## rooms_per_household
housing_train$rooms_per_household = housing_train$total_rooms / housing_train$households

## test_30
## population_per_household 
housing_test_30$ocean_proximity[housing_test_30$ocean_proximity == "ISLAND" ] = "NEAR BAY"
housing_test_30$population_per_household = housing_test_30$population / housing_test_30$households
## bedrooms_per_room 
housing_test_30$bedrooms_per_room = housing_test_30$total_bedrooms / housing_test_30$total_rooms
## rooms_per_household
housing_test_30$rooms_per_household = housing_test_30$total_rooms / housing_test_30$households

## test_70
## population_per_household 
housing_test_70$ocean_proximity[housing_test_70$ocean_proximity == "ISLAND" ] = "NEAR BAY"
housing_test_70$population_per_household = housing_test_70$population / housing_test_70$households
## bedrooms_per_room 
housing_test_70$bedrooms_per_room = housing_test_70$total_bedrooms / housing_test_70$total_rooms
## rooms_per_household
housing_test_70$rooms_per_household = housing_test_70$total_rooms / housing_test_70$households


library(rBayesianOptimization)
library(randomForest, quietly = T)
library(rpart)
library(rpart.plot)

library(tidyverse)
library(caret)
library(dplyr)
library(pROC)
library(regclass)
library(car)
library(e1071)
library(adabag)
library(nnet)
# 
# ### 1. SVM ####
FormulaV = as.formula(median_house_value ~ median_income + ocean_proximity +
                        latitude  + longitude + housing_median_age + # total_bedrooms + population
                        households + total_rooms +
                        population_per_household + bedrooms_per_room + rooms_per_household)

# may want to switch to library('e1071') svm() as had some state holding problems in some examles
svmM <- ksvm(FormulaV,data= housing_train, 	# Note: 1  # spamTrain
             kernel='rbfdot', 	# Note: 2
             C=10, 	# Note: 3  # C=10
             prob.model=T,cross=5, 	# Note: 4
             # class.weights=c('spam'=1,'non-spam'=10) 	# Note: 5
)

# Prediction
SVM_pred_train <- predict(svmM, housing_train)
# common <- intersect(names(housing_train), names(housing_test_30_mice))
# for (p in common) {
#   if (class(housing_train[[p]]) == "factor") {
#     levels(housing_test_30_mice[[p]]) <- levels(housing_train[[p]])} }
SVM_pred_test <- predict(svmM, cbind(housing_test_30,housing_test_30_ans))

# MAPE & RMSE
SVM_MAPE_train = regr.eval(housing_train$median_house_value, SVM_pred_train)[4]
SVM_MAPE_test = regr.eval(housing_test_30_ans[[1]], SVM_pred_test)[4]

SVM_MAPE_train ; SVM_MAPE_test
# C = 10  # (0.1607815 , 0.2463727) 


#### 2. RF ####
rf01 <- randomForest(median_house_value ~ median_income + ocean_proximity +
                       latitude  + longitude + housing_median_age + # total_bedrooms + population
                       households + total_rooms +
                       population_per_household + bedrooms_per_room + rooms_per_household ,
                     data = Data_train , ntree = 300, mtry = 6, importance = T, replace = T)

RF_train <- predict(rf01, Data_train)

common <- intersect(names(Data_train), names(housing_test_30)) 
for (p in common) { 
  if (class(Data_train[[p]]) == "factor") { 
    levels(housing_test_30[[p]]) <- levels(Data_train[[p]])} }

RF_test <- predict(rf01, housing_test_30)

# root-mean-square error, MAPE
RF_result_train = regr.eval(Data_train$median_house_value, RF_train)[4]
RF_result_test = regr.eval(housing_test_30_ans[[1]], RF_test)[4]
RF_result_train; RF_result_test 
# RF mape = (0.07857822  , 0.1991604 )


#### 3. GBT ####
# str(Data_train)
# head(Data_train)
# str(Data_train[,-c(5,6,9,10)])
# str(Data_train)
# str(housing_test_30)

data_train_nn = cbind(Data_train[,-c(5,6,9,10)] , class.ind(Data_train$ocean_proximity))
data_test_nn = cbind(housing_test_30[,-c(5,6,9)] , class.ind(housing_test_30$ocean_proximity))

dtrain = xgb.DMatrix(data = as.matrix(data_train_nn),
                     label = Data_train$median_house_value)
dtest = xgb.DMatrix(data = as.matrix(data_test_nn),
                    label = housing_test_30_ans[[1]])

# 2. set xgb.params

xgb.params = list(
  # col sampling proportion. Higher -> complexity up
  colsample_bytree = 0.5,
  # row sampling proportion. Higher -> complexity up
  subsample = 0.5,
  booster = "gbtree",
  # max depth of a tree. Higher -> complexity up
  max_depth = 4,
  # boosting would increase the weight of wrong classification. Higher -> complexity down
  eta = 0.03, # 0.03
  # 'mae' is ok
  eval_metric = "rmse",  # rmse or mae
  objective = "reg:linear",
  # Higher -> complexity down
  gamma = 0)

# 3. 使用xgb.cv()，tune 出最佳的決策樹數量
cv.model = xgb.cv(
  params = xgb.params,
  data = dtrain,
  nfold = 5,     # 5-fold cv
  nrounds= 130,   # 測試1-100，各個樹總數下的模型
  # 如果當nrounds < 30 時，就已經有overfitting情況發生，那表示不用繼續tune下去了，可以提早停止
  early_stopping_rounds = 100,
  print_every_n = 100 # 每20個單位才顯示一次結果，
)

tmp = cv.model$evaluation_log

plot(x=1:nrow(tmp), y= tmp$train_rmse_mean, col='red', xlab="nround", ylab="rmse", main="Avg.Performance in CV")
points(x=1:nrow(tmp), y= tmp$test_rmse_mean, col='blue')
legend("topright", pch=1, col = c("red", "blue"),
       legend = c("Train", "Validation") )

# 獲得 best nround
best.nrounds = cv.model$best_iteration
# best.nrounds

# 4. 用xgb.train()建立模型
xgb.model = xgb.train(paras = xgb.params,
                      data = dtrain,
                      nrounds = best.nrounds)

# 如果要畫出 xgb 內的所有決策樹，可以用以下函式(但因為會很多，這裡就不畫了)
# xgb.plot.tree(model = xgb.model)

# prediction
GBT_train = predict(xgb.model, dtrain)

# common <- intersect(names(dtrain), names(dtest)) 
# for (p in common) { 
#   if (class(dtrain[[p]]) == "factor") { 
#     levels(dtest[[p]]) <- levels(dtrain[[p]])} }

GBT_test = predict(xgb.model, dtest)

GBT_result_train = regr.eval(Data_train$median_house_value, GBT_train)[4]
GBT_result_test = regr.eval(housing_test_30_ans[[1]], GBT_test)[4]
GBT_result_train; GBT_result_test
# GBT mape = (0.06966617 , 0.1904664)



#### 4. Stacking ####

stacking_test = (RF_test + GBT_test)/2
stacking_result_test = regr.eval(housing_test_30_ans[[1]], stacking_test)[4]; stacking_result_test
# mape = 0.1872631

# cf. 
# RF mape = 0.1991604
# GBT mape = 0.1904664


#### 5. Public Score ####
# 1st submission => SVM : 0.2463727
# 2st submission => RF : 0.1991604
# 3st submission => GBT : 0.1904664
# 4st submission => Stacking : 0.1872631




# ___________-------------------------------------------------------------------------
# (7) test_100 (public score + private score) -------------------------------------------------------------------------

housing_test_100 = rbind(housing_test_30, housing_test_70)
housing_test_100_ans = rbind(housing_test_30_ans, housing_test_70_ans)

#### 1. SVM ####

# Prediction
SVM_pred_train <- predict(svmM, housing_train)
# common <- intersect(names(housing_train), names(housing_test_30_mice))
# for (p in common) {
#   if (class(housing_train[[p]]) == "factor") {
#     levels(housing_test_30_mice[[p]]) <- levels(housing_train[[p]])} }
SVM_pred_test <- predict(svmM, cbind(housing_test_100,housing_test_100_ans))

# MAPE & RMSE
SVM_MAPE_train = regr.eval(housing_train$median_house_value, SVM_pred_train)[4]
SVM_MAPE_test = regr.eval(housing_test_100_ans[[1]], SVM_pred_test)[4]

SVM_MAPE_train ; SVM_MAPE_test
# C = 10  
# SVM mape = (0.1615183 , 0.2111227) 


#### 2. RF ####
RF_train <- predict(rf01, Data_train)

common <- intersect(names(Data_train), names(housing_test_100)) 
for (p in common) { 
  if (class(Data_train[[p]]) == "factor") { 
    levels(housing_test_100[[p]]) <- levels(Data_train[[p]])} }

RF_test <- predict(rf01, housing_test_100)

# root-mean-square error, MAPE
RF_result_test = regr.eval(housing_test_100_ans[[1]], RF_test)[4]
RF_result_train; RF_result_test 
# RF mape = (0.07857822  , 0.1943278)


#### 3. GBT ####
data_train_nn = cbind(Data_train[,-c(5,6,9,10)] , class.ind(Data_train$ocean_proximity))
data_test_nn = cbind(housing_test_30[,-c(5,6,9)] , class.ind(housing_test_30$ocean_proximity))
data_test_nn_100 = cbind(housing_test_100[,-c(5,6,9)] , class.ind(housing_test_100$ocean_proximity))

dtrain = xgb.DMatrix(data = as.matrix(data_train_nn),
                     label = Data_train$median_house_value)
dtest = xgb.DMatrix(data = as.matrix(data_test_nn),
                    label = housing_test_30_ans[[1]])
dtest_100 = xgb.DMatrix(data = as.matrix(data_test_nn_100),
                    label = housing_test_100_ans[[1]])

# prediction
GBT_train = predict(xgb.model, dtrain)
# common <- intersect(names(dtrain), names(dtest)) 
# for (p in common) { 
#   if (class(dtrain[[p]]) == "factor") { 
#     levels(dtest[[p]]) <- levels(dtrain[[p]])} }
GBT_test = predict(xgb.model, dtest_100)

GBT_result_train = regr.eval(Data_train$median_house_value, GBT_train)[4]
GBT_result_test = regr.eval(housing_test_100_ans[[1]], GBT_test)[4]
GBT_result_train; GBT_result_test
# GBT mape = (0.06966617 , 0.1852774)



#### 4. Stacking ####

stacking_test = (RF_test + GBT_test)/2
stacking_result_test = regr.eval(housing_test_100_ans[[1]], stacking_test)[4]; stacking_result_test
# mape = 0.1816812 

# cf. 
# RF mape = 0.1943278 
# GBT mape = 0.1852774



#### 5. Final Score ####

# 1st submission => SVM : 0.2111227
# 2st submission => RF : 0.1943278
# 3st submission => GBT : 0.1852774
# 4st submission => Stacking : 0.1816812


## cf. public score 
# 1st submission => SVM : 0.2463727
# 2st submission => RF : 0.1991604
# 3st submission => GBT : 0.1904664
# 4st submission => Stacking : 0.1872631
