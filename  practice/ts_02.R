library(tidyverse)
library(reshape2)

# 1. 데이터 불러오기
housing = read.csv("data/housing.csv")

## 앞 / 뒤를 확인해서 해당 데이터 확인
head(housing)
tail(housing)
summary(housing) # df.info() 

## 데이터 전체 구조를 확인
str(housing)

## EDA를 위한 간단한 시각화
### "과정과 결과"를 정의해서 해당 문제를 빠르게 해결 
# hist(housing$longitude)
plot_histo <- ggplot(data = melt(housing), mapping = aes(x = value)) + 
  geom_histogram(bins = 30) + 
  facet_wrap(~variable, scales = 'free_x')
plot_histo

ggplot(data = housing, mapping = aes(x = longitude, y = latitude, color = median_house_value)) +
  geom_point(aes(size = population), alpha = 0.4)

# 2. 전처리
## 이상치 처리 - 집값이 아주 비싼게 있어서 그거먼저 처리
# bedroom_mean <- mean(housing$total_bedrooms, na.rm = T)
# bedroom_median <- median(housing$total_bedrooms, na.rm = T)
housing$total_bedrooms[is.na(housing$total_bedrooms)] <- median(housing$total_bedrooms, na.rm = T)
housing$mean_bedrooms <- housing$total_bedrooms / housing$households
housing$mean_rooms <- housing$total_rooms / housing$households
head(housing)

drops <- c('total_bedrooms', 'total_rooms')
housing <- housing[,!(names(housing) %in% drops)]
housing
# ggplot(data = housing, mapping = aes(x = total_bedrooms)) +
#   geom_histogram(bins = 30, color = "black", fill = "blue") + 
#   geom_vline(aes(xintercept = bedroom_mean, color = "red"), lwd = 1.5) +
#   geom_vline(aes(xintercept = bedroom_median, color = "yellow"), lwd = 1.5)

## 범주형 
categories <- unique(housing$ocean_proximity)
cat_housing <- data.frame(ocean_proximity = housing$ocean_proximity)
head(cat_housing)

for(cat in categories) {
  # 시리즈... 
  cat_housing[, cat] = rep(0, times = nrow(cat_housing))
}

  # R 묹법 (이니셜라이즈 / restconpresive??)
for(i in 1:length(cat_housing$ocean_proximity)) {
  cat <- as.character(cat_housing$ocean_proximity[i])
  cat_housing[, cat][i] <- 1
}

head(cat_housing)

cat_names <- names(cat_housing)
keep_colums <- cat_names[cat_names != "ocean_proximity"]
cat_housing <- select(cat_housing, one_of(keep_colums))
tail(cat_housing)

## 결측치 처리 - 중앙값은 평균에 비해 덜 민감하다(극단치가 있으면 평균이... 좀 ... )
### 중위값으로 한다고 가정하자! 
### (수치형)

colnames(housing)

drops <- c("ocean_proximity", "median_house_value")
housing_num <- housing[, !(names(housing) %in% drops)]
colnames(housing_num)

scaled_housing_num <- scale(housing_num)
head(scaled_housing_num)

## 결합
head(cat_housing)
head(scaled_housing_num)
head(housing$median_house_value)

cleaned_housing <- cbind(cat_housing, scaled_housing_num, median_house_value = housing$median_house_value)

head(cleaned_housing)

# 3. 머신러닝
set.seed(42)

## 데이터 분리
sample <- sample.int(n = nrow(cleaned_housing),
                     size = floor(.8 * nrow(cleaned_housing)),
                     replace = F)
train <- cleaned_housing[sample,]
test <- cleaned_housing[-sample,]

nrow(train) + nrow(test) == nrow(cleaned_housing)

## 모델 설계
### 선형 모델 
install.packages("boot")
library(boot)
colnames(cleaned_housing)
glm_house <- glm(median_house_value ~ median_income+mean_rooms+population,
                 data = cleaned_housing)

k_fold_cv_error <- cv.glm(cleaned_housing, glm_house, K=5)
k_fold_cv_error$delta

glm_cv_rmse = sqrt(k_fold_cv_error$delta)[1]
glm_cv_rmse

glm_house$coefficients

## 랜덤 포레스트
install.packages("randomForest")
library(randomForest)

### 중요 변수값 확인
names(train)
train_y = train[,'median_house_value']
train_x = train[, names(train) != 'median_house_value']

rf_model = randomForest(train_x,
                        y = train_y,
                        ntree = 500,
                        importance = T)

rf_model$importance

test_y = test[,'median_house_value']
test_x = test[, names(test) != 'median_house_value']
y_pred = predict(rf_model, test_x)
test_mse = mean((y_pred - test_y)^2)
test_rmse = sqrt(test_mse)
test_rmse

head(train_y)
head(train_x)

### XGBoost
install.packages("xgboost")
library(xgboost)

# 데이터타입 맞추는게 ... 어려움 
dtrain = xgb.DMatrix(data = as.matrix(train_x), label = train_y)
dtest = xgb.DMatrix(data = as.matrix(test_x), label = test_y)
watchlist = list(train = dtrain, test = dtest)
bst <- xgb.train(data = dtrain,
                 max.depth = 8,
                 eta = 0.3,
                 nthread = 2,
                 nround = 1000,
                 watchlist = watchlist,
                 objective = "reg:squarederror",
                 early_stopping_rounds = 50,
                 print_every_n = 500)

# 4. 결과 확인

# XGBoost를 활용한 RMSE 값이 가장 낮고(48403 -> 47810), 주요 특징값은 ''
## 1)
## 2)
## 3)