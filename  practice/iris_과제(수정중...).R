library(tidyverse)

# 1. 데이터 불러오기
data("iris")
head(iris)
tail(iris)

# 2. 데이터 구조 확인
str(iris)
sum(is.na(iris)) # 결측치 값이 없음...? 

summary(iris)
table(iris$'Species')

# 3. 데이터 시각화
plot(iris)

# ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
#   geom_point() +
#   labs(x = "Sepal Length", y = "Sepal Width")

ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Species)) +
  geom_point() +
  labs(x = "Petal Length", y = "Petal Width")

plot_histo <- ggplot(data = iris, mapping = aes(x = Petal.Length)) +
  geom_histogram(bins = 30) +
  facet_wrap(~Species, scales = 'free_x')
plot_histo


# 4. 머신러닝
set.seed(42)

## 데이터 분할
sample <- sample.int(n = nrow(iris),
                     size = floor(0.8 * nrow(iris)),
                     replace = FALSE)
train <- iris[sample, ]
test <- iris[-sample, ]

nrow(train) + nrow(test) == nrow(iris)

## 모델 설계
### 선형 모델
install.packages("boot")
library(boot)

colnames(iris)

# glm 모델 훈련
glm_iris <- glm(Sepal.Width ~ Sepal.Length + Petal.Length + Petal.Width,
                data = iris)

# 교차 검증 오차 계산
k_fold_cv_error <- cv.glm(iris, glm_iris, K = 5)
k_fold_cv_error$delta

glm_cv_rmse <- sqrt(k_fold_cv_error$delta)[1]
glm_cv_rmse

glm_iris$coefficients

### 랜덤 포레스트
install.packages("randomForest")
library(randomForest)

# 분리
train_y <- train[, 'Species']
train_x <- train[, names(train) != 'Species']

# 모델 훈련
rf_model <- randomForest(train_x,
                         y = train_y,
                         ntree = 500,
                         importance = TRUE)

# 중요 변수 확인
rf_model$importance

# 테스트 세트 예측 및 평가
test_y = test[, 'Species']
test_x = test[, names(test) != 'Species']
y_pred = predict(rf_model, test_x)
test_mse = mean((y_pred - test_y)^2)
test_rmse = sqrt(test_mse)
test_rmse

head(train_y)
head(train_x)

# 예측
predictions <- predict(glm_iris, testData[, -5])
predictions

# 모델 평가
confusionMatrix(predictions, testData$Species)

