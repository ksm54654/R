#연습용

## 1. 파일을 읽어야 됨

df <- read.csv("data/시군구_성_월별_출생_2021.csv", fileEncoding = "euc-kr")
df

## 2. (애매한) 전처리

colnames(df) # 이제 뭔가 망한 것 같아요...
# 뭘 어떻게 하죠?
# x 싹 날리고싶다 / 규칙성 ~ , ~.1, ~.2 -> 전체 남 여 
# .기준으로 split 해서 2개로 나뉘어지면 전체, 3개로 나뉘어지고 1이면 남, 2면 여

# gsub("X", "", paste(n[1], n[2], "전체"), sep =".")

f <- function(x) {
  n <- unlist(strsplit(x, "\\."))
  if (length(n) == 1) {
    return(x)
  } else if (length(n) == 2){
    return(gsub("X", "", paste(n[1], n[2], "전체", sep =".")))
  } else {
    if (identical(n[3], "1")) { # 1이 str !!! 
      return(gsub("X", "", paste(n[1], n[2], "남자", sep =".")))
    } else {
      return(gsub("X", "", paste(n[1], n[2], "여자", sep =".")))
    }
  }
}

names(df) <- lapply(colnames(df), f) # 벡터타입은 lapply
names(df)

head(df)

## 3. 데이터 분석이 용이하도록 구조 변경 (데이터 녹일거)

library(reshape2)

melt_data <- melt(df, id = "시군구별")
head(melt_data) # 시군구별 xx 

melt_data[melt_data["시군구별"] == "시군구별"] #[==]안은 불 값이므로 키값이 판단 제대로 x 
unique(melt_data$시군구별) # $는 컬럼 선택하는 것

df2 <- melt_data[!(melt_data["시군구별"] == "시군구별"),] # for문돌면 너무오래걸림... 
head(df2) # 인덱스1 사라짐 !!! 

## 4. 데이터 정리
f1 <- function(x) {
  n <- unlist(strsplit(x, "\\."))
  return(n[1])
}

f2 <- function(x) {
  n <- unlist(strsplit(x, "\\."))
  return(n[2])
}

f3 <- function(x) {
  n <- unlist(strsplit(x, "\\."))
  return(n[3])
}

df2["연도"] <- apply(df2["variable"], 1, f1)
df2["월"] <- apply(df2["variable"], 1, f2)
df2["성별"]  <- apply(df2["variable"], 1, f3)
head(df2)

colnames(df2)[3] <- "출생아수"
head(df2)

## 5. 데이터 선별

df_all = df2[(df2["시군구별"] == "전국") & (df2["성별"] == "전체"),]
df_all = df_all[, c("출생아수", "연도", "월")]
df_all

## 6. 시각화로 확인해보자  -> 합계가 필요하다
  # 원값을 그대로 넘겨야함, 
sum_agg = aggregate(as.integer(df_all$출생아수)~as.integer(df_all$연도), FUN=sum)
sum_agg

mode(df_all["출생아수"]) # "list"
mode(df_all$출생아수) # "character"
class(df_all["출생아수"]) # "data.frame"
class(df_all$출생아수) # "character"

colnames(sum_agg)[1] <- "연도"
colnames(sum_agg)[2] <- "출생아수"
colnames(sum_agg)
plot(sum_agg$연도, sum_agg$출생아수, type = 'b')

