library(tidyverse)
library(DBI)
library(RSQLite)

con <- dbConnect(RSQLite::SQLite(), "test.db")
dbListTables(con)
# 데이터 정리해서 데이터베이스에 넣어야함 .
moving_data <- read_csv("./data/seoul_moving_202107_09_hr.csv")
reference <- readxl::read_excel("./data/reference.xlsx")

glimpse(moving_data)
glimpse(reference)

names(moving_data) <- gsub(" ", "", names(moving_data))
names(moving_data)[9:10] <- c("평균이동시간_분", "이동인구_합")
names(reference) <- c("시도코드", "시군구코드", "시군구이름", "전체이름")

copy_to(con, moving_data, "moving_data",
        temporary = FALSE,
        indexes = list("대상연월", "요일", "도착시간",
                       "출발시군구코드", "도착시군구코드"),
        overwrite = TRUE)

copy_to(con, reference, "reference",
        temporary = FALSE,
        indexes = list("시군구코드"),
        overwrite = TRUE)

dbListTables(con)

moving_db <- tbl(con, "moving_data")
moving_db %>% head(6)

reference_db <- tbl(con,"reference")
reference_db 

# 평균 이동시간 기준으로 이동 거리를 중/단/장기로 구분 

moving_db <- moving_db %>% 
  mutate(평균이동시간_시 = 평균이동시간_분 / 60) %>% 
  mutate(이동타입 = case_when(
    between(평균이동시간_시, 0, 0.5) ~ "단기",
    between(평균이동시간_시, 0, 1) ~ "중기",
    평균이동시간_시 >= 1 ~ "장기",
    TRUE ~ as.character(평균이동시간_시)
  )) %>% 
  relocate(이동타입)

moving_db %>% colnames()

glimpse(moving_db)
moving_db$평균이동시간_시 # NULL로 나오는 이유?? 



