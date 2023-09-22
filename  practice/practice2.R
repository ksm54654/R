install.packages("tidyverse")
library("tidyverse")
data(mpg)
mpg
# tibble은 'data.frame'과 유사한 자료구조를 제공
# 데이터셋이 젤먼저 앞에 !!! 

filter(mpg, manufacturer == "hyundai")

hyundai_2008 <- filter(mpg, manufacturer == "hyundai", year == 2008)
hyundai_2008

slice(hyundai_2008, 1)

arrange(hyundai_2008, model, trans)
