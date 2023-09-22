# tidyverse
# palmerpenguins
library("tidyverse")
library("palmerpenguins")

## 1. 데이터 확인
glimpse(penguins)
t(map_df(penguins, ~sum(is.na(.))))

# 원데이터는 손대지 xx 
plot_data <- penguins %>%
  drop_na()

t(map_df(plot_data, ~sum(is.na(.))))

## 2. 데이터 구성(이미지 표현, 종)

count_data <- plot_data %>% 
  group_by(species) %>% 
  tally() # = count()

ggplot(count_data) + #데이터는 레이어
  aes(x = species, fill = species, weight = n) +
  geom_bar()
