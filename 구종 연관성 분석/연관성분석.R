library(dplyr)

mlb_pbp_2022 <-  read.csv('C:/Users/jaeb0/Desktop/baseball/mlb_pbp/mlb_pbp_2022.csv')
head(mlb_pbp_2022)

# 투구수 500개 이상 투수 추출
data <- mlb_pbp_2022 %>% group_by(pitcher) %>% summarise(n=n()) %>% 
  rename(total = n) %>% filter(total >= 500)
data

# 위 투수들의 구종 추출, 결측치(구종 안찍힌 공) 제거
data2 <- distinct(mlb_pbp_2022 %>% filter(pitcher %in% data$pitcher) %>% group_by(player_name, pitch_name)%>% 
  select(player_name, pitch_name), player_name, pitch_name)
data2 <- data2 %>% filter(pitch_name != '')
data2

# 구종별 투구수
data3 <- distinct(mlb_pbp_2022 %>% filter(pitcher %in% data$pitcher) %>% 
                group_by(player_name, pitch_name, pitcher) %>% summarise(n=n()))

# 평균 구사율이 5%이상인 구종만 추출
final <- left_join(data3, data, by = 'pitcher')
final <- final %>% mutate(avg = round(n/total, 2)) %>% filter(avg >= 0.05)
final

#############################
## 연관성 분석

# install.packages('arules')
library(arules)

list <- split(data2$pitch_name, data2$player_name)
list

list.tran <- as(list,'transactions')
summary(list.tran)

################
# lift(향상도)가 1.2보다 큰 경우

list.rules <- apriori(list.tran, maxlen=3, sup=0.01, conf = 0.2)
summary(list.rules) # 기본 지지도 0.1 신뢰도 0.8

rules <- subset(list.rules, subset=lift>1.2 & !(lhs %in% 'Slow Curve'))
rules.sorted <- sort(rules, by = 'conf')
inspect(rules.sorted)

# 슬라이더, 스위퍼에 커브 껴있으면 자체적으로 제외
# 3번, 8번, 9번, 10번, 11번

# 신뢰도가 너무 낮으면 의미없는 규칙밖에 볼 수 없을 것이고, 
# 너무 높다면 당연한 규칙밖에 볼 수 없을 것이다.

# lift(향상도)가 0.8보다 작은 경우
list.rules1 <- apriori(list.tran, maxlen=3, sup=0.01, conf = 0.2)
summary(list.rules1) # 기본 지지도 0.1 신뢰도 0.8

rules1 <- subset(list.rules1, subset=lift<0.8 & !(lhs %in% 'Slow Curve'))
rules1.sorted <- sort(rules1, by = 'lift')
inspect(rules1.sorted)
