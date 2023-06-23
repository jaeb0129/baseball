library(dplyr)
library(tidyverse)
library(ggplot2)

kbo2022 <- read.csv('C:/Users/jaeb0/2022.csv', fileEncoding = "CP949",encoding = 'UTF-8')
kbo2022 <- kbo2022 %>% filter(home_alias != '드림')
View(kbo2022)

taken_kbo <- kbo2022 %>%
  filter(pitch_result %in% c('볼', '스트라이크'))
unique(taken_kbo$pitch_result)

taken_kbo$type <- ifelse(taken_kbo$pitch_result %in% c('스트라이크'), 'S', 'B')
taken_kbo<- taken_kbo %>% filter(px >= -((plate_width/2)/12)*1.15 & px <= ((plate_width/2)/12)*1.15 &
                                   pz <= sz_top* 1.15 & pz >= sz_bot*0.85)

taken_kbo <- na.omit(taken_kbo)

# 모델링
library(mgcv)
strike_mod_kbo <- gam(type == 'S' ~ s(px, pz) + throws + stands,
                      family = binomial, data= taken_kbo)

library(broom)
hats_kbo <- strike_mod_kbo %>%
  augment(type.predict = "response") 

taken_kbo$.fitted <- hats_kbo$.fitted
taken_kbo <- taken_kbo %>% filter(!(.fitted >= 0.95 & type =='B'))

framing <- taken_kbo %>% filter(pitch_result %in% c('스트라이크', '볼')) %>%
  filter(pa_result %in% c('볼넷', '삼진', ''))
framing$s <- ifelse(framing$pitch_result == '스트라이크', (1-framing$.fitted), 0)
framing$b <- ifelse(framing$pitch_result == '볼', (-(framing$.fitted)), 0)
framing$expect <- with(framing, s+b)

name <- taken_kbo %>% group_by(pos_2) %>% summarise(n=n()) %>% filter(n > 2000) %>%
  arrange(desc(n))
c_name <- name$pos_2

df = data.frame()

for (i in 1:length(c_name)){
  catcher <- framing %>% filter(pos_2 == c_name[i])
  all = nrow(catcher)
  strike_plus = round(sum(catcher$s),0)
  ball_plus = round(sum(catcher$b),0)
  plus_call = round(with(catcher, strike_plus + ball_plus),0)
  df = rbind(df, c(c_name[i], all, strike_plus, ball_plus, plus_call))
  names(df) = c('이름', '판정 횟수', '추가 스트라이크', '추가 볼', '추가 콜')
}

result <- write.csv(df, 'C:/Users/jaeb0/Desktop/result.csv', fileEncoding = 'cp949',
                    row.names = TRUE) 
result
