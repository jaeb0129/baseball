# 2023 FA 분석
library(dplyr)
library(car)
library(lmtest)
library(olsrr)

fa <- read.csv('C:/Users/jaeb0/Desktop/baseball/22년 FA 예상/baserecord.csv')
head(fa)
fa23 <- fa %>% filter(Start_year == 2023)

str(fa)
names(fa)

fa_not23 <- fa %>% filter(Start_year != 2023) %>% filter(Start_year %in% seq(2012,2022,1))
fa_not23$rank <- factor(fa_not23$rank, levels=c('S', 'A', 'B', 'C'))
# 등급은 정성평가 / 시장에서의 평가
contrasts(fa_not23$rank)

fit1 <- lm(Salary_yearly ~ Start_age + X1b + X2b +X3b + Start_year + rank, data=fa_not23out1)
summary(fit1)
coef(fit1)

plot(fit1)
ols_plot_cooksd_bar(fit1)

fa_not23out <- fa_not23[-c(6, 33, 46, 50, 64),]
# 손아섭 롯데-> 4년 64억
# 오지환 LG 4년 40억
# 양의지 두산->NC 4년 125억
# 이재원 SK(SSG) 4년 69억
# 손아섭 롯데 4년 98억
fa_not23out1 <- fa_not23out[-c(7, 8, 91,100),] 
# 김재환 4년 115억
# 박건우 두산->NC 6년 100억
# 김태균 한화 4년 84억
# 유한준 넥센->KT 4년 60억

shapiro.test(fit1$residual) # 정규성 만족 
dwtest(fit1) # 독립성
fit1 %>% ncvTest()# 등분산성
fit1 %>% vif()# 다중공선성x

fit <- list('A' = coef(fit1))

money <- function(a, b, c, d, e, f){
  fit <- fit[[a]]
  fit[2] * b + fit[3] * c + fit[4] * d + fit[5] * e + fit[6] * f + fit[1]
}

money1 = round(money('A', fa23[,6], fa23[,7], fa23[,8], fa23[,9], fa23[,11]) + ifelse(fa23$rank =='A', -6.34136, 0 )+
                 ifelse(fa23$rank == 'B', -8.68242, 0) + ifelse(fa23$rank == 'C', -10.14557,0),1)
money1

final <- data.frame(Name=fa23$Name, X_money = money1, Real = c(3.1, 15, 2, 6.3, 1, 25.3, 10, 20, 12.5,
                                                               17.5, 3, 1.3, 11.5, 6.3, 3.5, 16.3, 1.5, 
                                                               7.3, 4.5))
final$diff <- with(final, X_money-Real)
final_diff <- final %>% arrange(desc(diff))


# 10점을 1승으로
# 2020 2021 2022
# 유강남 11.8 9.5 7.7
# 박세혁 8.3 6.9 1.3
# 박동원 -8.3 -3.3 -8.1
# 양의지 -5 -4.4 -0.2
framing20_22 <- data.frame(name = c('박동원', '박세혁', ' 양의지', '유강남'), 
           result_2020 = c(-8.3, 8.3, -5, 11.8) , 
           result_2021 = c(-3.3, 6.9, -4.4, 9.5), 
           result_2022 = c(-8.1, 1.3, -0.2, 7.7))
framing20_22 <- framing20_22 %>% rowwise() %>%
  mutate(war20 = result_2020/10, war21 = result_2021/10, war22 = result_2022/10 )
framing20_22 # 심플하게 1war = 10득점으로 환산

# 모델 반영
fa23_framing <- fa23  %>% filter(Name %in% c('박동원', '박세혁', '양의지', '유강남')) %>% arrange(Name)
fa23_framing
New <- cbind(fa23_framing, framing20_22) 
New <- New %>% rename(b3 = X3b, b2 = X2b, b1 = X1b) %>% rowwise() %>% mutate(X3b = sum(b3, war20),
                                                        X2b = sum(b2, war21),
                                                        X1b = sum(b1, war22))

ncol(New)
catcher_money <- round(money('A', New[,6], New[,26], New[,27], New[,28], New[,11]) + ifelse(New$rank =='A', -6.34136, 0 )+
                         ifelse(New$rank == 'B', -8.68242, 0) + ifelse(New$rank == 'C', -10.14557,0),1)
catcher_money

final <- data.frame(Name=fa23$Name, X_money = money1, Real = c(3.1, 15, 2, 6.3, 1, 25.3, 10, 20, 12.5,
                                                               17.5, 3, 1.3, 11.5, 6.3, 3.5, 16.3, 1.5, 
                                                               7.3, 4.5))
final2 <- final %>% filter(Name %in% c('박동원', '박세혁', '양의지', '유강남')) %>% select(Name:Real) %>% arrange(Name)
final2$framing_money <- c(10.5, 8.5, 22.2, 13.9)
final2

final2$diff <- with(final2, framing_money-X_money)
final2_diff <- final2 %>% arrange(desc(diff))
final2_diff
