library(rvest)
library(dplyr)
# 네이버 일정표
url = 'https://sports.news.naver.com/kbaseball/schedule/index'

html <- read_html(url)

html2 <- html_nodes(html, '.team_lft')

html3 <- html_nodes(html, '.team_rgt')

######################
# 원정팀
html %>% html_nodes('td') %>% html_nodes(css = 'span.team_lft') %>% html_text() 
# 경기결과(취소는 'VS'로 표기)
html %>% html_nodes('td') %>% html_nodes(css = 'strong.td_score') %>% html_text() 
# 홈팀
html %>% html_nodes('td') %>% html_nodes(css = 'span.team_rgt') %>% html_text()

basic_url = 'https://sports.news.naver.com/kbaseball/schedule/index?&month=09&year=2022'
urls <- NULL
for(x in 0:1){
  urls[x+1] <- paste0('https://sports.news.naver.com/kbaseball/schedule/index?&month=', x+9, '&year=2022')
}
urls

away <- NULL
home <- NULL
score <- NULL
for(url in urls){
  html <- read_html(url)
  away <- c(away, html %>% html_nodes('td') %>% html_nodes(css = 'span.team_lft') %>% html_text())
  home <- c(home, html %>% html_nodes('td') %>% html_nodes(css = 'span.team_rgt') %>% html_text())
  score <- c(score, html %>% html_nodes('td') %>% html_nodes(css = 'strong.td_score') %>% html_text()) 
}

schedule <- data.frame(away = away, home = home, score = score)
schedule <- schedule %>% filter(score != 'VS')
schedule <- schedule[1:147,] # 정규시즌만 추출

library(tidyverse)
library(forcats)

schedule$away <- fct_collapse(schedule$away, HW = '한화', SL = '삼성', DB = '두산', LT = '롯데', KW = '키움')
schedule$home <- fct_collapse(schedule$home, HW = '한화', SL = '삼성', DB = '두산', LT = '롯데', KW = '키움')
schedule

write.csv(schedule, 'C:/Users/jaeb0/Desktop/schedule.csv')

