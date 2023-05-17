library(dplyr)
ex_win_log5<- data.frame(team = c( 'SSG', 'LG',  'KT', 'KW', 'KIA', 'LT', 'NC', 'DB', 'SL', 'HW'),
           runs = c(576, 585, 514, 508, 588, 484, 500, 500, 504, 464),
           run_a = c(477, 437, 445, 511, 533, 578, 510, 536, 584, 611))%>% # 04-02~08-31
  mutate(pythago = runs^1.85/ ((runs)^1.85 + (run_a)^1.85))
ex_win_log5 %>% arrange(desc(pythago))         

# write.csv(ex_win_log5, file = 'C:/Users/jaeb0/Desktop/ex_win_log5.csv')

library(readxl)
result <- read_excel('C:/Users/jaeb0/Desktop/ex_win_log5.xlsx', sheet= 2)
result %>% select(c('SSG':'HW')) 
result <- as.matrix(result, )[,2:11]
rownames(result) <- c( 'SSG', 'LG',  'KT', 'KW', 'KIA', 'LT', 'NC', 'DB', 'SL', 'HW')
result

library(rvest)
library(tidyverse)
library(forcats)

# Naver 프로야구 일정표 2022/09/01 ~ 2022/10/11 잔여 정규경기만 추출

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

schedule$away <- fct_collapse(schedule$away, HW = '한화', SL = '삼성', DB = '두산', LT = '롯데', KW = '키움')
schedule$home <- fct_collapse(schedule$home, HW = '한화', SL = '삼성', DB = '두산', LT = '롯데', KW = '키움')
schedule

write.csv(schedule, 'C:/Users/jaeb0/Desktop/schedule.csv')
schedule <- read.csv('C:/Users/jaeb0/Desktop/schedule.csv')
schedule
nrow(schedule) # 147경기

sch <- function(x, y){
  Schedule <- data.frame(HOME = schedule$home, AWAY = schedule$away)
  Schedule %>% rowwise() %>%
    mutate(outcome1 = result[HOME, AWAY],
           outcome2 = result[AWAY, HOME])->Schedule
  win_ls=c()
  for (i in 1:nrow(Schedule)){
    win = rbinom(1, 1, as.numeric(Schedule$outcome1[i]))
    win_ls = append(win_ls, win)
  }
  Schedule$outcome = win_ls
  Schedule$winner = ifelse(Schedule$outcome == 1, HOME, AWAY)
  return(Schedule)
}

sch(HOME, AWAY)

SSG = c()
LG = c()
KT = c()
KW = c()
KIA = c()
LT = c()
NC = c()
DB = c()
SL = c()
HW = c()
prev <- data.frame(team = c('SSG', 'LG', 'KT', 'KW', 'KIA', 'LT', 'NC', 'DB', 'SL', 'HW'),
                   기존_승수 = c(76, 68, 63, 64, 56, 52, 48, 47, 47, 35),
                   무승부 = c(3, 1, 2, 2, 1, 4, 3, 2, 2, 2))

monte<- function(x){
  for (i in 1:x){
    sch(HOME, AWAY) %>% group_by(winner) %>% summarise(n=n()) %>% arrange(desc(n))->winner
    winner %>% inner_join(prev, by = c('winner' = 'team'))->winner
    
    winner %>% mutate(예상_승률 = (기존_승수+n)/(144-무승부)) %>% arrange(desc(예상_승률)) %>%
      mutate(rank = rank(-예상_승률)) ->winner
    SSG<- append(SSG, subset(winner, winner == 'SSG')[,6][[1]])
    LG <- append(LG, subset(winner, winner == 'LG')[,6][[1]])
    KT <- append(KT, subset(winner, winner == 'KT')[,6][[1]])
    KW <- append(KW, subset(winner, winner == 'KW')[,6][[1]])
    KIA <- append(KIA, subset(winner, winner == 'KIA')[,6][[1]])
    LT <- append(LT, subset(winner, winner == 'LT')[,6][[1]])
    NC <- append(NC, subset(winner, winner == 'NC')[,6][[1]])
    DB <- append(DB, subset(winner, winner == 'DB')[,6][[1]])
    SL <- append(SL, subset(winner, winner == 'SL')[,6][[1]])
    HW <- append(HW, subset(winner, winner == 'HW')[,6][[1]])
    df <- data.frame(SSG=SSG, LG=LG, KT=KT, KW=KW, KIA=KIA,
                     LT=LT, NC=NC, DB=DB, SL=SL, HW=HW)
    print(df)
  }
  write.csv(df, file = 'C:/Users/jaeb0/Desktop/df1.csv')
}
monte(10000)

result <- read.csv('C:/Users/jaeb0/Desktop/df1.csv')
table(result$SSG)
a<-sapply(result, table)
prop.table(a[[4]])
