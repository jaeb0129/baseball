library(dplyr)
ex_win<- data.frame(team = c( 'SSG', 'LG',  'KT', 'KW', 'KIA', 'LT', 'NC', 'DB', 'SL', 'HW'),
           runs = c(576, 585, 514, 508, 588, 484, 500, 500, 504, 464),
           run_a = c(477, 437, 445, 511, 533, 578, 510, 536, 584, 611))%>% # 04-02~08-31
  mutate(pythago = runs^1.85/ ((runs)^1.85 + (run_a)^1.85))
ex_win %>% arrange(desc(pythago))         

# write.csv(ex_win, file = 'C:/Users/jaeb0/Desktop/ex_win.csv')

library(readxl)
result <- read_excel('C:/Users/jaeb0/Desktop/ex_win.xlsx', sheet= 2)
result %>% select(c('SSG':'HW')) 
result <- as.matrix(result, )[,2:11]
rownames(result) <- c( 'SSG', 'LG',  'KT', 'KW', 'KIA', 'LT', 'NC', 'DB', 'SL', 'HW')
result

# 몬테카를로 시뮬레이션을 통해 시즌 끝까지 돌려보기(피타고라스 승률로)
library(dplyr)
HOME<-c('KW', 'KT', 'DB', 'SSG', 'KIA',
        'KW', 'KT', 'DB', 'SSG', 'KIA',
        'KIA', 'HW', 'DB', 'SSG',
        'KIA', 'LT', 'HW',
        'KT', 'LT', 'NC', 'LG', 'SL',
        'KT', 'LT', 'NC', 'LG', 'SL',
        'DB', 'SSG', 'KW', 'SL', 'KT',
        'DB', 'SSG', 'KW', 'SL', 'KT',
        'DB', 'KW', 'SL', 'LT', 'HW',
        'DB', 'KW', 'SL', 'LT', 'HW',
        'HW',  'DB', 'LT', 'NC', 'KIA',
        'HW', 'DB', 'LT', 'NC', 'KIA',
        'KIA', 'LG', 'SL', 'NC', 'LT',
        'KIA', 'SL', 'NC', 'LT',
        'LG', 'SL', 'KT', 'KW', 'SSG',
        'LG', 'SL', 'KT', 'KW', 'SSG',
        'SSG', 'KIA', 'HW', 'DB', 'KW',
        'SSG', 'KIA', 'HW', 'DB', 'KW',
        'SSG', 'NC', 'SL', 'LG', 'KW',
        'SSG', 'NC', 'SL', 'LG', 'KW',
        'LG', 'NC', 'KW', 'SSG',
        'DB', 'SL', 'NC', 'SSG',
        'HW', 'KT', 'NC',
        'HW', 'KT', 'NC',
        'LG', 'KIA', 'SL', 'HW', 'SSG',
        'HW', 'LG', 'SL', 'SSG',
        'LG', 'SL', 'KIA',
        'HW', 'LG', 'LT',
        'LT', 'HW',
        'LG', 'KT',
        'KIA', 'NC', 'DB', 'KT',
        'KIA', 'NC', 'DB', 'HW', 
        'KIA', 'NC', 'DB',
        'NC', 'KIA', 'LT', 'SL', 'DB',
        'KT',
        'LG')
AWAY<-c('HW', 'LG', 'LT', 'NC', 'SL',
        'HW', 'LG', 'LT', 'NC', 'SL',
        'KT', 'NC', 'SL', 'KW',
        'KT', 'LG', 'NC',
        'HW', 'KIA', 'DB', 'SSG', 'KW',
        'HW', 'KIA', 'DB', 'SSG', 'KW',
        'HW', 'KIA', 'LG', 'LT', 'NC',
        'HW', 'KIA', 'LG', 'LT', 'NC',
        'KIA', 'KT', 'LG', 'NC', 'SSG',
        'KIA', 'KT', 'LG', 'NC', 'SSG',
        'KT', 'LG', 'SSG', 'SL', 'KW',
        'KT', 'LG', 'SSG', 'SL', 'KW',
        'HW', 'KT', 'DB', 'SSG', 'KW', 
        'HW', 'DB', 'SSG', 'KW',
        'HW', 'KIA', 'LT', 'NC', 'DB',
        'HW', 'KIA', 'LT', 'NC', 'DB',
        'KT', 'LG','LT', 'NC', 'SL',
        'KT', 'LG', 'LT', 'NC', 'SL',
        'HW', 'KIA', 'KT', 'LT', 'DB',
        'HW', 'KIA', 'KT', 'LT', 'DB',
        'HW', 'KIA', 'LT', 'DB',
        'HW', 'KIA', 'KT', 'LG',
        'LG', 'DB', 'KW',
        'LG', 'DB', 'SL',
        'KT', 'LT', 'NC', 'DB', 'KW',
        'LT', 'NC', 'DB', 'KW',
        'NC', 'DB', 'SSG',
        'KIA', 'NC', 'DB', 
        'DB', 'SSG',
        'KIA', 'SL',
        'LG', 'LT', 'SSG', 'SL',
        'LG', 'SSG', 'SL', 'KW',
        'KT', 'LG', 'SL',
        'HW', 'KT', 'LG', 'SSG', 'KW',
        'NC',
        'KT')

length(HOME)
length(AWAY)

sch <- function(x, y){
  Schedule <- data.frame(HOME = x, AWAY = y)
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
