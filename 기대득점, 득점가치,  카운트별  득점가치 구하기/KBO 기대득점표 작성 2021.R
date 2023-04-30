data2021 <- read.csv("C:/Users/jaeb0/2021.csv")
str(data2021)

# install.packages("stringr")

library(stringr)
library(dplyr)
data2021 <-subset(data2021, pa_result != "")
data2021$RUNS <- with(data2021, score_away + score_home)
data2021$HALF.INNING <- with(data2021, paste(gameID, inning, inning_topbot))

data2021$RUNS.SCORED <- with(data2021, str_count(description, '홈인') + str_count(description, '홈런거리'))
sum(data2021$RUNS.SCORED)

# 각 이닝별(초, 말 구분) 득점합
RUNS.SCORED.INNING <- aggregate(data2021$RUNS.SCORED,
                                list(HALF.INNING=data2021$HALF.INNING), sum)
# 각 이닝별(초, 말 구분) 시작전 양팀 점수합
RUNS.SCORED.START <- aggregate(data2021$RUNS,
                               list(HALF.INNING=data2021$HALF.INNING), "[", 1)
RUNS.SCORED.START

# 이닝 identification 가져오기 
MAX <- data.frame(HALF.INNING=RUNS.SCORED.START$HALF.INNING)
# 각 이닝별 득점합+시작전 점수합
MAX$x <-RUNS.SCORED.INNING$x + RUNS.SCORED.START$x
data2021 <-merge(data2021, MAX)
N <-ncol(data2021)
names(data2021)[N] <- "MAX.RUNS"

MAX

data2021$RUNS.ROI <- with(data2021, MAX.RUNS -RUNS)
data2021$RUNS.ROI

# getwd()
# write.csv(data2011, "data2011.csv")

# Creating the Matrix
RUNNER1 <-ifelse(as.character(data2021[,"on_1b"]) == "", 0, 1)
RUNNER2 <-ifelse(as.character(data2021[,"on_2b"]) == "", 0, 1)
RUNNER3 <-ifelse(as.character(data2021[,"on_3b"]) == "", 0, 1)

get.state <- function(runner1, runner2, runner3, outs){
  runners <- paste(runner1, runner2, runner3, sep="")
  paste(runners, outs)
}
data2021$STATE <- get.state(RUNNER1, RUNNER2, RUNNER3, data2021$outs)

# STATE 기준(24가지 상황)으로 RUNS.ROI(이닝 종료시까지 발생 득점)을 평균
RUNS <-with(data2021, aggregate(RUNS.ROI, list(STATE), mean))
RUNS

RUNS$Outs <-substr(RUNS$Group, 5, 5)
RUNS <- RUNS[order(RUNS$Outs), ]

RUNS.out <-matrix(round(RUNS$x, 2), 8, 3)
dimnames(RUNS.out)[[2]] <- c("0 outs", "1 outs", "2 outs") #열
dimnames(RUNS.out)[[1]] <- c("000", "001", "010", "011", "100", "101", "110", "111") #행


# 득점가치
RE <- vector()
for (i in 1:nrow(data2021)){
  a <- RUNS.out[substr(data2021$STATE, 1, 3)[i], paste(substr(data2021$STATE, 5, 5), 'outs')[i]]
  RE <- append(RE, a)
}
data2021$RE <- RE

n <- nrow(data2021 %>% filter(pa_result == '2루타')) # 2루타 2181개
sum <- data2021 %>% filter(pa_result == '2루타') %>% summarise(run = sum(RUNS.ROI)) # 2루타 이후 3020점
# 2루타 기대득점 3020/2181
re1<- sum / n # 1.385점
re1 <- unlist(re1, use.name=F)
re1
r\# 2루타 나온 시점 기대득점
re2 <- data2021 %>% filter(pa_result == '2루타') %>% summarise(avg = mean(RE))  # 0.555
re2 <- unlist(re2, use.name=F)
re2
# 2루타 득점가치 1.385-0.555 = 0.83
1.385-0.555
re1-re2


rv <- function(event){
  n <- nrow(data2021 %>% filter(pa_result %in% event))
  sum <- data2021 %>% filter(pa_result %in% event) %>% summarise(run = sum(RUNS.ROI))
  re1 <- sum/n
  re1 <- unlist(re1, use.name=F)
  re2 <- data2021 %>% filter(pa_result %in% event) %>% summarise(avg = mean(RE)) %>%
  unlist(., use.name=F)
  re <- re1 - re2
  print(re)
}
unique(data2021$pa_result)
rv(c('안타', '내야안타', '번트 안타'))
rv('2루타')
rv('3루타')
rv('홈런')
rv('볼넷')
rv('몸에 맞는 볼')
rv('자동 고의4구')
rv('삼진')
rv(c('필드 아웃', '포스 아웃', '타구맞음 아웃'))


# 볼카운트별 득점가치
data2021a <- read.csv("C:/Users/jaeb0/2021.csv")
data2021a$STATE <- get.state(RUNNER1, RUNNER2, RUNNER3, data2021a$outs)
data2021a$index <- with(data2021a, paste(gameID, pa_number))
data2021a$count <- with(data2021a, paste(balls, strikes))


data2021a <- data2021a %>% mutate(rv = case_when(pa_result %in% c('안타', '내야안타', '번트 안타') ~ rv(c('안타', '내야안타', '번트 안타')),
pa_result == '2루타'~ rv('2루타'),
pa_result == '3루타'~ rv('3루타'),
pa_result == '볼넷'~ rv('볼넷'),
pa_result == '몸에 맞는 볼'~ rv('몸에 맞는 볼'),
pa_result == '자동 고의4구'~ rv('자동 고의4구'),
pa_result == '삼진'~ rv('삼진'),
pa_result %in% c('필드 아웃', '포스 아웃', '타구맞음 아웃') ~ rv(c('필드 아웃', '포스 아웃', '타구맞음 아웃')) ))

#### 초구 승부의 득점가치 구하기
# 0-0 -> 0-1 득점가치
run_effect_1st_st <- data2021a %>% group_by(index) %>% select(count, pa_result, rv) %>%
  filter(count == '0 1' | pa_result != "") %>% .[which(duplicated(.$index)),] %>%
  ungroup() %>% summarise(sum = sum(rv, na.rm=T)) %>% unlist(., use.name=F) # 총 -1614점, 22,004회

RV_of_1st_pitch_st <- run_effect_1st_st / nrow(data2021a %>% group_by(index) %>% select(count, pa_result, rv) %>%
                                                 filter(count == '0 1' | pa_result != "") %>% .[which(duplicated(.$index)),])
RV_of_1st_pitch_st # 초구S는 타자 입장에서 팀득점을 -0.073점 감소시킨다. 

# 0-0 -> 1-0 득점가치
run_effect_1st_ball <- data2021a %>% group_by(index) %>% select(count, pa_result, rv) %>%
  filter(count == '1 0' | pa_result != "") %>% .[which(duplicated(.$index)),] %>%
  ungroup() %>% summarise(sum = sum(rv, na.rm=T)) %>% unlist(., use.name=F) # 총 540점, 20,158회

RV_of_1st_pitch_ball <- run_effect_1st_ball / nrow(data2021a %>% group_by(index) %>% select(count, pa_result, rv) %>%
                                                     filter(count == '1 0' | pa_result != "") %>% .[which(duplicated(.$index)),])
RV_of_1st_pitch_ball #초구B을 고르면 타자 입장에서 팀득점을 0.027점 증가시킨다. 

# 0-0 타격시 득점가치
run_effect_1st_hitting <- data2021a %>% group_by(index) %>% select(count, pa_result, rv) %>% 
  filter(count == '0 0' & pa_result != "") %>%
  ungroup() %>% summarise(sum = sum(rv, na.rm=T)) %>% unlist(., use.name=F) # 총 118.6746점, 6,124회

RV_of_1st_pitch_hitting <- run_effect_1st_hitting / nrow(data2021a %>% group_by(index) %>% select(count, pa_result, rv) %>% 
                                                           filter(count == '0 0' & pa_result != ""))
RV_of_1st_pitch_hitting # 초구타격은 0.019점 증가시킨다
