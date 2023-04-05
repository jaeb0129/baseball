mlb2021 <- read.csv('C:/Users/jaeb0/Desktop/baseball/mlb_pbp_2021.csv')

id = c('622491',
       '572971',
       '621121',
       '608566',
       '573186',
       
       '645261',
       '608331',
       '489119',
       '554430',
       '502043',
       
       '450203',
       '669203',
       '425794',
       '571578',
       '547943',
       
       '425844',
       '605397',
       '605540',
       '543294',
       '621111',
       
       '621244',
       '593423',
       '543101',
       '543135',
       '640455',
       
       '623167',
       '641816',
       '592332',
       '543037',
       '605400',
       
       '608344',
       '628711',
       '543475',
       '592662',
       '506433',
       
       '542881',
       '608337',
       '656302',
       '453286'
)

gbs = c(56.6,
        54.9,
        56.4,
        51.6,
        50.8,
        53.3,
        51.8,
        49.4,
        49.8,
        51.7,
        47.8,
        48.8,
        47.5,
        47.2,
        46.8,
        44.4,
        43.5,
        41.5,
        43.1,
        44.7,
        42.8,
        42.8,
        44.3,
        42.0,
        42.0,
        42.4,
        42.2,
        41.9,
        43.1,
        40.5,
        37.6,
        40.2,
        37.6,
        37.2,
        37.0,
        35.1,
        33.2,
        33.3,
        33.5)

fbs = c(24.6,
        25.2,
        27.0,
        26.3,
        26.4,
        27.7,
        27.7,
        27.1,
        27.5,
        29.2,
        28.9,
        30.3,
        30.3,
        30.9,
        33.1,
        32.2,
        33.5,
        32.3,
        33.8,
        35.5,
        34.0,
        35.2,
        36.4,
        34.7,
        35.8,
        36.6,
        36.6,
        36.5,
        40.5,
        40.5,
        37.9,
        41.2,
        39.8,
        44.2,
        45.3,
        43.1,
        42.3,
        44.3,
        48.3)
df = data.frame()

pitcher <- function(names){
  require(dplyr)
  for (i in 1:length(names)){
    data <- subset(mlb2021, pitcher == names[i] & events == 'field_out')
    goao <- data %>% filter(bb_type == 'fly_ball' | bb_type == 'ground_ball' | bb_type == 'line_drive')
    name <- names[i]
    go <- nrow(subset(goao, bb_type == 'ground_ball'))
    ao <- nrow(subset(goao, bb_type == 'fly_ball' | bb_type == 'line_drive'))
    go_ao <- round((go / ao),1)
    fb <- fbs[i]
    gb <- gbs[i]
    df <- rbind(df, c(name, go, ao, go_ao, fb, gb))
    names(df) = c('ID', '땅볼아웃', '뜬공아웃', 'GO/AO', 'FB%', 'GB%')
  }
  return(df)
}

a <- pitcher(id)
a$'GO/AO' <- as.numeric(a$'GO/AO')

plot( log(exp((a$'GO/AO'))), a$'FB%')
summary(lm(a$'FB%' ~ log(exp(a$'GO/AO'))))
# y= 0.56 -0.19ln(x)


plot(log(exp((a$'GO/AO'))), a$'GB%')
summary(lm(a$'GB%' ~ log(exp(a$'GO/AO'))))
# y= 0.22 +0.19ln(x)

names = c('안우진', '스탁', '수아레즈', '데스파이네', '루친스키',
          '폰트', '박세웅', '이영하', '켈리', '플럿코',
          '이의리', '뷰캐넌', '애플러', '김광현', '원태인',
          '배제성', '반즈', '오원석', '소형준', '양현종',
          '김민우', '이태양', '최원준', '고영표', '요키시',
          '이인복')
goao = c(0.91, 1.59, 1.4, 1.72, 1.14, 
         0.82, 2.41, 1.08, 1.26, 0.48,
         0.8, 1.94, 1.77, 1.13, 0.88,
         0.95, 1.22, 0.94, 1.62, 1.01,
         1, 0.96, 0.55, 2.06, 2.12,
         1.96)

df1 = data.frame()
starter <- function(names){
  require(dplyr)
  for (i in 1:length(names)){
    name <- names[i]
    go_ao <- goao[i]
    gb <- round(0.22 + 0.19*log(exp(goao[i])), 3)*100
    fb <- round(0.56 - 0.19*log(exp(goao[i])), 3)*100
    df1 <- rbind(df1, c(name, go_ao, gb, fb))
    names(df1) = c('이름', 'GO/AO','기대 땅볼%', '기대 뜬공%')
  }
  return(df1)
}

b<- starter(names)
c <- b[c(order(b$'기대 뜬공%', decreasing=T)),]
c
rownames(c) <- NULL
getwd()
write.csv(c, file='기대FB%.csv', row.names=TRUE)
0.56-0.19*log(exp(0.8)) # 이의리 0.408 40.8% 기대 뜬공




