mlb2022 <- read.csv('C:/Users/jaeb0/Desktop/baseball/mlb_pbp/mlb_pbp_2022.csv')
head(mlb2022)

rodon <- mlb2022 %>% filter(pitcher == 607074)
head(rodon) 
unique(rodon$pitch_name)
library(dplyr)
library(lattice)
library(ggplot2)

topKzone <- 3.5
botKzone <- 1.5
inKzone <- -.95
outKzone <- 0.95

L = -0.708333
R = 0.708333
L_p = L+0.1
L_m = L-0.1
R_p = R+0.1
R_m = R-0.1
Center=0

S_height = 0 
M_height = -0.6
E_height = -1.0

k_zone_plot <- ggplot(NULL, aes(x = plate_x, y = plate_z)) +
  geom_rect(xmin = -0.947, xmax = 0.947, ymin = 1.5,
            ymax = 3.6, col = "black", fill = "lightgray", alpha = 0.01, size = 2) +
  geom_segment(x=L_p, y=S_height, xend=R_m, yend=S_height, color="black", size=2)+
  geom_segment(x=L_p, y=S_height, xend=L_m, yend=M_height, color="black", size=2)+
  geom_segment(x=L_m, y=M_height, xend=Center, yend=E_height, color="black", size=2)+
  geom_segment(x=Center, y=E_height, xend=R_p, yend=M_height, color="black", size=2)+
  geom_segment(x=R_p, y=M_height, xend=R_m, yend=S_height, color="black", size=2)+
  coord_equal() +
  scale_x_continuous("수평 로케이션(ft.)", limits = c(-2, 2)) +
  scale_y_continuous("수직 로케이션(ft.)", limits = c(-1, 4.5))
}

k_zone_plotr %+% rodon + 
  geom_point(alpha = 0.5, color = 'red') + 
  ggtitle("2022시즌 카를로스 로돈 구종, 좌우 스플릿별 로케이션") + 
  theme(plot.title = element_text(hjust = 0.5))


ggplot(rodon, aes(plate_x, plate_z)) +
  geom_density_2d_filled(contour_var = "ndensity") +
  geom_rect(xmin = -0.947, xmax = 0.947, ymin = 1.5,
            ymax = 3.6, col = "red", fill = "lightgray", alpha = 0.01, size = 2) +
  geom_segment(x=L_p, y=S_height, xend=R_m, yend=S_height, color="black", size=2)+
  geom_segment(x=L_p, y=S_height, xend=L_m, yend=M_height, color="black", size=2)+
  geom_segment(x=L_m, y=M_height, xend=Center, yend=E_height, color="black", size=2)+
  geom_segment(x=Center, y=E_height, xend=R_p, yend=M_height, color="black", size=2)+
  geom_segment(x=R_p, y=M_height, xend=R_m, yend=S_height, color="black", size=2)+
  coord_equal() +
  scale_x_continuous("수평 로케이션(ft.)", limits = c(-2, 2)) +
  scale_y_continuous("수직 로케이션(ft.)", limits = c(-1, 4.5))+ 
  theme(legend.position = "none") +
  facet_grid(stand ~ pitch_name, labeller = labeller(.rows = c('L' = '좌', 'R' = '우'), .cols = c('4-Seam Fastball' = '포심', 'Changeup' = '체인지업', 'Curveball'= '커브', 'Slider'= '슬라이더'))) +
  theme(text=element_text(size=18)) +
  ggtitle('로돈의 구종, 좌우 스플릿별 투구 분포도') +
  theme(plot.title = element_text(colour = "red",
                                  size = 18,
                                  hjust = 0.5, vjust = 0.8, angle = 0))



############# 무브먼트

ggplot(rodon, aes(x=pfx_x, y = pfx_z, color = factor(pitch_name))) + 
  theme_classic()+
  xlab('수평 무브먼트(.ft)')+
  ylab('수직 무브먼트(.ft)') +
  scale_color_discrete(name="구종", labels = c('포심', '체인지업', '커브', '슬라이더')) +
  theme(legend.title=element_text(size=20, face="bold",color="red"),
        plot.title=element_text(size=20, color="blue")) +
  ggtitle("로돈 구종별 무브먼트") +
  geom_point()

# 구종
#install.packages('tidyr')
#install.packages('tidyverse')
#install.packages('webr')
library(ggplot2)
library(webr)
library(tidyr)
rodon <- rodon %>% mutate(type = case_when(pitch_name == '4-Seam Fastball' ~ 'fastball',
                                              pitch_name %in% c('Curveball', 'Slider') ~ 'breaking ball',
                                              pitch_name == 'Changeup' ~ 'offspead'))
data <- rodon %>% group_by(type, pitch_name) %>% summarise(n = n())
PieDonut(data, aes(type, pitch_name, count=n), title = "Carlos Rodon's pitch type")

############ 평균 구속
mean <- rodon %>% group_by(pitch_name) %>% summarise(mean = mean(release_speed)) 

############ 최대 구속
max <- rodon %>% group_by(pitch_name) %>% summarise(max = max(release_speed)) 
max


ggplot(max, aes(reorder(pitch_name,max), max, fill=pitch_name))+
  geom_bar(stat='identity', width = 0.5) +
  scale_fill_discrete(name="구종", labels = c('포심', '체인지업', '커브', '슬라이더'))+
  xlab('구종')+
  ylab('구속(mph)') + 
  coord_flip()+
  theme_classic() +
  geom_text(aes(label=max), vjust=0, hjust=-0.2) +
  scale_y_continuous(labels = c('포심', '슬라이더', '커브', '체인지업'))
  
  
  

