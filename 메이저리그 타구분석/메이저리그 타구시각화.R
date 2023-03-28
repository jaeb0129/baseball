# install.packages("devtools")
library(devtools)
# devtools::install_github("bdilday/GeomMLBStadiums")

library(GeomMLBStadiums)
library(ggplot2)
library(dplyr)

# 양키 스타디움
ggplot() +
  geom_mlb_stadium(stadium_ids = "yankees", stadium_segments = "all") +
  coord_fixed() +
  theme_void()

# 모든 경기장
ggplot() + 
  geom_mlb_stadium(stadium_ids = "all_mlb", stadium_segments = "all") + 
  facet_wrap(~team) + 
  coord_fixed() +
  theme_void()

# 팬웨이 파크를 보면 그린몬스터가 우측에 있음.
# transform 시 좌우 반전 및 홈플래아트 하단으로 이동
ggplot() + 
  geom_mlb_stadium(stadium_ids = "all_mlb", 
                   stadium_segments = "all", 
                   stadium_transform_coords = TRUE) + 
  facet_wrap(~team) + 
  coord_fixed() +
  theme_void()

mlb_pbp_2022 <- read.csv("C:/Users/jaeb0/Desktop/baseball/mlb_pbp_2022.csv")
ha_seong <- mlb_pbp_2022 %>% filter(batter == 673490 & description == 'hit_into_play')
head(ha_seong %>% select(game_date, events, hc_x, hc_y, 
                           balls, strikes, outs_when_up, inning))

ha_seong <- mlbam_xy_transformation(ha_seong)
head(ha_seong  %>% select(hc_x, hc_y, hc_x_, hc_y_))

ggplot(ha_seong, aes(x = hc_x_, y = hc_y_)) +
  geom_spraychart(stadium_transform_coords = TRUE, stadium_segments = "all") +
  coord_fixed() +
  theme_void()
  
  # 김하성이 어떤 유형의 타구를 쳤는지
ha_seong$bb_type <- factor(ha_seong$bb_type, 
                               levels = c("ground_ball", "line_drive", "fly_ball", "popup"))
  
ggplot(ha_seong, aes(x = hc_x_, y = hc_y_, color = bb_type)) +
    geom_spraychart(stadium_transform_coords = TRUE, stadium_segments = "all") +
    coord_fixed() +
    theme_void()

# 김하성의 타구 결과에 따라 색상을 변경
ha_seong$events <- factor(ha_seong$events, 
                            levels = c("single", "double", "triple", "home_run", "sac_fly", "field_error", "field_out", "force_out", "fielders_choice_out", "grounded_into_double_play", "double_play"))

ggplot(ha_seong, aes(x = hc_x_, y = hc_y_, color = events)) +
  geom_spraychart(stadium_transform_coords = TRUE, stadium_segments = "all") +
  scale_color_brewer(palette = "Set3") +
  coord_fixed() +
  theme_void()

ggplot(ha_seong, aes(x = hc_x_, y = hc_y_, color = events)) +
  geom_spraychart(stadium_ids = "padres", stadium_transform_coords = TRUE, stadium_segments = "all") +
  scale_color_brewer(palette = "Set3") +
  coord_fixed() +
  theme_void()


# 팀 전체, 보기에 너무 복잡함
padres_bot <- mlb_pbp_2022 %>% filter(home_team == 'SD' & inning_topbot == 'Bot')
padres_top <- mlb_pbp_2022 %>% filter(away_team == 'SD' & inning_topbot == 'Top')
padres <- rbind(padres_bot,padres_top)
padres <- padres %>% filter(hc_x != "null")
padres$hc_x <- as.numeric(padres$hc_x)
padres$hc_y <- as.numeric(padres$hc_y)
padres <- mlbam_xy_transformation(padres)

ggplot(padres, aes(x = hc_x_, y = hc_y_)) +
  geom_spraychart(stadium_ids = "padres", stadium_transform_coords = TRUE, stadium_segments = "all") +
  coord_fixed() +
  theme_void()

# 이 결과 시각화는 원래 스프레이 차트에 비해 읽기가 훨씬 쉽습니다. 
# 이는 에서 geom_hex각 육각형 내의 데이터 포인트 양을 요약하여 스프레이 차트의 혼란을 방지하기 때문
ggplot(padres, aes(x = hc_x_, y = hc_y_)) +
  geom_hex() +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  geom_mlb_stadium(stadium_ids = "padres", stadium_transform_coords = TRUE, stadium_segments = "all") +
  coord_fixed() +
  theme_void()

# 색상을 사용하여 해당 위치에서 적중된 공의 발사 각도를 나타낼 수도 있습니다
padres$launch_angle <- as.numeric(padres$launch_angle)

ggplot(padres, aes(x = hc_x_, y = hc_y_, z = launch_angle)) +
  stat_summary_hex() +
  scale_fill_distiller(palette = "YlOrRd", direction = 1) +
  geom_mlb_stadium(stadium_ids = "padres", stadium_transform_coords = TRUE, stadium_segments = "all") +
  coord_fixed() +
  theme_void()