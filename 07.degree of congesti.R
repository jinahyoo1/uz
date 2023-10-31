#세부과제2: 2022년 12월 31일 수도권 지하철의 혼잡도를 어떻게 분석할 수 있는가?
library(dplyr)
library(ggplot2)

#csv형식의 파일 불러와서 subway객체에 입력하고 구조 확인
str(congestion)

#변수의 이상치와 결측치 확인하고 처리
summary(congestion)

#결측치 개수 확인
is.na(congestion)
sum(is.na(congestion))
colSums(is.na(congestion))

#우선 호선별로 첫차/막차의 시간대가 다르기때문에 가장 빠른 기차와 늦은 기차의 결측치가 있는 행제거
#결측치가 있는 행을 제거한 새로운 데이터 프레임 생성
#6시 출발기차의 결측치 제거
congestion1 <- congestion[!is.na(congestion$s0600),]
colSums(is.na(congestion1))
#23시30분 출발기차의 결측치 제거
congestion1 <- congestion[!is.na(congestion$s0530),]
colSums(is.na(congestion1))
#남은 결측치를 0으로 대체
congestion1[is.na(congestion1)]<-0
colSums(is.na(congestion1))

#이상치 확인
ggplot(congestion1,aes(y=s0530))+geom_boxplot() #이용목적에 따라 해당수치가 나올수 있어서 이상치를 별도처리 안함 
summary(congestion1$s0530)

#수도권 지하철 혼잡도 분석 
#1.지하철역의 하루 평균 혼잡도(수도권 지하철 하루평균혼잡도)
congestion1$day_mean <-
  rowMeans(congestion1[,c('s0530','s0600','s0630','s0700','s0730','s0800','s0830','s0900','s0930','s1000','s1030','s1100','s1130','s1200','s1230','s1300','s1330','s1400','s1430','s1500','s1530','s1600','s1630','s1700','s1730','s1800','s1830','s1900','s1930','s2000','s2030','s2100','s2130','s2200','s2230','s2300','s2330')])

#2. 지하철 호선별 하루 평균 혼잡도
# 데이터를 이용해서 각 호선의 하루 평균 혼잡도를 계산합니다.
avg_congestion_by_line <- congestion1 %>%
  group_by(line) %>%
  summarise(avg_day_congestion = mean(day_mean))

print(avg_congestion_by_line)

#3. 지하철 호선별 출근시간(07:00~09:00)대의 평균혼잡도 
# 출근시간대(07:00~09:00)의 열을 선택
commute_data <- congestion1 %>%
  select(line, station, s0700, s0730, s0800, s0830,s0900)

# 각 호선별로 출근시간대의 평균 혼잡도
commute_avg <- commute_data %>%
  group_by(line) %>%
  summarise(
    avg_s0700 = mean(s0700),
    avg_s0730 = mean(s0730),
    avg_s0800 = mean(s0800),
    avg_s0830 = mean(s0830),
    avg_s0900 = mean(s0900)
  )

print("출근시간대(07:00~09:00)의 평균 혼잡도")
print(commute_avg)

#3-1기술통계분석
summary_stats <- congestion1 %>%
  group_by(line) %>%
  summarise(
    mean_s0700 = mean(s0700),
    mean_s0730 = mean(s0730),
    mean_s0800 = mean(s0800),
    mean_s0830 = mean(s0830),
    mean_s0900 = mean(s0900),
    median_s0700 = median(s0700),
    median_s0730 = median(s0730),
    median_s0800 = median(s0800),
    median_s0830 = median(s0830),
    median_s0900 = median(s0900),
    max_s0700 = max(s0700),
    max_s0730 = max(s0730),
    max_s0800 = max(s0800),
    max_s0830 = max(s0830),
    max_s0900 = max(s0900),
    min_s0700 = min(s0700),
    min_s0730 = min(s0730),
    min_s0800 = min(s0800),
    min_s0830 = min(s0830),
    min_s0900 = min(s0900),
    sd_s0700 = sd(s0700),
    sd_s0730 = sd(s0730),
    sd_s0800 = sd(s0800),
    sd_s0830 = sd(s0830),
    sd_s0900 = sd(s0900)
  )

# 결과를 출력합니다.
print(summary_stats)

#3-2평균혼잡도가 가장 높은 시간대를 막대그래프로 그리기 

# 각 호선별로 평균 혼잡도가 가장 높은 시간대 찾기 
max_congestion <- congestion1 %>%
  group_by(line) %>%
  summarise(
    max_time = ifelse(max(s0700) == max(max(s0700), max(s0730), max(s0800), max(s0830)), "s0700",ifelse(max(s0730) == max(max(s0700), max(s0730), max(s0800), max(s0830)), "s0730",ifelse(max(s0800) == max(max(s0700), max(s0730), max(s0800), max(s0830)), "s0800", "s0830"))) )
print(max_congestion)

# 결과를 막대 그래프로 그리기
# ggplot2 패키지를 로드합니다.
library(ggplot2)

# max_congestion 데이터 프레임을 이용하여 막대 그래프를 그립니다.
ggplot(max_congestion, aes(x = as.factor(line), fill = max_time)) +
  geom_bar(stat = "count", position = "dodge") +
  labs(title = "각 호선별로 평균 혼잡도가 가장 높은 시간대", x = "호선", y = "혼잡도") +
  scale_fill_discrete(name = "최대 시간대")
# ggplot2 패키지를 로드합니다.
library(ggplot2)

#3-3평균혼잡도 상위 4개 호선도의 역별 기여도
mean_day_congestion <- congestion1 %>%
  group_by(line) %>%
  summarise(
    mean_congestion = mean(s0700 + s0730 + s0800 + s0830 +s0900)
  ) %>%
  arrange(desc(mean_congestion))

#평균 혼잡도가 가장 높은 4개 호선을 선택합니다.
top_4_lines <- mean_day_congestion %>% top_n(4, mean_congestion)
print(top_4_lines)

#선택된 4개 호선에서 역별로 혼잡도의 기여도를 계산합니다.
contribution_by_station <- congestion1 %>%
  filter(line %in% top_4_lines$line) %>%
  select(station, line, s0700, s0730, s0800, s0830,s0900) %>%
  group_by(station,line) %>%
  summarise(
    total_congestion = sum(s0700 + s0730 + s0800 + s0830 +s0900),
    contribution_percentage = total_congestion / sum(total_congestion) * 100
  )

# 결과를 출력합니다.
print(contribution_by_station)

#4.출발시간 8시의 지하철 혼잡도 범주화/범주별 빈도분석 
congestion1 %>% 
  mutate(s80_grade=ifelse(s0800<=80, "good", ifelse(s0800<=130, "normal", ifelse(s0800<=150, "caution", 
                                                                                 "bad"))))%>% 
  group_by(s80_grade) %>% 
  summarise(n=n())%>% 
  mutate(total=sum(n), pct=round(n/total*100,1))%>% 
  select(s80_grade,n,pct)%>% 
  arrange(desc(n))
#4-1. 호선별로 08시 지하철 혼잡도 범주화
congestion1 %>% 
  mutate(s80_grade=ifelse(s0800<=80, "good", ifelse(s0800<=130, "normal", ifelse(s0800<=150, "caution", 
                                                                                 "bad"))))%>% 
  group_by(line, s80_grade) %>% 
  summarise(n=n())%>% 
  mutate(total=sum(n), pct=round(n/total*100,1))%>% 
  filter(s80_grade=="caution")%>% 
  select(line, s80_grade,n,pct)%>% 
  arrange(desc(pct))%>% 
  head(5)

#5.지하철 호선별 퇴근시간(18:00~20:00)대의 평균혼잡도 
# 퇴근시간대(18:00~20:00)의 열을 선택
left_data <- congestion1 %>%
  select(line, station, s1800, s1830, s1900, s1930,s2000)

# 각 호선별로 출근시간대의 평균 혼잡도
left_avg <- left_data %>%
  group_by(line) %>%
  summarise(
    avg_s1800 = mean(s1800),
    avg_s1830 = mean(s1830),
    avg_s1900 = mean(s1900),
    avg_s1930 = mean(s1930),
    avg_s2000 = mean(s2000)
  )

print("퇴근시간대(18:00~20:00)의 평균 혼잡도")
print(left_avg)

#5-1기술통계분석
# 기술통계분석 결과 
summary_stats_left <- congestion1 %>%
  group_by(line) %>%
  summarise(
    mean_s1800 = mean(s1800),
    mean_s1830 = mean(s1830),
    mean_s1900 = mean(s1900),
    mean_s1930 = mean(s1930),
    mean_s2000 = mean(s2000),
    median_s1800 = median(s1800),
    median_s1830 = median(s1830),
    median_s1900 = median(s1900),
    median_s1930 = median(s1930),
    median_s2000 = median(s2000),
    max_s1800 = max(s1800),
    max_s1830 = max(s1830),
    max_s1900 = max(s1900),
    max_s1930 = max(s1930),
    max_s2000 = max(s2000),
    min_s1800 = min(s1800),
    min_s1830 = min(s1830),
    min_s1900 = min(s1900),
    min_s1930 = min(s1930),
    min_s2000 = min(s2000),
    sd_s1800 = sd(s1800),
    sd_s1830 = sd(s1830),
    sd_s1900 = sd(s1900),
    sd_s1930 = sd(s1930),
    sd_s2000 = sd(s2000)
  )

# 결과를 출력합니다.
print(summary_stats_left)

#5-2평균혼잡도가 가장 높은 시간대를 막대그래프로 그리기 

# 각 호선별로 평균 혼잡도가 가장 높은 시간대 찾기 
max_congestion_left <- congestion1 %>%
  group_by(line) %>%
  summarise(
    max_time = ifelse(max(s1800) == max(max(s1830), max(s1900), max(s1930), max(s2000)), "s1800",ifelse(max(s1830) == max(max(s1800), max(s1830), max(s1900), max(s1930)), "s1830",ifelse(max(s1800) == max(max(s1800), max(s1830), max(s1900), max(s1930)), "s1900", "s1930"))) )
print(max_congestion_left)

# 결과를 막대 그래프로 그리기
# ggplot2 패키지를 로드합니다.
library(ggplot2)

# max_congestion 데이터 프레임을 이용하여 막대 그래프를 그립니다.
ggplot(max_congestion_left, aes(x = as.factor(line), fill = max_time)) +
  geom_bar(stat = "count", position = "dodge") +
  labs(title = "각 호선별로 평균 혼잡도가 가장 높은 시간대", x = "호선", y = "혼잡도") +
  scale_fill_discrete(name = "최대 시간대")


#5-3평균혼잡도 상위 4개 호선도의 역별 기여도
mean_day_congestion_left <- congestion1 %>%
  group_by(line) %>%
  summarise(
    mean_congestion_left = mean(s1800 + s1830 + s1900 + s1930 +s2000)
  ) %>%
  arrange(desc(mean_congestion_left))

#평균 혼잡도가 가장 높은 4개 호선을 선택합니다.
top_4_lines <- mean_day_congestion_left %>% top_n(4, mean_congestion_left)
print(top_4_lines)

#선택된 4개 호선에서 역별로 혼잡도의 기여도를 계산합니다.
contribution_by_station_left <- congestion1 %>%
  filter(line %in% top_4_lines$line) %>%
  select(station, line, s1800, s1830, s1900, s1930,s2000) %>%
  group_by(station,line) %>%
  summarise(
    total_congestion = sum(s1800 + s1830 + s1900 + s1930 +s2000),
    contribution_percentage = total_congestion / sum(total_congestion) * 100
  )

# 결과를 출력합니다.
print(contribution_by_station_left)

#6.출발시간 18시의 지하철 혼잡도 범주화/범주별 빈도분석 
congestion1 %>% 
  mutate(s18_grade=ifelse(s1800<=80, "good", ifelse(s1800<=130, "normal", ifelse(s1800<=150, "caution", "bad"))))%>% 
  group_by(s18_grade) %>% 
  summarise(n=n())%>% 
  mutate(total=sum(n), pct=round(n/total*100,1))%>% 
  select(s18_grade,n,pct)%>% 
  arrange(desc(n))
#6-1 호선별로 18시 지하철 혼잡도 범주화
congestion1 %>% 
  mutate(s18_grade = ifelse(s1800 <= 80, "good", ifelse(s1800 <= 130, "normal", ifelse(s1800 <= 150, "caution", "bad")))) %>% 
  group_by(line, s18_grade) %>% 
  summarise(n = n()) %>% 
  mutate(total = sum(n), pct = round(n / total * 100, 1)) %>% 
  filter(s18_grade == "caution") %>% 
  select(line, s18_grade, n, pct) %>% 
  arrange(desc(pct)) %>% 
  head(5)

