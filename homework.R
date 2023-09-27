# table()함수/구분 1개의 인자를 가지고 도수분포표 작성
table(stb $Gender)

#  상대도수 계산
ECN <- table(stb $Gender)
prop.table(ECN)


# table()함수/2개의 인자를 가지고 교차표를 작성
table(stb $Gender, stb $Grade)

#막대그래프
barplot(table(stb $Nationality))

#막대그래프 가로 작성
barplot(table(stb$`residential area`), horiz=TRUE)

#Gender와 Grade 2개의 인자를 가지고 막대그래프 작성
entry <- table(stb $Gender, stb $Grade)
barplot(entry, legend = TRUE)

#파이차트
pie(table(stb $Grade))
