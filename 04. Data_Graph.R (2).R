#히스토그램
hist(finedust$'3_ultrafine dust',main = "서울시 서대문구 2020년 1월 초미세먼지 측정분포", col = terrain.colors(12))

#히스토그램
> hist(finedust$'3_ultrafine dust',main = "서울시 서대문구 2020년 1월 초미세먼지 측정분포",col = terrain.colors(12),freq = FALSE)
lines(density(finedust$'3_ultrafine dust'),lwd=2)

#박스 플롯 
boxplot(finedust$`3_fine dust`, main="야식업의2020년1월미세먼지발생현황", col="yellow")

#2개업종의 박스플롯
boxplot(finedust$`3_fine dust`, finedust$`7_fine dust`, main="업종별2020년1월미세먼지발생현황", col="yellow", names = c("야식업","중식"))

#산점도
plot(x=finedust$`3_fine dust`, y=finedust$`3_ultrafine dust`, xlab="미세먼지", ylab="초미세먼지", main="미세먼지와초미세먼지의변화")

#산점도 편집!
plot(x=finedust$`3_fine dust`, y=finedust$`3_ultrafine dust`, xlab="미세먼지", ylab="초미세먼지", main="미세먼지와초미세먼지의변화", pch=24, col="red", bg="yellow", cex=1.5)

#산점도 타입별 편집
plot(x=finedust$`3_fine dust`, y=finedust$`3_ultrafine dust`, xlab="미세먼지", ylab="초미세먼지", main="미세먼지와초미세먼지의변화", type = "h")
