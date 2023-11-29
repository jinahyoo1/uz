responce=c(2,3,4,4,5,6,6,7,8)
x=rep(c("model1","model2","model3"),c(3,3,3))
levels=factor(x)
ano.result=aov(responce~levels)
summary(ano.result)

#유의수준0.01보다 0.008이 작으므로 귀무가설을 기각한다. 