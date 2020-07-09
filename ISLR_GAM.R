
getwd()
setwd("E:/R")
.libPaths()
.libPaths("E:/R")
install.packages('ISLR')
library(ISLR)
attach(Wage)
#################다항식회귀와 계단함ㅅ
head(Wage)
fit=lm(wage~poly(age,4),data=Wage)
#poly(age,4)->aged의 4차 다항식
#fit->age의 4차다항식을 사용해 wage를 예측
#poly()->열들이 직교 다항식인 기저인 행렬 반환->각열들이 변수 a,a^2,a^3,a^4의 선형결합
coef(summary(fit))

fit2=lm(wage~poly(age,4,raw=T),data=Wage)# a,a^2,a^3,a^4를 직접구함
poly(age,4,raw=T)# a,a^2,a^3,a^4를 직접구함
poly(age,4)# 기저로 반환
coef(summary(fit2))#raw=T가 계수추정치에는 영향을 미치지만 적합된 값에는 영향 x

#######같은 의미의 식들

fit2a=lm(wage~age+I(age^2)+I(age^3)+I(age^4),data=Wage)
coef(fit2a)## a,a^2,a^3,a^4를 직접구한것과 같은 결과

fit2b=lm(wage~cbind(age,age^2,age^3,age^4),data=Wage)
coef(fit2b)#위와 같은 결과

agelims=range(age)
age.grid=seq(from=agelims[1],to=agelims[2])
age.grid
preds=predict(fit,newdata=list(age=age.grid),se=TRUE)
# lm( )을 통해 모델을 만들고 나면 새로운 데이터에 대한 예측값은 predict( )로 구할 수 있다.
se.bands=cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit)
# 표준오차 제공
par(mfrow=c(1,2),mar=c(4.5,4.5,1,1),oma=c(0,0,4,0))
plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
title("Degree -4 polynomial",outer=T)
lines(age.grid, preds$fit,lwd=2,col='blue')# 피팅라인
matlines(age.grid,se.bands,lwd=1,col='blue',lty=3)# 신뢰구간

preds2=predict(fit2,newdata=list(age=age.grid),se=TRUE)
max(abs(preds$fit-preds2$fit))

# 다항식 회귀-> 차수를 결정 해야함
# 선형에서 5차까지 모델을 적합해 결정
# 븐산분석(anova)를 사용
# H0: Model1 vs. H1: Model2
#->Model2이 Model1을 포함하는(nested) 모델-> Model2가 더 복잡
#->anova 사용 위해서는 위 가정이 있어야함(nested)
#->Model1의 설명변수들이 Model2에 내포

fit.1=lm(wage~age,data=Wage)
fit.2=lm(wage~poly(age,2),data=Wage)
fit.3=lm(wage~poly(age,3),data=Wage)
fit.4=lm(wage~poly(age,4),data=Wage)
fit.5=lm(wage~poly(age,5),data=Wage)






anova(fit.1,fit.2,fit.3,fit.4,fit.5)



