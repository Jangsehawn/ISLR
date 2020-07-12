
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
# p_value가 아주 작다 -> 해당모델이 전 모델보다 더 유의미하다.
# p_value가 아주 크다 -> 해당모델보다 전 모델이 더  유의미하다.
# -> 2,3차 정도의 모델을 사용한다. 

coef(summary(fit.5))
# poly()가 직교다항식을 생성하므로 같은 결과를 얻어낼 수 있다.


fit.1=lm(wage~education+age,data=Wage) 
fit.2=lm(wage~education+poly(age,2),data=Wage) 
fit.3=lm(wage~education+poly(age,3),data=Wage) 
anova(fit.1,fit.2, fit.3)
# 모델에 다른 항이 있다하더라도 작동함


# 개인의 연간소득이 25만 달러보디 높은지를 예측
fit=glm(I(wage>250)~poly(age,4),data=Wage, family=binomial)
# 다항식 로지스틱 회귀모형을 적합
# I(wage>250)->TRUE,FALSE로 나타내는 논리변수
# glm은 TRUE=1, FASLE=0 으로 이진수 변환
preds=predict(fit,newdata=list(age=age.grid),se=T)

# 신뢰구간이 오즈비(odds ratio)로 나옴 

pfit=exp(preds$fit)/(1+exp(preds$fit))
se.bands.logit=cbind(preds$fit+2*preds$se.fit, preds$fit-2*preds$se.fit)
se.bands=exp(se.bands.logit)/(1+exp(se.bands.logit))
preds=predict(fit,newdata=list(age=age.grid),type="response",se=T)
# preds
#-> 계산된 확률이 음수가 된다는데 확인해보니 음수가 없음

plot(age,I(wage>250),xlim=agelims,type='n',ylim=c(0,.2))
points(jitter(age),I((wage>250)/5),cex=.5,pch='|',col='darkgrey')
lines(age.grid,pfit,lwd=2,col='blue')
matlines(age.grid,se.bands)
# rug graph-> 0,1로 표현
# wage>250 은 윗부분에 회색
# wage<250 은 아랫부분에 회색
# 같은 age 값을 가진 관측치들이 서로 겹치지 않도록 jitter()을 이용해서 age를 변화

table(cut(age,4))# 4개로 나눠 절단
fit=lm(wage~cut(age,4),data=Wage)
coef(summary(fit))
#첫번째 계수는 나이가 33.5세가 되지 않는 사람들에 대한 평균급여로 해석 가능 
# 다른 계수들은 다른 나이그룹에 속하는 사람들의 추가적인 평균 급여로 해석가능

###########스플라인######
library(splines)
fit=lm(wage~bs(age,knots=c(25,40,60)),data=Wage)
# knots->매듭위치
# bs()는 knots을 가진 스플라인에 대한 기저함수들의 전체 행렬을 생성 
pred=predict(fit,newdata=list(age=age.grid),se=T)       
plot(age,wage,col='gray')
lines(age.grid,pred$fit,lwd=2)
lines(age.grid,pred$fit+2*pred$se,lty='dashed')
lines(age.grid,pred$fit-2*pred$se,lty='dashed')

dim(bs(age,knots=c(25,40,60)))
attr(bs(age,df=6),"knots")
#-> 분위수로 매듭위치 결정

##natural spline
fit2=lm(wage~ns(age,df=4),data=Wage)#자유도가 4인 자연스플라인
pred2=predict(fit2,newdata=list(age=age.grid),se=T)
lines(age.grid,pred2$fit,col="red",lwd=2)

plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
fit=smooth.spline(age,wage,df=16)
fit_smoo=smooth.spline(age,wage,cv=TRUE)
fit_smoo$df
lines(fit,col="red",lwd=2)
lines(fit_smoo,col="blue",lwd=2)
legend("topright",legend=c("16 DF","6.8 DF"), col=c("red","blue"),lty=1,lwd=2,cex=.8)
# 첫번째 선-> 자유도가 16인 람다
# 두번째 선-> 교차검증에 의해 자유도가 6.8인 람다 선택

######local regression######

plot(age,wage,xlim=agelims,cex=.5,col='darkgrey')
fit=loess(wage~age,span=.2,data=Wage)
fit2=loess(wage~age,span=.5,data=Wage)
lines(age.grid,predict(fit,data.frame(age=age.grid)),col="blue",lwd=2)
# span이 0.2 -> 각 이웃이 관측치들의 20%로구성
lines(age.grid,predict(fit2,data.frame(age=age.grid)),col="red",lwd=2)
#span이 0.5 -> 각 이웃이 관측치들의 50%로구성
# span이 클수록 적합이 더 평활하다

###GAMs

gam1=lm(wage~ns(year,4)+ns(age,5)+education,data=Wage)

install.packages("gam")
library(gam)

gam.m3=gam(wage~s(year,4)+s(age,5)+education,data=Wage)
par(mfrow=c(1,3))
plot(gam.m3,se=TRUE,col="blue")
plot.Gam(gam1,se=TRUE,col="red")


gam.m1=gam(wage~s(age,5)+education,data=Wage)
gam.m2=gam(wage~year+s(age,5)+education,data=Wage)
anova(gam.m1,gam.m2,gam.m3,test="F")
#->gam.m2가 제일 좋음
summary(gam.m3)
####year,age에 대한 p_value값들-> 귀무가설: 선형 상관관계, 대립가설: 비선형 상관관계
#year의 p값(0.3557>>0.05) -> 선형함수로 충분
#age의 p값 (<<0.05)-> 비선형 항이 필요

preds=predict(gam.m2,newdata=Wage)
gam.lo=gam(wage~s(year,df=4)+lo(age,span=0.7)+education,data=Wage)
# span이 0.7인 국소회귀사용
plot.Gam(gam.lo,se=TRUE, col="green")

gam.lo.i=gam(wage~lo(year,age,span=0.5)+education,data=Wage)
#lo() 함수로 interaction term 생성 가능

install.packages('akima')
library(akima)# 2차원 표면을 그래프로 나타냄
plot(gam.lo.i)

gam.lr=gam(I(wage>250)~year+s(age,df=5)+education,family=binomial,data=Wage)
# 로지스틱 회귀 gam을 적합하기 위해서 I()를 사용하고 family=binomial로 설정
par(mfrow=c(1,3))
plot(gam.lr,se=T,col="green")


table(education,I(wage>250))
#-> HS범주에는 소득 높은 사람이 없음

gam.lr.s=gam(I(wage>250)~year+s(age,df=5)+education,family=binomial,
             data=Wage,subset=(education!="1. < HS Gred"))
plot(gam.lr.s,se=T,col='green')
# -> HS범주제외히고 로지스틱 회귀 gam 적합




