
.libPaths()
.libPaths("E:/R")
getwd()
setwd("E:/hanim")

library(ISLR)
library(MASS)
library(randomForest)
set.seed(1)

train=sample(1:nrow(Boston),nrow(Boston)/2)

bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=13,importance=TRUE)
#mtry=13->트리의 각 분할에 13개 설명변수 모두를 고려한다

bag.boston

boston.test=Boston[-train,"medv"]
yhat.bag=predict(bag.boston, newdata = Boston[-train,])

plot(yhat.bag,boston.test)
abline(0,1)
mean((yhat.bag-boston.test)^2)
#->단일 트리의 절반수준

bag.boston2=randomForest(medv~.,data=Boston,subset=train,mtry=13,ntree=25)
#ntree->randomforest에서 만들어지는 트리의 수
yhat.bag2=predict(bag.boston2, newdata = Boston[-train,])
mean((yhat.bag2-boston.test)^2)
# randomForest()-> 배깅, 랜포 다 가능 단지 mtry 값에 따라 달라짐(당연히 랜포 mtry가 작아짐)
# randomForest()-> 회귀트리의 경우 p/3 개의 변수 사용, 분류트리의 경우 sqrt(p)개의 변수 사용

rf.boston=randomForest(medv~.,data=Boston,subset=train, mtry=6,importance=TRUE)
yhat.rf=predict(rf.boston,newdata=Boston[-train,])

mean((yhat.rf-boston.test)^2)

#랜포가 더 작음!!


importance(rf.boston)
# importance()로 변수 중요도 확인가능 
# 변수 중요도에 대한 두가지 측도 제공
# 첫번째 측도: 주어진 변수가 모델에서 제외될때 배깅되지 않은 표본에 대한 예측 정확도의 평균 감소량 기반
# 두번째 측도: 주어진 변수에 대한 분할로 인한 노드 impurity의 총감소량을 모든 트리에 대해 평균한것 
# 불순도가 감소 == 순수도가 증가 -> 잘 분류
# 회귀트리: 훈련RSS에 의해 노드 불순도 측정
# 분류트리: 이탈도에 의해 노드 불순도 측정


varImpPlot(rf.boston)
# 위 두가지 지표를 시각화
# rm(주택크기), lstat(재산 수준) 이 가장 중요한 지표

############################
# Boosting
library(gbm)
set.seed(1)

boost.boston=gbm(medv~.,data=Boston[train,],distribution = "gaussian", n.trees = 5000,interaction.depth = 4)
# 회귀문제 이므로 distribution = "gaussian", 이진분류라면 distribution = "bernoulli"
# n.trees = 5000 -> 5000개의 트리 생성
# interaction.depth = 4 -> 각 트리의 깊이를 제한한다.

summary(boost.boston)

par(mfrow=c(1,2))
plot(boost.boston,i='rm')
plot(boost.boston,i='lstat')
# rm, lstat에 대한 부분 종속성 그래프 생성
# 다른 변수들을 통합한 후 반응변수에 대한 선택된 변수들의 효과를 보여줌

yhat.boost=predict(boost.boston,newdata = Boston[-train,], n.trees = 5000)
mean((yhat.boost-boston.test)^2)

# 검정 mse -> 랜포 비슷, 배깅보다 우수



boost.boston2=gbm(medv~.,data=Boston[train,],distribution = "gaussian", n.trees = 5000,interaction.depth = 4,shrinkage = 0.2)
# shrinkage->수축 파라미터 람다를 조정 (기본값은 0.001)
yhat.boost2=predict(boost.boston2,newdata = Boston[-train,], n.trees = 5000)
mean((yhat.boost2-boston.test)^2)



