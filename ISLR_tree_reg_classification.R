
.libPaths()
.libPaths("E:/R")
getwd()
setwd("E:/bungae")

install.packages("tree")
library(tree)
library(ISLR)
attach(Carseats)#자료 carseats사용->NAs introduced by coercion

library(MASS)
attach(Boston)# 대체 자료 Boston 사용
head(Boston)
summary(Boston$medv)
High=ifelse(medv<=22,"NO","Yes")#8을 초과하면 yes, 그렇지 않으면 no

tree.Boston=tree(medv~.,Boston)
#High->이진분류 사용시 NAs introduced by coercion 발생

summary(tree.Boston)

plot(tree.Boston)
text(tree.Boston,pretty=0)#lstat은 사회 경제적 지위가 낮은 사람들의 백분률을 측정
# -> tree는 

tree.Boston#터미널 노드로 이어지는 가지에는 별표가 표시

#분류 트리의 성능을 평가하기 위해서는 훈련오차가 아니라 검정오차를 추정해야함
# train set, test set으로 분할 후 성능을 test set으로 평가

set.seed(2)
train=sample(1:nrow(Boston),nrow(Boston)/2)

tree.Boston2=tree(medv~.,Boston,subset=train)
summary(tree.Boston2)

plot(tree.Boston2)
text(tree.Boston2,pretty=0)
# lstat=사회경제적 지위가 늦은 사람들의 백분율을 측정
# lstat가 낮을수록 높은 주택가격에 대응됨
# 마지막 결과가 주택가격

cv.boston=cv.tree(tree.Boston2)# cv.tree함수로 트리 pruning 성능 개선 가능
plot(cv.boston$size,cv.boston$dev,type="b")
# 위 경우 가장 복잡한 tree가 cv에 의해 선택됨

# tree pruning을 원할경우 prune.tree() 함수를 사용가능
prune.boston=prune.tree(tree.Boston2,best=5)
plot(prune.boston)
text(prune.boston,pretty=0)

yhat=predict(tree.Boston2,newdata=Boston[-train,])
boston.test=Boston[-train,'medv']
plot(yhat,boston.test)
abline(0,1)
mean((yhat-boston.test)^2)
sqrt(21.45)
# test set의 mse=21.45, 제곱근은 4.6이다
# -> test set에 대한 모델의 예측값이 교외지역 실제 메디안 주택가격의 4600$안에 있다
