.libPaths()
.libPaths("E:/R")
getwd()
setwd("E:/hanim")


#pca
#pca시 centering을 해야한다 
#주어진 nxp 자료 X 에 대하여 X의 각 변수는 centering 되었다고 가정(분산에만 관심있기 때문)후 
#제곱해서 합한것의 1이 되는 가중치와 각 변수들의 선형결합이 가장 큰 뵤본 분산을 가지도록 첫번째 주성분을 찾음, 첫번째 주성분과 uncorrelated한 가중치와 변수들의 선형결합중 븐산을 최대로 하는 것을 찾음(uncorrleated이므로 첫번째 주성분과 두번째 성분이 직교하게된다 ) 

#pca시 scaling을 해야한다. (각 표준편차가 1이 되게 함)
#분산을 기준으로 주성분이 결정되기 때문에 단위가 다르다던가 해서 분산이 차이가 난다면 분산이 큰 변수 위주로 주성분의 가중치가 주어짐
#각 변수를 일괄적으로 scaling 해준다면 각 변수가 임의의 스케일링에 의존하는 결과가 될 것 이므로 각자 표준편차가 1이 되도록 scaling 

#BUT! 변수들이 같은 단위로 측정되었을때 scaling을 하지 않는 경우도 있다. 
#ex) P개의 유전자 발현수준들에 대응시 각 유전자의 발현 수준은 같은 "단위"로 측정되므로 스케일링 하지 않을 수도 있음 


#실제 분석에서 pca의 사용)
#데이터에서 관심있는 패턴을 찾기 위해 처음 몇개의 주성분 분석들을 살펴보는 경향이 있음
#이때 처음 몇개의 주성분에서 관심있는 패턴이 발견되지 않는다면 그 다음 주성분들이 흥미로운 결과를 줄 가능성은 거의 없다.
#처음 몇개의 주성분들이 흥미롭다면 관심있는 패턴이 발견되지 않을때까지 후속 주성분을 살펴본다.


states=row.names(USArrests)#미국의 50개주
states

head(USArrests)
names(USArrests)
apply(USArrests,2,mean)# 가운데 숫자가 1일경우 행에 적용, 2일 경우 열에 적용
apply(USArrests,2,var)#변수에 따라 분산이 매우 다름

#pca하기전에 scaling을 하지 않는다면 분산이 큰 Assualt 위주로 주성분이 구성됨

pr.out=prcomp(USArrests, scale=TRUE)#prcomp()는 평균이 0이 되도록 centering, scale=TRUE시에 표준편차가 1이 되도록 scaling
names(pr.out)
pr.out$center#centering전의 평균
pr.out$scale#scaling전의 분산
pr.out$rotation#주성분별 가중치 벡터
pr.out$x#행별 score vector?
summary(pr.out)
biplot(pr.out,scale=0)#biplot의 sclae=0인자는 화살표가 sclaing 되게 함

pr.out$sdev#표준편

