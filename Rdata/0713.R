setwd("c:/Rdata")

# nrow() 
# 리스트형 데이터의 갯수를 확인
# iris의 데이터형을 확인
# nrow(iris)  /  table(iris)   /   iris
# [1] 150
# iris는 150개의 열을 가진 데이터이다. 

# 데이터 전처리 단계의 기본 구조는 다음과 같다. 
# 데이터셋
#   데이터 셋은 학습 하려고 하는 전체 데이터를 말한다. 
#   데이터셋 사용자는 데이터의 구조와 NA데이터의 유무를 확인 하고
#   학습을 하기 위한 정형화된 데이터로 1차 가공을 해야 한다. 
# 
# 트레이닝 데이터, 테스트 데이터 분리
#   학습은 크게 지도학습과 비지도학습으로 나눌 수 있다. 
#   좀 더 세분화한다면 강화학습도 존재하지만 개념 이해를 위해 지도학습과 비지도학습만 우선적으로 다루겠습니다. 
# 
#   지도 학습 (Supervisor)
#     하나의 문제와 하나의 답을 알려주는 방식으로 학습하는 방식입니다. 
#     예를 들어 강아지 사진을 보여주고 이것은 강아지다 라고 알려주는 방법으로서 
#     강아지 사진을 보면 볼수록(학습할수록) 다음에 강아지 사진을 보았을 때 기계가 강아지라고 판단할 수 있도록 하는 방식입니다. 
# 
#   비지도 학습(Unsupervisor)
#     정답이 없는 문제들에 스스로 기계가 학습하고 분류하는 방식입니다. 
#     특정 패턴에 따라 컴퓨터가 데이터를 스스로 분석하고 비슷한 데이터(군집)의 묶음이나 특정 패턴으로서 결과를 도출 할 수 있습니다.  

# 포매팅

nrow(iris)
iris_train = iris[1:105,]
iris_test = iris[106:150,]

nrow(iris_train)
nrow(iris_test)

head(iris)

idx= sample( 1:nrow(iris)
            ,size = nrow(iris)*0.7
            ,replace = F)
idx

iris_train = iris[idx,]
iris_test  = iris[-idx,]
nrow(iris_train)
nrow(iris_test)

head(iris)

table(iris$Species)

## R 에서는 수동으로 데이터를 분리한다. 
# 데이터의 포매팅을 확인하고 트레인과 테스트 데이터의 분류 또한 
# 통계를 기준으로 해석할 때 동일 해야 한다. 

install.packages("caret")
library(caret)

train.idx = createDataPartition(iris$Species
                                , p = 0.7 
                                , list=F)
iris_train = iris[train.idx, ]
table(iris_train$Species)

iris_test = iris[-train.idx,]
table(iris_test$Species)




#naive bayes Model Method
library(e1071)
naive.result = naiveBayes(iris_train
                          , iris_train$Species
                          , laplace = 1)              #물리현상에 의한 해저드계산 적용

naive.pred = predict(naive.result, iris_test, type = "class")
table(naive.pred, iris_test$Species)
confusionMatrix(naive.pred, iris_test$Species)


#Logistic Regression Method
library(nnet)
multi.result = multinom(Species~., iris_train)
multi.pred = predict(multi.result, iris_test, type = "class")
table(multi.pred, iris_test$Species)

confusionMatrix(multi.pred, iris_test$Species)

#Decision Tree model mothod
library(rpart)
rpart.result = rpart(Species~.,data = iris_train)
rpart.pred = predict(rpart.result, iris_test, type = "class")
table(rpart.pred, iris_test$Species)

confusionMatrix(multi.pred, iris_test$Species)


install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(rpart.result)


#ANN Model Method
#정규화 내용이 필요함. 
library(nnet)
head(iris_train)
iris_train_scale = as.data.frame(sapply(iris_train[,-5],scale))
iris_train_scale
iris_test_scale = as.data.frame(sapply(iris_test[,-5],scale))
iris_test_scale
iris_train_scale$Species = iris_train$Species
iris_test_scale$Species = iris_test$Species

head(iris_train_scale)
head(iris_test_scale)

nnet.result = nnet(Species~.,iris_train_scale
                   , size = 3)
nnet.pred = predict(nnet.result
                    , iris_test_scale
                    , type = "class")

table(nnet.pred, iris_test_scale$Species)

confusionMatrix(nnet.pred,iris_test$Species)
f_predict = as.factor(nnet.pred)
f_Species = as.factor(iris_test$Species)

confusionMatrix(f_predict, f_Species)



#SVM (Support Vector Machine) Model Method
install.packages("kernlab")
library(kernlab)
svm.result = ksvm(Species~.
                  , data = iris_train
                  , kernel= "rbfdot")

svm.pred = predict(svm.result
                   , iris_test
                   , type = "response")

table(svm.pred, iris_test$Species)

confusionMatrix(svm.pred, iris_test$Species)

#Random Forest Model Method
install.packages("randomForest")
library (randomForest)
rf.result = randomForest(Species~.
                         , data = iris_train
                         , ntree = 500)
rf.pred = predict(rf.result 
                  , iris_test
                  , type = "response")

table(rf.pred , iris_test$Species)

confusionMatrix(rf.pred, iris_test$Species)


#Package안의 데이타?
install.packages("Mass")
Boston = as.data.frame(MASS::Boston)
names(boston)

idx = sample(1:nrow(Boston)
             ,size = nrow(Boston) * 0.7
             ,replace= F)

Boston_train = Boston[idx,]
Boston_test = Boston[-idx,]

dim(Boston_train) ; dim (Boston_test)

lm.fit = lm(medv~.
            ,data = Boston_train)
summary(lm.fit)


lm.fit2 = step(lm.fit
               ,method = "both")
summary(lm.fit2)

lm.yhat2 = predict(lm.fit2
                   ,newdata = Boston_test)
kk = mean((lm.yhat2-Boston_test$medv)^2)
sqrt(kk)
plot(lm.yhat2,Boston_test$medv)
abline(a = 0, b = 1, col = 2)


#의사결정 트리 기법 in 수치예측머신러닝
install.packages("tree")
library(tree)
tree.fit = tree(medv~.
                ,data = Boston_train)
summary(tree.fit)
plot(tree.fit)
text(tree.fit,pretty = 0)

tree.yhat = predict(tree.fit
                    , newdata = Boston_test)
kk = mean((tree.yhat - Boston_test$medv)^2)
sqrt(kk)
plot(tree.yhat, Boston_test$medv)
abline(a = 0 , b = 1, col = 2)

##rpart 함수를 사용한 의사결정트리 분석
library(rpart)
rpart.fit = rpart(medv~.
                  ,data = Boston_train)
summary(rpart.fit)
library(rpart.plot)
rpart.plot(rpart.fit,
           digits = 3
           ,type = 3
           ,extra = 1,
           fallen.leaves = F)

rpart.yhat = predict(rpart.fit
                     , newdata = Boston_test)
kk = mean((rpart.yhat - Boston_test$medv)^2)
sqrt(kk)


##ANN 수치예측모형의 머신러닝알고리즘 방법
normalize = function(x){return((x-min(x)/max(x)-min(x)))}
Boston_train_norm = as.data.frame(sapply(Boston_train, normalize))
Boston_test_norm = as.data.frame(sapply(Boston_test, normalize))

library(nnet)

nnet.fit = nnet(medv~.
         ,data = Boston_train_norm
         ,size =5 )
nnet.yhat =predict(nnet.fit
                   ,newdata = Boston_test_norm
                   ,type = "raw")
kk = mean((nnet.yhat-Boston_test_norm$medv)^2)
sqrt(kk)

#ANN의 시각화를 위해서 사용하는 알고리즘 방법
install.packages("neuralnet")
library(neuralnet)
neural.fit = neuralnet(medv~crim+zn+indus+chas+nox+rm+age+tax+ptratio+black+lstat
                       ,data = Boston_train_norm
                       ,hidden = 5)

neural.result = compute(neural.fit
                        ,Boston_test_norm[1:13])

neural.yhat = neural.result$net.result
kk = mean((neural.yhat-Boston_test_norm$medv)^2)
sqrt(kk)
plot(neural.fit)

#RF model(앙상블) Method 수치머신러닝
library(randomForest)
set.seed(1)
rf.fit = randomForest(medv~.
                      ,data = Boston_train
                      ,mtry = 6
                      ,importance = T)

plot(rf.fit)
importance(rf.fit)

varImpPlot(rf.fit)

rf.yhat = predict(rf.fit
                  ,newdata = Boston_test)

kk = mean((rf.yhat-Boston_test$medv)^2)
sqrt(kk)


#자율학습모델(Unsupervised Model)
#K-Means Model Method
iris2 = iris[,1:4]
#iris2

km.out.withness = c()
km.out.between = c()
kk = c()

for (i in 2:7){
  set.seed(1)
  km.out = kmeans(iris2, center = i )
  km.out.withness[i-1] = km.out$tot.withinss
  km.out.between[i-1] = km.out$betweenss
  kk = c(kk,(paste0("k=",i)))
}
tt = data.frame(kk, km.out.withness, km.out.between)
par(mfrow=c(1,2))
plot(kk=c(1:7), tt$km.out.withness, type = 'b')
plot(kk=c(1:7), tt$km.out.between, type = 'b')


km.out.k3 = kmeans(iris2,center = 3)
km.out.k3$centers
km.out.k3$cluster

km.out.k3$size
table(km.out.k3$cluster
      ,iris$Species)


plot(iris2[,1:2],col =km.out.k3$cluster, pch = ifelse(km.out.k3$cluster == 1,16,
                                                      ifelse(km.out.k3$cluster == 2,17,18)),
     cex = 2)

points(km.out.k3$centers, col= 1:3, pch = 16:18, cex = 5 )


#
USArrests
pc1 = princomp(USArrests
               ,cor = T)
plot(pc1)
summary(pc1)

pc1$center
pc1$scale
pc1$loadings
pc1$scores
plot(pc1$scores[,1],pc1$scores[,2], xlab = "Z1", ylab = "Z2")
abline(v= 0 ,h = 0)
biplot(pc1, cex = 0.7)
abline(v= 0, h= 0 , col = "gray")
