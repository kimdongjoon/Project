setwd("c:/Rdata")

# nrow() 
# ����Ʈ�� �������� ������ Ȯ��
# iris�� ���������� Ȯ��
# nrow(iris)  /  table(iris)   /   iris
# [1] 150
# iris�� 150���� ���� ���� �������̴�. 

# ������ ��ó�� �ܰ��� �⺻ ������ ������ ����. 
# �����ͼ�
#   ������ ���� �н� �Ϸ��� �ϴ� ��ü �����͸� ���Ѵ�. 
#   �����ͼ� ����ڴ� �������� ������ NA�������� ������ Ȯ�� �ϰ�
#   �н��� �ϱ� ���� ����ȭ�� �����ͷ� 1�� ������ �ؾ� �Ѵ�. 
# 
# Ʈ���̴� ������, �׽�Ʈ ������ �и�
#   �н��� ũ�� �����н��� �������н����� ���� �� �ִ�. 
#   �� �� ����ȭ�Ѵٸ� ��ȭ�н��� ���������� ���� ���ظ� ���� �����н��� �������н��� �켱������ �ٷ�ڽ��ϴ�. 
# 
#   ���� �н� (Supervisor)
#     �ϳ��� ������ �ϳ��� ���� �˷��ִ� ������� �н��ϴ� ����Դϴ�. 
#     ���� ��� ������ ������ �����ְ� �̰��� �������� ��� �˷��ִ� ������μ� 
#     ������ ������ ���� ������(�н��Ҽ���) ������ ������ ������ ������ �� ��谡 ��������� �Ǵ��� �� �ֵ��� �ϴ� ����Դϴ�. 
# 
#   ������ �н�(Unsupervisor)
#     ������ ���� �����鿡 ������ ��谡 �н��ϰ� �з��ϴ� ����Դϴ�. 
#     Ư�� ���Ͽ� ���� ��ǻ�Ͱ� �����͸� ������ �м��ϰ� ����� ������(����)�� �����̳� Ư�� �������μ� ����� ���� �� �� �ֽ��ϴ�.  

# ������

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

## R ������ �������� �����͸� �и��Ѵ�. 
# �������� �������� Ȯ���ϰ� Ʈ���ΰ� �׽�Ʈ �������� �з� ���� 
# ��踦 �������� �ؼ��� �� ���� �ؾ� �Ѵ�. 

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
                          , laplace = 1)              #�������� ���� �������� ����

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
#����ȭ ������ �ʿ���. 
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


#Package���� ����Ÿ?
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


#�ǻ���� Ʈ�� ��� in ��ġ�����ӽŷ���
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

##rpart �Լ��� ����� �ǻ����Ʈ�� �м�
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


##ANN ��ġ���������� �ӽŷ��׾˰����� ���
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

#ANN�� �ð�ȭ�� ���ؼ� ����ϴ� �˰����� ���
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

#RF model(�ӻ��) Method ��ġ�ӽŷ���
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


#�����н���(Unsupervised Model)
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