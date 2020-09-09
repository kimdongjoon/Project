setwd("c:/Rdata") # C드라이브 Rdata파일내
library(dplyr)    # dplyr패키지 로드
library(stringr)
data <- read.csv("winnerDrink.csv")  # winner팀의 winnerDrink.csv파일을 불러옴
head(data)       # 불러온 데이터 확인
str(data)        # CATEGORY값만이 chr 형식임을 확인
                 # 데이터는 10개의 변수와 166개의 object로 구성되어 있음
#View(data)       # 데이터 보기

rownames(data) = data[,1]

data=subset(data,select=-X)
head(data)
# T        : 순번
# TM       : 판매년월 
# CATEGORY : 카테고리
# ITEM_CNT : 상품품목수
# QTY      : 판매량
# PRICE    : 가격
# MAXTEMP  : 기온
# SALEDAY  : 영업(판매)일수
# RAIN_DAY : 강우일수
# HOLY_DAY : 휴일 일수

#인코딩과 별개로 제품 이름은 NA로 나오는데???
#텍스트 인코딩 
#에너지 음료 = 0
#일반탄산음료 = 1
#차음료 = 2

#인코딩이 필요한가??
data$CATEGORY <- ifelse(data$CATEGORY=="에너지음료",1,
                        ifelse(data$CATEGORY=="일반탄산음료",2,3))
                
                
#View(data)

data1= data%>%                        # 에너지음료 데이터 추출 46/166
  filter(CATEGORY == 1)
data2 = data%>%                       # 일반탄산음료 데이터 추출 60/166
  filter(CATEGORY == 2)
data3 = data%>%                       # 차음료 데이터 추출 60/166
  filter(CATEGORY == 3)

#stringr을 이용한 텍스트 분리(참고용)
# data1$CATEGORY = str_replace_all(data1$CATEGORY,"에너지음료","0")
# data1
# data2$CATEGORY = str_replace_all(data2$CATEGORY,"일반탄산음료","1")
# data2
# data3$CATEGORY = str_replace_all(data3$CATEGORY,"차음료","2")
# data3

head(data1)

head(data2)

head(data3)

# 각각의 히스토그램
hist(data1$QTY)
hist(data2$QTY)
hist(data3$QTY)

# 정규성 테스트
shapiro.test(data1$QTY) # p-value = 0.0001
shapiro.test(data2$QTY) # p-value = 0.01
shapiro.test(data3$QTY) # p-value = 0.08
# -> 이를통해 유의확률 p-value가 어느정도 인지 판별할수 있음.

# 상관관계 분석을 위해 cha 값인CATEGORY를 제외하여 다시 저장

#종속 변수 / 독립 변수
#상관관계 분석에서 제품명은 종속변수 이므로 데이터프레임에서 제외
data1=subset(data1,select=-CATEGORY)
data2=subset(data2,select=-CATEGORY)
data3=subset(data3,select=-CATEGORY)

# 판매량 상관관계 분석
cor(data1)

cor(data2)

cor(data3)

# 각각의 데이터에 대해 회귀분석을 적용
out1=lm(QTY~.,data=data1)
out2=lm(QTY~.,data=data2)
out3=lm(QTY~.,data=data3)

# 각데이터의 회귀분석 그래프
# plot(out1)
# plot(out2)
# plot(out3)

# (모형간소화)변수선택 방법을 지정. both이므도 단계
both1=step(out1,direction="both",trcce=FALSE) # 최종 AIC=542.58
both2=step(out2,direction="both",trcce=FALSE) # 최종 AIC=761.21
both3=step(out3,direction="both",trcce=FALSE) # 최종 AIC=654.38

# F분포를 통한 가설검정  F = (군간변동)/(군내변동)
anova(both1)
anova(both2)
anova(both3)


#최적화 진행
out1=lm(QTY~.,data=data1)
#   Df   Sum Sq  Mean Sq F value    Pr(>F)    
#   PRICE      1  7811664  7811664  64.001 5.594e-10 ***
#   MAXTEMP    1  3044171  3044171  24.941 1.085e-05 ***
#   SALEDAY    1 15024987 15024987 123.100 4.610e-14 ***
out2=lm(QTY~.,data=data2)
#   (Intercept) -1.105e+06  4.098e+05  -2.696 0.009340 ** 
#   YM           5.511e+00  2.045e+00   2.695 0.009351 ** 
#   ITEM_CNT    -5.603e+01  2.621e+01  -2.137 0.037114 *  
#   PRICE       -2.407e+00  1.118e+00  -2.153 0.035817 *  
#   MAXTEMP      9.669e+01  7.846e+00  12.323  < 2e-16 ***
#   SALEDAY      2.850e-02  8.015e-03   3.556 0.000792 ***
#   탄산음료의 경우 연월, 음료의 숫자, 가격은 매출에 상관없음을 확인
#   탄산음료에 대한 모델 2개 생성
#   1번모델) MAXTEMP , SALEDAY
#   2번모델) MAXTEMP , SALEDAY , YM
#   관계가 적은 ITEM_CNT와 PRICE 제거
out2_1=lm(QTY~MAXTEMP+SALEDAY,data=data2)
out2_2=lm(QTY~MAXTEMP+SALEDAY+YM,data=data2)

out3=lm(QTY~.,data=data3)
#   Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#  (Intercept) 407.001584 435.740293   0.934    0.354    
#   ITEM_CNT    -19.839826  12.502835  -1.587    0.118    
#   MAXTEMP      50.176566   4.235722  11.846  < 2e-16 ***
#   SALEDAY       0.011402   0.001013  11.255 6.84e-16 ***
#   RAIN_DAY     -0.006452   0.004013  -1.608    0.114  
#   차의 경우 MAXTEMP와 SALEDAY가 매출에 관계가 높고 
#   ITEM_CNT와 RAIN_DAY는 관계가 매우 적음
#   1번 모델) MAXTEMP, SALEDAY
out3_1=lm(QTY~MAXTEMP+SALEDAY,data=data3)

#최적화 진행
# (모형간소화)변수선택 방법을 지정. both이므도 단계
both1=step(out1,direction="both",trcce=FALSE) # 최종 AIC=542.58
# 동일
both2=step(out2,direction="both",trcce=FALSE) # 최종 AIC=764.29

# QTY ~ MAXTEMP + SALEDAY
both2_1=step(out2_1,direction="both",trcce=FALSE) # 최종 AIC=769.95 

both2_2=step(out2_2,direction="both",trcce=FALSE) # 최종 AIC=768.45

# QTY ~ ITEM_CNT + MAXTEMP + SALEDAY + RAIN_DAY
both3=step(out3,direction="both",trcce=FALSE) # 최종 AIC=654.38

# QTY ~ MAXTEMP + SALEDAY
both3_1=step(out3_1,direction="both",trcce=FALSE) # 최종 AIC=655.68


# F분포를 통한 가설검정  F = (군간변동)/(군내변동)
anova(both1)

anova(both2)
anova(both2_1)
anova(both2_2)

anova(both3)
anova(both3_1)


# 회귀식
summary(both1)
summary(both2)    summary(both2_1)    summary(both2_2)
summary(both3)    summary(both3_1)
# 에너지음료 R-squared: 0.8229    판매량 = -3323-0.9419*PRICE+28.49*MAXTEMP+0.02586*SALEDAY
# 일반탄산음료 R-squared: 0.8846  판매량 = 410.7761+68.6257(X)-73.2499(ITEM_CNT)-2.4391(PRICE)+93.8410(MAXTEMP)+0.0223(SALEDAY)
# 차음료 R-squared: 0.8814        판매량 = 407.0016-19.8398(ITEM_CNT)+50.1766(MAXTEMP)+0.0114(SALEDAY)-0.0065

# 회귀식을 통한 예측 판매량을 반올림하여 pred에 저장
pred1 = data1 %>%
  mutate(pred_QTY = -3323-0.9419*PRICE+28.49*MAXTEMP+0.02586*SALEDAY)%>%
  summarise(QTY,round(pred_QTY))

pred2 = data2 %>%
  mutate(pred_QTY = 410.7761+68.6257*X-73.2499*ITEM_CNT-2.4391*PRICE+93.8410*MAXTEMP+0.0223*SALEDAY)%>%
  summarise(QTY,round(pred_QTY))

pred3 = data3 %>%
  mutate(pred_QTY = 407.0016-19.8398*ITEM_CNT+50.1766*MAXTEMP+0.0114*SALEDAY-0.0065*RAIN_DAY)%>%
  summarise(QTY,round(pred_QTY))

# 실제 판매량과 예측 판매량 비교
pred1
pred2
pred3

# 그래프를 통한 X=Y그래프에 유하삼을 볼 수 있음
plot(pred1)
plot(pred2)
plot(pred3)
