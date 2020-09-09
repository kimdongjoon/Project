setwd("c:/Rdata")

crime = read.csv("http://datasets.flowingdata.com/crimeRatesByState-formatted.csv")
head(crime)

rownames(crime)= crime[,1]
rownames(crime)
stars(crime[,2:8],flip.labels = FALSE, key.loc = c(15,2))

stars(crime[,2:8],flip.labels = FALSE,
      draw.segments=TRUE, key.loc = c(15,2))

#ü������ ���̽�
install.packages("aplpack")
library("aplpack")
faces(crime[,2:8])



education = read.csv("http://datasets.flowingdata.com/education.csv")
head(education)

library(lattice)
parallel(education[,2:7])
parallel(education[,2:7],horizontal.axis = FALSE, col = 1)

summary(education$reading)
color = education$reading>523 
color
color+1
parallel(education[,2:7],horizontal.axis = FALSE, col= color+1)
summary(education$dropout_rate)

colorDropout_rate = education$dropout_rate>5.3 
colorDropout_rate+1
parallel(education[,2:7],horizontal.axis = FALSE, col= colorDropout_rate+1)


#�ּ��� �м�
# ������ �������� ���̸� ���� ���� ��ȯ�� ������ ����
# ������ ���� �������� ������� �� �� ���� ���� �������� ������
# ���� ���� ���� 
# ���� ������ ���� �ʰ� ������������ ����
# ����
# �������(Dimension reduction)
# ȸ�ͺм� �����м� ��� ������ �����ϱ� ���� �м����� ���
# �ڷ�Ž��
# �̻�ġ �Ǻ�
# �ڷ��� �׷�ȭ

data = read.csv("20140528_baseball.csv")
head(data)

model = prcomp(data[,2:6],scale = TRUE)
model
summary(model)
# > summary(model)
# Importance of components:
#                         PC1    PC2    PC3     PC4
# Standard deviation     1.7460 1.0357 0.7146 0.58385
# Proportion of Variance 0.6097 0.2145 0.1021 0.06818
# Cumulative Proportion  0.6097 0.8242 0.9264 0.99453
# PC5
# Standard deviation     0.16545
# Proportion of Variance 0.00547
# Cumulative Proportion  1.00000

# Cumulative Proportion ������
# 2�����̹Ƿ� PC1~2���� �� ����.
# �� 0.8242�ۼ�Ʈ��ŭ �������� ����.

plot(model)
head(data)
# ��  �·�  Ÿ�� ��Ÿ�� ����� �����å
# 1  KIA 0.455 0.285  0.424  0.345     5.40
# 2   LG 0.372 0.276  0.380  0.358     5.08
# 3   NC 0.578 0.283  0.445  0.360     4.11
# 4   SK 0.444 0.275  0.409  0.347     5.59
# 5 �ؼ� 0.545 0.283  0.471  0.372     5.44
# 6 �λ� 0.568 0.307  0.468  0.378     5.03

rownames(data) = data[,1]
head(data)

model = prcomp(data[,2:6],scale=TRUE)
biplot(model)