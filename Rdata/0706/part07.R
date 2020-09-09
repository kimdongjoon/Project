install.packages("dplyr")
library(dplyr) # dplyr ��Ű�� �ε�


df <- data.frame(sex = c("M", "F", NA, "M", "F"),
                 score = c(5, 4, 3, 4, NA))

df


is.na(df)         

table(is.na(df$sex))    # sex ����ġ �� ���

table(is.na(df$score))  # score ����ġ �� ���

df %>% filter(is.na(score))   # score�� NA�� �����͸� ���

df_nomiss <- df %>% filter(!is.na(score))  # score ����ġ ����
df_nomiss

# score, sex ����ġ ����
df_nomiss <- df %>% filter(!is.na(score) & !is.na(sex))
df_nomiss  


df_nomiss2 <- na.omit(df)  # ��� ������ ����ġ ���� ������ ����
df_nomiss2                 # ���

mean(df$score, na.rm = T)  # ����ġ �����ϰ� ��� ����
## [1] 4
sum(df$score, na.rm = T)   # ����ġ �����ϰ� �հ� ����
## [1] 16

mean(df$score, na.rm =T)
sum(df$score, na.rm = T)



setwd("c:/Rdata")
exam <- read.csv("csv_exam.csv")            # ������ �ҷ�����
exam

exam[c(3,8,15), "math"] = NA
exam           #������ ���� ������ ������

#���� ��� Ȯ��
origin = exam %>% summarise(mean_math = mean(math, na.rm = T))  
origin
##57.45

exam %>% summarise(mean_math = mean(math))             # ��� ����
##   mean_math
## 1        NA
exam %>% summarise(mean_math = mean(math, na.rm = T))  # ����ġ �����ϰ� ��� ����
##   mean_math
## 1  55.23529

exam %>% summarise(mean_math = mean(math, na.rm = T),      # ��� ����
                   sum_math = sum(math, na.rm = T),        # �հ� ����
                   median_math = median(math, na.rm = T))  # �߾Ӱ� ����
##   mean_math sum_math median_math
## 1  55.23529      939          50

mean(exam$math, na.rm = T)  # ����ġ �����ϰ� math ��� ����
## [1] 55.23529

exam$math <- ifelse(is.na(exam$math), 55, exam$math)  # math�� NA�� 55�� ��ü
table(is.na(exam$math))                               # ����ġ ��ǥ ����
## 
## FALSE 
##    20
exam


exam = read.csv("csv_exam.csv")
exam[c(3,8,15),"math"] = NA
exam
exam%>%
  summarize(mean_math= round(mean(math, na.rm = T)))

kk = table(is.na(exam$math))
tt = barplot(kk,col= rainbow(2),ylim= c(0,20))
text(tt,kk,label=paste0(kk,"��", pos = 3))

exam

exam %>% filter(id == 3
               |id == 8
               |id == 15)


#���
exam = read.csv("csv_exam.csv")
exam[c(3,8,15),"math"] = NA
exam

mean(exam$math)
mean(exam$math, na.rm = T)
exam$math= ifelse(is.na(exam$math), 55, exam$math)

mean(exam$math)

# ������ ������ Ȯ��
library(ggplot2)
mpg = ggplot2::mpg
mpg

# ������ ������ ����
mpg = as.data.frame(ggplot2::mpg)
mpg
View(mpg)
mpg[c(65,124,131,153,212), "hwy"] = NA
table(is.na(mpg$hwy))

df_mpg = mpg%>%
  group_by(drv)%>%
  summarize(mean_hwy = round(mean(hwy,na.rm = T),1))

df_mpg
ggplot(data=df_mpg,aes(x=drv, y=mean_hwy))+geom_col()
ggplot(data = mpg, aes(x=drv))+geom_bar()



# ȥ�ڼ� �غ���
# ����ġ�� ����ִ� mpg �����͸� Ȱ���ؼ� ������ �ذ��غ�����.
# ???	Q1. drv(�������)���� hwy(���ӵ��� ����) ����� ��� �ٸ��� �˾ƺ����� �մϴ�. �м��� �ϱ� ���� �켱 �� ������ ����ġ�� �ִ��� Ȯ���ؾ� �մϴ�. drv ������ hwy ������ ����ġ�� �� �� �ִ��� �˾ƺ�����.
# ???	Q2. filter()�� �̿��� hwy ������ ����ġ�� �����ϰ�, � ��������� hwy ����� ������ �˾ƺ�����. �ϳ��� dplyr �������� ������ �մϴ�.

# ��Ʈ
# Q1. ��ǥ�� ����� table()�� ����ġ�� Ȯ���ϴ� is.na()�� ������ ������.
# Q2. filter()�� is.na()�� ������ ����ġ�� �����ϰ�, ���ܺ� ����� ���ϴ� �ڵ带 %>%�� �����ϸ� �˴ϴ�.

# Q1.drv(�������)���� hwy(���ӵ��� ����) ����� ��� �ٸ��� �˾ƺ����� �մϴ�. �м��� �ϱ� ���� �켱 �� ������ ����ġ�� �ִ��� Ȯ���ؾ� �մϴ�. drv ������ hwy ������ ����ġ�� �� �� �ִ��� �˾ƺ�����.
# table(is.na(mpg$drv))  # drv ����ġ ��ǥ ���
# ## 
# ## FALSE 
# ##   234
# table(is.na(mpg$hwy))  # hwy ����ġ ��ǥ ���
# ## 
# ## FALSE  TRUE 
# ##   229     5

# mpg %>%
#   filter(!is.na(hwy)) %>%          # ����ġ ����
#   group_by(drv) %>%                # drv�� �и�
#   summarise(mean_hwy = mean(hwy))  # hwy ��� ���ϱ�
# ## # A tibble: 3 x 2
# ##     drv mean_hwy
# ##   <chr>    <dbl>
# ## 1     4 19.24242
# ## 2     f 28.20000
# ## 3     r 21.00000


# �̻�ġ �����ϱ� - 1. ������ �� ���� ��
# ???	���������� ������ �� �����Ƿ� �ٷ� ���� ó�� �� �м��� ����
# �̻�ġ ���Ե� ������ ���� - sex 3, score 6
outlier <- data.frame(sex = c(1, 2, 1, 3, 2, 1),
                      score = c(5, 4, 3, 4, 2, 6))
outlier

# �̻�ġ Ȯ���ϱ�
table(outlier$sex)
## 
## 1 2 3 
## 3 2 1
table(outlier$score)
## 
## 2 3 4 5 6 
## 1 1 2 1 1

# ���� ó���ϱ� - sex
# sex�� 3�̸� NA �Ҵ�
outlier$sex <- ifelse(outlier$sex == 3, NA, outlier$sex)
outlier
##   sex score
## 1   1     5
## 2   2     4
## 3   1     3
## 4  NA     4
## 5   2     2
## 6   1     6

# ���� ó���ϱ� - score
# sex�� 1~5 �ƴϸ� NA �Ҵ�
outlier$score <- ifelse(outlier$score > 5, NA, outlier$score)
outlier
##   sex score
## 1   1     5
## 2   2     4
## 3   1     3
## 4  NA     4
## 5   2     2
## 6   1    NA


#����ġ �����ϰ� �м�
outlier %>%
  filter(!is.na(sex) & !is.na(score)) %>%
  group_by(sex) %>%
  summarise(mean_score = mean(score))
## # A tibble: 2 x 2
##     sex mean_score
##   <dbl>      <dbl>
## 1     1          4
## 2     2          3


mpg = as.data.frame(ggplot2::mpg)
boxplot(mpg$hwy,horizontal = T, col =2)

summary(mpg$hwy)
IQR(mpg$hwy)

hist(mpg$hwy, probability = T)
lines(density(mpg$hwy),  col = 2  , type = 'h')

boxplot(mpg$hwy)$stats

mpg$hwy= ifelse(mpg$hwy<12 | mpg$hwy> 37 , NA , mpg$hwy)

kk = table(is.na(mpg$hwy))

tt= barplot(kk, col= rainbow(2), ylim = c(0,250))
text(tt,kk,label= paste0(kk,"��"), pos = 3)

mpg%>%
  group_by(drv)%>%
  summarise(mean_hwy = mean(hwy, na.rm = T))



# ȥ�ڼ� �غ���
# mpg �����͸� �̿��ؼ� �м� ������ �ذ��� ������.
# �켱 mpg �����͸� �ҷ��ͼ� �Ϻη� �̻�ġ�� ����ڽ��ϴ�. drv(�������) ������ ���� 4(�������), f(��������), r(�ķ�����) �� ������ �Ǿ��ֽ��ϴ�. �� ���� �࿡ ������ �� ���� �� k�� �Ҵ��ϰڽ��ϴ�. cty(���� ����) ������ �� ���� �࿡ �ش������� ũ�ų� ���� ���� �Ҵ��ϰڽ��ϴ�.
mpg <- as.data.frame(ggplot2::mpg)                  # mpg ������ �ҷ�����
mpg[c(10, 14, 58, 93), "drv"] <- "k"                # drv �̻�ġ �Ҵ�
mpg[c(29, 43, 129, 203), "cty"] <- c(3, 4, 39, 42)  # cty �̻�ġ �Ҵ�

# ȥ�ڼ� �غ���
# �̻�ġ�� ����ִ� mpg �����͸� Ȱ���ؼ� ������ �ذ��غ�����.
# ������ĺ��� ���� ���� �ٸ��� �˾ƺ����� �մϴ�. �м��� �Ϸ��� �켱 �� ������ �̻�ġ�� �ִ��� Ȯ���Ϸ��� �մϴ�.
# ???	Q1. drv�� �̻�ġ�� �ִ��� Ȯ���ϼ���. �̻�ġ�� ���� ó���� ���� �̻�ġ�� ��������� Ȯ���ϼ���. ���� ó�� �� ���� %in% ��ȣ�� Ȱ���ϼ���.
# ???	Q2. ���� �׸��� �̿��ؼ� cty�� �̻�ġ�� �ִ��� Ȯ���ϼ���. ���� �׸��� ���ġ�� �̿��� ���� ������ ��� ���� ���� ó���� �� �ٽ� ���� �׸��� ����� �̻�ġ�� ��������� Ȯ���ϼ���.
# ???	Q3. �� ������ �̻�ġ�� ����ó�� ������ ���� �м��� �����Դϴ�. �̻�ġ�� ������ ���� drv���� cty ����� ��� �ٸ��� �˾ƺ�����. �ϳ��� dplyr �������� ������ �մϴ�.

# ��Ʈ
# Q1. drv�� �������� ���̸� ���� ���� �����ϰ� �׷��� ������ NA�� �ο��ϴ� �ڵ带 �ۼ��ϸ� �˴ϴ�. �������� ���� ���� �� ������ %in%�� c()�� ������ �ڵ带 �����ϰ� ����� ������.
# Q2. ���� �׸��� ���� �� ����ϴ� �ټ� ���� ���ġ�� ����� ���� ������ ������ ã������. �׷� ���� filter()�� �̿��� cty�� �� ������ ��� ��� NA�� �ο��ϸ� �̻�ġ�� ���� ó�� �˴ϴ�.
# Q3. filter()�� �̿��� drv�� cty�� ��� ����ġ�� �ƴ� �����͸� ������ �� ���ܺ� ����� ���ϸ� �˴ϴ�.

#Q1. drv�� �̻�ġ�� �ִ��� Ȯ���ϼ���. �̻�ġ�� ���� ó���� ���� �̻�ġ�� ��������� Ȯ���ϼ���. ���� ó�� �� ���� %in% ��ȣ�� Ȱ���ϼ���.
# �̻�ġ Ȯ��
table(mpg$drv)
## 
##   4   f   k   r 
## 100 106   4  24
# drv�� 4, f, r�̸� ���� �� ����, �� �� NA�Ҵ�
mpg$drv <- ifelse(mpg$drv %in% c("4", "f", "r"), mpg$drv, NA)

# �̻�ġ Ȯ��
table(mpg$drv)
## 
##   4   f   r 
## 100 106  24


#Q2. ���� �׸��� �̿��ؼ� cty�� �̻�ġ�� �ִ��� Ȯ���ϼ���. ���� �׸��� ���ġ�� �̿��� ���� ������ ��� ���� ���� ó���� �� �ٽ� ���� �׸��� ����� �̻�ġ�� ��������� Ȯ���ϼ���.
# ���� �׸� ���� �� ���ġ ����
boxplot(mpg$cty)$stats
##      [,1]
## [1,]    9
## [2,]   14
## [3,]   17
## [4,]   19
## [5,]   26
# 9~26 ����� NA �Ҵ�
mpg$cty <- ifelse(mpg$cty < 9 | mpg$cty > 26, NA, mpg$cty)

# ���� �׸� ����
boxplot(mpg$cty)

# Q3. �� ������ �̻�ġ�� ����ó�� ������ ���� �м��� �����Դϴ�. �̻�ġ�� ������ ���� drv���� cty ����� ��� �ٸ��� �˾ƺ�����. �ϳ��� dplyr �������� ������ �մϴ�.
mpg %>%
  filter(!is.na(drv) & !is.na(cty)) %>%  # ����ġ ����
  group_by(drv) %>%                      # drv�� �и�
  summarise(mean_hwy = mean(cty))        # cty ��� ���ϱ�
## # A tibble: 3 x 2
##     drv mean_hwy
##   <chr>    <dbl>
## 1     4 14.24742
## 2     f 19.47000
## 3     r 13.95833



# �����ϱ�
# # 1.����ġ �����ϱ�
# 
# # ����ġ Ȯ��
# table(is.na(df$score))
# 
# # ����ġ ����
# df_nomiss <- df %>% filter(!is.na(score))
# 
# # ���� ���� ���ÿ� ����ġ ����
# df_nomiss <- df %>% filter(!is.na(score) & !is.na(sex))
# 
# # �Լ��� ����ġ ���� ��� �̿��ϱ�
# mean(df$score, na.rm = T)
# exam %>% summarise(mean_math = mean(math, na.rm = T))

# 2.�̻�ġ �����ϱ�

# # �̻�ġ Ȯ��
# table(outlier$sex)
# 
# # ���� ó��
# outlier$sex <- ifelse(outlier$sex == 3, NA, outlier$sex)
# 
# # boxplot���� �ش�ġ ���� ã��
# boxplot(mpg$hwy)$stats
# 
# # �ش�ġ ���� ó��
# mpg$hwy <- ifelse(mpg$hwy < 12 | mpg$hwy > 37, NA, mpg$hwy)




df_mpg

install.packages("plotly")
library(plotly)



#1. ��� �����ϱ�
# x�� displ, y�� hwy�� ������ ��� ����
ggplot(data = mpg, aes(x = displ, y = hwy))

# 2. �׷��� �߰��ϱ�
# ��濡 ������ �߰�
ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point()

#3. �� ������ �����ϴ� ���� �߰��ϱ�
# x�� ���� 3~6���� ����
ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point() + xlim(3, 6)


#3. �� ������ �����ϴ� ���� �߰��ϱ�
# x�� ���� 3~6, y�� ���� 10~30���� ����
ggplot(data = mpg, aes(x = displ, y = hwy)) + 
  geom_point() + 
  xlim(3, 6) + 
  ylim(10, 30)
