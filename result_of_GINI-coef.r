setwd('C://Users//user//Desktop')
m = read.csv('C://Users//user//Desktop//분기자료(2019~) - 전체가구_1인이상_2019_20210127_25862.csv')

#1단계
income = m$소득 - m$사회적현물이전 - m$가구간이전 - m$할인혜택 - m$기타이전소득
non_con_exp = m$비소비지출 - m$비경상조세 - m$이자비용 - m$가구간.이전지출 - m$비영리단체로.이전
dincome = income - non_con_exp
equal_dincome = dincome / sqrt(m$가구원수)
social_feature = m$이전소득 - m$공적연금 - m$연말정산.환급금 - m$가구간이전 - m$할인혜택 - m$기타이전소득


#행렬화
dincome = matrix(dincome)
social_feature = matrix(social_feature)

dincome_minus_social = matrix(nrow = 20884)
under_30_social = matrix(nrow = 20884)
count_upper = 0
count_under = 0

for (i in 1: 20884){
  if (social_feature[i,] >= 300000){
    dincome_minus_social[i,] = dincome[i,] - 300000
    count_upper = count_upper + 1
  } else {
    dincome_minus_social[i,] = dincome[i,] - social_feature[i,]
    under_30_social[i,] = social_feature[i,]
    count_under = count_under + 1
  }
}

dincome_minus_social = replace(dincome_minus_social, dincome_minus_social < 0, 0)
sum(dincome_minus_social) #66772374729
#sum(dincome_minus_social < 0) #0개
count_upper #4848
count_under #16036

#사회 보장 명목 전체 세원
tax_social_feature = 300000 * count_upper + sum(under_30_social, na.rm = T) #2284341221
bincome_social = tax_social_feature / 20884 #109382.4

reduction = 200000

object = sum(equal_dincome * 0.1) / 20884

bincome_total_step1 = reduction + bincome_social

real_dincome_step1 = dincome_minus_social / sqrt(m$가구원수)

equal_bincome_step1 = (real_dincome_step1 + bincome_total_step1)


###1단계 지니계수(기존과 비교)
#install.packages("dineq")
library(dineq)
gini.wtd(equal_dincome, weights = m$가구원수 * m$가중치.weight.) #0.3391352
gini.wtd(equal_bincome_step1, weights = m$가구원수 * m$가중치.weight.) #0.3139927


#===================================================

###목적세(1단계 안하고 그냥 기존과 비교)
object = equal_dincome * 0.1
sum_object = sum(object) / 20884 #216914.3
object_bincome = equal_dincome - object + sum_object

gini.wtd(equal_dincome, weights = m$가구원수 * m$가중치.weight.) #0.3391352
gini.wtd(object_bincome, weights = m$가구원수 * m$가중치.weight.) #0.308916

###목적세 1단계 이후로 시행했을 때
bincome_total_step3 = bincome_total_step1 + sum_object #526296.7
real_dincome_step3 = real_dincome_step1 - object
equal_bincome_step3 = (real_dincome_step3 + bincome_total_step3)

gini.wtd(equal_dincome, weights = m$가구원수 * m$가중치.weight.) #기존 0.3391352
gini.wtd(equal_bincome_step1, weights = m$가구원수 * m$가중치.weight.) #1단계 0.3139927
gini.wtd(equal_bincome_step3, weights = m$가구원수 * m$가중치.weight.) #3단계 0.2863577
