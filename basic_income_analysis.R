getwd()
setwd('I:\\RDC21011904(이지원)')
m = read.csv('I:\\RDC21011904(이지원)\\210127_추가신청2\\gd_qj2019.csv')

# elec
# 전기 (기타계절)
f_ect = function(m_elec) {
  if (m_elec <= 17690){
    x = (m_elec / 106.0821)+33.1189711
  } else if (m_elec <= 65760){
    x = (m_elec / 213.6423) + 92.1766897
  } else if (m_elec <= 263670) {
    x = (m_elec / 319.0422) + 173.556664
  } else {
    x = (m_elec / 806.7015) + 673.150106
  }
}

# 전기 (하계)
f_summer = function(m_elec){
  if (m_elec <= 17690){
    x = (m_elec / 106.0821)+33.1189711
  } else if (m_elec <= 32850){
    x = (m_elec / 106.0821)-9.753483
  } else if (m_elec <= 65680){
    x = (m_elec / 213.6423) + 142.522618
  } else if (m_elec <= 247630) {
    x = (m_elec / 319.0422) + 223.78831
  } else {
    x = (m_elec / 806.7015) + 693.01621
  }
}

co2_elec = vector()
# 전기 전체 (3분기 보완)
for (i in 20884){
  if (m$C1 == 201934){
    co2_elec = f_summer(m$C354)
  } else {
    co2_elec = f_ect(m$C354)
  }
}
# sum(is.na(co2_elec))

# 탄소 사용량
co2_elec = co2_elec * 0.000466

# 휘발유
m_cgas = m$C453
co2_cgas = (m_cgas / 1471.89) * 0.00212

# 경유
m_coil = m$C454
co2_coil = (m_coil / 1340.1) * 0.00259

#LPG
m_clpg = m$C455
co2_clpg = (m_clpg / 806.24) * 0.00289

#도시가스
m_hgas = m$C355
Q = ((m_hgas / 1.1) - 1000)/ (0.9996*42.596*15.34)

# 도시가스 사용량 0인 경우 0으로 대체
Q = replace(Q, Q < 0, 0)
Q = matrix(Q)
Q[267,] # lpg 쓰는 항목
class(Q) # matrix

# LPG로 대체 
m_hlpg = m$C356 / 1061.98 # lpg 금액 -> 사용량 변환 공식
co2_hlpg = m_hlpg * 1.105 * 0.713 # 연료 사용량 -> 탄소배출량 (kg) 변환 공식
co2_hlpg = matrix(co2_hlpg)

for (i in 20884){
  Q = replace(Q, Q == 0, co2_hlpg[i, ]) # 0인 항목에 대해 lpg 탄소 배출량으로 대체
}

# co2_hgas = Q * 1.055/1000*0.6374*3.667
co2_hgas_update = Q * 0.0021783
# df_hgas = nrow(m[!(m$C355 == 0),])

# 수도
m_water = m$C347
if (m_water <= 28980){
  x = (m_water - 1080) / 930
} else if (m_water <= 61980){
  x = (m_Water + 20520)/ 1650
} else {
  x = (m_water + 57020) / 2380
}

co2_water = 0.000332 * x


# 탄소사용량 항목별 데이터프레임 (도시가스 lpg 변환 완료)
df = data.frame(co2_elec, co2_cgas, co2_coil, co2_clpg ,co2_water, co2_hgas_update)
View(df)


# 항목별 탄소사용량 전체 sum
sum_co2 = apply(df, 1, sum)

#균등화 적용 
equal_sum_co2 = sum_co2 / sqrt(m$C7)
res = data.frame(equal_sum_co2, sum_co2)
View(res)
df[,'equal_sum_co2'] = equal_sum_co2
df[,'sum_co2'] = sum_co2
View(df)

# 탄소 세원 (균등화 전)
# tax_co2 = sum_co2 * 80000
# sum_tax_co2 = sum(tax_co2) # (비례식)

tax_co2_equal = equal_sum_co2 * 80000
sum_tax_co2_equal = sum(tax_co2_equal) # 5억 (균등화) # lpg 추가 완료
sum_tax_co2_equal / 20884


# 항목별 탄소 사용량 sum
sum_elec = apply(df[1], 2, sum)
sum_gas = apply(df[2], 2, sum)
sum_oil = apply(df[3], 2, sum)
sum_lpg = apply(df[4], 2, sum)
sum_water = apply(df[5], 2, sum)
sum_hgas = apply(df[6], 2, sum)

equal_sum_elec = apply(df[1] / sqrt(m$C7), 2, sum)
equal_sum_gas = apply(df[2]/ sqrt(m$C7), 2, sum)
equal_sum_oil = apply(df[3]/ sqrt(m$C7), 2, sum)
equal_sum_lpg = apply(df[4]/ sqrt(m$C7), 2, sum) 
equal_sum_water = apply(df[5]/ sqrt(m$C7), 2, sum)
equal_sum_hgas = apply(df[6]/ sqrt(m$C7), 2, sum) 


# 항목별 탄소 사용량 sum 데이터프레임
df_sum = data.frame()
df_sum = data.frame(sum_elec, sum_gas, sum_oil, sum_lpg, sum_hgas, sum_water, sum(sum_co2))
View(df_sum)

df_equal_sum = data.frame()
df_equal_sum = data.frame(equal_sum_elec, equal_sum_gas, equal_sum_oil, equal_sum_lpg, equal_sum_water, equal_sum_hgas, sum(equal_sum_co2))
View(df_equal_sum)


# 경상소득
income = m$C127 - m$C154 - m$C156 - m$C157 - m$C158 
non_con_exp = m$C633 - m$C640 - m$C652 - m$C655 -m$C662
dincome = income - non_con_exp # 가처분소득 
equal_dincome = dincome / sqrt(m$C7)

real_dincome = equal_dincome - tax_co2
df_income = data.frame(real_dincome)

#install.packages('dineq')
library(dineq)

# 기업 부문 탄소 세원
company_co2_tax = 50162840480000 / 50000000 * 20884 # 월 단위 기업 부문 탄소세원
total_co2_tax = company_co2_tax + sum_tax_co2_equal * 12 
co2_tax_income = total_co2_tax / 12 / 20884

#============================사회보장 part for-if 문 다시 돌리고 싶을 때 (한 번에 실행)=====================
# 이전소득 중 (기초연금 + 사회수혜금 + 사회적현물이전)
social_feature = m$C150 - m$C151 - m$C155 - m$C156 - m$C157 - m$C158
# mean(social_feature) # 아예 없앴을 때 1인당 받을 수 있는 액수
# m$C153 %>% filter(m$C153 != 0)

# 30만원을 기준금액으로 30 or 총액 걷어들이기
# 행렬로 만들기

income = m$C127 - m$C154 - m$C156 - m$C157 - m$C158 
non_con_exp = m$C633 - m$C640 - m$C652 - m$C655 -m$C662
dincome = income - non_con_exp # 가처분소득 
equal_dincome = dincome / sqrt(m$C7)
# 가구원 수 반영 (균등화 가처분소득)

# 행렬화
dincome = matrix(dincome)
social_feature = matrix(social_feature)
# dincome_minus_social = data.frame(dincome, social_feature)
# social_feature[30,]
# dincome[30,]

# dincome_minus_social -> 가처분소득에서 사회보장 명목 30만원 or 전부 뺏은 것
dincome_minus_social = matrix(nrow = 20884)
under_30_social = matrix(nrow = 20884)
count_upper = 0
count_under = 0

for (i in 1:20884){
  if (social_feature[i, ] >= 300000){
    dincome_minus_social[i, ] = dincome[i, ] - 300000
    count_upper = count_upper + 1
  } else {
    dincome_minus_social[i, ] = dincome[i, ] - social_feature[i, ]
    under_30_social[i, ] = social_feature[i, ]
    count_under = count_under + 1
  }
}

#class(dincome_minus_social)
dincome_minus_social = replace(dincome_minus_social, dincome_minus_social < 0, 0)
# sum(dincome_minus_social < 0) # 0개
count_upper # 4848
count_under # 16036

# 사회 보장 명목 전체 세원
tax_social_feature = 300000 * count_upper + sum(under_30_social, na.rm = T) # 829941221
bincome_social = tax_social_feature / 20884 #109382.4

# 감면 추가 (1인당)
reduction = 200000 # 7만원 + 13만원

# 목적세 
object = sum(equal_dincome * 0.1) / 20884

bincome_total_step1 = reduction + bincome_social
bincome_total_step2 = reduction + bincome_social + co2_tax_income
bincome_total_step3 = reduction + bincome_social + co2_tax_income + object

#install.packages('dineq')
library(dineq)

tax_co2 = matrix(tax_co2)
# 균등화된 세수 제거 후 소득
real_dincome_step1 = dincome_minus_social / sqrt(m$C7)
real_dincome_step2 = real_dincome_step1 - tax_co2_equal
real_dincome_step3 = real_dincome_step2 - object

# df_income = data.frame(real_dincome)

# 균등화 가처분소득 (제곱근 지수 활용)
equal_bincome_step1 = (real_dincome_step1 + bincome_total_step1)
equal_bincome_step2 = (real_dincome_step2 + bincome_total_step2)
equal_bincome_step3 = (real_dincome_step3 + bincome_total_step3)

#==============================
sum(equal_bincome_step2)
sum(equal_bincome_step3)

sum(real_dincome_step2)
sum(real_dincome_step3)
sum(bincome_total_step2)
sum(bincome_total_step3)

# equal_dincome = (real_dincome + co2_tax_income)

# 불평등도
# 1단계 기본소득 지급 후 불평등도
gini.wtd(equal_dincome, weights = m$C7 * m$C113) # 기존 가처분소득 불평등도
gini.wtd(equal_bincome_step1, weights = m$C7 * m$C113)

# 2단계 기본소득 지급 후 불평등도
gini.wtd(equal_bincome_step2, weights = m$C7 * m$C113)

# 3단계 기본소득 지급 후 불평등도
gini.wtd(equal_bincome_step3, weights = m$C7 * m$C113)

#===========================목적세 조정안 (5~10%) -> 10%로 최종 결정정=====================

# 목적세 (시나리오별)
object_5 = sum(equal_dincome * 0.05) / 20884
object_7 = sum(equal_dincome * 0.07) / 20884
object_10 = sum(equal_dincome * 0.1) / 20884

# 3단계 가처분소득 (시나리오별)
bincome_total_step3_5 = reduction + bincome_social + co2_tax_income + object_5
bincome_total_step3_7 = reduction + bincome_social + co2_tax_income + object_7
bincome_total_step3_10 = reduction + bincome_social + co2_tax_income + object_10

# 
real_dincome_step3_5 = real_dincome_step2 - object_5
real_dincome_step3_7 = real_dincome_step2 - object_7
real_dincome_step3_10 = real_dincome_step2 - object_10


# 균등화 가처분 소득 (시나리오별)
equal_bincome_step3_5 = (real_dincome_step3 + bincome_total_step3_5)
equal_bincome_step3_7 = (real_dincome_step3 + bincome_total_step3_7)
equal_bincome_step3_10 = (real_dincome_step3 + bincome_total_step3_10)


# 3단계 기본소득 지급 후 불평등도 (시나리오별)
gini.wtd(equal_dincome, weights = m$C7 * m$C113) # 기존 가처분소득 불평등도
gini.wtd(equal_bincome_step3_5, weights = m$C7 * m$C113)
gini.wtd(equal_bincome_step3_7, weights = m$C7 * m$C113)
gini.wtd(equal_bincome_step3_10, weights = m$C7 * m$C113)



#====================================================================
#install.packages('dineq')
library(dineq)

tax_co2 = matrix(tax_co2)
# 균등화된 사회보장 제거 후 소득
real_dincome_step1 = dincome_minus_social / sqrt(m$C7)
real_dincome_step2 = real_dincome_step1 - tax_co2_equal
real_dincome_step3 = real_dincome_step2 - object

# df_income = data.frame(real_dincome)

# 균등화 가처분소득 (제곱근 지수 활용)
equal_bincome_step1 = (real_dincome_step1 + bincome_total_step1)
equal_bincome_step2 = (real_dincome_step2 + bincome_total_step2)
equal_bincome_step3 = (real_dincome_step3 + bincome_total_step3)

# equal_dincome = (real_dincome + co2_tax_income)

# 불평등도
# 1단계 기본소득 지급 후 불평등도
gini.wtd(equal_dincome, weights = m$C7 * m$C113) # 기존 가처분소득 불평등도
gini.wtd(equal_bincome_step1, weights = m$C7 * m$C113)

# 2단계 기본소득 지급 후 불평등도
gini.wtd(equal_dincome, weights = m$C7 * m$C113) # 기존 가처분소득 불평등도
gini.wtd(equal_bincome_step2, weights = m$C7 * m$C113)

# 3단계 기본소득 지급 후 불평등도
gini.wtd(equal_dincome, weights = m$C7 * m$C113) # 기존 가처분소득 불평등도
gini.wtd(equal_bincome_step3, weights = m$C7 * m$C113)