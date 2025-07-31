install.packages("devtools")
devtools::install_github("bdemeshev/rlms")
install.packages("foreign")

library("lmtest")
library("dplyr")
library("GGally")
library("car")
library("sandwich")
library("rlms")

data <- rlms_read("r14i_os26b.sav")
glimpse(data2)
data2=select(data, jj13.2, jh5, j_marst, j_diplom, status, jj6.2, j_age, j_educ, jj1.1.2 )
data2= na.omit(data2)

#элементы нормализации

#зарплата
data2$jj13.2
sal = as.numeric(data2$jj13.2)
sal1 = as.character(data2$jj13.2)
sal2 = lapply(sal1, as.integer)
sal = as.numeric(unlist(sal2))
mean(sal)
data2["salary"]=(sal-mean(sal))/(sqrt(var(sal)))
data2["salary"]

#продолжительность рабочей недели
data2$jj6.2
work = as.numeric(data2$jj6.2)
work1 = as.character(data2$jj6.2)
work2 = lapply(work1, as.integer)
work = as.numeric(unlist(work2))
mean(work)
data2["workweek"]=(work-mean(work))/(sqrt(var(work)))
data2["workweek"]

#возраст
data2$j_age
age = as.numeric(data2$j_age)
age1 = as.character(data2$j_age)
age2 = lapply(age1, as.integer)
age = as.numeric(unlist(age2))
mean(age)
data2["age"]=(age-mean(age))/(sqrt(var(age)))
data2["age"]

#пол
data2["sex"]=data2$jh5
data2$sex = as.numeric(data2$sex)
data2$sex[which(data2$sex!='1')]<-0
data2["sex"]

#наличие высшего образования
data2["j_educ"] = data2$j_educ 
data2$j_educ = as.numeric(data2$j_educ)
data2["j_educ"] = lapply(data2$j_educ, as.character) 
data2$j_educ[which(data2$j_educ=='1')] <- 0
data2$j_educ[which(data2$j_educ=='21')] <- 1 
data2$j_educ[which(data2$j_educ=='22')] <- 1 
data2$j_educ[which(data2$j_educ=='23')] <- 1
data2$j_educ[which(data2$j_educ!='1')] <- 0
data2$j_educ

#населенный пункт 
data2["status2"]=data2$status 
data2$status2 = as.numeric(data2$status2)
data2$status2[which(data2$status2=='1')] <- 1 
data2$status2[which(data2$status2=='2')] <- 1 
data2$status2[which(data2$status2!='1')] <- 0
data2["status2"]

#семейное положение
data2["wed1"]= data2$j_marst 
data2$wed1 = as.numeric(data2$wed1)
data2["wed1"] = lapply(data2$j_marst, as.character) 
data2$wed1[which(data2$wed1=='1')] <- 0
data2$wed1[which(data2$wed1=='2')] <- 1 
data2$wed1[which(data2$wed1=='6')] <- 1
data2$wed1[which(data2$wed1!='1')] <- 0
data2$wed1

data2["wed2"]= data2$j_marst 
data2$wed2 = as.numeric(data2$wed2)
data2$wed2[which(data2$wed2=='1')] <- 0
data2$wed2[which(data2$wed2=='4')] <- 1 
data2$wed2[which(data2$wed2=='5')] <- 1
data2$wed2[which(data2$wed2!='1')] <- 0
data2$wed2

data2["wed3"]= data2$j_marst 
data2$wed3 = as.numeric(data2$wed3)
data2$wed3[which(data2$wed3!='1')] <- 0
data2$wed3

#удовлетворенность условиями труда
data2["satisfy1"] = data2$jj1.1.2 
data2$satisfy1 = as.numeric(data2$satisfy1) 
data2$satisfy1[which(data2$satisfy1=="1")] <- 1 
data2$satisfy1[which(data2$satisfy1=="2")] <- 1
data2$satisfy1[which(data2$satisfy1!="1")] <- 0
data2$satisfy1

data2["satisfy2"] = data2$jj1.1.2 
data2$satisfy2 = as.numeric(data2$satisfy2) 
data2$satisfy2[which(data2$satisfy2=="1")] <- 0
data2$satisfy2[which(data2$satisfy2=="4")] <- 1 
data2$satisfy2[which(data2$satisfy2=="5")] <- 1
data2$satisfy2[which(data2$satisfy2!="1")] <- 0
data2$satisfy2

#линейная регрессия зарплаты на все параметры
model1 =lm(data = data2, salary ~ workweek + age + sex + j_educ + status2 + wed1 + wed2 + wed3 + satisfy1 + satisfy2) 
summary(model1) 
vif(model1)
#коэффициент вздутия дисперсии VIF для всех регрессоров хороший
#R-squared: 0.1827 

#введем степени

model2 =lm(data = data2, salary ~ workweek + age + sex + j_educ + status2 + wed1 + wed2 + wed3 + satisfy1 + satisfy2 + I(age^(0.1))) 
summary(model2) 
vif(model2)
# R-squared: 0.2181 - повысился, vif приемлемый

model3 =lm(data = data2, salary ~ workweek + age + sex + j_educ + status2 + wed1 + wed2 + wed3 + satisfy1 + satisfy2 + I(workweek^(0.1))) 
summary(model3) 
vif(model3)
# R-squared снизился, но не сильно, звёздочек у доп регрессоров не много, vif приемлемый

model4 =lm(data = data2, salary ~ workweek + age + sex + j_educ + status2 + wed1 + wed2 + wed3 + satisfy1 + satisfy2 + I(workweek^(0.1)) + I(age^(0.1))) 
summary(model4) 
vif(model4)
# R-squared снизился, но не сильно, звёздочек у доп регрессоров не много, vif приемлемый

model5 =lm(data = data2, salary ~ workweek + age + sex + j_educ + status2 + wed1 + wed2 + wed3 + satisfy1 + satisfy2 + I(workweek^(0.2))) 
summary(model5) 
vif(model5)
# R-squared снизился, но не сильно, звёздочек у доп регрессоров не много, vif приемлемый

model6 =lm(data = data2, salary ~ workweek + age + sex + j_educ + status2 + wed1 + wed2 + wed3 + satisfy1 + satisfy2 + I(age^(0.2))) 
summary(model6) 
vif(model6)
# R-squared: 0.2182 - повысился, vif приемлемый

model7 =lm(data = data2, salary ~ workweek + age + sex + j_educ + status2 + wed1 + wed2 + wed3 + satisfy1 + satisfy2 + I(workweek^(0.2)) + I(age^(0.2))) 
summary(model7) 
vif(model7)
# R-squared снизился, но не сильно, звёздочек у доп регрессоров не много, vif приемлемый

model8 =lm(data = data2, salary ~ workweek + age + sex + j_educ + status2 + wed1 + wed2 + wed3 + satisfy1 + satisfy2 + I(workweek^(0.3))) 
summary(model8) 
vif(model8)
# R-squared снизился, vif приближается к 10

model9 =lm(data = data2, salary ~ workweek + age + sex + j_educ + status2 + wed1 + wed2 + wed3 + satisfy1 + satisfy2 + I(age^(0.3))) 
summary(model9) 
vif(model9)
# R-squared: 0.2183 - повысился, vif приемлемый

model10 =lm(data = data2, salary ~ workweek + age + sex + j_educ + status2 + wed1 + wed2 + wed3 + satisfy1 + satisfy2 + I(workweek^(0.3)) + I(age^(0.3))) 
summary(model10) 
vif(model10)
# R-squared снизился, но не сильно, звёздочек у доп регрессоров не много, vif приближается к 10

model11 =lm(data = data2, salary ~ workweek + age + sex + j_educ + status2 + wed1 + wed2 + wed3 + satisfy1 + satisfy2 + I(workweek^(0.4))) 
summary(model11) 
vif(model11)
# R-squared снизился, но не сильно, vif > 10

#лучшая модель среди степеней
model12 =lm(data = data2, salary ~ workweek + age + sex + j_educ + status2 + wed1 + wed2 + wed3 + satisfy1 + satisfy2 + I(age^(0.4))) 
summary(model12) 
vif(model12)
# R-squared: 0.2184 - повысился, но vif приближается к 10

model13 =lm(data = data2, salary ~ workweek + age + sex + j_educ + status2 + wed1 + wed2 + wed3 + satisfy1 + satisfy2 + I(workweek^(0.4)) + I(age^(0.4))) 
summary(model13) 
vif(model13)
# R-squared снизился, vif > 10, увеличивать, дальше, скорее всего, нет смысла

model14 =lm(data = data2, salary ~ workweek + age + sex + j_educ + status2 + wed1 + wed2 + wed3 + satisfy1 + satisfy2 + I(workweek^(0.5))) 
summary(model14) 
vif(model14)
# R-squared снизился, vif большой

model15 =lm(data = data2, salary ~ workweek + age + sex + j_educ + status2 + wed1 + wed2 + wed3 + satisfy1 + satisfy2 + I(age^(0.5))) 
summary(model15) 
vif(model15)
# R-squared: 0.2185 - повысился, но vif > 10

model16 =lm(data = data2, salary ~ workweek + age + sex + j_educ + status2 + wed1 + wed2 + wed3 + satisfy1 + satisfy2 + I(workweek^(0.5)) + I(age^(0.5))) 
summary(model16) 
vif(model16)
# R-squared снизился, vif большой

#значит, для степеней 0<n< 1: чем ближе степень к единице, тем хуже становится vif. Начиная со степени 0.4 vif уже
#начиная со степени 0.4 vif очень большим, модели становятся только хуже

model17 =lm(data = data2, salary ~ workweek + age + sex + j_educ + status2 + wed1 + wed2 + wed3 + satisfy1 + satisfy2 + I(workweek^(1.1))) 
summary(model17) 
vif(model17)
# R-squared снизился, но не сильно, vif очень большой

model18 =lm(data = data2, salary ~ workweek + age + sex + j_educ + status2 + wed1 + wed2 + wed3 + satisfy1 + satisfy2 + I(age^(1.1))) 
summary(model18) 
vif(model18)
# R-squared: 0.2191 - повысился, но vif очень большой

model19 =lm(data = data2, salary ~ workweek + age + sex + j_educ + status2 + wed1 + wed2 + wed3 + satisfy1 + satisfy2 + I(workweek^(1.1)) + I(age^(1.1))) 
summary(model19) 
vif(model19)
# R-squared снизился, vif очень большой

model20 =lm(data = data2, salary ~ workweek + age + sex + j_educ + status2 + wed1 + wed2 + wed3 + satisfy1 + satisfy2 + I(workweek^(1.2))) 
summary(model20) 
vif(model20)
# R-squared снизился, но не сильно, vif очень большой

model21 =lm(data = data2, salary ~ workweek + age + sex + j_educ + status2 + wed1 + wed2 + wed3 + satisfy1 + satisfy2 + I(age^(1.2))) 
summary(model21) 
vif(model21)
# R-squared: 0.2192 - повысился, но vif очень большой

model22 =lm(data = data2, salary ~ workweek + age + sex + j_educ + status2 + wed1 + wed2 + wed3 + satisfy1 + satisfy2 + I(workweek^(1.2)) + I(age^(1.2))) 
summary(model22) 
vif(model22)
# R-squared снизился, vif очень большой

model23 =lm(data = data2, salary ~ workweek + age + sex + j_educ + status2 + wed1 + wed2 + wed3 + satisfy1 + satisfy2 + I(workweek^(1.3))) 
summary(model23) 
vif(model23)
# R-squared снизился, vif большой, но по сравнению с прошлой моделью уменьшился

model24 =lm(data = data2, salary ~ workweek + age + sex + j_educ + status2 + wed1 + wed2 + wed3 + satisfy1 + satisfy2 + I(age^(1.3))) 
summary(model24) 
vif(model24)
# R-squared: 0.2192 - повысился, vif большой, но по сравнению с прошлой моделью уменьшился

model25 =lm(data = data2, salary ~ workweek + age + sex + j_educ + status2 + wed1 + wed2 + wed3 + satisfy1 + satisfy2 + I(workweek^(1.3)) + I(age^(1.3))) 
summary(model25) 
vif(model25)
# R-squared снизился, vif очень большой, но по сравнению с прошлой моделью уменьшился

model26 =lm(data = data2, salary ~ workweek + age + sex + j_educ + status2 + wed1 + wed2 + wed3 + satisfy1 + satisfy2 + I(workweek^(1.4))) 
summary(model26) 
vif(model26)
# R-squared снизился, но не сильно, vif большой, но снижается

model27 =lm(data = data2, salary ~ workweek + age + sex + j_educ + status2 + wed1 + wed2 + wed3 + satisfy1 + satisfy2 + I(age^(1.4))) 
summary(model27) 
vif(model27)
# R-squared: 0.2193 - повысился, но vif большой

model28 =lm(data = data2, salary ~ workweek + age + sex + j_educ + status2 + wed1 + wed2 + wed3 + satisfy1 + satisfy2 + I(workweek^(1.4)) + I(age^(1.4))) 
summary(model28) 
vif(model28)
# R-squared снизился, vif большой, снижается

model29 =lm(data = data2, salary ~ workweek + age + sex + j_educ + status2 + wed1 + wed2 + wed3 + satisfy1 + satisfy2 + I(workweek^(1.9))) 
summary(model29) 
vif(model29)
# R-squared снизился, vif приемлемый

model30 =lm(data = data2, salary ~ workweek + age + sex + j_educ + status2 + wed1 + wed2 + wed3 + satisfy1 + satisfy2 + I(age^(2))) 
summary(model30) 
vif(model30)
# R-squared: 0.1949 - понизился, но vif приемлемый

model31 =lm(data = data2, salary ~ workweek + age + sex + j_educ + status2 + wed1 + wed2 + wed3 + satisfy1 + satisfy2 + I(workweek^(1.9)) + I(age^(1.9))) 
summary(model31) 
vif(model31)
# R-squared снизился, vif > 10

model32 =lm(data = data2, salary ~ workweek + age + sex + j_educ + status2 + wed1 + wed2 + wed3 + satisfy1 + satisfy2 + I(workweek^(2)) + I(age^(2))) 
summary(model32) 
vif(model32)
# R-squared снизился, vif в норме

#лучшая модель среди степеней
model12 =lm(data = data2, salary ~ workweek + age + sex + j_educ + status2 + wed1 + wed2 + wed3 + satisfy1 + satisfy2 + I(age^(0.4))) 
summary(model12) 
vif(model12)
# R-squared: 0.2184 - выше первоначальной модели, vif < 10

#введем логарифмы

model33 =lm(data = data2, salary ~ workweek + age + sex + j_educ + status2 + wed1 + wed2 + wed3 + satisfy1 + satisfy2 + I(log(workweek)) + I(log(age))) 
summary(model33) 
vif(model33)
# R-squared:  0.1733 снизился, vif приемлемый

model34 =lm(data = data2, salary ~ workweek + age + sex + j_educ + status2 + wed1 + wed2 + wed3 + satisfy1 + satisfy2 + I(log(workweek))) 
summary(model34) 
vif(model34)
# R-squared:  0.1564 снизился, vif приемлемый

#лучшая среди логарифмов 
model35 =lm(data = data2, salary ~ workweek + age + sex + j_educ + status2 + wed1 + wed2 + wed3 + satisfy1 + satisfy2 + I(log(age))) 
summary(model35)
vif(model35)
# R-squared:  0.218, vif приемлемый

#сравнение лучших моделей

model1 =lm(data = data2, salary ~ workweek + age + sex + j_educ + status2 + wed1 + wed2 + wed3 + satisfy1 + satisfy2) 
summary(model1) 
vif(model1)
#Multiple R-squared:  0.1855,	Adjusted R-squared:  0.1827 
#попробуем убрать satisfy2
#vif хороший у всех регрессоров

model1.1 =lm(data = data2, salary ~ workweek + age + sex + j_educ + status2 + wed1+ wed2 + wed3 + satisfy1) 
summary(model1.1) 
vif(model1.1)
#Multiple R-squared:  0.1842,	Adjusted R-squared:  0.1817  - снизился, но не сильно
#vif хороший у всех регрессоров

model6 =lm(data = data2, salary ~ workweek + age + sex + j_educ + status2 + wed1 + wed2 + wed3 + satisfy1 + satisfy2 + I(age^(0.2))) 
summary(model6) 
vif(model6)
#Multiple R-squared:  0.2237,	Adjusted R-squared:  0.2182  - выше, чем у предыдущей модели
#vif у всех регрессоров < 5
#попробуем убрать satisfy2

model6.1 =lm(data = data2, salary ~ workweek + age + sex + j_educ + status2 + wed1 + wed2 + wed3 + satisfy1 + I(age^(0.2)))
summary(model6.1) 
vif(model6.1)
#Multiple R-squared:  0.222,	Adjusted R-squared:  0.2169  - незначительно сниился по сравнению с предыдущей моделью 6
#vif у всех регрессоров < 5

model12 =lm(data = data2, salary ~ workweek + age + sex + j_educ + status2 + wed1 + wed2 + wed3 + satisfy1 + satisfy2 + I(age^(0.4))) 
summary(model12) 
vif(model12)
# R-squared: 0.224, но vif приближается к 10
#Multiple R-squared:  0.224,	Adjusted R-squared:  0.2184 - повысился относительно модели 1
#попробуем убрать satisfy2
#vif увеличился у регрессоров age и I(age^(0.4))

model12.1 =lm(data = data2, salary ~ workweek + age + sex + j_educ + status2 + + wed1 + wed2 + wed3+ satisfy1 + I(age^(0.4))) 
summary(model12.1) 
vif(model12.1)
#Multiple R-squared:  0.2222,	Adjusted R-squared:  0.2171 - незначительно снизился относительно модели 12 
#почти у всех регрессоров по 3 звездочки

model35 =lm(data = data2, salary ~ workweek + age + sex + j_educ + status2 + wed1 + wed2 + wed3 + satisfy1 + satisfy2 + I(log(age))) 
summary(model35)
vif(model35)
#Multiple R-squared:  0.2236,	Adjusted R-squared:  0.218 
#попробуем убрать satisfy2
#у доп регрессоров мало звёздочек
#vif приемлемый

model35.1 =lm(data = data2, salary ~ workweek + age + sex + j_educ + status2 +  wed1 + wed2 + wed3 + satisfy1 + I(log(age))) 
summary(model35.1)
vif(model35.1)
# R-squared:  0.218, vif приемлемый
#Multiple R-squared:  0.2218,	Adjusted R-squared:  0.2167  - незначительно снизился относительно модели 35
#почти у всех регрессоров по 3 звездочки
#vif приемлемый

#лучшая модель, поскольку vif у всех регрессоров приемлемый, почти у всех регрессоров по 3 звездочки, и Adjusted R-squared:  0.2169 - один из самых высоких в представленных моделях
model6.1 =lm(data = data2, salary ~ workweek + age + sex + j_educ + status2 + wed1 + wed2 + wed3 + satisfy1 + I(age^(0.2)))
summary(model6.1) 
vif(model6.1)

#мужчины, разведённые с высшим образованием 
data3=subset(data2,sex==1)
data3

data4=subset(data3,wed2 == 1)
data4

data5=subset(data4,j_educ==1)
data5

model_subset1 =lm(data = data5, salary ~ workweek + age + status2 + satisfy1 + I(age^(0.2)))
summary(model_subset1) 
vif(model_subset1)

#женщины, живущие в городе, состоящие в браке
data6=subset(data2,sex==0)
data6

data7=subset(data6,status2==1)
data7

data8=subset(data7,wed1==1)
data8

model_subset2 =lm(data = data8, salary ~ workweek + age + j_educ + satisfy1 + I(age^(0.2)))
summary(model_subset2) 
vif(model_subset2)