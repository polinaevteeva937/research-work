library("lmtest")
library("GGally")
library("car")

data = Seatbelts
help(Seatbelts)

data
summary(data)

#построим зависимость, включающую несколько регрессоров и посмотрим, сможем ли мы объединить их в одну зависимость
model0 = lm(DriversKilled~law+kms+PetrolPrice, data)
model0
summary(model0)
vif(model0)
#vif у всех регрессоров < 2 - связи между регрессорами нет
#R-squared:  0.201
#p-value: 3.478e-09
#возможно law или kms лучше исключить из модели

model1 = lm(DriversKilled~law+kms, data)
model1
summary(model1)
#R-squared: 0.1416, зависимости нет

model2 = lm(DriversKilled~law+PetrolPrice, data)
model2
summary(model2)
#R-squared: 0.1866, зависимости нет 

model3 = lm(DriversKilled~kms+PetrolPrice, data)
model3
summary(model3)
#R-squared: 0.1844, зависимости нет

model4 = lm(law~kms+PetrolPrice, data)
model4
vif(model4)
#vif < 2, значит, сильной зависимости среди регрессоров нет

model5 = lm(kms~law+PetrolPrice, data)
model5
vif(model5)
#vif < 2, значит, сильной зависимости среди регрессоров нет

model6 = lm(PetrolPrice~law+kms, data)
model6
vif(model6)
#vif < 2, значит, сильной зависимости среди регрессоров нет

#введение в модели функций регрессоров
model7 = lm(DriversKilled~law + kms + PetrolPrice + I(log(PetrolPrice)) + I(log(kms)), data)
model7
summary(model7)
vif(model7)
#R-squared: 0.2205
#Есть точка только у I(log(PetrolPrice))              
#vif у law < 2, а у остальных регрессоров очень большой

model8 = lm(DriversKilled~law + kms + I(log(PetrolPrice)) + I(log(kms)), data)
model8
summary(model8)
vif(model8)
#R-squared: 0.2099
#Есть 3 звездочки только у I(log(PetrolPrice))              
#vif у law и I(log(PetrolPrice)) < 2, а у остальных регрессоров большой

model9 = lm(DriversKilled~law + I(log(PetrolPrice)) + I(log(kms)), data)
model9
summary(model9)
vif(model9)
#R-squared: 0.202
#Есть 3 звездочки у I(log(PetrolPrice)) и 1 звездочка у law             
#vif у всех регрессоров < 2

model10 = lm(DriversKilled~law + kms + PetrolPrice + I(log(PetrolPrice)), data)
model10
summary(model10)
vif(model10)
#R-squared: 0.2158
#Есть точка только у I(log(PetrolPrice)) и по 1 звездочке law и kms           
#vif у law и kms < 2, а у остальных регрессоров очень большой

model11 = lm(DriversKilled~law + kms + PetrolPrice + I(log(kms)), data)
model11
summary(model11)
vif(model11)
#R-squared: 0.2067
#Есть 3 звездочки у PetrolPrice         
#vif у law и PetrolPrice < 2, а у остальных регрессоров очень большой

#лучшая модель с логарифмом
model12 = lm(DriversKilled~law + kms + I(log(PetrolPrice)), data)
model12
summary(model12)
vif(model12)
#R-squared: 0.2044
#Есть 3 звездочки у I(log(PetrolPrice))       
#vif у всех регрессоров < 2, значит, зависимости нет

model13 = lm(DriversKilled~law + PetrolPrice + I(log(kms)), data)
model13
summary(model13)
vif(model13)
#R-squared: 0.1986
#Есть 3 звездочки PetrolPrice и 1 звездочка law       
#vif у всех регрессоров < 2, значит, зависимости нет

model14 = lm(DriversKilled~law + kms + PetrolPrice + I(PetrolPrice^2) + I(kms^2) + I(PetrolPrice*kms) + I(PetrolPrice*law) + I(law*kms), data)
model14
summary(model14)
vif(model14)
#R-squared: 0.2488
#vif у всех регрессоров очень большой - связь между регрессорами есть

model15 = lm(DriversKilled~kms + PetrolPrice + I(PetrolPrice^2) + I(kms^2) + I(PetrolPrice*kms) + I(PetrolPrice*law) + I(law*kms), data)
model15
summary(model15)
vif(model15)
#R-squared: 0.2466
#vif у всех регрессоров очень большой - связь между регрессорами есть

model16 = lm(DriversKilled~kms + I(PetrolPrice^2) + I(kms^2) + I(PetrolPrice*kms) + I(PetrolPrice*law) + I(law*kms), data)
model16
summary(model16)
vif(model16)
#R-squared: 0.207
#vif у всех регрессоров очень большой - связь между регрессорами есть

model17 = lm(DriversKilled~kms + I(PetrolPrice^2) + I(kms^2) + I(PetrolPrice*law) + I(law*kms), data)
model17
summary(model17)
vif(model17)
#R-squared: 0.2017
#vif у I(PetrolPrice^2) < 2, а у всех остальных регрессоров очень большой - связь между регрессорами есть

model18 = lm(DriversKilled~kms + I(PetrolPrice^2) + I(kms^2) + I(PetrolPrice*law), data)
model18
summary(model18)
vif(model18)
#R-squared: 0.2015
#vif у I(PetrolPrice^2) и I(PetrolPrice * law) < 2, а у всех остальных регрессоров очень большой - связь между регрессорами есть

model19 = lm(DriversKilled~kms + I(PetrolPrice^2) + I(PetrolPrice*law), data)
model19
summary(model19)
vif(model19)
#R-squared: 0.1972
#Есть 3 звездочки у I(PetrolPrice^2) и 1 звездочка у I(PetrolPrice * law)
#vif у регрессоров < 2

model20 = lm(DriversKilled~law + kms + PetrolPrice + I(PetrolPrice^2), data)
model20
summary(model20)
vif(model20)
#R-squared: 0.2149
#vif у law и kms < 2, у всех остальных регрессоров очень большой

model21 = lm(DriversKilled~law + kms + PetrolPrice + I(kms^2), data)
model21
summary(model21)
vif(model21)
#R-squared: 0.2051
#vif у law и PetrolPrice < 2, у всех остальных регрессоров очень большой

model22 = lm(DriversKilled~law + kms + PetrolPrice + I(kms*law), data)
model22
summary(model22)
vif(model22)
#R-squared: 0.2032
#vif у kms и PetrolPrice < 2, у всех остальных регрессоров очень большой

model23 = lm(DriversKilled~law + kms + PetrolPrice + I(kms*PetrolPrice), data)
model23
summary(model23)
vif(model23)
#R-squared: 0.211
#vif у kms < 2, у всех остальных регрессоров очень большой

model24 = lm(DriversKilled~law + kms + PetrolPrice + I(law*PetrolPrice), data)
model24
summary(model24)
vif(model24)
#R-squared: 0.201
#vif у kms и PetrolPrice < 2, у всех остальных регрессоров очень большой

model25 = lm(DriversKilled~law + kms + I(PetrolPrice^2), data)
model25
summary(model25)
vif(model25)
#R-squared: 0.1971
#vif у всех регрессоров < 2, значит, зависимости нет

#лучшая модель с введёнными произведениями пар регрессоров
model25 = lm(DriversKilled~law + PetrolPrice + I(kms^2), data)
model25
summary(model25)
vif(model25)
#R-squared: 0.2027
#vif у всех регрессоров < 2, значит, зависимости нет

model26 = lm(DriversKilled~kms + PetrolPrice + I(law^2), data)
model26
summary(model26)
vif(model26)
#R-squared: 0.201
#vif у всех регрессоров < 2, значит, зависимости нет

#сравним лучшие модели
model0 = lm(DriversKilled~law+kms+PetrolPrice, data)
model0
summary(model0)
vif(model0)
#R-squared:  0.201
#Есть 3 звездочки у PetrolPrice, 1 звездочка у law и точка kms 
#vif у всех регрессоров < 2, значит, зависимости нет

model12 = lm(DriversKilled~law + kms + I(log(PetrolPrice)), data)
model12
summary(model12)
vif(model12)
#R-squared: 0.2044
#Есть 3 звездочки у I(log(PetrolPrice)), по 1 точке у law и kms        
#vif у всех регрессоров < 2, значит, зависимости нет

model25 = lm(DriversKilled~law + PetrolPrice + I(kms^2), data)
model25
summary(model25)
vif(model25)
#R-squared: 0.2027
#Есть 3 звездочки у PetrolPrice, по 1 точке у law и I(kms^2) 
#vif у всех регрессоров < 2, значит, зависимости нет

#лучшая модель из всех
model12 = lm(DriversKilled~law + kms + I(log(PetrolPrice)), data)
model12
summary(model12)
vif(model12)
t_critical=qt(0.975,df=125-4)
t_critical
#law: B = -11.83; o = 6.006; t = 1.979764
#kms: B = -0.001; o = 0.0006; t = 1.979764
#I(log(PetrolPrice)): B = -59.14; o = 15.35; t = 1.979764

#Доверительный интервал law:
#B:[-11.83-(1.98*6), -11.83+(1.98*6)]
#B:[-23.71, 0.05]
#0 попадает в доверительный интервал
#этот коэффициент может быть равен 0, значит, регрессор практически не связан с обьясняемой переменной

#Доверительный интервал kms:
#B:[-0.001-(1.98*0), -0.001+(1.98*0)]
#B:[-0.001, 0.001]
#0 попадает в доверительный интервал
#этот коэффициент может быть равен 0, значит, регрессор практически не связан с обьясняемой переменной

#Доверительный интервал I(log(PetrolPrice)):
#B:[-59.14-(1.98*15.35), -59.14+(1.98*15.35)]
#B:[-89.533, -28.747]
#0 не попадает в доверительный интервал
#этот не коэффициент может быть равен 0

#[y-(Residual standard error: 22.82*1.98), y+(Residual standard error: 22.82*1.98)]
new.data = data.frame(law = 20, kms = 10, PetrolPrice = 10)
predict(model12, new.data, interval = "confidence")
#прогноз модели -364.4884
#[-617.2967, -111.6801]
#0 не попадает в доверительный интервал 
