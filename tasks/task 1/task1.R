library("lmtest")

data = swiss

data

#Задание 1

#Вычисление среднего значения для Examination, Education, Agriculture
mean(data$Examination)
mean(data$Education)
mean(data$Agriculture)

#Вычисление дисперсии для Examination, Education, Agriculture
var(data$Examination)
var(data$Education)
var(data$Agriculture)

#вычисление СКО для Examination, Education, Agriculture
sqrt(var(data$Examination))
sqrt(var(data$Education))
sqrt(var(data$Agriculture))

#Задание 2
 
#Построение зависимости вида y = a*x + b, где у - Examination (объясняемая переменная), х - Education (регрессор)
model1 = lm(Examination~Education, data)
model1 

plot(data$Examination~data$Education) + abline(a = 10.13, b = 0.58, col = "red")

#Построение зависимости вида y = a*x + b, где у - Examination, х - Agriculture
model2 = lm(Examination~Agriculture, data)
model2 

plot(data$Examination~data$Agriculture) + abline(a = 28.7, b = -0.24, col = "red")

#Задание 3

#Оценка моделей по коэффициенту детерминации R^2

summary(model1)
summary(model2)
#Коэффициент детерминации в model1 R^2 = 48.8%, а в model2 R^2 = 47.1%.
#А значит модели относительно неплохи и зависимости имеются.
#Поскольку коэффициент не очень высок в обоих случаях, 
#необходимо добавлять дополнительные параметры для полного описания процесса.


#Задание 4 
#Значимость регрессора и в model1, и в model2 <0.001, поэтому взаимосвязь сильная.


