#task 1
# Загрузка данных
data_bp <- read.table("/Users/alex_aredov/Downloads/Blood pressure -age.txt", header = TRUE)

# Построение линейной регрессии
model_bp <- lm(blood_pressure ~ age, data = data_bp)

# Отображение коэффициентов модели
summary(model_bp)

# Построение графика с линией тренда
plot(data_bp$age, data_bp$blood_pressure, 
     main = "Зависимость давления от возраста", 
     xlab = "Возраст", ylab = "Систолическое давление")
abline(model_bp, col = "blue", lwd = 2)

# Оценка выполнения предположений линейной регрессии
# Построим графики остатков
par(mfrow = c(2, 2))
plot(model_bp)

#p-value < 0.05 Есть статистически значимая линейная связь между возрастом и систолическим артериальным давлением.
#Выраженность зависимости можно судить по коэффициенту детерминации R2
#Нормальность: если точки на Q-Q графике отклоняются от линии, значит остатки ненормальны.
#Гетероскедастичность: если в "Scale-Location" или "Residuals vs Fitted" видно изменение дисперсии (например, веерообразный узор), это признак нарушения.
#Выбросы: на графике "Residuals vs Leverage" точки с высокими значениями "Leverage" или "Cook's distance" могут быть выбросами.


#task 2
# Загрузка данных
data_fat <- read.table("/Users/alex_aredov/Downloads/Blood fat.txt", header = TRUE, sep = "\t")

# Построение линейной регрессии с двумя предикторами
model_fat <- lm(Blood.fat.content ~ Age + Weight, data = data_fat)

# Отображение коэффициентов модели
summary(model_fat)

# Оценка выполнения предположений линейной регрессии
# Построим графики остатков
par(mfrow = c(2, 2))
plot(model_fat)
#все аналогично task 1

#task 3
# Загрузка данных
data_toxicity <- read.table("/Users/alex_aredov/Downloads/toxicity.txt", header = TRUE)

# Построение полной линейной регрессии
model_toxicity <- lm(toxicity ~ ., data = data_toxicity)

# Отображение коэффициентов модели
summary(model_toxicity)

# Использование критерия Акаике для отбора переменных
library(MASS)
model_aic <- stepAIC(model_toxicity, direction = "both")

# Финальная модель после отбора
summary(model_aic)

# Проверка выполнения предположений линейной регрессии
par(mfrow = c(2, 2))
plot(model_aic)
