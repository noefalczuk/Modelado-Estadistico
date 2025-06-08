library(tidyverse)
library(MASS)       # Para polr()
library(caret)      # Para dividir en train/test

# Cargar los datos
datos <- read.delim("data.csv", sep = "\t", header = TRUE, fileEncoding = "UTF-8")

glimpse(datos)

colnames(datos)

# 1 < 2 < 3 < 4 < 5
niveles <- c(1, 2, 3, 4, 5)
cols_ordenadas <- paste0("Q", 1:44)

datos[cols_ordenadas] <- lapply(datos[cols_ordenadas], function(x) {
  factor(x, levels = niveles, ordered = TRUE)
})

datos$age <- as.numeric(datos$age)
table(datos$age)
hist(datos$age)
max(datos$age)
sum(is.na(datos$age))
lenoriginal <- length(datos$age)
datos <- datos[!is.na(datos$age) & datos$age <= 50, ]
hist(datos$age)
max(datos$age)
lenupdate <- length(datos$age)
lenoriginal - lenupdate

library(dplyr)

table(datos$Q12)  # valores reales presentes

summary(datos$Q12)  # incluye niveles vacíos si hay

# Contar cuántos hay por clase
table(datos$Q12)

# Elegir la cantidad mínima por clase (para muestreo uniforme)
n_min <- datos %>%
  filter(!is.na(Q12)) %>%
  count(Q12) %>%
  summarise(min_n = min(n)) %>%
  pull(min_n)


# Tomar aleatoriamente n_min observaciones por clase
datos_balanceado <- datos %>%
  filter(!is.na(Q12)) %>%
  group_by(Q12) %>%
  slice_sample(n = n_min) %>%
  ungroup()

# Verificación
table(datos_balanceado$Q12)
summary(datos_balanceado$Q12)


library(ggplot2)

ggplot(datos_balanceado, aes(x = age, fill = Q12)) +
  geom_histogram(binwidth = 5, position = "dodge") +
  labs(title = "Distribución de edad por clase",
       x = "Edad",
       y = "Frecuencia") +
  theme_minimal()

# Q12	I use lotion on my hands.

set.seed(123)
particion <- createDataPartition(datos_balanceado$Q12, p = 0.8, list = FALSE) #stratified 
train_Q12 <- datos_balanceado[particion, ]
test_Q12 <- datos_balanceado[-particion, ]

sum(is.na(train_Q12$Q12))
hist(as.numeric(train_Q12$Q12))
hist(as.numeric(test_Q12$Q12))

modelo_ord_Q12 <- MASS::polr(Q12 ~ age, data = train_Q12, Hess = TRUE)
summary(modelo_ord_Q12)

# Predecir sobre test
pred_ordinal_Q12 <- predict(modelo_ord_Q12, newdata = test_Q12)

# Convertir a numérico
pred_ordinal_final_Q12 <- as.numeric(pred_ordinal_Q12)


# Q9	me gustan las armas.
particion <- createDataPartition(datos_balanceado$"Q9", p = 0.8, list = FALSE) #stratified 
train_Q9 <- datos[particion, ]
test_Q9 <- datos[-particion, ]

sum(is.na(train_Q9$Q9))
table(train_Q9$Q9)
hist(as.numeric(train_Q9$Q9))
hist(as.numeric(test_Q9$Q9))

modelo_ord_Q9 <- MASS::polr(Q9 ~ age, data = train_Q9, Hess = TRUE)
summary(modelo_ord_Q9)

# Predecir sobre test
pred_ordinal_Q9 <- predict(modelo_ord_Q9, newdata = test_Q9)

# Convertir a numérico
pred_ordinal_final_Q9 <- as.numeric(pred_ordinal_Q9)


levels(datos$"Q9")
lev <- c('totalmente en desacuerdo', 'en desacuerdo', 'indiferente', 'de acuerdo', 'totalmente de acuerdo')

#personas25_edades <- test[test$age == 25,]
nueva_persona <- data.frame(age = 25)
probs <- predict(modelo_ord, nueva_persona, type = "probs")
prob_al_menos_acuerdo <- sum(probs[c("4", "5")])
print(prob_al_menos_acuerdo)

# manualmente 
edad <- 25
beta <- -0.00755
B <- beta * edad  
theta <- c(-0.8199, -0.2851, 0.4630, 1.2220)
logit <- function(z) 1 / (1 + exp(-z))

# Probabilidades acumuladas
F1 <- logit(theta[1] - B)                # P(Q9 ≤ 1) = P(Q9 = 1)
F2 <- logit(theta[2] - B)                # P(Q9 ≤ 2)
F3 <- logit(theta[3] - B)                # P(Q9 ≤ 3)
F4 <- logit(theta[4] - B)                # P(Q9 ≤ 4)
F5 <- 1                                  # P(Q9 ≤ 5)

p1 <- F1
p2 <- F2 - F1          
p3 <- F3 - F2
p4 <- F4 - F3
p5 <- F5 - F4

round(c(p1, p2, p3, p4, p5), 4)

loss_L <- function(y, y_pred) {
  if (length(y) != length(y_pred)) {
    stop("Los vectores y y y_pred deben tener la misma longitud.")
  }
  
  # Filtrar los pares donde y no es NA
  valid_idx <- !is.na(y)
  
  return(mean(abs(y[valid_idx] - y_pred[valid_idx]), na.rm = TRUE))
}

# modelo lineal pregunta Q

# Convertir factor ordenado a numérico (1, 2, 3, 4, 5)
train_Q12$Q12_num <- as.numeric(train_Q12$Q12)
test_Q12$Q12_num <- as.numeric(test_Q12$Q12)

train_Q9$Q9_num <- as.numeric(train_Q9$Q9)
test_Q9$Q9_num <- as.numeric(test_Q9$Q9)

# Ajustar modelo lineal usando la variable numérica
modelo_lineal_Q12 <- lm(Q12_num ~ age, data = train_Q12)
modelo_lineal_Q9 <- lm(Q9_num ~ age, data = train_Q9)


pred_lineal_Q12 <- predict(modelo_lineal_Q12, newdata = test_Q12)
pred_lineal_Q9 <- predict(modelo_lineal_Q9, newdata = test_Q9)

pred_lineal_redondeada_Q12 <- round(pred_lineal_Q12)
pred_lineal_redondeada_Q9 <- round(pred_lineal_Q9)

pred_lineal_final_Q12 <- pmin(pmax(pred_lineal_redondeada_Q12, 1), 5)
pred_lineal_final_Q9 <- pmin(pmax(pred_lineal_redondeada_Q9, 1), 5)

plot(test_Q12$age, test_Q12$Q12_num, 
     col = "blue", pch = 16, 
     xlab = "Edad", ylab = "Respuesta Q9",
     main = "Respuestas reales (azul) y predicciones (rojo) - MODELO LINEAL")

points(test_Q12$age, pred_lineal_final_Q12, col = "red", pch = 17)

points(test_Q12$age, pred_lineal_Q12, col = "orange", pch = 17)

plot(test_Q9$age, test_Q9$Q9_num, 
     col = "blue", pch = 16, 
     xlab = "Edad", ylab = "Respuesta Q9",
     main = "Respuestas reales (azul) y predicciones (rojo) - MODELO LINEAL")

points(test_Q9$age, pred_lineal_final_Q9, col = "red", pch = 17)

points(test_Q9$age, pred_lineal_Q9, col = "orange", pch = 17)


plot(test_Q12$age, test_Q12$Q12_num, 
     col = "blue", pch = 16, 
     xlab = "Edad", ylab = "Respuesta Q9",
     main = "Respuestas reales (azul) y predicciones (rojo) - MODELO ORDINAL")

points(test_Q12$age, pred_ordinal_final_Q12, col = "red", pch = 17)

plot(test_Q9$age, test_Q9$Q9_num, 
     col = "blue", pch = 16, 
     xlab = "Edad", ylab = "Respuesta Q9",
     main = "Respuestas reales (azul) y predicciones (rojo) - MODELO ORDINAL")

points(test_Q9$age, pred_ordinal_final_Q9, col = "red", pch = 17)


# Pérdida para regresión lineal
loss_lineal <- loss_L(test_Q12$Q12_num, pred_lineal_final_Q12)

# Pérdida para regresión ordinal
loss_ordinal <- loss_L(test_Q12$Q12_num, pred_ordinal_final_Q12)

# Mostrar resultados
cat("Pérdida modelo lineal: ", loss_lineal, "\n")
cat("Pérdida modelo ordinal:", loss_ordinal, "\n")

# Pérdida para regresión lineal
loss_lineal <- loss_L(test_Q9$Q9_num, pred_lineal_final_Q9)

# Pérdida para regresión ordinal
loss_ordinal <- loss_L(test_Q9$Q9_num, pred_ordinal_final_Q9)

# Mostrar resultados
cat("Pérdida modelo lineal: ", loss_lineal, "\n")
cat("Pérdida modelo ordinal:", loss_ordinal, "\n")