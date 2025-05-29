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
datos <- datos[!is.na(datos$age) & datos$age <= 100, ]
hist(datos$age)

# Q12	I use lotion on my hands.

set.seed(123)
particion <- createDataPartition(datos$"Q12", p = 0.8, list = FALSE)
train <- datos[particion, ]
test <- datos[-particion, ]

sum(is.na(train$Q12))
table(train$Q12)

modelo_ord <- MASS::polr(Q12 ~ age, data = train, Hess = TRUE)
summary(modelo_ord)

# Q9	I use lotion on my hands.
sum(is.na(train$Q9))
table(train$Q9)

modelo_ord <- MASS::polr(Q9 ~ age, data = train, Hess = TRUE)
summary(modelo_ord)
