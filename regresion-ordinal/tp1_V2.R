library(tidyverse)
library(MASS)
library(caret)
library(purrr)
library(broom)
library(rstanarm)
library(bayesplot)

set.seed(123)
datos <- read_delim("data.csv", delim = "\t")

questions_cols_names <- paste0("Q", 1:44)
niveles <- 1:5
max_age <- 100

datos <- datos |> 
  mutate(across(all_of(questions_cols_names), ~ factor(.x, levels = niveles, ordered = TRUE))) |> 
  mutate(age = as.numeric(age)) |> 
  filter(!is.na(age), age <= max_age)

# Visualización exploratoria básica

datos %>%
  ggplot(aes(x = age)) +
  geom_histogram(bins = 30) +
  labs(title = "Distribución de edad", x = "Edad", y = "Frecuencia") %>% 
  print()

# Ejercicio 1
# Q12: "I use lotion on my hands"
# Q9 : "Me gustan las armas"

make_split <- function(df, response) {
  idx <- createDataPartition(df[[response]], p = .8, list = FALSE)
  list(train = df[idx,], test = df[-idx,])
}

split_Q12 <- make_split(datos, "Q12")
split_Q12$train <- split_Q12$train %>% 
  filter(!is.na(Q12))

split_Q12$test  <- split_Q12$test  %>% 
  filter(!is.na(Q12))

# Ejercicio 5

modelo_ord_Q12 <- polr(Q12 ~ age, data = split_Q12$train, Hess = TRUE)
cat("Coeficiente: ", modelo_ord_Q12$coefficients)
cat("Intercepts: ", modelo_ord_Q12$zeta)

# Ejercicio 6
split_Q9  <- make_split(datos, "Q9")
split_Q9$train <- split_Q9$train %>% 
  filter(!is.na(Q9))
split_Q9$test  <- split_Q9$test  %>% 
  filter(!is.na(Q9))

modelo_ord_Q9  <- polr(Q9  ~ age, data = split_Q9$train,  Hess = TRUE)
cat("Coeficiente: ", modelo_ord_Q9$coefficients)
cat("Intercepts: ", modelo_ord_Q9$zeta)

new_person <- tibble(age = 25)
probs_25 <- predict(modelo_ord_Q9, new_person, type = "probs") |> as.numeric()
prob_at_least_agree <- sum(probs_25[4:5])  # categorías 4 y 5
cat("P(Q9 ≥ 4 | edad = 25) =", round(prob_at_least_agree, 3), "\n")

# Ejercicio 7
loss_L <- function(y_true, y_pred_int) {
  mean(abs(as.numeric(y_true) - as.numeric(y_pred_int)))
}

pred_ord_Q12 <- predict(modelo_ord_Q12, newdata = split_Q12$test)
pred_ord_Q9  <- predict(modelo_ord_Q9,  newdata = split_Q9$test)

loss_ord_Q12 <- loss_L(split_Q12$test$Q12, pred_ord_Q12)
loss_ord_Q9  <- loss_L(split_Q9$test$Q9,  pred_ord_Q9)

# Ejercicio 8

auto_lin <- function(split, var) {
  
  train <- split$train %>% 
    filter(!is.na(.data[[var]])) %>%
    mutate(response = as.numeric(.data[[var]]))
  
  test  <- split$test %>% 
    filter(!is.na(.data[[var]]))
  
  model <- lm(response ~ age, data = train)
  
  pred <- predict(model, newdata = test) |>
    round() |>
    pmin(5) |> 
    pmax(1)
  
  list(model = model, pred = pred, test = test) 
}

lin_Q12 <- auto_lin(split_Q12, "Q12")
lin_Q9  <- auto_lin(split_Q9,  "Q9")

loss_lin_Q12 <- loss_L(split_Q12$test$Q12, lin_Q12$pred)
loss_lin_Q9  <- loss_L(split_Q9$test$Q9,  lin_Q9$pred)

# Ejercicio 9

plot_preds <- function(test_df, resp, pred_lin, pred_ord) {
  test_df |> 
    mutate(real = as.numeric(.data[[resp]]),
           lin  = pred_lin,
           ord  = as.numeric(pred_ord)) |> 
    pivot_longer(c(real, lin, ord), names_to = "tipo", values_to = "respuesta") |> 
    ggplot(aes(x = age, y = respuesta, colour = tipo)) +
    geom_jitter(width = .3, height = .1, alpha = .6, size = 1) +
    scale_colour_manual(values = c(real = "black", lin = "red", ord = "blue")) +
    labs(title = sprintf("%s: Observado vs Predicho", resp),
         y = "Respuesta (1‑5)", colour = "Tipo")
}

print(plot_preds(split_Q12$test, "Q12", lin_Q12$pred, pred_ord_Q12))
print(plot_preds(split_Q9$test,  "Q9",  lin_Q9$pred,  pred_ord_Q9))

metricas <- tibble(Modelo = rep(c("Lineal", "Ordinal"), each = 2),
                   Pregunta = rep(c("Q12", "Q9"), times = 2),
                   Loss = c(loss_lin_Q12, loss_lin_Q9,
                            loss_ord_Q12, loss_ord_Q9))
print(metricas)

# Ejercicio 10

prueba_fast <- stan_polr(
  Q9 ~ age,
  data            = split_Q9$train,
  prior           = R2(location = 0.5, what = "mean"),
  chains          = 3,
  iter            = 500,
  seed            = 123
)
print(prueba_fast)

# Ejercicio

r2_targets <- c(0.05, 0.30, 0.80)

fits <- map(r2_targets, ~ stan_polr(
  formula         = Q9 ~ age,
  data            = split_Q9$train,
  prior           = R2(location = .x, what = "mean"),  
  chains          = 2,
  iter            = 200,
  seed            = 123,
))
code1 <- stan_polr(
  formula         = Q9 ~ age,
  data            = split_Q9$train,
  prior           = R2(location = 0.1, what = "mean"),  
  chains          = 2,
  iter            = 200,
  seed            = 123,
)
code2 <- stan_polr(
  formula         = Q9 ~ age,
  data            = split_Q9$train,
  prior           = R2(location = 0.5, what = "mean"),  
  chains          = 2,
  iter            = 200,
  seed            = 123,
)
names(fits) <- paste0("R2_", r2_targets)

post_beta <- imap_dfr(fits, function(fit, tag) {
  as_draws_df(fit) %>%          # convierte a draws_df (tibble con cadenas, iteraciones y parámetros)
    as_tibble() %>%
    transmute(
      prior = tag,              # “R2_0.05”, “R2_0.3”, …
      beta  = age             # el nombre interno de la variable de pendiente es “b_age”
    )
})
post_beta_code_1 <- as_draws_df(code1) %>% as_tibble() %>% dplyr::select(age)
post_beta_code_2 <- as_draws_df(code2) %>% as_tibble() %>% dplyr::select(age)
# 4) Gráfico de densidades
ggplot(post_beta, aes(x = beta, colour = prior, fill = prior)) +
  geom_density(alpha = .25, adjust = 1.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Posterior de β bajo tres priors R2 distintas",
       subtitle = "Respuesta Q9 ~ edad · Regresión ordinal bayesiana (stan_polr)",
       x = expression(beta), y = "Densidad") +
  theme_minimal(base_size = 13)

# 5) Resumen estadístico para el informe
posterior_summary <- post_beta %>%
  group_by(prior) %>%
  summarise(
    media   = mean(beta),
    mediana = median(beta),
    sd      = sd(beta),
    .groups = "drop"
  )
print(posterior_summary)

# ── 11. (Opcional) Modelo bayesiano puro en Stan. Se omite por tiempo; ver informe si se decide implementarlo.


> post_beta_code_1
# A tibble: 200 × 1
age
<dbl>
  1 -0.00769
2 -0.00755
3 -0.00765
4 -0.00701
5 -0.00673
6 -0.00831
7 -0.00801
8 -0.00714
9 -0.00715
10 -0.00798
# ℹ 190 more rows
# ℹ Use `print(n = ...)` to see more rows
> post_beta_code_2
# A tibble: 200 × 1
age
<dbl>
  1 -0.00770
2 -0.00755
3 -0.00765
4 -0.00701
5 -0.00673
6 -0.00831
7 -0.00801
8 -0.00714
9 -0.00715
10 -0.00798
# ℹ 190 more rows





# ─── FIN DEL SCRIPT ───────────────────────────────────────────────────────────

# Pruebas Gian.
# Logre mejorar a 1.2, es decir que le erra mucho menos balanceando las clases de respuesta. Esto mejora
# Frente a regresion lineal pero porque predice todo 3 en vez de 1. Y minimiza la metrica porque esta en el 
# medio del rango de valores.

datos %>% 
  filter(!is.na(Q9)) %>% 
  count(Q9) %>%                             # calcula frecuencia por categoría
  mutate(prop = n / sum(n)) %>%             # proporción
  ggplot(aes(x = Q9, y = prop)) +
  geom_col(fill = "#1f78b4") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Distribución relativa de Q9",
       x = "Respuesta (1–5)",
       y = "Proporción") +
  theme_minimal(base_size = 14)

# 1. Eliminar filas con NA en Q9 y calcular el conteo mínimo por nivel
counts <- datos %>%
  filter(!is.na(Q9)) %>%
  count(Q9)

min_count <- min(counts$n)

# 2. Crear un dataset balanceado submuestreando cada nivel a 'min_count'
q19_balanced <- datos %>%
  filter(!is.na(Q9)) %>%
  group_by(Q9) %>%
  slice_sample(n = min_count, replace = FALSE) %>%
  ungroup()

# 3. Hacer el split train/test sobre el dataset balanceado
make_split <- function(df, response) {
  idx <- createDataPartition(df[[response]], p = 0.8, list = FALSE)
  list(train = df[idx, ], test = df[-idx, ])
}

q19_split_balanced <- make_split(q19_balanced, "Q9")

# 4. Ajustar regresión ordinal (polr) usando solo 'age' como predictor
mod_ord_q9_bal <- polr(
  formula = Q9 ~ age,
  data    = q19_split_balanced$train,
  Hess    = TRUE
)

# 5. Ver resumen del modelo
summary(mod_ord_q9_bal)

# 6. (Opcional) Predecir sobre el set de prueba balanceado y redondear
probs_bal <- predict(mod_ord_q9_bal, newdata = q19_split_balanced$test, type = "probs")
# calcular valor esperado y luego redondear (evita quedar siempre en la clase modal)
ev_bal  <- as.vector(probs_bal %*% (1:5))
pred_bal <- round(ev_bal) %>% pmin(5) %>% pmax(1)

# 7. (Opcional) Calcular pérdida absoluta media
loss_L <- function(y, y_pred) {
  valid <- !is.na(y)
  mean(abs(y[valid] - y_pred[valid]))
}

test_num_q9_bal <- as.numeric(q19_split_balanced$test$Q9)
loss_bal <- loss_L(test_num_q9_bal, pred_bal)
cat("Loss (MAE) en data balanceada Q9:", loss_bal, "\n")
print(plot_preds(q19_split_balanced$test,  "Q9",  pred_bal,  pred_bal))
