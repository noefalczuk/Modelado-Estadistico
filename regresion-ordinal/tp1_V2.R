library(tidyverse)
library(MASS)
library(caret)
library(purrr)
library(broom)
library(rstanarm)
library(bayesplot)
library(readr)


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

datos %>%
  ggplot(aes(x = age)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  facet_wrap(~ Q12) +
  labs(title = "Distribución de edad por clase en Q12",
       x = "Edad",
       y = "Frecuencia") +
  theme_minimal()

split_Q12$train %>%
  ggplot(aes(x = age)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  facet_wrap(~ Q12) +
  labs(title = "Distribución de edad por clase en Q12 - train",
       x = "Edad",
       y = "Frecuencia") +
  theme_minimal()

split_Q12$test %>%
  ggplot(aes(x = age)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  facet_wrap(~ Q12) +
  labs(title = "Distribución de edad por clase en Q12 - test",
       x = "Edad",
       y = "Frecuencia") +
  theme_minimal()

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

# manualmente 
edad <- 25
beta <- modelo_ord_Q9$coefficients[["age"]]
B <- beta * edad
theta <- unname(unlist(modelo_ord_Q9$zeta))
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

r2_targets <- c(0.001, 0.0000001, 0.2)

fits <- map(r2_targets, ~ stan_polr(
  formula         = Q9 ~ age,
  data            = split_Q9$train,
  prior           = R2(location = .x, what = "mean"),  
  chains          = 1,
  iter            = 100,
  seed            = 123,
))

names(fits) <- paste0("R2_", r2_targets)
post_beta <- imap_dfr(fits, function(fit, tag) {
  as_draws_df(fit) %>%         
    as_tibble() %>%
    transmute(
      prior = tag,
      beta  = age 
    )
})

ggplot(post_beta, aes(x = beta, colour = prior, fill = prior)) +
  geom_density(alpha = .25, adjust = 1.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Posterior de Beta bajo tres priors R2 distintas",
       subtitle = "Respuesta Q9 ~ edad · Regresión ordinal bayesiana (stan_polr)",
       x = expression(beta), y = "Densidad") +
  theme_minimal(base_size = 13)

# 11. (Opcional) Modelo bayesiano puro en Stan. Se omite por tiempo; ver informe si se decide implementarlo.

library(tidyverse)
library(rstan)
library(bayesplot)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

stan_code <- "
data {
  int<lower=1> N;
  int<lower=2> K;
  int<lower=1, upper=K> y[N];
  int<lower=1> D;
  matrix[N, D] X;
}
parameters {
  vector[D] beta;
  ordered[K-1] c;
}
model {
  beta ~ normal(0, 5);
  c ~ normal(0, 5);
  for (n in 1:N) {
    real eta = X[n] * beta;
    y[n] ~ ordered_logistic(eta, c);
  }
}
"

train_clean <- split_Q12$train %>% 
  filter(!is.na(Q9), !is.na(age))


stan_data <- list(
  N = nrow(train_clean),
  K = length(unique(train_clean$Q9)),
  y = train_clean$Q9,
  D = 1,
  X = as.matrix(train_clean$age)
)

modelo_ordinal <- stan_model(model_code = stan_code)
fit <- sampling(modelo_ordinal, data = stan_data, iter = 2000, chains = 4, seed = 123)
print(fit, pars = c("beta", "c"), probs = c(0.025, 0.5, 0.975))
mcmc_trace(as.array(fit), pars = c("beta", "c"))