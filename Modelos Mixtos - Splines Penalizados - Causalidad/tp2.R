required_packages <- c(
  "tidyverse",
  "lubridate",
  "lme4",
  "broom.mixed",
  "mgcv",
  "splines",
  "flexmix",
  "caret",
  "stringr",
  "GGally",
  "DataExplorer",
  "patchwork"
)

installed <- rownames(installed.packages())
for (p in required_packages) {
  if (!(p %in% installed)) install.packages(p, dependencies = TRUE)
  library(p, character.only = TRUE)
}

# 1. Lectura de datos ---------------------------------------------------------
titles_train <- read_csv("titles_train.csv", show_col_types = FALSE)
credits_train <- read_csv("credits_train.csv", show_col_types = FALSE)
titles_test  <- read_csv("titles_test.csv", show_col_types = FALSE)
credits_test <- read_csv("credits_test.csv", show_col_types = FALSE)

all_data_train <- titles_train
all_data_test <- titles_test
  
# 1b. Limpieza básica y features ---------------------------------------------
extract_country <- function(x) str_extract(x, "[A-Z]{2}")
has_genre       <- function(genres_string, g) str_detect(str_to_lower(genres_string), g)

all_data_train <- all_data_train %>% 
  mutate(
    country   = factor(extract_country(production_countries)),
    Comedy    = as.integer(has_genre(genres, "comedy")),
    Drama     = as.integer(has_genre(genres, "drama")),
    Action    = as.integer(has_genre(genres, "action")),
    Year_c    = release_year - 1953,
    runtime_c = runtime - mean(runtime, na.rm = TRUE)
  ) %>% 
  drop_na(imdb_score) %>% 
  select(-c(age_certification, ...1, genres, imdb_id, id)) %>% 
  mutate(
    seasons = if_else(type == "MOVIE", 1L, seasons)
  )

all_data_test <- titles_test %>%
  mutate(
    country   = factor(extract_country(production_countries)),
    Comedy    = as.integer(has_genre(genres, "comedy")),
    Drama     = as.integer(has_genre(genres, "drama")),
    Action    = as.integer(has_genre(genres, "action")),
    LogVote   = log1p(imdb_votes),
    Year_c    = release_year - 1950,
    runtime_c = runtime - mean(titles_train$runtime, na.rm = TRUE)
  )

# -----------------------------------------------------------------------------
# 1. Exploratory Data Analysis (EDA) -----------------------------------------
# -----------------------------------------------------------------------------

out_dir <- "figs"
dir.create(out_dir, showWarnings = FALSE)

save_plot <- function(p, name, width = 6, height = 4) {
  ggsave(
    filename = file.path(out_dir, name),
    plot     = p,
    width    = width,
    height   = height
  )
}

# 0. Informe DataExplorer -----------------------------------------------------
create_report(
  all_data_train,
  y             = "imdb_score",
  output_file   = file.path(out_dir, "EDA_report.html"),
  report_title  = "EDA automática – TP2"
)

# 1. Resumen numérico ---------------------------------------------------------
numeric_vars <- all_data_train %>% select(where(is.numeric))

summary_tbl <- summary(numeric_vars)
capture.output(summary_tbl,
               file = file.path(out_dir, "summary_numeric.txt"))

# 2. Distribución de la variable objetivo ------------------------------------
p_hist <- ggplot(all_data_train, aes(imdb_score)) +
  geom_histogram(bins = 30, fill = "steelblue", colour = "white") +
  geom_vline(aes(xintercept = mean(imdb_score, na.rm = TRUE)),
             linetype = "dashed", size = 0.8) +
  labs(title = "Distribución IMDB Score",
       x = "IMDB score", y = "Frecuencia") +
  theme_minimal()

p_dens <- ggplot(all_data_train, aes(imdb_score)) +
  geom_density(fill = "skyblue", alpha = 0.4) +
  labs(title = "Densidad IMDB Score",
       x = "IMDB score", y = "Densidad") +
  theme_minimal()

save_plot((p_hist | p_dens) + plot_annotation(title = "Variable objetivo"),
          "target_distribution.png", width = 10, height = 4)

# 3. IMDB vs. año de estreno --------------------------------------------------
p_year <- ggplot(all_data_train, aes(release_year, imdb_score)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "loess", se = FALSE, colour = "red") +
  labs(title = "IMDB Score vs. Año de estreno",
       x = "Año", y = "IMDB score") +
  theme_minimal()

save_plot(p_year, "score_vs_year.png")

# 4. Matriz de correlaciones numéricas ---------------------------------------
cor_mat <- GGally::ggcorr(
  numeric_vars,
  label       = TRUE,
  label_round = 2,
  digits      = 2,
  name        = "Correlación"
) +
  labs(title = "Matriz de correlaciones (numéricas)") +
  theme(plot.title = element_text(size = 12, face = "bold"))

save_plot(cor_mat, "correlation_matrix.png", width = 7, height = 6)

# 5. Puntuación por país (top-10) --------------------------------------------
top_countries <- all_data_train %>%
  count(country, sort = TRUE) %>%
  slice_head(n = 10) %>%
  pull(country)

p_country <- all_data_train %>%
  filter(country %in% top_countries) %>%
  ggplot(aes(country, imdb_score)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "IMDB Score por País (Top-10)",
       x = "País", y = "IMDB score") +
  theme_minimal()

save_plot(p_country, "score_by_country.png", width = 7, height = 4)

# 6. Puntuación por género ----------------------------------------------------
p_genre <- all_data_train %>%
  pivot_longer(
    cols      = c(Comedy, Drama, Action),
    names_to  = "Genre",
    values_to = "Flag"
  ) %>%
  filter(Flag == 1) %>%
  ggplot(aes(Genre, imdb_score)) +
  geom_boxplot(fill = "plum") +
  labs(title = "IMDB Score por Género",
       x = "Género principal", y = "IMDB score") +
  theme_minimal()

save_plot(p_genre, "score_by_genre.png", width = 6, height = 4)

# Versión 2 

genres_expanded <- titles_train %>%
  filter(!is.na(genres)) %>%
  mutate(genres = strsplit(genres, ",")) %>%
  unnest(genres) %>%
  mutate(genres = str_trim(genres))

# Paso 1: Quitar corchetes y comillas simples
genres_clean <- genres_expanded %>%
  mutate(genres = str_replace_all(genres, "\\[|\\]|'", "")) %>%
  mutate(genres = str_trim(genres)) %>%
  filter(genres != "")  # sacar vacíos

# Paso 2: Separar por coma si hay varios géneros en la misma cadena
genres_sep <- genres_clean %>%
  separate_rows(genres, sep = ",\\s*") %>%
  mutate(genres = str_trim(genres))  # limpiar espacios después de separar

# Ahora, `genres_sep` tiene una fila por cada género individual.

# Ejemplo de cálculo:
genre_scores <- genres_sep %>%
  group_by(genres) %>%
  summarise(mean_imdb_score = mean(imdb_score, na.rm = TRUE),
            count = n()) %>%
  arrange(desc(mean_imdb_score))

# Gráfico
library(ggplot2)
p_genre2 <- ggplot(genre_scores, aes(x = reorder(genres, mean_imdb_score), y = mean_imdb_score, fill = count)) +
  geom_col() +
  coord_flip() +
  labs(title = "Puntaje promedio IMDb por género",
       x = "Género",
       y = "Puntaje promedio IMDb",
       fill = "Cantidad de títulos") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))


save_plot(p_genre2, "score_by_genre2.png", width = 6, height = 4)


# DIRECTORES Y PUNTAJES 
# Unir créditos con títulos para puntajes
credits_scores <- credits_train %>%
  inner_join(titles_train %>% select(id, imdb_score), by = "id")

# Actores
actor_scores <- credits_scores %>%
  filter(role == "ACTOR") %>%
  group_by(name) %>%
  summarise(mean_imdb_score = mean(imdb_score, na.rm = TRUE),
            count = n()) %>%
  arrange(desc(mean_imdb_score))

# Directores
director_scores <- credits_scores %>%
  filter(role == "DIRECTOR") %>%
  group_by(name) %>%
  summarise(mean_imdb_score = mean(imdb_score, na.rm = TRUE),
            count = n()) %>%
  arrange(desc(mean_imdb_score))

# Top 10 mejores actores
top10_actors_best <- actor_scores %>%
  slice_max(mean_imdb_score, n = 10)

# Top 10 peores actores
top10_actors_worst <- actor_scores %>%
  slice_min(mean_imdb_score, n = 10)

# Top 10 mejores directores
top10_directors_best <- director_scores %>%
  slice_max(mean_imdb_score, n = 10)

# Top 10 peores directores
top10_directors_worst <- director_scores %>%
  slice_min(mean_imdb_score, n = 10)

# PALABRAS Y TÍTULOS 
library(tidytext)

# Tokenizar títulos
title_words <- titles_train %>%
  select(imdb_score, title) %>%
  filter(!is.na(title)) %>%
  unnest_tokens(word, title) %>%
  anti_join(stop_words, by = "word")

title_word_scores <- title_words %>%
  group_by(word) %>%
  summarise(mean_imdb_score = mean(imdb_score, na.rm = TRUE),
            count = n()) %>%
  filter(count >= 5) %>%
  arrange(desc(mean_imdb_score))

head(title_word_scores, 10)  # Palabras en títulos con mayor puntaje promedio

# Tokenizar descripciones
desc_words <- titles_train %>%
  select(imdb_score, description) %>%
  filter(!is.na(description)) %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words, by = "word")

desc_word_scores <- desc_words %>%
  group_by(word) %>%
  summarise(mean_imdb_score = mean(imdb_score, na.rm = TRUE),
            count = n()) %>%
  filter(count >= 5) %>%
  arrange(desc(mean_imdb_score))

head(desc_word_scores, 10)

# Ejercicio 2
rmse <- function(obs, pred) sqrt(mean((obs - pred)^2, na.rm = TRUE))
mod_fixed <- lm(imdb_score ~ country, data = all_data_train)
mod_rand  <- lmer(imdb_score ~ 1 + (1 | country), data = all_data_train, REML = FALSE)

# Efectos fijos por país

coef_fixed <- broom::tidy(mod_fixed) %>%                # coeficientes del lm
  dplyr::filter(term != "(Intercept)") %>%                     # quitamos intercepto
  dplyr::mutate(
    term   = paste0("country:", str_remove(term, "^country")),
    model  = "Fixed"
  ) %>% 
  dplyr::select(term, estimate, model)                         # dejamos solo columnas clave

#Efectos aleatorios

coef_rand <- broom.mixed::tidy(mod_rand, effects = "ran_vals") %>% 
  dplyr::filter(group == "country", term == "(Intercept)") %>% # intercepto por país
  dplyr::mutate(
    term  = paste0("country:", level),                  # p. ej. "country:US"
    model = "Random"
  ) %>% 
  dplyr::select(term, estimate, model)

# Comparacion
bind_rows(coef_fixed, coef_rand) %>% 
  ggplot(aes(x = reorder(term, estimate), y = estimate, fill = model)) +
  geom_col(position = position_dodge()) +
  coord_flip() +
  labs(
    title = "Comparación de efectos por país",
    x     = NULL,
    y     = "Estimación"
  ) +
  theme(axis.text.y = element_text(size = 5))

#¿Qué nos dice esto?
  
# La mayoría de los países tiene un efecto positivo, pues sus barras (rojas y verdes) 
# quedan por encima de la línea discontinua en 0, lo que significa que, en promedio, sus 
# películas obtienen puntajes IMDB por encima de la media global.
# Los extremos se moderan: países como CD o KR aparecen con coeficientes fijos 
# muy altos, pero sus efectos aleatorios son bastante menores, indicando que quizá hay 
# poca información para sostener esa gran desviación.
# Algunos países (por ejemplo AE, BG) muestran efectos negativos: sus películas rinden por 
# debajo del promedio, aunque de nuevo los valores absolutos se reducen en el modelo aleatorio.
# Ventaja del modelo mixto: da estimaciones más estables (menos varianza) para países poco 
# muestreados, aprovechando el “pooling” de información; mientras que el modelo de efectos 
# fijos puede sobreajustarse a pequeñas submuestras.

# Poco soporte estadístico: Un gran desfase fijo vs. aleatorio suele señalar que el país en 
# cuestión tiene pocas observaciones, de modo que su gran coeficiente fijo podría ser simplemente 
# un pico de azar.

# Mayor robustez: El modelo mixto corrige esos “picos” y produce estimaciones más estables y 
# generalizables.

# Confianza: Si necesitas predecir para futuros filmes de ese país, confiarás más en la 
# estimación parcial-pooling (aleatorio) que en la pura estimación de la muestra (fijo).

# 3.2 Spline cúbico en año ----------------------------------------------------
# vector de valores de k a probar
df <- all_data_train %>% 
  dplyr::filter(!is.na(release_year), !is.na(imdb_score)) %>% 
  dplyr::mutate(release_year = as.numeric(release_year))

# 2. Defino la grilla de años donde voy a predecir
year_grid <- seq(min(df$release_year), max(df$release_year), length.out = 200)

# 3. Valores de k (número de nodos internos)
ks <- c(1, 2, 3, 5, 10, 20)

# 4. Ajusto un modelo para cada k y calculo la curva estimada
all_preds <- lapply(ks, function(k) {
  # 4.1 calculo nodos equidistantes (pueden ser también en cuantiles)
  knots <- seq(min(df$release_year), max(df$release_year),
               length.out = k + 2)[-c(1, k + 2)]
  
  # 4.2 ajusto spline cúbico sin penalización
  mod <- lm(imdb_score ~ bs(release_year, knots = knots, degree = 3), data = df)
  
  # 4.3 predecir en la grilla
  preds <- predict(mod, newdata = data.frame(release_year = year_grid))
  
  # 4.4 devuelvo un data.frame con resultados y etiqueta k
  data.frame(release_year = year_grid,
             imdb_score    = preds,
             k              = as.factor(k))
}) %>% bind_rows()

# 5. Grafico todas las curvas juntas
ggplot(all_preds, aes(x = release_year, y = imdb_score, color = k)) +
  geom_line(size = 1) +
  labs(
    title = "Spline cúbico (λ=0) con distintos números de nodos",
    x     = "Año de estreno",
    y     = "Popularidad predicha",
    color = "nodos (k)"
  ) + 
  geom_point(
    data = all_data_train,
    aes(x = release_year, y = imdb_score),
    color = "grey50", alpha = 0.4, size = 1
  ) +
  theme_minimal()


# ks =50 predice muy mal

# 4 en latex

# 5. Selección de modelo predictivo ------------------------------------------
set.seed(2025)
train_index <- caret::createDataPartition(all_data_train$imdb_score, p = 0.8, list = FALSE)
train_set   <- all_data_train[train_index, ]
valid_set   <- all_data_train[-train_index, ]

# Modelo 1: lineal + spline
m1 <- lm(imdb_score ~ bs(Year_c, df = 5) + runtime_c + Comedy, data = train_set)
pred1 <- predict(m1, newdata = valid_set)
rmse1 <- rmse(valid_set$imdb_score, pred1)

# Modelo 2: mixto
m2 <- lmer(imdb_score ~ bs(Year_c, df = 5) + runtime_c + Comedy + (1 | country),
           data = train_set, REML = FALSE)
pred2 <- predict(m2, newdata = valid_set, allow.new.levels = TRUE)
rmse2 <- rmse(valid_set$imdb_score, pred2)

# Modelo 3: mezcla lineal
m3 <- flexmix(imdb_score ~ Year_c + runtime_c + Comedy, data = train_set, k = 2)
pred3_list <- pred3 <- flexmix::predict(m3, newdata = valid_set, aggregate = TRUE)
pred3_mat <- do.call(cbind, pred3_list)
pred3_vec <- rowMeans(pred3_mat)
rmse3 <- rmse(valid_set$imdb_score, pred3_vec)

rmse_tbl <- tibble(
  Modelo = c("Lineal + spline", "Mixto", "Mezcla lineal (k=2)"),
  RMSE   = c(rmse1, rmse2, rmse3)
)
print(rmse_tbl)

best_model <- list(m1 = m1, m2 = m2, m3 = m3)[[which.min(c(rmse1, rmse2, rmse3))]]
best_name  <- rmse_tbl %>% slice(which.min(RMSE)) %>% pull(Modelo)
cat('El modelo seleccionado es:', best_name, '\n')

# 6. Entrenamiento final y predicciones --------------------------------------
if (best_name == "Lineal + spline") {
  final_model <- lm(imdb_score ~ bs(Year_c, df = 5) + runtime_c + Comedy,
                    data = titles_train)
  preds <- predict(final_model, newdata = titles_test)
} else if (best_name == "Mixto") {
  final_model <- lmer(imdb_score ~ bs(Year_c, df = 5) + runtime_c + Comedy + (1 | country),
                      data = titles_train, REML = FALSE)
  preds <- predict(final_model, newdata = titles_test, allow.new.levels = TRUE)
} else {
  final_model <- flexmix(imdb_score ~ Year_c + runtime_c + Comedy,
                         data = titles_train, k = 2)
  preds <- predict(final_model, newdata = titles_test)
}

write_csv(tibble(imdb_score_pred = preds), "predicciones.csv")
cat('Archivo predicciones.csv generado.\n')


#Pruebas
# ---- Minimal exhaustive CV search for IMDb score -----------------
# Requires only 'lme4' and base R. Adjust variable names if needed.

library(lme4)     # for lmer()
library(splines)  # ns()

set.seed(123)

#-------------------------------------------------------------------
# 0) DATA -----------------------------------------------------------
# Replace this with your data frame
# datos <- readRDS("your_data.rds")

k <- 5                                 # k-folds
fold_id <- sample(rep(1:k, length.out = nrow(datos)))

#-------------------------------------------------------------------
# 1) Grid of candidate feature flags --------------------------------
df_spline <- 8

combis <- expand.grid(
  seasons   = c(TRUE, FALSE),
  imdb_votes= c(TRUE, FALSE),
  runtime   = c(TRUE, FALSE),        # drop if 'runtime' not present
  runtime_c = c(TRUE, FALSE),
  Year_c    = c(TRUE, FALSE),
  country   = c(TRUE, FALSE),
  Comedy    = c(TRUE, FALSE),
  Drama     = c(TRUE, FALSE),
  Action    = c(TRUE, FALSE),
  
  spline_imdb_votes = c(TRUE, FALSE),
  spline_runtime    = c(TRUE, FALSE),
  spline_Year_c     = c(TRUE, FALSE),
  spline_runtime_c  = c(TRUE, FALSE),
  
  country_random    = c(TRUE, FALSE)
)

# Remove invalid combos (cannot spline absent vars)
combis <- combis[!(combis$spline_imdb_votes & !combis$imdb_votes), ]
combis <- combis[!(combis$spline_runtime    & !combis$runtime), ]
combis <- combis[!(combis$spline_Year_c     & !combis$Year_c), ]
combis <- combis[!(combis$spline_runtime_c  & !combis$runtime_c), ]

#-------------------------------------------------------------------
# 2) Helpers --------------------------------------------------------
build_formula <- function(row) {
  row <- as.list(row)
  terms <- character()
  
  add <- function(x) terms <<- c(terms, x)
  
  if (row$seasons) add("seasons")
  
  if (row$imdb_votes)
    add(if (row$spline_imdb_votes) sprintf("ns(imdb_votes,%d)", df_spline) else "imdb_votes")
  
  if (row$runtime)
    add(if (row$spline_runtime) sprintf("ns(runtime,%d)", df_spline) else "runtime")
  
  if (row$runtime_c)
    add(if (row$spline_runtime_c) sprintf("ns(runtime_c,%d)", df_spline) else "runtime_c")
  
  if (row$Year_c)
    add(if (row$spline_Year_c) sprintf("ns(Year_c,%d)", df_spline) else "Year_c")
  
  if (row$country) add("country")
  if (row$Comedy)  add("Comedy")
  if (row$Drama)   add("Drama")
  if (row$Action)  add("Action")
  
  fixed <- if (length(terms)) paste(terms, collapse = " + ") else "1"
  
  if (row$country_random) {
    as.formula(paste0("imdb_score ~ ", fixed, " + (1|country)"))
  } else {
    as.formula(paste0("imdb_score ~ ", fixed))
  }
}

rmse_cv <- function(formula_obj, random_flag) {
  tryCatch({
    mod <- if (random_flag) {
      lme4::lmer(formula_obj, data = all_data_train, REML = FALSE)
    } else {
      stats::lm(formula_obj, data = all_data_train)
    }
    pred <- predict(mod, newdata = all_data_train, allow.new.levels = TRUE)
    sqrt(mean((all_data_train$imdb_score - pred)^2, na.rm = TRUE))
  },
  error = function(e) Inf)
}

#-------------------------------------------------------------------
# 3) Exhaustive search ---------------------------------------------
best_rmse <- Inf
best_formula <- NULL
best_random <- FALSE

for (i in 1:nrow(combis)) {
  row      <- combis[i, ]
  formula  <- build_formula(row)
  rflag    <- row$country_random
  cv_rmse  <- rmse_cv(formula, rflag)
  
  if (cv_rmse < best_rmse) {
    best_rmse    <- cv_rmse
    best_formula <- formula
    best_random  <- rflag
  }
  cat(sprintf("Model %d / %d  RMSE = %.4f\n", i, nrow(combis), cv_rmse))
}

cat("\n>>> Best model:", deparse(best_formula),
    "\n    RMSE =", best_rmse, "\n\n")

best_model_through_model_selecion <- imdb_score ~ seasons + ns(imdb_votes, 8) + runtime + ns(runtime_c, 8) + ns(Year_c, 8) + Comedy + Drama + Action + (1 | country)
#-------------------------------------------------------------------
# 4) Fit best model on full data & save -----------------------------
final_model <- if (best_random) {
  lmer(best_formula, data = datos, REML = FALSE)
} else {
  lmer(imdb_score ~ seasons + ns(imdb_votes, 8) + ns(runtime, 8) + ns(runtime_c, 8) + ns(Year_c, 8) + country + Comedy + Drama + Action + 
       (1 | country), data = all_data_train)
}

saveRDS(final_model, "best_model.rds")
cat("Saved as best_model.rds\n")
