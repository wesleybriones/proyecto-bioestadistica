# Definir los datos
datos <- data.frame(
  Repeticion = c("TIRI", "TIR2", "TIR3", "TIR4", "TIR5", "T2R1", "T2R2", "T2R3", "T2R4", "T2R5", "T3R1", "T3R2", "T3R3", "T3R4", "T3R5"),
  TRES_DIAS = c(90.52, 17.863, 17.921, 25.957, 6.722, 16.925, 16.96, 116.73, 81.622, 6.223, 12.912, 16.041, 17.643, 28.359, 96.21),
  SEMANA_1 = c(1.294, 1.496, 1.366, 1.562, 1.537, 1.927, 1.928, 2.013, 1.927, 1.927, 2.984, 2.611, 2.222, 2.651, 2.648),
  SEMANA_2 = c(6.045, 5.568, 5.491, 5.089, 2.649, 5.886, 4.562, 7.84, 7.715, 3.66, 4.353, 7.616, 8.635, 13.757, 8.075),
  SEMANA_3 = c(1.145, 1.25, 1.216, 1.399, 1.357, 1.725, 1.761, 1.718, 1.704, 1.739, 3.97, 2.472, 2.016, 1.995, 2.618),
  SEMANA_4 = c(80.85, 4.321, 4.125, 4.283, 2.231, 4.118, 3.66, 81.88, 5.052, 2.848, 3.163, 4.861, 6.09, 7.62, 82.46),
  SEMANA_5 = c(1.88, 1.102, 1.118, 1.182, 1.153, 1.492, 1.524, 1.54, 1.473, 1.489, 3.243, 2.127, 1.759, 1.785, 2.329),
  SEMANA_6 = c(1.19, 2.97, 2.609, 2.74, 1.862, 2.937, 2.782, 3.76, 3.691, 2.345, 2.698, 3.758, 4.317, 5.633, 4.18),
  SEMANA_7 = c(1.65, 0.867, 1.006, 1.049, 1.063, 1.382, 1.414, 1.44, 1.382, 1.405, 2.612, 1.91, 1.619, 1.669, 2.07),
  SEMANA_8 = c(78.09, 2.523, 2.205, 2.318, 1.667, 2.482, 2.482, 80.11, 3.133, 2.127, 2.339, 3.096, 3.307, 4.642, 3.454)
)

#* LITERAL A *#
# Combinar los valores de los diferentes sistemas de riego en un único vector
valores <- c(datos$TRES_DIAS, datos$SEMANA_1, datos$SEMANA_2, datos$SEMANA_3, datos$SEMANA_4, datos$SEMANA_5, datos$SEMANA_6, datos$SEMANA_7, datos$SEMANA_8 )

# Realizar la prueba de Shapiro-Wilk
shapiro_test <- shapiro.test(valores)

# Imprimir los resultados
print(shapiro_test)

#* LITERAL B *#
# Datos
TRES_DIAS = c(90.52, 17.863, 17.921, 25.957, 6.722, 16.925, 16.96, 116.73, 81.622, 6.223, 12.912, 16.041, 17.643, 28.359, 96.21)
SEMANA_1 = c(1.294, 1.496, 1.366, 1.562, 1.537, 1.927, 1.928, 2.013, 1.927, 1.927, 2.984, 2.611, 2.222, 2.651, 2.648)
SEMANA_2 = c(6.045, 5.568, 5.491, 5.089, 2.649, 5.886, 4.562, 7.84, 7.715, 3.66, 4.353, 7.616, 8.635, 13.757, 8.075)
SEMANA_3 = c(1.145, 1.25, 1.216, 1.399, 1.357, 1.725, 1.761, 1.718, 1.704, 1.739, 3.97, 2.472, 2.016, 1.995, 2.618)
SEMANA_4 = c(80.85, 4.321, 4.125, 4.283, 2.231, 4.118, 3.66, 81.88, 5.052, 2.848, 3.163, 4.861, 6.09, 7.62, 82.46)
SEMANA_5 = c(1.88, 1.102, 1.118, 1.182, 1.153, 1.492, 1.524, 1.54, 1.473, 1.489, 3.243, 2.127, 1.759, 1.785, 2.329)
SEMANA_6 = c(1.19, 2.97, 2.609, 2.74, 1.862, 2.937, 2.782, 3.76, 3.691, 2.345, 2.698, 3.758, 4.317, 5.633, 4.18)
SEMANA_7 = c(1.65, 0.867, 1.006, 1.049, 1.063, 1.382, 1.414, 1.44, 1.382, 1.405, 2.612, 1.91, 1.619, 1.669, 2.07)
SEMANA_8 = c(78.09, 2.523, 2.205, 2.318, 1.667, 2.482, 2.482, 80.11, 3.133, 2.127, 2.339, 3.096, 3.307, 4.642, 3.454)

# Medidas de Tendencia Central
media <- function(x) mean(x)
moda <- function(x) as.numeric(names(sort(table(x), decreasing = TRUE)[1]))
mediana <- function(x) median(x)

# Medidas de Dispersión
varianza <- function(x) var(x)
desviacion_estandar <- function(x) sd(x)
error_estandar <- function(x) sd(x) / sqrt(length(x))
coeficiente_variacion <- function(x) (sd(x) / mean(x)) * 100
rango <- function(x) max(x) - min(x)

# Medidas de Posición
cuartiles <- function(x) quantile(x, probs = c(0.25, 0.5, 0.75))
deciles <- function(x) quantile(x, probs = seq(0.1, 0.9, by = 0.1))
percentiles <- function(x) quantile(x, probs = seq(0.1, 0.9, by = 0.1))

# Aplicar funciones a todas las columnas
columns <- list(TRES_DIAS, SEMANA_1, SEMANA_2, SEMANA_3, SEMANA_4, SEMANA_5, SEMANA_6, SEMANA_7, SEMANA_8)
names <- c("TRES_DIAS", "SEMANA_1", "SEMANA_2", "SEMANA_3", "SEMANA_4", "SEMANA_5", "SEMANA_6", "SEMANA_7", "SEMANA_8")

for (i in 1:length(columns)) {
  cat("Medidas de", names[i], "\n")
  cat("Media:", media(columns[[i]]), "\n")
  cat("Moda:", moda(columns[[i]]), "\n")
  cat("Mediana:", mediana(columns[[i]]), "\n")
  
  cat("\nMedidas de Dispersión:\n")
  cat("Varianza:", varianza(columns[[i]]), "\n")
  cat("Desviación Estándar:", desviacion_estandar(columns[[i]]), "\n")
  cat("Error Estándar:", error_estandar(columns[[i]]), "\n")
  cat("Coeficiente de Variación:", coeficiente_variacion(columns[[i]]), "\n")
  cat("Rango:", rango(columns[[i]]), "\n")
  
  cat("\nMedidas de Posición:\n")
  cat("Cuartiles:\n")
  cat(cuartiles(columns[[i]]), "\n")
  cat("Deciles:\n")
  cat(deciles(columns[[i]]), "\n")
  cat("Percentiles:\n")
  cat(percentiles(columns[[i]]), "\n")
  cat("\n")
}

#* LITERAL C *#
# Transformar los datos a formato largo (long)
datos_long <- melt(datos, id.vars = "Repeticion", variable.name = "Tratamiento", value.name = "Valor")

# Crear un gráfico de barras general
gg <- ggplot(data = datos_long, aes(x = factor(Tratamiento), y = Valor, fill = Repeticion)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Valores de Repeticiones por Tratamiento", x = "Tratamiento", y = "Valor") +
  theme_minimal()

print(gg)

#* LITERAL D *#
# Tratamiento a analizar
Tratamiento <- unique(datos$Repeticion)

# Realizar análisis para cada fila
for (fila in Tratamiento) {
  fila_data <- unlist(datos[datos$Repeticion == fila, -1])  # Unlist all values in the row
  
  cat("Análisis para la fila", fila, ":\n")
  cat("Media:", mean(fila_data), "\n")
  cat("Mediana:", median(fila_data), "\n")
  cat("Desviación Estándar:", sd(fila_data), "\n")
  
  # Test de Shapiro-Wilk
  shapiro_test <- shapiro.test(fila_data)
  cat("Valor p del test de Shapiro-Wilk:", shapiro_test$p.value, "\n")
  
  cat("\n")
}

#* LITERAL E *#
# Definir los datos
repeticiones <- data.table(
  TRES_DIAS = c(90.52, 17.863, 17.921, 25.957, 6.722, 16.925, 16.96, 116.73, 81.622, 6.223, 12.912, 16.041, 17.643, 28.359, 96.21),
  SEMANA_1 = c(1.294, 1.496, 1.366, 1.562, 1.537, 1.927, 1.928, 2.013, 1.927, 1.927, 2.984, 2.611, 2.222, 2.651, 2.648),
  SEMANA_2 = c(6.045, 5.568, 5.491, 5.089, 2.649, 5.886, 4.562, 7.84, 7.715, 3.66, 4.353, 7.616, 8.635, 13.757, 8.075),
  SEMANA_3 = c(1.145, 1.25, 1.216, 1.399, 1.357, 1.725, 1.761, 1.718, 1.704, 1.739, 3.97, 2.472, 2.016, 1.995, 2.618),
  SEMANA_4 = c(80.85, 4.321, 4.125, 4.283, 2.231, 4.118, 3.66, 81.88, 5.052, 2.848, 3.163, 4.861, 6.09, 7.62, 82.46),
  SEMANA_5 = c(1.88, 1.102, 1.118, 1.182, 1.153, 1.492, 1.524, 1.54, 1.473, 1.489, 3.243, 2.127, 1.759, 1.785, 2.329),
  SEMANA_6 = c(1.19, 2.97, 2.609, 2.74, 1.862, 2.937, 2.782, 3.76, 3.691, 2.345, 2.698, 3.758, 4.317, 5.633, 4.18),
  SEMANA_7 = c(1.65, 0.867, 1.006, 1.049, 1.063, 1.382, 1.414, 1.44, 1.382, 1.405, 2.612, 1.91, 1.619, 1.669, 2.07),
  SEMANA_8 = c(78.09, 2.523, 2.205, 2.318, 1.667, 2.482, 2.482, 80.11, 3.133, 2.127, 2.339, 3.096, 3.307, 4.642, 3.454)
)

# Realizar el análisis de varianza (ANOVA)
anova_result <- aov(TRES_DIAS ~ ., data = repeticiones)

# Mostrar los resultados del ANOVA
summary(anova_result)
